use std::fmt;

use crate::value::Value;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OpCode {
    OpConstant,
    OpNil,
    OpTrue,
    OpFalse,
    OpPop,
    OpGetLocal,
    OpSetLocal,
    OpGetGlobal,
    OpDefineGlobal,
    OpSetGlobal,
    OpGetUpvalue,
    OpSetUpvalue,
    OpEqual,
    OpGreater,
    OpLess,
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,
    OpNot,
    OpNegate,
    OpPrint,
    OpJump,
    OpJumpIfFalse,
    OpLoop,
    OpCall,
    OpClosure,
    OpReturn,
}

impl OpCode {
    fn size(&self) -> usize {
        match self {
            Self::OpConstant
            | Self::OpGetGlobal
            | Self::OpDefineGlobal
            | Self::OpSetGlobal
            | Self::OpGetLocal
            | Self::OpSetLocal
            | Self::OpCall => 2,
            Self::OpJump | Self::OpJumpIfFalse | Self::OpLoop => 3,
            _ => 1,
        }
    }
}

impl From<u8> for OpCode {
    fn from(v: u8) -> Self {
        unsafe { std::mem::transmute::<u8, OpCode>(v) }
    }
}

impl From<OpCode> for u8 {
    fn from(v: OpCode) -> Self {
        v as u8
    }
}

#[derive(Default, Clone, PartialEq)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub lines: Vec<usize>,
    pub constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::with_capacity(8),
            lines: Vec::with_capacity(8),
            constants: Vec::with_capacity(8),
        }
    }

    pub fn push_byte<T: Into<u8>>(&mut self, op: T, line: usize) {
        self.lines.push(line);
        self.code.push(op.into());
    }

    pub fn push_constant(&mut self, value: Value) -> u8 {
        self.constants.push(value);
        (self.constants.len() - 1) as u8
    }

    #[allow(dead_code)]
    pub fn disassemble(&self, name: &str) {
        println!("== {} ==", name);
        print!("{:?}", self);
    }
}

impl fmt::Debug for Chunk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut offset = 0;
        while offset < self.code.len() {
            let op = &self.code[offset].into();
            let (_offset, repr) = self.debug_op(offset, op);
            write!(f, "{}", repr)?;
            offset = _offset;
        }

        Ok(())
    }
}

impl Chunk {
    pub fn debug_op(&self, offset: usize, op: &OpCode) -> (usize, String) {
        fn single_instr(name: &str) -> (usize, String) {
            (1, name.to_string())
        }

        fn const_instr(chunk: &Chunk, name: &str, offset: usize) -> (usize, String) {
            (
                2,
                format!(
                    "{:<16} {:>4} '{}'",
                    name,
                    chunk.code[offset + 1],
                    chunk.constants[chunk.code[offset + 1] as usize]
                ),
            )
        }

        fn byte_instr(chunk: &Chunk, name: &str, offset: usize) -> (usize, String) {
            let slot = chunk.code[offset + 1] as usize;
            (2, format!("{:<16} {:>4}", name, slot))
        }

        fn jump_instr(chunk: &Chunk, name: &str, offset: usize, sign: isize) -> (usize, String) {
            let jump = ((chunk.code[offset + 1] as isize) << 8) | (chunk.code[offset + 2] as isize);
            (
                3,
                format!(
                    "{:<16} {:>4} -> {}",
                    name,
                    offset,
                    offset as isize + 3 + sign * jump
                ),
            )
        }

        let (inc, repr) = match op {
            OpCode::OpConstant => const_instr(self, "OP_CONST", offset),
            OpCode::OpNil => single_instr("OP_NIL"),
            OpCode::OpTrue => single_instr("OP_TRUE"),
            OpCode::OpFalse => single_instr("OP_FALSE"),
            OpCode::OpPop => single_instr("OP_POP"),
            OpCode::OpGetLocal => byte_instr(self, "OP_GET_LOCAL", offset),
            OpCode::OpSetLocal => byte_instr(self, "OP_SET_LOCAL", offset),
            OpCode::OpGetGlobal => const_instr(self, "OP_GET_GLOBAL", offset),
            OpCode::OpDefineGlobal => const_instr(self, "OP_DEFINE_GLOBAL", offset),
            OpCode::OpSetGlobal => const_instr(self, "OP_SET_GLOBAL", offset),
            OpCode::OpGetUpvalue => byte_instr(self, "OP_GET_UPVALUE", offset),
            OpCode::OpSetUpvalue => byte_instr(self, "OP_SET_UPVALUE", offset),
            OpCode::OpEqual => single_instr("OP_EQUAL"),
            OpCode::OpGreater => single_instr("OP_GREATER"),
            OpCode::OpLess => single_instr("OP_LESS"),
            OpCode::OpAdd => single_instr("OP_ADD"),
            OpCode::OpSubtract => single_instr("OP_SUBTRACT"),
            OpCode::OpMultiply => single_instr("OP_MULTIPLY"),
            OpCode::OpDivide => single_instr("OP_DIVIDE"),
            OpCode::OpNot => single_instr("OP_NOT"),
            OpCode::OpNegate => single_instr("OP_NEGATE"),
            OpCode::OpPrint => single_instr("OP_PRINT"),
            OpCode::OpJump => jump_instr(self, "OP_JUMP", offset, 1),
            OpCode::OpJumpIfFalse => jump_instr(self, "OP_JUMP_IF_FALSE", offset, 1),
            OpCode::OpLoop => jump_instr(self, "OP_LOOP", offset, -1),
            OpCode::OpCall => byte_instr(self, "OP_CALL", offset),
            OpCode::OpClosure => {
                let mut offset = offset + 1;
                let constant = self.code[offset] as usize;
                offset += 1;

                let repr = format!(
                    "{:<16} {:>4} {}",
                    "OP_CLOSURE", constant, self.constants[constant]
                );

                let line = if offset - 2 > 0 && self.lines[offset - 2] == self.lines[offset - 3] {
                    "   |".to_string()
                } else {
                    format!("{:4}", self.lines[offset])
                };

                let mut top = format!("{:04} {} {}\n", offset - 2, line, repr);

                let function = self.constants[constant].as_function();
                for _ in 0..function.upvalue_count {
                    let is_local = if self.code[offset] == 0 {
                        "upvalue"
                    } else {
                        "local"
                    };
                    offset += 1;
                    let index = self.code[offset];
                    offset += 1;
                    top += &format!(
                        "{:04}    |                       {} {}\n",
                        offset - 2,
                        is_local,
                        index
                    )
                }
                return (offset, top);
            }
            OpCode::OpReturn => single_instr("OP_RETURN"),
        };

        let line = if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            "   |".to_string()
        } else {
            format!("{:4}", self.lines[offset])
        };

        (offset + inc, format!("{:04} {} {}\n", offset, line, repr))
    }
}
