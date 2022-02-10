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
            | Self::OpSetLocal => 2,
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
        eprintln!("== {} ==", name);
        eprint!("{:?}", self);
    }
}

impl fmt::Debug for Chunk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut offset = 0;
        while offset < self.code.len() {
            let op = &self.code[offset].into();
            let repr = self.debug_op(offset, op);
            writeln!(f, "{}", repr)?;
            offset += op.size();
        }

        Ok(())
    }
}

impl Chunk {
    pub fn debug_op(&self, offset: usize, op: &OpCode) -> String {
        fn const_instr(chunk: &Chunk, name: &str, offset: usize) -> String {
            format!(
                "{:<16} {:>4} '{}'",
                name,
                chunk.code[offset + 1],
                chunk.constants[chunk.code[offset + 1] as usize]
            )
        }

        fn byte_instr(chunk: &Chunk, name: &str, offset: usize) -> String {
            let slot = chunk.code[offset + 1] as usize;
            format!("{:<16} {:>4}", name, slot)
        }

        fn jump_instr(chunk: &Chunk, name: &str, offset: usize, sign: isize) -> String {
            let jump = ((chunk.code[offset + 1] as isize) << 8) | (chunk.code[offset + 2] as isize);
            format!(
                "{:<16} {:>4} -> {}",
                name,
                offset,
                offset as isize + 3 + sign * jump
            )
        }

        let repr = match op {
            OpCode::OpConstant => const_instr(self, "OP_CONST", offset),
            OpCode::OpNil => "OP_NIL".to_string(),
            OpCode::OpTrue => "OP_TRUE".to_string(),
            OpCode::OpFalse => "OP_FALSE".to_string(),
            OpCode::OpPop => "OP_POP".to_string(),
            OpCode::OpGetLocal => byte_instr(self, "OP_GET_LOCAL", offset),
            OpCode::OpSetLocal => byte_instr(self, "OP_SET_LOCAL", offset),
            OpCode::OpGetGlobal => const_instr(self, "OP_GET_GLOBAL", offset),
            OpCode::OpDefineGlobal => const_instr(self, "OP_DEFINE_GLOBAL", offset),
            OpCode::OpSetGlobal => const_instr(self, "OP_SET_GLOBAL", offset),
            OpCode::OpEqual => "OP_EQUAL".to_string(),
            OpCode::OpGreater => "OP_GREATER".to_string(),
            OpCode::OpLess => "OP_LESS".to_string(),
            OpCode::OpAdd => "OP_ADD".to_string(),
            OpCode::OpSubtract => "OP_SUBTRACT".to_string(),
            OpCode::OpMultiply => "OP_MULTIPLY".to_string(),
            OpCode::OpDivide => "OP_DIVIDE".to_string(),
            OpCode::OpNot => "OP_NOT".to_string(),
            OpCode::OpNegate => "OP_NEGATE".to_string(),
            OpCode::OpPrint => "OP_PRINT".to_string(),
            OpCode::OpJump => jump_instr(self, "OP_JUMP", offset, 1),
            OpCode::OpJumpIfFalse => jump_instr(self, "OP_JUMP_IF_FALSE", offset, 1),
            OpCode::OpLoop => jump_instr(self, "OP_LOOP", offset, -1),
            OpCode::OpReturn => "OP_RETURN".to_string(),
        };

        let line = if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            "   |".to_string()
        } else {
            format!("{:4}", self.lines[offset])
        };

        format!("{:04} {} {}", offset, line, repr)
    }
}
