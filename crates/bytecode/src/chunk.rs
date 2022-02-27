use crate::heap::Heap;
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
    OpGetProperty,
    OpSetProperty,
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
    OpCloseUpvalue,
    OpReturn,
    OpClass,
    OpMethod,
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

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub lines: Vec<usize>,
    pub constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::with_capacity(32),
            lines: Vec::with_capacity(32),
            constants: Vec::with_capacity(32),
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
    pub fn disassemble(&self, name: &str, heap: &Heap) {
        eprintln!("=== {} ===", name);

        let mut offset = 0;
        while offset < self.code.len() {
            let op = &self.code[offset].into();
            let (_offset, repr) = self.debug_op(offset, op, heap);
            eprint!("{}", repr);
            offset = _offset;
        }
    }
}

impl Chunk {
    pub fn debug_op(&self, offset: usize, op: &OpCode, heap: &Heap) -> (usize, String) {
        fn single_instr(name: &str) -> (usize, String) {
            (1, name.to_string())
        }

        fn const_instr(chunk: &Chunk, name: &str, offset: usize, heap: &Heap) -> (usize, String) {
            let s = format!(
                "{:<16} {:>4} '{}'",
                name,
                chunk.code[offset + 1],
                chunk.constants[chunk.code[offset + 1] as usize].print(heap)
            );
            (2, s)
        }

        fn byte_instr(chunk: &Chunk, name: &str, offset: usize) -> (usize, String) {
            let slot = chunk.code[offset + 1] as usize;
            (2, format!("{:<16} {:>4}", name, slot))
        }

        fn jump_instr(chunk: &Chunk, name: &str, offset: usize, sign: isize) -> (usize, String) {
            let jump = ((chunk.code[offset + 1] as isize) << 8) | (chunk.code[offset + 2] as isize);
            let s = format!(
                "{:<16} {:>4} -> {}",
                name,
                offset,
                offset as isize + 3 + sign * jump
            );
            (3, s)
        }

        let (inc, repr) = match op {
            OpCode::OpConstant => const_instr(self, "OP_CONST", offset, heap),
            OpCode::OpNil => single_instr("OP_NIL"),
            OpCode::OpTrue => single_instr("OP_TRUE"),
            OpCode::OpFalse => single_instr("OP_FALSE"),
            OpCode::OpPop => single_instr("OP_POP"),
            OpCode::OpGetLocal => byte_instr(self, "OP_GET_LOCAL", offset),
            OpCode::OpSetLocal => byte_instr(self, "OP_SET_LOCAL", offset),
            OpCode::OpGetGlobal => const_instr(self, "OP_GET_GLOBAL", offset, heap),
            OpCode::OpDefineGlobal => const_instr(self, "OP_DEFINE_GLOBAL", offset, heap),
            OpCode::OpSetGlobal => const_instr(self, "OP_SET_GLOBAL", offset, heap),
            OpCode::OpGetUpvalue => byte_instr(self, "OP_GET_UPVALUE", offset),
            OpCode::OpSetUpvalue => byte_instr(self, "OP_SET_UPVALUE", offset),
            OpCode::OpGetProperty => const_instr(self, "OP_GET_PROPERTY", offset, heap),
            OpCode::OpSetProperty => const_instr(self, "OP_SET_PROPERTY", offset, heap),
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
                    "OP_CLOSURE",
                    constant,
                    self.constants[constant].print(heap)
                );

                let line = if offset - 2 > 0 && self.lines[offset - 2] == self.lines[offset - 3] {
                    "   |".to_string()
                } else {
                    format!("{:4}", self.lines[offset])
                };

                let mut top = format!("{:04} {} {}\n", offset - 2, line, repr);

                let function = heap[self.constants[constant].as_obj()].as_function();
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
            OpCode::OpCloseUpvalue => single_instr("OP_CLOSE_UPVALUE"),
            OpCode::OpReturn => single_instr("OP_RETURN"),
            OpCode::OpClass => const_instr(self, "OP_CLASS", offset, heap),
            OpCode::OpMethod => const_instr(self, "OP_METHOD", offset, heap),
        };

        let line = if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            "   |".to_string()
        } else {
            format!("{:4}", self.lines[offset])
        };

        (offset + inc, format!("{:04} {} {}\n", offset, line, repr))
    }
}
