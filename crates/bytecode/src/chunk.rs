use std::fmt;

use crate::value::Value;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OpCode {
    OpConstant,
    OpNil,
    OpTrue,
    OpFalse,
    OpEqual,
    OpGreater,
    OpLess,
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,
    OpNot,
    OpNegate,
    OpReturn,
}

impl OpCode {
    fn size(&self) -> usize {
        match self {
            Self::OpConstant => 2,
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

#[derive(PartialEq)]
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
            let repr = self.debug_op(offset, op);
            writeln!(f, "{}", repr)?;
            offset += op.size();
        }

        Ok(())
    }
}

impl Chunk {
    pub fn debug_op(&self, offset: usize, op: &OpCode) -> String {
        let repr = match op {
            OpCode::OpConstant => format!(
                "{:<16} {:04} {}",
                "OP_CONSTANT",
                self.code[offset + 1],
                self.constants[self.code[offset + 1] as usize]
            ),
            OpCode::OpNil => "OP_NIL".to_string(),
            OpCode::OpTrue => "OP_TRUE".to_string(),
            OpCode::OpFalse => "OP_FALSE".to_string(),
            OpCode::OpEqual => "OP_EQUAL".to_string(),
            OpCode::OpGreater => "OP_GREATER".to_string(),
            OpCode::OpLess => "OP_LESS".to_string(),
            OpCode::OpAdd => "OP_ADD".to_string(),
            OpCode::OpSubtract => "OP_SUBTRACT".to_string(),
            OpCode::OpMultiply => "OP_MULTIPLY".to_string(),
            OpCode::OpDivide => "OP_DIVIDE".to_string(),
            OpCode::OpNot => "OP_NOT".to_string(),
            OpCode::OpNegate => "OP_NEGATE".to_string(),
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
