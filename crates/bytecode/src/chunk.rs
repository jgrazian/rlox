use std::fmt;

pub type Value = f64;

pub enum OpCode {
    OpConstant,
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,
    OpNegate,
    OpReturn,
}

impl OpCode {
    fn size(&self) -> usize {
        match self {
            OpCode::OpConstant => 2,
            OpCode::OpAdd => 1,
            OpCode::OpSubtract => 1,
            OpCode::OpMultiply => 1,
            OpCode::OpDivide => 1,
            OpCode::OpNegate => 1,
            OpCode::OpReturn => 1,
        }
    }
}

impl From<u8> for OpCode {
    fn from(v: u8) -> Self {
        match v {
            0 => OpCode::OpConstant,
            1 => OpCode::OpAdd,
            2 => OpCode::OpSubtract,
            3 => OpCode::OpMultiply,
            4 => OpCode::OpDivide,
            5 => OpCode::OpNegate,
            6 => OpCode::OpReturn,
            _ => panic!("Unknown opcode {}", v),
        }
    }
}

impl From<OpCode> for u8 {
    fn from(v: OpCode) -> Self {
        match v {
            OpCode::OpConstant => 0,
            OpCode::OpAdd => 1,
            OpCode::OpSubtract => 2,
            OpCode::OpMultiply => 3,
            OpCode::OpDivide => 4,
            OpCode::OpNegate => 5,
            OpCode::OpReturn => 6,
        }
    }
}

pub struct Chunk {
    pub code: Vec<u8>,
    lines: Vec<usize>,
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

    pub fn write_op<T: Into<u8>>(&mut self, op: T, line: usize) {
        self.lines.push(line);
        self.code.push(op.into());
    }

    pub fn add_constant(&mut self, value: Value) -> u8 {
        self.constants.push(value);
        (self.constants.len() - 1) as u8
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
            OpCode::OpAdd => "OP_ADD".to_string(),
            OpCode::OpSubtract => "OP_SUBTRACT".to_string(),
            OpCode::OpMultiply => "OP_MULTIPLY".to_string(),
            OpCode::OpDivide => "OP_DIVIDE".to_string(),
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