use std::fmt;

pub type Value = f64;

#[repr(u8)]
pub enum OpCode {
    OpConstant(u8),
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,
    OpNegate,
    OpReturn,
}

pub struct Chunk {
    pub code: Vec<OpCode>,
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

    pub fn write(&mut self, op: OpCode, line: usize) {
        self.lines.push(line);
        self.code.push(op);
    }

    pub fn add_constant(&mut self, value: Value) -> u8 {
        self.constants.push(value);
        (self.constants.len() - 1) as u8
    }
}

impl fmt::Debug for Chunk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, op) in self.code.iter().enumerate() {
            let repr = self.debug_op(i, op);
            writeln!(f, "{}", repr)?;
        }

        Ok(())
    }
}

impl Chunk {
    pub fn debug_op(&self, offset: usize, op: &OpCode) -> String {
        let repr = match op {
            OpCode::OpConstant(c) => format!(
                "{:<16} {:04} {}",
                "OP_CONSTANT", c, self.constants[*c as usize]
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
