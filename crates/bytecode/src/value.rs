use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    Obj(usize),
}

impl Value {
    pub fn as_bool(&self) -> bool {
        match self {
            Self::Bool(b) => *b,
            _ => panic!("Value is not a boolean"),
        }
    }

    pub fn as_number(&self) -> f64 {
        match self {
            Self::Number(n) => *n,
            _ => panic!("Value is not a number"),
        }
    }

    pub fn as_obj(&self) -> usize {
        match self {
            Self::Obj(o) => *o,
            _ => panic!("Value is not an object"),
        }
    }

    pub fn is_falsey(&self) -> bool {
        match self {
            Self::Nil | Self::Bool(false) => true,
            _ => false,
        }
    }

    pub fn function() -> Self {
        Self::Obj(0)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::Bool(true) => write!(f, "true"),
            Self::Bool(false) => write!(f, "false"),
            Self::Number(n) => write!(f, "{}", n),
            Self::Obj(o) => write!(f, "Obj<{}>", o),
        }
    }
}
