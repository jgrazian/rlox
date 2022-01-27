use std::fmt;

use crate::object::Obj;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    Obj(Box<Obj>),
}

impl Value {
    pub fn as_bool(&self) -> bool {
        match self {
            Self::Bool(b) => *b,
            _ => panic!("Value is not a bool"),
        }
    }

    pub fn as_number(&self) -> f64 {
        match self {
            Self::Number(v) => *v,
            _ => panic!("Value is not a number"),
        }
    }

    pub fn as_obj(self) -> Box<Obj> {
        match self {
            Self::Obj(v) => v,
            _ => panic!("Value is not a obj"),
        }
    }

    pub fn is_falsey(&self) -> bool {
        match self {
            Self::Nil | Self::Bool(false) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool(true) => write!(f, "true"),
            Self::Bool(false) => write!(f, "false"),
            Self::Nil => write!(f, "nil"),
            Self::Number(n) => write!(f, "{}", n),
            _ => write!(f, "???"),
        }
    }
}
