use std::fmt;

use crate::object::{Function, Obj};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    Obj(Box<Obj>),
}

impl Value {
    pub fn as_number(&self) -> f64 {
        match self {
            Self::Number(v) => *v,
            _ => panic!("Value is not a number"),
        }
    }

    pub fn as_string(&self) -> &str {
        match self {
            Self::Obj(o) => match &**o {
                Obj::String(s) => s,
                _ => panic!("Obj is not a string"),
            },
            _ => panic!("Value is not a obj"),
        }
    }

    pub fn as_function(&self) -> &Function {
        match self {
            Self::Obj(o) => match &**o {
                Obj::Function(f) => f,
                _ => panic!("Obj is not a function"),
            },
            _ => panic!("Value is not a obj"),
        }
    }

    pub fn is_falsey(&self) -> bool {
        match self {
            Self::Nil | Self::Bool(false) => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            Self::Obj(o) => match **o {
                Obj::String(_) => true,
                _ => false,
            },
            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool(true) => fmt::Display::fmt("true", f),
            Self::Bool(false) => fmt::Display::fmt("false", f),
            Self::Nil => fmt::Display::fmt("nil", f),
            Self::Number(n) => fmt::Display::fmt(n, f),
            Self::Obj(o) => fmt::Display::fmt(o, f),
        }
    }
}
