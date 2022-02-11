use std::fmt;

use crate::heap::Heap;
use crate::object::{Obj, ObjType};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    Obj(usize),
}

impl Value {
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

    pub fn print(&self, heap: &Heap<Obj>) -> String {
        match self {
            Self::Nil => "nil".to_string(),
            Self::Bool(true) => "true".to_string(),
            Self::Bool(false) => "false".to_string(),
            Self::Number(n) => format!("{}", n),
            Self::Obj(i) => match heap.get(*i) {
                None => "invalid".to_string(),
                Some(o) => format!("{}", o),
            },
        }
    }

    pub fn equal(left: &Self, right: &Self, heap: &Heap<Obj>) -> bool {
        match (left, right) {
            (Self::Nil, Self::Nil) => true,
            (Self::Bool(l), Self::Bool(r)) => l == r,
            (Self::Number(l), Self::Number(r)) => l == r,
            (Self::Obj(l), Self::Obj(r)) => match (heap.get(*l), heap.get(*r)) {
                (Some(l), Some(r)) => match (&l.value, &r.value) {
                    (ObjType::String(l), ObjType::String(r)) => l == r,
                    _ => false,
                },
                _ => false,
            },
            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::Bool(true) => write!(f, "true"),
            Self::Bool(false) => write!(f, "false"),
            Self::Number(n) => write!(f, "{}", n),
            Self::Obj(i) => write!(f, "Obj<{}>", i),
        }
    }
}
