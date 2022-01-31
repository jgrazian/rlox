use std::fmt;

use crate::chunk::Chunk;

#[derive(Debug, Clone, PartialEq)]
pub enum Obj {
    String(String),
    Function(Function),
}

impl fmt::Display for Obj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(s) => fmt::Display::fmt(s, f),
            Self::Function(fun) => fmt::Display::fmt(&format!("<fn {}>", fun.name), f),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Function {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: String,
}

impl Function {
    pub fn new() -> Self {
        Self {
            arity: 0,
            chunk: Chunk::new(),
            name: String::with_capacity(16),
        }
    }
}
