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

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    arity: usize,
    chunk: Chunk,
    name: String,
}
