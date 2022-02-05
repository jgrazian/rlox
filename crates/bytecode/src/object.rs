use std::fmt;

use crate::chunk::Chunk;
use crate::value::Value;

#[derive(Clone)]
pub enum Obj {
    String(String),
    Function(Function),
    Closure(Closure),
    Native(fn(usize, &[Value]) -> Value),
}

impl fmt::Display for Obj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(s) => fmt::Display::fmt(s, f),
            Self::Function(fun) => fmt::Display::fmt(
                &match fun.name.len() {
                    0 => "<script>".to_string(),
                    _ => format!("<fn {}>", fun.name),
                },
                f,
            ),
            Self::Closure(closure) => fmt::Display::fmt(
                &match unsafe { &(*closure.function).name } {
                    n if n.len() == 0 => "<script>".to_string(),
                    n => format!("<fn {}>", n),
                },
                f,
            ),
            Self::Native(..) => write!(f, "<native fn>"),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Function {
    pub arity: usize,
    pub upvalue_count: usize,
    pub chunk: Chunk,
    pub name: String,
}

impl Function {
    pub fn new() -> Self {
        Self {
            arity: 0,
            upvalue_count: 0,
            chunk: Chunk::new(),
            name: String::with_capacity(16),
        }
    }

    pub fn named(name: &str) -> Self {
        Self {
            arity: 0,
            upvalue_count: 0,
            chunk: Chunk::new(),
            name: name.to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub function: *const Function,
}

impl Closure {
    pub fn new(function: *const Function) -> Self {
        Self { function }
    }
}
