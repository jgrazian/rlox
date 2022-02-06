use std::fmt;
use std::ptr;

use crate::chunk::Chunk;
use crate::value::Value;

#[derive(Clone)]
pub enum Obj {
    String(String, *mut Obj),
    Function(Function, *mut Obj),
    Closure(Closure, *mut Obj),
    Native(fn(usize, &[Value]) -> Value, *mut Obj),
    Upvalue(Upvalue, *mut Obj),
}

impl Obj {
    pub fn next(&mut self) -> &mut *mut Obj {
        match self {
            Obj::String(_, next) => next,
            Obj::Function(_, next) => next,
            Obj::Closure(_, next) => next,
            Obj::Native(_, next) => next,
            Obj::Upvalue(_, next) => next,
        }
    }
}

impl fmt::Display for Obj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(s, _) => fmt::Display::fmt(s, f),
            Self::Function(fun, _) => fmt::Display::fmt(
                &match fun.name.len() {
                    0 => "<script>".to_string(),
                    _ => format!("<fn {}>", fun.name),
                },
                f,
            ),
            Self::Closure(closure, _) => fmt::Display::fmt(
                &match unsafe { &(*closure.function).name } {
                    n if n.len() == 0 => "<script>".to_string(),
                    n => format!("<fn {}>", n),
                },
                f,
            ),
            Self::Native(..) => write!(f, "<native fn>"),
            Self::Upvalue(..) => write!(f, "<upvalue>"),
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
    pub upvalues: Vec<*mut Upvalue>,
}

impl Closure {
    pub fn new(function: *const Function) -> Self {
        Self {
            function,
            upvalues: vec![ptr::null_mut(); unsafe { (*function).upvalue_count }],
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Upvalue {
    pub location: *mut Value,
    pub closed: Value,
    pub next: *mut Upvalue,
}
