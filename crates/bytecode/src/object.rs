use std::fmt;
use std::ptr;

use crate::chunk::Chunk;
use crate::value::Value;

#[derive(Clone)]
pub enum Obj {
    String(String, *mut Obj, bool),
    Function(Function, *mut Obj, bool),
    Closure(Closure, *mut Obj, bool),
    Native(fn(usize, &[Value]) -> Value, *mut Obj, bool),
    Upvalue(Upvalue, *mut Obj, bool),
}

impl Obj {
    pub fn next(&mut self) -> &mut *mut Obj {
        match self {
            Obj::String(_, next, _) => next,
            Obj::Function(_, next, _) => next,
            Obj::Closure(_, next, _) => next,
            Obj::Native(_, next, _) => next,
            Obj::Upvalue(_, next, _) => next,
        }
    }

    pub fn marked(&mut self) -> &mut bool {
        match self {
            Obj::String(.., marked) => marked,
            Obj::Function(.., marked) => marked,
            Obj::Closure(.., marked) => marked,
            Obj::Native(.., marked) => marked,
            Obj::Upvalue(.., marked) => marked,
        }
    }

    pub fn as_closure(&self) -> &Closure {
        match self {
            Obj::Closure(c, ..) => c,
            _ => panic!("Object is not a closure."),
        }
    }

    pub fn as_closure_mut(&mut self) -> &mut Closure {
        match self {
            Obj::Closure(c, ..) => c,
            _ => panic!("Object is not a closure."),
        }
    }

    pub fn as_upvalue(&self) -> &Upvalue {
        match self {
            Obj::Upvalue(c, ..) => c,
            _ => panic!("Object is not an upvalue."),
        }
    }

    pub fn as_upvalue_mut(&mut self) -> &mut Upvalue {
        match self {
            Obj::Upvalue(c, ..) => c,
            _ => panic!("Object is not an upvalue."),
        }
    }
}

impl fmt::Display for Obj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(s, ..) => fmt::Display::fmt(s, f),
            Self::Function(fun, ..) => fmt::Display::fmt(
                &match fun.name.len() {
                    0 => "<script>".to_string(),
                    _ => format!("<fn {}>", fun.name),
                },
                f,
            ),
            Self::Closure(closure, ..) => fmt::Display::fmt(
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
    pub upvalues: Vec<*mut Obj>,
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
    pub next: *mut Obj,
}
