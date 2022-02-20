use std::cell::Cell;
use std::fmt::{self, Debug};

use crate::chunk::Chunk;
use crate::heap::HeapKey;
use crate::value::Value;

pub enum ObjType {
    Null,
    String(String),
    Function(ObjFunction),
    Native(ObjNative),
    Closure(ObjClosure),
    Upvalue(ObjUpvalue),
}

impl Default for ObjType {
    fn default() -> Self {
        ObjType::Null
    }
}

impl Debug for ObjType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ObjType::Null => write!(f, "null",),
            ObjType::String(s) => write!(f, "{:?}", s),
            ObjType::Function(fun) => write!(f, "{:?}", fun),
            ObjType::Native(..) => write!(f, "fn<native>"),
            ObjType::Closure(c) => write!(f, "{:?}", c),
            ObjType::Upvalue(v) => write!(f, "{:?}", v),
        }
    }
}

#[derive(Debug, Default)]
pub struct Obj {
    pub value: ObjType,
    pub is_marked: bool,
}

impl Obj {
    pub fn string<S: Into<String>>(string: S) -> Self {
        Self {
            value: ObjType::String(string.into()),
            is_marked: false,
        }
    }

    pub fn function(function: ObjFunction) -> Self {
        Self {
            value: ObjType::Function(function),
            is_marked: false,
        }
    }

    pub fn closure(closure: ObjClosure) -> Self {
        Self {
            value: ObjType::Closure(closure),
            is_marked: false,
        }
    }

    pub fn native(function: fn(usize, &[Cell<Value>]) -> Value) -> Self {
        Obj {
            value: ObjType::Native(ObjNative { function }),
            is_marked: false,
        }
    }

    pub fn upvalue(state: UpvalueState) -> Self {
        Obj {
            value: ObjType::Upvalue(ObjUpvalue { state }),
            is_marked: false,
        }
    }

    pub fn as_string(&self) -> &String {
        match &self.value {
            ObjType::String(s) => s,
            _ => panic!("Expected string"),
        }
    }

    pub fn as_function(&self) -> &ObjFunction {
        match &self.value {
            ObjType::Function(f) => f,
            _ => panic!("Expected function"),
        }
    }

    pub fn as_closure(&self) -> &ObjClosure {
        match &self.value {
            ObjType::Closure(c) => c,
            _ => panic!("Expected closure"),
        }
    }

    pub fn as_closure_mut(&mut self) -> &mut ObjClosure {
        match &mut self.value {
            ObjType::Closure(c) => c,
            _ => panic!("Expected closure"),
        }
    }

    pub fn as_upvalue(&self) -> &ObjUpvalue {
        match &self.value {
            ObjType::Upvalue(u) => u,
            _ => panic!("Expected upvalue"),
        }
    }

    pub fn as_upvalue_mut(&mut self) -> &mut ObjUpvalue {
        match &mut self.value {
            ObjType::Upvalue(u) => u,
            _ => panic!("Expected upvalue"),
        }
    }

    pub fn is_string(&self) -> bool {
        match &self.value {
            ObjType::String(_) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Obj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.value {
            ObjType::String(s) => fmt::Display::fmt(s, f),
            ObjType::Function(fun) => fmt::Display::fmt(
                &match &fun.name {
                    None => "<script>".to_string(),
                    Some(n) => format!("<fn {}>", n),
                },
                f,
            ),
            ObjType::Native(..) => write!(f, "<fn native>"),
            ObjType::Closure(c) => fmt::Display::fmt(&format!("<closure {}>", c.function.0), f),
            ObjType::Null => fmt::Display::fmt("null", f),
            ObjType::Upvalue(..) => write!(f, "upvalue"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionType {
    Function,
    Script,
}

impl Default for FunctionType {
    fn default() -> Self {
        Self::Script
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct ObjFunction {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: Option<String>,
    pub ty: FunctionType,
    pub upvalue_count: usize,
}

impl ObjFunction {
    pub fn anon() -> Self {
        Self {
            arity: 0,
            chunk: Chunk::new(),
            name: None,
            ty: FunctionType::Script,
            upvalue_count: 0,
        }
    }

    pub fn named<S: Into<String>>(name: S) -> Self {
        Self {
            arity: 0,
            chunk: Chunk::new(),
            name: Some(name.into()),
            ty: FunctionType::Function,
            upvalue_count: 0,
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct ObjClosure {
    pub function: HeapKey,
    pub upvalues: Vec<HeapKey>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UpvalueState {
    Open(usize),
    Closed(Value),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ObjUpvalue {
    pub state: UpvalueState,
}

impl Default for ObjUpvalue {
    fn default() -> Self {
        Self {
            state: UpvalueState::Open(0),
        }
    }
}

pub struct ObjNative {
    pub function: fn(usize, &[Cell<Value>]) -> Value,
}
