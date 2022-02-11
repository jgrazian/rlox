use std::cell::Cell;
use std::fmt::{self, Debug};

use crate::chunk::Chunk;
use crate::heap::Heap;
use crate::value::Value;

pub enum ObjType {
    Null,
    String(String),
    Function(ObjFunction),
    Native(ObjNative),
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
        }
    }
}

#[derive(Debug, Default)]
pub struct Obj {
    pub value: ObjType,
}

impl Obj {
    pub fn alloc_string<S: Into<String>>(heap: &mut Heap<Self>, name: S) -> usize {
        let index = heap.push(Obj {
            value: ObjType::String(name.into()),
        });
        index
    }

    pub fn alloc_function(heap: &mut Heap<Self>, function: ObjFunction) -> usize {
        let index = heap.push(Obj {
            value: ObjType::Function(function),
        });
        index
    }

    pub fn alloc_native(
        heap: &mut Heap<Self>,
        function: fn(usize, &[Cell<Value>]) -> Value,
    ) -> usize {
        let index = heap.push(Obj {
            value: ObjType::Native(ObjNative { function }),
        });
        index
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
            ObjType::Null => fmt::Display::fmt("null", f),
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
}

impl ObjFunction {
    pub fn anon() -> Self {
        Self {
            arity: 0,
            chunk: Chunk::new(),
            name: None,
            ty: FunctionType::Script,
        }
    }

    pub fn named<S: Into<String>>(name: S) -> Self {
        Self {
            arity: 0,
            chunk: Chunk::new(),
            name: Some(name.into()),
            ty: FunctionType::Function,
        }
    }
}

pub struct ObjNative {
    pub function: fn(usize, &[Cell<Value>]) -> Value,
}
