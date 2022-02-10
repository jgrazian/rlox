use std::fmt;

use crate::chunk::Chunk;
use crate::heap::Heap;

#[derive(Debug, Clone, PartialEq)]
pub enum ObjType {
    String(String),
    Function(ObjFunction),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Obj {
    value: ObjType,
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
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct ObjFunction {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: Option<String>,
}

impl ObjFunction {
    pub fn anon() -> Self {
        Self {
            arity: 0,
            chunk: Chunk::new(),
            name: None,
        }
    }

    pub fn named<S: Into<String>>(name: S) -> Self {
        Self {
            arity: 0,
            chunk: Chunk::new(),
            name: Some(name.into()),
        }
    }
}
