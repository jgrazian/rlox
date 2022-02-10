use crate::chunk::Chunk;
use crate::heap::Heap;

// impl fmt::Display for Obj {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             Self::String(s) => fmt::Display::fmt(s, f),
//             Self::Function(fun) => fmt::Display::fmt(
//                 &match fun.name.len() {
//                     0 => "<script>".to_string(),
//                     _ => format!("<fn {}>", fun.name),
//                 },
//                 f,
//             ),
//         }
//     }
// }

#[derive(Debug, Clone, PartialEq)]
pub enum ObjType {
    Null,
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

#[derive(Debug, Default, Clone, PartialEq)]
pub struct ObjFunction {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: String,
}

impl ObjFunction {
    pub fn new<S: Into<String>>(name: S) -> Self {
        Self {
            arity: 0,
            chunk: Chunk::new(),
            name: name.into(),
        }
    }
}
