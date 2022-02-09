use std::fmt;

use crate::object::{Closure, Function, Obj, Upvalue};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    Obj(*mut Obj),
}

impl Value {
    pub fn new_obj(mut obj: Obj, head: &mut *mut Obj) -> Self {
        *obj.next() = *head;
        let alloc_obj = Box::into_raw(Box::new(obj));
        *head = alloc_obj;

        #[cfg(feature = "debug_log_gc")]
        {
            writeln!(
                out_stream,
                "{} allocate {} for {}",
                alloc_obj,
                std::mem::size_of_val(&obj),
                &obj
            );
        }

        Self::Obj(alloc_obj)
    }

    pub fn as_obj(&self) -> *mut Obj {
        match self {
            Self::Obj(p) => *p,
            _ => panic!("Value is not a object"),
        }
    }

    pub fn as_number(&self) -> f64 {
        match self {
            Self::Number(v) => *v,
            _ => panic!("Value is not a number"),
        }
    }

    pub fn as_string(&self) -> &str {
        match self {
            Self::Obj(o) => unsafe {
                match &**o {
                    Obj::String(s, ..) => s,
                    _ => panic!("Obj is not a string"),
                }
            },
            _ => panic!("Value is not a obj"),
        }
    }

    pub fn as_function(&self) -> &Function {
        match self {
            Self::Obj(o) => unsafe {
                match &**o {
                    Obj::Function(f, ..) => f,
                    _ => panic!("Obj is not a function"),
                }
            },
            _ => panic!("Value is not a obj"),
        }
    }

    pub fn as_closure(&self) -> &mut Closure {
        match self {
            Self::Obj(o) => unsafe {
                match &mut **o {
                    Obj::Closure(c, ..) => c,
                    _ => panic!("Obj is not a closure"),
                }
            },
            _ => panic!("Value is not a obj"),
        }
    }

    pub fn as_upvalue(&self) -> &mut Upvalue {
        match self {
            Self::Obj(o) => unsafe {
                match &mut **o {
                    Obj::Upvalue(u, ..) => u,
                    _ => panic!("Obj is not a upvalue"),
                }
            },
            _ => panic!("Value is not a obj"),
        }
    }

    pub fn is_falsey(&self) -> bool {
        match self {
            Self::Nil | Self::Bool(false) => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            Self::Obj(o) => unsafe {
                match **o {
                    Obj::String(..) => true,
                    _ => false,
                }
            },
            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool(true) => fmt::Display::fmt("true", f),
            Self::Bool(false) => fmt::Display::fmt("false", f),
            Self::Nil => fmt::Display::fmt("nil", f),
            Self::Number(n) => fmt::Display::fmt(n, f),
            Self::Obj(o) => fmt::Display::fmt(unsafe { &**o }, f),
        }
    }
}
