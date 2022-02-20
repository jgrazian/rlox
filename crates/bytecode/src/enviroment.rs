use std::cell::{RefCell, RefMut};

use crate::compiler::Compiler;
use crate::error::LoxError;
use crate::heap::{Heap, HeapKey};
use crate::object::Obj;
use crate::vm::Vm;

pub struct Enviroment {
    heap: RefCell<Heap>,
    vm: Option<RefCell<Vm>>,
}

pub struct InnerEnv<'h> {
    env: &'h Enviroment,
    pub heap: RefMut<'h, Heap>,
}

pub trait Mark {
    fn mark(&self, heap: &mut Heap);
}

impl<'s> Enviroment {
    pub fn new() -> Self {
        let mut out = Self {
            heap: RefCell::new(Heap::new()),
            vm: None,
        };

        let vm = {
            let mut env = InnerEnv {
                env: &out,
                heap: out.heap.borrow_mut(),
            };

            RefCell::new(Vm::new(&mut env))
        };

        out.vm = Some(vm);
        out
    }

    pub fn interpret<S: std::io::Write>(
        &self,
        source: &'s str,
        stream: &mut S,
    ) -> Result<(), LoxError> {
        let mut env = InnerEnv {
            env: self,
            heap: self.heap.borrow_mut(),
        };

        let mut compiler = Compiler::new(source);
        let function = compiler.compile(&mut env)?;

        self.vm
            .as_ref()
            .unwrap()
            .borrow_mut()
            .interpret(function, stream, &mut env)
    }

    pub fn alloc<'h>(&self, heap: &mut RefMut<'h, Heap>, obj: Obj) -> HeapKey {
        heap.push(obj)
    }
}

impl<'h> InnerEnv<'h> {
    pub fn alloc<'s, T: Mark>(&mut self, obj: Obj, target: &T) -> HeapKey {
        target.mark(&mut self.heap);
        self.env.alloc(&mut self.heap, obj)
    }
}
