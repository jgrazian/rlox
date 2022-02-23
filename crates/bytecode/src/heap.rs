use std::ops::{Index, IndexMut};

use crate::object::{Obj, ObjType, UpvalueState};
use crate::value::Value;

pub enum MarkState {
    Grey,
    Black,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct HeapKey(pub usize);

#[derive(Debug)]
pub struct Heap {
    objects: Vec<Obj>,
    open: Vec<usize>,
}

impl Heap {
    pub fn new() -> Self {
        Self {
            objects: Vec::with_capacity(64),
            open: Vec::with_capacity(64),
        }
    }

    pub fn push(&mut self, object: Obj) -> HeapKey {
        #[cfg(feature = "debug_log_gc")]
        eprintln!("{:>2} allocate {}", self.objects.len(), &object);

        if self.open.is_empty() {
            self.objects.push(object);
            HeapKey(self.objects.len() - 1)
        } else {
            let index = self.open.pop().unwrap();
            self.objects[index] = object;
            HeapKey(index)
        }
    }

    pub fn get(&self, index: HeapKey) -> Option<&Obj> {
        match self.objects.get(index.0) {
            Some(obj) => Some(obj),
            None => None,
        }
    }

    pub fn gc_should_run(&self) -> bool {
        self.objects.len() - self.open.len() > self.objects.capacity() / 2
    }

    pub fn gc_mark(&self, index: HeapKey) -> MarkState {
        let obj = &self[index];
        if obj.is_marked.get() {
            return MarkState::Black;
        }
        #[cfg(feature = "debug_log_gc")]
        eprintln!("{:>2} mark {}", index.0, &obj);

        obj.is_marked.set(true);
        MarkState::Grey
    }

    pub fn gc_mark_value(&self, value: Value) -> MarkState {
        match value {
            Value::Obj(o) => self.gc_mark(o),
            _ => MarkState::Black,
        }
    }

    pub fn gc_trace_references(&self) {
        let mut gray_stack = self
            .objects
            .iter()
            .enumerate()
            .filter_map(|(i, obj)| if obj.is_marked.get() { Some(i) } else { None })
            .collect::<Vec<_>>();

        while let Some(idx) = gray_stack.pop() {
            self.gc_blacken(HeapKey(idx), &mut gray_stack)
        }
    }

    fn gc_blacken(&self, obj_key: HeapKey, gray_stack: &mut Vec<usize>) {
        let obj = &self[obj_key];
        #[cfg(feature = "debug_log_gc")]
        eprintln!("{:>2} blacken {}", obj_key.0, &obj);

        fn mark_value(heap: &Heap, value: Value, gray_stack: &mut Vec<usize>) {
            match heap.gc_mark_value(value) {
                MarkState::Black => {}
                MarkState::Grey => gray_stack.push(value.as_obj().0),
            }
        }

        fn mark(heap: &Heap, key: HeapKey, gray_stack: &mut Vec<usize>) {
            match heap.gc_mark(key) {
                MarkState::Black => {}
                MarkState::Grey => gray_stack.push(key.0),
            }
        }

        match &obj.value {
            ObjType::String(..) | ObjType::Native(..) => {}
            ObjType::Upvalue(upvalue) => match upvalue.state {
                UpvalueState::Open(..) => {}
                UpvalueState::Closed(value) => {
                    mark_value(self, value, gray_stack);
                }
            },
            ObjType::Function(f) => {
                for v in f.chunk.constants.iter() {
                    mark_value(self, *v, gray_stack);
                }
            }
            ObjType::Closure(ref c) => {
                mark(self, c.function, gray_stack);
                for upvalue in c.upvalues.iter() {
                    mark(self, *upvalue, gray_stack);
                }
            }
            ObjType::Null => {}
        }
    }

    fn gc_free(&mut self, index: HeapKey) {
        let obj = &mut self[index];

        #[cfg(feature = "debug_log_gc")]
        eprintln!("{:>2} free {}", index.0, &obj);

        *obj = Obj::default();
        self.open.push(index.0);
    }

    pub fn gc_sweep(&mut self) {
        let mut _free_count = 0;
        for index in 0..self.objects.len() {
            let obj = &self[HeapKey(index)];
            if obj.is_marked.get() {
                obj.is_marked.set(false);
            } else {
                self.gc_free(HeapKey(index));
                _free_count += 1;
            }
        }
        #[cfg(feature = "debug_log_gc")]
        eprintln!("   freed {} slots", _free_count);
    }
}

impl<I: Into<HeapKey>> Index<I> for Heap {
    type Output = Obj;

    fn index(&self, index: I) -> &Self::Output {
        &self.objects[index.into().0]
    }
}

impl<I: Into<HeapKey>> IndexMut<I> for Heap {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        &mut self.objects[index.into().0]
    }
}
