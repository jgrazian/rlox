use std::ops::{Index, IndexMut};

use crate::object::Obj;

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
        #[cfg(feature = "debug_stress_gc")]
        {
            self.collect_garbage();
        }
        #[cfg(feature = "debug_log_gc")]
        {
            eprintln!("{:?} allocate {}", self.objects.len(), &object);
        }

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

    pub fn get_mut(&mut self, index: HeapKey) -> Option<&mut Obj> {
        match self.objects.get_mut(index.0) {
            Some(obj) => Some(obj),
            None => None,
        }
    }

    fn collect_garbage(&mut self) {
        #[cfg(feature = "debug_log_gc")]
        {
            eprintln!("-- gc begin");
        }

        #[cfg(feature = "debug_log_gc")]
        {
            eprintln!("-- gc end");
        }
    }

    pub fn gc_mark(&mut self, index: HeapKey) {
        let obj = &mut self[index];
        #[cfg(feature = "debug_log_gc")]
        {
            eprintln!("{} mark {}", index.0, &obj);
        }
        obj.is_marked = true;
    }
}

impl Heap {
    pub fn pop(&mut self, index: usize) -> Option<Obj> {
        if index >= self.objects.len() {
            return None;
        }
        Some(std::mem::take(&mut self.objects[index]))
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
