use std::ops::{Index, IndexMut};

#[derive(Debug)]
pub struct Heap<T> {
    objects: Vec<T>,
    open: Vec<usize>,
}

impl<T> Heap<T> {
    pub fn new() -> Self {
        Self {
            objects: Vec::with_capacity(64),
            open: Vec::with_capacity(64),
        }
    }

    pub fn push(&mut self, object: T) -> usize {
        if self.open.is_empty() {
            self.objects.push(object);
            self.objects.len() - 1
        } else {
            let index = self.open.pop().unwrap();
            self.objects[index] = object;
            index
        }
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        match self.objects.get(index) {
            Some(obj) => Some(obj),
            None => None,
        }
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        match self.objects.get_mut(index) {
            Some(obj) => Some(obj),
            None => None,
        }
    }
}

impl<T: Default> Heap<T> {
    pub fn pop(&mut self, index: usize) -> Option<T> {
        if index >= self.objects.len() {
            return None;
        }
        Some(std::mem::take(&mut self.objects[index]))
    }
}

impl<T> Index<usize> for Heap<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        &self.objects[index]
    }
}

impl<T> IndexMut<usize> for Heap<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.objects[index]
    }
}
