use std::ops::{Index, IndexMut};

#[derive(Debug)]
pub struct Heap<T> {
    objects: Vec<*mut T>,
}

impl<T> Heap<T> {
    pub fn new() -> Self {
        Self {
            objects: Vec::new(),
        }
    }

    pub fn push(&mut self, object: T) -> usize {
        let obj_ptr = Box::into_raw(Box::new(object));
        self.objects.push(obj_ptr);
        self.objects.len() - 1
    }

    pub fn pop(&mut self, index: usize) -> Option<Box<T>> {
        if index >= self.objects.len() {
            return None;
        }
        unsafe { Some(Box::from_raw(self.objects.swap_remove(index))) }
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        match self.objects.get(index) {
            Some(obj) => match obj.is_null() {
                true => None,
                false => Some(unsafe { &**obj }),
            },
            None => None,
        }
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        match self.objects.get(index) {
            Some(obj) => match obj.is_null() {
                true => None,
                false => Some(unsafe { &mut **obj }),
            },
            None => None,
        }
    }
}

impl<T> Index<usize> for Heap<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        unsafe { &*self.objects[index] }
    }
}

impl<T> IndexMut<usize> for Heap<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        unsafe { &mut *self.objects[index] }
    }
}
