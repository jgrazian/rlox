use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::LoxObject;
use crate::interpreter::RuntimeError;
use crate::token::Token;

#[derive(Debug, Clone)]
pub struct Enviroment {
    values: HashMap<String, LoxObject>,
    pub enclosing: Option<Rc<RefCell<Enviroment>>>,
}

impl Enviroment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn enclosed(enclosing: Rc<RefCell<Enviroment>>) -> Self {
        Self {
            values: HashMap::new(),
            enclosing: Some(enclosing),
        }
    }

    pub fn define(&mut self, name: &str, value: LoxObject) {
        self.values.insert(name.to_string(), value);
    }

    pub fn get(&self, name: &Token) -> Result<LoxObject, RuntimeError> {
        match self.values.get(&name.lexeme) {
            Some(value) => Ok(value.clone()),
            None => match &self.enclosing {
                Some(enclosed) => enclosed.borrow().get(name),
                None => Err(RuntimeError::new(
                    name,
                    &format!("Undefined variable '{}'.", name),
                )),
            },
        }
    }

    pub fn get_at(&self, distance: usize, name: String) -> Result<LoxObject, RuntimeError> {
        if distance == 0 {
            Ok(self.values.get(&name).unwrap().clone())
        } else {
            let ancestor = self.ancestor(distance);
            let value = ancestor.borrow().values.get(&name).unwrap().clone();
            Ok(value)
        }
    }

    fn ancestor(&self, distance: usize) -> Rc<RefCell<Enviroment>> {
        fn dive<'a>(env: &Rc<RefCell<Enviroment>>, distance: usize) -> Rc<RefCell<Enviroment>> {
            if distance == 0 {
                env.clone()
            } else {
                dive((*env).borrow().enclosing.as_ref().unwrap(), distance - 1)
            }
        }

        dive(&self.enclosing.as_ref().unwrap(), distance - 1)
    }

    pub fn assign(&mut self, name: &Token, value: LoxObject) -> Result<(), RuntimeError> {
        match self.values.get_mut(&name.lexeme) {
            Some(_value) => {
                *_value = value.clone();
                Ok(())
            }
            None => match &self.enclosing {
                Some(enclosed) => {
                    enclosed.borrow_mut().assign(name, value)?;
                    Ok(())
                }
                None => Err(RuntimeError::new(
                    name,
                    &format!("Undefined variable '{}'.", name),
                )),
            },
        }
    }

    pub fn assign_at(
        &mut self,
        distance: usize,
        name: &Token,
        value: LoxObject,
    ) -> Result<(), RuntimeError> {
        if distance == 0 {
            self.values.insert(name.lexeme.clone(), value);
        } else {
            let ancestor = self.ancestor(distance);
            ancestor
                .borrow_mut()
                .values
                .insert(name.lexeme.clone(), value);
        }
        Ok(())
    }
}
