use std::collections::HashMap;

use crate::ast::Literal;
use crate::interpreter::RuntimeError;
use crate::token::Token;

#[derive(Debug, Clone)]
pub struct Enviroment {
    values: HashMap<String, Literal>,
    enclosing: Option<Box<Enviroment>>,
}

impl Enviroment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn enclosed(enclosing: Enviroment) -> Self {
        Self {
            values: HashMap::new(),
            enclosing: Some(Box::new(enclosing)),
        }
    }

    pub fn define(&mut self, name: &str, value: Literal) {
        self.values.insert(name.to_string(), value);
    }

    pub fn get(&self, name: &Token) -> Result<Literal, RuntimeError> {
        match self.values.get(&name.lexeme) {
            Some(value) => Ok(value.clone()),
            None => match &self.enclosing {
                Some(enclosed) => enclosed.get(name),
                None => Err(RuntimeError {
                    token: name.clone(),
                    message: format!("Undefined variable '{}'.", name),
                }),
            },
        }
    }

    pub fn assign(&mut self, name: &Token, value: Literal) -> Result<(), RuntimeError> {
        match self.values.get_mut(&name.lexeme) {
            Some(_value) => {
                *_value = value.clone();
                Ok(())
            }
            None => match self.enclosing {
                Some(ref mut enclosed) => enclosed.assign(name, value),
                None => Err(RuntimeError {
                    token: name.clone(),
                    message: format!("Undefined variable '{}'.", name),
                }),
            },
        }
    }
}
