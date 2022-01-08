use std::collections::HashMap;

use crate::ast::Literal;
use crate::interpreter::RuntimeError;
use crate::token::Token;

pub struct Enviroment {
    values: HashMap<String, Literal>,
}

impl Enviroment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: &str, value: Literal) {
        self.values.insert(name.to_string(), value);
    }

    pub fn get(&self, name: &Token) -> Result<Literal, RuntimeError> {
        match self.values.get(&name.lexeme) {
            Some(value) => Ok(value.clone()),
            None => Err(RuntimeError {
                token: name.clone(),
                message: format!("Undefined variable '{}'.", name),
            }),
        }
    }

    pub fn assign(&mut self, name: &Token, value: Literal) -> Result<(), RuntimeError> {
        match self.values.get_mut(&name.lexeme) {
            Some(value) => {
                *value = value.clone();
                Ok(())
            }
            None => Err(RuntimeError {
                token: name.clone(),
                message: format!("Undefined variable '{}'.", name),
            }),
        }
    }
}
