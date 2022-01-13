use std::fmt::Debug;
use std::fmt::Display;

use crate::ast::LoxObject;
use crate::ast::Stmt;
use crate::enviroment::Enviroment;
use crate::interpreter::{Interpreter, RuntimeError};

pub trait LoxCallable: Debug + Display {
    fn arity(&self) -> usize;

    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<LoxObject>,
    ) -> Result<LoxObject, RuntimeError>;
}

#[derive(Debug)]
pub struct LoxFunction {
    declaration: Box<Stmt>,
}

impl LoxFunction {
    pub fn new(declaration: Box<Stmt>) -> Self {
        Self { declaration }
    }
}

impl Display for LoxFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Stmt::Function { name, .. } = &*self.declaration {
            write!(f, "<fn {}>", name)
        } else {
            panic!("Not a function declaration");
        }
    }
}

impl LoxCallable for LoxFunction {
    fn arity(&self) -> usize {
        if let Stmt::Function { params, .. } = &*self.declaration {
            params.len()
        } else {
            panic!("Not a function declaration");
        }
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<LoxObject>,
    ) -> Result<LoxObject, RuntimeError> {
        let mut env = Enviroment::enclosed(interpreter.globals.clone());

        if let Stmt::Function { params, body, .. } = &*self.declaration {
            for (param, argument) in params.iter().zip(arguments) {
                env.define(&param.lexeme, argument);
            }

            match interpreter.execute_block(&body, env)? {
                LoxObject::Return(o) => Ok(*o),
                _ => Ok(LoxObject::Nil),
            }
        } else {
            panic!("Not a function declaration");
        }
    }
}

#[derive(Debug)]
pub struct Clock;

impl LoxCallable for Clock {
    fn arity(&self) -> usize {
        0
    }

    fn call(
        &self,
        _interpreter: &mut Interpreter,
        _arguments: Vec<LoxObject>,
    ) -> Result<LoxObject, RuntimeError> {
        Ok(LoxObject::Number(
            std::time::SystemTime::now()
                .duration_since(std::time::SystemTime::UNIX_EPOCH)
                .unwrap()
                .as_secs_f64(),
        ))
    }
}

impl Display for Clock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn clock>")
    }
}
