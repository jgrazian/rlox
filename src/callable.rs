use std::cell::RefCell;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::rc::Rc;

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
    closure: Rc<RefCell<Enviroment>>,
}

impl LoxFunction {
    pub fn new(declaration: Box<Stmt>, closure: Rc<RefCell<Enviroment>>) -> Self {
        Self {
            declaration,
            closure,
        }
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
        let mut env = Enviroment::enclosed(self.closure.clone());

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

#[derive(Debug, Clone, Hash)]
struct LoxClassInner {
    name: String,
}

#[derive(Debug, Clone, Hash)]
pub struct LoxClass {
    inner: Rc<LoxClassInner>,
}

impl LoxClass {
    pub fn new(name: String) -> Self {
        Self {
            inner: Rc::new(LoxClassInner { name }),
        }
    }
}

impl Display for LoxClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inner.name)
    }
}

impl LoxCallable for LoxClass {
    fn arity(&self) -> usize {
        0
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<LoxObject>,
    ) -> Result<LoxObject, RuntimeError> {
        let instance = LoxInstance::new(self.inner.clone());
        Ok(LoxObject::Instance(instance))
    }
}

#[derive(Debug, Clone, Hash)]
pub struct LoxInstance {
    klass: Rc<LoxClassInner>,
}

impl LoxInstance {
    fn new(klass: Rc<LoxClassInner>) -> Self {
        Self { klass }
    }
}

impl Display for LoxInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} instance", self.klass.name)
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
