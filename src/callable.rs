use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use crate::ast::LoxObject;
use crate::ast::Stmt;
use crate::enviroment::Enviroment;
use crate::interpreter::{Interpreter, RuntimeError};
use crate::token::Token;

pub trait LoxCallable: Debug + Display {
    fn arity(&self) -> usize;

    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<LoxObject>,
    ) -> Result<LoxObject, RuntimeError>;
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
struct LoxClassInner {
    name: String,
    _instance_count: usize,
    methods: HashMap<String, Rc<RefCell<LoxFunction>>>,
}

impl LoxClassInner {
    fn find_method(&self, name: &str) -> Option<Rc<RefCell<LoxFunction>>> {
        self.methods.get(name).cloned()
    }
}

impl Hash for LoxClassInner {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

#[derive(Debug, Clone)]
pub struct LoxClass {
    inner: Rc<RefCell<LoxClassInner>>,
}

impl LoxClass {
    pub fn new(name: String, methods: HashMap<String, Rc<RefCell<LoxFunction>>>) -> Self {
        Self {
            inner: Rc::new(RefCell::new(LoxClassInner {
                name,
                _instance_count: 0,
                methods,
            })),
        }
    }
}

impl Display for LoxClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inner.borrow().name)
    }
}

impl Hash for LoxClass {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.inner.borrow().hash(state);
    }
}

impl LoxCallable for LoxClass {
    fn arity(&self) -> usize {
        0
    }

    fn call(
        &self,
        _interpreter: &mut Interpreter,
        _arguments: Vec<LoxObject>,
    ) -> Result<LoxObject, RuntimeError> {
        self.inner.borrow_mut()._instance_count += 1;
        let instance = LoxInstance::new(self.inner.clone(), self.inner.borrow()._instance_count);
        Ok(LoxObject::Instance(instance))
    }
}

#[derive(Debug, Clone)]
pub struct LoxInstance {
    klass: Rc<RefCell<LoxClassInner>>,
    fields: HashMap<String, LoxObject>,
    _hash_id: usize,
}

impl LoxInstance {
    fn new(klass: Rc<RefCell<LoxClassInner>>, instance_number: usize) -> Self {
        Self {
            klass,
            fields: HashMap::new(),
            _hash_id: instance_number,
        }
    }

    pub fn get(&self, name: &Token) -> Result<LoxObject, RuntimeError> {
        match self.fields.get(&name.lexeme) {
            Some(o) => Ok(o.clone()),
            None => {
                if let Some(method) = self.klass.borrow().find_method(&name.lexeme) {
                    Ok(LoxObject::Callable(method.clone()))
                } else {
                    Err(RuntimeError {
                        token: name.clone(),
                        message: format!(
                            "Instance of {} has no field {}",
                            self.klass.borrow().name,
                            name.lexeme
                        ),
                    })
                }
            }
        }
    }

    pub fn set(&mut self, name: &Token, value: LoxObject) {
        self.fields.insert(name.lexeme.clone(), value);
    }
}

impl Display for LoxInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} instance", self.klass.borrow().name)
    }
}

impl Hash for LoxInstance {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.klass.borrow().name.clone(), self._hash_id).hash(state);
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
