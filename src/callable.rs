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

pub trait Callable: Debug + Display {
    fn arity(&self) -> usize;

    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<LoxObject>,
    ) -> Result<LoxObject, RuntimeError>;
}

#[derive(Debug, Clone)]
pub enum LoxCallable {
    Function(Rc<dyn Callable>),
    Class(Rc<LoxClass>),
}

impl Display for LoxCallable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function(fun) => write!(f, "{}", fun),
            Self::Class(class) => write!(f, "{}", class),
        }
    }
}

impl Callable for LoxCallable {
    fn arity(&self) -> usize {
        match self {
            Self::Function(f) => f.arity(),
            Self::Class(c) => c.arity(),
        }
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<LoxObject>,
    ) -> Result<LoxObject, RuntimeError> {
        match self {
            Self::Function(f) => f.call(interpreter, arguments),
            Self::Class(c) => c.call(interpreter, arguments),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LoxFunction {
    declaration: Box<Stmt>,
    closure: Rc<RefCell<Enviroment>>,
    is_initializer: bool,
}

impl LoxFunction {
    pub fn new(
        declaration: Box<Stmt>,
        closure: Rc<RefCell<Enviroment>>,
        is_initializer: bool,
    ) -> Self {
        Self {
            declaration,
            closure,
            is_initializer,
        }
    }

    pub fn bind(&self, instance: LoxObject) -> LoxFunction {
        let mut enviroment = Enviroment::enclosed(self.closure.clone());
        enviroment.define("this", instance);
        Self {
            declaration: self.declaration.clone(),
            closure: Rc::new(RefCell::new(enviroment)),
            is_initializer: self.is_initializer,
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

impl Callable for LoxFunction {
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

            match (self.is_initializer, interpreter.execute_block(&body, env)?) {
                (true, _) => Ok(LoxObject::Return(Box::new(
                    self.closure.borrow().get_at(0, "this".to_string())?,
                ))),
                (_, LoxObject::Return(o)) => Ok(*o),
                _ => Ok(LoxObject::Nil),
            }
        } else {
            panic!("Not a function declaration");
        }
    }
}

#[derive(Debug, Clone)]
pub struct LoxClass {
    name: String,
    methods: Rc<RefCell<HashMap<String, Rc<LoxFunction>>>>,
    superclass: Option<Rc<LoxClass>>,
    instance_count: Rc<RefCell<usize>>,
}

impl LoxClass {
    pub fn new(
        name: &str,
        methods: HashMap<String, Rc<LoxFunction>>,
        superclass: Option<Rc<LoxClass>>,
    ) -> Self {
        Self {
            name: name.to_string(),
            methods: Rc::new(RefCell::new(methods)),
            superclass,
            instance_count: Rc::new(RefCell::new(0)),
        }
    }

    pub fn find_method(&self, name: &str) -> Option<Rc<LoxFunction>> {
        match self.methods.borrow().get(name) {
            Some(f) => Some(f.clone()),
            None => match &self.superclass {
                Some(superclass) => superclass.find_method(name),
                _ => None,
            },
        }
    }
}

impl Display for LoxClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Hash for LoxClass {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl Callable for LoxClass {
    fn arity(&self) -> usize {
        if let Some(initializer) = self.find_method("init") {
            initializer.arity()
        } else {
            0
        }
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<LoxObject>,
    ) -> Result<LoxObject, RuntimeError> {
        *self.instance_count.borrow_mut() += 1;

        let instance = LoxInstance::new(self.clone(), *self.instance_count.borrow());
        if let Some(initializer) = self.find_method("init") {
            initializer
                .bind(LoxObject::Instance(instance.clone()))
                .call(interpreter, arguments)?;
        }
        Ok(LoxObject::Instance(instance))
    }
}

#[derive(Debug, Clone)]
pub struct LoxInstance {
    klass: LoxClass,
    fields: Rc<RefCell<HashMap<String, LoxObject>>>,
    id: usize,
}

impl LoxInstance {
    fn new(klass: LoxClass, instance_number: usize) -> Self {
        Self {
            klass,
            fields: Rc::new(RefCell::new(HashMap::new())),
            id: instance_number,
        }
    }

    pub fn get(&self, name: &Token) -> Result<LoxObject, RuntimeError> {
        match self.fields.borrow().get(&name.lexeme) {
            Some(o) => Ok(o.clone()),
            None => {
                if let Some(method) = self.klass.find_method(&name.lexeme) {
                    let fun = method.bind(LoxObject::Instance(self.clone()));
                    Ok(LoxObject::Callable(LoxCallable::Function(Rc::new(fun))))
                } else {
                    Err(RuntimeError::new(
                        name,
                        &format!(
                            "Instance of {} has no field {}",
                            self.klass.name, name.lexeme
                        ),
                    ))
                }
            }
        }
    }

    pub fn set(&mut self, name: &Token, value: LoxObject) {
        self.fields.borrow_mut().insert(name.lexeme.clone(), value);
    }
}

impl Display for LoxInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} instance id {}", self.klass.name, self.id)
    }
}

impl Hash for LoxInstance {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.klass.name.clone(), self.id).hash(state);
    }
}

#[derive(Debug)]
pub struct Clock;

impl Callable for Clock {
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
