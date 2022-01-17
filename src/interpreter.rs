use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::rc::Rc;

use crate::ast::{AstVisitable, AstVisitor, Expr, LoxObject, Stmt};
use crate::callable::{Callable, Clock, LoxCallable, LoxClass, LoxFunction};
use crate::enviroment::Enviroment;
use crate::token::Token;
use crate::token_type::TokenType;

#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub message: String,
    pub token: Token,
}
impl Error for RuntimeError {}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}\n[line {}]", self.message, self.token.line)
    }
}

impl RuntimeError {
    pub fn new(token: &Token, message: &str) -> Self {
        Self {
            token: token.clone(),
            message: message.to_string(),
        }
    }
}

#[derive(Debug)]
pub struct Interpreter {
    pub globals: Rc<RefCell<Enviroment>>,
    locals: HashMap<Expr, usize>,
    pub enviroment: Rc<RefCell<Enviroment>>,
}

impl AstVisitor for Interpreter {
    type Result = Result<LoxObject, RuntimeError>;

    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<LoxObject, RuntimeError> {
        match &stmt {
            Stmt::Expression { expression } => self.evaluate(expression),
            Stmt::Function { name, .. } => {
                let func = Rc::new(LoxFunction::new(
                    Box::new(stmt.clone()),
                    self.enviroment.clone(),
                    false,
                ));
                self.enviroment.borrow_mut().define(
                    &name.lexeme,
                    LoxObject::Callable(LoxCallable::Function(func)),
                );
                Ok(LoxObject::Nil)
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition = &self.evaluate(condition)?;
                if self.is_truthy(condition) {
                    match self.execute(then_branch)? {
                        LoxObject::Return(o) => return Ok(LoxObject::Return(o)),
                        _ => (),
                    }
                } else if let Some(else_branch) = else_branch {
                    match self.execute(else_branch)? {
                        LoxObject::Return(o) => return Ok(LoxObject::Return(o)),
                        _ => (),
                    }
                }
                Ok(LoxObject::Nil)
            }
            Stmt::Print { expression } => {
                let value = self.evaluate(expression)?;
                println!("{}", self.stringify(&value));
                Ok(LoxObject::Nil)
            }
            Stmt::Return { value, .. } => {
                if let Some(value) = value {
                    Ok(LoxObject::Return(Box::new(self.evaluate(value)?)))
                } else {
                    Ok(LoxObject::Nil)
                }
            }
            Stmt::While { condition, body } => {
                let mut _condition = self.evaluate(condition)?;

                while self.is_truthy(&_condition) {
                    match self.execute(body)? {
                        LoxObject::Return(o) => return Ok(LoxObject::Return(o)),
                        _ => (),
                    }
                    _condition = self.evaluate(condition)?;
                }
                Ok(LoxObject::Nil)
            }
            Stmt::Var { name, initializer } => {
                let value = match initializer {
                    Some(expr) => self.evaluate(expr)?,
                    None => LoxObject::Nil,
                };
                self.enviroment.borrow_mut().define(&name.lexeme, value);
                Ok(LoxObject::Nil)
            }
            Stmt::Block { statements } => {
                match self
                    .execute_block(statements, Enviroment::enclosed(self.enviroment.clone()))?
                {
                    LoxObject::Return(o) => Ok(LoxObject::Return(o)),
                    _ => Ok(LoxObject::Nil),
                }
            }
            Stmt::Class {
                name,
                methods,
                superclass,
            } => {
                let superclass = if let Some(superclass) = superclass {
                    match self.evaluate(superclass)? {
                        LoxObject::Callable(callable) => match callable {
                            LoxCallable::Class(c) => Some(c),
                            _ => return Err(RuntimeError::new(name, "Superclass must be a class")),
                        },
                        _ => return Err(RuntimeError::new(name, "Superclass must be a class.")),
                    }
                } else {
                    None
                };

                self.enviroment
                    .borrow_mut()
                    .define(&name.lexeme, LoxObject::Nil);

                if let Some(superclass) = &superclass {
                    self.enviroment =
                        Rc::new(RefCell::new(Enviroment::enclosed(self.enviroment.clone())));
                    self.enviroment.borrow_mut().define(
                        "super",
                        LoxObject::Callable(LoxCallable::Class(superclass.clone())),
                    );
                }

                let mut _methods = HashMap::new();
                for method in methods {
                    let method_name = if let Stmt::Function { name, .. } = *method.clone() {
                        name.lexeme.clone()
                    } else {
                        continue;
                    };
                    let function = Rc::new(LoxFunction::new(
                        method.clone(),
                        self.enviroment.clone(),
                        &method_name == "init",
                    ));
                    _methods.insert(method_name, function);
                }

                let klass = LoxObject::Callable(LoxCallable::Class(Rc::new(LoxClass::new(
                    &name.lexeme,
                    _methods,
                    superclass.clone(),
                ))));

                if superclass.is_some() {
                    let enclosing = self.enviroment.borrow().enclosing.clone().unwrap();
                    self.enviroment = enclosing;
                }

                self.enviroment.borrow_mut().assign(name, klass)?;
                Ok(LoxObject::Nil)
            }
        }
    }

    fn visit_expr(&mut self, expr: &Expr) -> Result<LoxObject, RuntimeError> {
        match expr {
            Expr::Literal { value } => Ok(value.clone()),
            Expr::Logical {
                left,
                operator,
                right,
            } => {
                let left = self.evaluate(left)?;

                if operator.token_type == TokenType::Or {
                    if self.is_truthy(&left) {
                        return Ok(left);
                    }
                } else {
                    if !self.is_truthy(&left) {
                        return Ok(left);
                    }
                }
                self.evaluate(right)
            }
            Expr::Set {
                name,
                object,
                value,
            } => {
                let object = self.evaluate(object)?;

                if let LoxObject::Instance(mut instance) = object {
                    let value = self.evaluate(value)?;
                    instance.set(&name, value.clone());
                    Ok(value)
                } else {
                    Err(RuntimeError::new(name, "Only instances have fields"))
                }
            }
            Expr::Super { method, .. } => {
                let distance = self.locals.get(expr).unwrap();
                let superclass = self
                    .enviroment
                    .borrow()
                    .get_at(*distance, "super".to_string())?;
                let object = self
                    .enviroment
                    .borrow()
                    .get_at(*distance - 1, "this".to_string())?;

                let _method = match superclass {
                    LoxObject::Callable(callable) => match callable {
                        LoxCallable::Class(c) => c.find_method(&method.lexeme),
                        _ => panic!("Super"),
                    },
                    _ => panic!("Super"),
                };

                match _method {
                    Some(method) => Ok(LoxObject::Callable(LoxCallable::Function(Rc::new(
                        method.bind(object),
                    )))),
                    None => Err(RuntimeError::new(
                        method,
                        &format!("Undefined property '{}'.", method.lexeme),
                    )),
                }
            }
            Expr::This { keyword } => self.look_up_variable(keyword, expr),
            Expr::Grouping { expression } => Ok(self.evaluate(expression)?),
            Expr::Unary { operator, right } => {
                let right = self.evaluate(right)?;

                match operator.token_type {
                    TokenType::Minus => Ok(LoxObject::Number(-number(operator, &right)?)),
                    TokenType::Bang => return Ok(LoxObject::Boolean(!self.is_truthy(&right))),
                    _ => unimplemented!(),
                }
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left = self.evaluate(left)?;
                let right = self.evaluate(right)?;

                match operator.token_type {
                    TokenType::Greater => Ok(LoxObject::Boolean(
                        number(operator, &left)? > number(operator, &right)?,
                    )),
                    TokenType::GreaterEqual => Ok(LoxObject::Boolean(
                        number(operator, &left)? >= number(operator, &right)?,
                    )),
                    TokenType::Less => Ok(LoxObject::Boolean(
                        number(operator, &left)? < number(operator, &right)?,
                    )),
                    TokenType::LessEqual => Ok(LoxObject::Boolean(
                        number(operator, &left)? <= number(operator, &right)?,
                    )),
                    TokenType::BangEqual => return Ok(LoxObject::Boolean(left != right)),
                    TokenType::EqualEqual => return Ok(LoxObject::Boolean(left == right)),
                    TokenType::Minus => Ok(LoxObject::Number(
                        number(operator, &left)? - number(operator, &right)?,
                    )),
                    TokenType::Plus => match (&left, &right) {
                        (LoxObject::String(left), LoxObject::String(right)) => {
                            Ok(LoxObject::String(left.to_string() + &right))
                        }
                        (LoxObject::Number(left), LoxObject::Number(right)) => {
                            Ok(LoxObject::Number(left + right))
                        }
                        _ => Err(RuntimeError::new(
                            operator,
                            &format!(
                                "Expected two numbers or two strings found {:?} {:?}",
                                left, right
                            ),
                        )),
                    },
                    TokenType::Slash => Ok(LoxObject::Number(
                        number(operator, &left)? / number(operator, &right)?,
                    )),
                    TokenType::Star => Ok(LoxObject::Number(
                        number(operator, &left)? * number(operator, &right)?,
                    )),
                    _ => unimplemented!(),
                }
            }
            Expr::Call {
                callee,
                paren,
                arguments,
            } => {
                let callee = self.evaluate(callee)?;

                let mut args = Vec::new();
                for arg in arguments {
                    args.push(self.evaluate(arg)?);
                }

                let function = match callee {
                    LoxObject::Callable(callable) => callable,
                    _ => {
                        return Err(RuntimeError::new(
                            paren,
                            &format!("Can't call non-callable {:?}", callee),
                        ))
                    }
                };
                if function.arity() != args.len() {
                    return Err(RuntimeError::new(
                        paren,
                        &format!(
                            "Expected {} arguments found {}",
                            function.arity(),
                            args.len()
                        ),
                    ));
                }
                let res = function.call(self, args);
                res
            }
            Expr::Get { object, name } => {
                let object = self.evaluate(object)?;
                match object {
                    LoxObject::Instance(instance) => instance.get(&name),
                    _ => Err(RuntimeError::new(
                        name,
                        &format!("Can't get {} on {:?}", name.lexeme, object),
                    )),
                }
            }
            Expr::Variable { name } => self.look_up_variable(name, expr),
            Expr::Assign { name, value } => {
                let value = self.evaluate(value)?;

                if let Some(distance) = self.locals.get(expr) {
                    self.enviroment
                        .borrow_mut()
                        .assign_at(*distance, &name, value.clone())?;
                } else {
                    self.globals.borrow_mut().assign(&name, value.clone())?;
                }

                Ok(value)
            }
        }
    }
}

impl Interpreter {
    pub fn new() -> Self {
        let globals = Rc::new(RefCell::new(Enviroment::new()));
        globals.borrow_mut().define(
            "clock",
            LoxObject::Callable(LoxCallable::Function(Rc::new(Clock {}))),
        );

        Self {
            globals: globals.clone(),
            locals: HashMap::new(),
            enviroment: globals.clone(),
        }
    }

    pub fn interpret(&mut self, statements: &Vec<Box<Stmt>>) -> Result<(), RuntimeError> {
        for stmt in statements {
            if let Err(e) = self.execute(stmt) {
                return Err(e);
            }
        }
        Ok(())
    }

    fn stringify(&self, value: &LoxObject) -> String {
        value.to_string()
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<LoxObject, RuntimeError> {
        expr.accept(self)
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<LoxObject, RuntimeError> {
        stmt.accept(self)
    }

    pub fn resolve(&mut self, expr: &Expr, depth: usize) {
        self.locals.insert(expr.clone(), depth);
    }

    fn look_up_variable(&self, name: &Token, expr: &Expr) -> Result<LoxObject, RuntimeError> {
        if let Some(depth) = self.locals.get(expr) {
            let env = self.enviroment.borrow();
            let value = env.get_at(*depth, name.lexeme.clone())?;
            Ok(value)
        } else {
            self.globals.borrow().get(&name)
        }
    }

    pub fn execute_block(
        &mut self,
        statements: &[Box<Stmt>],
        enviroment: Enviroment,
    ) -> Result<LoxObject, RuntimeError> {
        let previous = self.enviroment.clone();
        self.enviroment = Rc::new(RefCell::new(enviroment));

        for stmt in statements {
            match self.execute(stmt)? {
                LoxObject::Return(o) => {
                    self.enviroment = previous;
                    return Ok(LoxObject::Return(o));
                }
                _ => (),
            }
        }

        self.enviroment = previous;
        Ok(LoxObject::Nil)
    }

    fn is_truthy(&self, value: &LoxObject) -> bool {
        match value {
            LoxObject::Nil => false,
            LoxObject::Boolean(value) => *value,
            _ => true,
        }
    }
}

fn number(operator: &Token, literal: &LoxObject) -> Result<f64, RuntimeError> {
    match literal {
        LoxObject::Number(value) => Ok(*value),
        _ => Err(RuntimeError::new(
            operator,
            &format!("Expected number found {:?}", literal),
        )),
    }
}
