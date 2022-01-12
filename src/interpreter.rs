use std::cell::RefCell;
use std::error::Error;
use std::fmt;
use std::rc::Rc;

use crate::ast::{AstVisitable, AstVisitor, Expr, Literal, Stmt};
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

pub struct Interpreter {
    enviroment: Rc<RefCell<Enviroment>>,
}

impl AstVisitor for Interpreter {
    type Result = Result<Literal, RuntimeError>;

    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<Literal, RuntimeError> {
        match stmt {
            Stmt::Expression { expression } => self.evaluate(expression),
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition = &self.evaluate(condition)?;
                if self.is_truthy(condition) {
                    self.execute(then_branch)?;
                } else if let Some(else_branch) = else_branch {
                    self.execute(else_branch)?;
                }
                Ok(Literal::Nil)
            }
            Stmt::Print { expression } => {
                let value = self.evaluate(expression)?;
                println!("{}", self.stringify(&value));
                Ok(Literal::Nil)
            }
            Stmt::While { condition, body } => {
                let mut _condition = self.evaluate(condition)?;

                while self.is_truthy(&_condition) {
                    self.execute(body)?;
                    _condition = self.evaluate(condition)?;
                }
                Ok(Literal::Nil)
            }
            Stmt::Var { name, initializer } => {
                let value = match initializer {
                    Some(expr) => self.evaluate(expr)?,
                    None => Literal::Nil,
                };
                self.enviroment.borrow_mut().define(&name.lexeme, value);
                Ok(Literal::Nil)
            }
            Stmt::Block { statements } => {
                self.execute_block(statements, Enviroment::enclosed(self.enviroment.clone()))?;
                Ok(Literal::Nil)
            }
        }
    }

    fn visit_expr(&mut self, expr: &Expr) -> Result<Literal, RuntimeError> {
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
            Expr::Grouping { expression } => Ok(self.evaluate(expression)?),
            Expr::Unary { operator, right } => {
                let right = self.evaluate(right)?;

                match operator.token_type {
                    TokenType::Minus => Ok(Literal::Number(-number(operator, &right)?)),
                    TokenType::Bang => return Ok(Literal::Boolean(!self.is_truthy(&right))),
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
                    TokenType::Greater => Ok(Literal::Boolean(
                        number(operator, &left)? > number(operator, &right)?,
                    )),
                    TokenType::GreaterEqual => Ok(Literal::Boolean(
                        number(operator, &left)? >= number(operator, &right)?,
                    )),
                    TokenType::Less => Ok(Literal::Boolean(
                        number(operator, &left)? < number(operator, &right)?,
                    )),
                    TokenType::LessEqual => Ok(Literal::Boolean(
                        number(operator, &left)? <= number(operator, &right)?,
                    )),
                    TokenType::BangEqual => return Ok(Literal::Boolean(left != right)),
                    TokenType::EqualEqual => return Ok(Literal::Boolean(left == right)),
                    TokenType::Minus => Ok(Literal::Number(
                        number(operator, &left)? - number(operator, &right)?,
                    )),
                    TokenType::Plus => match (&left, &right) {
                        (Literal::String(left), Literal::String(right)) => {
                            Ok(Literal::String(left.to_string() + &right))
                        }
                        (Literal::Number(left), Literal::Number(right)) => {
                            Ok(Literal::Number(left + right))
                        }
                        _ => Err(RuntimeError {
                            message: format!(
                                "Expected two numbers or two strings found {:?} {:?}",
                                left, right
                            ),
                            token: operator.clone(),
                        }),
                    },
                    TokenType::Slash => Ok(Literal::Number(
                        number(operator, &left)? / number(operator, &right)?,
                    )),
                    TokenType::Star => Ok(Literal::Number(
                        number(operator, &left)? * number(operator, &right)?,
                    )),
                    _ => unimplemented!(),
                }
            }
            Expr::Variable { name } => self.enviroment.borrow().get(name),
            Expr::Assign { name, value } => {
                let value = self.evaluate(value)?;
                self.enviroment.borrow_mut().assign(&name, value.clone())?;
                Ok(value)
            }
        }
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            enviroment: Rc::new(RefCell::new(Enviroment::new())),
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

    fn stringify(&self, value: &Literal) -> String {
        match value {
            Literal::String(s) => s.clone(),
            Literal::Number(n) => n.to_string(),
            Literal::Boolean(b) => b.to_string(),
            Literal::Nil => "nil".to_string(),
        }
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<Literal, RuntimeError> {
        expr.accept(self)
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<Literal, RuntimeError> {
        stmt.accept(self)
    }

    fn execute_block(
        &mut self,
        statements: &[Box<Stmt>],
        enviroment: Enviroment,
    ) -> Result<(), RuntimeError> {
        let previous = self.enviroment.clone();
        self.enviroment = Rc::new(RefCell::new(enviroment));

        for stmt in statements {
            if let Err(e) = self.execute(stmt) {
                self.enviroment = previous;
                return Err(e);
            }
        }

        self.enviroment = previous;
        Ok(())
    }

    fn is_truthy(&self, value: &Literal) -> bool {
        match value {
            Literal::Nil => false,
            Literal::Boolean(value) => *value,
            _ => true,
        }
    }
}

fn number(operator: &Token, literal: &Literal) -> Result<f64, RuntimeError> {
    match literal {
        Literal::Number(value) => Ok(*value),
        _ => Err(RuntimeError {
            message: format!("Expected number found {:?}", literal),
            token: operator.clone(),
        }),
    }
}
