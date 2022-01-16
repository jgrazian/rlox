use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::rc::Rc;

use crate::ast::{AstVisitable, AstVisitor, Expr, Stmt};
use crate::interpreter::Interpreter;
use crate::token::Token;
use crate::token_type::TokenType;

#[derive(Debug)]
pub struct ResolveError {
    token: Token,
    message: String,
}
impl Error for ResolveError {}

impl ResolveError {
    fn new(token: Token, message: String) -> Self {
        Self { token, message }
    }
}

impl fmt::Display for ResolveError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let location = match self.token.token_type {
            TokenType::Eof => "end".to_string(),
            _ => format!("'{}'", self.token.lexeme),
        };
        write!(
            f,
            "[line {}] Error at {} {}",
            self.token.line, location, self.message
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum FunctionType {
    None,
    Function,
    Method,
}

#[derive(Debug)]
pub struct Resolver {
    interpreter: Rc<RefCell<Interpreter>>,
    scopes: Vec<HashMap<String, bool>>,
    current_function: FunctionType,
}

impl Resolver {
    pub fn new(interpreter: Rc<RefCell<Interpreter>>) -> Self {
        Self {
            interpreter,
            scopes: Vec::new(),
            current_function: FunctionType::None,
        }
    }

    pub fn resolve_stmt(&mut self, statements: &[Box<Stmt>]) -> Result<(), ResolveError> {
        for stmt in statements {
            stmt.accept(self)?;
        }
        Ok(())
    }

    fn resolve_expr(&mut self, expr: &Expr) -> Result<(), ResolveError> {
        expr.accept(self)?;
        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &Token) -> Result<(), ResolveError> {
        if self.scopes.is_empty() {
            return Ok(());
        }
        let scope = self.scopes.last_mut().unwrap();
        if scope.contains_key(&name.lexeme) {
            return Err(ResolveError::new(
                name.clone(),
                "Already a variable with this name in this scope.".to_string(),
            ));
        }
        scope.insert(name.lexeme.clone(), false);
        Ok(())
    }

    fn define(&mut self, name: &Token) -> Result<(), ResolveError> {
        if self.scopes.is_empty() {
            return Ok(());
        }
        self.scopes
            .last_mut()
            .unwrap()
            .insert(name.lexeme.clone(), true);
        Ok(())
    }

    fn resolve_local(&mut self, expr: &Expr, name: &Token) -> Result<(), ResolveError> {
        for (i, scope) in self.scopes.iter().enumerate().rev() {
            if scope.contains_key(&name.lexeme) {
                self.interpreter
                    .borrow_mut()
                    .resolve(expr, self.scopes.len() - 1 - i);
                return Ok(());
            }
        }
        Ok(())
    }

    fn resolve_function(
        &mut self,
        params: &[Token],
        body: &[Box<Stmt>],
        function_type: FunctionType,
    ) -> Result<(), ResolveError> {
        let enclosing_function = self.current_function.clone();
        self.current_function = function_type;

        self.begin_scope();
        for param in params {
            self.declare(&param)?;
            self.define(&param)?;
        }
        self.resolve_stmt(body)?;
        self.end_scope();

        self.current_function = enclosing_function;
        Ok(())
    }
}

impl AstVisitor for Resolver {
    type Result = Result<(), ResolveError>;

    fn visit_stmt(&mut self, stmt: &Stmt) -> Self::Result {
        match stmt {
            Stmt::Block { statements } => {
                self.begin_scope();
                self.resolve_stmt(statements)?;
                self.end_scope();
                Ok(())
            }
            Stmt::Class { name, methods } => {
                self.declare(name)?;
                self.define(name)?;

                for method in methods {
                    match *method.clone() {
                        Stmt::Function { params, body, .. } => {
                            let declaration = FunctionType::Method;
                            self.resolve_function(&params, &body, declaration)?;
                        }
                        _ => {}
                    }
                }

                Ok(())
            }
            Stmt::Var { name, initializer } => {
                self.declare(name)?;
                if let Some(initializer) = initializer {
                    self.resolve_expr(initializer)?;
                }
                self.define(name)
            }
            Stmt::Function { name, params, body } => {
                self.declare(name)?;
                self.define(name)?;
                self.resolve_function(params, body, FunctionType::Function)
            }
            Stmt::Expression { expression } => self.resolve_expr(expression),
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(&[then_branch.clone()])?;
                if let Some(else_branch) = else_branch {
                    self.resolve_stmt(&[else_branch.clone()])?;
                }
                Ok(())
            }
            Stmt::Print { expression } => self.resolve_expr(expression),
            Stmt::Return { keyword, value, .. } => {
                if self.current_function == FunctionType::None {
                    return Err(ResolveError::new(
                        keyword.clone(),
                        "Cannot return from top-level code.".to_string(),
                    ));
                }

                if let Some(value) = value {
                    self.resolve_expr(value)?;
                }
                Ok(())
            }
            Stmt::While { condition, body } => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(&[body.clone()])
            }
        }
    }

    fn visit_expr(&mut self, expr: &Expr) -> Self::Result {
        match expr {
            Expr::Variable { name } => {
                if let Some(scope) = self.scopes.last() {
                    if scope.get(&name.lexeme) == Some(&false) {
                        return Err(ResolveError::new(
                            name.clone(),
                            "Can't read local variable in its own initializer.".to_string(),
                        ));
                    }
                }

                self.resolve_local(expr, name)?;
                Ok(())
            }
            Expr::Assign { name, value } => {
                self.resolve_expr(value)?;
                self.resolve_local(expr, name)?;
                Ok(())
            }
            Expr::Binary { left, right, .. } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)
            }
            Expr::Call {
                callee, arguments, ..
            } => {
                self.resolve_expr(callee)?;
                for arg in arguments {
                    self.resolve_expr(arg)?;
                }
                Ok(())
            }
            Expr::Get { object, .. } => self.resolve_expr(object),
            Expr::Grouping { expression } => self.resolve_expr(expression),
            Expr::Literal { .. } => Ok(()),
            Expr::Logical { left, right, .. } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)
            }
            Expr::Set { object, value, .. } => {
                self.resolve_expr(value)?;
                self.resolve_expr(object)
            }
            Expr::Unary { right, .. } => self.resolve_expr(right),
        }
    }
}
