use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{AstVisitable, AstVisitor, Expr, Stmt};
use crate::interpreter::Interpreter;
use crate::token::Token;
use crate::LoxError;

#[derive(Debug, Clone, Copy, PartialEq)]
enum FunctionType {
    None,
    Function,
    Initializer,
    Method,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum ClassType {
    None,
    Class,
    Subclass,
}

#[derive(Debug)]
pub struct Resolver {
    interpreter: Rc<RefCell<Interpreter>>,
    scopes: Vec<HashMap<String, bool>>,
    current_function: FunctionType,
    current_class: ClassType,
}

impl Resolver {
    pub fn new(interpreter: Rc<RefCell<Interpreter>>) -> Self {
        Self {
            interpreter,
            scopes: Vec::new(),
            current_function: FunctionType::None,
            current_class: ClassType::None,
        }
    }

    pub fn resolve_stmt(&mut self, statements: &[Box<Stmt>]) -> Result<(), LoxError> {
        for stmt in statements {
            stmt.accept(self)?;
        }
        Ok(())
    }

    fn resolve_expr(&mut self, expr: &Expr) -> Result<(), LoxError> {
        expr.accept(self)?;
        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &Token) -> Result<(), LoxError> {
        if self.scopes.is_empty() {
            return Ok(());
        }
        let scope = self.scopes.last_mut().unwrap();
        if scope.contains_key(&name.lexeme) {
            return Err(LoxError::resolve_error(
                name,
                "Already a variable with this name in this scope.",
            ));
        }
        scope.insert(name.lexeme.clone(), false);
        Ok(())
    }

    fn define(&mut self, name: &Token) -> Result<(), LoxError> {
        if self.scopes.is_empty() {
            return Ok(());
        }
        self.scopes
            .last_mut()
            .unwrap()
            .insert(name.lexeme.clone(), true);
        Ok(())
    }

    fn resolve_local(&mut self, expr: &Expr, name: &Token) -> Result<(), LoxError> {
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
    ) -> Result<(), LoxError> {
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
    type Result = Result<(), LoxError>;

    fn visit_stmt(&mut self, stmt: &Stmt) -> Self::Result {
        match stmt {
            Stmt::Block { statements } => {
                self.begin_scope();
                self.resolve_stmt(statements)?;
                self.end_scope();
                Ok(())
            }
            Stmt::Class {
                name,
                methods,
                superclass,
            } => {
                let enclosing_class = self.current_class;
                self.current_class = ClassType::Class;

                self.declare(name)?;
                self.define(name)?;

                if let Some(superclass) = superclass {
                    let super_name = if let Expr::Variable { name } = &**superclass {
                        name.clone()
                    } else {
                        return Err(LoxError::resolve_error(
                            name,
                            "Superclass resolution expected a Variable.",
                        ));
                    };
                    if name.lexeme == super_name.lexeme {
                        return Err(LoxError::resolve_error(
                            &super_name,
                            "A class can't inherit from itself.",
                        ));
                    }

                    self.current_class = ClassType::Subclass;
                    self.resolve_expr(superclass)?;

                    self.begin_scope();
                    self.scopes
                        .last_mut()
                        .unwrap()
                        .insert("super".to_string(), true);
                }

                self.begin_scope();
                self.scopes
                    .last_mut()
                    .unwrap()
                    .insert("this".to_string(), true);

                for method in methods {
                    match *method.clone() {
                        Stmt::Function { params, body, name } => {
                            let declaration = if name.lexeme == "init" {
                                FunctionType::Initializer
                            } else {
                                FunctionType::Method
                            };

                            self.resolve_function(&params, &body, declaration)?;
                        }
                        _ => {}
                    }
                }

                self.end_scope();

                if superclass.is_some() {
                    self.end_scope();
                }

                self.current_class = enclosing_class;
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
                    return Err(LoxError::resolve_error(
                        keyword,
                        "Cannot return from top-level code.",
                    ));
                }

                if let Some(value) = value {
                    if self.current_function == FunctionType::Initializer {
                        return Err(LoxError::resolve_error(
                            keyword,
                            "Can't return a value from an initializer.",
                        ));
                    }

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
                        return Err(LoxError::resolve_error(
                            name,
                            "Can't read local variable in its own initializer.",
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
            Expr::Super { keyword, .. } => match self.current_class {
                ClassType::Subclass => self.resolve_local(expr, keyword),
                ClassType::None => Err(LoxError::resolve_error(
                    keyword,
                    "Can't use 'super' outside of a class.",
                )),
                _ => Err(LoxError::resolve_error(
                    keyword,
                    "Can't use 'super' in a class with no superclass.",
                )),
            },
            Expr::This { keyword } => {
                if self.current_class == ClassType::None {
                    return Err(LoxError::resolve_error(
                        keyword,
                        "Can't use 'this' outside of a class.",
                    ));
                }
                self.resolve_local(expr, keyword)
            }
            Expr::Unary { right, .. } => self.resolve_expr(right),
        }
    }
}
