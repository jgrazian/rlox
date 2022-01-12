use std::fmt::Display;
use std::rc::Rc;

use crate::callable::LoxCallable;
use crate::token::Token;

use rlox_lib::{impl_new_methods, impl_visitor_methods, make_visitor_methods};

#[derive(Debug)]
pub enum LoxObject {
    Number(f64),
    String(String),
    Boolean(bool),
    Callable(Rc<Box<dyn LoxCallable>>),
    Nil,
}

impl Display for LoxObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxObject::Number(n) => write!(f, "{}", n),
            LoxObject::String(s) => write!(f, "{}", s),
            LoxObject::Boolean(b) => write!(f, "{}", b),
            LoxObject::Callable(_) => write!(f, "callable"),
            LoxObject::Nil => write!(f, "nil"),
        }
    }
}

impl Clone for LoxObject {
    fn clone(&self) -> Self {
        match self {
            LoxObject::Number(n) => LoxObject::Number(*n),
            LoxObject::String(s) => LoxObject::String(s.clone()),
            LoxObject::Boolean(b) => LoxObject::Boolean(*b),
            LoxObject::Callable(c) => LoxObject::Callable(c.clone()),
            LoxObject::Nil => LoxObject::Nil,
        }
    }
}

impl PartialEq for LoxObject {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LoxObject::Number(n1), LoxObject::Number(n2)) => n1 == n2,
            (LoxObject::String(s1), LoxObject::String(s2)) => s1 == s2,
            (LoxObject::Boolean(b1), LoxObject::Boolean(b2)) => b1 == b2,
            (LoxObject::Callable(_), LoxObject::Callable(_)) => false,
            (LoxObject::Nil, LoxObject::Nil) => true,
            _ => false,
        }
    }
}

impl LoxObject {
    fn is_callable(&self) -> bool {
        match self {
            LoxObject::Callable(_) => true,
            _ => false,
        }
    }
}

macro_rules! define_ast {
    (
        $($base_name:ident {
            $($class_name:ident {
                $($field_name:ident: $field_type:ty),*
            }),*
        }),*
    ) => {

        pub trait AstVisitor {
            type Result;

            $(
                make_visitor_methods!($base_name);
            )*
        }

        pub trait AstVisitable {
            fn accept<V: AstVisitor>(&self, visitor: &mut V) -> V::Result;
        }

        $(
        #[allow(dead_code)]
        #[derive(Debug, Clone)]
        pub enum $base_name {
            $($class_name {
                $($field_name: $field_type),*
            }),*
        }

        impl $base_name {
            $(
                impl_new_methods!($class_name$($field_name$field_type)*);
            )*
        }

        impl AstVisitable for $base_name {
            fn accept<V: AstVisitor>(&self, visitor: &mut V) -> V::Result {
                impl_visitor_methods!($base_name)
            }
        }
        )*

    };
}

define_ast!(
    Expr {
        Assign {
            name: Token,
            value: Box<Expr>
        },
        Binary {
            left: Box<Expr>,
            operator: Token,
            right: Box<Expr>
        },
        Call {
            callee: Box<Expr>,
            paren: Token,
            arguments: Vec<Box<Expr>>
        },
        Grouping {
            expression: Box<Expr>
        },
        Literal {
            value: LoxObject
        },
        Logical {
            left: Box<Expr>,
            operator: Token,
            right: Box<Expr>
        },
        Unary {
            operator: Token,
            right: Box<Expr>
        },
        Variable {
            name: Token
        }
    },
    Stmt {
        Block {
            statements: Vec<Box<Stmt>>
        },
        Expression {
            expression: Box<Expr>
        },
        If {
            condition: Box<Expr>,
            then_branch: Box<Stmt>,
            else_branch: Option<Box<Stmt>>
        },
        Print {
            expression: Box<Expr>
        },
        Var {
            name: Token,
            initializer: Option<Box<Expr>>
        },
        While {
            condition: Box<Expr>,
            body: Box<Stmt>
        }
    }
);
