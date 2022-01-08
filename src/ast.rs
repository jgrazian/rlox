use std::fmt::Display;

use crate::token::Token;

use rlox_lib::{impl_new_methods, impl_visitor_methods, make_visitor_methods};

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Number(n) => write!(f, "{}", n),
            Literal::String(s) => write!(f, "{}", s),
            Literal::Boolean(b) => write!(f, "{}", b),
            Literal::Nil => write!(f, "nil"),
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
        Grouping {
            expression: Box<Expr>
        },
        Literal {
            value: Literal
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
        Print {
            expression: Box<Expr>
        },
        Var {
            name: Token,
            initializer: Option<Box<Expr>>
        }
    }
);
