use std::fmt::Display;
use std::hash::{Hash, Hasher};

use crate::callable::{LoxCallable, LoxInstance};

use crate::token::Token;
use crate::token_type::integer_decode;

use rlox_lib::{impl_new_methods, impl_visitor_methods, make_visitor_methods};

#[derive(Debug)]
pub enum LoxObject {
    Number(f64),
    String(String),
    Boolean(bool),
    Callable(LoxCallable),
    Instance(LoxInstance),
    Return(Box<LoxObject>),
    Nil,
}

impl Display for LoxObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxObject::Number(n) => write!(f, "{}", n),
            LoxObject::String(s) => write!(f, "{}", s),
            LoxObject::Boolean(b) => write!(f, "{}", b),
            LoxObject::Callable(c) => write!(f, "{}", c),
            LoxObject::Instance(i) => write!(f, "{}", i),
            LoxObject::Return(o) => write!(f, "return {}", o),
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
            LoxObject::Instance(i) => LoxObject::Instance(i.clone()),
            LoxObject::Return(o) => LoxObject::Return(o.clone()),
            LoxObject::Nil => LoxObject::Nil,
        }
    }
}

impl Hash for LoxObject {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            LoxObject::Number(n) => integer_decode(*n).hash(state),
            LoxObject::String(s) => s.hash(state),
            LoxObject::Boolean(b) => b.hash(state),
            LoxObject::Callable(c) => c.to_string().hash(state),
            LoxObject::Instance(i) => i.hash(state),
            LoxObject::Return(o) => o.hash(state),
            LoxObject::Nil => 0.hash(state),
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
            (LoxObject::Return(o1), LoxObject::Return(o2)) => o1 == o2,
            (LoxObject::Nil, LoxObject::Nil) => true,
            _ => false,
        }
    }
}
impl Eq for LoxObject {}

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
        #[derive(Debug, Clone, Hash, PartialEq, Eq)]
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
        Get {
            object: Box<Expr>,
            name: Token
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
        Set {
            object: Box<Expr>,
            name: Token,
            value: Box<Expr>
        },
        This {
            keyword: Token
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
        Class {
            name: Token,
            methods: Vec<Box<Stmt>>,
            superclass: Option<Box<Expr>>
        },
        Expression {
            expression: Box<Expr>
        },
        Function {
            name: Token,
            params: Vec<Token>,
            body: Vec<Box<Stmt>>
        },
        If {
            condition: Box<Expr>,
            then_branch: Box<Stmt>,
            else_branch: Option<Box<Stmt>>
        },
        Print {
            expression: Box<Expr>
        },
        Return {
            keyword: Token,
            value: Option<Box<Expr>>
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
