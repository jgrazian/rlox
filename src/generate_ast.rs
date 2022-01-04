use crate::token::Token;

macro_rules! define_ast {
    ($base_name:ident, [$($class_name:ident => ($($field_name:ident : $field_type:ty),*)),*]) => {
        pub trait $base_name {}

        $(
            define_ast!($class_name, $($field_name, $field_type),*);
            impl $base_name for $class_name {}
        )*
    };

    ($class_name:ident, $($field_name:ident, $field_type:ty),*) => {
        pub struct $class_name {
            $($field_name: $field_type),*
        }

        impl $class_name {
            pub fn new($($field_name: $field_type),*) -> Self {
                Self {
                    $($field_name),*
                }
            }
        }
    }
}

define_ast!(
    Expr,
    [
        Binary =>
        (left: Box<dyn Expr>, operator: Token, right: Box<dyn Expr>),
        Grouping => (expression: Box<dyn Expr>),
        Literal => (value: String),
        Unary => (operator: Token, right: Box<dyn Expr>)
    ]
);
