use std::fmt;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::token_type::TokenType;

static TOKEN_ID: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
    _id: usize,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, line: usize) -> Self {
        let _id = TOKEN_ID.fetch_add(1, Ordering::SeqCst);
        Self {
            token_type,
            lexeme,
            line,
            _id,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.token_type {
            TokenType::Identifier(ref lexeme) => write!(f, "{}", lexeme),
            TokenType::String(ref s) => write!(f, "{}", s),
            TokenType::Number(ref n) => write!(f, "{}", n),
            _ => write!(f, "{:?}", self.token_type),
        }
    }
}
