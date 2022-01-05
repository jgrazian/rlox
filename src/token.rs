use std::fmt;

use crate::token_type::TokenType;

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    _line: usize,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, _line: usize) -> Self {
        Self {
            token_type,
            lexeme,
            _line,
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
