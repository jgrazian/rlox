use std::error::Error;
use std::fmt;

use crate::token::Token;
use crate::token_type::TokenType;

#[derive(Debug)]
pub struct ScanError {
    line: usize,
    message: String,
}

impl ScanError {
    fn new(line: usize, message: String) -> Self {
        Self { line, message }
    }
}

impl fmt::Display for ScanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[line {}] Error {}", self.line, self.message)
    }
}

impl Error for ScanError {}

#[derive(Debug)]
pub struct ScannerError {
    errors: Vec<ScanError>,
}

impl ScannerError {
    fn new() -> Self {
        Self { errors: Vec::new() }
    }

    fn push(&mut self, err: ScanError) {
        self.errors.push(err)
    }
}

impl fmt::Display for ScannerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.errors
                .iter()
                .map(|e| format!("{}\n", e))
                .collect::<String>()
        )
    }
}

impl Error for ScannerError {}

#[derive(Debug)]
pub struct Scanner {
    chars: Vec<char>,
    tokens: Vec<Token>,

    start: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    pub fn new(source: &str) -> Self {
        Self {
            chars: source.chars().collect(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>, ScannerError> {
        let mut errors = ScannerError::new();
        while !self.is_at_end() {
            self.start = self.current;
            if let Err(e) = self.scan_token() {
                errors.push(e);
            }
        }

        self.tokens
            .push(Token::new(TokenType::Eof, "".to_string(), self.line));

        if errors.errors.len() == 0 {
            Ok(self.tokens.clone())
        } else {
            Err(errors)
        }
    }

    fn scan_token(&mut self) -> Result<(), ScanError> {
        let c = self.advance();
        match c {
            ' ' | '\r' | '\t' => (),
            '\n' => self.line += 1,
            '(' => self.push_token(TokenType::LeftParen),
            ')' => self.push_token(TokenType::RightParen),
            '{' => self.push_token(TokenType::LeftBrace),
            '}' => self.push_token(TokenType::RightBrace),
            ',' => self.push_token(TokenType::Comma),
            '.' => self.push_token(TokenType::Dot),
            '-' => self.push_token(TokenType::Minus),
            '+' => self.push_token(TokenType::Plus),
            ';' => self.push_token(TokenType::Semicolon),
            '*' => self.push_token(TokenType::Star),
            '!' => {
                if self.match_char('=') {
                    self.push_token(TokenType::BangEqual)
                } else {
                    self.push_token(TokenType::Bang)
                }
            }
            '=' => {
                if self.match_char('=') {
                    self.push_token(TokenType::EqualEqual)
                } else {
                    self.push_token(TokenType::Equal)
                }
            }
            '<' => {
                if self.match_char('=') {
                    self.push_token(TokenType::LessEqual)
                } else {
                    self.push_token(TokenType::Less)
                }
            }
            '>' => {
                if self.match_char('=') {
                    self.push_token(TokenType::GreaterEqual)
                } else {
                    self.push_token(TokenType::Greater)
                }
            }
            '/' => {
                if self.match_char('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.push_token(TokenType::Slash)
                }
            }
            '"' => self.string()?,
            c if c.is_digit(10) => self.number()?,
            c if c.is_alphabetic() => self.identifier()?,

            x @ _ => {
                return Err(ScanError::new(
                    self.line,
                    format!("Unexpected character {}.", x),
                ))
            }
        }

        Ok(())
    }

    fn string(&mut self) -> Result<(), ScanError> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            return Err(ScanError::new(
                self.line,
                "Unterminated string.".to_string(),
            ));
        }

        self.advance();
        let value = String::from_iter(&self.chars[self.start + 1..self.current - 1]);
        self.push_token(TokenType::String(value));
        Ok(())
    }

    fn number(&mut self) -> Result<(), ScanError> {
        while self.peek().is_digit(10) {
            self.advance();
        }

        if self.peek() == '.' && self.next_peek().is_digit(10) {
            self.advance();

            while self.peek().is_digit(10) {
                self.advance();
            }
        }

        let value = String::from_iter(&self.chars[self.start..self.current])
            .parse::<f64>()
            .map_err(|_| ScanError::new(self.line, "Failed to parse as float.".to_string()))?;
        self.push_token(TokenType::Number(value));
        Ok(())
    }

    fn identifier(&mut self) -> Result<(), ScanError> {
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.advance();
        }

        let text = String::from_iter(&self.chars[self.start..self.current]);
        let token = Self::to_keyword_token(&text).unwrap_or(TokenType::Identifier(text));
        self.push_token(token);
        Ok(())
    }

    fn to_keyword_token(s: &str) -> Option<TokenType> {
        let token = match s {
            "and" => TokenType::And,
            "class" => TokenType::Class,
            "else" => TokenType::Else,
            "false" => TokenType::False,
            "for" => TokenType::For,
            "fun" => TokenType::Fun,
            "if" => TokenType::If,
            "nil" => TokenType::Nil,
            "or" => TokenType::Or,
            "print" => TokenType::Print,
            "return" => TokenType::Return,
            "super" => TokenType::Super,
            "this" => TokenType::This,
            "true" => TokenType::True,
            "var" => TokenType::Var,
            "while" => TokenType::While,
            _ => return None,
        };

        Some(token)
    }

    fn advance(&mut self) -> char {
        let c = self.chars[self.current];
        self.current += 1;
        c
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.chars[self.current] != expected {
            return false;
        }
        self.current += 1;
        true
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        self.chars[self.current]
    }

    fn next_peek(&self) -> char {
        if self.current + 1 >= self.chars.len() {
            '\0'
        } else {
            self.chars[self.current + 1]
        }
    }

    fn push_token(&mut self, token: TokenType) {
        let text = String::from_iter(&self.chars[self.start..self.current]);
        self.tokens.push(Token::new(token, text, self.line))
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.chars.len()
    }
}
