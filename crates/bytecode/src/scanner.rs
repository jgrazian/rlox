use core::iter::Peekable;
use core::str::CharIndices;

pub struct Scanner<'s> {
    source: &'s str,
    iter: Peekable<CharIndices<'s>>,
    start: usize,
    current: usize,
    line: usize,
}

impl<'s> Scanner<'s> {
    pub fn new(source: &'s str) -> Self {
        Self {
            source,
            iter: source.char_indices().peekable(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_token(&mut self) -> Token {
        loop {
            match self.iter.peek() {
                None => break,
                Some((_, c)) => match c {
                    ' ' | '\r' | '\t' => {
                        self.advance();
                    }
                    '\n' => {
                        self.line += 1;
                        self.advance();
                    }
                    '/' => {
                        let tmp_iter = self.iter.clone();
                        self.iter.next();
                        match self.iter.peek() {
                            Some((_, '/')) => loop {
                                match self.iter.peek() {
                                    None | Some((_, '\n')) => break,
                                    Some(_) => {
                                        self.advance();
                                    }
                                }
                            },
                            _ => {
                                self.iter = tmp_iter;
                                break;
                            }
                        }
                    }
                    _ => break,
                },
            };
        }

        self.start = self.current;
        match self.advance() {
            None => self.token(TokenType::Eof),
            Some(c) => match c {
                '(' => self.token(TokenType::LeftParen),
                ')' => self.token(TokenType::RightParen),
                '{' => self.token(TokenType::LeftBrance),
                '}' => self.token(TokenType::RightBrace),
                ';' => self.token(TokenType::Semicolon),
                ',' => self.token(TokenType::Comma),
                '.' => self.token(TokenType::Dot),
                '-' => self.token(TokenType::Minus),
                '+' => self.token(TokenType::Plus),
                '/' => self.token(TokenType::Slash),
                '*' => self.token(TokenType::Star),
                '!' => match self.match_next('=') {
                    true => self.token(TokenType::BangEqual),
                    false => self.token(TokenType::Bang),
                },
                '=' => match self.match_next('=') {
                    true => self.token(TokenType::EqualEqual),
                    false => self.token(TokenType::Equal),
                },
                '<' => match self.match_next('=') {
                    true => self.token(TokenType::LessEqual),
                    false => self.token(TokenType::Less),
                },
                '>' => match self.match_next('=') {
                    true => self.token(TokenType::GreaterEqual),
                    false => self.token(TokenType::Greater),
                },
                '"' => {
                    loop {
                        match self.iter.peek() {
                            None => return self.error_token("Unterminated string."),
                            Some((_, '"')) => break,
                            Some((_, c)) => {
                                if c == &'\n' {
                                    self.line += 1;
                                }
                                self.advance();
                            }
                        }
                    }
                    self.advance();
                    self.token(TokenType::String)
                }
                '0'..='9' => {
                    loop {
                        match self.iter.peek() {
                            None => break,
                            Some((_, c)) => match c {
                                '0'..='9' => self.advance(),
                                _ => break,
                            },
                        };
                    }

                    match self.iter.peek() {
                        Some((_, '.')) => match self.peek_next() {
                            Some('0'..='9') => {
                                self.advance();

                                loop {
                                    match self.iter.peek() {
                                        None => break,
                                        Some((_, c)) => match c {
                                            '0'..='9' => self.advance(),
                                            _ => break,
                                        },
                                    };
                                }
                            }
                            _ => (),
                        },
                        _ => (),
                    }

                    self.token(TokenType::Number)
                }
                'a'..='z' | 'A'..='Z' => {
                    loop {
                        match self.iter.peek() {
                            None => break,
                            Some((_, c)) => match c {
                                'a'..='z' | 'A'..='Z' | '0'..='9' => self.advance(),
                                _ => break,
                            },
                        };
                    }
                    self.token(self.identifier_type())
                }
                _ => self.error_token("Unexpected token."),
            },
        }
    }

    fn advance(&mut self) -> Option<char> {
        match self.iter.next() {
            Some((_, c)) => {
                self.current = match self.iter.peek() {
                    Some((i, _)) => *i,
                    None => self.current,
                };
                Some(c)
            }
            _ => None,
        }
    }

    fn match_next(&mut self, c: char) -> bool {
        match self.iter.peek() {
            Some((_, _c)) if _c == &c => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    fn peek_next(&mut self) -> Option<char> {
        let tmp_iter = self.iter.clone();
        self.iter.next();
        match self.iter.peek() {
            Some((_, c)) => Some(*c),
            _ => {
                self.iter = tmp_iter;
                None
            }
        }
    }

    fn identifier_type(&self) -> TokenType {
        match &self.source[self.start..self.start + 1] {
            "a" => self.check_keyword(1, "nd", TokenType::And),
            "c" => self.check_keyword(1, "lass", TokenType::Class),
            "e" => self.check_keyword(1, "lse", TokenType::Else),
            "i" => self.check_keyword(1, "f", TokenType::If),
            "n" => self.check_keyword(1, "il", TokenType::Nil),
            "o" => self.check_keyword(1, "r", TokenType::Or),
            "p" => self.check_keyword(1, "rint", TokenType::Print),
            "r" => self.check_keyword(1, "eturn", TokenType::Return),
            "s" => self.check_keyword(1, "uper", TokenType::Super),
            "v" => self.check_keyword(1, "ar", TokenType::Var),
            "w" => self.check_keyword(1, "hile", TokenType::While),
            "f" => match self.current - self.start > 1 {
                true => match &self.source[self.start + 1..self.start + 2] {
                    "a" => self.check_keyword(2, "lse", TokenType::False),
                    "o" => self.check_keyword(2, "r", TokenType::For),
                    "u" => self.check_keyword(2, "n", TokenType::Fun),
                    _ => TokenType::Identifier,
                },
                false => TokenType::Identifier,
            },
            "t" => match self.current - self.start > 1 {
                true => match &self.source[self.start + 1..self.start + 2] {
                    "h" => self.check_keyword(2, "is", TokenType::This),
                    "r" => self.check_keyword(2, "ue", TokenType::True),
                    _ => TokenType::Identifier,
                },
                false => TokenType::Identifier,
            },
            _ => TokenType::Identifier,
        }
    }

    fn check_keyword(&self, start: usize, rest: &str, ty: TokenType) -> TokenType {
        match rest.len() == self.current - self.start - start {
            true => match &self.source[self.start + start..self.current] == rest {
                true => ty,
                false => TokenType::Identifier,
            },
            false => TokenType::Identifier,
        }
    }

    pub fn token(&self, ty: TokenType) -> Token {
        Token {
            ty,
            lexeme: &self.source[self.start..self.current],
            line: self.line,
        }
    }

    pub fn error_token(&self, message: &'static str) -> Token {
        Token {
            ty: TokenType::Error,
            lexeme: message,
            line: self.line,
        }
    }
}

pub struct Token<'s> {
    pub ty: TokenType,
    pub lexeme: &'s str,
    pub line: usize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrance,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star, // 10
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Literals
    Identifier,
    String, // 20
    Number,
    // Keywords
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or, // 30
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    // -
    Error,
    Eof,
}
