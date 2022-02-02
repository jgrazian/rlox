use core::iter::Peekable;
use core::str::CharIndices;

#[derive(Debug, Clone)]
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

    pub fn scan_token(&mut self) -> Token<'s> {
        // Remove whitespace
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
                            Some((_, '/')) => {
                                self.current += 1;
                                loop {
                                    match self.iter.peek() {
                                        None | Some((_, '\n')) => break,
                                        Some(_) => {
                                            self.advance();
                                        }
                                    }
                                }
                            }
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
                            Some((_, c)) => match c {
                                '0'..='9' => self.advance(),
                                'a'..='z' | 'A'..='Z' => {
                                    return self
                                        .error_token("Identifiers can not begin with a number.")
                                }
                                _ => break,
                            },
                            None => break,
                        };
                    }

                    match self.iter.peek() {
                        Some((_, '.')) => match self.peek_next() {
                            Some('0'..='9') => {
                                self.advance();

                                loop {
                                    match self.iter.peek() {
                                        Some((_, '0'..='9')) => self.advance(),
                                        _ => break,
                                    };
                                }
                            }
                            Some(' ') => {
                                self.advance();
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
                self.current += c.len_utf8();
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
        let mut tmp_iter = self.iter.clone();
        tmp_iter.next();
        match tmp_iter.peek() {
            Some((_, c)) => Some(*c),
            _ => None,
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

    pub fn token(&self, ty: TokenType) -> Token<'s> {
        Token {
            ty,
            lexeme: &self.source[self.start..self.current],
            line: self.line,
        }
    }

    pub fn error_token(&self, message: &'static str) -> Token<'s> {
        Token {
            ty: TokenType::Error,
            lexeme: message,
            line: self.line,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
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

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_scan {
        ($name: tt, $lexeme: literal, $ty: expr) => {
            #[test]
            fn $name() {
                let mut scanner = Scanner::new($lexeme);
                let token = scanner.scan_token();
                assert_eq!(
                    token,
                    Token {
                        ty: $ty,
                        lexeme: $lexeme,
                        line: 1
                    }
                )
            }
        };
    }

    test_scan!(scan_paren_left, "(", TokenType::LeftParen);
    test_scan!(scan_paren_right, ")", TokenType::RightParen);
    test_scan!(scan_brace_left, "{", TokenType::LeftBrance);
    test_scan!(scan_brace_right, "}", TokenType::RightBrace);
    test_scan!(scan_comma, ",", TokenType::Comma);
    test_scan!(scan_dot, ".", TokenType::Dot);
    test_scan!(scan_minus, "-", TokenType::Minus);
    test_scan!(scan_plus, "+", TokenType::Plus);
    test_scan!(scan_semicolon, ";", TokenType::Semicolon);
    test_scan!(scan_slash, "/", TokenType::Slash);
    test_scan!(scan_star, "*", TokenType::Star);
    test_scan!(scan_bang, "!", TokenType::Bang);
    test_scan!(scan_bang_equal, "!=", TokenType::BangEqual);
    test_scan!(scan_equal, "=", TokenType::Equal);
    test_scan!(scan_equal_equal, "==", TokenType::EqualEqual);
    test_scan!(scan_greater, ">", TokenType::Greater);
    test_scan!(scan_greater_equal, ">=", TokenType::GreaterEqual);
    test_scan!(scan_less, "<", TokenType::Less);
    test_scan!(scan_less_equal, "<=", TokenType::LessEqual);
    test_scan!(scan_and, "and", TokenType::And);
    test_scan!(scan_class, "class", TokenType::Class);
    test_scan!(scan_else, "else", TokenType::Else);
    test_scan!(scan_false, "false", TokenType::False);
    test_scan!(scan_for, "for", TokenType::For);
    test_scan!(scan_fun, "fun", TokenType::Fun);
    test_scan!(scan_if, "if", TokenType::If);
    test_scan!(scan_nil, "nil", TokenType::Nil);
    test_scan!(scan_or, "or", TokenType::Or);
    test_scan!(scan_print, "print", TokenType::Print);
    test_scan!(scan_return, "return", TokenType::Return);
    test_scan!(scan_super, "super", TokenType::Super);
    test_scan!(scan_this, "this", TokenType::This);
    test_scan!(scan_true, "true", TokenType::True);
    test_scan!(scan_var, "var", TokenType::Var);
    test_scan!(scan_while, "while", TokenType::While);
    test_scan!(scan_eof, "", TokenType::Eof);

    #[test]
    fn scan_identifier() {
        let mut scanner = Scanner::new("ident ident1 1ident");
        assert_eq!(
            scanner.scan_token(),
            Token {
                ty: TokenType::Identifier,
                lexeme: "ident",
                line: 1
            }
        );
        assert_eq!(
            scanner.scan_token(),
            Token {
                ty: TokenType::Identifier,
                lexeme: "ident1",
                line: 1
            }
        );
        assert_eq!(
            scanner.scan_token(),
            Token {
                ty: TokenType::Error,
                lexeme: "Identifiers can not begin with a number.",
                line: 1
            }
        );
    }

    #[test]
    fn scan_number() {
        let mut scanner = Scanner::new("3 3. 3.14");
        assert_eq!(
            scanner.scan_token(),
            Token {
                ty: TokenType::Number,
                lexeme: "3",
                line: 1
            }
        );
        assert_eq!(
            scanner.scan_token(),
            Token {
                ty: TokenType::Number,
                lexeme: "3.",
                line: 1
            }
        );
        assert_eq!(
            scanner.scan_token(),
            Token {
                ty: TokenType::Number,
                lexeme: "3.14",
                line: 1
            }
        );
    }

    #[test]
    fn scan_string() {
        let mut scanner = Scanner::new(r#""string" "and or false true 123" "broken"#);
        assert_eq!(
            scanner.scan_token(),
            Token {
                ty: TokenType::String,
                lexeme: r#""string""#,
                line: 1
            }
        );
        assert_eq!(
            scanner.scan_token(),
            Token {
                ty: TokenType::String,
                lexeme: r#""and or false true 123""#,
                line: 1
            }
        );
        assert_eq!(
            scanner.scan_token(),
            Token {
                ty: TokenType::Error,
                lexeme: "Unterminated string.",
                line: 1
            }
        );
    }

    #[test]
    fn scan_whitespace() {
        let mut scanner = Scanner::new("    (\n//This is a comment\n)");
        assert_eq!(
            scanner.scan_token(),
            Token {
                ty: TokenType::LeftParen,
                lexeme: "(",
                line: 1
            }
        );
        assert_eq!(
            scanner.scan_token(),
            Token {
                ty: TokenType::RightParen,
                lexeme: ")",
                line: 3
            }
        );
    }
}
