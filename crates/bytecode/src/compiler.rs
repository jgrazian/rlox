use crate::chunk::{Chunk, OpCode, Value};
use crate::error::LoxError;
use crate::scanner::{Scanner, Token, TokenType};

struct Parser<'s> {
    scanner: &'s mut Scanner<'s>,
    current: Token<'s>,
    previous: Token<'s>,
    had_error: bool,
    panic_mode: bool,
}

impl<'s> Parser<'s> {
    fn new(scanner: &'s mut Scanner<'s>) -> Self {
        Self {
            scanner,
            current: Token {
                ty: TokenType::Eof,
                lexeme: "",
                line: 0,
            },
            previous: Token {
                ty: TokenType::Eof,
                lexeme: "",
                line: 0,
            },
            had_error: false,
            panic_mode: false,
        }
    }

    fn advance(&mut self) -> Result<(), LoxError> {
        self.previous = self.current;

        let mut result = Ok(());
        loop {
            self.current = self.scanner.scan_token();
            match self.current.ty {
                TokenType::Error => match self.error_at_current(self.current.lexeme) {
                    Some(e) => result = Err(e),
                    None => {}
                },
                _ => break,
            }
        }

        result
    }

    fn consume(&mut self, ty: TokenType, message: &str) -> Result<(), LoxError> {
        if self.current.ty == ty {
            return self.advance();
        }

        match self.error_at_current(message) {
            Some(e) => Err(e),
            None => Ok(()),
        }
    }

    // Errors
    fn error_at_current(&mut self, message: &str) -> Option<LoxError> {
        self.error_at(self.current, message)
    }

    fn error(&mut self, message: &str) -> Option<LoxError> {
        self.error_at(self.previous, message)
    }

    fn error_at(&mut self, token: Token<'s>, message: &str) -> Option<LoxError> {
        if self.panic_mode {
            return None;
        }
        self.panic_mode = true;

        let location = match token.ty {
            TokenType::Eof => " at end".to_string(),
            TokenType::Error => "".to_string(),
            _ => format!(" at '{}'", token.lexeme),
        };

        self.had_error = true;
        Some(LoxError::ParseError {
            message: message.to_string(),
            location,
            line: token.line,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
enum Precedence {
    None = 0,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl Precedence {
    fn bump(self) -> Precedence {
        match self {
            Precedence::None => Precedence::Assignment,
            Precedence::Assignment => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => Precedence::Primary,
        }
    }
}

struct ParseRule<'s> {
    prefix: Option<fn(&mut Compiler<'s>) -> Result<(), LoxError>>,
    infix: Option<fn(&mut Compiler<'s>) -> Result<(), LoxError>>,
    precedence: Precedence,
}

struct Compiler<'s> {
    parser: &'s mut Parser<'s>,
    compiling_chunk: &'s mut Chunk,
}

impl<'s> Compiler<'s> {
    fn new(parser: &'s mut Parser<'s>, compiling_chunk: &'s mut Chunk) -> Self {
        Self {
            parser,
            compiling_chunk,
        }
    }

    fn compile(&mut self) -> Result<(), LoxError> {
        self.parser.advance()?;
        self.expression()?;
        self.parser
            .consume(TokenType::Eof, "Expect end of expression.")?;
        self.end_compiler();
        Ok(())
    }

    fn expression(&mut self) -> Result<(), LoxError> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn number(&mut self) -> Result<(), LoxError> {
        let value = match self.parser.previous.lexeme.parse::<f64>() {
            Ok(value) => value,
            Err(_) => {
                return Err(self.error("Parse failure.").into());
            }
        };
        self.emit_constant(value)
    }

    fn grouping(&mut self) -> Result<(), LoxError> {
        self.expression()?;
        self.parser
            .consume(TokenType::RightParen, "Expect ')' after expression.")
    }

    fn unary(&mut self) -> Result<(), LoxError> {
        let operator_type = self.parser.previous.ty;
        self.parse_precedence(Precedence::Unary)?;
        match operator_type {
            TokenType::Minus => self.emit_byte(OpCode::OpNegate),
            _ => unreachable!("Expected unary operator."),
        }
        Ok(())
    }

    fn binary(&mut self) -> Result<(), LoxError> {
        let operator_type = self.parser.previous.ty;
        let rule = Self::get_rule(operator_type);
        self.parse_precedence(rule.precedence.bump())?;

        match operator_type {
            TokenType::Plus => self.emit_byte(OpCode::OpAdd),
            TokenType::Minus => self.emit_byte(OpCode::OpSubtract),
            TokenType::Star => self.emit_byte(OpCode::OpMultiply),
            TokenType::Slash => self.emit_byte(OpCode::OpDivide),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), LoxError> {
        self.parser.advance()?;
        match Self::get_rule(self.parser.previous.ty).prefix {
            Some(prefix_rule) => prefix_rule(self)?,
            None => {
                return Err(self.error("Expect expression.").into());
            }
        };

        while precedence <= Self::get_rule(self.parser.current.ty).precedence {
            self.parser.advance()?;
            match Self::get_rule(self.parser.previous.ty).infix {
                Some(infix_rule) => infix_rule(self)?,
                None => {}
            }
        }

        Ok(())
    }

    fn end_compiler(&mut self) {
        self.emit_return();
        #[cfg(feature = "debug_print_code")]
        {
            if !self.parser.had_error {
                self.compiling_chunk.disassemble("code");
            }
        }
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpCode::OpReturn);
    }

    fn emit_byte<T: Into<u8>>(&mut self, byte: T) {
        self.compiling_chunk
            .push_byte(byte, self.parser.previous.line);
    }

    fn emit_bytes<U: Into<u8>, V: Into<u8>>(&mut self, byte1: U, byte2: V) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn emit_constant(&mut self, value: Value) -> Result<(), LoxError> {
        let constant = self.make_constant(value);
        self.emit_bytes(OpCode::OpConstant, constant);
        Ok(())
    }

    fn make_constant(&mut self, value: Value) -> u8 {
        self.compiling_chunk.push_constant(value)
    }

    // Errors
    fn error_at_current(&mut self, message: &str) -> Option<LoxError> {
        self.error_at(self.parser.current, message)
    }

    fn error(&mut self, message: &str) -> Option<LoxError> {
        self.error_at(self.parser.previous, message)
    }

    fn error_at(&mut self, token: Token<'s>, message: &str) -> Option<LoxError> {
        if self.parser.panic_mode {
            return None;
        }
        self.parser.panic_mode = true;

        let location = match token.ty {
            TokenType::Eof => " at end".to_string(),
            TokenType::Error => "".to_string(),
            _ => format!(" at '{}'", token.lexeme),
        };

        self.parser.had_error = true;
        Some(LoxError::CompileError {
            message: message.to_string(),
            location,
            line: token.line,
        })
    }

    fn get_rule(ty: TokenType) -> ParseRule<'s> {
        match ty {
            TokenType::LeftParen => ParseRule {
                prefix: Some(Self::grouping),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Minus => ParseRule {
                prefix: Some(Self::unary),
                infix: Some(Self::binary),
                precedence: Precedence::Term,
            },
            TokenType::Plus => ParseRule {
                prefix: None,
                infix: Some(Self::binary),
                precedence: Precedence::Term,
            },
            TokenType::Slash | TokenType::Star => ParseRule {
                prefix: None,
                infix: Some(Self::binary),
                precedence: Precedence::Factor,
            },
            TokenType::Number => ParseRule {
                prefix: Some(Self::number),
                infix: None,
                precedence: Precedence::None,
            },
            _ => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
        }
    }
}

pub fn compile(source: &str, chunk: &mut Chunk) -> Result<(), LoxError> {
    let mut scanner = Scanner::new(source);
    let mut parser = Parser::new(&mut scanner);
    let mut compiler = Compiler::new(&mut parser, chunk);

    compiler.compile()

    // parser.advance();
    // parser.expression();
    // parser.consume(TokenType::Eof, "Expect end of expression.")
}
