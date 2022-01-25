use crate::chunk::Chunk;
use crate::scanner::{Scanner, Token, TokenType};
use crate::vm::InterpretError;

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

    fn advance(&mut self) {
        self.previous = self.current;

        loop {
            self.current = self.scanner.scan_token();
            match self.current.ty {
                TokenType::Error => self.error_at_current(self.current.lexeme),
                _ => break,
            }
        }
    }

    fn consume(&mut self, ty: TokenType, message: &str) {
        if self.current.ty == ty {
            self.advance();
            return;
        }

        self.error_at_current(message);
    }

    fn error_at_current(&mut self, message: &str) {
        self.error_at(self.current, message);
    }

    fn error(&mut self, message: &str) {
        self.error_at(self.previous, message);
    }

    fn error_at(&mut self, token: Token<'s>, message: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;

        eprint!("[line {}] Error", token.line);

        match token.ty {
            TokenType::Eof => eprint!(" at end"),
            TokenType::Error => {}
            _ => eprint!(" at '{}'", token.lexeme),
        }

        eprintln!(": {}", message);
        self.had_error = true;
    }
}

pub fn compile(source: &str, chunk: &mut Chunk) -> Result<(), InterpretError> {
    let mut scanner = Scanner::new(source);
    let mut parser = Parser::new(&mut scanner);

    parser.advance();
    // parser.expression();
    // consume(TokenType::Eof, "Expect end of expression.");
    !parser.had_error;

    // loop {
    //     parser.advance(&mut scanner);
    //     if parser.current.token_type == TokenType::Eof {
    //         break;
    //     }
    // }

    // let mut line = 0;
    // loop {
    //     let token = scanner.scan_token();
    //     if token.line != line {
    //         print!("{:4} ", line);
    //         line = token.line;
    //     } else {
    //         print!("   | ");
    //     }
    //     println!("{:2} '{}'", token.ty as u8, token.lexeme);

    //     if token.ty == TokenType::Eof {
    //         break;
    //     }
    // }

    Ok(())
}
