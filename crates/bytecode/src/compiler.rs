use crate::scanner::{Scanner, TokenType};
use crate::vm::InterpretError;

pub fn compile(source: &str) -> Result<(), InterpretError> {
    let mut scanner = Scanner::new(source);

    let mut line = 0;
    loop {
        let token = scanner.scan_token();
        if token.line != line {
            print!("{:4} ", line);
            line = token.line;
        } else {
            print!("   | ");
        }
        println!("{:2} '{}'", token.ty as u8, token.lexeme);

        if token.ty == TokenType::Eof {
            break;
        }
    }

    Ok(())
}
