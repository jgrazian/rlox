use std::cell::RefCell;
use std::error::Error;
use std::fmt;
use std::fs;
use std::io;
use std::io::Write;
use std::process;
use std::rc::Rc;

mod ast;
mod callable;
mod enviroment;
mod interpreter;
mod parser;
mod resolver;
mod scanner;
mod token;
mod token_type;

use interpreter::Interpreter;
use parser::Parser;
use resolver::Resolver;
use scanner::Scanner;
use token::Token;

pub fn run_file(path: &str) -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string(path)?;

    if let Err(e) = run(&contents) {
        match e[0] {
            LoxError::RuntimeError { .. } => {
                eprintln!("{}", flatten_errors(e, "\n"));
                process::exit(70)
            }
            _ => {
                eprintln!("{}", flatten_errors(e, "\n"));
                process::exit(65)
            }
        }
    }
    Ok(())
}

pub fn run_prompt() -> Result<(), Box<dyn Error>> {
    let reader = io::stdin();
    println!("rlox tree-walk interpreter\ntype 'quit' to exit");

    loop {
        print!("> ");
        io::stdout().flush()?;
        let mut buffer = String::new();
        reader.read_line(&mut buffer)?;
        if buffer == "quit\n" {
            break;
        }
        if let Err(e) = run(&buffer) {
            eprintln!("{}", flatten_errors(e, "\n"))
        }
    }
    Ok(())
}

pub fn run(source: &str) -> Result<(), Vec<LoxError>> {
    let tokens = match Scanner::new(source).scan_tokens() {
        Ok(tokens) => tokens,
        Err(e) => return Err(e),
    };

    let statements = match Parser::new(tokens).parse() {
        Ok(statements) => statements,
        Err(e) => return Err(e),
    };

    let interpreter = Rc::new(RefCell::new(Interpreter::new()));
    Resolver::new(interpreter.clone())
        .resolve_stmt(&statements)
        .map_err(|e| vec![e])?;

    interpreter
        .borrow_mut()
        .interpret(&statements)
        .map_err(|e| vec![e])?;
    Ok(())
}

fn flatten_errors(errors: Vec<LoxError>, sep: &str) -> String {
    errors
        .into_iter()
        .map(|e| e.to_string())
        .collect::<Vec<String>>()
        .join(sep)
}

#[derive(Debug, Clone)]
pub enum LoxError {
    ScanError { line: usize, message: String },
    ParseError { token: Token, message: String },
    ResolveError { token: Token, message: String },
    RuntimeError { token: Token, message: String },
    Output(String),
}
impl Error for LoxError {}

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ScanError { line, message } => write!(f, "[line {}] ScanError {}", line, message),
            Self::ParseError { token, message } => {
                let location = match token.token_type {
                    token_type::TokenType::Eof => "end".to_string(),
                    _ => format!("'{}'", token.lexeme),
                };
                write!(
                    f,
                    "[line {}] ParseError at {} {}",
                    token.line, location, message
                )
            }
            Self::ResolveError { token, message } => {
                let location = match token.token_type {
                    token_type::TokenType::Eof => "end".to_string(),
                    _ => format!("'{}'", token.lexeme),
                };
                write!(
                    f,
                    "[line {}] ResolveError at {} {}",
                    token.line, location, message
                )
            }
            Self::RuntimeError { token, message } => {
                write!(f, "[line {}] RuntimeError {}", token.line, message)
            }
            Self::Output(s) => write!(f, "{}", s),
        }
    }
}

impl LoxError {
    pub fn scan_error(line: usize, message: &str) -> Self {
        Self::ScanError {
            line,
            message: message.to_string(),
        }
    }

    pub fn parse_error(token: &Token, message: &str) -> Self {
        Self::ParseError {
            token: token.clone(),
            message: message.to_string(),
        }
    }

    pub fn resolve_error(token: &Token, message: &str) -> Self {
        Self::ResolveError {
            token: token.clone(),
            message: message.to_string(),
        }
    }

    pub fn runtime_error(token: &Token, message: &str) -> Self {
        Self::RuntimeError {
            token: token.clone(),
            message: message.to_string(),
        }
    }

    pub fn output(message: &str) -> Self {
        Self::Output(message.to_string())
    }
}
