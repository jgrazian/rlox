use std::cell::RefCell;
use std::error::Error;
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

pub fn run_file(path: &str) -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string(path)?;

    if let Err(e) = run(&contents) {
        eprintln!("{}", e);
        match e.downcast_ref() {
            Some(e) => match e {
                interpreter::RuntimeError { .. } => process::exit(70),
            },
            None => process::exit(65),
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
            eprintln!("{}", e)
        }
    }
    Ok(())
}

pub fn run(source: &str) -> Result<(), Box<dyn Error>> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens()?;
    let mut parser = Parser::new(tokens);
    let statements = parser.parse()?;

    let interpreter = Rc::new(RefCell::new(Interpreter::new()));
    let mut resolver = Resolver::new(interpreter.clone());
    resolver.resolve_stmt(&statements)?;

    interpreter.borrow_mut().interpret(&statements)?;
    Ok(())
}
