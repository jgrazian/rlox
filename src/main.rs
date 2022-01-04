use std::env;
use std::error::Error;
use std::fs;
use std::io;
use std::io::Write;
use std::process;

mod generate_ast;
mod scanner;
mod token;
mod token_type;

use scanner::Scanner;

fn main() -> Result<(), Box<dyn Error>> {
    let args = env::args().collect::<Vec<_>>();

    match args.len() {
        1 => run_prompt(),
        2 => run_file(&args[1]),
        _ => {
            println!("Usage: rlox [script]");
            process::exit(65)
        }
    }
}

fn run_file(path: &str) -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string(path)?;

    if let Err(_) = run(&contents) {
        process::exit(65);
    }
    Ok(())
}

fn run_prompt() -> Result<(), Box<dyn Error>> {
    let reader = io::stdin();

    loop {
        print!("> ");
        io::stdout().flush()?;
        let mut buffer = String::new();
        reader.read_line(&mut buffer)?;
        if buffer == "\n" {
            break;
        }
        if let Err(e) = run(&buffer) {
            println!("{}", e)
        }
    }
    Ok(())
}

fn run(source: &str) -> Result<(), Box<dyn Error>> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens()?;

    for token in tokens {
        println!("{}", token);
    }
    Ok(())
}
