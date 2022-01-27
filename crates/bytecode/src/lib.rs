use std::error::Error;
use std::fs;
use std::io;
use std::io::Write;
use std::process;

mod chunk;
mod compiler;
mod error;
mod scanner;
mod vm;

pub fn repl() -> Result<(), Box<dyn Error>> {
    let reader = io::stdin();
    println!("rlox\ntype 'quit' to exit");

    loop {
        print!("> ");
        io::stdout().flush()?;
        let mut buffer = String::new();
        reader.read_line(&mut buffer)?;
        if buffer == "quit\n" {
            break;
        }

        match vm::Vm::interpret(&buffer) {
            Err(e) => println!("{}", e),
            Ok(()) => (),
        }
    }
    Ok(())
}

pub fn run_file(path: &str) -> Result<(), Box<dyn Error>> {
    let file = fs::read_to_string(path)?;
    let result = vm::Vm::interpret(&file);

    match result {
        Ok(_) => Ok(()),
        Err(e) => {
            process::exit(65);
        }
    }
}
