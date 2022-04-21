use std::error::Error;
use std::fs;
use std::io;
use std::io::Write;
use std::process;

mod chunk;
mod compiler;
mod enviroment;
mod error;
mod heap;
mod object;
mod scanner;
mod value;
mod vm;

pub fn repl() -> Result<(), Box<dyn Error>> {
    let mut stdout = io::stdout();
    let reader = io::stdin();
    eprintln!("rlox\ntype 'quit' to exit");

    let mut buffer = String::new();
    let env = enviroment::Enviroment::new();

    loop {
        eprint!("> ");
        io::stdout().flush()?;
        buffer.clear();
        reader.read_line(&mut buffer)?;
        if buffer == "quit\n" {
            break;
        }

        match env.interpret(&buffer, &mut stdout) {
            Err(e) => eprintln!("{}", e),
            Ok(()) => (),
        }
    }
    Ok(())
}

pub fn run_file<F: Write>(path: &str, out_stream: &mut F) -> Result<(), Box<dyn Error>> {
    let file = fs::read_to_string(path)?;
    let result = enviroment::Enviroment::new().interpret(&file, out_stream);

    match result {
        Ok(_) => Ok(()),
        Err(e) => {
            eprint!("{}", e);
            process::exit(65);
        }
    }
}

pub fn run<F: Write>(source: &str, out_stream: &mut F) -> Result<(), Box<dyn Error>> {
    let result = enviroment::Enviroment::new().interpret(source, out_stream);

    match result {
        Ok(_) => Ok(()),
        Err(e) => {
            eprint!("{}", e);
            process::exit(65);
        }
    }
}
