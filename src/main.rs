use std::env;
use std::error::Error;
use std::fs;
use std::io;
use std::process;

mod token;
mod token_type;

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
        let mut buffer = String::new();
        reader.read_line(&mut buffer)?;
        if buffer == "" {
            break;
        }
        if let Err(e) = run(&buffer) {
            println!("{}", e)
        }
    }
    Ok(())
}

fn run(source: &str) -> Result<(), Box<dyn Error>> {
    let scanner = Scanner { source };
    let tokens = scanner.scan_tokens()?;

    for token in tokens {
        println!("{}", token);
    }
    Ok(())
}
