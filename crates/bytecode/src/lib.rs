use std::error::Error;
use std::fs;
use std::io;
use std::io::Write;

mod chunk;
mod compiler;
mod error;
mod object;
mod scanner;
mod value;
mod vm;

pub fn repl() -> Result<(), Box<dyn Error>> {
    let mut stdout = io::stdout();
    let reader = io::stdin();
    eprintln!("rlox\ntype 'quit' to exit");
    let mut vm = vm::Vm::new();

    let mut buffer = String::new();
    loop {
        eprint!("> ");
        io::stdout().flush()?;
        reader.read_line(&mut buffer)?;
        match buffer.trim_end() {
            "quit" => break,
            _ => (),
        }

        match vm.interpret(&buffer, &mut stdout) {
            Err(e) => eprintln!("{}", e),
            Ok(()) => (),
        }
        buffer.clear();
    }
    Ok(())
}

pub fn run_file(path: &str, out_stream: &mut impl Write) -> Result<(), Box<dyn Error>> {
    let file = fs::read_to_string(path)?;
    let result = vm::Vm::new().interpret(&file, out_stream);

    match result {
        Ok(_) => Ok(()),
        Err(e) => Err(Box::new(e) as Box<dyn Error>),
    }
}
