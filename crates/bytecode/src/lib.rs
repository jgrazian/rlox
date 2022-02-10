use std::error::Error;
use std::fs;
use std::io;
use std::io::Write;
use std::process;

mod chunk;
mod compiler;
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
    let mut vm = vm::Vm::new();

    loop {
        eprint!("> ");
        io::stdout().flush()?;
        let mut buffer = String::new();
        reader.read_line(&mut buffer)?;
        if buffer == "quit\n" {
            break;
        }

        match vm.interpret(&buffer, &mut stdout) {
            Err(e) => eprintln!("{}", e),
            Ok(()) => (),
        }
    }
    Ok(())
}

// pub fn run_file(path: &str) -> Result<(), Box<dyn Error>> {
//     let file = fs::read_to_string(path)?;
//     let result = vm::Vm::new().interpret(&file);

//     match result {
//         Ok(_) => Ok(()),
//         Err(e) => {
//             eprint!("{}", e);
//             process::exit(65);
//         }
//     }
// }
