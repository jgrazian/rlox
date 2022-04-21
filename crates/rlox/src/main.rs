use std::env;
use std::error::Error;
use std::process;

fn main() -> Result<(), Box<dyn Error>> {
    let mut stdout = std::io::stdout();
    let args = env::args().collect::<Vec<_>>();
    if args.iter().any(|s| s == "--tree") {
        match args.len() {
            1 => tree_walk::run_prompt(),
            2 => tree_walk::run_file(&args[1]),
            _ => {
                println!("Usage: rlox [script]");
                process::exit(65)
            }
        }
    } else {
        match args.len() {
            1 => bytecode::repl(),
            2 => bytecode::run_file(&args[1], &mut stdout),
            _ => {
                println!("Usage: rlox [script]");
                process::exit(64)
            }
        }
    }
}
