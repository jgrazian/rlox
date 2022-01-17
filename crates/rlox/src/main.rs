use std::env;
use std::error::Error;
use std::process;

use tree_walk::{run_file, run_prompt};

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
