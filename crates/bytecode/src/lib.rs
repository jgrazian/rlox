use std::error::Error;
use std::fs;
use std::io;
use std::io::Write;
use std::process;

mod chunk;
mod debug;
mod vm;

use chunk::*;

pub fn run() {
    // let mut chunk = Chunk::new();

    // let mut constant = chunk.add_constant(1.2);
    // chunk.write_op(OpCode::OpConstant, 123);
    // chunk.write_op(constant, 123);

    // constant = chunk.add_constant(3.4);
    // chunk.write_op(OpCode::OpConstant, 123);
    // chunk.write_op(constant, 123);

    // chunk.write_op(OpCode::OpAdd, 123);

    // constant = chunk.add_constant(5.6);
    // chunk.write_op(OpCode::OpConstant, 123);
    // chunk.write_op(constant, 123);

    // chunk.write_op(OpCode::OpDivide, 123);
    // chunk.write_op(OpCode::OpNegate, 123);

    // chunk.write_op(OpCode::OpReturn, 123);
    // debug::disassemble_chunk(&chunk, "test chunk");
    // vm::Vm::interpret(&chunk);
}

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

        vm::Vm::interpret(&buffer)?;
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
