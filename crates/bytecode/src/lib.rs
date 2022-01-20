#[macro_use]

mod chunk;
mod debug;
mod vm;

use chunk::*;

pub fn run() {
    let mut chunk = Chunk::new();
    let mut constant = chunk.add_constant(1.2);
    chunk.write(OpCode::OpConstant(constant), 123);

    constant = chunk.add_constant(3.4);
    chunk.write(OpCode::OpConstant(constant), 123);

    chunk.write(OpCode::OpAdd, 123);

    constant = chunk.add_constant(5.6);
    chunk.write(OpCode::OpConstant(constant), 123);

    chunk.write(OpCode::OpDivide, 123);
    chunk.write(OpCode::OpNegate, 123);

    chunk.write(OpCode::OpReturn, 123);

    vm::Vm::interpret(&chunk);

    debug::disassemble_chunk(&chunk, "test chunk");
}
