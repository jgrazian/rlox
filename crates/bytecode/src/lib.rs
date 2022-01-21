#[macro_use]

mod chunk;
mod debug;
mod vm;

use chunk::*;

pub fn run() {
    let mut chunk = Chunk::new();

    let mut constant = chunk.add_constant(1.2);
    chunk.write_op(OpCode::OpConstant, 123);
    chunk.write_op(constant, 123);

    constant = chunk.add_constant(3.4);
    chunk.write_op(OpCode::OpConstant, 123);
    chunk.write_op(constant, 123);

    chunk.write_op(OpCode::OpAdd, 123);

    constant = chunk.add_constant(5.6);
    chunk.write_op(OpCode::OpConstant, 123);
    chunk.write_op(constant, 123);

    chunk.write_op(OpCode::OpDivide, 123);
    chunk.write_op(OpCode::OpNegate, 123);

    chunk.write_op(OpCode::OpReturn, 123);
    debug::disassemble_chunk(&chunk, "test chunk");
    vm::Vm::interpret(&chunk);
}
