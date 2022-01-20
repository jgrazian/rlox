use crate::chunk::Chunk;

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);
    print!("{:?}", chunk);
}
