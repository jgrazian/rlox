use std::cell::RefCell;
use std::rc::Rc;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub enum InterpreterType {
    TreeWalk,
    Bytecode,
}

#[wasm_bindgen]
pub fn run(source: &str, interpreter: InterpreterType) -> String {
    match interpreter {
        InterpreterType::TreeWalk => {
            let interpreter = Rc::new(RefCell::new(tree_walk::Interpreter::new()));
            match tree_walk::run(source, interpreter) {
                Ok(output) => output.join("\n"),
                Err(e) => tree_walk::flatten_errors(e, "\n"),
            }
        }
        InterpreterType::Bytecode => {
            let mut out: Vec<_> = Vec::new();

            match bytecode::run(&source, &mut out) {
                Ok(_) => match String::from_utf8(out) {
                    Ok(output) => output,
                    Err(e) => format!("{}", e),
                },
                Err(e) => e.to_string(),
            }
        }
    }
}
