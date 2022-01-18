use std::cell::RefCell;
use std::rc::Rc;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn run(source: &str) -> String {
    let interpreter = Rc::new(RefCell::new(tree_walk::Interpreter::new()));
    match tree_walk::run(source, interpreter) {
        Ok(output) => output.join("\n"),
        Err(e) => tree_walk::flatten_errors(e, "\n"),
    }
}
