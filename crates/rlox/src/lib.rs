use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);
}

#[wasm_bindgen]
pub fn run(source: &str) {
    tree_walk::run(source).unwrap();
    alert(&format!("Hello, World!"));
}
