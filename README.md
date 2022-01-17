# rlox
An interpreted programing language based on `jlox` in [Crafting Interpreters](https://craftinginterpreters.com).

## Usage
`cargo run [path_to_script]`

`cargo wasm` to build web-assembly
`wasm-bindgen ./target/wasm32-unknown-unknown/debug/rlox_lib.wasm --out-dir ./build/node_modules/rlox` to build js bindings