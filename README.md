# rlox
An interpreted programing language based on `jlox` in [Crafting Interpreters](https://craftinginterpreters.com).

## Usage
### REPL
`cargo run`

### From File
`cargo run [path_to_script]`

## Building
1. Make sure `wasm32-unknown-unknown` toolchain is installed:
`rustup target add wasm32-unknown-unknown`

2. Install `wasm-bindgen-cli`:
`cargo install wasm-bindgen-cli`

3. Build rlox for `wasm32-unknown-unknown`:
`wasm32-unknown-unknown`

4. Create js bindings with `wasm-bindgen`:
`wasm-bindgen ./target/wasm32-unknown-unknown/debug/rlox_lib.wasm --out-dir ./pages/rlox`
