# rlox
An interpreted programing language based on `jlox` in [Crafting Interpreters](https://craftinginterpreters.com).

## Usage
### REPL
`cargo run`

### From File
`cargo run [path_to_script]`


## Lox Language Guide

- Functions

    ```javascript
    fun fib(n) {
        if (n < 2) return n;
        return fib(n - 1) + fib(n - 2);
    }

    var before = clock();
    print fib(5);
    var after = clock();
    print after - before;
    ```

    `rlox` has a single built-in function `clock`.

- Classes

    ```javascript
    class Shape {
        fun say_name() {
            print this.name;
        }
    }

    class Circle < Shape {
        fun init(name, radius) {
            this.name = name;
            this.radius = radius;
        }
        fun area() {
            3.14 * this.radius * this.radius
        }
    }

    var circle = Circle("circle 1", 1.0);
    circle.say_name();   // circle 1
    print circle.area(); // 3.14
    ```


## Building
1. Make sure `wasm32-unknown-unknown` toolchain is installed:
`rustup target add wasm32-unknown-unknown`

2. Install `wasm-bindgen-cli`:
`cargo install wasm-bindgen-cli`

3. Build rlox for `wasm32-unknown-unknown`:
`cargo build -p rlox --target wasm32-unknown-unknown`

4. Create js bindings with `wasm-bindgen`:
`wasm-bindgen ./target/wasm32-unknown-unknown/debug/rlox_lib.wasm --out-dir ./pages/rlox --target web`
