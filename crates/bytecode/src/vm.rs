use crate::compiler::compile;
use crate::error::LoxError;

use crate::chunk::{
    Chunk,
    OpCode::{self, *},
    Value,
};

const STACK_MAX: usize = 256;

pub struct Vm {
    chunk: Chunk,
    ip: usize,
    stack: [Value; STACK_MAX],
    stack_top: usize,
}

impl Vm {
    pub fn interpret(source: &str) -> Result<(), LoxError> {
        let mut chunk = Chunk::new();

        compile(source, &mut chunk)?;

        let mut vm = Self {
            chunk,
            ip: 0,
            stack: [Value::Nil; STACK_MAX],
            stack_top: 0,
        };
        vm.run()
    }

    fn run(&mut self) -> Result<(), LoxError> {
        macro_rules! read_byte {
            () => {{
                let v: OpCode = self.chunk.code[self.ip].into();
                self.ip += 1;
                v
            }};
        }

        macro_rules! binary_op {
            ($value_type: expr, $op: tt) => {
                {
                    match (self.peek(0), self.peek(1)) {
                        (Value::Number(_), Value::Number(a)) => {
                            let b = self.pop().as_number();
                            self.stack[self.stack_top - 1] = $value_type(a $op b);
                        },
                        _ => return self.runtime_error("Operands must be numbers."),
                    }
                }
            };
        }

        loop {
            #[cfg(feature = "debug_trace_execution")]
            {
                print!("          ");
                self.stack.iter().for_each(|v| print!("[ {} ]", v));
                print!("\n");
                println!(
                    "{}",
                    self.chunk
                        .debug_op(self.ip, &self.chunk.code[self.ip].into())
                );
            }
            let instruction: OpCode = read_byte!();

            match instruction {
                OpConstant => {
                    let constant = self.chunk.constants[read_byte!() as usize];
                    self.push(constant);
                }
                OpAdd => binary_op!(Value::Number, +),
                OpSubtract => binary_op!(Value::Number, -),
                OpMultiply => binary_op!(Value::Number, *),
                OpDivide => binary_op!(Value::Number, /),
                OpNegate => {
                    match self.peek(0) {
                        Value::Number(_) => {
                            let v = Value::Number(-self.pop().as_number());
                            self.push(v)
                        }
                        _ => return self.runtime_error("Operand must be a number."),
                    }
                    // self.stack[self.stack_top - 1] = -self.stack[self.stack_top - 1];
                }
                OpReturn => {
                    println!("{}", self.pop());
                    return Ok(());
                }
            }
        }
    }

    fn reset_stack(&mut self) {
        self.stack_top = 0;
    }

    fn push(&mut self, value: Value) {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    fn pop(&mut self) -> Value {
        self.stack_top -= 1;
        self.stack[self.stack_top]
    }

    fn peek(&self, distance: usize) -> Value {
        self.stack[self.stack_top - 1 - distance]
    }

    fn runtime_error<T: Into<String>>(&self, message: T) -> Result<(), LoxError> {
        let instruction = self.chunk.code[self.ip - 1] as usize;
        let line = self.chunk.lines[instruction];
        Err(LoxError::RuntimeError(format!(
            "{}\n[line {}] in script\n",
            message.into(),
            line
        )))
    }
}
