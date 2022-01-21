use crate::chunk::{
    Chunk,
    OpCode::{self, *},
    Value,
};

const STACK_MAX: usize = 256;

pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

pub struct Vm<'c> {
    chunk: &'c Chunk,
    ip: usize,
    stack: [Value; STACK_MAX],
    stack_top: usize,
}

impl<'c> Vm<'c> {
    pub fn interpret(chunk: &'c Chunk) -> InterpretResult {
        let mut vm = Self {
            chunk,
            ip: 0,
            stack: [Value::default(); STACK_MAX],
            stack_top: 0,
        };

        vm.run()
    }

    fn run(&mut self) -> InterpretResult {
        macro_rules! read_byte {
            () => {{
                let v: OpCode = self.chunk.code[self.ip].into();
                self.ip += 1;
                v
            }};
        }

        macro_rules! binary_op {
            ($op: tt) => {
                {
                    let b = self.pop();
                    self.stack[self.stack_top - 1] = self.stack[self.stack_top - 1] $op b;
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
                OpAdd => binary_op!(+),
                OpSubtract => binary_op!(-),
                OpMultiply => binary_op!(*),
                OpDivide => binary_op!(/),
                OpNegate => {
                    self.stack[self.stack_top - 1] = -self.stack[self.stack_top - 1];
                }
                OpReturn => {
                    println!("{}", self.pop());
                    return InterpretResult::Ok;
                }
            }
        }

        InterpretResult::Ok
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
}
