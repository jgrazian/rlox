use crate::chunk::{
    Chunk,
    OpCode::{self, *},
};
use crate::compiler::Compiler;
use crate::error::LoxError;
use crate::object::Obj;
use crate::value::Value;

const STACK_MAX: usize = 256;

pub struct Vm {
    chunk: Chunk,
    ip: usize,
    stack: [Value; STACK_MAX],
    stack_top: usize,
}

impl Vm {
    pub fn interpret(source: &str) -> Result<(), LoxError> {
        const NIL: Value = Value::Nil;
        let mut chunk = Chunk::new();

        Compiler::new(source, &mut chunk).compile()?;

        let mut vm = Self {
            chunk,
            ip: 0,
            stack: [NIL; STACK_MAX],
            stack_top: 0,
        };
        vm.run()
    }

    fn run(&mut self) -> Result<(), LoxError> {
        macro_rules! read_byte {
            () => {{
                self.ip += 1;
                self.chunk.code[self.ip - 1]
            }};
        }

        macro_rules! binary_op {
            ($value_type: expr, $op: tt) => {
                {
                    match (self.peek(0), self.peek(1)) {
                        (Value::Number(b), Value::Number(a)) => {
                            let v = a $op b;
                            self.stack_top -= 2;
                            self.push($value_type(v));
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

            match read_byte!().into() {
                OpConstant => {
                    let constant = self.chunk.constants[read_byte!() as usize].clone();
                    self.push(constant);
                }
                OpNil => self.push(Value::Nil),
                OpTrue => self.push(Value::Bool(true)),
                OpFalse => self.push(Value::Bool(false)),
                OpEqual => {
                    let (a, b) = self.pop2();
                    let v = Value::Bool(a == b);
                    self.push(v)
                }
                OpGreater => binary_op!(Value::Bool, >),
                OpLess => binary_op!(Value::Bool, <),
                OpAdd => match (self.peek(0), self.peek(1)) {
                    (b, a) if b.is_string() && a.is_string() => {
                        let (s1, s2) = self.pop2();
                        let s = s1.as_string().to_owned() + s2.as_string();
                        self.push(Value::Obj(Box::new(Obj::String(s))))
                    }
                    (Value::Number(b), Value::Number(a)) => {
                        let v = a + b;
                        self.stack_top -= 1;
                        self.push(Value::Number(v));
                    }
                    _ => return self.runtime_error("Operands must be two numbers or two strings."),
                },
                OpSubtract => binary_op!(Value::Number, -),
                OpMultiply => binary_op!(Value::Number, *),
                OpDivide => binary_op!(Value::Number, /),
                OpNot => {
                    let v = Value::Bool(self.pop().is_falsey());
                    self.push(v)
                }
                OpNegate => match self.peek(0) {
                    Value::Number(_) => {
                        let v = Value::Number(-self.pop().as_number());
                        self.push(v)
                    }
                    _ => return self.runtime_error("Operand must be a number."),
                },
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

    fn pop(&mut self) -> &Value {
        self.stack_top -= 1;
        &self.stack[self.stack_top]
    }

    fn pop2(&mut self) -> (&Value, &Value) {
        self.stack_top -= 1;
        let b = &self.stack[self.stack_top];
        self.stack_top -= 1;
        let a = &self.stack[self.stack_top];
        (a, b)
    }

    fn peek(&self, distance: usize) -> &Value {
        &self.stack[self.stack_top - 1 - distance]
    }

    fn runtime_error<T: Into<String>>(&mut self, message: T) -> Result<(), LoxError> {
        let instruction = self.chunk.code[self.ip - 2] as usize;
        let line = self.chunk.lines[instruction];
        self.reset_stack();
        Err(LoxError::RuntimeError(format!(
            "{}\n[line {}] in script",
            message.into(),
            line
        )))
    }
}
