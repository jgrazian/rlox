use std::collections::HashMap;

use crate::chunk::OpCode::*;
use crate::compiler::Compiler;
use crate::error::LoxError;
use crate::heap::Heap;
use crate::object::{Obj, ObjFunction};
use crate::value::Value;

const STACK_MAX: usize = 64 * 64;

#[derive(Debug, PartialEq)]
struct CallFrame<'f> {
    function: &'f ObjFunction,
    ip: usize,
    base_slot: usize,
}

pub struct Vm {
    stack: Vec<Value>,
    heap: Heap<Obj>,
    globals: HashMap<String, Value>,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            stack: vec![Value::Nil; STACK_MAX],
            heap: Heap::new(),
            globals: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, source: &str) -> Result<(), LoxError> {
        let function = Compiler::new(source, &mut self.heap).compile()?;

        self.stack
            .push(Value::Obj(Obj::alloc_function(&mut self.heap, function)));

        self.run()
    }

    fn run(&mut self) -> Result<(), LoxError> {
        let mut frames = vec![CallFrame {
            function: &self
                .heap
                .get(self.stack.last().unwrap().as_obj())
                .unwrap()
                .as_function(),
            ip: 0,
            base_slot: 0,
        }];

        loop {
            let frame = frames.last_mut().unwrap();

            #[cfg(feature = "debug_trace_execution")]
            {
                eprint!("          ");
                let end = self
                    .stack
                    .iter()
                    .rposition(|x| x != &Value::Nil)
                    .unwrap_or(0);
                self.stack[0..=end]
                    .iter()
                    .for_each(|v| eprint!("[ {:>3} ]", v));
                eprint!("\n");
                let ip = self.frame().ip;
                let op = &self.frame_function().chunk.code[ip].into();
                eprintln!("{}", self.frame_function().chunk.debug_op(ip, op));
            }

            match Self::read_byte(frame).into() {
                OpConstant => {
                    let constant = Self::read_constant(frame);
                    self.push(constant);
                }
                OpNil => self.push(Value::Nil),
                OpTrue => self.push(Value::Bool(true)),
                OpFalse => self.push(Value::Bool(false)),
                OpPop => {
                    self.pop();
                }
                OpGetLocal => {
                    let v = self.frame_slots(frame)[Self::read_byte(frame) as usize];
                    self.push(v);
                }
                OpSetLocal => {
                    self.frame_slots(frame)[Self::read_byte(frame) as usize] = *self.peek(0);
                }
                OpGetGlobal => {
                    let name = self.heap[Self::read_constant(frame).as_obj()].as_string();
                    match self.globals.get(name) {
                        Some(v) => {
                            let v = v.clone();
                            self.push(v)
                        }
                        None => {
                            let msg = format!("Undefined variable '{}'.", name);
                            return self.runtime_error(msg, frame);
                        }
                    }
                }
                OpDefineGlobal => {
                    let name = self.heap[Self::read_constant(frame).as_obj()].as_string();
                    self.globals.insert(name.to_owned(), self.peek(0).clone());
                    self.pop();
                }
                OpSetGlobal => {
                    let name = self.heap[Self::read_constant(frame).as_obj()].as_string();
                    let _v = self.peek(0).clone();
                    match self.globals.get_mut(name) {
                        Some(v) => *v = _v,
                        None => {
                            let msg = format!("Undefined variable '{}'.", name);
                            return self.runtime_error(msg, frame);
                        }
                    }
                }
                OpEqual => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value::Bool(a == b))
                }
                OpGreater => self.binary_op(|a, b| Value::Bool(a > b)),
                OpLess => self.binary_op(|a, b| Value::Bool(a < b)),
                OpAdd => match (self.peek(0), self.peek(1)) {
                    (Value::Number(b), Value::Number(a)) => {
                        self.pop();
                        self.pop();
                        self.push(Value::Number(a + b));
                    }
                    (Value::Obj(a), Value::Obj(b)) => {
                        if !self.heap[*a].is_string() || !self.heap[*b].is_string() {
                            return self.runtime_error("Operands must be two strings.", frame);
                        }
                        self.pop();
                        self.pop();
                        let s = self.heap[*a].as_string().to_owned() + self.heap[*b].as_string();
                        self.push(Value::Obj(Obj::alloc_string(&mut self.heap, s)))
                    }
                    _ => {
                        return self
                            .runtime_error("Operands must be two numbers or two strings.", frame)
                    }
                },
                OpSubtract => self.binary_op(|a, b| Value::Number(a - b)),
                OpMultiply => self.binary_op(|a, b| Value::Number(a * b)),
                OpDivide => self.binary_op(|a, b| Value::Number(a / b)),
                OpNot => {
                    let v = Value::Bool(self.pop().is_falsey());
                    self.push(v)
                }
                OpNegate => match self.peek(0) {
                    Value::Number(_) => {
                        let v = Value::Number(-self.pop().as_number());
                        self.push(v)
                    }
                    _ => return self.runtime_error("Operand must be a number.", frame),
                },
                OpPrint => {
                    println!("{}", self.pop());
                }
                OpJump => {
                    let offset = Self::read_u16(frame);
                    frame.ip += offset as usize;
                }
                OpJumpIfFalse => {
                    let offset = Self::read_u16(frame);
                    if self.peek(0).is_falsey() {
                        frame.ip += offset as usize;
                    }
                }
                OpLoop => {
                    let offset = Self::read_u16(frame);
                    frame.ip -= offset as usize;
                }
                OpReturn => break,
            }
        }
        self.reset_stack();
        Ok(())
    }

    fn frame_slots(&mut self, frame: &mut CallFrame<'_>) -> &mut [Value] {
        &mut self.stack[frame.base_slot..]
    }

    fn read_byte(frame: &mut CallFrame<'_>) -> u8 {
        let b = frame.function.chunk.code[frame.ip];
        frame.ip += 1;
        b
    }

    fn read_u16(frame: &mut CallFrame<'_>) -> u16 {
        frame.ip += 2;
        let i = frame.ip;
        (frame.function.chunk.code[i - 2] as u16) << 8 | frame.function.chunk.code[i - 1] as u16
    }

    fn read_constant(frame: &mut CallFrame<'_>) -> Value {
        let byte = Self::read_byte(frame) as usize;
        frame.function.chunk.constants[byte]
    }

    fn binary_op<F>(&mut self, f: F)
    where
        F: Fn(f64, f64) -> Value,
    {
        let b = self.pop().as_number();
        let a = self.pop().as_number();
        self.push(f(a, b));
    }

    fn reset_stack(&mut self) {
        self.stack.drain(..);
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    // fn pop2(&mut self) -> (&Value, &Value) {
    //     self.stack_top -= 1;
    //     let b = &self.stack[self.stack_top];
    //     self.stack_top -= 1;
    //     let a = &self.stack[self.stack_top];
    //     (a, b)
    // }

    fn peek(&self, distance: usize) -> &Value {
        &self.stack[self.stack.len() - 1 - distance]
    }

    fn runtime_error<T: Into<String>>(
        &mut self,
        message: T,
        frame: &CallFrame<'_>,
    ) -> Result<(), LoxError> {
        let instr = frame.ip - 1;
        let line = frame.function.chunk.lines[instr];
        self.reset_stack();
        Err(LoxError::RuntimeError(format!(
            "{}\n[line {}] in script",
            message.into(),
            line
        )))
    }
}
