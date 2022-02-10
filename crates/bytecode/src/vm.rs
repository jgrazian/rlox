use std::cell::Cell;
use std::collections::HashMap;
use std::io::Write;

use crate::chunk::OpCode::{self, *};
use crate::compiler::Compiler;
use crate::error::LoxError;
use crate::heap::Heap;
use crate::object::{Obj, ObjFunction};
use crate::value::Value;

const STACK_MAX: usize = 64;

#[derive(Debug)]
struct CallFrame<'f> {
    function: ObjFunction,
    ip: usize,
    slot_top: usize,
    slots: &'f [Cell<Value>],
}

impl CallFrame<'_> {
    fn read_byte(&mut self) -> u8 {
        let b = self.function.chunk.code[self.ip];
        self.ip += 1;
        b
    }

    fn read_u16(&mut self) -> u16 {
        self.ip += 2;
        let i = self.ip;
        (self.function.chunk.code[i - 2] as u16) << 8 | self.function.chunk.code[i - 1] as u16
    }

    fn read_constant(&mut self) -> Value {
        let byte = Self::read_byte(self) as usize;
        self.function.chunk.constants[byte]
    }

    fn binary_op<F>(&mut self, f: F)
    where
        F: Fn(f64, f64) -> Value,
    {
        let b = Self::pop(self).as_number();
        let a = Self::pop(self).as_number();
        Self::push(self, f(a, b));
    }

    fn push(&mut self, value: Value) {
        self.slots[self.slot_top].set(value);
        self.slot_top += 1;
    }

    fn pop(&mut self) -> Value {
        self.slot_top -= 1;
        self.slots[self.slot_top].get()
    }

    fn peek(&mut self, distance: usize) -> Value {
        self.slots[self.slot_top - 1 - distance].get()
    }
}

pub struct Vm {
    stack: Vec<Cell<Value>>,
    heap: Heap<Obj>,
    globals: HashMap<String, Value>,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            stack: vec![Cell::new(Value::Nil); STACK_MAX],
            heap: Heap::new(),
            globals: HashMap::new(),
        }
    }

    pub fn interpret<S: Write>(&mut self, source: &str, stream: &mut S) -> Result<(), LoxError> {
        let function = Compiler::new(source, &mut self.heap).compile()?;

        let value = Value::Obj(Obj::alloc_function(&mut self.heap, function));
        self.stack[0] = Cell::new(value);

        self.run(stream)
    }

    fn run<S: Write>(&mut self, stream: &mut S) -> Result<(), LoxError> {
        let mut frame = CallFrame {
            function: self
                .heap
                .get(self.stack[0].get().as_obj())
                .unwrap()
                .as_function()
                .clone(),
            ip: 0,
            slot_top: 0,
            slots: &self.stack[1..],
        };

        loop {
            #[cfg(feature = "debug_trace_execution")]
            {
                eprint!("          ");
                let end = self
                    .stack
                    .iter()
                    .rposition(|x| x.get() != Value::Nil)
                    .unwrap_or(0);
                self.stack[0..=end]
                    .iter()
                    .for_each(|v| eprint!("[ {:>3} ]", v.get().print(&self.heap)));
                eprint!("\n");
                let ip = frame.ip;
                let op = frame.function.chunk.code[ip].into();
                eprintln!("{}", frame.function.chunk.debug_op(ip, &op));
            }

            match frame.read_byte().into() {
                OpConstant => {
                    let value = frame.read_constant();
                    frame.push(value);
                }
                OpNil => frame.push(Value::Nil),
                OpTrue => frame.push(Value::Bool(true)),
                OpFalse => frame.push(Value::Bool(false)),
                OpPop => {
                    frame.pop();
                }
                OpGetLocal => {
                    let v = frame.slots[frame.read_byte() as usize].get();
                    frame.push(v);
                }
                OpSetLocal => {
                    frame.slots[frame.read_byte() as usize].set(frame.peek(0));
                }
                OpGetGlobal => {
                    let name = self.heap[frame.read_constant().as_obj()].as_string();
                    match self.globals.get(name) {
                        Some(v) => {
                            let v = v.clone();
                            frame.push(v)
                        }
                        None => {
                            let msg = format!("Undefined variable '{}'.", name);
                            return Self::runtime_error(msg, &mut frame);
                        }
                    }
                }
                OpDefineGlobal => {
                    let name = self.heap[frame.read_constant().as_obj()].as_string();
                    self.globals.insert(name.to_owned(), frame.peek(0));
                    frame.pop();
                }
                OpSetGlobal => {
                    let name = self.heap[frame.read_constant().as_obj()].as_string();
                    let _v = frame.peek(0);
                    match self.globals.get_mut(name) {
                        Some(v) => *v = _v,
                        None => {
                            let msg = format!("Undefined variable '{}'.", name);
                            return Self::runtime_error(msg, &mut frame);
                        }
                    }
                }
                OpEqual => {
                    let b = frame.pop();
                    let a = frame.pop();
                    frame.push(Value::Bool(a == b))
                }
                OpGreater => frame.binary_op(|a, b| Value::Bool(a > b)),
                OpLess => frame.binary_op(|a, b| Value::Bool(a < b)),
                OpAdd => match (frame.peek(0), frame.peek(1)) {
                    (Value::Number(b), Value::Number(a)) => {
                        frame.pop();
                        frame.pop();
                        frame.push(Value::Number(a + b));
                    }
                    (Value::Obj(a), Value::Obj(b)) => {
                        if !self.heap[a].is_string() || !self.heap[b].is_string() {
                            return Self::runtime_error(
                                "Operands must be two strings.",
                                &mut frame,
                            );
                        }
                        frame.pop();
                        frame.pop();
                        let s = self.heap[b].as_string().to_owned() + self.heap[a].as_string();
                        frame.push(Value::Obj(Obj::alloc_string(&mut self.heap, s)))
                    }
                    _ => {
                        return Self::runtime_error(
                            "Operands must be two numbers or two strings.",
                            &mut frame,
                        )
                    }
                },
                OpSubtract => frame.binary_op(|a, b| Value::Number(a - b)),
                OpMultiply => frame.binary_op(|a, b| Value::Number(a * b)),
                OpDivide => frame.binary_op(|a, b| Value::Number(a / b)),
                OpNot => {
                    let v = Value::Bool(frame.pop().is_falsey());
                    frame.push(v)
                }
                OpNegate => match frame.peek(0) {
                    Value::Number(_) => {
                        let v = Value::Number(-frame.pop().as_number());
                        frame.push(v)
                    }
                    _ => return Self::runtime_error("Operand must be a number.", &mut frame),
                },
                OpPrint => {
                    writeln!(stream, "{}", frame.pop().print(&self.heap))
                        .expect("Error writing to stream.");
                }
                OpJump => {
                    let offset = frame.read_u16();
                    frame.ip += offset as usize;
                }
                OpJumpIfFalse => {
                    let offset = frame.read_u16();
                    if frame.peek(0).is_falsey() {
                        frame.ip += offset as usize;
                    }
                }
                OpLoop => {
                    let offset = frame.read_u16();
                    frame.ip -= offset as usize;
                }
                OpReturn => break,
            }
        }
        // self.reset_stack();
        Ok(())
    }

    // fn reset_stack(&mut self) {
    //     self.stack.fill(Cell::new(Value::Nil));
    // }

    fn runtime_error<M: Into<String>>(message: M, frame: &CallFrame<'_>) -> Result<(), LoxError> {
        let instr = frame.ip - 1;
        let line = frame.function.chunk.lines[instr];
        Err(LoxError::RuntimeError(format!(
            "{}\n[line {}] in script",
            message.into(),
            line
        )))
    }
}
