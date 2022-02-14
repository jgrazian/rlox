use std::cell::Cell;
use std::collections::HashMap;
use std::io::Write;
use std::time::{SystemTime, UNIX_EPOCH};

#[allow(unused_imports)]
use crate::chunk::OpCode;
use crate::chunk::OpCode::*;
use crate::compiler::Compiler;
use crate::error::LoxError;
use crate::heap::Heap;
use crate::object::{Obj, ObjClosure, ObjFunction, ObjType};
use crate::value::Value;

const FRAME_MAX: usize = 64;
const STACK_MAX: usize = 64 * FRAME_MAX;

#[derive(Debug)]
struct CallFrame<'f> {
    closure: ObjClosure,
    ip: usize,
    slot_top: usize,
    slots: &'f [Cell<Value>],
}

impl CallFrame<'_> {
    fn read_byte(&mut self, heap: &Heap<Obj>) -> u8 {
        let function = heap[self.closure.function].as_function();

        let b = function.chunk.code[self.ip];
        self.ip += 1;
        b
    }

    fn read_u16(&mut self, heap: &Heap<Obj>) -> u16 {
        let function = heap[self.closure.function].as_function();

        self.ip += 2;
        let i = self.ip;
        (function.chunk.code[i - 2] as u16) << 8 | function.chunk.code[i - 1] as u16
    }

    fn read_constant(&mut self, heap: &Heap<Obj>) -> Value {
        let function = heap[self.closure.function].as_function();

        let byte = Self::read_byte(self, heap) as usize;
        function.chunk.constants[byte]
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
        let mut vm = Self {
            stack: vec![Cell::new(Value::Nil); STACK_MAX],
            heap: Heap::new(),
            globals: HashMap::new(),
        };

        vm.define_native("clock", clock_native);
        vm
    }

    pub fn interpret<S: Write>(&mut self, source: &str, stream: &mut S) -> Result<(), LoxError> {
        let function = Compiler::new(source, &mut self.heap).compile()?;
        let function_value = Value::Obj(Obj::alloc_function(&mut self.heap, function));
        self.stack[1] = Cell::new(function_value);

        let closure = ObjClosure {
            function: function_value.as_obj(),
            upvalues: Vec::new(),
        };
        let value = Value::Obj(Obj::alloc_closure(&mut self.heap, closure));
        self.stack[0] = Cell::new(value);

        self.run(stream)
    }

    fn run<S: Write>(&mut self, stream: &mut S) -> Result<(), LoxError> {
        let mut frames = vec![CallFrame {
            closure: self
                .heap
                .get(self.stack[0].get().as_obj())
                .unwrap()
                .as_closure()
                .clone(),
            ip: 0,
            slot_top: 1,
            slots: &self.stack[0..],
        }];
        let mut frame = frames.last_mut().unwrap();

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
                eprintln!("{}", frame.function.chunk.debug_op(ip, &op, &self.heap));
            }

            match frame.read_byte(&self.heap).into() {
                OpConstant => {
                    let value = frame.read_constant(&self.heap);
                    frame.push(value);
                }
                OpNil => frame.push(Value::Nil),
                OpTrue => frame.push(Value::Bool(true)),
                OpFalse => frame.push(Value::Bool(false)),
                OpPop => {
                    frame.pop();
                }
                OpGetLocal => {
                    let v = frame.slots[frame.read_byte(&self.heap) as usize].get();
                    frame.push(v);
                }
                OpSetLocal => {
                    frame.slots[frame.read_byte(&self.heap) as usize].set(frame.peek(0));
                }
                OpGetGlobal => {
                    let name = self.heap[frame.read_constant(&self.heap).as_obj()].as_string();
                    match self.globals.get(name) {
                        Some(v) => {
                            let v = v.clone();
                            frame.push(v)
                        }
                        None => {
                            let msg = format!("Undefined variable '{}'.", name);
                            return self.runtime_error(msg, &frames);
                        }
                    }
                }
                OpDefineGlobal => {
                    let name = self.heap[frame.read_constant(&self.heap).as_obj()].as_string();
                    self.globals.insert(name.to_owned(), frame.peek(0));
                    frame.pop();
                }
                OpSetGlobal => {
                    let name = self.heap[frame.read_constant(&self.heap).as_obj()].as_string();
                    let _v = frame.peek(0);
                    match self.globals.get_mut(name) {
                        Some(v) => *v = _v,
                        None => {
                            let msg = format!("Undefined variable '{}'.", name);
                            return self.runtime_error(msg, &frames);
                        }
                    }
                }
                OpGetUpValue => {}
                OpSetUpValue => {}
                OpEqual => {
                    let b = frame.pop();
                    let a = frame.pop();
                    frame.push(Value::Bool(Value::equal(&a, &b, &self.heap)))
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
                            return self.runtime_error("Operands must be two strings.", &frames);
                        }
                        frame.pop();
                        frame.pop();
                        let s = self.heap[b].as_string().to_owned() + self.heap[a].as_string();
                        frame.push(Value::Obj(Obj::alloc_string(&mut self.heap, s)))
                    }
                    _ => {
                        return self
                            .runtime_error("Operands must be two numbers or two strings.", &frames)
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
                    _ => return self.runtime_error("Operand must be a number.", &frames),
                },
                OpPrint => {
                    writeln!(stream, "{}", frame.pop().print(&self.heap))
                        .expect("Error writing to stream.");
                }
                OpJump => {
                    let offset = frame.read_u16(&self.heap);
                    frame.ip += offset as usize;
                }
                OpJumpIfFalse => {
                    let offset = frame.read_u16(&self.heap);
                    if frame.peek(0).is_falsey() {
                        frame.ip += offset as usize;
                    }
                }
                OpLoop => {
                    let offset = frame.read_u16(&self.heap);
                    frame.ip -= offset as usize;
                }
                OpCall => {
                    let arg_count = frame.read_byte(&self.heap) as usize;
                    self.call_value(frame.peek(arg_count), arg_count, &mut frames)?;
                    frame = frames.last_mut().unwrap();
                }
                OpClosure => {
                    let function = frame.read_constant(&self.heap).as_obj();
                    let closure = ObjClosure {
                        function,
                        upvalues: Vec::new(),
                    };
                    let value = Value::Obj(Obj::alloc_closure(&mut self.heap, closure));
                    frame.push(value);

                    // let closure = self.heap.get(frame.peek(0).as_obj()).unwrap().as_closure();
                    // for i in 0..closure.upvalues.len() {
                    //     let is_local = frame.read_byte() != 0;
                    //     let index = frame.read_byte() as usize;

                    //     if is_local {
                    //         let value_ptr: *mut Value = &mut value!(index);
                    //         let upvalue = self.capture_upvalue(value_ptr, out_stream);
                    //         self.peek(0).as_closure().upvalues[i] = upvalue;
                    //     } else {
                    //         self.peek(0).as_closure().upvalues[i] =
                    //             unsafe { (*(*frame).closure).as_closure().upvalues[index] }
                    //     }
                    // }
                }
                OpCloseUpvalue => {
                    // let ptr: *mut Value = &mut self.stack[self.stack_top - 1];
                    // self.close_upvalues(ptr);
                    // self.pop();
                }
                OpReturn => {
                    let result = frame.pop();
                    frames.pop();
                    if frames.is_empty() {
                        return Ok(());
                    }
                    frame = frames.last_mut().unwrap();
                    frame.push(result);
                }
            }
        }
    }

    fn call_value(
        &self,
        callee: Value,
        arg_count: usize,
        frames: &mut Vec<CallFrame<'_>>,
    ) -> Result<(), LoxError> {
        match callee {
            Value::Obj(o) => match &self.heap[o].value {
                ObjType::Closure(closure) => {
                    self.call(closure, arg_count, frames)?;
                    return Ok(());
                }
                ObjType::Native(n) => {
                    let slots = &frames.last().unwrap().slots[1..1 + arg_count];
                    let result = (n.function)(arg_count, slots);
                    frames.last().unwrap().slots[1].set(result);
                    return Ok(());
                }
                _ => (),
            },
            _ => (),
        };
        self.runtime_error("Can only call functions and classes.", frames)
    }

    fn call(
        &self,
        closure: &ObjClosure,
        arg_count: usize,
        frames: &mut Vec<CallFrame<'_>>,
    ) -> Result<(), LoxError> {
        let function = self.heap[closure.function].as_function();
        if arg_count != function.arity {
            let msg = format!(
                "Expected {} arguments but got {}.",
                function.arity, arg_count
            );
            return self.runtime_error(msg, frames);
        }

        if frames.len() == FRAME_MAX {
            return self.runtime_error("stack overflow", frames);
        }

        let prev = frames.last_mut().unwrap();
        let frame = CallFrame {
            closure: closure.clone(),
            ip: 0,
            slot_top: arg_count + 1,
            slots: &prev.slots[prev.slot_top - arg_count - 1..],
        };
        prev.slot_top = prev.slot_top - arg_count - 1;

        frames.push(frame);

        Ok(())
    }

    fn runtime_error<M: Into<String>>(
        &self,
        message: M,
        frames: &[CallFrame<'_>],
    ) -> Result<(), LoxError> {
        let mut msg = format!("{}\n", message.into());

        for f in frames.iter().rev() {
            let instr = f.ip - 1;
            let function = self.heap[f.closure.function].as_function();
            let line = function.chunk.lines[instr];

            if let Some(name) = &function.name {
                msg.push_str(&format!("[line {}] in {}\n", line, name));
            } else {
                msg.push_str(&format!("[line {}] in <script>\n", line));
            }
        }

        Err(LoxError::RuntimeError(msg))
    }

    fn define_native<M: Into<String>>(
        &mut self,
        name: M,
        function: fn(usize, &[Cell<Value>]) -> Value,
    ) {
        let value = Value::Obj(Obj::alloc_native(&mut self.heap, function));
        self.stack[0] = Cell::new(value);
        self.globals.insert(name.into(), self.stack[0].get());
        self.stack[0] = Cell::new(Value::Nil);
    }
}

fn clock_native(_arg_count: usize, _args: &[Cell<Value>]) -> Value {
    let time = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs_f64();
    Value::Number(time)
}
