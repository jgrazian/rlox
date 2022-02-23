use std::cell::Cell;
use std::collections::{BinaryHeap, HashMap};
use std::io::Write;
use std::time::{SystemTime, UNIX_EPOCH};

#[allow(unused_imports)]
use crate::chunk::OpCode;
use crate::chunk::OpCode::*;
use crate::enviroment::{InnerEnv, Mark, TargetType};
use crate::error::LoxError;
use crate::heap::{Heap, HeapKey};
use crate::object::{Obj, ObjClosure, ObjFunction, ObjType, UpvalueState};
use crate::value::Value;

const FRAME_MAX: usize = 64;
const STACK_MAX: usize = 64 * FRAME_MAX;

#[derive(Debug)]
struct CallFrame {
    closure: HeapKey,
    ip: Cell<usize>,
    slot_base: usize,
    slot_top: Cell<usize>,
}

impl CallFrame {
    fn closure<'s>(&self, heap: &'s Heap) -> &'s ObjClosure {
        &heap[self.closure].as_closure()
    }

    fn function<'s>(&self, heap: &'s Heap) -> &'s ObjFunction {
        &heap[heap[self.closure].as_closure().function].as_function()
    }

    fn slots<'s>(&self, stack: &'s [Cell<Value>]) -> &'s [Cell<Value>] {
        &stack[self.slot_base..]
    }

    fn read_byte(&self, heap: &Heap) -> u8 {
        let b = self.function(heap).chunk.code[self.ip.get()];
        self.ip.set(self.ip.get() + 1);
        b
    }

    fn read_u16(&self, heap: &Heap) -> u16 {
        self.ip.set(self.ip.get() + 2);
        let i = self.ip.get();
        (self.function(heap).chunk.code[i - 2] as u16) << 8
            | self.function(heap).chunk.code[i - 1] as u16
    }

    fn read_constant(&self, heap: &Heap) -> Value {
        let byte = Self::read_byte(self, heap) as usize;
        self.function(heap).chunk.constants[byte]
    }

    fn binary_op<F>(&self, stack: &[Cell<Value>], f: F)
    where
        F: Fn(f64, f64) -> Value,
    {
        let b = self.pop(stack).as_number();
        let a = self.pop(stack).as_number();
        self.push(stack, f(a, b));
    }

    fn push(&self, stack: &[Cell<Value>], value: Value) {
        stack[self.slot_base + self.slot_top.get()].set(value);
        self.slot_top.set(self.slot_top.get() + 1);
    }

    fn pop(&self, stack: &[Cell<Value>]) -> Value {
        self.slot_top.set(self.slot_top.get() - 1);
        stack[self.slot_base + self.slot_top.get()].get()
    }

    fn peek(&self, stack: &[Cell<Value>], distance: usize) -> Value {
        stack[self.slot_base + self.slot_top.get() - 1 - distance].get()
    }
}

pub struct Vm {
    pub stack: Vec<Cell<Value>>,
    globals: HashMap<String, Value>,
    open_upvalues: BinaryHeap<(usize, HeapKey)>, // (slot_index, heap_index)
    frames: Vec<CallFrame>,
}

impl Mark for Vm {
    fn mark(&self, heap: &mut Heap) -> TargetType {
        // Stack
        for i in 0..self.stack.len() {
            let value = self.stack[i].get();
            heap.gc_mark_value(value);
        }

        // Frames
        for frame in self.frames.iter() {
            heap.gc_mark(frame.closure);
        }

        // Open upvalues
        for (_, upvalue) in self.open_upvalues.iter() {
            heap.gc_mark(*upvalue);
        }

        // Globals
        let values = self.globals.iter().map(|(_, v)| *v).collect::<Vec<_>>();
        for value in values {
            heap.gc_mark_value(value);
        }

        TargetType::Vm
    }
}

impl<'h> Vm {
    pub fn new(env: &mut InnerEnv<'h>) -> Self {
        let mut vm = Self {
            stack: vec![Cell::new(Value::Nil); STACK_MAX],
            globals: HashMap::new(),
            open_upvalues: BinaryHeap::new(),
            frames: Vec::with_capacity(FRAME_MAX),
        };

        vm.define_native("clock", clock_native, env);
        vm
    }

    pub fn interpret<S: Write>(
        &mut self,
        function_ptr: Value,
        stream: &mut S,
        env: &mut InnerEnv<'h>,
    ) -> Result<(), LoxError> {
        self.stack[1] = Cell::new(function_ptr);

        let closure = ObjClosure {
            function: function_ptr.as_obj(),
            upvalues: Vec::new(),
        };
        let value = Value::Obj(env.alloc(Obj::closure(closure), self));
        self.stack[0] = Cell::new(value);

        self.frames.push(CallFrame {
            closure: self.stack[0].get().as_obj(),
            ip: Cell::new(0),
            slot_base: 0,
            slot_top: Cell::new(1),
        });

        self.run(stream, env)
    }

    fn run<S: Write>(&mut self, stream: &mut S, env: &mut InnerEnv<'h>) -> Result<(), LoxError> {
        loop {
            #[cfg(feature = "debug_trace_execution")]
            {
                let function = env.heap[self.frame().closure.function].as_function();
                eprint!("          ");
                let end = self
                    .stack
                    .iter()
                    .rposition(|x| x.get() != Value::Nil)
                    .unwrap_or(0);
                self.stack[0..=end]
                    .iter()
                    .for_each(|v| eprint!("[ {:>3} ]", v.get().print(&env.heap)));
                eprint!("\n");
                let ip = self.frame().ip;
                let op = function.chunk.code[ip].into();
                eprintln!("{}", function.chunk.debug_op(ip, &op, &env.heap).1);
            }

            match self.frame().read_byte(&env.heap).into() {
                OpConstant => {
                    let value = self.frame().read_constant(&env.heap);
                    self.frame().push(&self.stack, value);
                }
                OpNil => self.frame().push(&self.stack, Value::Nil),
                OpTrue => self.frame().push(&self.stack, Value::Bool(true)),
                OpFalse => self.frame().push(&self.stack, Value::Bool(false)),
                OpPop => {
                    self.frame().pop(&self.stack);
                }
                OpGetLocal => {
                    let v = self.frame().slots(&self.stack)
                        [self.frame().read_byte(&env.heap) as usize]
                        .get();
                    self.frame().push(&self.stack, v);
                }
                OpSetLocal => {
                    self.frame().slots(&self.stack)[self.frame().read_byte(&env.heap) as usize]
                        .set(self.frame().peek(&self.stack, 0));
                }
                OpGetGlobal => {
                    let name = env.heap[self.frame().read_constant(&env.heap).as_obj()].as_string();
                    match self.globals.get(name) {
                        Some(v) => {
                            let v = v.clone();
                            self.frame().push(&self.stack, v)
                        }
                        None => {
                            let msg = format!("Undefined variable '{}'.", name);
                            return self.runtime_error(msg, env);
                        }
                    }
                }
                OpDefineGlobal => {
                    let name = env.heap[self.frame().read_constant(&env.heap).as_obj()].as_string();
                    self.globals
                        .insert(name.to_owned(), self.frame().peek(&self.stack, 0));
                    self.frame().pop(&self.stack);
                }
                OpSetGlobal => {
                    let name = env.heap[self.frame().read_constant(&env.heap).as_obj()].as_string();
                    let _v = self.frame().peek(&self.stack, 0);
                    match self.globals.get_mut(name) {
                        Some(v) => *v = _v,
                        None => {
                            let msg = format!("Undefined variable '{}'.", name);
                            return self.runtime_error(msg, env);
                        }
                    }
                }
                OpGetUpvalue => {
                    let slot = self.frame().read_byte(&env.heap);
                    let value = match env.heap
                        [self.frame().closure(&env.heap).upvalues[slot as usize]]
                        .as_upvalue()
                        .state
                    {
                        UpvalueState::Open(location) => self.stack[location].get(),
                        UpvalueState::Closed(value) => value,
                    };
                    self.frame().push(&self.stack, value);
                }
                OpSetUpvalue => {
                    let slot = self.frame().read_byte(&env.heap);
                    let upvalue = self.frame().closure(&env.heap).upvalues[slot as usize];
                    match env.heap[upvalue].as_upvalue_mut().state {
                        UpvalueState::Open(location) => {
                            self.stack[location].set(self.frame().peek(&self.stack, 0))
                        }
                        UpvalueState::Closed(ref mut value) => {
                            *value = self.frame().peek(&self.stack, 0)
                        }
                    }
                }
                OpEqual => {
                    let b = self.frame().pop(&self.stack);
                    let a = self.frame().pop(&self.stack);
                    self.frame()
                        .push(&self.stack, Value::Bool(Value::equal(&a, &b, &env.heap)))
                }
                OpGreater => self
                    .frame()
                    .binary_op(&self.stack, |a, b| Value::Bool(a > b)),
                OpLess => self
                    .frame()
                    .binary_op(&self.stack, |a, b| Value::Bool(a < b)),
                OpAdd => match (
                    self.frame().peek(&self.stack, 0),
                    self.frame().peek(&self.stack, 1),
                ) {
                    (Value::Number(b), Value::Number(a)) => {
                        self.frame().pop(&self.stack);
                        self.frame().pop(&self.stack);
                        self.frame().push(&self.stack, Value::Number(a + b));
                    }
                    (Value::Obj(a), Value::Obj(b)) => {
                        if !env.heap[a].is_string() || !env.heap[b].is_string() {
                            return self.runtime_error("Operands must be two strings.", env);
                        }

                        let s = env.heap[b].as_string().to_owned() + env.heap[a].as_string();
                        let value = Value::Obj(env.alloc(Obj::string(s), self));
                        self.frame().pop(&self.stack);
                        self.frame().pop(&self.stack);
                        self.frame().push(&self.stack, value);
                    }
                    _ => {
                        return self
                            .runtime_error("Operands must be two numbers or two strings.", env)
                    }
                },
                OpSubtract => self
                    .frame()
                    .binary_op(&self.stack, |a, b| Value::Number(a - b)),
                OpMultiply => self
                    .frame()
                    .binary_op(&self.stack, |a, b| Value::Number(a * b)),
                OpDivide => self
                    .frame()
                    .binary_op(&self.stack, |a, b| Value::Number(a / b)),
                OpNot => {
                    let v = Value::Bool(self.frame().pop(&self.stack).is_falsey());
                    self.frame().push(&self.stack, v)
                }
                OpNegate => match self.frame().peek(&self.stack, 0) {
                    Value::Number(_) => {
                        let v = Value::Number(-self.frame().pop(&self.stack).as_number());
                        self.frame().push(&self.stack, v)
                    }
                    _ => return self.runtime_error("Operand must be a number.", env),
                },
                OpPrint => {
                    writeln!(
                        stream,
                        "{}",
                        self.frame().pop(&self.stack,).print(&env.heap)
                    )
                    .expect("Error writing to stream.");
                }
                OpJump => {
                    let offset = self.frame().read_u16(&env.heap);
                    self.frame().ip.set(self.frame().ip.get() + offset as usize);
                }
                OpJumpIfFalse => {
                    let offset = self.frame().read_u16(&env.heap);
                    if self.frame().peek(&self.stack, 0).is_falsey() {
                        self.frame().ip.set(self.frame().ip.get() + offset as usize);
                    }
                }
                OpLoop => {
                    let offset = self.frame().read_u16(&env.heap);
                    self.frame().ip.set(self.frame().ip.get() - offset as usize);
                }
                OpCall => {
                    let arg_count = self.frame().read_byte(&env.heap) as usize;
                    self.call_value(self.frame().peek(&self.stack, arg_count), arg_count, env)?;
                }
                OpClosure => {
                    let function = self.frame().read_constant(&env.heap).as_obj();
                    let upvalue_count = env.heap[function].as_function().upvalue_count;

                    let closure = ObjClosure {
                        function,
                        upvalues: vec![HeapKey::default(); upvalue_count],
                    };
                    let value = Value::Obj(env.alloc(Obj::closure(closure), self));
                    self.frame().push(&self.stack, value);

                    for i in 0..upvalue_count {
                        let is_local = self.frame().read_byte(&env.heap) != 0;
                        let index = self.frame().read_byte(&env.heap) as usize;

                        if is_local {
                            let upvalue_ptr =
                                self.capture_upvalue(self.frame().slot_base + index, env);
                            env.heap[self.frame().peek(&self.stack, 0).as_obj()]
                                .as_closure_mut()
                                .upvalues[i] = upvalue_ptr;
                        } else {
                            env.heap[self.frame().peek(&self.stack, 0).as_obj()]
                                .as_closure_mut()
                                .upvalues[i] = self.frame().closure(&env.heap).upvalues[index];
                        }
                    }
                }
                OpCloseUpvalue => {
                    let last = self.frame().slot_base + self.frame().slot_top.get() - 1;
                    Self::close_upvalues(&self.stack, &mut env.heap, &mut self.open_upvalues, last);
                    self.frame().pop(&self.stack);
                }
                OpReturn => {
                    let result = self.frame().pop(&self.stack);
                    let last = self.frame().slot_base;
                    Self::close_upvalues(&self.stack, &mut env.heap, &mut self.open_upvalues, last);
                    self.frames.pop();
                    if self.frames.is_empty() {
                        return Ok(());
                    }
                    self.frame().push(&self.stack, result);
                }
            }
        }
    }

    fn frame(&self) -> &CallFrame {
        self.frames.last().unwrap()
    }

    fn call_value(
        &mut self,
        callee: Value,
        arg_count: usize,
        env: &mut InnerEnv<'h>,
    ) -> Result<(), LoxError> {
        match callee {
            Value::Obj(o) => match &env.heap[o].value {
                ObjType::Closure(_closure) => {
                    self.call(o, arg_count, env)?;
                    return Ok(());
                }
                ObjType::Native(n) => {
                    let slots = &self.frames.last().unwrap().slots(&self.stack)[1..1 + arg_count];
                    let result = (n.function)(arg_count, slots);
                    self.frames.last().unwrap().slots(&self.stack)[1].set(result);
                    return Ok(());
                }
                _ => (),
            },
            _ => (),
        };
        self.runtime_error("Can only call functions and classes.", env)
    }

    fn call(
        &mut self,
        closure: HeapKey,
        arg_count: usize,
        env: &mut InnerEnv<'h>,
    ) -> Result<(), LoxError> {
        let function_key = env.heap[closure].as_closure().function;
        let arity = env.heap[function_key].as_function().arity;
        if arg_count != arity {
            let msg = format!("Expected {} arguments but got {}.", arity, arg_count);
            return self.runtime_error(msg, env);
        }

        if self.frames.len() == FRAME_MAX {
            return self.runtime_error("stack overflow", env);
        }

        let prev = self.frames.last().unwrap();
        let frame = CallFrame {
            closure,
            ip: Cell::new(0),
            slot_base: prev.slot_base + prev.slot_top.get() - arg_count - 1,
            slot_top: Cell::new(arg_count + 1),
        };
        prev.slot_top.set(prev.slot_top.get() - arg_count - 1);

        self.frames.push(frame);

        Ok(())
    }

    fn runtime_error<M: Into<String>>(
        &self,
        message: M,
        env: &mut InnerEnv<'h>,
    ) -> Result<(), LoxError> {
        let mut msg = format!("{}\n", message.into());

        for f in self.frames.iter().rev() {
            let instr = f.ip.get() - 1;
            let function = f.function(&env.heap);
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
        env: &mut InnerEnv<'h>,
    ) {
        let value = Value::Obj(env.alloc(Obj::native(function), self));
        self.stack[0] = Cell::new(value);
        self.globals.insert(name.into(), self.stack[0].get());
        self.stack[0] = Cell::new(Value::Nil);
    }

    fn capture_upvalue(&mut self, local: usize, env: &mut InnerEnv<'h>) -> HeapKey {
        for (location, upvalue_ptr) in self.open_upvalues.iter() {
            if location < &local {
                break;
            }
            if location == &local {
                return *upvalue_ptr;
            }
        }

        let upvalue_ptr = env.alloc(Obj::upvalue(UpvalueState::Open(local)), self);
        self.open_upvalues.push((local, upvalue_ptr));
        upvalue_ptr
    }

    fn close_upvalues(
        stack: &Vec<Cell<Value>>,
        heap: &mut Heap,
        open_upvalues: &mut BinaryHeap<(usize, HeapKey)>,
        last: usize,
    ) {
        loop {
            match open_upvalues.peek() {
                Some((_loc, _)) if _loc >= &last => {
                    let (location, upvalue_ptr) = open_upvalues.pop().unwrap();
                    heap[upvalue_ptr] = Obj::upvalue(UpvalueState::Closed(stack[location].get()))
                }
                _ => {
                    break;
                }
            }
        }
    }
}

fn clock_native(_arg_count: usize, _args: &[Cell<Value>]) -> Value {
    let time = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs_f64();
    Value::Number(time)
}
