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
    fn function<'s>(&self, heap: &'s Heap) -> &'s ObjFunction {
        &heap[heap[self.closure].as_closure().function].as_function()
    }
}

pub struct Vm {
    stack: Vec<Cell<Value>>,
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
                let function = self.function(&env.heap);
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
                let ip = self.frames.last().unwrap().ip.get();
                let op = function.chunk.code[ip].into();
                eprintln!("{}", function.chunk.debug_op(ip, &op, &env.heap).1);
            }

            match self.read_byte(&env.heap).into() {
                OpConstant => {
                    let value = self.read_constant(&env.heap);
                    self.push(value);
                }
                OpNil => self.push(Value::Nil),
                OpTrue => self.push(Value::Bool(true)),
                OpFalse => self.push(Value::Bool(false)),
                OpPop => {
                    self.pop();
                }
                OpGetLocal => {
                    let v = self.slots()[self.read_byte(&env.heap) as usize].get();
                    self.push(v);
                }
                OpSetLocal => {
                    self.slots()[self.read_byte(&env.heap) as usize].set(self.peek(0));
                }
                OpGetGlobal => {
                    let name = self.string(&env.heap);
                    match self.globals.get(name) {
                        Some(v) => {
                            let v = v.clone();
                            self.push(v)
                        }
                        None => {
                            let msg = format!("Undefined variable '{}'.", name);
                            return self.runtime_error(msg, env);
                        }
                    }
                }
                OpDefineGlobal => {
                    let name = self.string(&env.heap);
                    self.globals.insert(name.to_owned(), self.peek(0));
                    self.pop();
                }
                OpSetGlobal => {
                    let name = self.string(&env.heap);
                    let _v = self.peek(0);
                    match self.globals.get_mut(name) {
                        Some(v) => *v = _v,
                        None => {
                            let msg = format!("Undefined variable '{}'.", name);
                            return self.runtime_error(msg, env);
                        }
                    }
                }
                OpGetUpvalue => {
                    let slot = self.read_byte(&env.heap);
                    let value = match env.heap[self.closure(&env.heap).upvalues[slot as usize]]
                        .as_upvalue()
                        .state
                    {
                        UpvalueState::Open(location) => self.stack[location].get(),
                        UpvalueState::Closed(value) => value,
                    };
                    self.push(value);
                }
                OpSetUpvalue => {
                    let slot = self.read_byte(&env.heap);
                    let upvalue = self.closure(&env.heap).upvalues[slot as usize];
                    match env.heap[upvalue].as_upvalue_mut().state {
                        UpvalueState::Open(location) => self.stack[location].set(self.peek(0)),
                        UpvalueState::Closed(ref mut value) => *value = self.peek(0),
                    }
                }
                OpGetProperty => {
                    if !env.heap[self.peek(0).as_obj()].is_instance() {
                        return self.runtime_error("Only instances have properties.", env);
                    }

                    let instance = env.heap[self.peek(0).as_obj()].as_instance();
                    let name = env.heap[self.read_constant(&env.heap).as_obj()]
                        .as_string()
                        .to_owned();

                    if let Some(prop_name) = instance.fields.get(&name) {
                        self.pop();
                        self.push(*prop_name);
                        continue;
                    }

                    self.bind_method(instance.klass, &name, env)?;
                }
                OpSetProperty => {
                    if !env.heap[self.peek(1).as_obj()].is_instance() {
                        return self.runtime_error("Only instances have properties.", env);
                    }

                    let name = env.heap[self.read_constant(&env.heap).as_obj()]
                        .as_string()
                        .to_owned();
                    let instance = env.heap[self.peek(1).as_obj()].as_instance_mut();
                    let ins_value = self.peek(0);

                    instance.fields.insert(name, ins_value);
                    let value = self.pop();
                    self.pop();
                    self.push(value);
                }
                OpGetSuper => {
                    return self.runtime_error("No superclasses! Sorry :)", env);
                    // let name = env.heap[self.read_constant(&env.heap).as_obj()]
                    //     .as_string()
                    //     .to_owned();
                    // let superclass = self.pop().as_obj();

                    // self.bind_method(superclass, &name, env)?;
                }
                OpEqual => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value::Bool(Value::equal(&a, &b, &env.heap)))
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
                        if !env.heap[a].is_string() || !env.heap[b].is_string() {
                            return self.runtime_error("Operands must be two strings.", env);
                        }

                        let s = env.heap[b].as_string().to_owned() + env.heap[a].as_string();
                        let value = Value::Obj(env.alloc(Obj::string(s), self));
                        self.pop();
                        self.pop();
                        self.push(value);
                    }
                    _ => {
                        return self
                            .runtime_error("Operands must be two numbers or two strings.", env)
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
                    _ => return self.runtime_error("Operand must be a number.", env),
                },
                OpPrint => {
                    writeln!(stream, "{}", self.pop().print(&env.heap))
                        .expect("Error writing to stream.");
                }
                OpJump => {
                    let offset = self.read_u16(&env.heap);
                    let ip = &self.frame().ip;
                    ip.set(ip.get() + offset as usize);
                }
                OpJumpIfFalse => {
                    let offset = self.read_u16(&env.heap);
                    if self.peek(0).is_falsey() {
                        let ip = &self.frame().ip;
                        ip.set(ip.get() + offset as usize);
                    }
                }
                OpLoop => {
                    let offset = self.read_u16(&env.heap);
                    let ip = &self.frame().ip;
                    ip.set(ip.get() - offset as usize);
                }
                OpCall => {
                    let arg_count = self.read_byte(&env.heap) as usize;
                    self.call_value(self.peek(arg_count), arg_count, env)?;
                }
                OpInvoke => {
                    let method = env.heap[self.read_constant(&env.heap).as_obj()]
                        .as_string()
                        .to_owned();
                    let arg_count = self.read_byte(&env.heap) as usize;
                    self.invoke(&method, arg_count, env)?;
                }
                OpClosure => {
                    let function = self.read_constant(&env.heap).as_obj();
                    let upvalue_count = env.heap[function].as_function().upvalue_count;

                    let closure = ObjClosure {
                        function,
                        upvalues: vec![HeapKey::default(); upvalue_count],
                    };
                    let value = Value::Obj(env.alloc(Obj::closure(closure), self));
                    self.push(value);

                    for i in 0..upvalue_count {
                        let is_local = self.read_byte(&env.heap) != 0;
                        let index = self.read_byte(&env.heap) as usize;

                        if is_local {
                            let upvalue_ptr =
                                self.capture_upvalue(self.frame().slot_base + index, env);
                            env.heap[self.peek(0).as_obj()].as_closure_mut().upvalues[i] =
                                upvalue_ptr;
                        } else {
                            env.heap[self.peek(0).as_obj()].as_closure_mut().upvalues[i] =
                                self.closure(&env.heap).upvalues[index];
                        }
                    }
                }
                OpCloseUpvalue => {
                    let frame = self.frame();
                    let last = frame.slot_base + frame.slot_top.get() - 1;
                    Self::close_upvalues(&self.stack, &mut env.heap, &mut self.open_upvalues, last);
                    self.pop();
                }
                OpReturn => {
                    let result = self.pop();
                    let last = self.frame().slot_base;
                    Self::close_upvalues(&self.stack, &mut env.heap, &mut self.open_upvalues, last);
                    self.frames.pop();
                    if self.frames.is_empty() {
                        return Ok(());
                    }
                    self.push(result);
                }
                OpClass => {
                    let name = self.string(&env.heap).to_owned();
                    let value = Value::Obj(env.alloc(Obj::class(name), self));
                    self.push(value);
                }
                OpInherit => {
                    let superclass = self.peek(1).as_obj();
                    if !env.heap[superclass].is_class() {
                        return self.runtime_error("Superclass must be a class.", env);
                    }

                    let superclass_methods = env.heap[superclass].as_class().methods.clone();
                    let subclass = env.heap[self.peek(0).as_obj()].as_class_mut();
                    subclass.methods.extend(superclass_methods);

                    self.pop();
                }
                OpMethod => {
                    self.define_method(self.string(&env.heap).to_owned(), env)?;
                }
            }
        }
    }

    fn frame(&self) -> &CallFrame {
        self.frames.last().unwrap()
    }

    fn closure<'s>(&self, heap: &'s Heap) -> &'s ObjClosure {
        &heap[self.frame().closure].as_closure()
    }

    fn function<'s>(&self, heap: &'s Heap) -> &'s ObjFunction {
        &heap[heap[self.frame().closure].as_closure().function].as_function()
    }

    fn string<'s>(&self, heap: &'s Heap) -> &'s str {
        heap[self.read_constant(heap).as_obj()].as_string()
    }

    fn slots<'s>(&'s self) -> &'s [Cell<Value>] {
        &self.stack[self.frame().slot_base..]
    }

    fn read_byte(&self, heap: &Heap) -> u8 {
        let frame = self.frame();
        let b = self.function(heap).chunk.code[frame.ip.get()];
        frame.ip.set(frame.ip.get() + 1);
        b
    }

    fn read_u16(&self, heap: &Heap) -> u16 {
        let frame = self.frame();
        frame.ip.set(frame.ip.get() + 2);
        let i = frame.ip.get();
        (self.function(heap).chunk.code[i - 2] as u16) << 8
            | self.function(heap).chunk.code[i - 1] as u16
    }

    fn read_constant(&self, heap: &Heap) -> Value {
        let byte = Self::read_byte(self, heap) as usize;
        self.function(heap).chunk.constants[byte]
    }

    fn binary_op<F>(&self, f: F)
    where
        F: Fn(f64, f64) -> Value,
    {
        let b = self.pop().as_number();
        let a = self.pop().as_number();
        self.push(f(a, b));
    }

    fn push(&self, value: Value) {
        let frame = self.frame();
        self.stack[frame.slot_base + frame.slot_top.get()].set(value);
        frame.slot_top.set(frame.slot_top.get() + 1);
    }

    fn pop(&self) -> Value {
        let frame = self.frame();
        frame.slot_top.set(frame.slot_top.get() - 1);
        let r = self.stack[frame.slot_base + frame.slot_top.get()].get();
        self.stack[frame.slot_base + frame.slot_top.get()].set(Value::Nil);
        r
    }

    fn peek(&self, distance: usize) -> Value {
        let frame = self.frame();
        self.stack[frame.slot_base + frame.slot_top.get() - 1 - distance].get()
    }

    fn call_value(
        &mut self,
        callee: Value,
        arg_count: usize,
        env: &mut InnerEnv<'h>,
    ) -> Result<(), LoxError> {
        match callee {
            Value::Obj(o) => match &env.heap[o].value {
                ObjType::BoundMethod(b) => {
                    self.stack[self.frame().slot_top.get() - arg_count - 1].set(b.reciever);
                    return self.call(b.method, arg_count, env);
                }
                ObjType::Class(_klass) => {
                    let initializer = _klass.methods.get("init").copied();

                    let instance = Obj::instance(o);
                    let value = Value::Obj(env.alloc(instance, self));
                    self.stack[self.frame().slot_top.get() - arg_count - 1].set(value);

                    if let Some(initializer) = initializer {
                        return self.call(initializer.as_obj(), arg_count, env);
                    } else if arg_count != 0 {
                        return self.runtime_error(
                            format!("Expected 0 arguments but got {}.", arg_count),
                            env,
                        );
                    }

                    return Ok(());
                }
                ObjType::Closure(_closure) => {
                    self.call(o, arg_count, env)?;
                    return Ok(());
                }
                ObjType::Native(n) => {
                    let slots = &self.slots()[1..1 + arg_count];
                    let result = (n.function)(arg_count, slots);
                    self.slots()[1].set(result);
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

    fn invoke(
        &mut self,
        name: &String,
        arg_count: usize,
        env: &mut InnerEnv<'h>,
    ) -> Result<(), LoxError> {
        let reviever = self.peek(arg_count);

        let instance = if env.heap[reviever.as_obj()].is_instance() {
            env.heap[reviever.as_obj()].as_instance()
        } else {
            return self.runtime_error(format!("Only instances have methods."), env);
        };

        if let Some(value) = instance.fields.get(name) {
            self.stack[self.frame().slot_top.get() - arg_count - 1].set(*value);
            return self.call(value.as_obj(), arg_count, env);
        }

        self.invoke_from_class(instance.klass, name, arg_count, env)
    }

    fn invoke_from_class(
        &mut self,
        klass: HeapKey,
        name: &String,
        arg_count: usize,
        env: &mut InnerEnv<'h>,
    ) -> Result<(), LoxError> {
        let method = env.heap[klass].as_class().methods.get(name);
        if let Some(method) = method {
            self.call(method.as_obj(), arg_count, env)
        } else {
            self.runtime_error(format!("Undefined property '{}'.", name), env)
        }
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

    fn define_method(&mut self, name: String, env: &mut InnerEnv<'h>) -> Result<(), LoxError> {
        let method = self.peek(0);
        let klass = env.heap[self.peek(1).as_obj()].as_class_mut();

        klass.methods.insert(name, method);

        self.pop();
        Ok(())
    }

    fn bind_method(
        &mut self,
        klass: HeapKey,
        name: &String,
        env: &mut InnerEnv<'h>,
    ) -> Result<(), LoxError> {
        let klass_obj = env.heap[klass].as_class();
        let method = klass_obj
            .methods
            .get(name)
            .ok_or(LoxError::RuntimeError(format!(
                "Undefined property '{}'.",
                name
            )))?;

        let obj = Obj::bound_method(self.peek(0), method.as_obj());
        let bound_method = env.alloc(obj, self);

        self.pop();
        self.push(Value::Obj(bound_method));

        Ok(())
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
