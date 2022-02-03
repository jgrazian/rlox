use core::ptr;
use std::collections::HashMap;

use crate::chunk::OpCode::*;
use crate::compiler::{compile, U8_COUNT};
use crate::error::LoxError;
use crate::object::{Function, Obj};
use crate::value::Value;

const FRAME_MAX: usize = 64;
const STACK_MAX: usize = FRAME_MAX * U8_COUNT;

#[derive(Debug, Clone, Copy, PartialEq)]
struct CallFrame {
    function: *const Function,
    ip: usize,
    slot_base: usize,
}

impl Default for CallFrame {
    fn default() -> Self {
        Self {
            function: ptr::null(),
            ip: usize::default(),
            slot_base: usize::default(),
        }
    }
}

pub struct Vm {
    frames: [CallFrame; FRAME_MAX],
    frame_count: usize,

    stack: [Value; STACK_MAX],
    stack_top: usize,
    globals: HashMap<String, Value>,
}

impl Vm {
    pub fn new() -> Self {
        const NIL: Value = Value::Nil;
        Self {
            frames: [CallFrame::default(); FRAME_MAX],
            frame_count: 0,
            stack: [NIL; STACK_MAX],
            stack_top: 0,
            globals: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, source: &str) -> Result<(), LoxError> {
        let function = compile(source)?;

        self.push(Value::new_obj(Obj::Function(function)));
        self.frames[0] = CallFrame {
            function: self.stack[0].as_function(),
            ip: 0,
            slot_base: 0,
        };

        self.call(self.stack[0].as_function(), 0)?;
        self.run()
    }

    fn run(&mut self) -> Result<(), LoxError> {
        let mut frame: *mut CallFrame = &mut self.frames[self.frame_count - 1];

        macro_rules! value {
            ($slot: expr) => {
                self.stack[unsafe { (*frame).slot_base }..][$slot]
            };
        }

        macro_rules! read_byte {
            () => {
                unsafe {
                    (*frame).ip += 1;
                    let i = (*frame).ip - 1;
                    (*(*frame).function).chunk.code[i]
                }
            };
        }

        macro_rules! read_u16 {
            () => {
                unsafe {
                    (*frame).ip += 2;
                    let i = (*frame).ip;
                    ((*(*frame).function).chunk.code[i - 2] as u16) << 8
                        | (*(*frame).function).chunk.code[i - 1] as u16
                }
            };
        }

        macro_rules! read_constant {
            () => {{
                let byte = read_byte!() as usize;
                unsafe { (*(*frame).function).chunk.constants[byte] }
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
            unsafe {
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
                let ip = (*frame).ip;
                let op = (*(*frame).function).chunk.code[ip].into();
                eprintln!("{}", (*(*frame).function).chunk.debug_op(ip, &op));
            }

            match read_byte!().into() {
                OpConstant => {
                    let constant = read_constant!();
                    self.push(constant);
                }
                OpNil => self.push(Value::Nil),
                OpTrue => self.push(Value::Bool(true)),
                OpFalse => self.push(Value::Bool(false)),
                OpPop => {
                    self.pop();
                }
                OpGetLocal => {
                    let slot = read_byte!() as usize;
                    let v = value!(slot);
                    self.push(v);
                }
                OpSetLocal => {
                    let slot = read_byte!() as usize;
                    value!(slot) = *self.peek(0);
                }
                OpGetGlobal => {
                    let constant = read_constant!();
                    let name = constant.as_string();

                    let v = match self.globals.get(name) {
                        Some(v) => *v,
                        None => {
                            let msg = format!("Undefined variable '{}'.", name);
                            return self.runtime_error(msg);
                        }
                    };

                    self.push(v)
                }
                OpDefineGlobal => {
                    let name = read_constant!().as_string().to_string();
                    self.globals.insert(name, *self.peek(0));
                    self.pop();
                }
                OpSetGlobal => {
                    let name = read_constant!().as_string().to_string();
                    let _v = *self.peek(0);
                    match self.globals.get_mut(&name) {
                        Some(v) => *v = _v,
                        None => {
                            let msg = format!("Undefined variable '{}'.", name);
                            return self.runtime_error(msg);
                        }
                    }
                }
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
                        self.push(Value::new_obj(Obj::String(s)))
                    }
                    (Value::Number(b), Value::Number(a)) => {
                        let v = a + b;
                        self.stack_top -= 2;
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
                OpPrint => {
                    println!("{}", self.pop());
                }
                OpJump => {
                    let offset = read_u16!();
                    unsafe {
                        (*frame).ip += offset as usize;
                    }
                }
                OpJumpIfFalse => {
                    let offset = read_u16!();
                    if self.peek(0).is_falsey() {
                        unsafe {
                            (*frame).ip += offset as usize;
                        }
                    }
                }
                OpLoop => {
                    let offset = read_u16!();
                    unsafe {
                        (*frame).ip -= offset as usize;
                    }
                }
                OpCall => {
                    let arg_count = read_byte!() as usize;
                    self.call_value(*self.peek(arg_count), arg_count)?;
                    frame = &mut self.frames[self.frame_count - 1];
                }
                OpReturn => {
                    let result = *self.pop();
                    self.frame_count -= 1;
                    if self.frame_count == 0 {
                        self.reset_stack();
                        return Ok(());
                    }

                    self.stack_top = unsafe { (*frame).slot_base };
                    self.push(result);
                    frame = &mut self.frames[self.frame_count - 1];
                }
            }
        }
    }

    fn reset_stack(&mut self) {
        self.stack_top = 0;
        self.frame_count = 0;
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

    fn call_value(&mut self, callee: Value, arg_count: usize) -> Result<(), LoxError> {
        match callee {
            Value::Obj(o) => match unsafe { &*o } {
                Obj::Function(ref f) => self.call(f, arg_count),
                _ => self.runtime_error("Can only call functions and classes."),
            },
            _ => self.runtime_error("Can only call functions and classes."),
        }
    }

    fn call(&mut self, function: *const Function, arg_count: usize) -> Result<(), LoxError> {
        if arg_count != unsafe { (*function).arity } {
            let msg = format!(
                "Expected {} arguments but got {}.",
                unsafe { (*function).arity },
                arg_count
            );
            return self.runtime_error(msg);
        }

        if self.frame_count == FRAME_MAX {
            return self.runtime_error("Stack overflow.");
        }

        let frame = &mut self.frames[self.frame_count];
        self.frame_count += 1;

        frame.function = function;
        frame.ip = 0;
        frame.slot_base = self.stack_top - arg_count - 1;

        Ok(())
    }

    fn runtime_error<T: Into<String>>(&mut self, message: T) -> Result<(), LoxError> {
        let mut msg = String::new();
        for i in (0..=self.frame_count - 1).rev() {
            let frame = &self.frames[i];
            let function = unsafe { &*frame.function };
            let line = function.chunk.lines[frame.ip - 1];

            if function.name == "" {
                msg.push_str(&format!("[line {}] in script\n", line));
            } else {
                msg.push_str(&format!("[line {}] in {}()\n", line, function.name));
            }
        }

        self.reset_stack();

        Err(LoxError::RuntimeError(format!(
            "{}\n{}",
            message.into(),
            msg
        )))
    }
}
