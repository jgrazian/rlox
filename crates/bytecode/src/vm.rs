use std::collections::HashMap;
use std::fmt::Pointer;

use crate::compiler::{Compiler, FunctionType, U8_COUNT};
use crate::error::LoxError;
use crate::object::Obj;
use crate::value::Value;
use crate::{
    chunk::{
        Chunk,
        OpCode::{self, *},
    },
    object::Function,
};

const FRAME_MAX: usize = 64;
const STACK_MAX: usize = FRAME_MAX * U8_COUNT;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
struct CallFrame {
    function_slot: usize,
    ip: usize,
    slot_base: usize,
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
        let function = Compiler::new(source, FunctionType::Script).compile()?;

        self.push(Value::Obj(Box::new(Obj::Function(function))));
        self.frames[0] = CallFrame {
            function_slot: 0,
            ip: 0,
            slot_base: 0,
        };
        self.frame_count += 1;

        self.run()
    }

    fn frame(&self) -> &CallFrame {
        &self.frames[self.frame_count - 1]
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        &mut self.frames[self.frame_count - 1]
    }

    fn frame_function(&self) -> &Function {
        let slot = self.frame().function_slot;
        self.stack[slot].as_function()
    }

    fn frame_slots(&mut self) -> &mut [Value] {
        let slot_base = self.frame().slot_base;
        &mut self.stack[slot_base..]
    }

    fn run(&mut self) -> Result<(), LoxError> {
        macro_rules! read_byte {
            () => {{
                self.frame_mut().ip += 1;
                let i = self.frame().ip - 1;
                self.frame_function().chunk.code[i]
            }};
        }

        macro_rules! read_u16 {
            () => {{
                self.frame_mut().ip += 2;
                let i = self.frame_mut().ip;
                (self.frame_function().chunk.code[i - 2] as u16) << 8
                    | self.frame_function().chunk.code[i - 1] as u16
            }};
        }

        macro_rules! read_constant {
            ($method: ident) => {{
                let byte = read_byte!() as usize;
                self.frame_function().chunk.constants[byte].$method()
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

            match read_byte!().into() {
                OpConstant => {
                    let constant = read_constant!(clone);
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
                    let v = self.frame_slots()[slot].clone();
                    self.push(v);
                }
                OpSetLocal => {
                    let slot = read_byte!() as usize;
                    self.frame_slots()[slot] = self.peek(0).clone();
                }
                OpGetGlobal => {
                    let name = read_constant!(as_string);

                    match self.globals.get(name) {
                        Some(v) => {
                            let v = v.clone();
                            self.push(v)
                        }
                        None => {
                            let msg = format!("Undefined variable '{}'.", name);
                            return self.runtime_error(msg);
                        }
                    }
                }
                OpDefineGlobal => {
                    let name = read_constant!(as_string).to_string();
                    self.globals.insert(name, self.peek(0).clone());
                    self.pop();
                }
                OpSetGlobal => {
                    let name = read_constant!(as_string).to_string();
                    let _v = self.peek(0).clone();
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
                        self.push(Value::Obj(Box::new(Obj::String(s))))
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
                    self.frame_mut().ip += offset as usize;
                }
                OpJumpIfFalse => {
                    let offset = read_u16!();
                    if self.peek(0).is_falsey() {
                        self.frame_mut().ip += offset as usize;
                    }
                }
                OpLoop => {
                    let offset = read_u16!();
                    self.frame_mut().ip -= offset as usize;
                }
                OpReturn => break,
            }
        }
        self.reset_stack();
        Ok(())
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

    fn runtime_error<T: Into<String>>(&mut self, message: T) -> Result<(), LoxError> {
        let instr = self.frame().ip - 1;
        let line = self.frame_function().chunk.lines[instr];
        self.reset_stack();
        Err(LoxError::RuntimeError(format!(
            "{}\n[line {}] in script",
            message.into(),
            line
        )))
    }
}
