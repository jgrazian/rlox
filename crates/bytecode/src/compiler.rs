use crate::chunk::OpCode;
use crate::error::LoxError;
use crate::heap::Heap;
use crate::object::{FunctionType, Obj, ObjClosure, ObjFunction};
use crate::scanner::{Scanner, Token, TokenType};
use crate::value::Value;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
enum Precedence {
    None = 0,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl Precedence {
    fn bump(self) -> Precedence {
        match self {
            Precedence::None => Precedence::Assignment,
            Precedence::Assignment => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => Precedence::Primary,
        }
    }
}

struct ParseRule<'s> {
    prefix: Option<fn(&mut Compiler<'s>, bool) -> Result<(), LoxError>>,
    infix: Option<fn(&mut Compiler<'s>, bool) -> Result<(), LoxError>>,
    precedence: Precedence,
}

#[derive(Debug, Default, Clone, Copy)]
struct Local<'s> {
    name: Token<'s>,
    depth: isize,
}

#[derive(Debug, Default, Clone, Copy)]
struct Upvalue {
    index: u8,
    is_local: bool,
}

pub const U8_COUNT: usize = 256;

pub struct Compiler<'s> {
    had_error: bool,
    panic_mode: bool,

    scanner: Scanner<'s>,
    previous: Token<'s>,
    current: Token<'s>,

    heap: &'s mut Heap<Obj>,

    functions: Vec<ObjFunction>,
    upvalues: Vec<Vec<Upvalue>>,
    locals: Vec<Vec<Local<'s>>>,
    scope_depth: usize,
}

impl<'s> Compiler<'s> {
    pub fn new(source: &'s str, heap: &'s mut Heap<Obj>) -> Self {
        let mut compiler = Self {
            had_error: false,
            panic_mode: false,
            scanner: Scanner::new(source),
            previous: Token::default(),
            current: Token::default(),
            heap,
            functions: Vec::with_capacity(16),
            upvalues: Vec::with_capacity(8),
            locals: Vec::with_capacity(8),
            scope_depth: 0,
        };
        compiler.locals.push(vec![Local {
            name: Token::default(),
            depth: 0,
        }]);
        compiler.upvalues.push(vec![]);
        compiler
    }

    pub fn compile(&mut self) -> Result<ObjFunction, LoxError> {
        self.functions.push(ObjFunction::anon());
        self.advance()?;

        while !self.match_token(TokenType::Eof)? {
            self.declaration()?;
        }

        Ok(self.end_compiler())
    }

    fn compiling_function(&mut self) -> &mut ObjFunction {
        self.functions.last_mut().unwrap()
    }

    fn declaration(&mut self) -> Result<(), LoxError> {
        if self.match_token(TokenType::Fun)? {
            self.fun_declaration()?;
        } else if self.match_token(TokenType::Var)? {
            self.var_declaration()?;
        } else {
            self.statement()?;
        }

        if self.panic_mode {
            self.synchronize()?;
        }
        Ok(())
    }

    fn var_declaration(&mut self) -> Result<(), LoxError> {
        let global = self.parse_variable("Expected variable name.")?;

        if self.match_token(TokenType::Equal)? {
            self.expression()?;
        } else {
            self.emit_byte(OpCode::OpNil);
        }
        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        )?;
        self.define_variable(global);
        Ok(())
    }

    fn expression(&mut self) -> Result<(), LoxError> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn statement(&mut self) -> Result<(), LoxError> {
        if self.match_token(TokenType::Print)? {
            self.print_statement()
        } else if self.match_token(TokenType::For)? {
            self.for_statement()
        } else if self.match_token(TokenType::If)? {
            self.if_statement()
        } else if self.match_token(TokenType::Return)? {
            self.return_statement()
        } else if self.match_token(TokenType::While)? {
            self.while_statement()
        } else if self.match_token(TokenType::LeftBrance)? {
            self.begin_scope();
            self.block()?;
            self.end_scope();
            Ok(())
        } else {
            self.expression_statement()
        }
    }

    fn if_statement(&mut self) -> Result<(), LoxError> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.")?;
        self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after condition.")?;

        let then_jump = self.emit_jump(OpCode::OpJumpIfFalse);
        self.emit_byte(OpCode::OpPop);
        self.statement()?;

        let else_jump = self.emit_jump(OpCode::OpJump);

        self.patch_jump(then_jump);
        self.emit_byte(OpCode::OpPop);

        if self.match_token(TokenType::Else)? {
            self.statement()?;
        }
        self.patch_jump(else_jump);

        Ok(())
    }

    fn print_statement(&mut self) -> Result<(), LoxError> {
        self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after value.")?;
        self.emit_byte(OpCode::OpPrint);
        Ok(())
    }

    fn while_statement(&mut self) -> Result<(), LoxError> {
        let loop_start = self.compiling_function().chunk.code.len();
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.")?;
        self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after condition.")?;

        let exit_jump = self.emit_jump(OpCode::OpJumpIfFalse);
        self.emit_byte(OpCode::OpPop);
        self.statement()?;
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit_byte(OpCode::OpPop);
        Ok(())
    }

    fn for_statement(&mut self) -> Result<(), LoxError> {
        self.begin_scope();

        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.")?;
        if self.match_token(TokenType::Semicolon)? {
            // No initializer.
        } else if self.match_token(TokenType::Var)? {
            self.var_declaration()?;
        } else {
            self.expression_statement()?;
        }

        let mut loop_start = self.compiling_function().chunk.code.len();
        let mut exit_jump = None;
        if !self.match_token(TokenType::Semicolon)? {
            self.expression()?;
            self.consume(TokenType::Semicolon, "Expect ';' after loop condition.")?;

            exit_jump = Some(self.emit_jump(OpCode::OpJumpIfFalse));
            self.emit_byte(OpCode::OpPop);
        }

        if !self.match_token(TokenType::RightParen)? {
            let body_jump = self.emit_jump(OpCode::OpJump);
            let increment_start = self.compiling_function().chunk.code.len();
            self.expression()?;
            self.emit_byte(OpCode::OpPop);
            self.consume(TokenType::RightParen, "Expect ')' after for clauses.")?;

            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        self.statement()?;
        self.emit_loop(loop_start);

        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump);
            self.emit_byte(OpCode::OpPop);
        }

        self.end_scope();
        Ok(())
    }

    fn expression_statement(&mut self) -> Result<(), LoxError> {
        self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after expression.")?;
        self.emit_byte(OpCode::OpPop);
        Ok(())
    }

    fn return_statement(&mut self) -> Result<(), LoxError> {
        if self.compiling_function().ty == FunctionType::Script {
            match self.error("Cannot return from top-level code.") {
                Some(e) => return Err(e),
                None => {}
            }
        }

        if self.match_token(TokenType::Semicolon)? {
            self.emit_return();
        } else {
            self.expression()?;
            self.consume(TokenType::Semicolon, "Expect ';' after return value.")?;
            self.emit_byte(OpCode::OpReturn);
        }
        Ok(())
    }

    fn block(&mut self) -> Result<(), LoxError> {
        while !self.check_token(TokenType::RightBrace) && !self.check_token(TokenType::Eof) {
            self.declaration()?;
        }
        self.consume(TokenType::RightBrace, "Expect '}' after block.")
    }

    fn number(&mut self, _: bool) -> Result<(), LoxError> {
        let value = match self.previous.lexeme.parse::<f64>() {
            Ok(value) => value,
            Err(_) => {
                return Err(self.error("Parse failure.").into());
            }
        };
        self.emit_constant(Value::Number(value))
    }

    fn string(&mut self, _: bool) -> Result<(), LoxError> {
        let mut chars = self.previous.lexeme.chars();
        chars.next();
        chars.next_back();
        let value = Value::Obj(Obj::alloc_string(&mut self.heap, chars.collect::<String>()));
        self.emit_constant(value)
    }

    fn variable(&mut self, can_assign: bool) -> Result<(), LoxError> {
        let previous = self.previous;
        self.named_variable(previous, can_assign)?;
        Ok(())
    }

    fn named_variable(&mut self, name: Token, can_assign: bool) -> Result<(), LoxError> {
        let mut arg = self.resolve_local(name, self.locals.len() - 1)?;
        let (get_op, set_op) = match arg {
            v if v != -1 => (OpCode::OpGetLocal, OpCode::OpSetLocal),
            _ => match self.resolve_upvalue(name, self.locals.len() - 1)? {
                -1 => {
                    arg = self.identifier_constant(name) as i8;
                    (OpCode::OpGetGlobal, OpCode::OpSetGlobal)
                }
                v @ _ => {
                    arg = v;
                    (OpCode::OpGetUpvalue, OpCode::OpSetUpvalue)
                }
            },
        };

        if can_assign && self.match_token(TokenType::Equal)? {
            self.expression()?;
            self.emit_bytes(set_op, arg as u8);
        } else {
            self.emit_bytes(get_op, arg as u8);
        }
        Ok(())
    }

    fn grouping(&mut self, _: bool) -> Result<(), LoxError> {
        self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after expression.")
    }

    fn unary(&mut self, _: bool) -> Result<(), LoxError> {
        let operator_type = self.previous.ty;
        self.parse_precedence(Precedence::Unary)?;
        match operator_type {
            TokenType::Bang => self.emit_byte(OpCode::OpNot),
            TokenType::Minus => self.emit_byte(OpCode::OpNegate),
            _ => unreachable!("Expected unary operator."),
        }
        Ok(())
    }

    fn binary(&mut self, _: bool) -> Result<(), LoxError> {
        let operator_type = self.previous.ty;
        let rule = Self::get_rule(operator_type);
        self.parse_precedence(rule.precedence.bump())?;

        match operator_type {
            TokenType::BangEqual => self.emit_bytes(OpCode::OpEqual, OpCode::OpNot),
            TokenType::EqualEqual => self.emit_byte(OpCode::OpEqual),
            TokenType::Greater => self.emit_byte(OpCode::OpGreater),
            TokenType::GreaterEqual => self.emit_bytes(OpCode::OpLess, OpCode::OpNot),
            TokenType::Less => self.emit_byte(OpCode::OpLess),
            TokenType::LessEqual => self.emit_bytes(OpCode::OpGreater, OpCode::OpNot),
            TokenType::Plus => self.emit_byte(OpCode::OpAdd),
            TokenType::Minus => self.emit_byte(OpCode::OpSubtract),
            TokenType::Star => self.emit_byte(OpCode::OpMultiply),
            TokenType::Slash => self.emit_byte(OpCode::OpDivide),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn literal(&mut self, _: bool) -> Result<(), LoxError> {
        match self.previous.ty {
            TokenType::False => self.emit_byte(OpCode::OpFalse),
            TokenType::Nil => self.emit_byte(OpCode::OpNil),
            TokenType::True => self.emit_byte(OpCode::OpTrue),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn fun_declaration(&mut self) -> Result<(), LoxError> {
        let global = self.parse_variable("Expect function name.")?;
        self.mark_initialized();
        self.function()?;
        self.define_variable(global);
        Ok(())
    }

    fn function(&mut self) -> Result<(), LoxError> {
        self.functions
            .push(ObjFunction::named(self.previous.lexeme));
        self.locals.push(vec![Local {
            name: self.previous,
            depth: self.scope_depth as isize + 1,
        }]);
        self.upvalues.push(vec![]);
        self.begin_scope();

        self.consume(TokenType::LeftParen, "Expect '(' after function name.")?;

        if !self.check_token(TokenType::RightParen) {
            loop {
                self.compiling_function().arity += 1;
                if self.compiling_function().arity > 255 {
                    return Err(self.error("Cannot have more than 255 parameters."))?;
                }

                let constant = self.parse_variable("Expect parameter name.")?;
                self.define_variable(constant);

                if !self.match_token(TokenType::Comma)? {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen, "Expect ')' after parameters.")?;
        self.consume(TokenType::LeftBrance, "Expect '{' before function body.")?;
        self.block()?;
        self.end_scope();

        let closure = self.end_compiler();

        let value = Value::Obj(Obj::alloc_function(&mut self.heap, closure));
        let constant = self.make_constant(value);
        self.emit_bytes(OpCode::OpClosure, constant);

        for i in 0..self.upvalues.last().unwrap().len() {
            let upvalue = self.upvalues.last().unwrap()[i];
            self.emit_byte(if upvalue.is_local { 1 } else { 0 });
            self.emit_byte(upvalue.index);
        }
        self.locals.pop();
        self.upvalues.pop();

        Ok(())
    }

    fn call(&mut self, _: bool) -> Result<(), LoxError> {
        let arg_count = self.argument_list()?;
        self.emit_bytes(OpCode::OpCall, arg_count as u8);
        Ok(())
    }

    fn argument_list(&mut self) -> Result<u8, LoxError> {
        let mut arg_count = 0;
        if !self.check_token(TokenType::RightParen) {
            loop {
                self.expression()?;
                if arg_count == 255 {
                    match self.error("Can't have more than 255 arguments.") {
                        Some(e) => return Err(e),
                        None => {}
                    }
                }
                arg_count += 1;

                if !self.match_token(TokenType::Comma)? {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after arguments.")?;
        Ok(arg_count)
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), LoxError> {
        self.advance()?;
        let can_assign;

        match Self::get_rule(self.previous.ty).prefix {
            Some(prefix_rule) => {
                can_assign = precedence <= Precedence::Assignment;
                prefix_rule(self, can_assign)?;
            }
            None => {
                return Err(self.error("Expect expression.").into());
            }
        };

        while precedence <= Self::get_rule(self.current.ty).precedence {
            self.advance()?;
            match Self::get_rule(self.previous.ty).infix {
                Some(infix_rule) => infix_rule(self, can_assign)?,
                None => {}
            }
        }

        if can_assign && self.match_token(TokenType::Equal)? {
            match self.error("Invalid assignment target.") {
                Some(e) => return Err(e),
                None => {}
            }
        }

        Ok(())
    }

    fn parse_variable(&mut self, error_message: &str) -> Result<u8, LoxError> {
        self.consume(TokenType::Identifier, error_message)?;

        self.declare_variable()?;
        if self.scope_depth > 0 {
            return Ok(0);
        }

        let previous = self.previous;
        Ok(self.identifier_constant(previous))
    }

    fn identifier_constant(&mut self, name: Token) -> u8 {
        let value = Value::Obj(Obj::alloc_string(&mut self.heap, name.lexeme.clone()));
        self.make_constant(value)
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.scope_depth -= 1;

        // for _ in 0..self.locals.last().unwrap().len() {
        //     self.emit_byte(OpCode::OpPop);
        // }
    }

    fn end_compiler(&mut self) -> ObjFunction {
        self.emit_return();
        #[cfg(feature = "debug_print_code")]
        {
            if !self.had_error {
                let name = match &self.compiling_function().name {
                    Some(n) => n.to_string(),
                    None => "script".to_string(),
                };
                self.functions
                    .last()
                    .unwrap()
                    .chunk
                    .disassemble(&name, &self.heap);
            }
        }
        self.functions.pop().unwrap()
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpCode::OpNil);
        self.emit_byte(OpCode::OpReturn);
    }

    fn emit_byte<T: Into<u8>>(&mut self, byte: T) {
        let previous_line = self.previous.line;
        self.compiling_function()
            .chunk
            .push_byte(byte, previous_line);
    }

    fn emit_bytes<U: Into<u8>, V: Into<u8>>(&mut self, byte1: U, byte2: V) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn emit_constant(&mut self, value: Value) -> Result<(), LoxError> {
        let constant = self.make_constant(value);
        self.emit_bytes(OpCode::OpConstant, constant);
        Ok(())
    }

    fn make_constant(&mut self, value: Value) -> u8 {
        self.compiling_function().chunk.push_constant(value)
    }

    fn emit_jump(&mut self, instruction: OpCode) -> usize {
        self.emit_byte(instruction);
        self.emit_byte(0xff);
        self.emit_byte(0xff);
        self.compiling_function().chunk.code.len() - 2
    }

    fn patch_jump(&mut self, offset: usize) {
        let jump = self.compiling_function().chunk.code.len() - offset - 2;

        if jump > u16::max_value() as usize {
            self.error("Too much code to jump over.");
        }

        self.compiling_function().chunk.code[offset] = ((jump >> 8) & 0xff) as u8;
        self.compiling_function().chunk.code[offset + 1] = (jump & 0xff) as u8;
    }

    fn emit_loop(&mut self, loop_start: usize) {
        self.emit_byte(OpCode::OpLoop);

        let offset = self.compiling_function().chunk.code.len() - loop_start + 2;
        if offset > u16::max_value() as usize {
            self.error("Loop body too large.");
        }

        self.emit_byte((offset >> 8) as u8 & 0xff);
        self.emit_byte(offset as u8 & 0xff);
    }

    fn add_local(&mut self, name: Token<'s>) -> Result<(), LoxError> {
        if self.locals.last().unwrap().len() >= U8_COUNT {
            match self.error("Too many local variables in function.") {
                Some(e) => return Err(e),
                None => {}
            }
        }

        self.locals
            .last_mut()
            .unwrap()
            .push(Local { name, depth: -1 });
        Ok(())
    }

    fn resolve_local(&mut self, name: Token, depth: usize) -> Result<i8, LoxError> {
        for i in (0..self.locals[depth].len()).rev() {
            let local = self.locals[depth][i];

            if name.lexeme == local.name.lexeme {
                if local.depth == -1 {
                    match self.error("Can't read local variable in its own initializer.") {
                        Some(e) => return Err(e),
                        None => {}
                    }
                }
                return Ok(i as i8);
            }
        }
        return Ok(-1);
    }

    fn declare_variable(&mut self) -> Result<(), LoxError> {
        if self.scope_depth == 0 {
            return Ok(());
        }

        let name = self.previous;
        for i in (0..self.locals.last().unwrap().len()).rev() {
            let local = self.locals.last().unwrap()[i];
            if local.depth != -1 && local.depth < self.scope_depth as isize {
                break;
            }

            if name.lexeme == local.name.lexeme {
                match self.error("Already a variable with this name in this scope.") {
                    Some(e) => return Err(e),
                    None => {}
                }
            }
        }
        self.add_local(name)
    }

    fn define_variable(&mut self, global: u8) {
        if self.scope_depth > 0 {
            self.mark_initialized();
            return;
        }

        self.emit_bytes(OpCode::OpDefineGlobal, global);
    }

    fn resolve_upvalue(&mut self, name: Token, depth: usize) -> Result<i8, LoxError> {
        if depth <= 1 {
            return Ok(-1);
        }

        // TODO: This local lookup is probably wrong
        let local = self.resolve_local(name, depth - 1)?;
        if local != -1 {
            // self.locals[local as usize].is_captured = true;
            return self.add_upvalue(local as u8, true, depth).map(|v| v as i8);
        }

        let upvalue = self.resolve_upvalue(name, depth - 1)?;
        if upvalue != -1 {
            return self
                .add_upvalue(upvalue as u8, false, depth)
                .map(|v| v as i8);
        }

        return Ok(-1);
    }

    fn add_upvalue(&mut self, index: u8, is_local: bool, depth: usize) -> Result<u8, LoxError> {
        let upvalue_count = self.functions[depth].upvalue_count;

        for i in 0..upvalue_count as usize {
            let upvalue = self.upvalues[depth][i];
            if upvalue.index == index && upvalue.is_local == is_local {
                return Ok(i as u8);
            }
        }

        if upvalue_count == U8_COUNT {
            self.error("Too many closure variables in function.");
        }

        self.upvalues[depth].push(Upvalue { index, is_local });
        self.functions[depth].upvalue_count += 1;
        Ok(self.functions[depth].upvalue_count as u8 - 1)
    }

    fn and_(&mut self, _: bool) -> Result<(), LoxError> {
        let end_jump = self.emit_jump(OpCode::OpJumpIfFalse);

        self.emit_byte(OpCode::OpPop);
        self.parse_precedence(Precedence::And)?;

        self.patch_jump(end_jump);
        Ok(())
    }

    fn or_(&mut self, _: bool) -> Result<(), LoxError> {
        let else_jump = self.emit_jump(OpCode::OpJumpIfFalse);
        let end_jump = self.emit_jump(OpCode::OpJump);

        self.patch_jump(else_jump);
        self.emit_byte(OpCode::OpPop);

        self.parse_precedence(Precedence::Or)?;
        self.patch_jump(end_jump);
        Ok(())
    }

    fn mark_initialized(&mut self) {
        if self.scope_depth == 0 {
            return;
        }
        self.locals.last_mut().unwrap().last_mut().unwrap().depth = self.scope_depth as isize;
    }

    // Errors
    fn synchronize(&mut self) -> Result<(), LoxError> {
        self.panic_mode = false;

        while self.current.ty != TokenType::Eof {
            if self.previous.ty == TokenType::Semicolon {
                return Ok(());
            }
            match self.current.ty {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return Ok(()),
                _ => (),
            }
            self.advance()?;
        }
        Ok(())
    }

    fn get_rule(ty: TokenType) -> ParseRule<'s> {
        match ty {
            TokenType::LeftParen => ParseRule {
                prefix: Some(Self::grouping),
                infix: Some(Self::call),
                precedence: Precedence::Call,
            },
            TokenType::Minus => ParseRule {
                prefix: Some(Self::unary),
                infix: Some(Self::binary),
                precedence: Precedence::Term,
            },
            TokenType::Plus => ParseRule {
                prefix: None,
                infix: Some(Self::binary),
                precedence: Precedence::Term,
            },
            TokenType::Slash | TokenType::Star => ParseRule {
                prefix: None,
                infix: Some(Self::binary),
                precedence: Precedence::Factor,
            },
            TokenType::Number => ParseRule {
                prefix: Some(Self::number),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::False | TokenType::True | TokenType::Nil => ParseRule {
                prefix: Some(Self::literal),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Bang => ParseRule {
                prefix: Some(Self::unary),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::BangEqual | TokenType::EqualEqual => ParseRule {
                prefix: None,
                infix: Some(Self::binary),
                precedence: Precedence::Equality,
            },
            TokenType::Greater
            | TokenType::GreaterEqual
            | TokenType::Less
            | TokenType::LessEqual => ParseRule {
                prefix: None,
                infix: Some(Self::binary),
                precedence: Precedence::Comparison,
            },
            TokenType::String => ParseRule {
                prefix: Some(Self::string),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Identifier => ParseRule {
                prefix: Some(Self::variable),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::And => ParseRule {
                prefix: None,
                infix: Some(Self::and_),
                precedence: Precedence::And,
            },
            TokenType::Or => ParseRule {
                prefix: None,
                infix: Some(Self::or_),
                precedence: Precedence::Or,
            },
            _ => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
        }
    }
}

// Parser Parts
impl<'s> Compiler<'s> {
    fn advance(&mut self) -> Result<(), LoxError> {
        self.previous = self.current;

        let mut result = Ok(());
        loop {
            self.current = self.scanner.scan_token();
            match self.current.ty {
                TokenType::Error => match self.error_at_current(self.current.lexeme) {
                    Some(e) => result = Err(e),
                    None => {}
                },
                _ => break,
            }
        }

        result
    }

    fn check_token(&self, ty: TokenType) -> bool {
        self.current.ty == ty
    }

    fn consume(&mut self, ty: TokenType, message: &str) -> Result<(), LoxError> {
        if self.check_token(ty) {
            return self.advance();
        }

        match self.error_at_current(message) {
            Some(e) => Err(e),
            None => Ok(()),
        }
    }

    fn match_token(&mut self, ty: TokenType) -> Result<bool, LoxError> {
        match self.check_token(ty) {
            false => Ok(false),
            true => {
                self.advance()?;
                Ok(true)
            }
        }
    }

    // Errors
    fn error_at_current(&mut self, message: &str) -> Option<LoxError> {
        self.error_at(self.current, message)
    }

    fn error(&mut self, message: &str) -> Option<LoxError> {
        self.error_at(self.previous, message)
    }

    fn error_at(&mut self, token: Token<'s>, message: &str) -> Option<LoxError> {
        if self.panic_mode {
            return None;
        }
        self.panic_mode = true;

        let location = match token.ty {
            TokenType::Eof => " at end".to_string(),
            TokenType::Error => "".to_string(),
            _ => format!(" at '{}'", token.lexeme),
        };

        self.had_error = true;
        Some(LoxError::CompileError {
            message: message.to_string(),
            location,
            line: token.line,
        })
    }
}
