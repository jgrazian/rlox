use crate::chunk::{Chunk, OpCode};
use crate::error::LoxError;
use crate::object::Obj;
use crate::scanner::{Scanner, Token, TokenType};
use crate::value::Value;

struct Parser<'s> {
    scanner: Scanner<'s>,
    current: Token<'s>,
    previous: Token<'s>,
    had_error: bool,
    panic_mode: bool,
}

impl<'s> Parser<'s> {
    fn new(source: &'s str) -> Self {
        Self {
            scanner: Scanner::new(source),
            current: Token {
                ty: TokenType::Eof,
                lexeme: "",
                line: 0,
            },
            previous: Token {
                ty: TokenType::Eof,
                lexeme: "",
                line: 0,
            },
            had_error: false,
            panic_mode: false,
        }
    }

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

    fn consume(&mut self, ty: TokenType, message: &str) -> Result<(), LoxError> {
        if self.current.ty == ty {
            return self.advance();
        }

        match self.error_at_current(message) {
            Some(e) => Err(e),
            None => Ok(()),
        }
    }

    fn check_token(&self, ty: TokenType) -> bool {
        self.current.ty == ty
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
        Some(LoxError::ParseError {
            message: message.to_string(),
            location,
            line: token.line,
        })
    }
}

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

#[derive(Debug, Clone, Copy)]
struct Local<'s> {
    name: Token<'s>,
    depth: isize,
}

pub struct Compiler<'s> {
    parser: Parser<'s>,
    compiling_chunk: &'s mut Chunk,

    locals: [Local<'s>; 256],
    local_count: usize,
    scope_depth: usize,
}

impl<'s> Compiler<'s> {
    pub fn new(source: &'s str, compiling_chunk: &'s mut Chunk) -> Self {
        const LOCAL: Local = Local {
            name: Token {
                ty: TokenType::Eof,
                lexeme: "",
                line: 0,
            },
            depth: 0,
        };

        Self {
            parser: Parser::new(source),
            compiling_chunk,
            locals: [LOCAL; 256],
            local_count: 0,
            scope_depth: 0,
        }
    }

    pub fn compile(&mut self) -> Result<(), LoxError> {
        self.parser.advance()?;
        while !self.parser.match_token(TokenType::Eof)? {
            self.declaration()?;
        }
        self.end_compiler();
        Ok(())
    }

    fn expression(&mut self) -> Result<(), LoxError> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn var_declaration(&mut self) -> Result<(), LoxError> {
        let global = self.parse_variable("Expected variable name.")?;

        if self.parser.match_token(TokenType::Equal)? {
            self.expression()?;
        } else {
            self.emit_byte(OpCode::OpNil);
        }
        self.parser.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        )?;
        self.define_variable(global);
        Ok(())
    }

    fn expression_statement(&mut self) -> Result<(), LoxError> {
        self.expression()?;
        self.parser
            .consume(TokenType::Semicolon, "Expect ';' after expression.")?;
        self.emit_byte(OpCode::OpPop);
        Ok(())
    }

    fn if_statement(&mut self) -> Result<(), LoxError> {
        self.parser
            .consume(TokenType::LeftParen, "Expect '(' after 'if'.")?;
        self.expression()?;
        self.parser
            .consume(TokenType::RightParen, "Expect ')' after condition.")?;

        let then_jump = self.emit_jump(OpCode::OpJumpIfFalse);
        self.emit_byte(OpCode::OpPop);
        self.statement()?;

        let else_jump = self.emit_jump(OpCode::OpJump);

        self.patch_jump(then_jump);
        self.emit_byte(OpCode::OpPop);

        if self.parser.match_token(TokenType::Else)? {
            self.statement()?;
        }
        self.patch_jump(else_jump);

        Ok(())
    }

    fn print_statement(&mut self) -> Result<(), LoxError> {
        self.expression()?;
        self.parser
            .consume(TokenType::Semicolon, "Expect ';' after value.")?;
        self.emit_byte(OpCode::OpPrint);
        Ok(())
    }

    fn synchronize(&mut self) -> Result<(), LoxError> {
        self.parser.panic_mode = false;

        while self.parser.current.ty != TokenType::Eof {
            if self.parser.previous.ty == TokenType::Semicolon {
                return Ok(());
            }
            match self.parser.current.ty {
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
            self.parser.advance()?;
        }
        Ok(())
    }

    fn declaration(&mut self) -> Result<(), LoxError> {
        if self.parser.match_token(TokenType::Var)? {
            self.var_declaration()?;
        } else {
            self.statement()?;
        }

        if self.parser.panic_mode {
            self.synchronize()?;
        }
        Ok(())
    }

    fn statement(&mut self) -> Result<(), LoxError> {
        if self.parser.match_token(TokenType::Print)? {
            self.print_statement()
        } else if self.parser.match_token(TokenType::If)? {
            self.if_statement()
        } else if self.parser.match_token(TokenType::LeftBrance)? {
            self.begin_scope();
            self.block()?;
            self.end_scope();
            Ok(())
        } else {
            self.expression_statement()
        }
    }

    fn block(&mut self) -> Result<(), LoxError> {
        while !self.parser.check_token(TokenType::RightBrace)
            && !self.parser.check_token(TokenType::Eof)
        {
            self.declaration()?;
        }
        self.parser
            .consume(TokenType::RightBrace, "Expect '}' after block.")
    }

    fn number(&mut self, _: bool) -> Result<(), LoxError> {
        let value = match self.parser.previous.lexeme.parse::<f64>() {
            Ok(value) => value,
            Err(_) => {
                return Err(self.error("Parse failure.").into());
            }
        };
        self.emit_constant(Value::Number(value))
    }

    fn string(&mut self, _: bool) -> Result<(), LoxError> {
        let mut chars = self.parser.previous.lexeme.chars();
        chars.next();
        chars.next_back();
        self.emit_constant(Value::Obj(Box::new(Obj::String(chars.collect()))))
    }

    fn variable(&mut self, can_assign: bool) -> Result<(), LoxError> {
        self.named_variable(self.parser.previous, can_assign)?;
        Ok(())
    }

    fn named_variable(&mut self, name: Token, can_assign: bool) -> Result<(), LoxError> {
        let mut arg = self.resolve_local(name)?;
        let (get_op, set_op) = match arg {
            -1 => {
                arg = self.identifier_constant(name) as i8;
                (OpCode::OpGetGlobal, OpCode::OpSetGlobal)
            }
            _ => (OpCode::OpGetLocal, OpCode::OpSetLocal),
        };

        if can_assign && self.parser.match_token(TokenType::Equal)? {
            self.expression()?;
            self.emit_bytes(set_op, arg as u8);
        } else {
            self.emit_bytes(get_op, arg as u8);
        }
        Ok(())
    }

    fn grouping(&mut self, _: bool) -> Result<(), LoxError> {
        self.expression()?;
        self.parser
            .consume(TokenType::RightParen, "Expect ')' after expression.")
    }

    fn unary(&mut self, _: bool) -> Result<(), LoxError> {
        let operator_type = self.parser.previous.ty;
        self.parse_precedence(Precedence::Unary)?;
        match operator_type {
            TokenType::Bang => self.emit_byte(OpCode::OpNot),
            TokenType::Minus => self.emit_byte(OpCode::OpNegate),
            _ => unreachable!("Expected unary operator."),
        }
        Ok(())
    }

    fn binary(&mut self, _: bool) -> Result<(), LoxError> {
        let operator_type = self.parser.previous.ty;
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
        match self.parser.previous.ty {
            TokenType::False => self.emit_byte(OpCode::OpFalse),
            TokenType::Nil => self.emit_byte(OpCode::OpNil),
            TokenType::True => self.emit_byte(OpCode::OpTrue),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), LoxError> {
        self.parser.advance()?;
        let can_assign;

        match Self::get_rule(self.parser.previous.ty).prefix {
            Some(prefix_rule) => {
                can_assign = precedence <= Precedence::Assignment;
                prefix_rule(self, can_assign)?;
            }
            None => {
                return Err(self.error("Expect expression.").into());
            }
        };

        while precedence <= Self::get_rule(self.parser.current.ty).precedence {
            self.parser.advance()?;
            match Self::get_rule(self.parser.previous.ty).infix {
                Some(infix_rule) => infix_rule(self, can_assign)?,
                None => {}
            }
        }

        if can_assign && self.parser.match_token(TokenType::Equal)? {
            match self.error("Invalid assignment target.") {
                Some(e) => return Err(e),
                None => {}
            }
        }

        Ok(())
    }

    fn parse_variable(&mut self, error_message: &str) -> Result<u8, LoxError> {
        self.parser.consume(TokenType::Identifier, error_message)?;

        self.declare_variable()?;
        if self.scope_depth > 0 {
            return Ok(0);
        }

        Ok(self.identifier_constant(self.parser.previous))
    }

    fn identifier_constant(&mut self, name: Token) -> u8 {
        self.make_constant(Value::Obj(Box::new(Obj::String(name.lexeme.to_string()))))
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.scope_depth -= 1;

        while self.local_count > 0
            && self.locals[self.local_count - 1].depth > self.scope_depth as isize
        {
            self.emit_byte(OpCode::OpPop);
            self.local_count -= 1;
        }
    }

    fn end_compiler(&mut self) {
        self.emit_return();
        #[cfg(feature = "debug_print_code")]
        {
            if !self.parser.had_error {
                self.compiling_chunk.disassemble("code");
            }
        }
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpCode::OpReturn);
    }

    fn emit_byte<T: Into<u8>>(&mut self, byte: T) {
        self.compiling_chunk
            .push_byte(byte, self.parser.previous.line);
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
        self.compiling_chunk.push_constant(value)
    }

    fn emit_jump(&mut self, instruction: OpCode) -> usize {
        self.emit_byte(instruction);
        self.emit_byte(0xff);
        self.emit_byte(0xff);
        self.compiling_chunk.code.len() - 2
    }

    fn patch_jump(&mut self, offset: usize) {
        let jump = self.compiling_chunk.code.len() - offset - 2;

        if jump > u16::max_value() as usize {
            self.error("Too much code to jump over.");
        }

        self.compiling_chunk.code[offset] = ((jump >> 8) & 0xff) as u8;
        self.compiling_chunk.code[offset + 1] = (jump & 0xff) as u8;
    }

    fn add_local(&mut self, name: Token<'s>) -> Result<(), LoxError> {
        if self.local_count == 256 {
            match self.error("Too many local variables in function.") {
                Some(e) => return Err(e),
                None => {}
            }
        }

        self.locals[self.local_count as usize] = Local { name, depth: -1 };
        self.local_count += 1;
        Ok(())
    }

    fn resolve_local(&mut self, name: Token) -> Result<i8, LoxError> {
        for i in (0..self.local_count).rev() {
            let local = self.locals[i];
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

        let name = self.parser.previous;
        for i in (0..self.local_count).rev() {
            let local = self.locals[i];
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

    fn mark_initialized(&mut self) {
        self.locals[self.local_count - 1].depth = self.scope_depth as isize;
    }

    // Errors
    fn error_at_current(&mut self, message: &str) -> Option<LoxError> {
        self.error_at(self.parser.current, message)
    }

    fn error(&mut self, message: &str) -> Option<LoxError> {
        self.error_at(self.parser.previous, message)
    }

    fn error_at(&mut self, token: Token<'s>, message: &str) -> Option<LoxError> {
        if self.parser.panic_mode {
            return None;
        }
        self.parser.panic_mode = true;

        let location = match token.ty {
            TokenType::Eof => " at end".to_string(),
            TokenType::Error => "".to_string(),
            _ => format!(" at '{}'", token.lexeme),
        };

        self.parser.had_error = true;
        Some(LoxError::CompileError {
            message: message.to_string(),
            location,
            line: token.line,
        })
    }

    fn get_rule(ty: TokenType) -> ParseRule<'s> {
        match ty {
            TokenType::LeftParen => ParseRule {
                prefix: Some(Self::grouping),
                infix: None,
                precedence: Precedence::None,
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
            _ => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
        }
    }
}
