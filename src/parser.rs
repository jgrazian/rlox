use std::error::Error;
use std::fmt;

use crate::ast::*;
use crate::token::Token;
use crate::token_type::TokenType;

#[derive(Debug)]
pub struct ParseError {
    token: Token,
    message: String,
}
impl Error for ParseError {}

impl ParseError {
    fn new(token: Token, message: String) -> Self {
        Self { token, message }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let location = match self.token.token_type {
            TokenType::Eof => "end".to_string(),
            _ => format!("'{}'", self.token.lexeme),
        };
        write!(
            f,
            "[line {}] Error at {} {}",
            self.token.line, location, self.message
        )
    }
}

#[derive(Debug)]
pub struct ParserError {
    errors: Vec<ParseError>,
}
impl Error for ParserError {}

impl ParserError {
    fn new() -> Self {
        Self { errors: Vec::new() }
    }

    fn push(&mut self, err: ParseError) {
        self.errors.push(err)
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.errors
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Vec<Box<Stmt>>, ParserError> {
        let mut errors = ParserError::new();
        let mut statements = Vec::new();

        while !self.is_at_end() {
            match self.declaration() {
                Ok(expr) => statements.push(expr),
                Err(e) => errors.push(e),
            }
        }

        if errors.errors.is_empty() {
            Ok(statements)
        } else {
            Err(errors)
        }
    }

    fn expression(&mut self) -> Result<Box<Expr>, ParseError> {
        self.assignment()
    }

    fn statement(&mut self) -> Result<Box<Stmt>, ParseError> {
        if self.match_token(&[TokenType::For]) {
            return self.for_statement();
        }
        if self.match_token(&[TokenType::If]) {
            return self.if_statement();
        }
        if self.match_token(&[TokenType::Print]) {
            return self.print_statement();
        }
        if self.match_token(&[TokenType::Return]) {
            return self.return_statement();
        }
        if self.match_token(&[TokenType::While]) {
            return self.while_statement();
        }
        if self.match_token(&[TokenType::LeftBrace]) {
            return self.block();
        }
        self.expression_statement()
    }

    fn declaration(&mut self) -> Result<Box<Stmt>, ParseError> {
        if self.match_token(&[TokenType::Fun]) {
            return self.function("function".to_string());
        }
        let res = if self.match_token(&[TokenType::Var]) {
            self.var_declaration()
        } else {
            self.statement()
        };
        match res {
            Ok(stmt) => Ok(stmt),
            Err(e) => {
                self.synchronize();
                Err(e)
            }
        }
    }

    fn for_statement(&mut self) -> Result<Box<Stmt>, ParseError> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.")?;
        let initializer = if self.match_token(&[TokenType::Semicolon]) {
            None
        } else if self.match_token(&[TokenType::Var]) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        let mut condition = if !self.check(&TokenType::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(TokenType::Semicolon, "Expect ';' after loop condition.")?;

        let increment = if !self.check(&TokenType::RightParen) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(TokenType::RightParen, "Expect ')' after for clauses.")?;

        let mut body = self.statement()?;

        if let Some(increment) = increment {
            body = Box::new(Stmt::Block {
                statements: vec![
                    body,
                    Box::new(Stmt::Expression {
                        expression: increment,
                    }),
                ],
            });
        };

        if condition.is_none() {
            condition = Some(Box::new(Expr::Literal {
                value: LoxObject::Boolean(true),
            }));
        }
        body = Box::new(Stmt::While {
            condition: condition.unwrap(),
            body,
        });

        if let Some(initializer) = initializer {
            body = Box::new(Stmt::Block {
                statements: vec![initializer, body],
            });
        }
        Ok(body)
    }

    fn if_statement(&mut self) -> Result<Box<Stmt>, ParseError> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after if condition.")?;

        let then_branch = self.statement()?;
        let else_branch = if self.match_token(&[TokenType::Else]) {
            Some(self.statement()?)
        } else {
            None
        };

        Ok(Box::new(Stmt::If {
            condition,
            then_branch,
            else_branch,
        }))
    }

    fn print_statement(&mut self) -> Result<Box<Stmt>, ParseError> {
        let value = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after value.")?;
        Ok(Box::new(Stmt::Print { expression: value }))
    }

    fn return_statement(&mut self) -> Result<Box<Stmt>, ParseError> {
        let keyword = self.previous().clone();
        let value = if !self.check(&TokenType::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(TokenType::Semicolon, "Expect ';' after return value.")?;
        Ok(Box::new(Stmt::Return { keyword, value }))
    }

    fn while_statement(&mut self) -> Result<Box<Stmt>, ParseError> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after condition.")?;

        let body = self.statement()?;

        Ok(Box::new(Stmt::While { condition, body }))
    }

    fn expression_statement(&mut self) -> Result<Box<Stmt>, ParseError> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after expression.")?;
        Ok(Box::new(Stmt::Expression { expression: expr }))
    }

    fn function(&mut self, kind: String) -> Result<Box<Stmt>, ParseError> {
        let name = self
            .consume(
                TokenType::Identifier("".to_string()),
                &format!("Expect {} name.", kind),
            )?
            .clone();

        self.consume(TokenType::LeftParen, "Expect '(' after {} name.")?;
        let mut parameters = Vec::new();
        if !self.check(&TokenType::RightParen) {
            loop {
                if parameters.len() >= 255 {
                    return Err(ParseError::new(
                        self.peek().clone(),
                        "Cannot have more than 255 parameters.".to_string(),
                    ));
                }

                parameters.push(
                    self.consume(
                        TokenType::Identifier("".to_string()),
                        "Expect parameter name.",
                    )?
                    .clone(),
                );
                if !self.match_token(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen, "Expect ')' after parameters.")?;
        self.consume(
            TokenType::LeftBrace,
            &format!("Expect '{{' before {} body.", kind),
        )?;
        if let Stmt::Block { statements } = *self.block()? {
            Ok(Box::new(Stmt::Function {
                name: name.clone(),
                params: parameters,
                body: statements,
            }))
        } else {
            Err(ParseError::new(
                self.peek().clone(),
                "Expect {} body.".to_string(),
            ))
        }
    }

    fn var_declaration(&mut self) -> Result<Box<Stmt>, ParseError> {
        let name = self
            .consume(
                TokenType::Identifier("".to_string()),
                "Expect variable name.",
            )?
            .clone();
        let initializer = if self.match_token(&[TokenType::Equal]) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        )?;
        Ok(Box::new(Stmt::Var { name, initializer }))
    }

    fn block(&mut self) -> Result<Box<Stmt>, ParseError> {
        let mut statements = Vec::new();

        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.declaration()?);
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.")?;
        Ok(Box::new(Stmt::Block { statements }))
    }

    fn assignment(&mut self) -> Result<Box<Expr>, ParseError> {
        let expr = self.or();

        if self.match_token(&[TokenType::Equal]) {
            let equals = self.previous().clone();
            let value = self.assignment()?;

            match expr {
                Ok(expr) => {
                    if let Expr::Variable { name } = *expr {
                        return Ok(Box::new(Expr::Assign {
                            name: name.clone(),
                            value,
                        }));
                    }
                }
                Err(e) => return Err(e),
            };

            return Err(ParseError::new(
                equals,
                "Invalid assignment target.".to_string(),
            ));
        }
        expr
    }

    fn or(&mut self) -> Result<Box<Expr>, ParseError> {
        let mut expr = self.and();

        while self.match_token(&[TokenType::Or]) {
            let operator = self.previous().clone();
            let right = self.and()?;

            expr = Ok(Box::new(Expr::Logical {
                left: expr?,
                operator: operator.clone(),
                right,
            }));
        }
        expr
    }

    fn and(&mut self) -> Result<Box<Expr>, ParseError> {
        let mut expr = self.equality();

        while self.match_token(&[TokenType::And]) {
            let operator = self.previous().clone();
            let right = self.equality()?;

            expr = Ok(Box::new(Expr::Logical {
                left: expr?,
                operator: operator.clone(),
                right,
            }));
        }
        expr
    }

    fn equality(&mut self) -> Result<Box<Expr>, ParseError> {
        let mut expr = self.comparison();

        while self.match_token(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous().clone();
            let right = self.comparison()?;
            expr = Ok(Box::new(Expr::Binary {
                left: expr?,
                operator,
                right,
            }));
        }
        expr
    }

    fn comparison(&mut self) -> Result<Box<Expr>, ParseError> {
        let mut expr = self.term();

        while self.match_token(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator = self.previous().clone();
            let right = self.term()?;
            expr = Ok(Box::new(Expr::Binary {
                left: expr?,
                operator,
                right,
            }));
        }
        expr
    }

    fn term(&mut self) -> Result<Box<Expr>, ParseError> {
        let mut expr = self.factor();

        while self.match_token(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous().clone();
            let right = self.factor()?;
            expr = Ok(Box::new(Expr::Binary {
                left: expr?,
                operator,
                right,
            }));
        }
        expr
    }

    fn factor(&mut self) -> Result<Box<Expr>, ParseError> {
        let mut expr = self.unary();

        while self.match_token(&[TokenType::Slash, TokenType::Star]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            expr = Ok(Box::new(Expr::Binary {
                left: expr?,
                operator,
                right,
            }));
        }
        expr
    }

    fn unary(&mut self) -> Result<Box<Expr>, ParseError> {
        if self.match_token(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            return Ok(Box::new(Expr::Unary { operator, right }));
        }
        self.call()
    }

    fn call(&mut self) -> Result<Box<Expr>, ParseError> {
        let mut expr = self.primary()?;

        while self.match_token(&[TokenType::LeftParen]) {
            expr = self.finish_call(expr)?;
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: Box<Expr>) -> Result<Box<Expr>, ParseError> {
        let mut arguments = Vec::new();

        if !self.check(&TokenType::RightParen) {
            loop {
                if arguments.len() >= 255 {
                    return Err(ParseError {
                        token: self.peek().clone(),
                        message: "Can't have more than 255 arguments.".to_string(),
                    });
                }

                arguments.push(self.expression()?);
                if !self.match_token(&[TokenType::Comma]) {
                    break;
                }
            }
        }
        let paren = self.consume(TokenType::RightParen, "Expect ')' after arguments.")?;

        Ok(Box::new(Expr::Call {
            callee,
            paren: paren.clone(),
            arguments,
        }))
    }

    fn primary(&mut self) -> Result<Box<Expr>, ParseError> {
        if self.match_token(&[TokenType::False]) {
            return Ok(Box::new(Expr::Literal {
                value: LoxObject::Boolean(false),
            }));
        }
        if self.match_token(&[TokenType::True]) {
            return Ok(Box::new(Expr::Literal {
                value: LoxObject::Boolean(true),
            }));
        }
        if self.match_token(&[TokenType::Nil]) {
            return Ok(Box::new(Expr::Literal {
                value: LoxObject::Nil,
            }));
        }
        if self.match_token(&[TokenType::Number(0.0)]) {
            if let TokenType::Number(value) = self.previous().token_type {
                return Ok(Box::new(Expr::Literal {
                    value: LoxObject::Number(value),
                }));
            }
            return Err(ParseError::new(
                self.previous().clone(),
                "Expected number".to_string(),
            ));
        }
        if self.match_token(&[TokenType::String("".to_string())]) {
            if let TokenType::String(value) = &self.previous().token_type {
                return Ok(Box::new(Expr::Literal {
                    value: LoxObject::String(value.to_string()),
                }));
            }
            return Err(ParseError::new(
                self.previous().clone(),
                "Expected string".to_string(),
            ));
        }
        if self.match_token(&[TokenType::LeftParen]) {
            let expr = self.expression()?;
            self.consume(TokenType::RightParen, "Expect ')' after expression.")?;
            return Ok(expr);
        }
        if self.match_token(&[TokenType::Identifier("".to_string())]) {
            return Ok(Box::new(Expr::Variable {
                name: self.previous().clone(),
            }));
        }
        Err(ParseError::new(
            self.peek().clone(),
            "Expect expression.".to_string(),
        ))
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().token_type == TokenType::Semicolon {
                return;
            }
            match self.peek().token_type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => {}
            }
            self.advance();
        }
    }

    fn match_token(&mut self, types: &[TokenType]) -> bool {
        for t in types {
            if self.check(t) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<&Token, ParseError> {
        if self.check(&token_type) {
            return Ok(self.advance());
        }
        Err(ParseError::new(self.peek().clone(), message.to_string()))
    }

    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        // Discriminant checks only enum varaiant and not inner value
        std::mem::discriminant(&self.peek().token_type) == std::mem::discriminant(token_type)
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::Eof
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&mut self) -> &Token {
        &self.tokens[self.current - 1]
    }
}
