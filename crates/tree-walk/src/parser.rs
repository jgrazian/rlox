use crate::ast::*;
use crate::token::Token;
use crate::token_type::TokenType;
use crate::LoxError;

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Vec<Box<Stmt>>, Vec<LoxError>> {
        let mut errors = Vec::new();
        let mut statements = Vec::new();

        while !self.is_at_end() {
            match self.declaration() {
                Ok(expr) => statements.push(expr),
                Err(e) => errors.push(e),
            }
        }

        if errors.is_empty() {
            Ok(statements)
        } else {
            Err(errors)
        }
    }

    fn expression(&mut self) -> Result<Box<Expr>, LoxError> {
        self.assignment()
    }

    fn statement(&mut self) -> Result<Box<Stmt>, LoxError> {
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

    fn declaration(&mut self) -> Result<Box<Stmt>, LoxError> {
        if self.match_token(&[TokenType::Class]) {
            return self.class_declaration();
        }
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

    fn class_declaration(&mut self) -> Result<Box<Stmt>, LoxError> {
        let name = self
            .consume(TokenType::Identifier("".to_string()), "Expect class name.")?
            .clone();

        let superclass = if self.match_token(&[TokenType::Less]) {
            self.consume(
                TokenType::Identifier("".to_string()),
                "Expect superclass name",
            )?;
            Some(Box::new(Expr::Variable {
                name: self.previous().clone(),
            }))
        } else {
            None
        };

        self.consume(TokenType::LeftBrace, "Expect '{' before class body.")?;

        let mut methods = Vec::new();
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            methods.push(self.function("method".to_string())?);
        }

        self.consume(TokenType::RightBrace, "Expect '}' after class body.")?;
        Ok(Box::new(Stmt::Class {
            name,
            methods,
            superclass,
        }))
    }

    fn for_statement(&mut self) -> Result<Box<Stmt>, LoxError> {
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

    fn if_statement(&mut self) -> Result<Box<Stmt>, LoxError> {
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

    fn print_statement(&mut self) -> Result<Box<Stmt>, LoxError> {
        let value = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after value.")?;
        Ok(Box::new(Stmt::Print { expression: value }))
    }

    fn return_statement(&mut self) -> Result<Box<Stmt>, LoxError> {
        let keyword = self.previous().clone();
        let value = if !self.check(&TokenType::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(TokenType::Semicolon, "Expect ';' after return value.")?;
        Ok(Box::new(Stmt::Return { keyword, value }))
    }

    fn while_statement(&mut self) -> Result<Box<Stmt>, LoxError> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after condition.")?;

        let body = self.statement()?;

        Ok(Box::new(Stmt::While { condition, body }))
    }

    fn expression_statement(&mut self) -> Result<Box<Stmt>, LoxError> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after expression.")?;
        Ok(Box::new(Stmt::Expression { expression: expr }))
    }

    fn function(&mut self, kind: String) -> Result<Box<Stmt>, LoxError> {
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
                    return Err(LoxError::parse_error(
                        self.peek(),
                        "Cannot have more than 255 parameters.",
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
            Err(LoxError::parse_error(self.peek(), "Expect function body."))
        }
    }

    fn var_declaration(&mut self) -> Result<Box<Stmt>, LoxError> {
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

    fn block(&mut self) -> Result<Box<Stmt>, LoxError> {
        let mut statements = Vec::new();

        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.declaration()?);
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.")?;
        Ok(Box::new(Stmt::Block { statements }))
    }

    fn assignment(&mut self) -> Result<Box<Expr>, LoxError> {
        let expr = self.or();

        if self.match_token(&[TokenType::Equal]) {
            let equals = self.previous().clone();
            let value = self.assignment()?;

            match expr {
                Ok(expr) => match *expr {
                    Expr::Variable { name } => {
                        return Ok(Box::new(Expr::Assign {
                            name: name.clone(),
                            value,
                        }))
                    }
                    Expr::Get { object, name } => {
                        return Ok(Box::new(Expr::Set {
                            object,
                            name: name.clone(),
                            value,
                        }))
                    }
                    _ => return Err(LoxError::parse_error(&equals, "Invalid assignment target.")),
                },
                Err(e) => return Err(e),
            };
        }
        expr
    }

    fn or(&mut self) -> Result<Box<Expr>, LoxError> {
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

    fn and(&mut self) -> Result<Box<Expr>, LoxError> {
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

    fn equality(&mut self) -> Result<Box<Expr>, LoxError> {
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

    fn comparison(&mut self) -> Result<Box<Expr>, LoxError> {
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

    fn term(&mut self) -> Result<Box<Expr>, LoxError> {
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

    fn factor(&mut self) -> Result<Box<Expr>, LoxError> {
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

    fn unary(&mut self) -> Result<Box<Expr>, LoxError> {
        if self.match_token(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            return Ok(Box::new(Expr::Unary { operator, right }));
        }
        self.call()
    }

    fn call(&mut self) -> Result<Box<Expr>, LoxError> {
        let mut expr = self.primary()?;

        loop {
            if self.match_token(&[TokenType::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else if self.match_token(&[TokenType::Dot]) {
                let name = self.consume(
                    TokenType::Identifier("".to_string()),
                    "Expect property name after '.'.",
                )?;
                expr = Box::new(Expr::Get {
                    object: expr,
                    name: name.clone(),
                });
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Box<Expr>) -> Result<Box<Expr>, LoxError> {
        let mut arguments = Vec::new();

        if !self.check(&TokenType::RightParen) {
            loop {
                if arguments.len() >= 255 {
                    return Err(LoxError::parse_error(
                        self.peek(),
                        "Can't have more than 255 arguments.",
                    ));
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

    fn primary(&mut self) -> Result<Box<Expr>, LoxError> {
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
            return Err(LoxError::parse_error(self.previous(), "Expected number"));
        }
        if self.match_token(&[TokenType::String("".to_string())]) {
            if let TokenType::String(value) = &self.previous().token_type {
                return Ok(Box::new(Expr::Literal {
                    value: LoxObject::String(value.to_string()),
                }));
            }
            return Err(LoxError::parse_error(self.previous(), "Expected string"));
        }
        if self.match_token(&[TokenType::LeftParen]) {
            let expr = self.expression()?;
            self.consume(TokenType::RightParen, "Expect ')' after expression.")?;
            return Ok(expr);
        }
        if self.match_token(&[TokenType::Super]) {
            let keyword = self.previous().clone();
            self.consume(TokenType::Dot, "Expect '.' after 'super'.")?;
            let method = self
                .consume(
                    TokenType::Identifier("".to_string()),
                    "Expect superclass method name.",
                )?
                .clone();
            return Ok(Box::new(Expr::Super { keyword, method }));
        }
        if self.match_token(&[TokenType::This]) {
            return Ok(Box::new(Expr::This {
                keyword: self.previous().clone(),
            }));
        }
        if self.match_token(&[TokenType::Identifier("".to_string())]) {
            return Ok(Box::new(Expr::Variable {
                name: self.previous().clone(),
            }));
        }
        Err(LoxError::parse_error(self.peek(), "Expect expression."))
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

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<&Token, LoxError> {
        if self.check(&token_type) {
            return Ok(self.advance());
        }
        Err(LoxError::parse_error(self.peek(), message))
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
