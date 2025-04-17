use thiserror::Error;

use crate::lexer::{Literal, Token, TokenType};

pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

macro_rules! next_token_matches {
    ($self: ident, $token_type:pat$(,$lexeme:literal)?) => {
        $self.peek().is_some_and(|token| matches!(token.token_type,$token_type $(if token.lexeme == $lexeme)?))
    }
}

macro_rules! second_next_token_matches {
    ($self: ident, $token_type:pat$(,$lexeme:literal)?) => {
        $self.peek2().is_some_and(|token| matches!(token.token_type,$token_type $(if token.lexeme == $lexeme)?))
    }
}

macro_rules! define_binary_expression_parsers {
    ($($expr_type:ident,$subexpr_type:ident,$operator_token_type:pat),*) => {
        $(
            fn $expr_type(&mut self) -> Result<Expr, ParsingError> {
                let mut $expr_type = self.$subexpr_type()?;

                while next_token_matches!(self,$operator_token_type)
                {
                    let operator = self.advance().unwrap().clone();
                    let $subexpr_type = self.$subexpr_type()?;
                    $expr_type = Expr::Binary(Box::new($expr_type), operator, Box::new($subexpr_type));
                }
                Ok($expr_type)
            }
        )*
    }
}

macro_rules! expect {
    ($self: ident, $token_type:path$(,$lexeme:literal)?) => {
        {
            let next_token = $self.advance();
            match next_token {
                None => { return Err(ParsingError::new(0, ParsingErrorType::NothingToParse)); }
                Some(token) => {
                    if !matches!(token,
                        Token {
                            token_type: $token_type,
                            lexeme: _lexeme,
                            ..
                        }
                        $(if _lexeme == $lexeme)?
                    ) {
                        return Err(ParsingError::new(
                            token.line,
                            ParsingErrorType::UnexpectedToken(token.lexeme.clone()),
                        ));
                    }
                }
            }
        }
    };
}

impl Parser {
    pub fn new(tokens: &[Token]) -> Self {
        Self {
            tokens: tokens.to_vec(),
            position: 0,
        }
    }

    fn advance(&mut self) -> Option<&Token> {
        if self.position < self.tokens.len() {
            let token = &self.tokens[self.position];
            self.position += 1;
            Some(token)
        } else {
            None
        }
    }

    fn peek(&self) -> Option<&Token> {
        if self.position < self.tokens.len() {
            Some(&self.tokens[self.position])
        } else {
            None
        }
    }

    fn peek2(&self) -> Option<&Token> {
        if self.position + 1 < self.tokens.len() {
            Some(&self.tokens[self.position + 1])
        } else {
            None
        }
    }

    fn primary(&mut self) -> Result<Expr, ParsingError> {
        let Some(
            token @ Token {
                token_type,
                value,
                lexeme,
                line,
            },
        ) = self.advance()
        else {
            return Err(ParsingError::new(0, ParsingErrorType::NothingToParse));
        };

        let line = *line;

        match (token_type, value) {
            (TokenType::Number, Some(l @ Literal::Number(_)))
            | (TokenType::String, Some(l @ Literal::String(_))) => {
                Ok(Expr::Literal(LiteralExpr::Lit(l.clone())))
            }
            (TokenType::Keyword, _) => match lexeme.as_str() {
                "true" => Ok(Expr::Literal(LiteralExpr::True)),
                "false" => Ok(Expr::Literal(LiteralExpr::False)),
                "nil" => Ok(Expr::Literal(LiteralExpr::Nil)),
                _ => Err(ParsingError::new(
                    line,
                    ParsingErrorType::UnexpectedToken(lexeme.to_string()),
                )),
            },
            (TokenType::LeftParen, _) => {
                let expr = self.expr()?;
                if !matches!(
                    self.advance(),
                    Some(Token {
                        token_type: TokenType::RightParen,
                        ..
                    })
                ) {
                    return Err(ParsingError::new(
                        line,
                        ParsingErrorType::UnbalancedGrouping,
                    ));
                }
                Ok(Expr::Grouping(Box::new(expr)))
            }
            (TokenType::Identifier, _) => Ok(Expr::Var(token.clone())),
            _ => Err(ParsingError::new(
                line,
                ParsingErrorType::UnexpectedToken(lexeme.clone()),
            )),
        }
    }

    fn arguments(&mut self) -> Result<Vec<Expr>, ParsingError> {
        let mut arguments = vec![self.expr()?];
        while next_token_matches!(self, TokenType::Comma) {
            expect!(self, TokenType::Comma);
            arguments.push(self.expr()?);
        }
        Ok(arguments)
    }

    fn function_call(&mut self) -> Result<Expr, ParsingError> {
        let callee = self.primary()?;
        if next_token_matches!(self, TokenType::LeftParen) {
            let lparen = self.advance().unwrap().clone();
            let mut arguments: Vec<Expr> = vec![];
            if next_token_matches!(self, TokenType::RightParen) {
                arguments.append(&mut self.arguments()?);
            }
            expect!(self, TokenType::RightParen);
            Ok(Expr::FnCall(Box::new(callee), lparen, arguments))
        } else {
            Ok(callee)
        }
    }

    fn unary(&mut self) -> Result<Expr, ParsingError> {
        if next_token_matches!(self, TokenType::Bang | TokenType::Minus) {
            let operator = self.advance().unwrap().clone();
            Ok(Expr::Unary(operator, Box::new(self.unary()?)))
        } else {
            self.function_call()
        }
    }

    define_binary_expression_parsers! {
        equality,comparison,TokenType::BangEqual | TokenType::EqualEqual,
        comparison,term,TokenType::Greater | TokenType::GreaterEqual | TokenType::Less | TokenType::LessEqual,
        term,factor,TokenType::Minus | TokenType::Plus,
        factor,unary,TokenType::Slash | TokenType::Star
    }

    fn assignment(&mut self) -> Result<Expr, ParsingError> {
        if self.peek().is_none() {
            // TODO: proper and clean error handling with correct line numbers
            return Err(ParsingError::new(0, ParsingErrorType::NothingToParse));
        }
        if next_token_matches!(self, TokenType::Identifier)
            && second_next_token_matches!(self, TokenType::Equal)
        {
            let identifier_token = self.advance().unwrap().clone();
            expect!(self, TokenType::Equal);
            let assignment = self.assignment()?;
            Ok(Expr::Assign(
                identifier_token.clone(),
                Box::new(assignment.clone()),
            ))
        } else {
            self.logic_or()
        }
    }

    fn expr(&mut self) -> Result<Expr, ParsingError> {
        self.assignment()
    }

    fn ast(&mut self) -> Result<Ast, ParsingError> {
        let expr = self.expr()?;
        // TODO: Refactor after error handling cleanup
        match self.peek() {
            Some(Token {
                token_type, line, ..
            }) if !matches!(token_type, TokenType::Eof) => {
                Err(ParsingError::new(*line, ParsingErrorType::UnparsedTokens))
            }
            _ => Ok(Ast::Expr(expr)),
        }
    }

    pub fn statement(&mut self) -> Result<Statement, ParsingError> {
        let Some(Token {
            token_type, lexeme, ..
        }) = self.peek().cloned()
        else {
            return Err(ParsingError::new(0, ParsingErrorType::NothingToParse));
        };
        let mut expect_semicolon = true;
        let return_value = match token_type {
            TokenType::Keyword if lexeme == "print" => {
                self.advance();
                let expr = self.expr()?;
                Ok(Statement::Print(expr))
            }
            TokenType::LeftBrace => {
                self.advance();
                let mut statements = vec![];
                while !next_token_matches!(self, TokenType::RightBrace | TokenType::Eof) {
                    statements.push(self.declaration()?);
                }
                if let Some(Token {
                    token_type: TokenType::Eof,
                    line,
                    ..
                }) = self.peek()
                {
                    return Err(ParsingError::new(
                        *line,
                        ParsingErrorType::UnexpectedEof("}".to_string()),
                    ));
                }
                self.advance();
                expect_semicolon = false;
                Ok(Statement::Block(statements))
            }
            TokenType::Keyword if lexeme == "if" || lexeme == "while" => {
                self.advance();
                expect!(self, TokenType::LeftParen);
                let condition = self.expr()?;
                expect!(self, TokenType::RightParen);
                let body = self.statement()?;
                expect_semicolon = false;
                if lexeme == "if" {
                    let else_body = if next_token_matches!(self, TokenType::Keyword, "else") {
                        expect!(self, TokenType::Keyword, "else");
                        Some(self.statement()?)
                    } else {
                        None
                    };
                    Ok(Statement::If(
                        condition,
                        Box::new(body),
                        else_body.map(Box::new),
                    ))
                } else {
                    Ok(Statement::While(condition, Box::new(body)))
                }
            }
            TokenType::Keyword if lexeme == "for" => {
                expect!(self, TokenType::Keyword, "for");
                expect!(self, TokenType::LeftParen);
                let initializer = if !next_token_matches!(self, TokenType::Semicolon) {
                    Some(self.declaration()?)
                } else {
                    expect!(self, TokenType::Semicolon);
                    None
                };
                if !matches!(
                    initializer,
                    None | Some(
                        Declaration::Var(_, _) | Declaration::Statement(Statement::Expr(_))
                    ),
                ) {
                    return Err(ParsingError::new(
                        self.peek().map(|t| t.line).unwrap_or(0),
                        ParsingErrorType::UnexpectedToken(
                            self.peek()
                                .map(|t| t.lexeme.clone())
                                .unwrap_or("".to_string()),
                        ),
                    ));
                }
                let condition = if !next_token_matches!(self, TokenType::Semicolon) {
                    Some(self.expr()?)
                } else {
                    None
                };
                expect!(self, TokenType::Semicolon);
                let increment = if !next_token_matches!(self, TokenType::RightParen) {
                    Some(self.expr()?)
                } else {
                    None
                };
                expect!(self, TokenType::RightParen);
                let body = self.statement()?;
                expect_semicolon = false;
                Ok(Statement::For(
                    initializer.map(Box::new),
                    condition,
                    increment,
                    Box::new(body),
                ))
            }
            _ => {
                let expr = self.expr()?;
                Ok(Statement::Expr(expr))
            }
        };

        if expect_semicolon {
            expect!(self, TokenType::Semicolon);
        }
        return_value
    }

    pub fn var_declaration(&mut self) -> Result<Declaration, ParsingError> {
        if !matches!(self.advance(),
        Some(Token {
            token_type: TokenType::Keyword,
            lexeme,
            ..
        }) if lexeme == "var")
        {
            return Err(ParsingError::new(0, ParsingErrorType::ExpectedVar));
        }
        let Some(
            identifier_token @ Token {
                token_type: TokenType::Identifier,
                ..
            },
        ) = self.advance()
        else {
            return Err(ParsingError::new(0, ParsingErrorType::ExpectedIdentifier));
        };

        let identifier_token = identifier_token.clone();

        let expression = if next_token_matches!(self, TokenType::Equal) {
            self.advance();
            Some(self.expr()?)
        } else {
            None
        };

        if !matches!(
            self.advance(),
            Some(Token {
                token_type: TokenType::Semicolon,
                ..
            })
        ) {
            return Err(ParsingError::new(
                identifier_token.line,
                ParsingErrorType::ExpectedSemicolon,
            ));
        }

        Ok(Declaration::Var(identifier_token, expression))
    }

    pub fn declaration(&mut self) -> Result<Declaration, ParsingError> {
        if next_token_matches!(self, TokenType::Keyword, "var") {
            Ok(self.var_declaration()?)
        } else if self.peek().is_some() {
            Ok(Declaration::Statement(self.statement()?))
        } else {
            todo!("Error handling")
        }
    }

    pub fn program(&mut self) -> Result<Program, ParsingError> {
        let mut declarations = Vec::new();
        while !next_token_matches!(self, TokenType::Eof) {
            declarations.push(self.declaration()?);
        }
        Ok(declarations)
    }

    pub fn parse_program(&mut self) -> Result<Program, ParsingError> {
        self.program()
    }

    pub fn parse_ast(&mut self) -> Result<Ast, ParsingError> {
        self.ast()
    }

    // TODO: refactor this into the macro for binaries?
    fn logic_or(&mut self) -> Result<Expr, ParsingError> {
        let mut logic_or = self.logic_and()?;
        while next_token_matches!(self, TokenType::Keyword, "or") {
            let operator = self.advance().unwrap().clone();
            let logic_and = self.logic_and()?;
            logic_or = Expr::Logical(Box::new(logic_or), operator, Box::new(logic_and));
        }
        Ok(logic_or)
    }

    fn logic_and(&mut self) -> Result<Expr, ParsingError> {
        let mut logic_and = self.equality()?;
        while next_token_matches!(self, TokenType::Keyword, "and") {
            let operator = self.advance().unwrap().clone();
            let equality = self.equality()?;
            logic_and = Expr::Logical(Box::new(logic_and), operator, Box::new(equality));
        }
        Ok(logic_and)
    }
}

#[derive(Debug)]
pub enum Ast {
    Expr(Expr),
}

pub type Program = Vec<Declaration>;

#[derive(Clone, Debug)]
pub enum Declaration {
    Var(Token, Option<Expr>),
    Statement(Statement),
}

#[derive(Clone, Debug)]
pub enum Statement {
    Expr(Expr),
    Print(Expr),
    Block(Vec<Declaration>),
    If(Expr, Box<Statement>, Option<Box<Statement>>),
    While(Expr, Box<Statement>),
    // TODO: remove this and just use While at the syntax level
    For(
        Option<Box<Declaration>>,
        Option<Expr>,
        Option<Expr>,
        Box<Statement>,
    ),
}

#[derive(Clone, Debug)]
pub enum Expr {
    Literal(LiteralExpr),
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    Var(Token),
    Assign(Token, Box<Expr>),
    Logical(Box<Expr>, Token, Box<Expr>),
    FnCall(Box<Expr>, Token, Vec<Expr>),
}

#[derive(Debug, Clone)]
pub enum LiteralExpr {
    Lit(Literal),
    True,
    False,
    Nil,
}

#[derive(Error, Debug)]
pub enum ParsingErrorType {
    #[error("Unparsed tokens")]
    UnparsedTokens,
    #[error("Nothing to parse")]
    NothingToParse,
    #[error("Error at '{0}': Expect expression")]
    UnexpectedToken(String),
    #[error("Unbalanced grouping")]
    UnbalancedGrouping,
    #[error("Expected semicolon")]
    ExpectedSemicolon,
    #[error("Expected 'var'")]
    ExpectedVar,
    #[error("Expected identifier")]
    ExpectedIdentifier,
    #[error("Error at end: Expect '{0}'")]
    UnexpectedEof(String),
}

#[derive(Error, Debug)]
#[error("[line {line}] {error}.")]
pub struct ParsingError {
    line: usize,
    error: ParsingErrorType,
}

impl ParsingError {
    pub fn new(line: usize, error: ParsingErrorType) -> Self {
        Self { line, error }
    }
}

pub trait Visit {
    fn visit_ast(&self, ast: Ast) {
        match ast {
            Ast::Expr(expr) => self.visit_expr(expr),
        }
    }

    fn visit_expr(&self, expr: Expr) {
        match expr {
            Expr::Literal(literal_expr) => self.visit_expr_literal(literal_expr),
            Expr::Unary(_, unary) => self.visit_expr(*unary),
            Expr::Binary(expr, _, expr1) => {
                self.visit_expr(*expr);
                self.visit_expr(*expr1);
            }
            Expr::Grouping(expr) => self.visit_grouping(*expr),
            Expr::Var(_) => todo!(),
            Expr::Assign(_token, _expr) => todo!(),
            Expr::Logical(_expr, _token, _expr1) => todo!(),
            Expr::FnCall(_expr, _token, _vec) => todo!(),
        }
    }

    fn visit_expr_literal(&self, literal_expr: LiteralExpr) {
        if let LiteralExpr::Lit(literal) = literal_expr {
            self.visit_literal(literal);
        }
    }

    fn visit_literal(&self, _: Literal) {}

    fn visit_grouping(&self, expr: Expr) {
        self.visit_expr(expr);
    }
}

pub struct AstPrinter;

impl Visit for AstPrinter {
    fn visit_expr_literal(&self, literal_expr: LiteralExpr) {
        print!(
            "{}",
            match literal_expr {
                LiteralExpr::Lit(literal) => format!("{literal}"),
                LiteralExpr::True => "true".to_string(),
                LiteralExpr::False => "false".to_string(),
                LiteralExpr::Nil => "nil".to_string(),
            }
        )
    }

    fn visit_grouping(&self, expr: Expr) {
        print!("(group ");
        self.visit_expr(expr);
        print!(")")
    }

    fn visit_ast(&self, ast: Ast) {
        match ast {
            Ast::Expr(expr) => self.visit_expr(expr),
        }
        println!();
    }

    fn visit_expr(&self, expr: Expr) {
        match expr {
            Expr::Literal(literal_expr) => self.visit_expr_literal(literal_expr),
            Expr::Unary(operator, expr) => {
                print!("({} ", operator.lexeme);
                self.visit_expr(*expr);
                print!(")");
            }
            Expr::Binary(expr, operator, expr1) => {
                print!("({} ", operator.lexeme);
                self.visit_expr(*expr);
                print!(" ");
                self.visit_expr(*expr1);
                print!(")");
            }
            Expr::Grouping(expr) => self.visit_grouping(*expr),
            Expr::Var(token) => {
                print!("{}", token.lexeme)
            }
            Expr::Assign(token, expr) => {
                print!("{} = ", token.lexeme);
                self.visit_expr(*expr)
            }
            Expr::Logical(expr1, token, expr2) => {
                self.visit_expr(*expr1);
                print!("{}", token.lexeme);
                self.visit_expr(*expr2);
            }
            Expr::FnCall(_expr, _token, _vec) => todo!(),
        }
    }
}
