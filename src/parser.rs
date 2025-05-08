use std::fmt::Display;

use thiserror::Error;

use crate::{
    error::ErrorAtToken,
    lexer::{Literal, Token, TokenType as TT},
};

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
    ($($expr_type:ident = chain of $subexpr_type:ident joined by $operator_token_type:pat$(=$lexeme:literal)?),*) => {
        $(
            fn $expr_type(&mut self) -> ParsingResult<Expr> {
                let mut $expr_type = self.$subexpr_type()?;

                while next_token_matches!(self,$operator_token_type $(,$lexeme)?)
                {
                    let operator = self.advance().unwrap().clone();
                    let $subexpr_type = self.$subexpr_type()?;
                    let type_constructor = if matches!(TT::Keyword, $operator_token_type) {
                        Expr::Logical
                    }
                    else {
                        Expr::Binary
                    };
                    $expr_type = type_constructor(Box::new($expr_type), operator, Box::new($subexpr_type));
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
            #[allow(unused_variables)]
            let expected_lexeme = format!("{}",$token_type);
            $(let expected_lexeme = $lexeme.to_string();)?
            match next_token {
                None => { return $self.error(NothingToParse); }
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
                            token,
                            UnexpectedToken{
                                expected: format!("'{}'",expected_lexeme)
                            },
                        ));
                    }
                }
            }
            next_token.unwrap()
        }
    };
}

pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
    current_token: Token,
    current_address_prefix: String,
    current_address_index: usize,
}
pub type Program = Statement;

#[derive(Clone, Debug)]
pub enum Declaration {
    Var(Token, Option<Expr>),
    Statement(Statement),
    Function(Token, Vec<Token>, Statement),
    Class(Token, Option<Expr>, Statement),
}

#[derive(Clone, Debug)]
pub enum Statement {
    Expr(Expr),
    Print(Expr),
    Block(Vec<Declaration>),
    If(Expr, Box<Statement>, Option<Box<Statement>>),
    While(Expr, Box<Statement>),
    Return(Token, Option<Expr>),
}

#[derive(Clone, Debug)]
pub enum Expr {
    Literal(LiteralExpr),
    Name(Token, String),
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    Assign(Box<Expr>, Box<Expr>),
    Logical(Box<Expr>, Token, Box<Expr>),
    Call(Box<Expr>, Token, Vec<Expr>),
    Dotted(Token, Box<Expr>, Box<Expr>),
    This(Token),
    Super(Token, Token),
}

#[derive(Debug, Clone)]
pub enum LiteralExpr {
    Lit(Literal),
    True,
    False,
    Nil,
}

impl Display for LiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralExpr::Lit(literal) => f.write_fmt(format_args!("{literal}")),
            LiteralExpr::True => f.write_str("true"),
            LiteralExpr::False => f.write_str("false"),
            LiteralExpr::Nil => f.write_str("nil"),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(literal_expr) => f.write_fmt(format_args!("{literal_expr}")),
            Expr::Unary(operator, unary) => {
                f.write_fmt(format_args!("({} {unary})", operator.lexeme))
            }
            Expr::Binary(expr1, operator, expr2) => {
                f.write_fmt(format_args!("({} {expr1} {expr2})", operator.lexeme))
            }
            Expr::Grouping(expr) => f.write_fmt(format_args!("(group {expr})")),
            Expr::Name(token, _) => f.write_fmt(format_args!("{}", token.lexeme)),
            Expr::Assign(assignee, expr) => f.write_fmt(format_args!("{assignee} = {expr}")),
            Expr::Logical(expr1, token, expr2) => {
                f.write_fmt(format_args!("{expr1}{}{expr2}", token.lexeme))
            }
            Expr::Call(callee, _, args) => {
                f.write_fmt(format_args!("({}) (", callee))?;
                for (i, arg) in args.iter().enumerate() {
                    f.write_fmt(format_args!("{arg}"))?;
                    if i < args.len() - 1 {
                        f.write_str(" ")?;
                    }
                }
                f.write_str(")")
            }
            Expr::Dotted(_, lhs, rhs) => f.write_fmt(format_args!("({}).[{}]", lhs, rhs)),
            Expr::This(token) => f.write_fmt(format_args!("{}", token.lexeme)),
            Expr::Super(super_token, method) => {
                f.write_fmt(format_args!("{}.{}", super_token.lexeme, method.lexeme))
            }
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Expr(expr) => f.write_fmt(format_args!("{expr}")),
            Statement::Print(expr) => f.write_fmt(format_args!("print {expr}")),
            Statement::Block(declarations) => {
                f.write_str("{")?;
                for (i, decl) in declarations.iter().enumerate() {
                    f.write_fmt(format_args!("{decl}"))?;
                    if i < declarations.len() - 1 {
                        f.write_str("; ")?;
                    }
                }
                f.write_str("}")
            }
            Statement::If(condition, then_body, else_body) => {
                f.write_fmt(format_args!("if {condition} {then_body}"))?;
                if let Some(statement) = else_body {
                    f.write_fmt(format_args!(" else {statement}"))?;
                }
                Ok(())
            }
            Statement::While(condition, body) => {
                f.write_fmt(format_args!("while {condition} {body}"))
            }
            Statement::Return(_, expr) => {
                f.write_str("return")?;
                if let Some(expr) = expr {
                    f.write_fmt(format_args!(" {expr}"))?;
                }
                Ok(())
            }
        }
    }
}

impl Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Declaration::Var(token, expr) => {
                f.write_fmt(format_args!("var {}", token.lexeme))?;
                if let Some(expr) = expr {
                    f.write_fmt(format_args!(" = {expr}"))?;
                }
                Ok(())
            }
            Declaration::Statement(statement) => f.write_fmt(format_args!("{statement}")),
            Declaration::Function(token, tokens, statement) => {
                f.write_fmt(format_args!("fun {}(", token.lexeme))?;
                for (i, arg) in tokens.iter().enumerate() {
                    f.write_fmt(format_args!("{}", arg.lexeme))?;
                    if i < tokens.len() - 1 {
                        f.write_str(", ")?;
                    }
                }
                f.write_str(")")?;
                f.write_fmt(format_args!("{statement}"))
            }
            Declaration::Class(token, expr, statement) => {
                f.write_fmt(format_args!("class {}", token.lexeme))?;
                if let Some(expr) = expr {
                    f.write_fmt(format_args!(" < {expr}"))?;
                }
                f.write_fmt(format_args!("{statement}"))
            }
        }
    }
}

#[derive(Error, Debug)]
pub enum ParsingErrorType {
    #[error("Unparsed tokens")]
    UnparsedTokens,
    #[error("Parsed over the EOF token")]
    ParsedTooMuch,
    #[error("Nothing to parse")]
    NothingToParse,
    #[error("Expected {expected}")]
    UnexpectedToken { expected: String },
    #[error("Invalid epxression on the left side of '='")]
    InvalidLhs(Option<Expr>),
    #[error("A class can't inherit from itself")]
    CantInheritFromSelf,
}

use ParsingErrorType::*;

type ParsingError = ErrorAtToken<ParsingErrorType>;
type ParsingResult<T> = Result<T, ParsingError>;

impl Parser {
    pub fn new(tokens: &[Token]) -> Self {
        Self {
            tokens: tokens.to_vec(),
            position: 0,
            current_token: tokens[0].clone(), // There must be at least one token, the EOF one
            current_address_prefix: "".to_string(),
            current_address_index: 0,
        }
    }

    fn advance(&mut self) -> Option<&Token> {
        if self.position < self.tokens.len() {
            let token = &self.tokens[self.position];
            self.position += 1;
            self.current_token = token.clone();
            Some(token)
        } else {
            None
        }
    }

    fn set_position(&mut self, pos: usize) -> bool {
        if pos < self.tokens.len() {
            let token = &self.tokens[pos];
            self.position = pos;
            self.current_token = token.clone();
            true
        } else {
            false
        }
    }

    fn error<T>(&self, error_type: ParsingErrorType) -> ParsingResult<T> {
        Err(ParsingError::new(&self.current_token, error_type))
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

    fn primary(&mut self) -> ParsingResult<Expr> {
        let Some(
            token @ Token {
                token_type,
                value,
                lexeme,
                ..
            },
        ) = self.advance()
        else {
            return self.error(NothingToParse);
        };

        match (token_type, value) {
            (TT::Number, Some(l @ Literal::Number(_)))
            | (TT::String, Some(l @ Literal::String(_))) => {
                Ok(Expr::Literal(LiteralExpr::Lit(l.clone())))
            }
            (TT::Keyword, _) => match lexeme.as_str() {
                "true" => Ok(Expr::Literal(LiteralExpr::True)),
                "false" => Ok(Expr::Literal(LiteralExpr::False)),
                "nil" => Ok(Expr::Literal(LiteralExpr::Nil)),
                "this" => Ok(Expr::This(token.clone())),
                "super" => Ok(Expr::Super(token.clone(), {
                    expect!(self, TT::Dot);
                    expect!(self, TT::Identifier).clone()
                })),
                _ => self.error(UnexpectedToken {
                    expected: "expression".to_string(),
                }),
            },
            (TT::LeftParen, _) => {
                let expr = self.expr()?;
                expect!(self, TT::RightParen);
                Ok(Expr::Grouping(Box::new(expr)))
            }
            (TT::Identifier, _) => {
                let token = token.clone();
                let first_name = Expr::Name(
                    token.clone(),
                    format!(
                        "{}{}",
                        self.current_address_prefix, self.current_address_index
                    ),
                );
                self.current_address_index += 1;
                Ok(first_name)
            }
            _ => self.error(UnexpectedToken {
                expected: "expression".to_string(),
            }),
        }
    }

    fn arguments(&mut self) -> ParsingResult<Vec<Expr>> {
        let mut arguments = vec![self.expr()?];
        while next_token_matches!(self, TT::Comma) {
            expect!(self, TT::Comma);
            arguments.push(self.expr()?);
        }
        Ok(arguments)
    }

    fn call_or_ident(&mut self, no_grouping: bool, no_literals: bool) -> ParsingResult<Expr> {
        let mut call_or_ident = self.primary()?;
        if let Expr::Literal(_) = call_or_ident {
            if no_literals {
                return self.error(UnexpectedToken {
                    expected: "any expression other than a literal".into(),
                });
            }
        }
        if let Expr::Grouping(_) = call_or_ident {
            if no_grouping {
                todo!("No grouping allowed");
            }
        }
        while next_token_matches!(self, TT::LeftParen) {
            let mut arguments: Vec<Expr> = vec![];
            let lparen = expect!(self, TT::LeftParen).clone();
            if !next_token_matches!(self, TT::RightParen) {
                arguments.append(&mut self.arguments()?);
            }
            expect!(self, TT::RightParen);
            call_or_ident = Expr::Call(Box::new(call_or_ident), lparen, arguments.clone());
        }
        Ok(call_or_ident)
    }

    fn unary(&mut self) -> ParsingResult<Expr> {
        if next_token_matches!(self, TT::Bang | TT::Minus) {
            let operator = self.advance().unwrap().clone();
            Ok(Expr::Unary(operator, Box::new(self.unary()?)))
        } else {
            let previous_position = self.position;
            match self.lhs() {
                Err(ParsingError {
                    error: InvalidLhs(Some(expr)),
                    ..
                })
                | Ok(expr) => Ok(expr),
                _ => {
                    self.set_position(previous_position);
                    self.call_or_ident(false, false)
                }
            }
        }
    }

    define_binary_expression_parsers! {
        logic_or = chain of logic_and joined by TT::Keyword = "or",
        logic_and = chain of equality joined by TT::Keyword = "and",
        equality = chain of comparison joined by TT::BangEqual | TT::EqualEqual,
        comparison = chain of term joined by TT::Greater | TT::GreaterEqual | TT::Less | TT::LessEqual,
        term = chain of factor joined by TT::Minus | TT::Plus,
        factor = chain of unary joined by TT::Slash | TT::Star
    }

    fn lhs(&mut self) -> ParsingResult<Expr> {
        if (next_token_matches!(self, TT::Identifier)
            || next_token_matches!(self, TT::Keyword, "this")
            || next_token_matches!(self, TT::Keyword, "super"))
            && second_next_token_matches!(self, TT::Dot | TT::LeftParen | TT::Equal)
        {
            let mut expr = self.call_or_ident(true, true)?;
            while next_token_matches!(self, TT::Dot) {
                let dot = expect!(self, TT::Dot).clone();
                expr = Expr::Dotted(
                    dot,
                    Box::new(expr),
                    Box::new(self.call_or_ident(false, true)?),
                )
            }
            if !next_token_matches!(self, TT::Equal) {
                return self.error(InvalidLhs(Some(expr)));
            }
            Ok(expr)
        } else {
            self.error(InvalidLhs(None))
        }
    }

    fn assignment(&mut self) -> ParsingResult<Expr> {
        let previous_position = self.position;
        match self.lhs() {
            Ok(lhs) => {
                expect!(self, TT::Equal);
                let assignment = if next_token_matches!(self, TT::Identifier)
                    && second_next_token_matches!(self, TT::LeftParen)
                {
                    self.call_or_ident(false, false)?
                } else {
                    self.assignment()?
                };

                Ok(Expr::Assign(Box::new(lhs), Box::new(assignment.clone())))
            }
            Err(ParsingError {
                error: InvalidLhs(Some(expr)),
                ..
            }) => {
                if next_token_matches!(
                    self,
                    TT::Plus
                        | TT::Minus
                        | TT::Slash
                        | TT::Star
                        | TT::BangEqual
                        | TT::EqualEqual
                        | TT::Greater
                        | TT::GreaterEqual
                        | TT::Less
                        | TT::LessEqual
                ) || next_token_matches!(self, TT::Keyword, "or")
                    || next_token_matches!(self, TT::Keyword, "and")
                {
                    self.set_position(previous_position);
                    self.logic_or()
                } else {
                    Ok(expr)
                }
            }
            _ => self.logic_or(),
        }
    }

    fn expr(&mut self) -> ParsingResult<Expr> {
        self.assignment()
    }

    pub fn statement(&mut self) -> ParsingResult<Statement> {
        let Some(Token {
            token_type, lexeme, ..
        }) = self.peek().cloned()
        else {
            return self.error(NothingToParse);
        };
        let mut expect_semicolon = true;
        let return_value = match token_type {
            TT::Keyword if lexeme == "print" => {
                expect!(self, TT::Keyword, "print");
                let expr = self.expr()?;
                Ok(Statement::Print(expr))
            }
            TT::LeftBrace => {
                expect!(self, TT::LeftBrace);
                let current_index = self.current_address_index;
                let previous_address_prefix = self.current_address_prefix.clone();
                self.current_address_prefix = format!(
                    "{}{}.",
                    self.current_address_prefix, self.current_address_index
                );
                self.current_address_index = 0;
                let mut statements = vec![];
                while !next_token_matches!(self, TT::RightBrace | TT::Eof) {
                    statements.push(self.declaration()?);
                }
                expect!(self, TT::RightBrace);
                expect_semicolon = false;
                self.current_address_index = current_index + 1;
                self.current_address_prefix = previous_address_prefix;
                Ok(Statement::Block(statements))
            }
            TT::Keyword if lexeme == "if" || lexeme == "while" => {
                self.advance();
                expect!(self, TT::LeftParen);
                let condition = self.expr()?;
                expect!(self, TT::RightParen);
                let body = self.statement()?;
                expect_semicolon = false;
                if lexeme == "if" {
                    let else_body = if next_token_matches!(self, TT::Keyword, "else") {
                        expect!(self, TT::Keyword, "else");
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
            TT::Keyword if lexeme == "for" => {
                expect!(self, TT::Keyword, "for");
                expect!(self, TT::LeftParen);
                let mut equivalent_statements = vec![];
                if !next_token_matches!(self, TT::Semicolon) {
                    let initializer = self.declaration()?;
                    if !matches!(
                        initializer,
                        Declaration::Var(_, _) | Declaration::Statement(Statement::Expr(_)),
                    ) {
                        return self.error(UnexpectedToken {
                            expected: "variable declaration or expression".to_string(),
                        });
                    }
                    equivalent_statements.push(initializer);
                } else {
                    expect!(self, TT::Semicolon);
                }
                let condition = if !next_token_matches!(self, TT::Semicolon) {
                    Some(self.expr()?)
                } else {
                    None
                };
                expect!(self, TT::Semicolon);
                let increment = if !next_token_matches!(self, TT::RightParen) {
                    Some(self.expr()?)
                } else {
                    None
                };
                expect!(self, TT::RightParen);
                let mut parsed_body = self.statement()?;
                expect_semicolon = false;
                if let Some(increment) = increment {
                    parsed_body = Statement::Block(vec![
                        Declaration::Statement(parsed_body),
                        Declaration::Statement(Statement::Expr(increment)),
                    ]);
                }
                equivalent_statements.push(Declaration::Statement(Statement::While(
                    condition.unwrap_or(Expr::Literal(LiteralExpr::True)),
                    Box::new(parsed_body),
                )));
                Ok(Statement::Block(equivalent_statements))
            }
            TT::Keyword if lexeme == "return" => {
                let return_token = expect!(self, TT::Keyword, "return").clone();
                let return_expr = if !next_token_matches!(self, TT::Semicolon) {
                    Some(self.expr()?)
                } else {
                    None
                };
                Ok(Statement::Return(return_token, return_expr))
            }
            _ => {
                let expr = self.expr()?;
                Ok(Statement::Expr(expr))
            }
        };

        if expect_semicolon {
            expect!(self, TT::Semicolon);
        }
        return_value
    }

    pub fn var_declaration(&mut self) -> ParsingResult<Declaration> {
        expect!(self, TT::Keyword, "var");
        let Some(
            identifier_token @ Token {
                token_type: TT::Identifier,
                ..
            },
        ) = self.advance()
        else {
            return self.error(UnexpectedToken {
                expected: "identifier".to_string(),
            });
        };

        let identifier_token = identifier_token.clone();

        let expression = if next_token_matches!(self, TT::Equal) {
            self.advance();
            Some(self.expr()?)
        } else {
            None
        };

        expect!(self, TT::Semicolon);

        Ok(Declaration::Var(identifier_token, expression))
    }

    pub fn fun_or_method_declaration(&mut self, is_method: bool) -> ParsingResult<Declaration> {
        if !is_method {
            expect!(self, TT::Keyword, "fun");
        }
        let identifier_token = expect!(self, TT::Identifier).clone();

        let mut arguments = vec![];
        expect!(self, TT::LeftParen);
        if !next_token_matches!(self, TT::RightParen) {
            arguments.push(expect!(self, TT::Identifier).clone());
            while next_token_matches!(self, TT::Comma) {
                expect!(self, TT::Comma);
                arguments.push(expect!(self, TT::Identifier).clone());
            }
        }

        expect!(self, TT::RightParen);
        let body @ Statement::Block(_) = self.statement()? else {
            return self.error(UnexpectedToken {
                expected: "block".to_string(),
            });
        };

        Ok(Declaration::Function(identifier_token, arguments, body))
    }

    pub fn class_declaration(&mut self) -> ParsingResult<Declaration> {
        expect!(self, TT::Keyword, "class");
        let class_identifier_token = expect!(self, TT::Identifier).clone();
        let mut super_class = None;
        if next_token_matches!(self, TT::Less) {
            expect!(self, TT::Less);
            super_class = Some(self.primary()?);
            if let Some(Expr::Name(superclass_identifier_token, _)) = super_class.as_ref() {
                if superclass_identifier_token.lexeme == class_identifier_token.lexeme {
                    return self.error(CantInheritFromSelf);
                }
            } else {
                return self.error(UnexpectedToken {
                    expected: "identifier".into(),
                });
            }
        }
        expect!(self, TT::LeftBrace);
        let mut body = Vec::new();
        while !next_token_matches!(self, TT::RightBrace) {
            body.push(self.fun_or_method_declaration(true)?);
        }
        expect!(self, TT::RightBrace);

        Ok(Declaration::Class(
            class_identifier_token,
            super_class,
            Statement::Block(body),
        ))
    }

    pub fn declaration(&mut self) -> ParsingResult<Declaration> {
        if next_token_matches!(self, TT::Keyword, "var") {
            Ok(self.var_declaration()?)
        } else if next_token_matches!(self, TT::Keyword, "fun") {
            Ok(self.fun_or_method_declaration(false)?)
        } else if next_token_matches!(self, TT::Keyword, "class") {
            Ok(self.class_declaration()?)
        } else if self.peek().is_some() {
            Ok(Declaration::Statement(self.statement()?))
        } else {
            todo!("Error handling")
        }
    }

    pub fn parse_program(&mut self) -> ParsingResult<Program> {
        let mut declarations = Vec::new();
        if next_token_matches!(self, TT::Eof) {
            return self.error(NothingToParse);
        }
        while !next_token_matches!(self, TT::Eof) {
            declarations.push(self.declaration()?);
        }
        Ok(Statement::Block(declarations))
    }

    pub fn parse_expr(&mut self) -> ParsingResult<Expr> {
        let expr = self.expr()?;
        if !next_token_matches!(self, TT::Eof) {
            return if self.peek().is_some() {
                self.error(UnparsedTokens)
            } else {
                self.error(ParsedTooMuch)
            };
        }
        Ok(expr)
    }

    pub fn reset(&mut self) -> &mut Self {
        self.position = 0;
        self.current_token = self.tokens[0].clone();
        self.current_address_prefix = "".to_string();
        self.current_address_index = 0;
        self
    }
}
