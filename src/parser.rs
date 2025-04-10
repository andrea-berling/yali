use thiserror::Error;

use crate::lexer::{Literal, Token, TokenType};

pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

macro_rules! define_binary_expression_parsers {
    ($($expr_type:ident,$subexpr_type:ident,$operator_token_type:pat),*) => {
        $(
            fn $expr_type(&mut self) -> Result<Expr, ParsingError> {
                let mut $expr_type = self.$subexpr_type()?;

                while matches!(
                    self.peek(),
                    Some(Token {
                        token_type: $operator_token_type,
                        ..
                    })
                ) {
                    let operator = self.advance().unwrap().clone();
                    let $subexpr_type = self.$subexpr_type()?;
                    $expr_type = Expr::Binary(Box::new($expr_type), operator, Box::new($subexpr_type));
                }
                Ok($expr_type)
            }
        )*
    }
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

    fn primary(&mut self) -> Result<Expr, ParsingError> {
        let Some(Token {
            token_type,
            value,
            lexeme,
            line,
        }) = self.advance()
        else {
            return Err(ParsingError::new(0, ParsingErrorType::NothingToParse));
        };

        let line = *line;

        match (token_type, value) {
            (TokenType::Number, Some(l @ Literal::Number(_)))
            | (TokenType::String, Some(l @ Literal::String(_))) => {
                Ok(Expr::Literal(LiteralExpr::Lit(l.clone())))
            }
            (TokenType::Keyword, _) => {
                if lexeme == "true" {
                    Ok(Expr::Literal(LiteralExpr::True))
                } else if lexeme == "false" {
                    Ok(Expr::Literal(LiteralExpr::False))
                } else if lexeme == "nil" {
                    Ok(Expr::Literal(LiteralExpr::Nil))
                } else {
                    return Err(ParsingError::new(
                        line,
                        ParsingErrorType::UnexpectedToken(lexeme.to_string()),
                    ));
                }
            }
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
            _ => Err(ParsingError::new(
                line,
                ParsingErrorType::UnexpectedToken(lexeme.clone()),
            )),
        }
    }

    fn unary(&mut self) -> Result<Expr, ParsingError> {
        match self.peek() {
            Some(Token {
                token_type: TokenType::Bang | TokenType::Minus,
                ..
            }) => {
                let operator = self.advance().unwrap().clone();
                Ok(Expr::Unary(operator, Box::new(self.unary()?)))
            }
            _ => self.primary(),
        }
    }

    define_binary_expression_parsers! {
        equality,comparison,TokenType::BangEqual | TokenType::EqualEqual,
        comparison,term,TokenType::Greater | TokenType::GreaterEqual | TokenType::Less | TokenType::LessEqual,
        term,factor,TokenType::Minus | TokenType::Plus,
        factor,unary,TokenType::Slash | TokenType::Star
    }

    fn expr(&mut self) -> Result<Expr, ParsingError> {
        self.equality()
    }

    fn ast(&mut self) -> Result<Ast, ParsingError> {
        let expr = self.expr()?;
        match self.peek() {
            Some(Token {
                token_type, line, ..
            }) if !matches!(token_type, TokenType::Eof) => {
                Err(ParsingError::new(*line, ParsingErrorType::UnparsedTokens))
            }
            _ => Ok(Ast::Expr(expr)),
        }
    }

    pub fn parse(&mut self) -> Result<Ast, ParsingError> {
        self.ast()
    }
}

#[derive(Debug)]
pub enum Ast {
    Expr(Expr),
}

#[derive(Debug)]
pub enum Expr {
    Literal(LiteralExpr),
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
}

#[derive(Debug)]
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
    #[error("Token type mismatch")]
    TokenTypeMismatch,
    #[error("Error at '{0}': Expect expression")]
    UnexpectedToken(String),
    #[error("Unbalanced grouping")]
    UnbalancedGrouping,
    #[error("Invalid expression")]
    InvalidExpression,
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
        }
    }

    fn visit_expr_literal(&self, literal_expr: LiteralExpr) {
        if let LiteralExpr::Lit(literal) = literal_expr {
            self.visit_literal(literal);
        }
    }

    fn visit_literal(&self, literal: Literal) {}

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
        }
    }
}
