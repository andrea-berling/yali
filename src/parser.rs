use std::{collections::VecDeque, iter::Peekable};

use crate::lexer::{Literal, SingleCharTokenType, Token, TokenType, TwoCharsTokenType};

#[derive(Debug)]
pub enum Ast {
    Expr(Expr),
}

#[derive(Debug)]
enum Expr {
    Literal(LiteralExpr),
    Unary(Unary),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Grouping(Box<Expr>),
}

#[derive(Debug)]
pub enum LiteralExpr {
    Lit(Literal),
    True,
    False,
    Nil,
}

#[derive(Debug)]
pub enum Unary {
    Minus(Box<Expr>),
    Neg(Box<Expr>),
}

#[derive(Debug, Clone, Copy)]
#[allow(non_camel_case_types)]
pub enum BinaryOp {
    EQUAL_EQUAL,
    BANG_EQUAL,
    LESS,
    LESS_EQUAL,
    GREATER,
    GREATER_EQUAL,
    PLUS,
    MINUS,
    STAR,
    SLASH,
}

trait HasPrecedence {
    fn precedence_level(&self) -> i8;
}

impl HasPrecedence for BinaryOp {
    fn precedence_level(&self) -> i8 {
        match self {
            BinaryOp::LESS
            | BinaryOp::GREATER
            | BinaryOp::LESS_EQUAL
            | BinaryOp::GREATER_EQUAL
            | BinaryOp::EQUAL_EQUAL
            | BinaryOp::BANG_EQUAL => 2,
            BinaryOp::STAR | BinaryOp::SLASH => 1,
            BinaryOp::MINUS | BinaryOp::PLUS => 0,
        }
    }
}

impl HasPrecedence for Unary {
    fn precedence_level(&self) -> i8 {
        i8::MAX
    }
}

#[derive(Debug)]
pub enum ParsingErrorType {
    UnparsedTokens,
    NothingToParse,
    TokenTypeMismatch,
    UnexpectedToken(String),
    UnbalancedGrouping,
    MalformedBinaryOperation,
    InvalidExpression,
}

#[derive(Debug)]
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
            Expr::Unary(unary) => self.visit_unary(unary),
            Expr::Binary(expr, binary_op, expr1) => {
                self.visit_expr(*expr);
                self.visit_binary_op(binary_op);
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

    fn visit_unary(&self, unary: Unary) {
        match unary {
            Unary::Minus(expr) => self.visit_expr(*expr),
            Unary::Neg(expr) => self.visit_expr(*expr),
        }
    }

    fn visit_binary_op(&self, binary_op: BinaryOp) {}

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

    fn visit_unary(&self, unary: Unary) {
        print!("(");
        let expr = match unary {
            Unary::Minus(expr) => {
                print!("-");
                expr
            }
            Unary::Neg(expr) => {
                print!("!");
                expr
            }
        };
        print!(" ");
        self.visit_expr(*expr);
        print!(")");
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
            Expr::Unary(unary) => self.visit_unary(unary),
            Expr::Binary(expr, binary_op, expr1) => {
                print!("(");
                match binary_op {
                    BinaryOp::EQUAL_EQUAL => print!("=="),
                    BinaryOp::BANG_EQUAL => print!("!="),
                    BinaryOp::LESS => print!("<"),
                    BinaryOp::LESS_EQUAL => print!("<="),
                    BinaryOp::GREATER => print!(">"),
                    BinaryOp::GREATER_EQUAL => print!(">="),
                    BinaryOp::PLUS => print!("+"),
                    BinaryOp::MINUS => print!("-"),
                    BinaryOp::STAR => print!("*"),
                    BinaryOp::SLASH => print!("/"),
                }
                print!(" ");
                self.visit_expr(*expr);
                print!(" ");
                self.visit_expr(*expr1);
                print!(")");
            }
            Expr::Grouping(expr) => self.visit_grouping(*expr),
        }
    }
}

fn parse_expr<'a>(
    tokens_iterator: &mut Peekable<impl Iterator<Item = &'a Token>>,
    previous_operators_precedences_stack: &mut VecDeque<i8>,
) -> Result<Expr, ParsingError> {
    let Some(Token {
        token_type,
        lexeme,
        value,
        line,
    }) = tokens_iterator.next()
    else {
        return Err(ParsingError::new(0, ParsingErrorType::NothingToParse));
    };

    let line = *line;

    let mut expr = match token_type {
        TokenType::String => {
            if let Some(l @ Literal::String(_)) = value {
                Ok(Expr::Literal(LiteralExpr::Lit(l.clone())))
            } else {
                Err(ParsingError::new(line, ParsingErrorType::TokenTypeMismatch))
            }
        }
        TokenType::Number => {
            if let Some(l @ Literal::Number(_)) = value {
                Ok(Expr::Literal(LiteralExpr::Lit(l.clone())))
            } else {
                Err(ParsingError::new(line, ParsingErrorType::TokenTypeMismatch))
            }
        }

        TokenType::Keyword => {
            if lexeme == "true" {
                Ok(Expr::Literal(LiteralExpr::True))
            } else if lexeme == "false" {
                Ok(Expr::Literal(LiteralExpr::False))
            } else if lexeme == "nil" {
                Ok(Expr::Literal(LiteralExpr::Nil))
            } else {
                todo!()
            }
        }
        TokenType::SingleChar(token_type) => match token_type {
            SingleCharTokenType::LeftParen => {
                previous_operators_precedences_stack.push_front(i8::MIN);
                let expr = parse_expr(tokens_iterator, previous_operators_precedences_stack)?;
                if !tokens_iterator
                    .next()
                    .is_some_and(|Token { token_type, .. }| {
                        if let TokenType::SingleChar(t) = token_type {
                            matches!(t, SingleCharTokenType::RightParen)
                        } else {
                            false
                        }
                    })
                {
                    return Err(ParsingError::new(
                        line,
                        ParsingErrorType::UnbalancedGrouping,
                    ));
                }
                previous_operators_precedences_stack.pop_front();
                Ok(Expr::Grouping(Box::new(expr)))
            }
            SingleCharTokenType::Bang => {
                previous_operators_precedences_stack.push_front(i8::MAX);
                let expr = parse_expr(tokens_iterator, previous_operators_precedences_stack)?;
                previous_operators_precedences_stack.pop_front();
                Ok(Expr::Unary(Unary::Neg(Box::new(expr))))
            }
            SingleCharTokenType::Minus => {
                previous_operators_precedences_stack.push_front(i8::MAX);
                let expr = parse_expr(tokens_iterator, previous_operators_precedences_stack)?;
                previous_operators_precedences_stack.pop_front();
                Ok(Expr::Unary(Unary::Minus(Box::new(expr))))
            }
            _ => Err(ParsingError::new(
                line,
                ParsingErrorType::UnexpectedToken(lexeme.clone()),
            )),
        },

        _ => Err(ParsingError::new(
            line,
            ParsingErrorType::UnexpectedToken(lexeme.clone()),
        )),
    }?;

    while let Some(Ok(next_binary_operator)) = tokens_iterator
        .peek()
        .map(|&t| parse_binary_operator(&mut std::iter::once(&t.clone()).peekable()))
    {
        if previous_operators_precedences_stack.is_empty()
            || previous_operators_precedences_stack
                .front()
                .is_some_and(|previous_precedence| {
                    next_binary_operator.precedence_level() > *previous_precedence
                })
        {
            let binary_operator = parse_binary_operator(tokens_iterator)?;

            previous_operators_precedences_stack.push_front(binary_operator.precedence_level());
            let expr2 = parse_expr(tokens_iterator, previous_operators_precedences_stack).or_else(
                |ParsingError { error: err, .. }| match err {
                    ParsingErrorType::UnparsedTokens => todo!(),
                    ParsingErrorType::NothingToParse => todo!(),
                    ParsingErrorType::TokenTypeMismatch => todo!(),
                    ParsingErrorType::UnexpectedToken(ref lexeme) => {
                        eprintln!("[line {line}] Error at '{lexeme}': Expect expression.");
                        Err(ParsingError::new(line, err))
                    }
                    ParsingErrorType::UnbalancedGrouping => todo!(),
                    ParsingErrorType::MalformedBinaryOperation => todo!(),
                    ParsingErrorType::InvalidExpression => todo!(),
                },
            )?;

            previous_operators_precedences_stack.pop_front();
            expr = Expr::Binary(Box::new(expr), binary_operator, Box::new(expr2));
        } else {
            break;
        }
    }
    Ok(expr)
}

fn parse_binary_operator<'a>(
    tokens_iterator: &mut Peekable<impl Iterator<Item = &'a Token>>,
) -> Result<BinaryOp, ParsingError> {
    let Some(Token {
        token_type, line, ..
    }) = tokens_iterator.next()
    else {
        return Err(ParsingError::new(0, ParsingErrorType::NothingToParse));
    };
    match token_type {
        TokenType::SingleChar(token_type) => match token_type {
            SingleCharTokenType::Minus => Ok(BinaryOp::MINUS),
            SingleCharTokenType::Plus => Ok(BinaryOp::PLUS),
            SingleCharTokenType::Star => Ok(BinaryOp::STAR),
            SingleCharTokenType::Slash => Ok(BinaryOp::SLASH),
            SingleCharTokenType::Greater => Ok(BinaryOp::GREATER),
            SingleCharTokenType::Less => Ok(BinaryOp::LESS),
            _ => Err(ParsingError::new(
                *line,
                ParsingErrorType::InvalidExpression,
            )),
        },

        TokenType::TwoChars(token_type) => match token_type {
            TwoCharsTokenType::GreaterEqual => Ok(BinaryOp::GREATER_EQUAL),
            TwoCharsTokenType::LessEqual => Ok(BinaryOp::LESS_EQUAL),
            TwoCharsTokenType::EqualEqual => Ok(BinaryOp::EQUAL_EQUAL),
            TwoCharsTokenType::BangEqual => Ok(BinaryOp::BANG_EQUAL),
        },
        _ => Err(ParsingError::new(
            *line,
            ParsingErrorType::InvalidExpression,
        )),
    }
}

pub fn parse_ast(tokens: &[Token]) -> Result<Ast, ParsingError> {
    let mut tokens_iterator = tokens.iter().peekable();
    let expr = parse_expr(&mut tokens_iterator, &mut VecDeque::new())?;
    match tokens_iterator.next() {
        Some(Token {
            token_type, line, ..
        }) if !matches!(token_type, TokenType::Eof) => {
            Err(ParsingError::new(*line, ParsingErrorType::UnparsedTokens))
        }
        _ => Ok(Ast::Expr(expr)),
    }
}
