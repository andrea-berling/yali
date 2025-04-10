use std::{collections::VecDeque, iter::Peekable};

use crate::lexer::{Literal, Token, TokenType};

#[derive(Debug)]
pub enum AST {
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
pub enum ParsingError {
    UnparsedTokens,
    NothingToParse,
    TokenTypeMismatch,
    UnexpectedToken(String),
    UnbalancedGrouping,
    MalformedBinaryOperation,
    InvalidExpression,
}

pub trait Visit {
    fn visit_ast(&self, ast: AST) {
        match ast {
            AST::Expr(expr) => self.visit_expr(expr),
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

    fn visit_ast(&self, ast: AST) {
        match ast {
            AST::Expr(expr) => self.visit_expr(expr),
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
    tokens_iterator: &mut Peekable<impl Iterator<Item = &'a (usize, Token)>>,
    previous_operators_precedences_stack: &mut VecDeque<i8>,
) -> Result<Expr, (usize, ParsingError)> {
    let Some((current_line, Token(token_type, lexeme, value))) = tokens_iterator.next() else {
        return Err((0, ParsingError::NothingToParse));
    };

    let current_line = *current_line;

    let mut expr = match token_type {
        TokenType::STRING => {
            if let Some(l @ Literal::String(_)) = value {
                Ok(Expr::Literal(LiteralExpr::Lit(l.clone())))
            } else {
                Err((current_line, ParsingError::TokenTypeMismatch))
            }
        }
        TokenType::NUMBER => {
            if let Some(l @ Literal::Number(_)) = value {
                Ok(Expr::Literal(LiteralExpr::Lit(l.clone())))
            } else {
                Err((current_line, ParsingError::TokenTypeMismatch))
            }
        }

        TokenType::KEYWORD(keyword) => {
            if keyword == "true" {
                Ok(Expr::Literal(LiteralExpr::True))
            } else if keyword == "false" {
                Ok(Expr::Literal(LiteralExpr::False))
            } else if keyword == "nil" {
                Ok(Expr::Literal(LiteralExpr::Nil))
            } else {
                todo!()
            }
        }
        TokenType::LEFT_PAREN => {
            previous_operators_precedences_stack.push_front(i8::MIN);
            let expr = parse_expr(tokens_iterator, previous_operators_precedences_stack)?;
            if !tokens_iterator
                .next()
                .is_some_and(|(current_line, Token(token_type, _, _))| {
                    matches!(token_type, TokenType::RIGHT_PAREN)
                })
            {
                return Err((current_line, ParsingError::UnbalancedGrouping));
            }
            previous_operators_precedences_stack.pop_front();
            Ok(Expr::Grouping(Box::new(expr)))
        }
        TokenType::BANG => {
            previous_operators_precedences_stack.push_front(i8::MAX);
            let expr = parse_expr(tokens_iterator, previous_operators_precedences_stack)?;
            previous_operators_precedences_stack.pop_front();
            Ok(Expr::Unary(Unary::Neg(Box::new(expr))))
        }
        TokenType::MINUS => {
            previous_operators_precedences_stack.push_front(i8::MAX);
            let expr = parse_expr(tokens_iterator, previous_operators_precedences_stack)?;
            previous_operators_precedences_stack.pop_front();
            Ok(Expr::Unary(Unary::Minus(Box::new(expr))))
        }
        _ => Err((current_line, ParsingError::UnexpectedToken(lexeme.clone()))),
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
                |(line, err)| match err {
                    ParsingError::UnparsedTokens => todo!(),
                    ParsingError::NothingToParse => todo!(),
                    ParsingError::TokenTypeMismatch => todo!(),
                    ParsingError::UnexpectedToken(ref lexeme) => {
                        eprintln!("[line {line}] Error at '{lexeme}': Expect expression.");
                        Err((line, err))
                    }
                    ParsingError::UnbalancedGrouping => todo!(),
                    ParsingError::MalformedBinaryOperation => todo!(),
                    ParsingError::InvalidExpression => todo!(),
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
    tokens_iterator: &mut Peekable<impl Iterator<Item = &'a (usize, Token)>>,
) -> Result<BinaryOp, (usize, ParsingError)> {
    let Some((current_line, Token(token_type, _, _))) = tokens_iterator.next() else {
        return Err((0, ParsingError::NothingToParse));
    };
    match token_type {
        TokenType::MINUS => Ok(BinaryOp::MINUS),
        TokenType::PLUS => Ok(BinaryOp::PLUS),
        TokenType::STAR => Ok(BinaryOp::STAR),
        TokenType::SLASH => Ok(BinaryOp::SLASH),
        TokenType::GREATER => Ok(BinaryOp::GREATER),
        TokenType::LESS => Ok(BinaryOp::LESS),
        TokenType::GREATER_EQUAL => Ok(BinaryOp::GREATER_EQUAL),
        TokenType::LESS_EQUAL => Ok(BinaryOp::LESS_EQUAL),
        TokenType::EQUAL_EQUAL => Ok(BinaryOp::EQUAL_EQUAL),
        TokenType::BANG_EQUAL => Ok(BinaryOp::BANG_EQUAL),
        _ => Err((*current_line, ParsingError::InvalidExpression)),
    }
}

pub fn parse_ast(tokens: &[(usize, Token)]) -> Result<AST, ParsingError> {
    let mut tokens_iterator = tokens.iter().peekable();
    let expr = parse_expr(&mut tokens_iterator, &mut VecDeque::new()).map_err(|(_, e)| e)?;
    if !tokens_iterator
        .next()
        .is_some_and(|(_, Token(token_type, _, _))| matches!(token_type, TokenType::EOF))
    {
        return Err(ParsingError::UnparsedTokens);
    }
    Ok(AST::Expr(expr))
}
