use std::fmt::{Display, Formatter};

use thiserror::Error;

use crate::lexer::TokenType;
use crate::parser::{Ast, Expr, Program, Statement};

pub enum EvalResult {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
}

#[derive(Debug, Error)]
pub enum EvalErrorType {
    #[error("Invalid operand")]
    IvalidOperand,
    #[error("Invalid operator")]
    IvalidOperator,
    #[error("Unexpected token")]
    UnexpectedToken,
    #[error("Operand must be a number.")]
    OperandMustBeANumber,
    #[error("Operands must be numbers.")]
    OperandsMustBeNumbers,
    #[error("Operands must be two numbers or two strings.")]
    OperandsMustBeTwoNumbersOrTwoStrings,
}

#[derive(Debug)]
pub struct EvalError {
    pub line: usize,
    pub error: EvalErrorType,
}

impl EvalError {
    pub fn new(line: usize, error: EvalErrorType) -> Self {
        Self { line, error }
    }
}

impl Display for EvalResult {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            EvalResult::Number(n) => write!(f, "{}", n),
            EvalResult::String(s) => write!(f, "{}", s),
            EvalResult::Bool(b) => write!(f, "{}", b),
            EvalResult::Nil => write!(f, "nil"),
        }
    }
}

pub fn eval_expr(expr: &Expr) -> Result<EvalResult, EvalError> {
    match expr {
        Expr::Literal(literal_expr) => match literal_expr {
            crate::parser::LiteralExpr::Lit(literal) => match literal {
                crate::lexer::Literal::Number(n) => Ok(EvalResult::Number(*n)),
                crate::lexer::Literal::String(s) => Ok(EvalResult::String(s.clone())),
            },
            crate::parser::LiteralExpr::True => Ok(EvalResult::Bool(true)),
            crate::parser::LiteralExpr::False => Ok(EvalResult::Bool(false)),
            crate::parser::LiteralExpr::Nil => Ok(EvalResult::Nil),
        },
        Expr::Unary(token, expr) => match token.token_type {
            TokenType::Minus => {
                if let EvalResult::Number(n) = eval_expr(expr)? {
                    Ok(EvalResult::Number(-n))
                } else {
                    Err(EvalError::new(
                        token.line,
                        EvalErrorType::OperandMustBeANumber,
                    ))
                }
            }
            TokenType::Bang => match eval_expr(expr)? {
                EvalResult::Bool(false) | EvalResult::Nil => Ok(EvalResult::Bool(true)),
                _ => Ok(EvalResult::Bool(false)),
            },
            _ => Err(EvalError::new(token.line, EvalErrorType::IvalidOperator)),
        },
        Expr::Binary(expr, token, expr1) => match &token.token_type {
            TokenType::Plus => {
                let left = eval_expr(expr)?;
                let right = eval_expr(expr1)?;
                match (left, right) {
                    (EvalResult::Number(l), EvalResult::Number(r)) => Ok(EvalResult::Number(l + r)),
                    (EvalResult::String(l), EvalResult::String(r)) => {
                        Ok(EvalResult::String(format!("{}{}", l, r)))
                    }
                    _ => Err(EvalError::new(
                        token.line,
                        EvalErrorType::OperandsMustBeTwoNumbersOrTwoStrings,
                    )),
                }
            }
            TokenType::Minus => {
                let left = eval_expr(expr)?;
                let right = eval_expr(expr1)?;
                if let (EvalResult::Number(l), EvalResult::Number(r)) = (left, right) {
                    Ok(EvalResult::Number(l - r))
                } else {
                    Err(EvalError::new(
                        token.line,
                        EvalErrorType::OperandsMustBeTwoNumbersOrTwoStrings,
                    ))
                }
            }

            TokenType::Star => {
                let left = eval_expr(expr)?;
                let right = eval_expr(expr1)?;
                if let (EvalResult::Number(l), EvalResult::Number(r)) = (left, right) {
                    Ok(EvalResult::Number(l * r))
                } else {
                    Err(EvalError::new(
                        token.line,
                        EvalErrorType::OperandsMustBeNumbers,
                    ))
                }
            }
            TokenType::Slash => {
                let left = eval_expr(expr)?;
                let right = eval_expr(expr1)?;
                if let (EvalResult::Number(l), EvalResult::Number(r)) = (left, right) {
                    if r == 0.0 {
                        return Err(EvalError::new(token.line, EvalErrorType::IvalidOperator));
                    }
                    Ok(EvalResult::Number(l / r))
                } else {
                    Err(EvalError::new(
                        token.line,
                        EvalErrorType::OperandsMustBeNumbers,
                    ))
                }
            }
            TokenType::EqualEqual => {
                let left = eval_expr(expr)?;
                let right = eval_expr(expr1)?;
                match (left, right) {
                    (EvalResult::Number(l), EvalResult::Number(r)) => Ok(EvalResult::Bool(l == r)),
                    (EvalResult::String(l), EvalResult::String(r)) => Ok(EvalResult::Bool(l == r)),
                    (EvalResult::Bool(l), EvalResult::Bool(r)) => Ok(EvalResult::Bool(l == r)),
                    _ => Ok(EvalResult::Bool(false)),
                }
            }
            TokenType::BangEqual => {
                let left = eval_expr(expr)?;
                let right = eval_expr(expr1)?;
                match (left, right) {
                    (EvalResult::Number(l), EvalResult::Number(r)) => Ok(EvalResult::Bool(l != r)),
                    (EvalResult::String(l), EvalResult::String(r)) => Ok(EvalResult::Bool(l != r)),
                    (EvalResult::Bool(l), EvalResult::Bool(r)) => Ok(EvalResult::Bool(l != r)),
                    _ => Ok(EvalResult::Bool(true)),
                }
            }
            TokenType::Greater => {
                let left = eval_expr(expr)?;
                let right = eval_expr(expr1)?;
                if let (EvalResult::Number(l), EvalResult::Number(r)) = (left, right) {
                    Ok(EvalResult::Bool(l > r))
                } else {
                    Err(EvalError::new(
                        token.line,
                        EvalErrorType::OperandsMustBeNumbers,
                    ))
                }
            }
            TokenType::GreaterEqual => {
                let left = eval_expr(expr)?;
                let right = eval_expr(expr1)?;
                if let (EvalResult::Number(l), EvalResult::Number(r)) = (left, right) {
                    Ok(EvalResult::Bool(l >= r))
                } else {
                    Err(EvalError::new(
                        token.line,
                        EvalErrorType::OperandsMustBeNumbers,
                    ))
                }
            }
            TokenType::Less => {
                let left = eval_expr(expr)?;
                let right = eval_expr(expr1)?;
                if let (EvalResult::Number(l), EvalResult::Number(r)) = (left, right) {
                    Ok(EvalResult::Bool(l < r))
                } else {
                    Err(EvalError::new(
                        token.line,
                        EvalErrorType::OperandsMustBeNumbers,
                    ))
                }
            }
            TokenType::LessEqual => {
                let left = eval_expr(expr)?;
                let right = eval_expr(expr1)?;
                if let (EvalResult::Number(l), EvalResult::Number(r)) = (left, right) {
                    Ok(EvalResult::Bool(l <= r))
                } else {
                    Err(EvalError::new(
                        token.line,
                        EvalErrorType::OperandsMustBeNumbers,
                    ))
                }
            }
            _ => Err(EvalError::new(token.line, EvalErrorType::IvalidOperator)),
        },
        Expr::Grouping(expr) => eval_expr(expr),
    }
}

pub fn eval_ast(ast: &Ast) -> Result<EvalResult, EvalError> {
    match ast {
        Ast::Expr(expr) => eval_expr(expr),
    }
}
