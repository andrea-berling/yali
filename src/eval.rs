use std::fmt::{Display, Formatter};

use thiserror::Error;

use crate::lexer::TokenType;
use crate::parser::{Ast, Expr};

pub enum EvalResult {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
}

#[derive(Debug, Error)]
pub enum EvalError {
    #[error("Invalid operand")]
    IvalidOperand,
    #[error("Invalid operator")]
    IvalidOperator,
    #[error("Unexpected token")]
    UnexpectedToken,
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
                    Err(EvalError::IvalidOperand)
                }
            }
            TokenType::Bang => {
                if let EvalResult::Bool(b) = eval_expr(expr)? {
                    Ok(EvalResult::Bool(!b))
                } else {
                    Err(EvalError::IvalidOperand)
                }
            }
            _ => Err(EvalError::IvalidOperator),
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
                    _ => Err(EvalError::IvalidOperator),
                }
            }
            TokenType::Minus => {
                let left = eval_expr(expr)?;
                let right = eval_expr(expr1)?;
                if let (EvalResult::Number(l), EvalResult::Number(r)) = (left, right) {
                    Ok(EvalResult::Number(l - r))
                } else {
                    Err(EvalError::IvalidOperator)
                }
            }

            TokenType::Star => {
                let left = eval_expr(expr)?;
                let right = eval_expr(expr1)?;
                if let (EvalResult::Number(l), EvalResult::Number(r)) = (left, right) {
                    Ok(EvalResult::Number(l * r))
                } else {
                    Err(EvalError::IvalidOperator)
                }
            }
            TokenType::Slash => {
                let left = eval_expr(expr)?;
                let right = eval_expr(expr1)?;
                if let (EvalResult::Number(l), EvalResult::Number(r)) = (left, right) {
                    if r == 0.0 {
                        return Err(EvalError::IvalidOperator);
                    }
                    Ok(EvalResult::Number(l / r))
                } else {
                    Err(EvalError::IvalidOperator)
                }
            }
            TokenType::EqualEqual => {
                let left = eval_expr(expr)?;
                let right = eval_expr(expr1)?;
                match (left, right) {
                    (EvalResult::Number(l), EvalResult::Number(r)) => Ok(EvalResult::Bool(l == r)),
                    (EvalResult::String(l), EvalResult::String(r)) => Ok(EvalResult::Bool(l == r)),
                    (EvalResult::Bool(l), EvalResult::Bool(r)) => Ok(EvalResult::Bool(l == r)),
                    _ => Err(EvalError::IvalidOperator),
                }
            }
            TokenType::BangEqual => {
                let left = eval_expr(expr)?;
                let right = eval_expr(expr1)?;
                match (left, right) {
                    (EvalResult::Number(l), EvalResult::Number(r)) => Ok(EvalResult::Bool(l != r)),
                    (EvalResult::String(l), EvalResult::String(r)) => Ok(EvalResult::Bool(l != r)),
                    (EvalResult::Bool(l), EvalResult::Bool(r)) => Ok(EvalResult::Bool(l != r)),
                    _ => Err(EvalError::IvalidOperator),
                }
            }
            TokenType::Greater => {
                let left = eval_expr(expr)?;
                let right = eval_expr(expr1)?;
                if let (EvalResult::Number(l), EvalResult::Number(r)) = (left, right) {
                    Ok(EvalResult::Bool(l > r))
                } else {
                    Err(EvalError::IvalidOperator)
                }
            }
            TokenType::GreaterEqual => {
                let left = eval_expr(expr)?;
                let right = eval_expr(expr1)?;
                if let (EvalResult::Number(l), EvalResult::Number(r)) = (left, right) {
                    Ok(EvalResult::Bool(l >= r))
                } else {
                    Err(EvalError::IvalidOperator)
                }
            }
            TokenType::Less => {
                let left = eval_expr(expr)?;
                let right = eval_expr(expr1)?;
                if let (EvalResult::Number(l), EvalResult::Number(r)) = (left, right) {
                    Ok(EvalResult::Bool(l < r))
                } else {
                    Err(EvalError::IvalidOperator)
                }
            }
            TokenType::LessEqual => {
                let left = eval_expr(expr)?;
                let right = eval_expr(expr1)?;
                if let (EvalResult::Number(l), EvalResult::Number(r)) = (left, right) {
                    Ok(EvalResult::Bool(l <= r))
                } else {
                    Err(EvalError::IvalidOperator)
                }
            }
            _ => Err(EvalError::IvalidOperator),
        },
        Expr::Grouping(expr) => eval_expr(expr),
    }
}

pub fn eval_ast(ast: &Ast) -> Result<EvalResult, EvalError> {
    match ast {
        Ast::Expr(expr) => eval_expr(expr),
    }
}
