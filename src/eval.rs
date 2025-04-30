use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

use thiserror::Error;

use crate::interpreter::{Callable, Environment, Interpreter};
use crate::lexer::{Token, TokenType};
use crate::parser::{Ast, Expr, Statement};

#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
    Fn {
        name: String,
        formal_args: Vec<Token>,
        body: Statement,
        environment: Rc<RefCell<Environment>>,
        this: Option<Box<Value>>,
    },
    Class {
        name: String,
        methods: HashMap<String, Value>,
    },
    ClassInstance {
        class: Box<Value>,
        properties: HashMap<String, Value>,
    },
}

#[allow(dead_code)]
impl Value {
    pub fn get_this(&self) -> Option<Value> {
        if let Value::Fn { this, .. } = self {
            this.clone().as_deref().cloned()
        } else {
            None
        }
    }

    pub fn get_name(&self) -> Option<String> {
        match self {
            Value::Fn { name, .. } | Value::Class { name, .. } => Some(name.clone()),
            _ => None,
        }
    }

    pub fn get_properties(&self) -> Option<&HashMap<String, Value>> {
        if let Value::ClassInstance { properties, .. } = self {
            Some(properties)
        } else {
            None
        }
    }

    pub fn get_type(&self) -> String {
        match self {
            Value::Number(_) => "Number",
            Value::String(_) => "String",
            Value::Bool(_) => "Bool",
            Value::Nil => "Nil",
            Value::Fn { .. } => "Fn",
            Value::Class { .. } => "Class",
            Value::ClassInstance { .. } => "ClassInstance",
        }
        .to_string()
    }
}

#[derive(Debug, Error)]
pub enum EvalErrorType {
    #[error("Invalid operator")]
    IvalidOperator,
    #[error("Operand must be a number")]
    OperandMustBeANumber,
    #[error("Operands must be numbers")]
    OperandsMustBeNumbers,
    #[error("Operands must be two numbers or two strings")]
    OperandsMustBeTwoNumbersOrTwoStrings,
    #[error("Undefined variable")]
    UndefinedVariable,
    #[error("Undefined variable or function")]
    UndefinedVariableOrFunction,
    #[error("Undefined primitive function")]
    UndefinedPrimitiveFunction,
    #[error("Undefined function")]
    UndefinedFunction,
    #[error("Wrong argument number: expected {0}, got {1}")]
    WrongArgumentNumber(usize, usize),
    #[error("Undefined field")]
    UndefinedField,
    #[error("Undefined method")]
    UndefinedMethod,
    #[error("Invalid reference to this")]
    InvalidReferenceToThis,
}

use EvalErrorType::*;

#[derive(Error, Debug)]
#[error("Error at {}: {error}.\n[line {}]",if !token.lexeme.is_empty() {format!("'{}'",&token.lexeme)} else {"end".to_string()}, token.line)]
pub struct EvalError {
    token: Token,
    error: EvalErrorType,
}

impl EvalError {
    pub fn set_token(&mut self, token: &Token) {
        self.token = token.clone()
    }
}

pub fn eval_error<T>(token: &Token, error: EvalErrorType) -> Result<T, EvalError> {
    Err(EvalError {
        token: token.clone(),
        error,
    })
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
            Value::Fn { name, .. } => write!(f, "<fn {name}>"),
            Value::Class { name, .. } => write!(f, "{name}"),
            Value::ClassInstance { class, .. } => write!(f, "{class} instance"),
        }
    }
}

pub fn eval_expr(expr: &Expr, interpreter: &mut Interpreter) -> Result<Value, EvalError> {
    match expr {
        Expr::Literal(literal_expr) => match literal_expr {
            crate::parser::LiteralExpr::Lit(literal) => match literal {
                crate::lexer::Literal::Number(n) => Ok(Value::Number(*n)),
                crate::lexer::Literal::String(s) => Ok(Value::String(s.clone())),
            },
            crate::parser::LiteralExpr::True => Ok(Value::Bool(true)),
            crate::parser::LiteralExpr::False => Ok(Value::Bool(false)),
            crate::parser::LiteralExpr::Nil => Ok(Value::Nil),
        },
        Expr::Unary(token, expr) => match token.token_type {
            TokenType::Minus => {
                if let Value::Number(n) = eval_expr(expr, interpreter)? {
                    Ok(Value::Number(-n))
                } else {
                    eval_error(token, OperandMustBeANumber)
                }
            }
            TokenType::Bang => match eval_expr(expr, interpreter)? {
                Value::Bool(false) | Value::Nil => Ok(Value::Bool(true)),
                _ => Ok(Value::Bool(false)),
            },
            _ => eval_error(token, IvalidOperator),
        },
        Expr::Binary(expr, token, expr1) => match &token.token_type {
            TokenType::Plus => {
                let left = eval_expr(expr, interpreter)?;
                let right = eval_expr(expr1, interpreter)?;
                match (left, right) {
                    (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
                    (Value::String(l), Value::String(r)) => {
                        Ok(Value::String(format!("{}{}", l, r)))
                    }
                    _ => eval_error(token, OperandsMustBeTwoNumbersOrTwoStrings),
                }
            }
            TokenType::Minus => {
                let left = eval_expr(expr, interpreter)?;
                let right = eval_expr(expr1, interpreter)?;
                if let (Value::Number(l), Value::Number(r)) = (left, right) {
                    Ok(Value::Number(l - r))
                } else {
                    eval_error(token, OperandsMustBeNumbers)
                }
            }

            TokenType::Star => {
                let left = eval_expr(expr, interpreter)?;
                let right = eval_expr(expr1, interpreter)?;
                if let (Value::Number(l), Value::Number(r)) = (left, right) {
                    Ok(Value::Number(l * r))
                } else {
                    eval_error(token, OperandsMustBeNumbers)
                }
            }
            TokenType::Slash => {
                let left = eval_expr(expr, interpreter)?;
                let right = eval_expr(expr1, interpreter)?;
                if let (Value::Number(l), Value::Number(r)) = (left, right) {
                    if r == 0.0 {
                        return eval_error(token, IvalidOperator);
                    }
                    Ok(Value::Number(l / r))
                } else {
                    eval_error(token, OperandsMustBeNumbers)
                }
            }
            TokenType::EqualEqual => {
                let left = eval_expr(expr, interpreter)?;
                let right = eval_expr(expr1, interpreter)?;
                match (left, right) {
                    (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l == r)),
                    (Value::String(l), Value::String(r)) => Ok(Value::Bool(l == r)),
                    (Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l == r)),
                    _ => Ok(Value::Bool(false)),
                }
            }
            TokenType::BangEqual => {
                let left = eval_expr(expr, interpreter)?;
                let right = eval_expr(expr1, interpreter)?;
                match (left, right) {
                    (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l != r)),
                    (Value::String(l), Value::String(r)) => Ok(Value::Bool(l != r)),
                    (Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l != r)),
                    _ => Ok(Value::Bool(true)),
                }
            }
            TokenType::Greater => {
                let left = eval_expr(expr, interpreter)?;
                let right = eval_expr(expr1, interpreter)?;
                if let (Value::Number(l), Value::Number(r)) = (left, right) {
                    Ok(Value::Bool(l > r))
                } else {
                    eval_error(token, OperandsMustBeNumbers)
                }
            }
            TokenType::GreaterEqual => {
                let left = eval_expr(expr, interpreter)?;
                let right = eval_expr(expr1, interpreter)?;
                if let (Value::Number(l), Value::Number(r)) = (left, right) {
                    Ok(Value::Bool(l >= r))
                } else {
                    eval_error(token, OperandsMustBeNumbers)
                }
            }
            TokenType::Less => {
                let left = eval_expr(expr, interpreter)?;
                let right = eval_expr(expr1, interpreter)?;
                if let (Value::Number(l), Value::Number(r)) = (left, right) {
                    Ok(Value::Bool(l < r))
                } else {
                    eval_error(token, OperandsMustBeNumbers)
                }
            }
            TokenType::LessEqual => {
                let left = eval_expr(expr, interpreter)?;
                let right = eval_expr(expr1, interpreter)?;
                if let (Value::Number(l), Value::Number(r)) = (left, right) {
                    Ok(Value::Bool(l <= r))
                } else {
                    eval_error(token, OperandsMustBeNumbers)
                }
            }
            _ => eval_error(token, IvalidOperator),
        },
        Expr::Grouping(expr) => eval_expr(expr, interpreter),
        Expr::Name(token, address) => {
            if let Some(value) = interpreter.get_var_by_address(token, address) {
                Ok(value.clone())
            } else {
                eval_error(token, UndefinedVariableOrFunction)
            }
        }
        Expr::Assign(lhs, expr) => match lhs.as_ref() {
            Expr::Name(ref token, ref address) => {
                let result = eval_expr(expr, interpreter)?;
                if !interpreter.assign_var(token, address, result.clone()) {
                    return eval_error(token, UndefinedVariable);
                }
                Ok(result)
            }
            Expr::Dotted(ref _token, ref head, ref tail) => {
                let mut instance @ Value::ClassInstance { .. } = eval_expr(head, interpreter)?
                else {
                    todo!();
                };
                let Expr::Name(left_token, left_address) = head.as_ref() else {
                    todo!();
                };

                let Expr::Name(right_token, _) = tail.as_ref() else {
                    todo!();
                };

                let rhs = eval_expr(expr, interpreter)?;
                if let Value::ClassInstance {
                    ref mut properties, ..
                } = &mut instance
                {
                    properties.insert(right_token.lexeme.clone(), rhs.clone());
                } else {
                    todo!()
                }
                if !interpreter.assign_var(left_token, left_address, instance.clone()) {
                    return eval_error(left_token, UndefinedVariable);
                }
                Ok(rhs.clone())
            }
            _ => todo!("Invalid assingee"),
        },
        Expr::Logical(expr1, operator, expr2) => {
            let result1 = eval_expr(expr1, interpreter)?;
            match (result1.clone(), operator.lexeme.as_str()) {
                (Value::Nil | Value::Bool(false), "or") => Ok(eval_expr(expr2, interpreter)?),
                (_, "or") => Ok(result1),
                (Value::Nil | Value::Bool(false), "and") => Ok(result1),
                (_, "and") => Ok(eval_expr(expr2, interpreter)?),
                _ => {
                    todo!("Shouldn't happen");
                }
            }
        }
        Expr::Call(callee, token, args) => {
            let mut call_args = vec![];
            for expr in args {
                call_args.push((expr.clone(), eval_expr(expr, interpreter)?))
            }
            match eval_expr(callee, interpreter)? {
                Value::Fn { name, .. } | Value::Class { name, .. } if is_primitive(&name) => {
                    eval_primitive_function(&name, token)
                }
                Value::Fn { .. } => {
                    interpreter.call(&Callable::Expr(*callee.clone()), call_args, None, token)
                }
                val @ Value::Class { .. } => {
                    interpreter.call(&Callable::Class(val), call_args, None, token)
                }
                _ => eval_error(token, UndefinedFunction),
            }
        }
        Expr::Dotted(_, ref head, ref tail) => eval_expr_in_class_instance(tail, head, interpreter),
        Expr::This(token) => interpreter
            .get_this()
            .map_or(eval_error(token, InvalidReferenceToThis), Result::Ok),
    }
}

fn eval_expr_in_class_instance(
    tail: &Expr,
    head: &Expr,
    interpreter: &mut Interpreter,
) -> Result<Value, EvalError> {
    let instance @ Value::ClassInstance { class, properties } = &eval_expr(head, interpreter)?
    else {
        todo!("Can't be");
    };
    let Value::Class { methods, .. } = class.as_ref() else {
        todo!()
    };
    match tail {
        Expr::Literal(_)
        | Expr::Unary(_, _)
        | Expr::Binary(_, _, _)
        | Expr::Logical(_, _, _)
        | Expr::Assign(_, _) => todo!("Can't be"),
        Expr::Grouping(expr) => eval_expr_in_class_instance(expr, head, interpreter),
        Expr::Name(token, _) => properties
            .get(&token.lexeme)
            .cloned()
            .or(
                if let Some(mut method) = methods.get(&token.lexeme).cloned() {
                    if let Value::Fn { this, .. } = &mut method {
                        *this = Some(Box::new(instance.clone()));
                        Some(method)
                    } else {
                        None
                    }
                } else {
                    None
                },
            )
            .map_or_else(|| eval_error(token, UndefinedField), Result::Ok),
        Expr::Call(callee, token, args_exprs) => {
            let callable = match callee.as_ref() {
                Expr::Name(callee, _) => {
                    let Some(mut method) = methods.get(&callee.lexeme).cloned() else {
                        return eval_error(callee, UndefinedMethod);
                    };
                    if let Value::Fn { this, .. } = &mut method {
                        *this = Some(Box::new(instance.clone()));
                    }
                    if let Some(overridden_method @ Value::Fn { .. }) =
                        properties.get(&callee.lexeme)
                    {
                        method = overridden_method.clone();
                    }
                    Callable::Method(method)
                }
                Expr::Call(sub_callee, sub_token, sub_args_exprs) => {
                    let mut call_args = vec![];
                    for expr in sub_args_exprs {
                        call_args.push((expr.clone(), eval_expr(expr, interpreter)?))
                    }
                    let method = &Callable::Method(eval_expr_in_class_instance(
                        sub_callee,
                        head,
                        interpreter,
                    )?);
                    Callable::Method(interpreter.call(
                        method,
                        call_args,
                        Some(instance.clone()),
                        sub_token,
                    )?)
                }
                _ => {
                    todo!()
                }
            };
            let mut call_args = vec![];
            for expr in args_exprs {
                call_args.push((expr.clone(), eval_expr(expr, interpreter)?))
            }
            interpreter.call(&callable, call_args, Some(instance.clone()), token)
        }
        Expr::Dotted(_, _, _) => {
            todo!()
        }
        Expr::This(_) => todo!(),
    }
}

fn eval_primitive_function(callee: &str, token: &Token) -> Result<Value, EvalError> {
    match callee {
        "clock" => Ok(Value::Number(
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_secs() as f64,
        )),
        _ => eval_error(token, UndefinedPrimitiveFunction),
    }
}

fn is_primitive(callee: &str) -> bool {
    match callee {
        "clock" => true,
        "" => false, // rids us of warnings
        _ => false,
    }
}

pub fn eval_ast(ast: &Ast) -> Result<Value, EvalError> {
    match ast {
        Ast::Expr(expr) => eval_expr(
            expr,
            &mut Interpreter::new(&Statement::Block(vec![]), HashMap::new()),
        ),
    }
}
