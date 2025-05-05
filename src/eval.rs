use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

use thiserror::Error;

use crate::interpreter::{Callable, Environment, Interpreter};
use crate::lexer::{Token, TokenType};
use crate::parser::{Ast, Expr, Statement};

#[derive(Clone)]
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
        // TODO: just put it into the environment
        this: Option<Rc<RefCell<Value>>>,
    },
    Class {
        name: String,
        methods: HashMap<String, Rc<RefCell<Value>>>,
        superclass: Option<Rc<RefCell<Value>>>,
    },
    ClassInstance {
        class: Rc<RefCell<Value>>,
        properties: HashMap<String, Rc<RefCell<Value>>>,
    },
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Value::")?;
        match self {
            Value::Number(n) => f.debug_tuple("Number").field(n).finish(),
            Value::String(s) => f.debug_tuple("String").field(s).finish(),
            Value::Bool(b) => f.debug_tuple("Bool").field(b).finish(),
            Value::Nil => f.debug_tuple("Nil").finish(),
            Value::Fn {
                ref name,
                ref formal_args,
                ref environment,
                ref this,
                ..
            } => f
                .debug_struct("Fn")
                .field("name", name)
                .field("format_args", formal_args)
                .field("environment", environment)
                .field("this", this)
                .finish(),
            Value::Class {
                name,
                methods,
                superclass,
            } => f
                .debug_struct("Class")
                .field("name", name)
                .field("methods", &methods.keys())
                .field("superclass", superclass)
                .finish(),
            Value::ClassInstance { class, properties } => f
                .debug_struct("ClassInstance")
                .field("properties", properties)
                .field("class", class)
                .finish(),
        }
    }
}

impl From<Value> for Rc<RefCell<Value>> {
    fn from(val: Value) -> Self {
        Rc::new(RefCell::new(val))
    }
}

impl Value {
    pub fn get_this(&self) -> Option<Rc<RefCell<Value>>> {
        if let Value::Fn { this, .. } = self {
            this.clone()
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

    pub fn get_properties(&self) -> Option<&HashMap<String, Rc<RefCell<Value>>>> {
        if let Value::ClassInstance { properties, .. } = self {
            Some(properties)
        } else {
            None
        }
    }

    pub fn get_properties_mut(&mut self) -> Option<&mut HashMap<String, Rc<RefCell<Value>>>> {
        if let Value::ClassInstance { properties, .. } = self {
            Some(properties)
        } else {
            None
        }
    }

    pub fn get_class(&self) -> Option<Rc<RefCell<Value>>> {
        if let Value::ClassInstance { class, .. } = self {
            Some(class.clone())
        } else {
            None
        }
    }

    pub fn get_methods(&self) -> Option<&HashMap<String, Rc<RefCell<Value>>>> {
        if let Value::Class { methods, .. } = self {
            Some(methods)
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
    #[error("Undefined reference")]
    UndefinedReference,
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
    #[error("Can only call functions and classes")]
    CantCallThis,
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
            Value::ClassInstance { class, .. } => write!(f, "{} instance", class.borrow()),
        }
    }
}

pub fn eval_expr(
    expr: &Expr,
    interpreter: &mut Interpreter,
) -> Result<Rc<RefCell<Value>>, EvalError> {
    match expr {
        Expr::Literal(literal_expr) => Ok(match literal_expr {
            crate::parser::LiteralExpr::Lit(literal) => match literal {
                crate::lexer::Literal::Number(n) => Value::Number(*n),
                crate::lexer::Literal::String(s) => Value::String(s.clone()),
            },
            crate::parser::LiteralExpr::True => Value::Bool(true),
            crate::parser::LiteralExpr::False => Value::Bool(false),
            crate::parser::LiteralExpr::Nil => Value::Nil,
        }
        .into()),
        Expr::Unary(token, expr) => match token.token_type {
            TokenType::Minus => {
                if let Value::Number(n) = &*eval_expr(expr, interpreter)?.borrow() {
                    Ok(Value::Number(-n).into())
                } else {
                    eval_error(token, OperandMustBeANumber)
                }
            }
            TokenType::Bang => match &*eval_expr(expr, interpreter)?.borrow() {
                Value::Bool(false) | Value::Nil => Ok(Value::Bool(true).into()),
                _ => Ok(Value::Bool(false).into()),
            },
            _ => eval_error(token, IvalidOperator),
        },
        Expr::Binary(expr, token, expr1) => {
            let left = eval_expr(expr, interpreter)?;
            let right = eval_expr(expr1, interpreter)?;
            Ok(match &token.token_type {
                TokenType::Plus => match (&*left.borrow(), &*right.borrow()) {
                    (Value::Number(l), Value::Number(r)) => Value::Number(l + r),
                    (Value::String(l), Value::String(r)) => Value::String(format!("{}{}", l, r)),
                    _ => eval_error(token, OperandsMustBeTwoNumbersOrTwoStrings)?,
                },
                TokenType::Minus => {
                    if let (Value::Number(l), Value::Number(r)) =
                        (&*left.borrow(), &*right.borrow())
                    {
                        Value::Number(l - r)
                    } else {
                        eval_error(token, OperandsMustBeNumbers)?
                    }
                }
                TokenType::Star => {
                    if let (Value::Number(l), Value::Number(r)) =
                        (&*left.borrow(), &*right.borrow())
                    {
                        Value::Number(l * r)
                    } else {
                        eval_error(token, OperandsMustBeNumbers)?
                    }
                }
                TokenType::Slash => {
                    if let (Value::Number(l), Value::Number(r)) =
                        (&*left.borrow(), &*right.borrow())
                    {
                        if *r == 0.0 {
                            return eval_error(token, IvalidOperator)?;
                        }
                        Value::Number(l / r)
                    } else {
                        eval_error(token, OperandsMustBeNumbers)?
                    }
                }
                TokenType::EqualEqual => match (&*left.borrow(), &*right.borrow()) {
                    (Value::Number(l), Value::Number(r)) => Value::Bool(l == r),
                    (Value::String(l), Value::String(r)) => Value::Bool(l == r),
                    (Value::Bool(l), Value::Bool(r)) => Value::Bool(l == r),
                    _ => Value::Bool(false),
                },
                TokenType::BangEqual => match (&*left.borrow(), &*right.borrow()) {
                    (Value::Number(l), Value::Number(r)) => Value::Bool(l != r),
                    (Value::String(l), Value::String(r)) => Value::Bool(l != r),
                    (Value::Bool(l), Value::Bool(r)) => Value::Bool(l != r),
                    _ => Value::Bool(true),
                },
                TokenType::Greater => {
                    if let (Value::Number(l), Value::Number(r)) =
                        (&*left.borrow(), &*right.borrow())
                    {
                        Value::Bool(l > r)
                    } else {
                        eval_error(token, OperandsMustBeNumbers)?
                    }
                }
                TokenType::GreaterEqual => {
                    if let (Value::Number(l), Value::Number(r)) =
                        (&*left.borrow(), &*right.borrow())
                    {
                        Value::Bool(l >= r)
                    } else {
                        eval_error(token, OperandsMustBeNumbers)?
                    }
                }
                TokenType::Less => {
                    if let (Value::Number(l), Value::Number(r)) =
                        (&*left.borrow(), &*right.borrow())
                    {
                        Value::Bool(l < r)
                    } else {
                        eval_error(token, OperandsMustBeNumbers)?
                    }
                }
                TokenType::LessEqual => {
                    if let (Value::Number(l), Value::Number(r)) =
                        (&*left.borrow(), &*right.borrow())
                    {
                        Value::Bool(l <= r)
                    } else {
                        eval_error(token, OperandsMustBeNumbers)?
                    }
                }
                _ => eval_error(token, IvalidOperator)?,
            }
            .into())
        }
        Expr::Grouping(expr) => eval_expr(expr, interpreter),
        Expr::Name(token, address) => match interpreter.get_var_by_address(token, address) {
            Some(value) => Ok(value),
            None => eval_error(token, UndefinedReference),
        },
        Expr::Assign(lhs, expr) => match lhs.as_ref() {
            Expr::Name(ref token, ref address) => {
                let result = eval_expr(expr, interpreter)?;
                if !interpreter.assign_var(token, address, result.clone()) {
                    return eval_error(token, UndefinedVariable);
                }
                Ok(result)
            }
            Expr::Dotted(ref _token, ref head, ref tail) => {
                let class_instance = eval_expr(head, interpreter)?;
                let rhs = eval_expr(expr, interpreter)?;
                match tail.as_ref() {
                    Expr::Literal(_)
                    | Expr::Unary(_, _)
                    | Expr::Binary(_, _, _)
                    | Expr::Logical(_, _, _)
                    | Expr::Assign(_, _)
                    | Expr::Grouping(_)
                    | Expr::Call(_, _, _) => todo!("Can't be"),
                    Expr::Name(token, _) => {
                        class_instance
                            .borrow_mut()
                            .get_properties_mut()
                            .unwrap()
                            .insert(token.lexeme.clone(), rhs.clone());
                        Ok(rhs)
                    }
                    Expr::Dotted(_, _, _) => {
                        todo!()
                    }
                    Expr::This(_) => todo!(),
                }
            }
            _ => todo!("Invalid assingee"),
        },
        Expr::Logical(expr1, operator, expr2) => {
            let result1 = eval_expr(expr1, interpreter)?;
            match (
                &*eval_expr(expr1, interpreter)?.borrow(),
                operator.lexeme.as_str(),
            ) {
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
                call_args.push((expr, eval_expr(expr, interpreter)?))
            }
            let callable = eval_expr(callee, interpreter)?;
            match &*eval_expr(callee, interpreter)?.borrow() {
                Value::Fn { name, .. } | Value::Class { name, .. } if is_primitive(name) => {
                    eval_primitive_function(name, token)
                }
                Value::Fn { .. } => {
                    interpreter.call(&Callable::Function(callable), &call_args, None, token)
                }
                Value::Class { .. } => {
                    interpreter.call(&Callable::Class(callable), &call_args, None, token)
                }
                _ => eval_error(token, UndefinedFunction)?,
            }
        }
        Expr::Dotted(_, ref head, ref tail) => {
            let class_instance = eval_expr(head, interpreter)?;
            Ok(eval_expr_in_class_instance(
                class_instance,
                tail,
                interpreter,
            )?)
        }
        Expr::This(token) => interpreter
            .get_this()
            .map_or(eval_error(token, InvalidReferenceToThis), Result::Ok),
    }
}

fn eval_expr_in_class_instance(
    class_instance: Rc<RefCell<Value>>,
    expr: &Expr,
    interpreter: &mut Interpreter,
) -> Result<Rc<RefCell<Value>>, EvalError> {
    match expr {
        Expr::Literal(_)
        | Expr::Unary(_, _)
        | Expr::Binary(_, _, _)
        | Expr::Logical(_, _, _)
        | Expr::Assign(_, _)
        | Expr::Grouping(_) => todo!("Can't be"),
        Expr::Name(token, _) => {
            if let Value::ClassInstance { properties, class } = &*class_instance.borrow() {
                match properties.get(&token.lexeme) {
                    Some(property) => Ok(property.clone()),
                    None => {
                        if let Value::Class { methods, .. } = &*class.borrow() {
                            match methods.get(&token.lexeme) {
                                Some(method) => {
                                    let mut new_bound_function = method.borrow().clone();
                                    if let Value::Fn { this, .. } = &mut new_bound_function {
                                        let _ = this.insert(class_instance.clone());
                                    }
                                    Ok(new_bound_function.into())
                                }
                                None => eval_error(token, UndefinedField)?,
                            }
                        } else {
                            todo!()
                        }
                    }
                }
            } else {
                todo!()
            }
        }
        Expr::Call(callee, token, args_exprs) => {
            let callable = match callee.as_ref() {
                Expr::Name(callee_token, _) => {
                    if let Value::ClassInstance { properties, class } = &*class_instance.borrow() {
                        let mut method = properties.get(&callee_token.lexeme).cloned();
                        if method.is_none() {
                            if let Value::Class { methods, .. } = &*class.borrow() {
                                if let Some(class_method) = methods.get(&callee_token.lexeme) {
                                    let mut new_bound_function = class_method.borrow().clone();
                                    if let Value::Fn { this, .. } = &mut new_bound_function {
                                        let _ = this.insert(class_instance.clone());
                                    }
                                    let _ = method.insert(new_bound_function.into());
                                }
                            } else {
                                todo!()
                            }
                        }
                        match method {
                            Some(method) => Callable::Method(method.clone()),
                            None => {
                                return eval_error(callee_token, UndefinedMethod);
                            }
                        }
                    } else {
                        todo!()
                    }
                }
                Expr::Call(sub_callee, sub_token, sub_args_exprs) => {
                    let mut call_args = vec![];
                    for expr in sub_args_exprs {
                        call_args.push((expr, eval_expr(expr, interpreter)?))
                    }
                    let method = &Callable::Method(eval_expr_in_class_instance(
                        class_instance.clone(),
                        sub_callee,
                        interpreter,
                    )?);
                    Callable::Method(interpreter.call(
                        method,
                        &call_args,
                        Some(class_instance.clone()),
                        sub_token,
                    )?)
                }
                _ => eval_error(token, CantCallThis)?,
            };
            let mut call_args = vec![];
            for expr in args_exprs {
                call_args.push((expr, eval_expr(expr, interpreter)?))
            }
            interpreter.call(&callable, &call_args, Some(class_instance.clone()), token)
        }
        Expr::Dotted(_, _, _) => {
            todo!()
        }
        Expr::This(_) => todo!(),
    }
}

fn eval_primitive_function(callee: &str, token: &Token) -> Result<Rc<RefCell<Value>>, EvalError> {
    match callee {
        "clock" => Ok(Value::Number(
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_secs() as f64,
        )
        .into()),
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
        Ast::Expr(expr) => Ok(eval_expr(expr, &mut Interpreter::new(&HashMap::new()))?
            .borrow()
            .clone()),
    }
}
