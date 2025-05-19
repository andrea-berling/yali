use std::iter::zip;
use std::vec;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use std::fmt::{Debug, Display, Formatter};

use crate::error::ErrorAtToken;
use crate::lexer::TokenType;
use crate::{
    lexer::Token,
    parser::{Declaration, Expr, Program, Statement},
};

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
                ..
            } => f
                .debug_struct("Fn")
                .field("name", name)
                .field("format_args", formal_args)
                .field("environment", environment)
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
        match self {
            Value::ClassInstance { class, .. } => Some(class.clone()),
            _ => None,
        }
    }

    pub fn get_superclass(&self) -> Option<Rc<RefCell<Value>>> {
        if let Value::Class { superclass, .. } = self {
            superclass.clone()
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
pub enum InternalErrorType {
    #[error("Malformed function body: should be a Block")]
    FunctionBodyShouldBeABlock,
    #[error("Malformed class body: should be a Block")]
    ClassBodyShouldBeABlock,
    #[error("Malformed lhs: can't assign to {0}")]
    CantAssignTo(Box<Expr>),
    #[error("Malformed expression: {0}")]
    MalformedExpression(Box<Expr>),
    #[error("Invalid class subexpression: {0}")]
    InvalidClassSubexpression(Box<Expr>),
}

use InternalErrorType::*;

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
    #[error("Can only call functions and classes")]
    CantCallThis,
    #[error("Superclass must be a class")]
    SuperclassesMustBeClasses,
    #[error("No error")]
    EarlyExit(Rc<RefCell<Value>>),
    #[error("Operand must be a class")]
    OperandMustBeClass,
    #[error("Internal error: {0}")]
    InternalError(InternalErrorType),
}

use thiserror::Error;
use EvalErrorType::*;

pub type EvalError = ErrorAtToken<EvalErrorType>;
type EvalResult = Result<Rc<RefCell<Value>>, EvalError>;
type RuntimeResult = Result<(), EvalError>;

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

fn eval_primitive_function(callee: &str, token: &Token) -> EvalResult {
    match callee {
        "clock" => Ok(Value::Number(
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_secs() as f64,
        )
        .into()),
        _ => Err(EvalError::new(token, UndefinedPrimitiveFunction)),
    }
}

fn is_primitive(callee: &str) -> bool {
    match callee {
        "clock" => true,
        "" => false, // rids us of warnings in case we want to keep the match to add primitive
        // functions
        _ => false,
    }
}

pub fn eval(expr: &Expr) -> Result<Value, EvalError> {
    Ok(Interpreter::new(&HashMap::new())
        .eval_expr(expr)?
        .borrow()
        .clone())
}

#[derive(Default)]
pub struct Environment {
    parent: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, Rc<RefCell<Value>>>,
}

impl Clone for Environment {
    fn clone(&self) -> Self {
        let mut values = HashMap::new();
        for (key, value) in &self.values {
            values.insert(key.clone(), value.borrow().clone().into());
        }
        Self {
            parent: self.parent.clone(),
            values,
        }
    }
}

impl Debug for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Environment")
            .field("values", &self.values.keys())
            .field(
                "this",
                &self.values.get("this").map(|x| {
                    x.borrow()
                        .get_properties()
                        .cloned()
                        .map(|x| x.keys().cloned().collect::<Vec<_>>())
                        .clone()
                }),
            )
            .field(
                "parent_addr",
                &self.parent.as_ref().map(|this| this.as_ptr()),
            )
            .field("parent", &self.parent)
            .finish()
    }
}

#[derive(Debug)]
pub enum Callable {
    Class(Rc<RefCell<Value>>),
    Function(Rc<RefCell<Value>>),
}

pub struct Interpreter<'a> {
    pub current_environment: Rc<RefCell<Environment>>,
    pub n_environments: usize,
    pub resolution_table: &'a HashMap<String, usize>,
}

impl Environment {
    pub fn new_with_builtin_functions(builtins: &[&str]) -> Self {
        let mut new_env = Self::default();
        for builtin in builtins {
            new_env.set(
                builtin,
                Value::Fn {
                    name: builtin.to_string(),
                    formal_args: vec![],
                    body: Statement::Block(vec![]),
                    environment: Default::default(),
                }
                .into(),
                true,
            );
        }
        new_env
    }

    pub fn get(&self, name: &str) -> Option<Rc<RefCell<Value>>> {
        if let Some(value) = self.values.get(name) {
            Some(value.clone())
        } else if let Some(environment) = self.parent.as_ref() {
            environment.borrow().get(name)
        } else {
            None
        }
    }

    pub fn set(&mut self, name: &str, new_value: Rc<RefCell<Value>>, new: bool) -> bool {
        if new || self.values.contains_key(name) {
            self.values.insert(name.to_string(), new_value);
            true
        } else if let Some(environment) = &self.parent {
            environment.borrow_mut().set(name, new_value, new)
        } else {
            false
        }
    }
}

impl<'a> Interpreter<'a> {
    pub fn new(resolution_table: &'a HashMap<String, usize>) -> Self {
        Self {
            current_environment: Rc::new(RefCell::new(Environment::new_with_builtin_functions(&[
                "clock",
            ]))),
            resolution_table,
            n_environments: 0,
        }
    }

    pub fn get_var_by_address(&self, token: &Token, address: &str) -> Option<Rc<RefCell<Value>>> {
        let depth = match self.resolution_table.get(address) {
            Some(depth) => *depth,
            None => usize::MAX,
        };

        self.get_var_at_depth(&token.lexeme, depth)
    }

    fn environment_chain_length(&self) -> usize {
        let mut n_envs = 0;
        let mut curr_env = self.current_environment.clone();
        while let Some(parent_env) = &curr_env.clone().borrow().parent {
            n_envs += 1;
            curr_env = parent_env.clone();
        }
        n_envs
    }

    // When adding the environment, we assume the caller owns the environment
    pub fn add_environment(&mut self, mut environment: Environment) {
        environment.parent = Some(self.current_environment.clone());
        self.current_environment = Rc::new(RefCell::new(environment));
        self.n_environments += 1;
    }

    pub fn set_environment(&mut self, environment: Rc<RefCell<Environment>>) {
        self.current_environment = environment;
        self.n_environments = self.environment_chain_length()
    }

    // When giving back the environment, we can't assume the one we have this is the only reference
    // to it (there may be other closures pointing to it)
    pub fn pop_environment(&mut self) -> Rc<RefCell<Environment>> {
        let return_value = self.current_environment.clone();
        let new_env = self.current_environment.borrow().parent.clone().unwrap();
        self.current_environment = new_env;
        self.n_environments -= 1;
        return_value
    }

    pub fn set_var(&mut self, name: &str, value: Rc<RefCell<Value>>, new: bool) -> bool {
        self.current_environment.borrow_mut().set(name, value, new)
    }

    fn find_environment(&self, depth: usize) -> Rc<RefCell<Environment>> {
        let mut return_value = self.current_environment.clone();
        let mut depth = depth.min(self.n_environments - 1);
        while depth > 0 {
            return_value = match &return_value.clone().borrow().parent {
                Some(parent) => parent.clone(),
                None => return_value,
            };
            if return_value.borrow().parent.is_none() {
                break;
            }
            depth -= 1
        }
        return_value
    }

    fn set_var_at_depth(&mut self, name: &str, value: Rc<RefCell<Value>>, depth: usize) -> bool {
        self.find_environment(depth)
            .borrow_mut()
            .set(name, value, false)
    }

    fn get_var_at_depth(&self, name: &str, depth: usize) -> Option<Rc<RefCell<Value>>> {
        self.find_environment(depth).borrow().get(name)
    }

    pub fn get_var(&self, name: &str) -> Option<Rc<RefCell<Value>>> {
        self.current_environment.borrow().get(name)
    }

    pub fn assign_var(&mut self, token: &Token, address: &str, value: Rc<RefCell<Value>>) -> bool {
        let depth = match self.resolution_table.get(address) {
            Some(depth) => *depth,
            None => usize::MAX,
        };
        self.set_var_at_depth(&token.lexeme, value, depth)
    }

    pub fn statement(&mut self, statement: &Statement) -> RuntimeResult {
        match statement {
            Statement::Expr(expr) => {
                self.eval_expr(expr)?;
                Ok(())
            }
            Statement::Print(expr) => {
                println!("{}", self.eval_expr(expr)?.borrow());
                Ok(())
            }
            Statement::Block(declarations) => {
                self.add_environment(Environment::default());
                for declaration in declarations.iter() {
                    self.declaration(declaration)?;
                }
                self.pop_environment();
                Ok(())
            }
            Statement::If(condition, if_statement, else_statement) => {
                if !matches!(
                    &*self.eval_expr(condition)?.borrow(),
                    Value::Bool(false) | Value::Nil
                ) {
                    self.statement(if_statement)?;
                } else if let Some(else_statement) = else_statement {
                    self.statement(else_statement)?;
                }
                Ok(())
            }
            Statement::While(while_condition, statement) => {
                let mut condition = self.eval_expr(while_condition)?;
                while !matches!(&*condition.borrow(), Value::Bool(false) | Value::Nil) {
                    self.statement(statement)?;
                    condition = self.eval_expr(while_condition)?;
                }
                Ok(())
            }
            Statement::Return(_, expr) => Err(EvalError::new_without_token(EarlyExit(
                self.eval_expr(
                    expr.as_ref()
                        .unwrap_or(&Expr::Literal(crate::parser::LiteralExpr::Nil)),
                )?,
            ))),
        }
    }

    pub fn declaration(&mut self, declaration: &Declaration) -> RuntimeResult {
        match declaration {
            Declaration::Var(ident_token, expr) => {
                match expr {
                    Some(expr) => {
                        let value_to_insert = self.eval_expr(expr)?;
                        self.set_var(&ident_token.lexeme, value_to_insert, true);
                    }
                    None => {
                        self.set_var(&ident_token.lexeme, Value::Nil.into(), true);
                    }
                }
                Ok(())
            }
            Declaration::Statement(statement) => self.statement(statement),
            Declaration::Function(name, args, body) => {
                self.set_var(
                    &name.lexeme,
                    Value::Fn {
                        name: name.lexeme.clone(),
                        formal_args: args.clone(),
                        body: body.clone(),
                        environment: self.current_environment.clone(),
                    }
                    .into(),
                    true,
                );
                Ok(())
            }
            Declaration::Class(name, superclass, body) => {
                let superclass = if let Some(superclass) = superclass {
                    let Expr::Name(superclass_token, _) = &superclass else {
                        unreachable!()
                    };
                    let superclass = self.eval_expr(superclass)?.clone();
                    if superclass.borrow().get_type() != "Class" {
                        return Err(EvalError::new(superclass_token, SuperclassesMustBeClasses));
                    }
                    Some(superclass)
                } else {
                    None
                };
                let Statement::Block(body) = body else {
                    return Err(EvalError::new_without_token(InternalError(
                        ClassBodyShouldBeABlock,
                    )));
                };
                let mut new_env = Environment::default();
                if let Some(superclass) = superclass.as_ref() {
                    new_env.set("super", superclass.clone(), true);
                }
                self.add_environment(new_env);
                let new_class: Rc<RefCell<Value>> = Value::Class {
                    name: name.lexeme.clone(),
                    superclass,
                    methods: HashMap::new(),
                }
                .into();
                for declaration in body {
                    if let Declaration::Function(name, args, body) = declaration {
                        if let Value::Class { methods, .. } = &mut *new_class.borrow_mut() {
                            methods.insert(
                                name.lexeme.clone(),
                                Value::Fn {
                                    name: name.lexeme.clone(),
                                    formal_args: args.clone(),
                                    body: body.clone(),
                                    environment: self.current_environment.clone(),
                                }
                                .into(),
                            );
                        }
                    }
                }
                self.pop_environment();
                self.set_var(&name.lexeme, new_class, true);
                Ok(())
            }
        }
    }

    pub fn run(&mut self, program: &Program) -> RuntimeResult {
        self.statement(program)
    }

    pub fn eval_expr(&mut self, expr: &Expr) -> EvalResult {
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
                    if let Value::Number(n) = &*self.eval_expr(expr)?.borrow() {
                        Ok(Value::Number(-n).into())
                    } else {
                        Err(EvalError::new(token, OperandMustBeANumber))
                    }
                }
                TokenType::Bang => match &*self.eval_expr(expr)?.borrow() {
                    Value::Bool(false) | Value::Nil => Ok(Value::Bool(true).into()),
                    _ => Ok(Value::Bool(false).into()),
                },
                _ => Err(EvalError::new(token, IvalidOperator)),
            },
            Expr::Binary(expr, token, expr1) => {
                let left = self.eval_expr(expr)?;
                let right = self.eval_expr(expr1)?;
                Ok(match &token.token_type {
                    TokenType::Plus => match (&*left.borrow(), &*right.borrow()) {
                        (Value::Number(l), Value::Number(r)) => Value::Number(l + r),
                        (Value::String(l), Value::String(r)) => {
                            Value::String(format!("{}{}", l, r))
                        }
                        _ => Err(EvalError::new(token, OperandsMustBeTwoNumbersOrTwoStrings))?,
                    },
                    TokenType::Minus => {
                        if let (Value::Number(l), Value::Number(r)) =
                            (&*left.borrow(), &*right.borrow())
                        {
                            Value::Number(l - r)
                        } else {
                            Err(EvalError::new(token, OperandsMustBeNumbers))?
                        }
                    }
                    TokenType::Star => {
                        if let (Value::Number(l), Value::Number(r)) =
                            (&*left.borrow(), &*right.borrow())
                        {
                            Value::Number(l * r)
                        } else {
                            Err(EvalError::new(token, OperandsMustBeNumbers))?
                        }
                    }
                    TokenType::Slash => {
                        if let (Value::Number(l), Value::Number(r)) =
                            (&*left.borrow(), &*right.borrow())
                        {
                            if *r == 0.0 {
                                return Err(EvalError::new(token, IvalidOperator))?;
                            }
                            Value::Number(l / r)
                        } else {
                            Err(EvalError::new(token, OperandsMustBeNumbers))?
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
                            Err(EvalError::new(token, OperandsMustBeNumbers))?
                        }
                    }
                    TokenType::GreaterEqual => {
                        if let (Value::Number(l), Value::Number(r)) =
                            (&*left.borrow(), &*right.borrow())
                        {
                            Value::Bool(l >= r)
                        } else {
                            Err(EvalError::new(token, OperandsMustBeNumbers))?
                        }
                    }
                    TokenType::Less => {
                        if let (Value::Number(l), Value::Number(r)) =
                            (&*left.borrow(), &*right.borrow())
                        {
                            Value::Bool(l < r)
                        } else {
                            Err(EvalError::new(token, OperandsMustBeNumbers))?
                        }
                    }
                    TokenType::LessEqual => {
                        if let (Value::Number(l), Value::Number(r)) =
                            (&*left.borrow(), &*right.borrow())
                        {
                            Value::Bool(l <= r)
                        } else {
                            Err(EvalError::new(token, OperandsMustBeNumbers))?
                        }
                    }
                    _ => Err(EvalError::new(token, IvalidOperator))?,
                }
                .into())
            }
            Expr::Grouping(expr) => self.eval_expr(expr),
            Expr::Name(token, address) => match self.get_var_by_address(token, address) {
                Some(value) => Ok(value),
                None => Err(EvalError::new(token, UndefinedReference)),
            },
            Expr::Assign(lhs, expr) => match lhs.as_ref() {
                Expr::Name(ref token, ref address) => {
                    let result = self.eval_expr(expr)?;
                    if !self.assign_var(token, address, result.clone()) {
                        return Err(EvalError::new(token, UndefinedVariable));
                    }
                    Ok(result)
                }
                Expr::Dotted(ref _token, ref head, ref tail) => {
                    let class_instance = self.eval_expr(head)?;
                    let rhs = self.eval_expr(expr)?;
                    match tail.as_ref() {
                        Expr::Literal(..)
                        | Expr::Unary(..)
                        | Expr::Binary(..)
                        | Expr::Logical(..)
                        | Expr::Assign(..)
                        | Expr::Grouping(..)
                        | Expr::Call(..)
                        | Expr::Dotted(..)
                        | Expr::This(_)
                        | Expr::Super(..) => Err(EvalError::new_without_token(InternalError(
                            CantAssignTo(expr.clone()),
                        ))),
                        Expr::Name(token, _) => {
                            class_instance
                                .borrow_mut()
                                .get_properties_mut()
                                .unwrap()
                                .insert(token.lexeme.clone(), rhs.clone());
                            Ok(rhs)
                        }
                    }
                }
                _ => Err(EvalError::new_without_token(InternalError(CantAssignTo(
                    expr.clone(),
                )))),
            },
            Expr::Logical(expr1, operator, expr2) => {
                let result1 = self.eval_expr(expr1)?;
                match (&*self.eval_expr(expr1)?.borrow(), operator.lexeme.as_str()) {
                    (Value::Nil | Value::Bool(false), "or") => Ok(self.eval_expr(expr2)?),
                    (_, "or") => Ok(result1),
                    (Value::Nil | Value::Bool(false), "and") => Ok(result1),
                    (_, "and") => Ok(self.eval_expr(expr2)?),
                    _ => Err(EvalError::new_without_token(InternalError(
                        MalformedExpression(expr.clone().into()),
                    ))),
                }
            }
            Expr::Call(callee, token, args) => {
                let mut call_args = vec![];
                for expr in args {
                    call_args.push((expr, self.eval_expr(expr)?))
                }
                let callable = self.eval_expr(callee)?;
                match &*self.eval_expr(callee)?.borrow() {
                    Value::Fn { name, .. } | Value::Class { name, .. } if is_primitive(name) => {
                        eval_primitive_function(name, token)
                    }
                    Value::Fn { .. } => self.call(&Callable::Function(callable), &call_args, token),
                    Value::Class { .. } => self.call(&Callable::Class(callable), &call_args, token),
                    _ => Err(EvalError::new(token, UndefinedFunction))?,
                }
            }
            Expr::Dotted(_, ref head, ref tail) => {
                let class_instance = self.eval_expr(head)?;
                Ok(self.eval_expr_in_class_instance(class_instance, tail)?)
            }
            Expr::This(token) => match self.get_var(&token.lexeme) {
                Some(value) => Ok(value),
                None => Err(EvalError::new(token, UndefinedReference)),
            },
            Expr::Super(token, method) => match self.get_var(&token.lexeme) {
                Some(value) => value
                    .borrow()
                    .get_methods()
                    .unwrap()
                    .get(&method.lexeme)
                    .cloned()
                    .ok_or(EvalError::new(method, UndefinedMethod)),
                None => Err(EvalError::new(token, UndefinedReference)),
            },
        }
    }

    fn bind_function(
        function: &Rc<RefCell<Value>>,
        this: &Rc<RefCell<Value>>,
    ) -> Rc<RefCell<Value>> {
        // Cloning data structures that have pointers is fun :D
        let new_bound_function: Rc<RefCell<Value>> = function.borrow().clone().into();
        if let Value::Fn { environment, .. } = &mut *new_bound_function.borrow_mut() {
            let new_env = Rc::new(RefCell::new(environment.borrow().clone()));
            *environment = new_env;
            environment.borrow_mut().set("this", this.clone(), true);
        }
        new_bound_function.clone()
    }

    fn eval_expr_in_class_instance(
        &mut self,
        class_instance: Rc<RefCell<Value>>,
        expr: &Expr,
    ) -> EvalResult {
        match expr {
            Expr::Literal(..)
            | Expr::Unary(..)
            | Expr::Binary(..)
            | Expr::Logical(..)
            | Expr::Assign(..)
            | Expr::Grouping(..)
            | Expr::Dotted(..)
            | Expr::This(_)
            | Expr::Super(..) => Err(EvalError::new_without_token(InternalError(
                InvalidClassSubexpression(expr.clone().into()),
            ))),
            Expr::Name(token, _) => {
                if let Value::ClassInstance { properties, class } = &*class_instance.borrow() {
                    match properties.get(&token.lexeme) {
                        Some(property) => Ok(property.clone()),
                        None => {
                            let mut class = Some(class.clone());
                            while class.is_some() {
                                if let Value::Class {
                                    methods,
                                    superclass,
                                    ..
                                } = &*class.clone().unwrap().borrow()
                                {
                                    match methods.get(&token.lexeme) {
                                        Some(method) => {
                                            return Ok(Self::bind_function(
                                                method,
                                                &class_instance,
                                            ));
                                        }
                                        None => class = superclass.clone(),
                                    }
                                }
                            }
                            Err(EvalError::new(token, UndefinedField))?
                        }
                    }
                } else {
                    Err(EvalError::new_without_token(OperandMustBeClass))
                }
            }
            Expr::Call(callee, token, args_exprs) => {
                let callable = match callee.as_ref() {
                    Expr::Name(..) => Callable::Function(
                        self.eval_expr_in_class_instance(class_instance.clone(), callee)?,
                    ),
                    Expr::Call(sub_callee, sub_token, sub_args_exprs) => {
                        let mut call_args = vec![];
                        for expr in sub_args_exprs {
                            call_args.push((expr, self.eval_expr(expr)?))
                        }
                        let sub_callable = &Callable::Function(
                            self.eval_expr_in_class_instance(class_instance.clone(), sub_callee)?,
                        );
                        Callable::Function(self.call(sub_callable, &call_args, sub_token)?)
                    }
                    _ => Err(EvalError::new(token, CantCallThis))?,
                };
                let mut call_args = vec![];
                for expr in args_exprs {
                    call_args.push((expr, self.eval_expr(expr)?))
                }
                self.call(&callable, &call_args, token)
            }
        }
    }

    pub fn call(
        &mut self,
        callable: &Callable,
        call_args: &[(&Expr, Rc<RefCell<Value>>)],
        token: &Token,
    ) -> Result<Rc<RefCell<Value>>, EvalError> {
        let callee = match callable {
            Callable::Class(val) | Callable::Function(val)
                if matches!(&*val.borrow(), Value::Class { .. } | Value::Fn { .. }) =>
            {
                val.clone()
            }
            _ => unreachable!(),
        };
        let Value::Fn {
            name: function_name,
            formal_args,
            body,
            environment,
            ..
        } = &*callee.borrow()
        else {
            if let Value::Class {
                methods,
                superclass,
                ..
            } = &*callee.borrow()
            {
                let new_instance: Rc<RefCell<Value>> = Value::ClassInstance {
                    class: callee.clone(),
                    properties: match superclass {
                        Some(superclass) if methods.get("init").is_none() => self
                            .call(&Callable::Class(superclass.clone()), call_args, token)?
                            .borrow()
                            .get_properties()
                            .unwrap()
                            .clone(),
                        _ => HashMap::new(),
                    },
                }
                .into();

                if let Some(initializer) = methods.get("init") {
                    let initializer = Self::bind_function(initializer, &new_instance);
                    self.call(&Callable::Function(initializer.clone()), call_args, token)?;
                }
                return Ok(new_instance);
            } else {
                return Err(EvalError::new(token, UndefinedFunction))?;
            }
        };
        if formal_args.len() != call_args.len() {
            return Err(EvalError::new(
                token,
                WrongArgumentNumber(formal_args.len(), call_args.len()),
            ));
        }
        let Statement::Block(declarations) = body.clone() else {
            return Err(EvalError::new_without_token(InternalError(
                FunctionBodyShouldBeABlock,
            )));
        };

        let old_environment = self.current_environment.clone();
        self.set_environment(environment.clone());
        let mut new_env = Environment::default();
        for (var_token, var_value) in zip(
            formal_args.clone(),
            call_args
                .iter()
                .cloned()
                .unzip::<&Expr, Rc<RefCell<Value>>, Vec<_>, Vec<_>>()
                .1,
        ) {
            new_env.set(&var_token.lexeme, var_value.clone(), true);
        }

        self.add_environment(new_env);

        let env_before_running = self.current_environment.clone();
        let mut return_value = Value::Nil.into();
        for declaration in &declarations {
            match self.declaration(declaration) {
                Err(EvalError {
                    error: EvalErrorType::EarlyExit(val),
                    ..
                }) => {
                    self.set_environment(env_before_running.clone());
                    return_value = val;
                    break;
                }
                Err(e) => {
                    return Err(e);
                }
                _ => {}
            };
        }

        if function_name == "init" {
            return_value = self.get_var("this").unwrap().clone()
        }

        self.pop_environment();
        self.set_environment(old_environment);
        Ok(return_value)
    }
}
