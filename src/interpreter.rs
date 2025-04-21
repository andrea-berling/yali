use std::{
    collections::{HashMap, VecDeque},
    fmt::Display,
    iter::zip,
};

use thiserror::Error;

use crate::{
    eval::{eval_error, EvalError, EvalErrorType, EvalResult},
    parser::{Declaration, Expr, Program, Statement},
};

#[derive(Debug, Error)]
pub enum RuntimeError {
    #[error(transparent)]
    EvalError(#[from] EvalError),
}

#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
    Function(String),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
            Value::String(s) => write!(f, "{s}"),
            Value::Boolean(b) => write!(f, "{b}"),
            Value::Nil => write!(f, "nil"),
            Value::Function(func) => write!(f, "<fn {func}>"),
        }
    }
}

impl From<&EvalResult> for Value {
    fn from(eval_result: &EvalResult) -> Self {
        match eval_result {
            EvalResult::Number(n) => Value::Number(*n),
            EvalResult::String(s) => Value::String(s.clone()),
            EvalResult::Bool(b) => Value::Boolean(*b),
            EvalResult::Nil => Value::Nil,
            EvalResult::Assign(_, eval_result) => Self::from(eval_result.as_ref()),
            EvalResult::Logical(token, eval_result1, eval_result2) => {
                match (Self::from(eval_result1.as_ref()), token.lexeme.as_str()) {
                    (Value::Nil | Value::Boolean(false), "or") => Self::from(eval_result2.as_ref()),
                    (result1, "or") => result1,
                    (result1 @ (Value::Nil | Value::Boolean(false)), "and") => result1,
                    (_, "and") => Self::from(eval_result2.as_ref()),
                    _ => todo!("Shouldn't happen"),
                }
            }
            EvalResult::FnCall(token, vec) => todo!(),
            EvalResult::Fn(func) => Value::Function(func.to_string()),
        }
    }
}

pub struct Environment(VecDeque<std::collections::HashMap<String, Value>>);

pub struct State {
    pub environment: Environment,
    pub functions: HashMap<String, (Vec<String>, Vec<Declaration>)>,
}

pub struct Interpreter {
    pub program: Program,
    pub state: State,
}

impl Environment {
    pub fn new() -> Self {
        Self(VecDeque::from(vec![std::collections::HashMap::new()]))
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        for scope in self.0.iter() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }
        None
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut Value> {
        for scope in self.0.iter_mut() {
            if let Some(value) = scope.get_mut(name) {
                return Some(value);
            }
        }
        None
    }

    pub fn add_scope(&mut self) {
        self.0.push_front(std::collections::HashMap::new());
    }

    pub fn drop_scope(&mut self) {
        self.0.pop_front();
    }

    pub fn set(&mut self, name: &str, value: Value, new: bool) -> bool {
        let latest_scope = self
            .0
            .front_mut()
            .expect("Environment should have at least one scope");
        if new {
            latest_scope.insert(name.to_string(), value);
            true
        } else {
            let Some(old_value) = self.get_mut(name) else {
                return false;
            };
            *old_value = value;
            true
        }
    }
}

impl Interpreter {
    pub fn new(program: Program) -> Self {
        Self {
            program,
            state: State {
                environment: Environment::new(),
                functions: HashMap::new(),
            },
        }
    }

    fn eval_expr(&mut self, expr: &Expr) -> Result<EvalResult, RuntimeError> {
        let eval_result =
            self.handle_function_calls(crate::eval::eval_expr(expr, &self.state.environment)?)?;
        self.handle_side_effects(&eval_result)?;
        Ok(eval_result)
    }

    pub fn statement(&mut self, statement: &Statement) -> Result<(), RuntimeError> {
        match statement {
            Statement::Expr(expr) => self.eval_expr(expr).map(|_| ()),
            Statement::Print(expr) => {
                println!("{}", Value::from(&self.eval_expr(expr)?));
                Ok(())
            }
            Statement::Block(declarations) => {
                self.state.environment.add_scope();
                for declaration in declarations.iter() {
                    self.declaration(declaration)?;
                }
                self.state.environment.drop_scope();
                Ok(())
            }
            Statement::If(condition, if_statement, else_statement) => {
                let condition = Value::from(&self.eval_expr(condition)?);
                if !matches!(condition, Value::Boolean(false) | Value::Nil) {
                    self.statement(if_statement)?;
                } else if let Some(else_statement) = else_statement {
                    self.statement(else_statement)?;
                }
                Ok(())
            }
            Statement::While(while_condition, statement) => {
                let mut condition = Value::from(&self.eval_expr(while_condition)?);
                while !matches!(condition, Value::Boolean(false) | Value::Nil) {
                    self.statement(statement)?;
                    condition = Value::from(&self.eval_expr(while_condition)?);
                }
                Ok(())
            }
        }
    }

    pub fn declaration(&mut self, declaration: &Declaration) -> Result<(), RuntimeError> {
        match declaration {
            Declaration::Var(ident_token, expr) => {
                match expr {
                    Some(expr) => {
                        let value_to_insert = Value::from(&self.eval_expr(expr)?);
                        self.state
                            .environment
                            .set(&ident_token.lexeme, value_to_insert, true);
                    }
                    None => {
                        self.state
                            .environment
                            .set(&ident_token.lexeme, Value::Nil, true);
                    }
                }
                Ok(())
            }
            Declaration::Statement(statement) => self.statement(statement),
            Declaration::Function(name, args, body) => {
                self.state.functions.insert(
                    name.lexeme.clone(),
                    (
                        args.iter().map(|x| x.lexeme.clone()).collect(),
                        body.clone(),
                    ),
                );
                self.state.environment.set(
                    &name.lexeme,
                    Value::Function(name.lexeme.clone()),
                    true,
                );
                Ok(())
            }
        }
    }

    /*

    */

    pub fn run(&mut self) -> Result<(), RuntimeError> {
        for declaration in self.program.clone().iter() {
            self.declaration(declaration)?;
        }
        Ok(())
    }

    fn handle_side_effects(&mut self, eval_result: &EvalResult) -> Result<(), RuntimeError> {
        match eval_result {
            EvalResult::Assign(var_token, eval_result) => {
                self.state.environment.set(
                    &var_token.lexeme,
                    Value::from(eval_result.as_ref()),
                    false,
                );
                return self.handle_side_effects(eval_result);
            }
            EvalResult::Logical(token, eval_result1, eval_result2) => {
                match (Value::from(eval_result1.as_ref()), token.lexeme.as_str()) {
                    (Value::Nil | Value::Boolean(false), "or") => {
                        self.handle_side_effects(eval_result1)?;
                        return self.handle_side_effects(eval_result2);
                    }
                    (_, "or") => {
                        return self.handle_side_effects(eval_result1);
                    }
                    (Value::Nil | Value::Boolean(false), "and") => {
                        return self.handle_side_effects(eval_result1);
                    }
                    (_, "and") => {
                        self.handle_side_effects(eval_result1)?;
                        return self.handle_side_effects(eval_result2);
                    }
                    _ => {
                        todo!("Shouldn't happen");
                    }
                }
            }
            EvalResult::Number(_)
            | EvalResult::String(_)
            | EvalResult::Bool(_)
            | EvalResult::Nil => {}
            EvalResult::FnCall(_, vec) => {
                for result in vec {
                    self.handle_side_effects(result)?
                }
            }
            EvalResult::Fn(_) => (),
        }
        Ok(())
    }

    fn handle_function_calls(
        &mut self,
        eval_result: EvalResult,
    ) -> Result<EvalResult, RuntimeError> {
        match eval_result {
            EvalResult::Number(_)
            | EvalResult::String(_)
            | EvalResult::Bool(_)
            | EvalResult::Nil => Ok(eval_result),
            EvalResult::Assign(t, eval_result) => Ok(EvalResult::Assign(
                t,
                Box::new(self.handle_function_calls(*eval_result)?),
            )),
            EvalResult::Logical(operator, result1, result2) => Ok(EvalResult::Logical(
                operator,
                Box::new(self.handle_function_calls(*result1)?),
                Box::new(self.handle_function_calls(*result2)?),
            )),
            EvalResult::FnCall(ref token, ref call_args) => {
                let function_name = &token.lexeme;
                let Some((parameters, body)) = self.state.functions.get(function_name).cloned()
                else {
                    return eval_error(token, EvalErrorType::UndefinedFunction)?;
                };
                if parameters.len() != call_args.len() {
                    return eval_error(
                        token,
                        EvalErrorType::WrongArgumentNumber(parameters.len(), call_args.len()),
                    )?;
                }
                self.state.environment.add_scope();
                for (var_name, var_value) in zip(parameters, call_args) {
                    let var_value = Value::from(&self.handle_function_calls(var_value.clone())?);
                    let latest_scope = self.state.environment.0.front_mut().unwrap();
                    latest_scope.insert(var_name.clone(), var_value);
                }
                for declaration in body {
                    self.declaration(&declaration)?;
                }
                self.state.environment.drop_scope();
                Ok(eval_result.clone())
            }
            EvalResult::Fn(_) => Ok(eval_result),
        }
    }
}
