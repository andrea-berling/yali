use std::{collections::VecDeque, fmt::Display};

use thiserror::Error;

use crate::{
    eval::{eval_expr, EvalError, EvalResult},
    parser::{Declaration, Program, Statement},
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
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
            Value::String(s) => write!(f, "{s}"),
            Value::Boolean(b) => write!(f, "{b}"),
        }
    }
}

pub struct Environment(VecDeque<std::collections::HashMap<String, Option<Value>>>);

pub struct State {
    pub environment: Environment,
}

pub struct Interpreter {
    pub program: Program,
    pub state: State,
}

impl Environment {
    pub fn new() -> Self {
        Self(VecDeque::from(vec![std::collections::HashMap::new()]))
    }

    pub fn get(&self, name: &str) -> Option<&Option<Value>> {
        for scope in self.0.iter() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }
        None
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut Option<Value>> {
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

    pub fn set(&mut self, name: &str, value: Option<Value>, new: bool) -> bool {
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
            },
        }
    }

    pub fn statement(&mut self, statement: &Statement) -> Result<(), RuntimeError> {
        match statement {
            Statement::Expr(expr) => self.maybe_assign(&eval_expr(expr, &self.state.environment)?),
            Statement::Print(expr) => {
                let eval_result = eval_expr(expr, &self.state.environment)?;
                self.maybe_assign(&eval_result)?;
                if let Some(value) = Self::eval_result_to_value(&eval_result) {
                    println!("{value}");
                } else {
                    println!("nil")
                }
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
                let eval_result = eval_expr(condition, &self.state.environment)?;
                self.maybe_assign(&eval_result)?;
                let condition = Self::eval_result_to_value(&eval_result);
                if condition.is_some_and(|condition| !matches!(condition, Value::Boolean(false))) {
                    self.statement(if_statement)?;
                } else if let Some(else_statement) = else_statement {
                    self.statement(else_statement)?;
                }
                Ok(())
            }
            Statement::While(while_condition, statement) => {
                let eval_result = eval_expr(while_condition, &self.state.environment)?;
                let mut condition = Self::eval_result_to_value(&eval_result);
                while condition.is_some_and(|condition| !matches!(condition, Value::Boolean(false)))
                {
                    self.statement(statement)?;
                    condition = Self::eval_result_to_value(&eval_expr(
                        while_condition,
                        &self.state.environment,
                    )?);
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
                        let value_to_insert =
                            Self::eval_result_to_value(&eval_expr(expr, &self.state.environment)?);
                        self.state
                            .environment
                            .set(&ident_token.lexeme, value_to_insert, true);
                    }
                    None => {
                        self.state.environment.set(&ident_token.lexeme, None, true);
                    }
                }
                Ok(())
            }
            Declaration::Statement(statement) => self.statement(statement),
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

    fn eval_result_to_value(eval_result: &EvalResult) -> Option<Value> {
        match eval_result {
            EvalResult::Number(n) => Some(Value::Number(*n)),
            EvalResult::String(s) => Some(Value::String(s.clone())),
            EvalResult::Bool(b) => Some(Value::Boolean(*b)),
            EvalResult::Nil => None,
            EvalResult::Assign(_, eval_result) => Self::eval_result_to_value(eval_result),
            EvalResult::Logical(token, eval_result1, eval_result2) => match (
                Self::eval_result_to_value(eval_result1),
                token.lexeme.as_str(),
            ) {
                (None | Some(Value::Boolean(false)), "or") => {
                    Self::eval_result_to_value(eval_result2)
                }
                (result1, "or") => result1,
                (result1 @ (None | Some(Value::Boolean(false))), "and") => result1,
                (_, "and") => Self::eval_result_to_value(eval_result2),
                _ => todo!("Shouldn't happen"),
            },
        }
    }

    // TODO: clean up and turn into "side_effects"
    fn maybe_assign(&mut self, eval_result: &EvalResult) -> Result<(), RuntimeError> {
        if let EvalResult::Assign(var_token, eval_result) = eval_result {
            self.state.environment.set(
                &var_token.lexeme,
                Self::eval_result_to_value(eval_result),
                false,
            );
            return self.maybe_assign(eval_result);
        } else if let EvalResult::Logical(token, eval_result1, eval_result2) = eval_result {
            match (
                Self::eval_result_to_value(eval_result1),
                token.lexeme.as_str(),
            ) {
                (None | Some(Value::Boolean(false)), "or") => {
                    self.maybe_assign(eval_result1)?;
                    return self.maybe_assign(eval_result2);
                }
                (_, "or") => {
                    return self.maybe_assign(eval_result1);
                }
                (None | Some(Value::Boolean(false)), "and") => {
                    return self.maybe_assign(eval_result1);
                }
                (_, "and") => {
                    self.maybe_assign(eval_result1)?;
                    return self.maybe_assign(eval_result2);
                }
                _ => {
                    todo!("Shouldn't happen");
                }
            }
        }
        Ok(())
    }
}
