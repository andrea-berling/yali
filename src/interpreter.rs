use std::collections::VecDeque;

use crate::{
    eval::{eval_expr, EvalError, EvalResult},
    parser::{Declaration, Program, Statement},
};

#[derive(Debug)]
pub enum RuntimeErrorType {
    EvalError,
    UndefinedVariable(String),
}

#[derive(Debug)]
pub struct RuntimeError {
    pub line: usize,
    pub error: RuntimeErrorType,
}

impl RuntimeError {
    pub fn new(line: usize, error: RuntimeErrorType) -> Self {
        Self { line, error }
    }
}

#[derive(Clone)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
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
        Self(VecDeque::new())
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

    pub fn set(
        &mut self,
        name: String,
        value: Option<Value>,
        new: bool,
    ) -> Result<(), RuntimeError> {
        let latest_scope = self
            .0
            .front_mut()
            .expect("Environment should have at least one scope");
        if new {
            latest_scope.insert(name, value);
            Ok(())
        } else {
            let Some(old_value) = self.get_mut(&name) else {
                return Err(RuntimeError::new(
                    0,
                    RuntimeErrorType::UndefinedVariable(name.clone()),
                ));
            };
            *old_value = value;
            Ok(())
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
            Statement::Expr(expr) => match eval_expr(expr, &self.state.environment) {
                Ok(eval_result) => {
                    self.maybe_assign(&eval_result)?;
                    Ok(())
                }
                Err(err) => {
                    eprintln!("{}\n[line {}]", err.error, err.line);
                    Err(RuntimeError::new(err.line, RuntimeErrorType::EvalError))
                }
            },
            Statement::Print(expr) => match eval_expr(expr, &self.state.environment) {
                Err(EvalError { line, error }) => {
                    eprintln!("{}\n[line {}]", error, line);
                    Err(RuntimeError::new(line, RuntimeErrorType::EvalError))
                }
                Ok(eval_result) => {
                    self.maybe_assign(&eval_result)?;
                    println!("{}", eval_result);
                    Ok(())
                }
            },
            Statement::Block(declarations) => {
                self.state.environment.add_scope();
                for declaration in declarations.iter() {
                    self.declaration(declaration)?;
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
                        let value_to_insert = Self::eval_result_to_value(
                            &eval_expr(expr, &self.state.environment).map_err(|err| {
                                eprintln!("{}\n[line {}]", err.error, err.line);
                                RuntimeError::new(err.line, RuntimeErrorType::EvalError)
                            })?,
                        );
                        self.state.environment.set(
                            ident_token.lexeme.clone(),
                            value_to_insert,
                            true,
                        )?;
                    }
                    None => {
                        self.state
                            .environment
                            .set(ident_token.lexeme.clone(), None, true)?;
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
        }
    }

    fn maybe_assign(&mut self, eval_result: &EvalResult) -> Result<(), RuntimeError> {
        if let EvalResult::Assign(var_token, eval_result) = eval_result {
            self.state.environment.set(
                var_token.lexeme.clone(),
                Self::eval_result_to_value(eval_result),
                false,
            )?;
            return self.maybe_assign(eval_result);
        }
        Ok(())
    }
}
