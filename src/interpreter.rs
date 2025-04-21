use std::{
    collections::{HashMap, VecDeque},
    iter::zip,
};

use thiserror::Error;

use crate::{
    eval::{eval_error, eval_expr, EvalError, EvalErrorType, Value},
    lexer::Token,
    parser::{Declaration, Expr, Program, Statement},
};

#[derive(Debug, Error)]
pub enum RuntimeError {
    #[error(transparent)]
    EvalError(#[from] EvalError),
    #[error("No error")]
    EarlyExit(Value),
}

type Scope = std::collections::HashMap<String, Value>;

#[derive(Debug, Clone)]
pub struct Environment(VecDeque<Scope>);

pub struct State {
    pub environment: Environment,
}

pub struct Interpreter {
    pub program: Program,
    pub state: State,
}

impl Environment {
    pub fn new() -> Self {
        // TODO: Clean this crap up
        Self(VecDeque::from(vec![std::collections::HashMap::from([(
            "clock".to_string(),
            Value::Fn(
                "clock".to_string(),
                vec![],
                vec![],
                Self(VecDeque::from(vec![HashMap::new()])),
            ),
        )])]))
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

    pub fn new_scope(&mut self) {
        self.0.push_front(std::collections::HashMap::new());
    }

    pub fn add_scopes(&mut self, scopes: &[Scope]) {
        for scope in scopes {
            self.0.push_front(scope.clone());
        }
    }

    pub fn drop_scopes(&mut self, n: usize) {
        for _ in 0..n {
            self.0.pop_front();
        }
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

    pub fn take_snapshot(&self) -> Self {
        let mut scopes = self.0.clone();
        scopes.pop_back();
        Self(scopes)
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
            Statement::Expr(expr) => {
                eval_expr(expr, self)?;
                Ok(())
            }
            Statement::Print(expr) => {
                println!("{}", &eval_expr(expr, self)?);
                Ok(())
            }
            Statement::Block(declarations) => {
                self.state.environment.new_scope();
                for declaration in declarations.iter() {
                    self.declaration(declaration)?;
                }
                self.state.environment.drop_scope();
                Ok(())
            }
            Statement::If(condition, if_statement, else_statement) => {
                let condition = &eval_expr(condition, self)?;
                if !matches!(condition, Value::Bool(false) | Value::Nil) {
                    self.statement(if_statement)?;
                } else if let Some(else_statement) = else_statement {
                    self.statement(else_statement)?;
                }
                Ok(())
            }
            Statement::While(while_condition, statement) => {
                let mut condition = eval_expr(while_condition, self)?;
                while !matches!(condition, Value::Bool(false) | Value::Nil) {
                    self.statement(statement)?;
                    condition = eval_expr(while_condition, self)?;
                }
                Ok(())
            }
            Statement::Return(_, expr) => Err(RuntimeError::EarlyExit(eval_expr(
                expr.as_ref()
                    .unwrap_or(&Expr::Literal(crate::parser::LiteralExpr::Nil)),
                self,
            )?)),
        }
    }

    pub fn declaration(&mut self, declaration: &Declaration) -> Result<(), RuntimeError> {
        match declaration {
            Declaration::Var(ident_token, expr) => {
                match expr {
                    Some(expr) => {
                        let value_to_insert = eval_expr(expr, self)?;
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
                self.state.environment.set(
                    &name.lexeme,
                    Value::Fn(
                        name.lexeme.clone(),
                        args.clone(),
                        body.clone(),
                        self.state.environment.take_snapshot(),
                    ),
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

    pub fn call_function(
        &mut self,
        function_expr: &Expr,
        call_args: Vec<Value>,
        token: &Token,
    ) -> Result<Value, EvalError> {
        let Value::Fn(_, parameters, body, environment) = eval_expr(function_expr, self)? else {
            return eval_error(token, EvalErrorType::UndefinedFunction)?;
        };
        if parameters.len() != call_args.len() {
            return eval_error(
                token,
                EvalErrorType::WrongArgumentNumber(parameters.len(), call_args.len()),
            )?;
        }
        let n_scopes = environment.0.len();
        self.state
            .environment
            .add_scopes(environment.0.into_iter().collect::<Vec<_>>().as_slice());
        self.state.environment.new_scope();
        for (var_token, var_value) in zip(parameters, call_args) {
            let latest_scope = self.state.environment.0.front_mut().unwrap();
            latest_scope.insert(var_token.lexeme.clone(), var_value);
        }
        let mut return_value = Value::Nil;
        for declaration in body {
            let val = self.declaration(&declaration);
            if let Err(RuntimeError::EarlyExit(val)) = val {
                return_value = val;
                break;
            }
            if let Err(RuntimeError::EvalError(err)) = val {
                return Err(err);
            }
        }
        self.state.environment.drop_scopes(n_scopes + 1);
        Ok(return_value)
    }
}
