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
    EarlyExit(Box<Value>),
}

type Scope = (usize, std::collections::HashMap<String, Value>);

#[derive(Debug, Clone)]
pub struct Environment(VecDeque<Scope>);

pub struct State {
    pub environment: Environment,
}

pub struct Interpreter {
    pub program: Program,
    pub state: State,
    pub scope_index: usize,
}

impl Environment {
    pub fn new() -> Self {
        // TODO: Clean this crap up
        Self(VecDeque::from(vec![(
            0,
            std::collections::HashMap::from([(
                "clock".to_string(),
                Value::Fn {
                    name: "clock".to_string(),
                    formal_args: vec![],
                    body: vec![],
                    environment: Self(VecDeque::from(vec![(0, HashMap::new())])),
                },
            )]),
        )]))
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        for (_, scope) in self.0.iter() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }
        None
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut Value> {
        for (_, scope) in self.0.iter_mut() {
            if let Some(value) = scope.get_mut(name) {
                return Some(value);
            }
        }
        None
    }

    pub fn new_scope(&mut self, scope_id: usize) {
        self.0
            .push_front((scope_id, std::collections::HashMap::new()));
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

    pub fn pop_scopes(&mut self, n: usize) -> Vec<Scope> {
        let mut result = vec![];
        for _ in 0..n {
            result.push(self.0.pop_front().unwrap());
        }
        result.reverse();
        result
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
            latest_scope.1.insert(name.to_string(), value);
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
        Self(self.0.clone())
    }
}

impl Interpreter {
    pub fn new(program: Program) -> Self {
        Self {
            program,
            state: State {
                environment: Environment::new(),
            },
            scope_index: 0,
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
                self.scope_index += 1;
                self.state.environment.new_scope(self.scope_index);
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
            Statement::Return(_, expr) => Err(RuntimeError::EarlyExit(Box::new(eval_expr(
                expr.as_ref()
                    .unwrap_or(&Expr::Literal(crate::parser::LiteralExpr::Nil)),
                self,
            )?))),
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
                    Value::Fn {
                        name: name.lexeme.clone(),
                        formal_args: args.clone(),
                        body: body.clone(),
                        environment: self.state.environment.take_snapshot(),
                    },
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
        let Value::Fn {
            name,
            formal_args,
            body,
            environment: fn_environment,
            ..
        } = eval_expr(function_expr, self)?
        else {
            return eval_error(token, EvalErrorType::UndefinedFunction)?;
        };
        if formal_args.len() != call_args.len() {
            return eval_error(
                token,
                EvalErrorType::WrongArgumentNumber(formal_args.len(), call_args.len()),
            )?;
        }
        let n_scopes = fn_environment.0.len();
        let n_scopes_to_keep = zip(
            fn_environment.0.iter().rev(),
            self.state.environment.0.iter().rev(),
        )
        .take_while(|(x, y)| x.0 == y.0)
        .count();
        let old_scopes = self
            .state
            .environment
            .pop_scopes(self.state.environment.0.len() - n_scopes_to_keep);
        self.state.environment.add_scopes(
            fn_environment.0.clone().make_contiguous()[n_scopes_to_keep..]
                .iter()
                .cloned()
                .rev()
                .collect::<Vec<_>>()
                .as_slice(),
        );
        self.scope_index += 1;
        self.state.environment.new_scope(self.scope_index);
        for (var_token, var_value) in zip(formal_args, call_args) {
            let (_, latest_scope) = self.state.environment.0.front_mut().unwrap();
            latest_scope.insert(var_token.lexeme.clone(), var_value);
        }
        let mut return_value = Value::Nil;
        match self.statement(&Statement::Block(body)) {
            Err(RuntimeError::EarlyExit(val)) => return_value = *val,
            Err(RuntimeError::EvalError(e)) => {
                return Err(e);
            }
            _ => {}
        };
        self.state
            .environment
            .drop_scopes(n_scopes - n_scopes_to_keep + 1);
        self.state.environment.add_scopes(&old_scopes);
        Ok(return_value)
    }
}
