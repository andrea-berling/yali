use std::{cell::RefCell, collections::HashMap, iter::zip, rc::Rc};

use std::fmt::Debug;

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

#[derive(Default)]
pub struct Environment {
    parent: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, Value>,
}

impl Debug for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Environment")
            .field(
                "parent_addr",
                &self.parent.as_ref().map(|this| this.as_ptr()),
            )
            .field("parent", &self.parent)
            .field("values", &self.values)
            .finish()
    }
}

pub struct Interpreter {
    pub program: Program,
    pub current_environment: Rc<RefCell<Environment>>,
}

impl Environment {
    pub fn new_with_builtin_functions(builtins: &[&str]) -> Self {
        Self {
            values: HashMap::from_iter(builtins.iter().map(|function| {
                (
                    function.to_string(),
                    Value::Fn {
                        name: function.to_string(),
                        formal_args: vec![],
                        body: Statement::Block(vec![]),
                        environment: Default::default(),
                    },
                )
            })),
            parent: Default::default(),
        }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(value) = self.values.get(name) {
            Some(value.clone())
        } else if let Some(environment) = self.parent.as_ref() {
            environment.borrow().get(name)
        } else {
            None
        }
    }

    pub fn set(&mut self, name: &str, new_value: Value, new: bool) -> bool {
        if new {
            self.values.insert(name.to_string(), new_value);
        } else if let Some(old_value) = self.values.get_mut(name) {
            *old_value = new_value;
        } else if let Some(environment) = &self.parent {
            environment.borrow_mut().set(name, new_value, new);
        } else {
            return false;
        }
        true
    }
}

impl Interpreter {
    pub fn new(program: Program) -> Self {
        Self {
            program,
            current_environment: Rc::new(RefCell::new(Environment::new_with_builtin_functions(&[
                "clock",
            ]))),
        }
    }

    pub fn get_var(&self, name: &str) -> Option<Value> {
        self.current_environment.borrow().get(name)
    }

    // When adding the environment, we assume the caller owns the environment
    pub fn add_environment(&mut self, mut environment: Environment) {
        environment.parent = Some(self.current_environment.clone());
        self.current_environment = Rc::new(RefCell::new(environment));
    }

    pub fn set_environment(&mut self, environment: Rc<RefCell<Environment>>) {
        self.current_environment = environment
    }

    // When giving back the environment, we can't assume the one we have this is the only reference
    // to it (there may be other closures pointing to it)
    pub fn pop_environment(&mut self) -> Rc<RefCell<Environment>> {
        let return_value = self.current_environment.clone();
        let new_env = self.current_environment.borrow().parent.clone().unwrap();
        self.current_environment = new_env;
        return_value
    }

    pub fn set_var(&mut self, name: &str, value: Value, new: bool) -> bool {
        self.current_environment.borrow_mut().set(name, value, new)
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
                self.add_environment(Environment::default());
                for declaration in declarations.iter() {
                    self.declaration(declaration)?;
                }
                self.pop_environment();
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
            Statement::Return(token, expr) => Err(RuntimeError::EarlyExit(Box::new(
                eval_expr(
                    expr.as_ref()
                        .unwrap_or(&Expr::Literal(crate::parser::LiteralExpr::Nil)),
                    self,
                )
                .map_err(|mut err| {
                    err.set_token(token);
                    err
                })?,
            ))),
        }
    }

    pub fn declaration(&mut self, declaration: &Declaration) -> Result<(), RuntimeError> {
        match declaration {
            Declaration::Var(ident_token, expr) => {
                match expr {
                    Some(expr) => {
                        let value_to_insert = eval_expr(expr, self)?;
                        self.set_var(&ident_token.lexeme, value_to_insert, true);
                    }
                    None => {
                        self.set_var(&ident_token.lexeme, Value::Nil, true);
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
                    },
                    true,
                );
                Ok(())
            }
        }
    }

    pub fn run(&mut self) -> Result<(), RuntimeError> {
        self.statement(&self.program.clone())
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
            environment,
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
        let Statement::Block(_) = body.clone() else {
            todo!();
        };

        let old_environment = self.current_environment.clone();
        self.set_environment(environment);
        let mut new_env = Environment::default();
        for (var_token, var_value) in zip(formal_args.clone(), call_args) {
            new_env.set(&var_token.lexeme, var_value, true);
        }
        self.add_environment(new_env);

        let env_before_running = self.current_environment.clone();
        let mut return_value = Value::Nil;
        match self.statement(&body) {
            Err(RuntimeError::EvalError(e)) => {
                return Err(e);
            }
            Err(RuntimeError::EarlyExit(val)) => {
                self.current_environment = env_before_running;
                return_value = *val;
            }
            _ => {}
        };

        self.pop_environment();
        let fn_environment = self.pop_environment();
        self.current_environment = old_environment;
        if let Expr::Var(t) = function_expr {
            self.set_var(
                &t.lexeme,
                Value::Fn {
                    environment: fn_environment,
                    name,
                    formal_args,
                    body,
                },
                false,
            );
        }

        Ok(return_value)
    }
}
