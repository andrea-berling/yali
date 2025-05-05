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
    EarlyExit(Rc<RefCell<Value>>),
}

#[derive(Default)]
pub struct Environment {
    parent: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, Rc<RefCell<Value>>>,
    // TODO: just make it a value in the env
    this: Option<Rc<RefCell<Value>>>,
}

impl Debug for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Environment")
            .field("values", &self.values.keys())
            .field(
                "parent_addr",
                &self.parent.as_ref().map(|this| this.as_ptr()),
            )
            .field("parent", &self.parent)
            .finish()
    }
}

pub enum Callable {
    Class(Rc<RefCell<Value>>),
    Method(Rc<RefCell<Value>>),
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
                    this: None,
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

    fn get_var(&self, name: &str) -> Option<Rc<RefCell<Value>>> {
        self.current_environment.borrow().get(name)
    }

    pub fn assign_var(&mut self, token: &Token, address: &str, value: Rc<RefCell<Value>>) -> bool {
        let depth = match self.resolution_table.get(address) {
            Some(depth) => *depth,
            None => usize::MAX,
        };
        self.set_var_at_depth(&token.lexeme, value, depth)
    }

    pub fn statement(&mut self, statement: &Statement) -> Result<(), RuntimeError> {
        match statement {
            Statement::Expr(expr) => {
                eval_expr(expr, self)?;
                Ok(())
            }
            Statement::Print(expr) => {
                println!("{}", eval_expr(expr, self)?.borrow());
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
                    &*eval_expr(condition, self)?.borrow(),
                    Value::Bool(false) | Value::Nil
                ) {
                    self.statement(if_statement)?;
                } else if let Some(else_statement) = else_statement {
                    self.statement(else_statement)?;
                }
                Ok(())
            }
            Statement::While(while_condition, statement) => {
                let mut condition = eval_expr(while_condition, self)?;
                while !matches!(&*condition.borrow(), Value::Bool(false) | Value::Nil) {
                    self.statement(statement)?;
                    condition = eval_expr(while_condition, self)?;
                }
                Ok(())
            }
            Statement::Return(token, expr) => Err(RuntimeError::EarlyExit(
                eval_expr(
                    expr.as_ref()
                        .unwrap_or(&Expr::Literal(crate::parser::LiteralExpr::Nil)),
                    self,
                )
                .map_err(|mut err| {
                    err.set_token(token);
                    err
                })?,
            )),
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
                        this: None,
                    }
                    .into(),
                    true,
                );
                Ok(())
            }
            Declaration::Class(name, superclass, body) => {
                let superclass = if let Some(superclass) = superclass {
                    Some(eval_expr(superclass, self)?.clone())
                } else {
                    None
                };
                let mut methods = HashMap::new();
                let Statement::Block(body) = body else {
                    todo!();
                };
                for declaration in body {
                    if let Declaration::Function(name, args, body) = declaration {
                        methods.insert(
                            name.lexeme.clone(),
                            Value::Fn {
                                name: name.lexeme.clone(),
                                formal_args: args.clone(),
                                body: body.clone(),
                                environment: self.current_environment.clone(),
                                this: None,
                            }
                            .into(),
                        );
                    }
                }
                self.set_var(
                    &name.lexeme,
                    Value::Class {
                        name: name.lexeme.clone(),
                        superclass,
                        methods,
                    }
                    .into(),
                    true,
                );
                Ok(())
            }
        }
    }

    pub fn run(&mut self, program: &Program) -> Result<(), RuntimeError> {
        self.statement(program)
    }

    pub fn call(
        &mut self,
        callable: &Callable,
        call_args: &[(&Expr, Rc<RefCell<Value>>)],
        this: Option<Rc<RefCell<Value>>>,
        token: &Token,
    ) -> Result<Rc<RefCell<Value>>, EvalError> {
        let callee = match callable {
            Callable::Class(val) | Callable::Method(val) | Callable::Function(val)
                if matches!(&*val.borrow(), Value::Class { .. } | Value::Fn { .. }) =>
            {
                val.clone()
            }
            _ => todo!(),
        };
        let Value::Fn {
            name: callable_name,
            formal_args,
            body,
            environment,
            this: fn_this,
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
                        Some(class) => self
                            .call(&Callable::Class(class.clone()), call_args, None, token)?
                            .borrow()
                            .get_properties()
                            .unwrap()
                            .clone(),
                        None => HashMap::new(),
                    },
                }
                .into();

                if let Some(initializer) = methods.get("init") {
                    if matches!(&*initializer.borrow(), Value::Fn { .. }) {
                        self.call(
                            &Callable::Method(initializer.clone()),
                            call_args,
                            Some(new_instance.clone()),
                            token,
                        )?;
                    }
                }
                return Ok(new_instance);
            } else {
                return eval_error(token, EvalErrorType::UndefinedFunction)?;
            }
        };
        if formal_args.len() != call_args.len() {
            return eval_error(
                token,
                EvalErrorType::WrongArgumentNumber(formal_args.len(), call_args.len()),
            )?;
        }
        let Statement::Block(declarations) = body.clone() else {
            todo!();
        };

        let old_environment = self.current_environment.clone();
        if !matches!(callable, Callable::Method(_)) {
            self.set_environment(environment.clone());
        }
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
        if matches!(callable, Callable::Method(_)) {
            new_env.this = if let Some(this) = fn_this {
                Some(this.clone())
            } else {
                this
            }
        }
        self.add_environment(new_env);

        let env_before_running = self.current_environment.clone();
        let mut return_value = Value::Nil.into();
        for declaration in &declarations {
            match self.declaration(declaration) {
                Err(RuntimeError::EvalError(e)) => {
                    return Err(e);
                }
                Err(RuntimeError::EarlyExit(val)) => {
                    self.set_environment(env_before_running.clone());
                    return_value = val;
                    break;
                }
                _ => {}
            };
        }

        if matches!(callable, Callable::Method(_)) && callable_name == "init" {
            return_value = if let Some(this) = fn_this {
                this.clone()
            } else {
                self.current_environment.borrow().this.clone().unwrap()
            }
        }

        let mut values_to_update = vec![];

        for (expr, formal_arg) in zip(
            call_args
                .iter()
                .cloned()
                .unzip::<&Expr, Rc<RefCell<Value>>, Vec<_>, Vec<_>>()
                .0,
            formal_args.clone(),
        ) {
            if let Some(val) = self.get_var(&formal_arg.lexeme) {
                if matches!(&*val.borrow(), Value::ClassInstance { .. }) {
                    values_to_update.push((expr, val.clone()));
                }
            }
        }

        self.pop_environment();

        if !matches!(callable, Callable::Method(_)) {
            self.set_environment(old_environment);
        }

        for (expr, value) in values_to_update {
            if let Expr::Name(token, address) = expr {
                self.assign_var(token, address, value);
            } else if let Expr::Dotted(_, left, _) = expr {
                if let Expr::Name(token, address) = left.as_ref() {
                    self.assign_var(token, address, value);
                }
            }
        }

        Ok(return_value)
    }

    pub fn get_this(&self) -> Option<Rc<RefCell<Value>>> {
        let mut cur_env = self.current_environment.clone();
        let mut this = cur_env.borrow().this.clone();
        while this.is_none() && cur_env.borrow().parent.is_some() {
            let tmp = cur_env.borrow().parent.as_ref().unwrap().clone();
            this = tmp.borrow().this.clone();
            cur_env = tmp;
        }
        this
    }
}
