use crate::{
    eval::{eval_expr, EvalError, EvalResult},
    parser::{Declaration, Program, Statement},
};

#[derive(Debug)]
pub enum RuntimeErrorType {
    EvalError,
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

pub type Environment = std::collections::HashMap<String, Option<Value>>;

pub struct State {
    pub variables: Environment,
}

pub struct Interpreter {
    pub program: Program,
    pub state: State,
}

impl Interpreter {
    pub fn new(program: Program) -> Self {
        Self {
            program,
            state: State {
                variables: std::collections::HashMap::new(),
            },
        }
    }

    pub fn statement(&mut self, statement: &Statement) -> Result<(), RuntimeError> {
        match statement {
            Statement::Expr(expr) => match eval_expr(expr, Some(&self.state.variables.clone())) {
                Ok(eval_result) => {
                    self.maybe_assign(&eval_result);
                    Ok(())
                }
                Err(err) => {
                    eprintln!("{}\n[line {}]", err.error, err.line);
                    Err(RuntimeError::new(err.line, RuntimeErrorType::EvalError))
                }
            },
            Statement::Print(expr) => match eval_expr(expr, Some(&self.state.variables.clone())) {
                Err(EvalError { line, error }) => {
                    eprintln!("{}\n[line {}]", error, line);
                    Err(RuntimeError::new(line, RuntimeErrorType::EvalError))
                }
                Ok(eval_result) => {
                    self.maybe_assign(&eval_result);
                    println!("{}", eval_result);
                    Ok(())
                }
            },
        }
    }

    pub fn declaration(&mut self, declaration: &Declaration) -> Result<(), RuntimeError> {
        match declaration {
            Declaration::Var(ident_token, expr) => {
                match expr {
                    Some(expr) => {
                        let value_to_insert = Self::eval_result_to_value(
                            &eval_expr(expr, Some(&self.state.variables)).map_err(|err| {
                                eprintln!("{}\n[line {}]", err.error, err.line);
                                RuntimeError::new(err.line, RuntimeErrorType::EvalError)
                            })?,
                        );
                        self.state
                            .variables
                            .insert(ident_token.lexeme.clone(), value_to_insert);
                    }
                    None => {
                        self.state
                            .variables
                            .insert(ident_token.lexeme.clone(), None);
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

    fn maybe_assign(&mut self, eval_result: &EvalResult) {
        if let EvalResult::Assign(var_token, eval_result) = eval_result {
            self.state.variables.insert(
                var_token.lexeme.clone(),
                Self::eval_result_to_value(eval_result),
            );
            self.maybe_assign(eval_result)
        }
    }
}
