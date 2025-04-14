use crate::{
    eval::{eval_expr, EvalError},
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

    pub fn statement(&self, statement: &Statement) -> Result<(), RuntimeError> {
        match statement {
            Statement::Expr(expr) => {
                if let Err(err) = eval_expr(expr, &Some(self.state.variables.clone())) {
                    eprintln!("{}\n[line {}]", err.error, err.line);
                    return Err(RuntimeError::new(err.line, RuntimeErrorType::EvalError));
                }
                Ok(())
            }
            Statement::Print(expr) => match eval_expr(expr, &Some(self.state.variables.clone())) {
                Err(EvalError { line, error }) => {
                    eprintln!("{}\n[line {}]", error, line);
                    Err(RuntimeError::new(line, RuntimeErrorType::EvalError))
                }
                Ok(result) => {
                    println!("{}", result);
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
                        self.state.variables.insert(
                            ident_token.lexeme.clone(),
                            match eval_expr(expr, &Some(self.state.variables.clone())).map_err(
                                |err| RuntimeError::new(err.line, RuntimeErrorType::EvalError),
                            )? {
                                crate::eval::EvalResult::Number(n) => Some(Value::Number(n)),
                                crate::eval::EvalResult::String(s) => {
                                    Some(Value::String(s.clone()))
                                }
                                crate::eval::EvalResult::Bool(b) => Some(Value::Boolean(b)),
                                crate::eval::EvalResult::Nil => None,
                            },
                        );
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
            self.declaration(&declaration)?;
        }
        Ok(())
    }
}
