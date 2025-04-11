use crate::{
    eval::{eval_expr, EvalError},
    parser::{Program, Statement},
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

pub fn run_statement(statement: &Statement) -> Result<(), RuntimeError> {
    match statement {
        Statement::Expr(expr) => {
            if let Err(err) = eval_expr(expr) {
                eprintln!("{}\n[line {}]", err.error, err.line);
                return Err(RuntimeError::new(err.line, RuntimeErrorType::EvalError));
            }
            Ok(())
        }
        Statement::Print(expr) => match eval_expr(expr) {
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

pub fn run_program(program: &Program) -> Result<(), RuntimeError> {
    for statement in program.iter() {
        run_statement(statement)?;
    }
    Ok(())
}
