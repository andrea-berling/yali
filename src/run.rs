use crate::{
    eval::{eval_expr, EvalError},
    parser::{Program, Statement},
};

pub fn run_statement(statement: &Statement) {
    match statement {
        Statement::Expr(expr) => todo!(),
        Statement::Print(expr) => match eval_expr(expr) {
            Err(EvalError { line, error }) => {
                eprintln!("[line {}] Error: {}", line, error);
            }
            Ok(result) => {
                println!("{}", result);
            }
        },
    }
}

pub fn run_program(program: &Program) {
    for statement in program.iter() {
        run_statement(statement);
    }
}
