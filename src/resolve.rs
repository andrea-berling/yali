use crate::{
    lexer::Token,
    parser::{Declaration, Expr, Program, Statement},
};
use std::collections::{HashMap, HashSet, VecDeque};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ResolvingErrorType {
    #[error("Invalid program")]
    InvalidProgram,
    #[error("Name was already bound in this scope")]
    NameRebound,
    #[error("The referenced name is still being defined")]
    InconsistentReference,
    #[error("Return statements are only allowed within function bodies")]
    InvalidReturn,
    #[error("Can't use 'this' outside of a class")]
    UseOfThisOutsideOfClass,
}

use ResolvingErrorType::*;

pub fn resolving_error<T>(token: &Token, error: ResolvingErrorType) -> Result<T, ResolvingError> {
    Err(ResolvingError {
        token: token.clone(),
        error,
    })
}

#[derive(Error, Debug)]
#[error("Error at {}: {error}.\n[line {}]",if !token.lexeme.is_empty() {format!("'{}'",&token.lexeme)} else {"end".to_string()}, token.line)]
pub struct ResolvingError {
    token: Token,
    error: ResolvingErrorType,
}

type Scope = HashMap<String, bool>;

pub struct Resolver {
    resolved_expressions: HashMap<String, usize>,
    scopes: VecDeque<Scope>,
    in_function_scope: bool,
    in_class_scope: bool,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            resolved_expressions: HashMap::new(),
            scopes: VecDeque::new(),
            in_function_scope: false,
            in_class_scope: false,
        }
    }

    fn add_scope(&mut self) {
        self.scopes.push_front(HashMap::new());
    }

    fn pop_scope(&mut self) -> Scope {
        self.scopes.pop_front().unwrap_or_default()
    }

    pub fn resolve(mut self, program: &Program) -> Result<HashMap<String, usize>, ResolvingError> {
        let Statement::Block(declarations) = program else {
            return resolving_error(
                &Token {
                    token_type: todo!(),
                    lexeme: todo!(),
                    value: todo!(),
                    line: todo!(),
                },
                InvalidProgram,
            );
        };
        for declaration in declarations {
            self.resolve_declaration(declaration)?;
        }
        Ok(self.resolved_expressions)
    }

    fn resolve_name(&self, token: &Token) -> Option<(usize, bool)> {
        if self.scopes.is_empty() {
            return None;
        }
        for (i, scope) in self.scopes.iter().enumerate() {
            if let Some(defined) = scope.get(&token.lexeme) {
                return Some((i, *defined));
            }
        }
        None
    }

    fn name_is_declared_in_current_scope(&self, token: &Token) -> bool {
        if let Some(scope) = self.scopes.front() {
            scope.contains_key(&token.lexeme)
        } else {
            false
        }
    }

    fn declare(&mut self, token: &Token) {
        if let Some(scope) = self.scopes.front_mut() {
            scope.insert(token.lexeme.clone(), false);
        }
    }

    fn define(&mut self, token: &Token) {
        if let Some(scope) = self.scopes.front_mut() {
            scope.insert(token.lexeme.clone(), true);
        }
    }

    fn resolve_declaration(&mut self, declaration: &Declaration) -> Result<(), ResolvingError> {
        match declaration {
            Declaration::Var(token, expr) => {
                if self.name_is_declared_in_current_scope(token) {
                    return resolving_error(token, NameRebound);
                }
                self.declare(token);
                if let Some(expr) = expr {
                    self.resolve_expr(expr)?;
                }
                self.define(token);
            }
            Declaration::Statement(statement) => {
                self.resolve_statement(statement)?;
            }
            Declaration::Function(token, args, statement) => {
                if self.name_is_declared_in_current_scope(token) {
                    return resolving_error(token, NameRebound);
                }
                self.declare(token);
                let Statement::Block(body) = statement else {
                    todo!()
                };
                self.add_scope();
                let old_in_function_scope = self.in_function_scope;
                self.in_function_scope = true;
                let mut defined_args = HashSet::new();
                for token in args {
                    if defined_args.contains(&token.lexeme) {
                        return resolving_error(token, NameRebound);
                    }
                    defined_args.insert(token.lexeme.clone());
                    self.define(token);
                }
                for declaration in body {
                    self.resolve_declaration(declaration)?;
                }
                self.pop_scope();
                self.in_function_scope = old_in_function_scope;
                self.define(token);
            }
            Declaration::Class(token, statement) => {
                self.in_class_scope = true;
                self.define(token);
                let Statement::Block(body) = statement else {
                    todo!()
                };
                for declaration in body {
                    self.resolve_declaration(declaration)?
                }
                self.in_class_scope = false;
            }
        }
        Ok(())
    }

    fn resolve_expr(&mut self, expr: &Expr) -> Result<(), ResolvingError> {
        match expr {
            Expr::Literal(_) => {}
            Expr::Unary(_, expr) | Expr::Grouping(expr) => {
                self.resolve_expr(expr)?;
            }
            Expr::Assign(expr1, expr2)
            | Expr::Binary(expr1, _, expr2)
            | Expr::Logical(expr1, _, expr2) => {
                self.resolve_expr(expr1)?;
                self.resolve_expr(expr2)?;
            }
            Expr::Name(token, address) => match self.resolve_name(token) {
                Some((depth, true)) => {
                    self.resolved_expressions.insert(address.clone(), depth);
                }
                Some((_, false)) => {
                    return resolving_error(token, InconsistentReference);
                }
                None => {
                    //return resolving_error(token, UndefinedName); // Might be global
                }
            },
            Expr::Call(expr, _, exprs) => {
                self.resolve_expr(expr)?;
                for expr in exprs {
                    self.resolve_expr(expr)?;
                }
            }
            Expr::Dotted(_, left, right) => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
            }
            Expr::This(token) => {
                if !self.in_class_scope {
                    return resolving_error(token, UseOfThisOutsideOfClass);
                }
            }
        }
        Ok(())
    }

    fn resolve_statement(&mut self, statement: &Statement) -> Result<(), ResolvingError> {
        match statement {
            Statement::Expr(expr) | Statement::Print(expr) => {
                self.resolve_expr(expr)?;
            }
            Statement::Block(declarations) => {
                self.add_scope();
                for declaration in declarations {
                    self.resolve_declaration(declaration)?;
                }
                self.pop_scope();
            }
            Statement::If(condition, then_body, else_body) => {
                self.resolve_expr(condition)?;
                self.resolve_statement(then_body)?;
                if let Some(else_body) = else_body {
                    self.resolve_statement(else_body)?;
                }
            }
            Statement::While(condition, body) => {
                self.resolve_expr(condition)?;
                self.resolve_statement(body)?;
            }
            Statement::Return(t, expr) => {
                if !self.in_function_scope {
                    return resolving_error(t, InvalidReturn);
                }
                if let Some(expr) = expr {
                    self.resolve_expr(expr)?;
                }
            }
        }
        Ok(())
    }
}
