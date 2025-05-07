use crate::{
    error::ErrorAtToken,
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
    #[error("Can't return a value from an initializer")]
    InvalidReturnInConstructor,
    #[error("Can't use 'super' outside of a class")]
    UseOfSuperOutsideOfClass,
    #[error("Can't use 'super' in a class with no superclass")]
    UseOfSuperOutsideOfSubclass,
    #[error("Expected a block for the body")]
    ExpectedBody,
}

use ResolvingErrorType::*;

type ResolvingError = ErrorAtToken<ResolvingErrorType>;
type ResolutionResult = Result<(), ResolvingError>;

type Scope = HashMap<String, bool>;

#[derive(Default, Clone, Copy)]
enum ContextType {
    #[default]
    None,
    Function,
    Method,
    Class,
    Constructor,
}

#[derive(Default, Clone, Copy)]
struct Context {
    context_type: ContextType,
    in_subclass: bool,
    in_dotted_expression: bool,
}

#[derive(Default)]
pub struct Resolver {
    resolved_expressions: HashMap<String, usize>,
    scopes: VecDeque<Scope>,
    globals: Scope,
    context: Context,
}

impl Resolver {
    fn add_scope(&mut self) {
        self.scopes.push_front(HashMap::new());
    }

    fn pop_scope(&mut self) -> Scope {
        self.scopes.pop_front().unwrap_or_default()
    }

    pub fn resolve(mut self, program: &Program) -> Result<HashMap<String, usize>, ResolvingError> {
        let Statement::Block(declarations) = program else {
            return Err(ResolvingError::new_without_token(InvalidProgram));
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
        } else {
            self.globals.entry(token.lexeme.clone()).or_insert(false);
        }
    }

    fn define(&mut self, token: &Token) {
        if let Some(scope) = self.scopes.front_mut() {
            scope.insert(token.lexeme.clone(), true);
        } else {
            self.globals.insert(token.lexeme.clone(), true);
        }
    }

    fn resolve_declaration(&mut self, declaration: &Declaration) -> ResolutionResult {
        match declaration {
            Declaration::Var(token, expr) => {
                if self.name_is_declared_in_current_scope(token) {
                    return Err(ResolvingError::new(token, NameRebound));
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
                    return Err(ResolvingError::new(token, NameRebound));
                }
                let previous_context = self.context;
                self.context.context_type = match self.context.context_type {
                    ContextType::None | ContextType::Function => ContextType::Function,
                    ContextType::Class => {
                        if token.lexeme == "init" {
                            ContextType::Constructor
                        } else {
                            ContextType::Method
                        }
                    }
                    ContextType::Constructor => ContextType::Method,
                    _ => self.context.context_type,
                };
                self.define(token);
                let Statement::Block(body) = statement else {
                    return Err(ResolvingError::new(token, ExpectedBody));
                };
                self.add_scope();
                let mut defined_args = HashSet::new();
                for token in args {
                    if defined_args.contains(&token.lexeme) {
                        return Err(ResolvingError::new(token, NameRebound));
                    }
                    defined_args.insert(token.lexeme.clone());
                    self.define(token);
                }
                for declaration in body {
                    self.resolve_declaration(declaration)?;
                }
                self.pop_scope();
                self.context = previous_context;
                self.define(token);
            }
            Declaration::Class(token, superclass, statement) => {
                let previous_context = self.context;
                self.context.context_type = ContextType::Class;
                if let Some(superclass) = superclass {
                    self.resolve_expr(superclass)?;
                    self.context.in_subclass = true;
                }
                self.define(token);
                let Statement::Block(body) = statement else {
                    return Err(ResolvingError::new(token, ExpectedBody));
                };
                for declaration in body {
                    self.resolve_declaration(declaration)?
                }
                self.context = previous_context;
            }
        }
        Ok(())
    }

    fn resolve_expr(&mut self, expr: &Expr) -> ResolutionResult {
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
            Expr::Name(token, address) => {
                if !self.context.in_dotted_expression {
                    if let Some((depth, defined)) = self.resolve_name(token) {
                        if defined {
                            self.resolved_expressions.insert(address.clone(), depth);
                        } else {
                            return Err(ResolvingError::new(token, InconsistentReference));
                        }
                    }
                }
            }
            Expr::Call(expr, _, exprs) => {
                self.resolve_expr(expr)?;
                let in_dotted_expression = self.context.in_dotted_expression;
                self.context.in_dotted_expression = false;
                for expr in exprs {
                    self.resolve_expr(expr)?;
                }
                self.context.in_dotted_expression = in_dotted_expression;
            }
            Expr::Dotted(_, left, right) => {
                self.resolve_expr(left)?;
                let in_dotted_expression = self.context.in_dotted_expression;
                self.context.in_dotted_expression = true;
                self.resolve_expr(right)?;
                self.context.in_dotted_expression = in_dotted_expression;
            }
            Expr::This(token) => {
                if !matches!(
                    self.context.context_type,
                    ContextType::Method | ContextType::Constructor,
                ) {
                    return Err(ResolvingError::new(token, UseOfThisOutsideOfClass));
                }
            }
            Expr::Super(token) => {
                if !matches!(
                    self.context.context_type,
                    ContextType::Method | ContextType::Constructor,
                ) {
                    return Err(ResolvingError::new(token, UseOfSuperOutsideOfClass));
                }
                if !self.context.in_subclass {
                    return Err(ResolvingError::new(token, UseOfSuperOutsideOfSubclass));
                }
            }
        }
        Ok(())
    }

    fn resolve_statement(&mut self, statement: &Statement) -> ResolutionResult {
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
                if !matches!(
                    self.context.context_type,
                    ContextType::Method | ContextType::Function | ContextType::Constructor
                ) {
                    return Err(ResolvingError::new(t, InvalidReturn));
                }
                if let Some(expr) = expr {
                    if matches!(self.context.context_type, ContextType::Constructor) {
                        return Err(ResolvingError::new(t, InvalidReturnInConstructor));
                    }
                    self.resolve_expr(expr)?;
                }
            }
        }
        Ok(())
    }
}
