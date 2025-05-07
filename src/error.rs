use crate::lexer::Token;

#[derive(thiserror::Error, Debug)]
#[error("[line {}] Error at {}: {error}.",
    token.as_ref().map_or(0,|token| token.line),
    if token.is_none() {"the start".into()} 
    else {
        let token = token.clone().unwrap();
        if token.lexeme.is_empty() {"end".into()}
        else { format!("'{}'",&token.lexeme)}
    }
)]
pub struct ErrorAtToken<ErrorType: std::error::Error> {
    token: Option<Token>,
    pub error: ErrorType,
}

impl<ErrorType: std::error::Error> ErrorAtToken<ErrorType> {
    pub fn new(token: &Token, error: ErrorType) -> Self {
        Self {
            token: Some(token.clone()),
            error,
        }
    }

    pub fn new_without_token(error: ErrorType) -> Self {
        Self { token: None, error }
    }
}
