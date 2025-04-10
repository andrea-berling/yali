use std::fmt::Display;

pub const KEYWORDS: [&str; 16] = [
    "and", "class", "else", "false", "for", "fun", "if", "nil", "or", "print", "return", "super",
    "this", "true", "var", "while",
];

#[derive(Debug, Clone)]
#[allow(non_camel_case_types)]
pub enum TokenType {
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,
    EQUAL,
    EQUAL_EQUAL,
    BANG,
    BANG_EQUAL,
    LESS,
    LESS_EQUAL,
    GREATER,
    GREATER_EQUAL,
    STRING,
    NUMBER,
    IDENTIFIER,
    EOF,
    KEYWORD(String),
}

#[derive(Clone)]
pub struct Token(pub TokenType, pub String, pub Option<Literal>);

pub const EOF_TOKEN: Token = Token(TokenType::EOF, String::new(), None);

#[non_exhaustive]
#[derive(Clone, Debug)]
pub enum Literal {
    Number(f64),
    String(String),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Number(n) => {
                if n.fract() == 0.0 {
                    write!(f, "{n}.0")
                } else {
                    write!(f, "{n}")
                }
            }
            Literal::String(s) => write!(f, "{s}"),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} {}",
            if let TokenType::KEYWORD(keyword) = &self.0 {
                keyword.to_uppercase()
            } else {
                format!("{:?}", self.0)
            },
            self.1,
            match self.2 {
                Some(ref l) => format!("{l}"),
                None => "null".to_string(),
            }
        )
    }
}

#[derive(Debug)]
pub enum ScanningError {
    UnexpectedCharacter(char),
    UnterminatedString,
    InvalidNumberLiteral,
}

impl Display for ScanningError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ScanningError::UnexpectedCharacter(c) => write!(f, "Unexpected character: {}", c),
            ScanningError::UnterminatedString => write!(f, "Unterminated string."),
            ScanningError::InvalidNumberLiteral => write!(f, "Invalid number literal."),
        }
    }
}

impl TryFrom<char> for Token {
    type Error = ScanningError;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '(' => Ok(Token(TokenType::LEFT_PAREN, value.to_string(), None)),
            ')' => Ok(Token(TokenType::RIGHT_PAREN, value.to_string(), None)),
            '{' => Ok(Token(TokenType::LEFT_BRACE, value.to_string(), None)),
            '}' => Ok(Token(TokenType::RIGHT_BRACE, value.to_string(), None)),
            ',' => Ok(Token(TokenType::COMMA, value.to_string(), None)),
            '.' => Ok(Token(TokenType::DOT, value.to_string(), None)),
            '-' => Ok(Token(TokenType::MINUS, value.to_string(), None)),
            '+' => Ok(Token(TokenType::PLUS, value.to_string(), None)),
            ';' => Ok(Token(TokenType::SEMICOLON, value.to_string(), None)),
            '/' => Ok(Token(TokenType::SLASH, value.to_string(), None)),
            '*' => Ok(Token(TokenType::STAR, value.to_string(), None)),
            '=' => Ok(Token(TokenType::EQUAL, value.to_string(), None)),
            '!' => Ok(Token(TokenType::BANG, value.to_string(), None)),
            '<' => Ok(Token(TokenType::LESS, value.to_string(), None)),
            '>' => Ok(Token(TokenType::GREATER, value.to_string(), None)),
            _ => Err(ScanningError::UnexpectedCharacter(value)),
        }
    }
}

pub fn tokenize(input: &str) -> (Vec<(usize, Token)>, Vec<(usize, ScanningError)>) {
    let mut tokens = vec![];
    let mut errors = vec![];
    let mut current_line = 1usize;
    let mut input_iterator = input.chars().peekable();
    while let Some(c) = input_iterator.next() {
        if c.is_whitespace() {
            if c == '\n' {
                current_line += 1
            }
            continue;
        }

        if c == '/' && input_iterator.next_if(|c| *c == '/').is_some() {
            while input_iterator.next_if(|c| *c != '\n').is_some() {}
            continue;
        }

        if c == '=' && input_iterator.next_if(|c| *c == '=').is_some() {
            tokens.push((
                current_line,
                Token(TokenType::EQUAL_EQUAL, "==".to_string(), None),
            ));
            continue;
        }

        if c == '!' && input_iterator.next_if(|c| *c == '=').is_some() {
            tokens.push((
                current_line,
                Token(TokenType::BANG_EQUAL, "!=".to_string(), None),
            ));
            continue;
        }

        if c == '<' && input_iterator.next_if(|c| *c == '=').is_some() {
            tokens.push((
                current_line,
                Token(TokenType::LESS_EQUAL, "<=".to_string(), None),
            ));
            continue;
        }

        if c == '>' && input_iterator.next_if(|c| *c == '=').is_some() {
            tokens.push((
                current_line,
                Token(TokenType::GREATER_EQUAL, ">=".to_string(), None),
            ));
            continue;
        }

        if c == '"' {
            match scan_string_literal(&mut input_iterator) {
                Ok(t) => tokens.push((current_line, t)),
                Err(e) => errors.push((current_line, e)),
            }
            continue;
        }

        if c.is_ascii_digit() {
            match scan_number_literal(&mut input_iterator, c) {
                Ok(t) => tokens.push((current_line, t)),
                Err(e) => errors.push((current_line, e)),
            }
            continue;
        }

        if c.is_ascii_alphabetic() || c == '_' {
            match scan_identifier_or_keyword(&mut input_iterator, c) {
                Ok(t) => tokens.push((current_line, t)),
                Err(e) => errors.push((current_line, e)),
            }
            continue;
        }

        match Token::try_from(c) {
            Ok(t) => tokens.push((current_line, t)),
            Err(err) => {
                errors.push((current_line, err));
            }
        }
    }
    (tokens, errors)
}

fn scan_string_literal(
    input_iterator: &mut std::iter::Peekable<std::str::Chars<'_>>,
) -> Result<Token, ScanningError> {
    let mut string_literal = String::new();
    let mut terminated = false;
    while let Some(c) = input_iterator.next() {
        if c == '"' {
            terminated = true;
            break;
        }
        string_literal.push(c);
    }
    if input_iterator.peek().is_none() && !terminated {
        Err(ScanningError::UnterminatedString)
    } else {
        Ok(Token(
            TokenType::STRING,
            format!("\"{string_literal}\""),
            Some(Literal::String(string_literal)),
        ))
    }
}

fn scan_number_literal(
    input_iterator: &mut std::iter::Peekable<std::str::Chars<'_>>,
    first_digit: char,
) -> Result<Token, ScanningError> {
    let mut number_literal = first_digit.to_string();
    while let Some(c) = input_iterator.next_if(|&c| c.is_ascii_digit() || c == '.') {
        number_literal.push(c);
    }
    if let Ok(n) = number_literal.parse::<f64>() {
        Ok(Token(
            TokenType::NUMBER,
            number_literal,
            Some(Literal::Number(n)),
        ))
    } else {
        Err(ScanningError::InvalidNumberLiteral)
    }
}

fn scan_identifier_or_keyword(
    input_iterator: &mut std::iter::Peekable<std::str::Chars<'_>>,
    first_char: char,
) -> Result<Token, ScanningError> {
    let mut identifier = first_char.to_string();
    while let Some(c) = input_iterator.next_if(|&c| c.is_ascii_alphanumeric() || c == '_') {
        identifier.push(c);
    }
    if KEYWORDS.contains(&identifier.as_str()) {
        Ok(Token(
            TokenType::KEYWORD(identifier.clone()),
            identifier,
            None,
        ))
    } else {
        Ok(Token(TokenType::IDENTIFIER, identifier, None))
    }
}
