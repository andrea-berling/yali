use const_str::convert_ascii_case;
use std::fmt::Display;
use thiserror::Error;

pub const KEYWORDS: [&str; 16] = [
    "and", "class", "else", "false", "for", "fun", "if", "nil", "or", "print", "return", "super",
    "this", "true", "var", "while",
];

macro_rules! display_as_shouty_snake {
    ($type:ty, $( $variant:ident ),* $(,)? $(- $($variant_to_display_as_is:path,$parameter:ident),*)? ) => {
        impl std::fmt::Display for $type {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $( <$type>::$variant => write!(f, "{}", convert_ascii_case!(shouty_snake,stringify!($variant))), )*
                    $($($variant_to_display_as_is($parameter) => write!(f, "{}", $parameter)),*)?
                }
            }
        }
    };
}

macro_rules! define_single_char_token_types {
    ( $( $variant:ident, $char:literal ),* $(,)? ) => {
        #[derive(Debug, PartialEq,Clone)]
        pub enum SingleCharTokenType {
            $( $variant ),*
        }

        display_as_shouty_snake!(SingleCharTokenType, $( $variant),*);

        pub enum SingleTokenConversionError {
            UnrecognizedCharacter,
        }

        impl TryFrom<char> for SingleCharTokenType {
            type Error = SingleTokenConversionError;
            fn try_from(value: char) -> Result<Self, Self::Error> {
                match value {
                    $( $char => Ok(SingleCharTokenType::$variant), )*
                    _ => Err(SingleTokenConversionError::UnrecognizedCharacter),
                }
            }
        }
    }
}

macro_rules! define_two_chars_token_types {
    ( $( $variant:ident ),* $(,)? ) => {
        #[derive(Debug, PartialEq, Clone)]
        pub enum TwoCharsTokenType {
            $( $variant ),*
        }


        display_as_shouty_snake!(TwoCharsTokenType, $( $variant),*);
    }
}

define_single_char_token_types! {
    LeftParen, '(',
    RightParen, ')',
    LeftBrace, '{',
    RightBrace, '}',
    Comma, ',',
    Dot, '.',
    Minus, '-',
    Plus, '+',
    Semicolon, ';',
    Slash, '/',
    Star, '*',
    Equal, '=',
    Bang,'!',
    Less, '<',
    Greater, '>',
}

define_two_chars_token_types! {
    EqualEqual,
    BangEqual,
    LessEqual,
    GreaterEqual,
}

#[derive(Debug, Clone)]
pub enum TokenType {
    SingleChar(SingleCharTokenType),
    TwoChars(TwoCharsTokenType),
    String,
    Number,
    Identifier,
    Keyword,
    Eof,
}

display_as_shouty_snake! {
    TokenType,
    String,
    Number,
    Identifier,
    Keyword,
    Eof,
    -
    TokenType::SingleChar,t,
    TokenType::TwoChars,t
}

#[derive(Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub value: Option<Literal>,
    pub line: usize,
}

impl Token {
    pub const fn new(
        token_type: TokenType,
        lexeme: String,
        value: Option<Literal>,
        line: usize,
    ) -> Self {
        Token {
            token_type,
            lexeme,
            value,
            line,
        }
    }
}

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
            if let TokenType::Keyword = &self.token_type {
                self.lexeme.to_uppercase()
            } else {
                self.token_type.to_string()
            },
            self.lexeme,
            match self.value {
                Some(ref l) => format!("{l}"),
                None => "null".to_string(),
            }
        )
    }
}

#[derive(Debug)]
pub struct ScanningError {
    pub line: usize,
    pub error: ScanningErrorType,
}

#[derive(Error, Debug)]
pub enum ScanningErrorType {
    #[error("Unexpected character: {0}")]
    UnexpectedCharacter(char),
    #[error("Unterminated string.")]
    UnterminatedString,
    #[error("Invalid number literal.")]
    InvalidNumberLiteral,
}

pub fn tokenize(input: &str) -> (Vec<Token>, Vec<ScanningError>) {
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
            tokens.push(Token::new(
                TokenType::TwoChars(TwoCharsTokenType::EqualEqual),
                "==".to_string(),
                None,
                current_line,
            ));
            continue;
        }

        if c == '!' && input_iterator.next_if(|c| *c == '=').is_some() {
            tokens.push(Token::new(
                TokenType::TwoChars(TwoCharsTokenType::BangEqual),
                "!=".to_string(),
                None,
                current_line,
            ));
            continue;
        }

        if c == '<' && input_iterator.next_if(|c| *c == '=').is_some() {
            tokens.push(Token::new(
                TokenType::TwoChars(TwoCharsTokenType::LessEqual),
                "<=".to_string(),
                None,
                current_line,
            ));
            continue;
        }

        if c == '>' && input_iterator.next_if(|c| *c == '=').is_some() {
            tokens.push(Token::new(
                TokenType::TwoChars(TwoCharsTokenType::GreaterEqual),
                ">=".to_string(),
                None,
                current_line,
            ));
            continue;
        }

        let mut next_scanner: Option<
            fn(
                &mut std::iter::Peekable<std::str::Chars<'_>>,
                first_char: char,
                current_line: usize,
            ) -> Result<Token, ScanningError>,
        > = None;

        if c == '"' {
            next_scanner = Some(scan_string_literal);
        }

        if c.is_ascii_digit() {
            next_scanner = Some(scan_number_literal);
        }

        if c.is_ascii_alphabetic() || c == '_' {
            next_scanner = Some(scan_identifier_or_keyword);
        }

        if let Some(scanner) = next_scanner {
            match scanner(&mut input_iterator, c, current_line) {
                Ok(t) => tokens.push(t),
                Err(e) => errors.push(e),
            }
            continue;
        }

        match SingleCharTokenType::try_from(c) {
            Ok(token_type) => tokens.push(Token {
                token_type: TokenType::SingleChar(token_type),
                lexeme: c.to_string(),
                value: None,
                line: current_line,
            }),
            Err(_) => {
                errors.push(ScanningError {
                    line: current_line,
                    error: ScanningErrorType::UnexpectedCharacter(c),
                });
            }
        }
    }
    tokens.push(Token::new(
        TokenType::Eof,
        String::new(),
        None,
        current_line,
    ));
    (tokens, errors)
}

fn scan_string_literal(
    input_iterator: &mut std::iter::Peekable<std::str::Chars<'_>>,
    first_char: char,
    current_line: usize,
) -> Result<Token, ScanningError> {
    let mut quoted_string_literal = first_char.to_string();
    let mut terminated = false;
    while let Some(c) = input_iterator.next() {
        quoted_string_literal.push(c);
        if c == '"' {
            terminated = true;
            break;
        }
    }
    if input_iterator.peek().is_none() && !terminated {
        Err(ScanningError {
            line: current_line,
            error: ScanningErrorType::UnterminatedString,
        })
    } else {
        Ok(Token::new(
            TokenType::String,
            quoted_string_literal.to_string(),
            Some(Literal::String(
                quoted_string_literal[1..quoted_string_literal.len() - 1].to_string(),
            )),
            current_line,
        ))
    }
}

fn scan_number_literal(
    input_iterator: &mut std::iter::Peekable<std::str::Chars<'_>>,
    first_char: char,
    current_line: usize,
) -> Result<Token, ScanningError> {
    let mut number_literal = first_char.to_string();
    while let Some(c) = input_iterator.next_if(|&c| c.is_ascii_digit() || c == '.') {
        number_literal.push(c);
    }
    if let Ok(n) = number_literal.parse::<f64>() {
        Ok(Token::new(
            TokenType::Number,
            number_literal,
            Some(Literal::Number(n)),
            current_line,
        ))
    } else {
        Err(ScanningError {
            line: current_line,
            error: ScanningErrorType::InvalidNumberLiteral,
        })
    }
}

fn scan_identifier_or_keyword(
    input_iterator: &mut std::iter::Peekable<std::str::Chars<'_>>,
    first_char: char,
    current_line: usize,
) -> Result<Token, ScanningError> {
    let mut identifier = first_char.to_string();
    while let Some(c) = input_iterator.next_if(|&c| c.is_ascii_alphanumeric() || c == '_') {
        identifier.push(c);
    }
    if KEYWORDS.contains(&identifier.as_str()) {
        Ok(Token::new(
            TokenType::Keyword,
            identifier,
            None,
            current_line,
        ))
    } else {
        Ok(Token::new(
            TokenType::Identifier,
            identifier,
            None,
            current_line,
        ))
    }
}
