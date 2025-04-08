use std::env;
use std::fmt::Display;
use std::fs;

#[non_exhaustive]
#[derive(Debug, Clone)]
enum TokenType {
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
    EOF,
}

#[non_exhaustive]
#[derive(Clone)]
enum Literal {
    Number(f64),
    String(String),
}

#[derive(Clone)]
struct Token(TokenType, String, Option<Literal>);

const EOF_TOKEN: Token = Token(TokenType::EOF, String::new(), None);

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?} {} {}",
            self.0,
            self.1,
            match self.2 {
                Some(ref l) => match l {
                    Literal::Number(n) => n.to_string(),
                    Literal::String(s) => s.clone(),
                },
                None => "null".to_string(),
            }
        )
    }
}

#[derive(Debug)]
enum ScanningError {
    UnexpectedCharacter(char),
    UnexpectedString(String),
}

impl Display for ScanningError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ScanningError::UnexpectedCharacter(c) => write!(f, "Unexpected character: {}", c),
            ScanningError::UnexpectedString(s) => write!(f, "Unexpected string: {}", s),
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

fn tokenize(input: &str) -> (Vec<Token>, Vec<(usize, ScanningError)>) {
    let mut tokens = vec![];
    let mut errors = vec![];
    let mut current_line = 1;
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
            tokens.push(Token(TokenType::EQUAL_EQUAL, "==".to_string(), None));
            continue;
        }

        if c == '!' && input_iterator.next_if(|c| *c == '=').is_some() {
            tokens.push(Token(TokenType::BANG_EQUAL, "!=".to_string(), None));
            continue;
        }

        if c == '<' && input_iterator.next_if(|c| *c == '=').is_some() {
            tokens.push(Token(TokenType::LESS_EQUAL, "<=".to_string(), None));
            continue;
        }

        if c == '>' && input_iterator.next_if(|c| *c == '=').is_some() {
            tokens.push(Token(TokenType::GREATER_EQUAL, ">=".to_string(), None));
            continue;
        }

        match Token::try_from(c) {
            Ok(token) => tokens.push(token),
            Err(err) => {
                errors.push((current_line, err));
            }
        }
    }
    (tokens, errors)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        return;
    }

    let command = &args[1];
    let filename = &args[2];
    let mut exit_code = 0;

    match command.as_str() {
        "tokenize" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });

            // Uncomment this block to pass the first stage
            let (mut tokens, scanning_errors) = tokenize(&file_contents);
            tokens.push(EOF_TOKEN.clone());
            if !scanning_errors.is_empty() {
                exit_code = 65;
                for (line, err) in scanning_errors {
                    eprintln!("[line {}] Error: {}", line, err);
                }
            }
            for token in tokens {
                println!("{}", token);
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command);
        }
    }
    std::process::exit(exit_code)
}
