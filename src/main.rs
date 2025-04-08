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
    UnrecognizedToken,
}

impl TryFrom<char> for Token {
    type Error = ScanningError;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '(' => Ok(Token(TokenType::LEFT_PAREN, value.to_string(), None)),
            ')' => Ok(Token(TokenType::RIGHT_PAREN, value.to_string(), None)),
            '{' => Ok(Token(TokenType::LEFT_BRACE, value.to_string(), None)),
            '}' => Ok(Token(TokenType::RIGHT_BRACE, value.to_string(), None)),
            _ => Err(ScanningError::UnrecognizedToken),
        }
    }
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = vec![];
    for c in input.chars() {
        if c.is_whitespace() {
            continue;
        }
        match Token::try_from(c) {
            Ok(t) => tokens.push(t),
            Err(e) => panic!("{:?}", e),
        }
    }
    tokens
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });

            // Uncomment this block to pass the first stage
            let mut tokens = tokenize(&file_contents);
            tokens.push(EOF_TOKEN.clone());
            for token in tokens {
                println!("{}", token);
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command);
        }
    }
}
