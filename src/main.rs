use std::collections::VecDeque;
use std::env;
use std::fmt::Display;
use std::fs;
use std::iter::Peekable;

const KEYWORDS: [&str; 16] = [
    "and", "class", "else", "false", "for", "fun", "if", "nil", "or", "print", "return", "super",
    "this", "true", "var", "while",
];

#[derive(Debug)]
enum AST {
    Expr(Expr),
}

#[derive(Debug)]
enum Expr {
    Literal(LiteralExpr),
    Unary(Unary),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Grouping(Box<Expr>),
}

#[derive(Debug)]
enum LiteralExpr {
    Lit(Literal),
    True,
    False,
    Nil,
}

#[derive(Debug)]
enum Unary {
    Minus(Box<Expr>),
    Neg(Box<Expr>),
}

#[derive(Debug, Clone, Copy)]
#[allow(non_camel_case_types)]
enum BinaryOp {
    EQUAL_EQUAL,
    BANG_EQUAL,
    LESS,
    LESS_EQUAL,
    GREATER,
    GREATER_EQUAL,
    PLUS,
    MINUS,
    STAR,
    SLASH,
}

trait HasPrecedence {
    fn precedence_level(&self) -> i8;
}

impl HasPrecedence for BinaryOp {
    fn precedence_level(&self) -> i8 {
        match self {
            BinaryOp::LESS
            | BinaryOp::GREATER
            | BinaryOp::LESS_EQUAL
            | BinaryOp::GREATER_EQUAL
            | BinaryOp::EQUAL_EQUAL
            | BinaryOp::BANG_EQUAL => 2,
            BinaryOp::STAR | BinaryOp::SLASH => 1,
            BinaryOp::MINUS | BinaryOp::PLUS => 0,
        }
    }
}

impl HasPrecedence for Unary {
    fn precedence_level(&self) -> i8 {
        i8::MAX
    }
}

#[derive(Debug, Clone)]
#[allow(non_camel_case_types)]
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
    STRING,
    NUMBER,
    IDENTIFIER,
    EOF,
    KEYWORD(String),
}

#[non_exhaustive]
#[derive(Clone, Debug)]
enum Literal {
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

#[derive(Clone)]
struct Token(TokenType, String, Option<Literal>);

const EOF_TOKEN: Token = Token(TokenType::EOF, String::new(), None);

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
enum ScanningError {
    UnexpectedCharacter(char),
    UnterminatedString,
    InvalidNumberLiteral,
}

#[derive(Debug)]
enum ParsingError {
    UnparsedTokens,
    NothingToParse,
    TokenTypeMismatch,
    UnexpectedToken,
    UnbalancedGrouping,
    MalformedBinaryOperation,
    InvalidExpression,
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

        if c == '"' {
            match scan_string_literal(&mut input_iterator) {
                Ok(t) => tokens.push(t),
                Err(e) => errors.push((current_line, e)),
            }
            continue;
        }

        if c.is_ascii_digit() {
            match scan_number_literal(&mut input_iterator, c) {
                Ok(t) => tokens.push(t),
                Err(e) => errors.push((current_line, e)),
            }
            continue;
        }

        if c.is_ascii_alphabetic() || c == '_' {
            match scan_identifier_or_keyword(&mut input_iterator, c) {
                Ok(t) => tokens.push(t),
                Err(e) => errors.push((current_line, e)),
            }
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

trait Visit {
    fn visit_ast(&self, ast: AST) {
        match ast {
            AST::Expr(expr) => self.visit_expr(expr),
        }
    }

    fn visit_expr(&self, expr: Expr) {
        match expr {
            Expr::Literal(literal_expr) => self.visit_expr_literal(literal_expr),
            Expr::Unary(unary) => self.visit_unary(unary),
            Expr::Binary(expr, binary_op, expr1) => {
                self.visit_expr(*expr);
                self.visit_binary_op(binary_op);
                self.visit_expr(*expr1);
            }
            Expr::Grouping(expr) => self.visit_grouping(*expr),
        }
    }

    fn visit_expr_literal(&self, literal_expr: LiteralExpr) {
        if let LiteralExpr::Lit(literal) = literal_expr {
            self.visit_literal(literal);
        }
    }

    fn visit_literal(&self, literal: Literal) {}

    fn visit_unary(&self, unary: Unary) {
        match unary {
            Unary::Minus(expr) => self.visit_expr(*expr),
            Unary::Neg(expr) => self.visit_expr(*expr),
        }
    }

    fn visit_binary_op(&self, binary_op: BinaryOp) {}

    fn visit_grouping(&self, expr: Expr) {
        self.visit_expr(expr);
    }
}

pub struct AstPrinter;

impl Visit for AstPrinter {
    fn visit_expr_literal(&self, literal_expr: LiteralExpr) {
        print!(
            "{}",
            match literal_expr {
                LiteralExpr::Lit(literal) => format!("{literal}"),
                LiteralExpr::True => "true".to_string(),
                LiteralExpr::False => "false".to_string(),
                LiteralExpr::Nil => "nil".to_string(),
            }
        )
    }

    fn visit_grouping(&self, expr: Expr) {
        print!("(group ");
        self.visit_expr(expr);
        print!(")")
    }

    fn visit_unary(&self, unary: Unary) {
        print!("(");
        let expr = match unary {
            Unary::Minus(expr) => {
                print!("-");
                expr
            }
            Unary::Neg(expr) => {
                print!("!");
                expr
            }
        };
        print!(" ");
        self.visit_expr(*expr);
        print!(")");
    }

    fn visit_ast(&self, ast: AST) {
        match ast {
            AST::Expr(expr) => self.visit_expr(expr),
        }
        println!();
    }

    fn visit_expr(&self, expr: Expr) {
        match expr {
            Expr::Literal(literal_expr) => self.visit_expr_literal(literal_expr),
            Expr::Unary(unary) => self.visit_unary(unary),
            Expr::Binary(expr, binary_op, expr1) => {
                print!("(");
                match binary_op {
                    BinaryOp::EQUAL_EQUAL => print!("=="),
                    BinaryOp::BANG_EQUAL => print!("!="),
                    BinaryOp::LESS => print!("<"),
                    BinaryOp::LESS_EQUAL => print!("<="),
                    BinaryOp::GREATER => print!(">"),
                    BinaryOp::GREATER_EQUAL => print!(">="),
                    BinaryOp::PLUS => print!("+"),
                    BinaryOp::MINUS => print!("-"),
                    BinaryOp::STAR => print!("*"),
                    BinaryOp::SLASH => print!("/"),
                }
                print!(" ");
                self.visit_expr(*expr);
                print!(" ");
                self.visit_expr(*expr1);
                print!(")");
            }
            Expr::Grouping(expr) => self.visit_grouping(*expr),
        }
    }
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

fn parse_expr<'a>(
    tokens_iterator: &mut Peekable<impl Iterator<Item = &'a Token>>,
    previous_operators_precedences_stack: &mut VecDeque<i8>,
) -> Result<Expr, ParsingError> {
    let Some(Token(token_type, _lexeme, value)) = tokens_iterator.next() else {
        return Err(ParsingError::NothingToParse);
    };

    let mut expr = match token_type {
        TokenType::STRING => {
            if let Some(l @ Literal::String(_)) = value {
                Ok(Expr::Literal(LiteralExpr::Lit(l.clone())))
            } else {
                Err(ParsingError::TokenTypeMismatch)
            }
        }
        TokenType::NUMBER => {
            if let Some(l @ Literal::Number(_)) = value {
                Ok(Expr::Literal(LiteralExpr::Lit(l.clone())))
            } else {
                Err(ParsingError::TokenTypeMismatch)
            }
        }

        TokenType::KEYWORD(keyword) => {
            if keyword == "true" {
                Ok(Expr::Literal(LiteralExpr::True))
            } else if keyword == "false" {
                Ok(Expr::Literal(LiteralExpr::False))
            } else if keyword == "nil" {
                Ok(Expr::Literal(LiteralExpr::Nil))
            } else {
                todo!()
            }
        }
        TokenType::LEFT_PAREN => {
            previous_operators_precedences_stack.push_front(i8::MIN);
            let expr = parse_expr(tokens_iterator, previous_operators_precedences_stack)?;
            if !tokens_iterator
                .next()
                .is_some_and(|Token(token_type, _, _)| matches!(token_type, TokenType::RIGHT_PAREN))
            {
                return Err(ParsingError::UnbalancedGrouping);
            }
            previous_operators_precedences_stack.pop_front();
            Ok(Expr::Grouping(Box::new(expr)))
        }
        TokenType::BANG => {
            previous_operators_precedences_stack.push_front(i8::MAX);
            let expr = parse_expr(tokens_iterator, previous_operators_precedences_stack)?;
            previous_operators_precedences_stack.pop_front();
            Ok(Expr::Unary(Unary::Neg(Box::new(expr))))
        }
        TokenType::MINUS => {
            previous_operators_precedences_stack.push_front(i8::MAX);
            let expr = parse_expr(tokens_iterator, previous_operators_precedences_stack)?;
            previous_operators_precedences_stack.pop_front();
            Ok(Expr::Unary(Unary::Minus(Box::new(expr))))
        }
        _ => Err(ParsingError::UnexpectedToken),
    }?;

    while let Some(Ok(next_binary_operator)) = tokens_iterator
        .peek()
        .map(|&t| parse_binary_operator(&mut std::iter::once(&t.clone()).peekable()))
    {
        if previous_operators_precedences_stack.is_empty()
            || previous_operators_precedences_stack
                .front()
                .is_some_and(|previous_precedence| {
                    next_binary_operator.precedence_level() > *previous_precedence
                })
        {
            let binary_operator = parse_binary_operator(tokens_iterator)?;

            previous_operators_precedences_stack.push_front(binary_operator.precedence_level());
            let expr2 = parse_expr(tokens_iterator, previous_operators_precedences_stack)?;

            previous_operators_precedences_stack.pop_front();
            expr = Expr::Binary(Box::new(expr), binary_operator, Box::new(expr2));
        } else {
            break;
        }
    }
    Ok(expr)
}

fn parse_binary_operator<'a>(
    tokens_iterator: &mut Peekable<impl Iterator<Item = &'a Token>>,
) -> Result<BinaryOp, ParsingError> {
    let Some(Token(token_type, _, _)) = tokens_iterator.next() else {
        return Err(ParsingError::NothingToParse);
    };
    match token_type {
        TokenType::MINUS => Ok(BinaryOp::MINUS),
        TokenType::PLUS => Ok(BinaryOp::PLUS),
        TokenType::STAR => Ok(BinaryOp::STAR),
        TokenType::SLASH => Ok(BinaryOp::SLASH),
        TokenType::GREATER => Ok(BinaryOp::GREATER),
        TokenType::LESS => Ok(BinaryOp::LESS),
        TokenType::GREATER_EQUAL => Ok(BinaryOp::GREATER_EQUAL),
        TokenType::LESS_EQUAL => Ok(BinaryOp::LESS_EQUAL),
        TokenType::EQUAL_EQUAL => Ok(BinaryOp::EQUAL_EQUAL),
        TokenType::BANG_EQUAL => Ok(BinaryOp::BANG_EQUAL),
        _ => Err(ParsingError::InvalidExpression),
    }
}

fn parse_ast(tokens: &[Token]) -> Result<AST, ParsingError> {
    let mut tokens_iterator = tokens.iter().peekable();
    let expr = parse_expr(&mut tokens_iterator, &mut VecDeque::new())?;
    if !tokens_iterator
        .next()
        .is_some_and(|Token(token_type, _, _)| matches!(token_type, TokenType::EOF))
    {
        return Err(ParsingError::UnparsedTokens);
    }
    Ok(AST::Expr(expr))
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

        "parse" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });
            let (mut tokens, _scanning_errors) = tokenize(&file_contents);
            tokens.push(EOF_TOKEN.clone());
            match parse_ast(&tokens) {
                Ok(ast) => {
                    AstPrinter.visit_ast(ast);
                }
                Err(err) => println!("{err:?}"),
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command);
        }
    }
    std::process::exit(exit_code)
}
