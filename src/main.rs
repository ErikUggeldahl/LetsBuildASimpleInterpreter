use std::fmt;

#[derive(Debug, Clone)]
enum Operator {
    Plus,
    Minus,
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone)]
enum Token {
    Integer(i32),
    Operator(Operator),
    EoF,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (name, value) = match self {
            Token::Integer(n) => ("Integer", n.to_string()),
            Token::Operator(kind) => ("Operator", kind.to_string()),
            Token::EoF => ("EOF", "None".to_string()),
        };
        write!(f, "Token({}, {})", name, value)
    }
}

struct InterpreterIter<'a> {
    iter: std::iter::Enumerate<std::str::Chars<'a>>,
}

impl<'a> Iterator for InterpreterIter<'a> {
    type Item = Result<Token, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        let (position, character) = match self.iter.next() {
            Some(token) => token,
            None => return None,
        };

        if character.is_digit(10) {
            return Some(Ok(Token::Integer(character.to_digit(10).unwrap() as i32)));
        }

        match character {
            '+' => Some(Ok(Token::Operator(Operator::Plus))),
            '-' => Some(Ok(Token::Operator(Operator::Minus))),
            _ => Some(Err(ParseError::UnexpectedCharacter(character, position))),
        }

    }
}

#[derive(Debug, Clone)]
enum ParseError {
    UnexpectedCharacter(char, usize),
    UnexpectedToken(Token),
    UnexpectedEoF,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::UnexpectedCharacter(character, index) =>
                write!(f, "Invalid character {} at index {}.", character, index),
            ParseError::UnexpectedToken(token) =>
                write!(f, "Invalid token: {}", token),
            ParseError::UnexpectedEoF =>
                write!(f, "Unexpected end of file."),
        }
    }
}

struct Interpreter {
    text: String,
}

impl Interpreter {
    fn new(text: String) -> Interpreter {
        Interpreter { text }
    }

    fn iter(&self) -> InterpreterIter {
        InterpreterIter {
            iter: self.text.chars().enumerate(),
        }
    }

    fn parse_digit(token: Option<Result<Token, ParseError>>) -> Result<i32, ParseError> {
        match token {
            Some(Ok(Token::Integer(n))) => Ok(n),
            Some(Ok(token)) => Err(ParseError::UnexpectedToken(token)),
            Some(Err(error)) => Err(error),
            None => Err(ParseError::UnexpectedEoF),
        }
    }

    fn parse_operator(token: Option<Result<Token, ParseError>>) -> Result<Operator, ParseError> {
        match token {
            Some(Ok(Token::Operator(operator))) => Ok(operator),
            Some(Ok(token)) => Err(ParseError::UnexpectedToken(token)),
            Some(Err(error)) =>  Err(error),
            None => Err(ParseError::UnexpectedEoF),
        }
    }

    fn expr(&self) -> Result<i32, ParseError> {
        let mut iter = self.iter();
        let left = Self::parse_digit(iter.next())?;

        let _operator = Self::parse_operator(iter.next())?;

        let right = Self::parse_digit(iter.next())?;

        Ok(left + right)
    }
}

fn main() {
    println!("{}", Token::Integer(42));
    println!("{}", Token::Operator(Operator::Plus));
    println!("{}", Token::EoF);

    let i = Interpreter::new("3+5".to_string());
    let mut iter = i.iter();
    println!("{}", iter.next().unwrap().unwrap());
    println!("{}", iter.next().unwrap().unwrap());
    println!("{}", iter.next().unwrap().unwrap());

    println!("{}", i.expr().unwrap());

    let i = Interpreter::new("9+9".to_string());
    println!("{}", i.expr().unwrap());

    let i = Interpreter::new("3".to_string());
    println!("{}", i.expr().unwrap_err());

    let i = Interpreter::new("+".to_string());
    println!("{}", i.expr().unwrap_err());

    let i = Interpreter::new("i".to_string());
    println!("{}", i.expr().unwrap_err());
}
