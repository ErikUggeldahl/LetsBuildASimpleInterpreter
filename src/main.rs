use std::fmt;
use std::io::{self, Write};

#[derive(Debug, Clone)]
enum Operator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
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
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (name, value) = match self {
            Token::Integer(n) => ("Integer", n.to_string()),
            Token::Operator(kind) => ("Operator", kind.to_string()),
        };
        write!(f, "Token({}, {})", name, value)
    }
}

struct InterpreterIter<'a> {
    iter: std::iter::Peekable<std::iter::Enumerate<std::str::Chars<'a>>>,
}

impl<'a> Iterator for InterpreterIter<'a> {
    type Item = Result<Token, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.advance_whitespace();

        let (position, character) = match self.iter.next() {
            Some(character) => character,
            None => return None,
        };

        if character.is_digit(10) {
            let mut base = character.to_digit(10).unwrap() as i32;
            loop {
                match self.iter.peek() {
                    Some(&(_, c)) if c.is_digit(10) => {
                        base = base * 10 + c.to_digit(10).unwrap() as i32;
                        self.iter.next();
                    }
                    _ => break,
                }
            }
            return Some(Ok(Token::Integer(base)));
        }

        match character {
            '+' => Some(Ok(Token::Operator(Operator::Addition))),
            '-' => Some(Ok(Token::Operator(Operator::Subtraction))),
            '×' | '*' => Some(Ok(Token::Operator(Operator::Multiplication))),
            '÷' | '/' => Some(Ok(Token::Operator(Operator::Division))),
            _ => Some(Err(ParseError::UnexpectedCharacter(character, position))),
        }
    }
}

impl<'a> InterpreterIter<'a> {
    fn advance_whitespace(&mut self) {
        loop {
            match self.iter.peek() {
                Some(&(_, character)) if character.is_whitespace() => self.iter.next(),
                _ => return,
            };
        }
    }

    fn peek(&mut self) -> Option<()> {
        self.advance_whitespace();
        self.iter.peek().map(|_| ())
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
            ParseError::UnexpectedCharacter(character, index) => {
                write!(f, "Invalid character {} at index {}.", character, index)
            }
            ParseError::UnexpectedToken(token) => write!(f, "Invalid token: {}", token),
            ParseError::UnexpectedEoF => write!(f, "Unexpected end of file."),
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
            iter: self.text.chars().enumerate().peekable(),
        }
    }

    fn parse_number(token: Option<Result<Token, ParseError>>) -> Result<i32, ParseError> {
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
            Some(Err(error)) => Err(error),
            None => Err(ParseError::UnexpectedEoF),
        }
    }

    fn expr(&self) -> Result<i32, ParseError> {
        let mut iter = self.iter();
        let mut left = Self::parse_number(iter.next())?;

        loop {
            let operator = Self::parse_operator(iter.next())?;

            let right = Self::parse_number(iter.next())?;

            left = match operator {
                Operator::Addition => left + right,
                Operator::Subtraction => left - right,
                Operator::Multiplication => left * right,
                Operator::Division => left / right,
            };

            if iter.peek().is_none() {
                break;
            }
        }

        Ok(left)
    }
}

fn main() {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut input = String::new();
        match std::io::stdin().read_line(&mut input) {
            Ok(_) if input == "\n" || input == "\r\n" => break,
            Ok(_) => {
                let interpteter = Interpreter::new(input);
                match interpteter.expr() {
                    Ok(result) => println!("{}", result),
                    Err(e) => println!("Error: {}", e),
                };
            }
            Err(error) => println!("error: {}", error),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_addition() {
        assert_eq!(Interpreter::new("3+5".to_string()).expr().unwrap(), 8);
    }

    #[test]
    fn addition_with_whitespace() {
        assert_eq!(
            Interpreter::new("  20  +   55  ".to_string())
                .expr()
                .unwrap(),
            75
        );
    }

    #[test]
    fn subtraction() {
        assert_eq!(
            Interpreter::new("  20  +   55  ".to_string())
                .expr()
                .unwrap(),
            75
        );
    }

    #[test]
    fn multiplication() {
        assert_eq!(
            Interpreter::new(" 6 × 30 ".to_string()).expr().unwrap(),
            180
        );
    }

    #[test]
    fn division() {
        assert_eq!(Interpreter::new("35  ÷  6".to_string()).expr().unwrap(), 5);
    }

    #[test]
    fn multiple_operations() {
        assert_eq!(
            Interpreter::new("35  ÷  6 * 10".to_string())
                .expr()
                .unwrap(),
            50
        );
    }

    #[test]
    fn single_integer() {
        assert!(Interpreter::new("3".to_string()).expr().is_err());
    }

    #[test]
    fn incomplete_addition() {
        assert!(Interpreter::new("+".to_string()).expr().is_err());
        assert!(Interpreter::new("3 +".to_string()).expr().is_err());
    }

    #[test]
    fn non_number() {
        assert!(Interpreter::new("i".to_string()).expr().is_err());
    }
}
