use std::fmt;
use std::io::{self, Write};

#[derive(Debug, Clone)]
enum Operator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    LeftParenthesis,
    RightParenthesis,
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

struct Lexer<'a> {
    iter: std::iter::Peekable<std::iter::Enumerate<std::str::Chars<'a>>>,
}

type PeekLexer<'a> = std::iter::Peekable<Lexer<'a>>;

impl<'a> Lexer<'a> {
    fn new(string: &'a str) -> PeekLexer<'a> {
        Self {
            iter: string.chars().enumerate().peekable(),
        }
        .peekable()
    }

    fn advance_whitespace(&mut self) {
        loop {
            match self.iter.peek() {
                Some(&(_, character)) if character.is_whitespace() => self.iter.next(),
                _ => return,
            };
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
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
            '(' => Some(Ok(Token::Operator(Operator::LeftParenthesis))),
            ')' => Some(Ok(Token::Operator(Operator::RightParenthesis))),
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
            ParseError::UnexpectedCharacter(character, index) => {
                write!(f, "Invalid character {} at index {}.", character, index)
            }
            ParseError::UnexpectedToken(token) => write!(f, "Invalid token: {}", token),
            ParseError::UnexpectedEoF => write!(f, "Unexpected end of file."),
        }
    }
}

struct BinaryOperation {
    left: Box<ASTNode>,
    right: Box<ASTNode>,
}

enum ASTNodeData {
    BinaryOperation(BinaryOperation),
    Number,
}

struct ASTNode {
    token: Token,
    data: ASTNodeData,
}

struct Parser;

impl Parser {
    fn factor(lexer: &mut PeekLexer) -> Result<ASTNode, ParseError> {
        match lexer.next() {
            Some(Ok(Token::Operator(Operator::LeftParenthesis))) => {
                let result = Self::expr(lexer);
                let close_parenthesis = lexer.next();
                match close_parenthesis {
                    Some(Ok(Token::Operator(Operator::RightParenthesis))) => result,
                    Some(Ok(token)) => Err(ParseError::UnexpectedToken(token)),
                    Some(Err(error)) => Err(error),
                    None => Err(ParseError::UnexpectedEoF),
                }
            }
            Some(Ok(token @ Token::Integer(_))) => Ok(ASTNode {
                token,
                data: ASTNodeData::Number,
            }),
            Some(Ok(token)) => Err(ParseError::UnexpectedToken(token)),
            Some(Err(error)) => Err(error),
            None => Err(ParseError::UnexpectedEoF),
        }
    }

    fn term(lexer: &mut PeekLexer) -> Result<ASTNode, ParseError> {
        let mut left = Self::factor(lexer)?;
        loop {
            match lexer.peek() {
                Some(Ok(Token::Operator(Operator::Multiplication)))
                | Some(Ok(Token::Operator(Operator::Division))) => {
                    let operator = lexer.next().unwrap()?;
                    let right = Self::factor(lexer)?;
                    left = ASTNode {
                        token: operator,
                        data: ASTNodeData::BinaryOperation(BinaryOperation {
                            left: Box::new(left),
                            right: Box::new(right),
                        }),
                    }
                }
                _ => return Ok(left),
            }
        }
    }

    fn expr(lexer: &mut PeekLexer) -> Result<ASTNode, ParseError> {
        let mut left = Self::term(lexer)?;
        loop {
            match lexer.peek() {
                Some(Ok(Token::Operator(Operator::Addition)))
                | Some(Ok(Token::Operator(Operator::Subtraction))) => {
                    let operator = lexer.next().unwrap()?;
                    let right = Self::term(lexer)?;
                    left = ASTNode {
                        token: operator,
                        data: ASTNodeData::BinaryOperation(BinaryOperation {
                            left: Box::new(left),
                            right: Box::new(right),
                        }),
                    }
                }
                _ => return Ok(left),
            }
        }
    }
}

trait NodeVisitor<T> {
    fn visit(node: &ASTNode) -> Result<T, ParseError>;
}

struct Interpreter;

impl Interpreter {
    fn interpret(mut lexer: PeekLexer) -> Result<i32, ParseError> {
        let result = Parser::expr(&mut lexer)?;
        Self::visit(&result)
    }

    fn visit_binary_operation(node: &ASTNode) -> Result<i32, ParseError> {
        match &node.data {
            ASTNodeData::BinaryOperation(BinaryOperation { left, right }) => match node.token {
                Token::Operator(Operator::Addition) => {
                    Ok(Interpreter::visit(&left)? + Interpreter::visit(&right)?)
                }
                Token::Operator(Operator::Subtraction) => {
                    Ok(Interpreter::visit(&left)? - Interpreter::visit(&right)?)
                }
                Token::Operator(Operator::Multiplication) => {
                    Ok(Interpreter::visit(&left)? * Interpreter::visit(&right)?)
                }
                Token::Operator(Operator::Division) => {
                    Ok(Interpreter::visit(&left)? / Interpreter::visit(&right)?)
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }


    fn visit_number(node: &ASTNode) -> Result<i32, ParseError> {
        match &node.data {
            ASTNodeData::Number => match node.token {
                Token::Integer(n) => Ok(n),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

}

impl NodeVisitor<i32> for Interpreter {
    fn visit(node: &ASTNode) -> Result<i32, ParseError> {
        match node.data {
            ASTNodeData::BinaryOperation(_) => Self::visit_binary_operation(node),
            ASTNodeData::Number => Self::visit_number(node),
        }
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
                match Interpreter::interpret(Lexer::new(&input)) {
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
        assert_eq!(Interpreter::interpret(Lexer::new("3+5")).unwrap(), 8);
    }

    #[test]
    fn addition_with_whitespace() {
        assert_eq!(
            Interpreter::interpret(Lexer::new("  20  +   55  ")).unwrap(),
            75
        );
    }

    #[test]
    fn subtraction() {
        assert_eq!(
            Interpreter::interpret(Lexer::new(" 821 - 437 ")).unwrap(),
            384
        );
    }

    #[test]
    fn multiplication() {
        assert_eq!(
            Interpreter::interpret(Lexer::new(" 6 × 30 ")).unwrap(),
            180
        );
    }

    #[test]
    fn division() {
        assert_eq!(Interpreter::interpret(Lexer::new("35  ÷  6")).unwrap(), 5);
    }

    #[test]
    fn multiple_operations() {
        assert_eq!(
            Interpreter::interpret(Lexer::new("35  ÷  6 * 10")).unwrap(),
            50
        );
    }

    #[test]
    fn single_integer() {
        assert_eq!(Interpreter::interpret(Lexer::new("3")).unwrap(), 3);
    }

    #[test]
    fn operator_precedence() {
        assert_eq!(
            Interpreter::interpret(Lexer::new("3 + 4 * 5 + 6")).unwrap(),
            29
        );
    }

    #[test]
    fn incomplete_addition() {
        assert!(Interpreter::interpret(Lexer::new("+")).is_err());
        assert!(Interpreter::interpret(Lexer::new("3 +")).is_err());
    }

    #[test]
    fn non_number() {
        assert!(Interpreter::interpret(Lexer::new("i")).is_err());
    }

    #[test]
    fn parentheses() {
        assert_eq!(
            Interpreter::interpret(Lexer::new("(3 + 4) * 5 + 6")).unwrap(),
            41
        );

        assert_eq!(
            Interpreter::interpret(Lexer::new("3 + 4 * (5 + 6)")).unwrap(),
            47
        );

        assert_eq!(
            Interpreter::interpret(Lexer::new("(3 + 4) * (5 + 6)")).unwrap(),
            77
        );

        assert_eq!(
            Interpreter::interpret(Lexer::new("(3 + (4 * 5) + 6)")).unwrap(),
            29
        );

        assert_eq!(
            Interpreter::interpret(Lexer::new("3 + 4 * 5 + 6")).unwrap(),
            29
        );

        assert_eq!(
            Interpreter::interpret(Lexer::new(
                "7 + 3 * (10 / (12 / (3 + 1) - 1)) / (2 + 3) - 5 - 3 + (8)"
            ))
            .unwrap(),
            10
        );

        assert_eq!(
            Interpreter::interpret(Lexer::new("7 + (((3 + 2)))")).unwrap(),
            12
        );

        assert!(Interpreter::interpret(Lexer::new("((3 + 4) * 5 + 6")).is_err());

        assert!(Interpreter::interpret(Lexer::new("((3 + 4 *) 5 + 6)")).is_err());
    }
}
