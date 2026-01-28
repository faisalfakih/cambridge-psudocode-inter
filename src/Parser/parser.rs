use std::collections::HashMap;
use crate::Lexer::{lexer::Token, lexer::TokenType};
use crate::errortype::{CPSError, ErrorType};
use crate::Parser::ast::{BinaryExpr};
use super::ast::{Ast, Operator, Position, Associativity, Precedence};


#[derive(Debug, Clone)]
pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
    operators: HashMap<TokenType, Operator>,
    source: String,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, source: String) -> Self {
        let operators: HashMap<TokenType, Operator> = [
            (TokenType::Plus, Operator { precedence: 10, position: Position::Infix, associativity: Associativity::Left }),
            (TokenType::Minus, Operator { precedence: 10, position: Position::Infix, associativity: Associativity::Left }),
            (TokenType::Asterisk, Operator { precedence: 20, position: Position::Infix, associativity: Associativity::Left }),
            (TokenType::ForwardSlash, Operator { precedence: 20, position: Position::Infix, associativity: Associativity::Left }),
            (TokenType::Mod, Operator { precedence: 20, position: Position::Infix, associativity: Associativity::Left }),
            (TokenType::Div, Operator { precedence: 20, position: Position::Infix, associativity: Associativity::Left }),
            (TokenType::Caret, Operator { precedence: 30, position: Position::Infix, associativity: Associativity::Right }), 
            // comparison operators
            (TokenType::Equal, Operator { precedence: 5, position: Position::Infix, associativity: Associativity::Left }),
            (TokenType::NotEqual, Operator { precedence: 5, position: Position::Infix, associativity: Associativity::Left }),
            (TokenType::LessThan, Operator { precedence: 5, position: Position::Infix, associativity: Associativity::Left }),
            (TokenType::LessEqual, Operator { precedence: 5, position: Position::Infix, associativity: Associativity::Left }),
            (TokenType::GreaterThan, Operator { precedence: 5, position: Position::Infix, associativity: Associativity::Left }),
            (TokenType::GreaterEqual, Operator { precedence: 5, position: Position::Infix, associativity: Associativity::Left }),
            // logical operators
            (TokenType::Or, Operator { precedence: 2, position: Position::Infix, associativity: Associativity::Left }),
            (TokenType::And, Operator { precedence: 3, position: Position::Infix, associativity: Associativity::Left }),
            // string concatination
            (TokenType::Ampersand, Operator { precedence: 8, position: Position::Infix, associativity: Associativity::Left})
        ].iter().cloned().collect();

        Parser { tokens, position: 0, operators, source }
    }


    // helper functions
    fn peek(&self, n: usize) -> Token {
        if let Some(token) = self.tokens.get(self.position + n) {
            token.clone()
        } else {
            Token::new("".to_string(), TokenType::Eof, 0, 0)
        }
    }

    fn peek_result(&self, n: usize) -> Result<Token, CPSError> {
        if let Some(token) = self.tokens.get(self.position + n) {
            Ok(token.clone())
        } else {
            Err(CPSError {
                error_type: ErrorType::Syntax,
                message: "Unexpected end of input".to_string(),
                hint: Some("Expected more tokens".to_string()),
                line: 0,
                column: 0,
                source: Some(self.source.clone()),
            })
        }
    }

    fn advance(&mut self) -> Token {
        self.position += 1;
        self.peek(0)
    }

    fn is_terminator(&self, token_type: TokenType) -> bool {
        !matches!(token_type, 
            TokenType::Plus |
            TokenType::Minus |
            TokenType::Asterisk |
            TokenType::ForwardSlash |
            TokenType::Caret
            | TokenType::Identifier | TokenType::NumberLiteral | TokenType::StringLiteral | TokenType::CharLiteral
            | TokenType::True | TokenType::False
            | TokenType::LParen | TokenType::Eof
        )
    }

    fn is_operator(&self, token_type: &TokenType) -> bool {
        self.operators.contains_key(&token_type)
    }

    // parsing logic
    pub fn parse_top_expr(&mut self) -> Result<Ast, CPSError> {
        self.parse_expr(0)
    }

    fn parse_primary(&mut self) -> Result<Ast, CPSError> {
        let token = self.peek(0);

        match token.token_type {
            TokenType::NumberLiteral => {
                self.advance();
                token.lexeme.parse::<f64>()
                    .map(Ast::Number)
                    .map_err(|_| CPSError {
                        error_type: ErrorType::Syntax,
                        message: format!("Invalid number literal: '{}'", token.lexeme),
                        hint: None,
                        line: token.line,
                        column: token.column,
                        source: Some(self.source.clone()),
                    })
            }
            TokenType::Identifier => {
                self.advance();
                Ok(Ast::Identifier(token.lexeme.clone()))
            }
            TokenType::LParen => {
                self.advance();
                let expr = self.parse_expr(0)?;
                if self.peek(0).token_type != TokenType::RParen {
                    return Err(CPSError {
                        error_type: ErrorType::Syntax,
                        message: "Expected ')'".to_string(),
                        hint: Some("Mismatched parentheses".to_string()),
                        line: self.peek(0).line,
                        column: self.peek(0).column,
                        source: Some(self.source.clone()),
                    });
                }
                self.advance();
                Ok(expr)
            }
            TokenType::Eof => {
                Err(CPSError {
                    error_type: ErrorType::Syntax,
                    message: "Unexpected end of input".to_string(),
                    hint: Some("Expected an expression".to_string()),
                    line: token.line,
                    column: token.column,
                    source: Some(self.source.clone()),
                })
            }
            _ => {
                Err(CPSError {
                    error_type: ErrorType::Syntax,
                    message: format!("Unexpected token: {:?}", token.token_type),
                    hint: Some("Expected a number, identifier, or '('".to_string()),
                    line: token.line,
                    column: token.column,
                    source: Some(self.source.clone()),
                })
            }
        }
    }

    fn parse_expr(&mut self, min_precedence: Precedence) -> Result<Ast, CPSError> {
        let mut lhs = self.parse_primary()?;

        loop {
            let current = self.peek(0);

            if self.is_terminator(current.token_type.clone()) || current.token_type == TokenType::Eof {
                break;
            }

            if !self.is_operator(&current.token_type) {
                break;
            }

            let operator = match self.operators.get(&current.token_type) {
                Some(op) => op.clone(),
                None => break,
            };

            if operator.precedence < min_precedence {
                break;
            }

            let op_token = current.token_type.clone();
            self.advance();

            let next_min_prec = match operator.associativity {
                Associativity::Left => operator.precedence + 1,
                Associativity::Right => operator.precedence,
            };

            let rhs = self.parse_expr(next_min_prec)?;

            lhs = Ast::BinaryOp(BinaryExpr {
                left: Box::new(lhs),
                operator: op_token,
                right: Box::new(rhs),
            });
        }

        Ok(lhs)
    }
}

