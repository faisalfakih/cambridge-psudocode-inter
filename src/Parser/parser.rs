use std::collections::HashMap;
use crate::Lexer::{lexer::Token, lexer::TokenType};
use crate::errortype::{CPSError, ErrorType};
use crate::Parser::ast::{BinaryExpr};
use super::ast::{Ast, Operator, Position, Associativity, Precedence, Stmt};


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
        let peek = self.peek(0);
        self.position += 1;
        peek
    }

    fn is_terminator(&self, token_type: TokenType) -> bool {
        !matches!(token_type, 
            TokenType::Plus |
            TokenType::Minus |
            TokenType::Asterisk |
            TokenType::ForwardSlash |
            TokenType::Caret
            | TokenType::Mod | TokenType::Div
            | TokenType::Equal | TokenType::NotEqual | TokenType::LessThan | TokenType::LessEqual
            | TokenType::Identifier | TokenType::NumberLiteral | TokenType::StringLiteral | TokenType::CharLiteral
            // | TokenType::True | TokenType::False
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

            lhs = Ast::Expression(super::ast::Expr::Binary(BinaryExpr {
                left: Box::new(lhs),
                operator: op_token,
                right: Box::new(rhs),
            }));
        }

        // consume last token 
        // let _ = self.advance();

        Ok(lhs)
    }

    pub fn parse_statements(&mut self) -> Result<Vec<Ast>, CPSError> {
        let mut ast = vec![]; 
        loop {
            let token = self.peek(0);
            let statement = match token.token_type {
                TokenType::If => self.parse_if_statement(),
                TokenType::Output => self.parse_output(),
                TokenType::NumberLiteral => self.parse_top_expr(),
                // terminate if it leaves the scope
                TokenType::Eof | TokenType::EndIf | TokenType::EndCase | TokenType::EndType | TokenType::Else
                    | TokenType::EndClass | TokenType::EndWhile | TokenType::EndFunction | TokenType::EndProcedure => break,
                _ => {
                    return Err(CPSError { 
                        error_type: ErrorType::Syntax,
                        message: format!("Unexpected token in statement: {:?}", token.token_type),
                        hint: Some("Expected a valid statement".to_string()),
                        line: token.line,
                        column: token.column,
                        source: Some(self.source.clone()),
                    })
                }
            };
            match statement {
                Err(e) => return Err(e),
                _ => {}
            }
            ast.push(statement?);
        }
        Ok(ast)
    }

    fn parse_output(&mut self) -> Result<Ast, CPSError> {
        // consume 'output' token
        let output_token = self.advance();
        let expr = match self.parse_expr(0)? {
            Ast::Expression(e) => e,
            _ => {
                return Err(CPSError {
                    error_type: ErrorType::Syntax,
                    message: "Expected an expression after OUTPUT".to_string(),
                    hint: Some("OUTPUT must be followed by a valid expression".to_string()),
                    line: output_token.line,
                    column: output_token.column,
                    source: Some(self.source.clone()),
                });
            }
        };
        Ok(Ast::Stmt(Stmt::Output { target: expr }))
    }

    

    fn parse_if_statement(&mut self) -> Result<Ast, CPSError> {
        let if_token = self.advance(); // consume 'if'
        let condition = self.parse_expr(0)?;
        // consume the 'THEN'
        let then = self.peek(0);
        if then.token_type != TokenType::Then {
            return Err(CPSError {
                error_type: ErrorType::Syntax,
                message: "Expected 'then' after if condition".to_string(),
                hint: Some("IF statements must have a THEN clause".to_string()),
                line: then.line,
                column: then.column,
                source: Some(self.source.clone()),
            });
        }

        // consume 'then'
        self.advance();
        let then_branch = self.parse_statements()?;
        let else_branch = if self.peek(0).token_type == TokenType::Else {
            self.advance(); // consume 'else'
            Some(self.parse_statements()?)
        } else {
            None
        };


        // consume endif
        let end_token = self.peek(0);
        if end_token.token_type != TokenType::EndIf {
            return Err(CPSError {
                error_type: ErrorType::Syntax,
                message: "Expected 'endif' after if statement".to_string(),
                hint: Some("IF statements must be closed with ENDIF".to_string()),
                line: end_token.line,
                column: end_token.column,
                source: Some(self.source.clone()),
            });
        }


        let condition = super::ast::Expr::Binary(BinaryExpr {
            left: Box::new(condition),
            operator: TokenType::Equal,
            right: Box::new(Ast::Identifier("true".to_string())),
        });

        let then_statements: Result<Vec<Stmt>, CPSError> = then_branch.into_iter().map(|a| match a {
            Ast::Stmt(s) => Ok(s),
            _ => Err(CPSError {
                error_type: ErrorType::Syntax,
                message: "Expected statement in then branch".to_string(),
                hint: Some("IF statement body must contain valid statements".to_string()),
                line: if_token.line,
                column: if_token.column,
                source: Some(self.source.clone()),
            }),
        }).collect();

        let else_statements: Option<Result<Vec<Stmt>, CPSError>> = else_branch.map(|branch| {
            branch.into_iter().map(|a| match a {
                Ast::Stmt(s) => Ok(s),
                _ => Err(CPSError {
                    error_type: ErrorType::Syntax,
                    message: "Expected statement in else branch".to_string(),
                    hint: Some("ELSE statement body must contain valid statements".to_string()),
                    line: if_token.line,
                    column: if_token.column,
                    source: Some(self.source.clone()),
                }),
            }).collect()
        });

        Ok(Ast::Stmt(Stmt::If {
            condition: Box::new(condition),
            then_branch: super::ast::BlockStmt { 
                statements: then_statements?
            },
            else_branch: match else_statements {
                Some(result) => Some(super::ast::BlockStmt { 
                    statements: result?
                }),
                None => None,
            },
        }))
    }

}

