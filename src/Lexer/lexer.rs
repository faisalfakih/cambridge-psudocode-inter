use crate::errortype::LexicalError;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
enum TokenType {
    // KEYWORDS
    AND,
    APPEND, 
    ARRAY, 
    BOOLEAN, 
    BYREF,
    BYVAL,
    CALL,
    CASE,
    OF,
    CHAR,  
    CLASS,
    CLOSEFILE,
    CONSTANT, 
    DATE,
    DECLARE, 
    DIV, 
    ELSE,
    ENDCASE,
    ENDCLASS, 
    ENDFUNCTION, 
    ENDIF,
    ENDPROCEDURE,
    ENDTYPE,
    ENDWHILE,
    EOF,
    FALSE,
    FOR,
    TO,
    FUNCTION,
    GETRECORD,
    IF,
    INHERITS,
    INPUT, 
    INT, 
    INTEGER,
    LCASE,
    LENGTH, 
    MID,
    MOD,
    NEXT,
    NEW,
    NOT,
    OPENFILE,
    OR,
    OTHERWISE, 
    OUTPUT,
    PROCEDURE,
    PRIVATE,
    PUBLIC,
    PUTRECORD,
    RAND,
    RANDOM,
    READ,
    READFILE,
    REAL,
    REPEAT,
    RETURN, 
    RETURNS,
    RIGHT,
    SEEK,
    STEP,
    STRING,
    SUPER,
    THEN,
    TRUE,
    TYPE,
    UCASE,
    UNTIL,
    WHILE,
    WRITE,
    WRITEFILE,

    // IDENTIFIERS AND LITERALS
    IDENTIFIER,
    NUMBER_LITERAL,
    STRING_LITERAL,
    CHAR_LITERAL,

    // OPERATORS
    MINUS,
    ARROW,
    ASTERISK,
    FORWARD_SLASH,
    COMMENT,
    PLUS,
    LESS_THAN,
    LESS_EQUAL,
    NOT_EQUAL,
    EQUAL,
    GREATER_THAN,
    GREATER_EQUAL,
    CARET,
    AMPERSAND,
    COLON,
}


#[derive(Debug, Clone)]
pub struct Token {
    lexeme: String,
    token_type: TokenType,
    line: usize,
    column: usize,
    error: Option<LexicalError>,
}

impl Token {
    fn new(lexeme: String, token_type: TokenType, line: usize, column: usize) -> Self {
        Token {
            lexeme,
            token_type,
            line,
            column,
            error: None,
        }
    }

}


pub struct Lexer {
    source: String,
    position: usize,
    line: usize,
    column: usize,
    token_map: HashMap<String, TokenType>,
}


impl Lexer { pub fn new(source: String) -> Self {
        let token_map: HashMap<String, TokenType> = [
            // Keywords A-C
            ("AND".to_string(), TokenType::AND),
            ("APPEND".to_string(), TokenType::APPEND),
            ("ARRAY".to_string(), TokenType::ARRAY),
            ("BOOLEAN".to_string(), TokenType::BOOLEAN),
            ("BYREF".to_string(), TokenType::BYREF),
            ("BYVAL".to_string(), TokenType::BYVAL),
            ("CALL".to_string(), TokenType::CALL),
            ("CASE".to_string(), TokenType::CASE),
            ("CHAR".to_string(), TokenType::CHAR),
            ("CLASS".to_string(), TokenType::CLASS),
            ("CLOSEFILE".to_string(), TokenType::CLOSEFILE),
            ("CONSTANT".to_string(), TokenType::CONSTANT),

            // Keywords D-F
            ("DATE".to_string(), TokenType::DATE),
            ("DECLARE".to_string(), TokenType::DECLARE),
            ("DIV".to_string(), TokenType::DIV),
            ("ELSE".to_string(), TokenType::ELSE),
            ("ENDCASE".to_string(), TokenType::ENDCASE),
            ("ENDCLASS".to_string(), TokenType::ENDCLASS),
            ("ENDFUNCTION".to_string(), TokenType::ENDFUNCTION),
            ("ENDIF".to_string(), TokenType::ENDIF),
            ("ENDPROCEDURE".to_string(), TokenType::ENDPROCEDURE),
            ("ENDTYPE".to_string(), TokenType::ENDTYPE),
            ("ENDWHILE".to_string(), TokenType::ENDWHILE),
            ("EOF".to_string(), TokenType::EOF),
            ("FALSE".to_string(), TokenType::FALSE),
            ("FOR".to_string(), TokenType::FOR),
            ("FUNCTION".to_string(), TokenType::FUNCTION),

            // Keywords G-L
            ("GETRECORD".to_string(), TokenType::GETRECORD),
            ("IF".to_string(), TokenType::IF),
            ("INHERITS".to_string(), TokenType::INHERITS),
            ("INPUT".to_string(), TokenType::INPUT),
            ("INT".to_string(), TokenType::INT),
            ("INTEGER".to_string(), TokenType::INTEGER),
            ("LCASE".to_string(), TokenType::LCASE),
            ("LENGTH".to_string(), TokenType::LENGTH),

            // Keywords M-O
            ("MID".to_string(), TokenType::MID),
            ("MOD".to_string(), TokenType::MOD),
            ("NEW".to_string(), TokenType::NEW),
            ("NEXT".to_string(), TokenType::NEXT),
            ("NOT".to_string(), TokenType::NOT),
            ("OF".to_string(), TokenType::OF),
            ("OPENFILE".to_string(), TokenType::OPENFILE),
            ("OR".to_string(), TokenType::OR),
            ("OTHERWISE".to_string(), TokenType::OTHERWISE),
            ("OUTPUT".to_string(), TokenType::OUTPUT),

            // Keywords P-R
            ("PRIVATE".to_string(), TokenType::PRIVATE),
            ("PROCEDURE".to_string(), TokenType::PROCEDURE),
            ("PUBLIC".to_string(), TokenType::PUBLIC),
            ("PUTRECORD".to_string(), TokenType::PUTRECORD),
            ("RAND".to_string(), TokenType::RAND),
            ("RANDOM".to_string(), TokenType::RANDOM),
            ("READ".to_string(), TokenType::READ),
            ("READFILE".to_string(), TokenType::READFILE),
            ("REAL".to_string(), TokenType::REAL),
            ("REPEAT".to_string(), TokenType::REPEAT),
            ("RETURN".to_string(), TokenType::RETURN),
            ("RETURNS".to_string(), TokenType::RETURNS),
            ("RIGHT".to_string(), TokenType::RIGHT),

            // Keywords S-U
            ("SEEK".to_string(), TokenType::SEEK),
            ("STEP".to_string(), TokenType::STEP),
            ("STRING".to_string(), TokenType::STRING),
            ("SUPER".to_string(), TokenType::SUPER),
            ("THEN".to_string(), TokenType::THEN),
            ("TO".to_string(), TokenType::TO),
            ("TRUE".to_string(), TokenType::TRUE),
            ("TYPE".to_string(), TokenType::TYPE),
            ("UCASE".to_string(), TokenType::UCASE),
            ("UNTIL".to_string(), TokenType::UNTIL),

            // Keywords W
            ("WHILE".to_string(), TokenType::WHILE),
            ("WRITE".to_string(), TokenType::WRITE),
            ("WRITEFILE".to_string(), TokenType::WRITEFILE),
            ].iter().cloned().collect();

        Lexer {
            source,
            position: 0,
            line: 1,
            column: 1,
            token_map,
        }
    }

    // helper functions 
    fn peek(&self, n: usize) -> Option<char> {
        self.source.chars().nth(self.position + n)
    }

    pub fn tokenize(mut self) -> Result<Vec<Token>, LexicalError> {
        let mut tokens = Vec::new();

        loop {
            let token = self.get_next_token()?;
            if token.token_type == TokenType::EOF {
                tokens.push(token);
                break;
            }
            tokens.push(token);
        }

        Ok(tokens)
    }

    fn get_next_token(&mut self) -> Result<Token, LexicalError> {

        // tokenize symbols and literals first
        // iterate through source code character by character
        for c in self.source.chars() {
            // skip whitespace
            while let Some(c) = self.peek(0) {
                if c.is_whitespace() {
                    if c == '\n' {
                        self.line += 1;
                        self.column = 1;
                    } else {
                        self.column += 1;
                    }
                    self.position += 1;
                } else {
                    break;
                }
            }

            // Check for end of file
            let c = match self.peek(0) {
                Some(ch) => ch,
                None => return Ok(Token::new("".to_string(), TokenType::EOF, self.line, self.column)),
            };

            // check for symbols
            match c {
                '+' => {
                    self.position += 1;
                    self.column += 1;
                    return Ok(Token::new("+".to_string(), TokenType::PLUS, self.line, self.column - 1));
                },
                '-' => {
                    self.position += 1;
                    self.column += 1;
                    return Ok(Token::new("-".to_string(), TokenType::MINUS, self.line, self.column - 1));
                },
                '*' => {
                    self.position += 1;
                    self.column += 1;
                    return Ok(Token::new("*".to_string(), TokenType::ASTERISK, self.line, self.column - 1));
                },
                '=' => {
                    self.position += 1;
                    self.column += 1;
                    return Ok(Token::new("=".to_string(), TokenType::EQUAL, self.line, self.column - 1));
                },
                '/' => {
                    if self.peek(1) == Some('/') {
                        // it's a comment, skip to end of line
                        while let Some(next_char) = self.peek(1) {
                            if next_char == '\n' {
                                break;
                            }
                            self.position += 1;
                            self.column += 1;
                        }
                        continue; // skip adding a token for the comment
                    }
                    self.position += 1;
                    self.column += 1;
                    return Ok(Token::new("/".to_string(), TokenType::FORWARD_SLASH, self.line, self.column - 1));
                },
                '<' => {
                    let next_char = self.peek(1);
                    if next_char == Some('=') {
                        self.position += 2;
                        self.column += 2;
                        return Ok(Token::new("<=".to_string(), TokenType::LESS_EQUAL, self.line, self.column - 2));
                    } else if next_char == Some('>') {
                        self.position += 2;
                        self.column += 2;
                        return Ok(Token::new("<>".to_string(), TokenType::NOT_EQUAL, self.line, self.column - 2));
                    } else {
                        self.position += 1;
                        self.column += 1;
                        return Ok(Token::new("<".to_string(), TokenType::LESS_THAN, self.line, self.column - 1));
                    }
                }
                '>' => {
                    let next_char = self.peek(1);
                    if next_char == Some('=') {
                        self.position += 2;
                        self.column += 2;
                        return Ok(Token::new(">=".to_string(), TokenType::GREATER_EQUAL, self.line, self.column - 2));
                    } else {
                        self.position += 1;
                        self.column += 1;
                        return Ok(Token::new(">".to_string(), TokenType::GREATER_THAN, self.line, self.column - 1));
                    }
                },
                '^' => {
                    self.position += 1;
                    self.column += 1;
                    return Ok(Token::new("^".to_string(), TokenType::CARET, self.line, self.column - 1));
                },
                '&' => {
                    self.position += 1;
                    self.column += 1;
                    return Ok(Token::new("&".to_string(), TokenType::AMPERSAND, self.line, self.column - 1));
                },
                '"' => {
                    return self.handle_string_literal();
                },
                ':' => {
                    self.position += 1;
                    self.column += 1;
                    return Ok(Token::new(":".to_string(), TokenType::COLON, self.line, self.column - 1));
                }
                _ if c.is_alphabetic() || c == '_' => return self.handle_identifier(),
                _ if c.is_digit(10) =>  return self.handle_number_literal(),
                _ => return Err(LexicalError {
                    message: format!("Unexpected character: '{}'", c),
                    hint: None,
                    line: self.line,
                    column: self.column,
                    source: Some(self.source.clone()),
                }),
            }
        }

        Ok(Token::new("".to_string(), TokenType::EOF, self.line, self.column))
    }

    fn handle_string_literal(&mut self) -> Result<Token, LexicalError> {
        let start_line = self.line;
        let start_column = self.column;
        let mut lexeme = String::new();

        // consume the opening quote
        self.position += 1;
        self.column += 1;

        while let Some(c) = self.peek(0) {
            if c == '"' {
                // closing quote found
                self.position += 1;
                self.column += 1;
                return Ok(Token::new(lexeme, TokenType::STRING_LITERAL, start_line, start_column));
            } else {
                lexeme.push(c);
                self.position += 1;
                self.column += 1;
            }
        }

        // if we reach here, the string literal was not closed
        Err(LexicalError {
            message: "Your string was not closed!".to_string(),
            hint: Some("Make sure to close your string with a \"".to_string()),
            line: start_line,
            column: start_column,
            source: Some(self.source.clone()),
        })
    }

    fn handle_identifier(&mut self) -> Result<Token, LexicalError> {
        let start_line = self.line;
        let start_column = self.column;
        let mut lexeme = String::new();

        while let Some(c) = self.peek(0) {
            if c.is_alphanumeric() || c == '_' {
                lexeme.push(c);
                self.position += 1;
                self.column += 1;
            } else {
                break;
            }
        }

        let token_type = match self.token_map.get(lexeme.as_str()) {
            Some(t) => t.clone(),
            None => TokenType::IDENTIFIER,
        };

        Ok(Token::new(lexeme, token_type, start_line, start_column))
    }  

    fn handle_number_literal(&mut self) -> Result<Token, LexicalError> {
        let start_line = self.line;
        let start_column = self.column;
        let mut lexeme = String::new();

        while let Some(c) = self.peek(0) {
            if c.is_digit(10) || c == '.' {
                lexeme.push(c);
                self.position += 1;
                self.column += 1;
            } else if c == '_' {
                // ignore underscores in number literals
                self.position += 1;
                self.column += 1;
            } else if c.is_alphabetic() {
                return Err(LexicalError {
                    message: "Invalid character in number literal".to_string(),
                    hint: Some("Number literals can only contain digits and decimal points.".to_string()),
                    line: self.line,
                    column: self.column,
                    source: Some(self.source.clone()),
                });
            }
            else {
                break;
            }
        }

        Ok(Token::new(lexeme, TokenType::NUMBER_LITERAL, start_line, start_column))
    }

} 

