use crate::Lexer::lexer::TokenType;

#[derive(Debug, Clone)]
pub struct Operator {
    pub precedence: Precedence,
    pub position: Position,
    pub associativity: Associativity,
}

pub type Precedence = u16;

#[derive(Debug, Clone)]
pub enum Position {
    Prefix,
    Infix,
    Postfix,
}

#[derive(Debug, Clone)]
pub enum Associativity {
    Left,
    Right,
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub left: Box<Ast>,
    pub operator: TokenType,
    pub right: Box<Ast>,
}

#[derive(Debug, Clone)]
pub enum Ast {
    Number(f64),
    Identifier(String),
    BinaryOp(BinaryExpr),
}

impl Ast {
    fn to_prefix(&self) -> String {
        match self {
            Ast::Number(n) => n.to_string(),
            Ast::Identifier(name) => name.clone(),
            Ast::BinaryOp(binary) => {
                let op_str = match binary.operator {
                    TokenType::Plus => "+",
                    TokenType::Minus => "-",
                    TokenType::Asterisk => "*",
                    TokenType::ForwardSlash => "/",
                    TokenType::Caret => "^",
                    TokenType::And => "AND",
                    TokenType::Or => "OR",
                    TokenType::Div => "DIV",
                    TokenType::Mod => "MOD",
                    TokenType::Equal => "=",
                    TokenType::NotEqual => "<>",
                    TokenType::LessThan => "<",
                    TokenType::LessEqual => "<=",
                    TokenType::GreaterThan => ">",
                    TokenType::GreaterEqual => ">=",
                    _ => "?",
                };
                format!(
                    "({} {} {})",
                    op_str,
                    binary.left.to_prefix(),
                    binary.right.to_prefix()
                )
            }
        }
    }
    
    pub fn print_prefix(&self) {
        println!("{}", self.to_prefix());
    }
}
