use crate::{Inter::cps::{Type, Value}, Lexer::lexer::TokenType};

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
    Identifier(String),
    Expression(Expr),
    Stmt(Stmt)
}

#[derive(Debug, Clone)]
pub enum Expr {
    Binary(BinaryExpr),
    Literal(Value),
}




#[derive(Debug, Clone)]
pub enum Stmt {
    If { condition: Box<Expr>, then_branch: BlockStmt, else_branch: Option<BlockStmt> },
    While { condition: Box<Expr>, body: BlockStmt },
    For { identifier: String, start: Box<Expr>, end: Box<Expr>, body: BlockStmt },
    Assignment { identifier: String, value: Box<Ast> },
    Decleration { identifier: String, type_: Type},
    Input { identifier: String }, 
    Output { target: Expr },
    Block(BlockStmt),
}

#[derive(Debug, Clone)]
pub struct BlockStmt {
    pub statements: Vec<Stmt>,
}




impl Expr {
    fn to_prefix(&self) -> String {
        match self {
            Expr::Binary(binary) => {
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
            Expr::Literal(value) => {
                match value {
                    Value::Integer(n) => n.to_string(),
                    Value::Real(f) => f.to_string(),
                    Value::String(s) => format!("\"{}\"", s),
                    Value::Boolean(b) => b.to_string(),
                    Value::Char(c) => format!("'{}'", c),
                    _ => "?".to_string(),
                }
            }
        }
    }
}

impl Stmt {
    fn to_prefix(&self, indent: usize) -> String {
        let indent_str = "  ".repeat(indent);
        match self {
            Stmt::If { condition, then_branch, else_branch } => {
                let mut result = format!("{}IF {}\n", indent_str, condition.to_prefix());
                result.push_str(&format!("{}THEN\n", indent_str));
                for stmt in &then_branch.statements {
                    result.push_str(&stmt.to_prefix(indent + 1));
                    result.push('\n');
                }
                if let Some(else_block) = else_branch {
                    result.push_str(&format!("{}ELSE\n", indent_str));
                    for stmt in &else_block.statements {
                        result.push_str(&stmt.to_prefix(indent + 1));
                        result.push('\n');
                    }
                }
                result.push_str(&format!("{}ENDIF", indent_str));
                result
            }
            Stmt::While { condition, body } => {
                let mut result = format!("{}WHILE {}\n", indent_str, condition.to_prefix());
                for stmt in &body.statements {
                    result.push_str(&stmt.to_prefix(indent + 1));
                    result.push('\n');
                }
                result.push_str(&format!("{}ENDWHILE", indent_str));
                result
            }
            Stmt::For { identifier, start, end, body } => {
                let mut result = format!("{}FOR {} = {} TO {}\n", indent_str, identifier, start.to_prefix(), end.to_prefix());
                for stmt in &body.statements {
                    result.push_str(&stmt.to_prefix(indent + 1));
                    result.push('\n');
                }
                result.push_str(&format!("{}ENDFOR", indent_str));
                result
            }
            Stmt::Assignment { identifier, value } => {
                format!("{}ASSIGN {} = {}", indent_str, identifier, value.to_prefix())
            }
            Stmt::Decleration { identifier, type_ } => {
                format!("{}DECLARE {} : {:?}", indent_str, identifier, type_)
            }
            Stmt::Input { identifier } => {
                format!("{}INPUT {}", indent_str, identifier)
            }
            Stmt::Output { target } => {
                format!("{}OUTPUT {}", indent_str, target.to_prefix())
            }
            Stmt::Block(block) => {
                let mut result = format!("{}BEGIN\n", indent_str);
                for stmt in &block.statements {
                    result.push_str(&stmt.to_prefix(indent + 1));
                    result.push('\n');
                }
                result.push_str(&format!("{}END", indent_str));
                result
            }
        }
    }
}

impl Ast {
    fn to_prefix(&self) -> String {
        match self {
            Ast::Identifier(name) => name.clone(),
            Ast::Stmt(stmt) => stmt.to_prefix(0),
            Ast::Expression(expr) => expr.to_prefix(),
        }
    }
    
    pub fn print_prefix(&self) {
        println!("{}", self.to_prefix());
    }
}
