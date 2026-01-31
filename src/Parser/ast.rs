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

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub left: Box<Ast>,
    pub operator: TokenType,
    pub right: Box<Ast>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ast {
    Identifier(String),
    Expression(Expr),
    Stmt(Stmt)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binary(BinaryExpr),
    Literal(Value),
    Call { name: String, arguments: Vec<Expr> },
}




#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    If { condition: Box<Expr>, then_branch: BlockStmt, else_branch: Option<BlockStmt> },
    While { condition: Box<Expr>, body: BlockStmt },
    For { identifier: String, start: Box<Expr>, end: Box<Expr>, body: BlockStmt },
    Assignment { identifier: String, value: Box<Ast> },
    Decleration { identifier: String, type_: Type},
    Input { identifier: String }, 
    Output { target: Expr },
    Block(BlockStmt),
    Procedure {
        name: String,
        parameters: Vec<(String, Type)>,
        body: BlockStmt,
    },
    Function {
        name: String,
        parameters: Vec<(String, Type)>,
        return_type: Type,
        body: BlockStmt,
    },
    Return { value: Box<Expr> },
    Call { name: String, arguments: Vec<Expr> },
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockStmt {
    pub statements: Vec<Stmt>,
}

impl BlockStmt {
    pub fn new(statements: Vec<Stmt>) -> Self {
        BlockStmt { statements }
    }
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
            Expr::Call { name, arguments } => {
                let args: Vec<String> = arguments.iter().map(|arg| arg.to_prefix()).collect();
                format!("CALL {}({})", name, args.join(", "))
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
            Stmt::Procedure { name, parameters, body } => {
                let mut result = format!("{}PROCEDURE {}(", indent_str, name);
                let params: Vec<String> = parameters.iter().map(|(n, t)| format!("{}: {:?}", n, t)).collect();
                result.push_str(&params.join(", "));
                result.push_str(")\n");
                for stmt in &body.statements {
                    result.push_str(&stmt.to_prefix(indent + 1));
                    result.push('\n');
                }
                result.push_str(&format!("{}ENDPROCEDURE", indent_str));
                result
            }
            Stmt::Function { name, parameters, return_type, body } => {
                let mut result = format!("{}FUNCTION {}(", indent_str, name);
                let params: Vec<String> = parameters.iter().map(|(n, t)| format!("{}: {:?}", n, t)).collect();
                result.push_str(&params.join(", "));
                result.push_str(&format!(") : {:?}\n", return_type));
                for stmt in &body.statements {
                    result.push_str(&stmt.to_prefix(indent + 1));
                    result.push('\n');
                }
                result.push_str(&format!("{}ENDFUNCTION", indent_str));
                result
            }
            Stmt::Call { name, arguments } => {
                let args: Vec<String> = arguments.iter().map(|arg| arg.to_prefix()).collect();
                format!("{}CALL {}({})", indent_str, name, args.join(", "))
            }
            Stmt::Return { value } => {
                format!("{}RETURN {:?}", indent_str, *value)
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
