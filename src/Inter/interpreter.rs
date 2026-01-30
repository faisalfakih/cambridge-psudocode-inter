use std::{cell::RefCell, rc::Rc};

use crate::errortype::{CPSError, ErrorType};
use crate::Inter::cps::{Environment, Type, Value};
use crate::Lexer::lexer::TokenType;
use crate::Parser::ast::{Ast, BinaryExpr, BlockStmt, Expr, Stmt};


#[derive(Debug, Clone)]
pub struct Interpreter {
    current_env: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            current_env: Environment::new_global(),
        }
    }

    pub fn interpret(&mut self, ast_nodes: Vec<Ast>) -> Result<(), CPSError> {
        for node in ast_nodes.into_iter() {
            self.evaluate_ast(node)?;
        }
        Ok(())
    }

    fn evaluate_ast(&mut self, node: Ast) -> Result<Value, CPSError> {
        match node {
            Ast::Expression(expr) => {
                self.evaluate_expr(&expr)
            },
            Ast::Identifier(name) => {
                match self.current_env
                    .borrow()
                    .get(&name) {
                        Some(value) => Ok(value),
                        None => Err(CPSError {
                            error_type: ErrorType::Runtime,
                            message: format!("Undefined identifier: {}", name),
                            hint: None,
                            line: 0,
                            column: 0,
                            source: None,
                        }),
                    }
            },
            Ast::Stmt(stmt) => {
                match self.evaluate_stmt(&stmt) {
                    Ok(_) => Ok(Value::Boolean(true)),
                    Err(e) => Err(e),
                }
            },
            _ => {
                return Err(CPSError {
                    error_type: ErrorType::Runtime,
                    message: format!("Unsupported AST node in interpreter: {:?}", node),
                    hint: None,
                    line: 0,
                    column: 0,
                    source: None,
                });
            }
        }
        // Ok(Value::Boolean(true)) // Placeholder return value
    }

    fn evaluate_stmt(&mut self, statement: &crate::Parser::ast::Stmt) -> Result<(), CPSError> {
        match statement {
            Stmt::Output { target } => self.evaluate_output_stmt(target),
            Stmt::Decleration { identifier, type_ } => self.evaluate_declaration_stmt(identifier, type_),
            Stmt::Assignment { identifier, value } => self.evaluate_assignment_stmt(identifier, value),
            Stmt::Input { identifier } => self.evaluate_input_stmt(identifier),
            Stmt::If { condition, then_branch, else_branch } => self.evaluate_if_stmt(condition, then_branch, else_branch),
            Stmt::While { condition, body } => self.evaluate_while_stmt(condition, body),
            Stmt::Block(block) => {
                for stmt in &block.statements {
                    self.evaluate_stmt(stmt)?;
                }
                Ok(())
            }
            Stmt::For { identifier, start, end, body } => self.evaluate_for(identifier, start, end, body),
            _ => {
                return Err(CPSError {
                    error_type: ErrorType::Runtime,
                    message: format!("Unsupported statement in interpreter: {:?}", statement),
                    hint: None,
                    line: 0,
                    column: 0,
                    source: None,
                });
            }

        }
    }

    fn evaluate_assignment_stmt(&mut self, identifier: &String, value: &Ast) -> Result<(), CPSError> {
        let value_expression = match value {
            Ast::Expression(expr) => expr,
            _ => {
                return Err(CPSError {
                    error_type: ErrorType::Runtime,
                    message: format!("Invalid assignment value for '{}': {:?}", identifier, value),
                    hint: None,
                    line: 0,
                    column: 0,
                    source: None,
                });
            }
        };


        let val = self.evaluate_expr(value_expression)?;

        let expected_type = self.current_env.borrow_mut().get_type(identifier)?;

        let actual_type = match &val {
            Value::Integer(_) => Type::Integer,
            Value::Real(_) => Type::Real,
            Value::String(_) => Type::String,
            Value::Boolean(_) => Type::Boolean,
            Value::Char(_) => Type::Char,
            Value::Array(_) => Type::Array(Box::new(Type::Integer), 0), // Simplified
            Value::Identifier(_) => {
                return Err(CPSError {
                    error_type: ErrorType::Runtime,
                    message: format!("Cannot assign unresolved identifier to '{}'", identifier),
                    hint: None,
                    line: 0,
                    column: 0,
                    source: None,
                });
            }
        };

        let converted_val = match (&val, &expected_type, &actual_type) {
            (Value::Real(r), Type::Integer, Type::Real) => Value::Integer(*r as i64),

            (Value::Integer(i), Type::Real, Type::Integer) => Value::Real(*i as f64),

            _ if actual_type == expected_type => val.clone(),

            _ => {
                return Err(CPSError {
                    error_type: ErrorType::Runtime,
                    message: format!(
                        "Type mismatch: cannot assign {:?} to variable '{}' of type {:?}",
                        actual_type, identifier, expected_type
                    ),
                    hint: Some("The value type must match the declared variable type".to_string()),
                    line: 0,
                    column: 0,
                    source: None,
                });
            }
        };

        
        self.current_env
            .borrow_mut()
            .set(identifier, converted_val)
            .map_err(|e| CPSError {
                error_type: ErrorType::Runtime,
                message: format!("Failed to assign value to '{}': {}", identifier, e.message),
                hint: None,
                line: 0,
                column: 0,
                source: None,
            })
    }

    fn evaluate_for(&mut self, identifier: &String, start: &Expr, end: &Expr, body: &BlockStmt) -> Result<(), CPSError> {
        let start_value = self.evaluate_expr(start)?;
        let end_value = self.evaluate_expr(end)?;

        let start_int;
        let end_int;

        match start_value {
            Value::Integer(n) => start_int = n,
            Value::Real(n) => {
                if n.fract() != 0.0 {
                    return Err(CPSError {
                        error_type: ErrorType::Runtime,
                        message: format!("FOR loop start value must be an integer, got real number: {}", n),
                        hint: None,
                        line: 0,
                        column: 0,
                        source: None,
                    });
                }
                start_int = n as i64;
            }
            _ => {
                return Err(CPSError {
                    error_type: ErrorType::Runtime,
                    message: format!("FOR loop start value must be an integer, got: {:?}", start_value),
                    hint: None,
                    line: 0,
                    column: 0,
                    source: None,
                });
            }
        }
        match end_value {
            Value::Integer(n) => end_int = n,
            Value::Real(n) => {
                if n.fract() != 0.0 {
                    return Err(CPSError {
                        error_type: ErrorType::Runtime,
                        message: format!("FOR loop start value must be an integer, got real number: {}", n),
                        hint: None,
                        line: 0,
                        column: 0,
                        source: None,
                    });
                }
                end_int = n as i64;
            }
            _ => {
                return Err(CPSError {
                    error_type: ErrorType::Runtime,
                    message: format!("FOR loop end value must be an integer, got: {:?}", end_value),
                    hint: None,
                    line: 0,
                    column: 0,
                    source: None,
                });
            }
        }

        // first declare the loop variable
        self.current_env
            .borrow_mut()
            .define(identifier.to_owned(), Value::Integer(start_int.to_owned()));

        for i in start_int.to_owned()..=end_int.to_owned() {
            self.current_env
                .borrow_mut()
                .set(identifier, Value::Integer(i))
                .map_err(|e| CPSError {
                    error_type: ErrorType::Runtime,
                    message: format!("Failed to assign value to loop variable '{}': {}", identifier, e.message),
                    hint: None,
                    line: 0,
                    column: 0,
                    source: None,
                })?;

            for stmt in &body.statements {
                self.evaluate_stmt(stmt)?;
            }
        }

        Ok(())
    }

    fn evaluate_output_stmt(&mut self, target: &Expr) -> Result<(), CPSError> {
        let value = match self.evaluate_expr(target)? {
            Value::String(s) => s,
            Value::Integer(i) => i.to_string(),
            Value::Real(r) => r.to_string(),
            Value::Boolean(b) => b.to_string(),
            Value::Char(c) => c.to_string(),
            Value::Identifier(id) => match self.current_env.borrow().get(&id) {
                Some(v) => match v {
                    Value::String(s) => s,
                    Value::Integer(i) => i.to_string(),
                    Value::Real(r) => r.to_string(),
                    Value::Boolean(b) => b.to_string(),
                    Value::Char(c) => c.to_string(),
                    _ => {
                        return Err(CPSError {
                            error_type: ErrorType::Runtime,
                            message: format!("Unsupported identifier type for output: {:?}", v),
                            hint: None,
                            line: 0,
                            column: 0,
                            source: None,
                        });
                    }
                },
                None => {
                    return Err(CPSError {
                        error_type: ErrorType::Runtime,
                        message: format!("Undefined identifier: {}", id),
                        hint: None,
                        line: 0,
                        column: 0,
                        source: None,
                    });
                }
            },
            _ => {
                return Err(CPSError {
                    error_type: ErrorType::Runtime,
                    message: format!("Unsupported output type: {:?}", target),
                    hint: None,
                    line: 0,
                    column: 0,
                    source: None,
                });
            }
        };
        println!("{}", value);
        Ok(())
    }

    fn evaluate_input_stmt(&mut self, identifier: &String) -> Result<(), CPSError> {
        use std::io::{self, Write};

        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        let input = input.trim().to_string();

        self.current_env // all inputs r treated as strings
            .borrow_mut()
            .set(identifier, Value::String(input))
            .map_err(|e| CPSError {
                error_type: ErrorType::Runtime,
                message: format!("Failed to assign input to '{}': {}", identifier, e.message),
                hint: None,
                line: 0,
                column: 0,
                source: None,
            })
    }

    fn evaluate_if_stmt(&mut self, condition: &Expr, then_branch: &BlockStmt, else_brach: &Option<BlockStmt>) -> Result<(), CPSError> {
        let cond_value = self.evaluate_expr(condition)?;
        match cond_value {
            Value::Boolean(true) => {
                for stmt in &then_branch.statements {
                    self.evaluate_stmt(stmt)?;
                }
            }
            Value::Boolean(false) => {
                if let Some(else_block) = else_brach {
                    for stmt in &else_block.statements {
                        self.evaluate_stmt(stmt)?;
                    }
                }
            }
            _ => {
                return Err(CPSError {
                    error_type: ErrorType::Runtime,
                    message: format!("Condition in IF statement did not evaluate to a boolean: {:?}", cond_value),
                    hint: None,
                    line: 0,
                    column: 0,
                    source: None,
                });
            }
        }
        Ok(())
    }

    fn evaluate_while_stmt(&mut self, condition: &Expr, body: &BlockStmt) -> Result<(), CPSError> {
        loop {
            let cond_value = self.evaluate_expr(condition)?;
            match cond_value {
                Value::Boolean(true) => {
                    self.evaluate_stmt(&Stmt::Block(body.to_owned()))?;
                }
                Value::Boolean(false) => break,
                _ => {
                    return Err(CPSError {
                        error_type: ErrorType::Runtime,
                        message: format!("Condition in WHILE statement did not evaluate to a boolean: {:?}", cond_value),
                        hint: None,
                        line: 0,
                        column: 0,
                        source: None,
                    });
                }
            }
        }
        Ok(())
    }


    fn evaluate_declaration_stmt(&mut self, identifier: &String, type_: &Type) -> Result<(), CPSError> {
        let inital_value = match type_ {
            Type::Integer => Value::Integer(0),
            Type::Real => Value::Real(0.0),
            Type::String => Value::String(String::new()),
            Type::Boolean => Value::Boolean(false),
            Type::Char => Value::Char('\0'),
            _ => {
                return Err(CPSError {
                    error_type: ErrorType::Runtime,
                    message: format!("Unsupported type for declaration: {:?}", type_),
                    hint: None,
                    line: 0,
                    column: 0,
                    source: None,
                });
            }
        };

        self.current_env
            .borrow_mut()
            .define(identifier.to_owned(), inital_value);
        Ok(())
    }


    fn evaluate_expr(&mut self, expression: &Expr) -> Result<Value, CPSError> {
        match expression {
            Expr::Binary(expr) => self.evaluate_binary(expr),
            Expr::Literal(value) => self.evaluate_literal(value),
            // _ => {
            //     return Err(CPSError {
            //         error_type: ErrorType::Runtime,
            //         message: format!("Unsupported expression in interpreter: {:?}", expression),
            //         hint: None,
            //         line: 0,
            //         column: 0,
            //         source: None,
            //     });
            // }
        }
    }

    fn evaluate_binary(&mut self, binary: &BinaryExpr) -> Result<Value, CPSError> {
        let left = self.evaluate_ast(*binary.left.clone())?;
        let right = self.evaluate_ast(*binary.right.clone())?;

        match binary.operator {
            TokenType::Plus => self.add(left, right),
            TokenType::Minus => self.subtract(left, right),
            TokenType::Asterisk => self.multiply(left, right),
            TokenType::ForwardSlash => self.divide(left, right),
            TokenType::Div => self.integer_divide(left, right),
            TokenType::Mod => self.modulo(left, right),
            TokenType::Caret => self.power(left, right),
            TokenType::Ampersand => self.concatenate(left, right),

            // Comparison operators
            TokenType::Equal => self.equal(left, right),
            TokenType::NotEqual => self.not_equal(left, right),
            TokenType::LessThan => self.less_than(left, right),
            TokenType::LessEqual => self.less_equal(left, right),
            TokenType::GreaterThan => self.greater_than(left, right),
            TokenType::GreaterEqual => self.greater_equal(left, right),

            // Logical operators
            TokenType::And => self.logical_and(left, right),
            TokenType::Or => self.logical_or(left, right),

            _ => Err(CPSError {
                error_type: ErrorType::Runtime,
                message: format!("Unsupported binary operator in interpreter: {:?}", binary.operator),
                hint: None,
                line: 0,
                column: 0,
                source: None,
            }),
        }

    }

    fn add(&self, left: Value, right: Value) -> Result<Value, CPSError> {
        match (left.clone(), right.clone()) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l + r)),
            (Value::Real(l), Value::Real(r)) => Ok(Value::Real(l + r)),
            (Value::Integer(l), Value::Real(r)) => Ok(Value::Real(l as f64 + r)),
            (Value::Real(l), Value::Integer(r)) => Ok(Value::Real(l + r as f64)),
            _ => Err(CPSError {
                error_type: ErrorType::Runtime,
                message: format!("Unsupported types for addition: {:?} + {:?}", left, right),
                hint: None,
                line: 0,
                column: 0,
                source: None,
            }),
        }
    }

    fn subtract(&self, left: Value, right: Value) -> Result<Value, CPSError> {
        match (left.clone(), right.clone()) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l - r)),
            (Value::Real(l), Value::Real(r)) => Ok(Value::Real(l - r)),
            (Value::Integer(l), Value::Real(r)) => Ok(Value::Real(l as f64 - r)),
            (Value::Real(l), Value::Integer(r)) => Ok(Value::Real(l - r as f64)),
            _ => Err(CPSError {
                error_type: ErrorType::Runtime,
                message: format!("Unsupported types for subtraction: {:?} - {:?}", left, right),
                hint: None,
                line: 0,
                column: 0,
                source: None,
            }),
        }
    }

    fn multiply(&self, left: Value, right: Value) -> Result<Value, CPSError> {
        match (left.clone(), right.clone()) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l * r)),
            (Value::Real(l), Value::Real(r)) => Ok(Value::Real(l * r)),
            (Value::Integer(l), Value::Real(r)) => Ok(Value::Real(l as f64 * r)),
            (Value::Real(l), Value::Integer(r)) => Ok(Value::Real(l * r as f64)),
            _ => Err(CPSError {
                error_type: ErrorType::Runtime,
                message: format!("Unsupported types for multiplication: {:?} * {:?}", left, right),
                hint: None,
                line: 0,
                column: 0,
                source: None,
            }),
        }
    }

    fn divide(&self, left: Value, right: Value) -> Result<Value, CPSError> {
        let is_zero = match &right {
            Value::Integer(n) => *n == 0,
            Value::Real(f) => *f == 0.0,
            _ => false,
        };

        if is_zero {
            return Err(
                CPSError {
                    error_type: ErrorType::Runtime,
                    message: "Division by zero".to_string(),
                    hint: None,
                    line: 0,
                    column: 0,
                    source: None,
                }
            );
        }

        match (left.clone(), right.clone()) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Real(a as f64 / b as f64)),
            (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a / b)),
            (Value::Integer(a), Value::Real(b)) => Ok(Value::Real(a as f64 / b)),
            (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a / b as f64)),
            _ => Err(CPSError {
                error_type: ErrorType::Runtime,
                message: format!("Unsupported types for division: {:?} / {:?}", left, right),
                hint: None,
                line: 0,
                column: 0,
                source: None,
            }),
        }

    }

    fn integer_divide(&self, left: Value, right: Value) -> Result<Value, CPSError> {
        match (left.clone(), right.clone()) {
            (Value::Integer(a), Value::Integer(b)) => {
                if b == 0 {
                    return Err(CPSError {
                        error_type: ErrorType::Runtime,
                        message: "Division by zero".to_string(),
                        hint: None,
                        line: 0,
                        column: 0,
                        source: None,
                    });
                }
                Ok(Value::Integer(a / b))
            }
            (Value::Real(a), Value::Real(b)) => {
                if b == 0.0 {
                    return Err(CPSError {
                        error_type: ErrorType::Runtime,
                        message: "Division by zero".to_string(),
                        hint: None,
                        line: 0,
                        column: 0,
                        source: None,
                    });
                }
                Ok(Value::Real((a / b).floor()))
            }
            _ => Err(CPSError {
                error_type: ErrorType::Runtime,
                message: format!("Unsupported types for integer division: {:?} // {:?}", left, right),
                hint: None,
                line: 0,
                column: 0,
                source: None,
            }),
        }
    }

    fn modulo(&self, left: Value, right: Value) -> Result<Value, CPSError> {
        match (left.clone(), right.clone()) {
            (Value::Integer(a), Value::Integer(b)) => {
                if b == 0 {
                    return Err(CPSError {
                        error_type: ErrorType::Runtime,
                        message: "Modulo by zero".to_string(),
                        hint: None,
                        line: 0,
                        column: 0,
                        source: None,
                    });
                }
                Ok(Value::Integer(a % b))
            },
            (Value::Real(a), Value::Real(b)) => {
                if b == 0.0 {
                    return Err(CPSError {
                        error_type: ErrorType::Runtime,
                        message: "Modulo by zero".to_string(),
                        hint: None,
                        line: 0,
                        column: 0,
                        source: None,
                    });
                }
                Ok(Value::Real(a % b))
            },
            _ => Err(CPSError {
                error_type: ErrorType::Runtime,
                message: format!("Unsupported types for modulo: {:?} % {:?}", left, right),
                hint: None,
                line: 0,
                column: 0,
                source: None,
            }),
        }
    }

    fn power(&self, left: Value, right: Value) -> Result<Value, CPSError> {
        match (left.clone(), right.clone()) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a.pow(b as u32))),
            (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a.powf(b))),
            (Value::Integer(a), Value::Real(b)) => Ok(Value::Real((a as f64).powf(b))),
            (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a.powf(b as f64))),
            _ => Err(CPSError {
                error_type: ErrorType::Runtime,
                message: format!("Unsupported types for power: {:?} ^ {:?}", left, right),
                hint: None,
                line: 0,
                column: 0,
                source: None,
            }),
        }
    }

    fn concatenate(&self, left: Value, right: Value) -> Result<Value, CPSError> {
        match (left.clone(), right.clone()) {
            (Value::String(l), Value::String(r)) => Ok(Value::String(l + &r)),
            _ => Err(CPSError {
                error_type: ErrorType::Runtime,
                message: format!("Unsupported types for concatenation: {:?} & {:?}", left, right),
                hint: None,
                line: 0,
                column: 0,
                source: None,
            }),
        }
    }

    fn equal(&self, left: Value, right: Value) -> Result<Value, CPSError> {
        match (left.clone(), right.clone()) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Boolean(l == r)),
            (Value::Real(l), Value::Real(r)) => Ok(Value::Boolean(l == r)),
            (Value::String(l), Value::String(r)) => Ok(Value::Boolean(l == r)),
            (Value::Boolean(l), Value::Boolean(r)) => Ok(Value::Boolean(l == r)),
            (Value::Char(l), Value::Char(r)) => Ok(Value::Boolean(l == r)),
            _ => Err(CPSError {
                error_type: ErrorType::Runtime,
                message: format!("Unsupported types for equality comparison: {:?} == {:?}", left, right),
                hint: None,
                line: 0,
                column: 0,
                source: None,
            }),
        }
    }

    fn not_equal(&self, left: Value, right: Value) -> Result<Value, CPSError> {
        match (left.clone(), right.clone()) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Boolean(l != r)),
            (Value::Real(l), Value::Real(r)) => Ok(Value::Boolean(l != r)),
            (Value::String(l), Value::String(r)) => Ok(Value::Boolean(l != r)),
            (Value::Boolean(l), Value::Boolean(r)) => Ok(Value::Boolean(l != r)),
            (Value::Char(l), Value::Char(r)) => Ok(Value::Boolean(l != r)),
            _ => Err(CPSError {
                error_type: ErrorType::Runtime,
                message: format!("Unsupported types for inequality comparison: {:?} != {:?}", left, right),
                hint: None,
                line: 0,
                column: 0,
                source: None,
            }),
        }
    }

    fn less_than(&self, left: Value, right: Value) -> Result<Value, CPSError> {
        match (left.clone(), right.clone()) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Boolean(l < r)),
            (Value::Real(l), Value::Real(r)) => Ok(Value::Boolean(l < r)),
            (Value::Integer(l), Value::Real(r)) => Ok(Value::Boolean((l as f64) < r)),
            (Value::Real(l), Value::Integer(r)) => Ok(Value::Boolean(l < (r as f64))),
            _ => Err(CPSError {
                error_type: ErrorType::Runtime,
                message: format!("Unsupported types for less-than comparison: {:?} < {:?}", left, right),
                hint: None,
                line: 0,
                column: 0,
                source: None,
            }),
        }
    }

    fn less_equal(&self, left: Value, right: Value) -> Result<Value, CPSError> {
        match (left.clone(), right.clone()) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Boolean(l <= r)),
            (Value::Real(l), Value::Real(r)) => Ok(Value::Boolean(l <= r)),
            (Value::Integer(l), Value::Real(r)) => Ok(Value::Boolean((l as f64) <= r)),
            (Value::Real(l), Value::Integer(r)) => Ok(Value::Boolean(l <= (r as f64))),
            _ => Err(CPSError {
                error_type: ErrorType::Runtime,
                message: format!("Unsupported types for less-than-or-equal comparison: {:?} <= {:?}", left, right),
                hint: None,
                line: 0,
                column: 0,
                source: None,
            }),
        }
    }

    fn greater_than(&self, left: Value, right: Value) -> Result<Value, CPSError> {
        match (left.clone(), right.clone()) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Boolean(l > r)),
            (Value::Real(l), Value::Real(r)) => Ok(Value::Boolean(l > r)),
            (Value::Integer(l), Value::Real(r)) => Ok(Value::Boolean((l as f64) > r)),
            (Value::Real(l), Value::Integer(r)) => Ok(Value::Boolean(l > (r as f64))),
            _ => Err(CPSError {
                error_type: ErrorType::Runtime,
                message: format!("Unsupported types for greater-than comparison: {:?} > {:?}", left, right),
                hint: None,
                line: 0,
                column: 0,
                source: None,
            }),
        }
    }

    fn greater_equal(&self, left: Value, right: Value) -> Result<Value, CPSError> {
        match (left.clone(), right.clone()) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Boolean(l >= r)),
            (Value::Real(l), Value::Real(r)) => Ok(Value::Boolean(l >= r)),
            (Value::Integer(l), Value::Real(r)) => Ok(Value::Boolean((l as f64) >= r)),
            (Value::Real(l), Value::Integer(r)) => Ok(Value::Boolean(l >= (r as f64))),
            _ => Err(CPSError {
                error_type: ErrorType::Runtime,
                message: format!("Unsupported types for greater-than-or-equal comparison: {:?} >= {:?}", left, right),
                hint: None,
                line: 0,
                column: 0,
                source: None,
            }),
        }
    }

    fn logical_and(&self, left: Value, right: Value) -> Result<Value, CPSError> {
        match (left.clone(), right.clone()) {
            (Value::Boolean(l), Value::Boolean(r)) => Ok(Value::Boolean(l && r)),
            _ => Err(CPSError {
                error_type: ErrorType::Runtime,
                message: format!("Unsupported types for logical AND: {:?} AND {:?}", left, right),
                hint: None,
                line: 0,
                column: 0,
                source: None,
            }),
        }
    }

    fn logical_or(&self, left: Value, right: Value) -> Result<Value, CPSError> {
        match (left.clone(), right.clone()) {
            (Value::Boolean(l), Value::Boolean(r)) => Ok(Value::Boolean(l || r)),
            _ => Err(CPSError {
                error_type: ErrorType::Runtime,
                message: format!("Unsupported types for logical OR: {:?} OR {:?}", left, right),
                hint: None,
                line: 0,
                column: 0,
                source: None,
            }),
        }
    }





    fn evaluate_literal(&self, lit: &Value) -> Result<Value, CPSError> {
        match lit {
            Value::Integer(_) |
            Value::Real(_) |
            Value::String(_) |
            Value::Boolean(_) |
            Value::Char(_) => {},
            Value::Identifier(iden) => {
                match self.current_env 
                    .borrow()
                    .get(iden) {
                        Some(value) => return Ok(value),
                        None => {
                            return Err(CPSError {
                                error_type: ErrorType::Runtime,
                                message: format!("Undefined identifier: {}", iden),
                                hint: None,
                                line: 0,
                                column: 0,
                                source: None,
                            });
                        }
                    }
            }
            _ => {
                return Err(CPSError {
                    error_type: ErrorType::Runtime,
                    message: format!("Unsupported literal in interpreter: {:?}", lit),
                    hint: None,
                    line: 0,
                    column: 0,
                    source: None,
                });
            }
        }
        Ok(lit.clone())
    }


}
