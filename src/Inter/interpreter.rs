use std::{cell::RefCell, rc::Rc};

use crate::errortype::{CPSError, ErrorType};
use crate::Inter::cps::{Environment, Type, Value};
use crate::Lexer::lexer::TokenType;
use crate::Parser::ast::{Ast, BinaryExpr, Expr, Stmt};


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
            Ast::Number(n) => {
                Ok(Value::Real(n))
            }
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
            Stmt::Assignment { identifier, value } => {
                let val = self.evaluate_ast(*value.clone())?;
                self.current_env
                    .borrow_mut()
                    .set(identifier, val)
                    .map_err(|e| CPSError {
                        error_type: ErrorType::Runtime,
                        message: format!("Failed to assign value to '{}': {}", identifier, e.message),
                        hint: None,
                        line: 0,
                        column: 0,
                        source: None,
                    })
            }
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
            }
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
        Ok(lit.clone())
    }


}
