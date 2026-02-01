use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use crate::errortype::CPSError;
use crate::Parser::ast::{BlockStmt, Expr, Stmt};


#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Integer,
    Real,
    String,
    Boolean,
    Char,
    Function,
    Array(ArrayType),
    Record(String), 
    Enum(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ArrayType {
    pub lower_bound: Box<Expr>,
    pub upper_bound: Box<Expr>,
    pub base_type: Box<Type>,
}

// Runtime values (actual data)
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Integer(i64),
    Real(f64),
    String(String),
    Boolean(bool),
    Char(char),
    Array { array: Vec<Value>, lower_bound: usize } ,
    Identifier(String),
    Function(Function),
    // Record(HashMap<String, Value>),
    // Enum { type_name: String, variant: String },
    // Null,  
}

pub enum FunctionType {
    UserDefined(Function),
    Builtin(BuiltinFunction),
}

#[derive(Clone, Debug, PartialEq)]
pub struct BuiltinFunction {
    pub name: String,
    pub parameters: Vec<(String, Type)>,
    pub return_type: Option<Type>,
    pub implementation: fn(Vec<Value>) -> Result<Option<Value>, CPSError>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub parameters: Vec<(String, Type)>,
    pub return_type: Option<Type>,
    pub body: BlockStmt
}


#[derive(Debug, Clone)]
pub struct Environment {
    pub bindings: HashMap<String, Value>,
    parent: Option<Rc<RefCell<Environment>>>
}

impl Environment {
    pub fn new_global() -> Rc<RefCell<Self>> {
        let global = Rc::new(RefCell::new(Environment {
            bindings: HashMap::new(),
            parent: None,
        }));
        
        // declare builtin functions here


        global
    }

    fn register_builtins(env: Rc<RefCell<Environment>>) {
        env.borrow_mut()
            .define("RIGHT".to_string(), Value::Function(Function {
                parameters: vec![
                    ("string".to_string(), Type::String),
                    ("length".to_string(), Type::Integer),
                ],
                return_type: Some(Type::String),
                body: BlockStmt { statements: vec![] },
            }));
    }

    pub fn new_child(parent: Rc<RefCell<Environment>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Environment {
            bindings: HashMap::new(),
            parent: Some(parent),
        }))
    }


    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(value) = self.bindings.get(name) {
            return Some(value.clone());
        }

        match &self.parent {
            Some(parent_rc) => parent_rc.borrow().get(name),
            None => {
                None
            },
        }
    }

    pub fn set(&mut self, name: &str, value: Value) -> Result<(), CPSError> {
        if self.bindings.contains_key(name) {
            self.bindings.insert(name.to_string(), value);
            return Ok(());
        }

        match &self.parent {
            Some(parent_rc) => parent_rc.borrow_mut().set(name, value),
            None => Err(CPSError {
                error_type: crate::errortype::ErrorType::Runtime,
                message: format!("Undefined variable '{}'", name),
                hint: Some("Check if the variable is declared before use.".to_string()),
                line: 0,
                column: 0,
                source: None,
            }),
        }
    }

    pub fn set_array_element(&mut self, name: &str, index: usize, value: Value) -> Result<(), CPSError> {
        if let Some(current_value) = self.bindings.get_mut(name) {
            match current_value {
                Value::Array { array, lower_bound } => {
                    if index < *lower_bound {
                        let current_lb = *lower_bound;
                        return Err(CPSError {
                            error_type: crate::errortype::ErrorType::Runtime,
                            message: format!(
                                "Array index {} is below lower bound {} for '{}'",
                                index, lower_bound, name
                            ),
                            hint: Some(format!("Valid indices range from {} to {}", lower_bound, current_lb + array.len() - 1)),
                            line: 0,
                            column: 0,
                            source: None,
                        });
                    }

                    let array_index = index - *lower_bound;

                    if array_index >= array.len() {
                        let current_lb = *lower_bound;
                        return Err(CPSError {
                            error_type: crate::errortype::ErrorType::Runtime,
                            message: format!(
                                "Array index {} is out of bounds for '{}' (length: {})",
                                index, name, array.len()
                            ),
                            hint: Some(format!("Valid indices range from {} to {}", lower_bound, current_lb + array.len() - 1)),
                            line: 0,
                            column: 0,
                            source: None,
                        });
                    }

                    array[array_index] = value;
                    return Ok(());
                }
                _ => {
                    return Err(CPSError {
                        error_type: crate::errortype::ErrorType::Runtime,
                        message: format!("Variable '{}' is not an array", name),
                        hint: Some("Array indexing can only be used on array variables".to_string()),
                        line: 0,
                        column: 0,
                        source: None,
                    });
                }
            }
        }

        // If not found in current scope, check parent
        match &self.parent {
            Some(parent_rc) => parent_rc.borrow_mut().set_array_element(name, index, value),
            None => Err(CPSError {
                error_type: crate::errortype::ErrorType::Runtime,
                message: format!("Variable '{}' not found", name),
                hint: Some("Ensure the variable is declared before use".to_string()),
                line: 0,
                column: 0,
                source: None,
            }),
        }
    }


    pub fn get_type(&mut self, name: &str) -> Result<Type, CPSError> {
        if let Some(value) = self.bindings.get(name) {
            let var_type = match value {
                Value::Integer(_) => Type::Integer,
                Value::Real(_) => Type::Real,
                Value::String(_) => Type::String,
                Value::Boolean(_) => Type::Boolean,
                Value::Char(_) => Type::Char,
                Value::Array { array, lower_bound } => {
                    Type::Array(ArrayType { 
                    lower_bound: Box::new(Expr::Literal(Value::Integer(*lower_bound as i64))),
                    upper_bound: Box::new(Expr::Literal(Value::Integer((array.len() + *lower_bound - 1) as i64))),
                    base_type: Box::new(if let Some(first_elem) = array.first() {
                        match first_elem {
                            Value::Integer(_) => Type::Integer,
                            Value::Real(_) => Type::Real,
                            Value::String(_) => Type::String,
                            Value::Boolean(_) => Type::Boolean,
                            Value::Char(_) => Type::Char,
                            _ => {
                                return Err(CPSError {
                                    error_type: crate::errortype::ErrorType::Runtime,
                                    message: format!("Unsupported array element type for variable '{}'", name),
                                    hint: Some("Check the array's element types.".to_string()),
                                    line: 0,
                                    column: 0,
                                    source: None,
                                });
                            }
                        }
                    } else {
                        return Err(CPSError {
                            error_type: crate::errortype::ErrorType::Runtime,
                            message: format!("Cannot determine base type of empty array for variable '{}'", name),
                            hint: Some("Ensure the array is not empty.".to_string()),
                            line: 0,
                            column: 0,
                            source: None,
                        });
                    }

                    )
                })},
                Value::Identifier(_) => Type::String, // Placeholder
                Value::Function(_) => Type::Function,
                // _ => {
                //     return Err(CPSError {
                //         error_type: crate::errortype::ErrorType::Runtime,
                //         message: format!("Cannot determine type of variable '{}'", name),
                //         hint: Some("Check the variable's value.".to_string()),
                //         line: 0,
                //         column: 0,
                //         source: None,
                //     });
                // }
            };
            return Ok(var_type);
        }

        match &self.parent {
            Some(parent_rc) => parent_rc.borrow_mut().get_type(name),
            None => Err(CPSError {
                error_type: crate::errortype::ErrorType::Runtime,
                message: format!("Undefined variable '{}'", name),
                hint: Some("Check if the variable is declared before use.".to_string()),
                line: 0,
                column: 0,
                source: None,
            }),
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.bindings.insert(name, value);
    }

}
