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
    Array(Vec<Value>),
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
    bindings: HashMap<String, Value>,
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

    pub fn get_type(&mut self, name: &str) -> Result<Type, CPSError> {
        if let Some(value) = self.bindings.get(name) {
            let var_type = match value {
                Value::Integer(_) => Type::Integer,
                Value::Real(_) => Type::Real,
                Value::String(_) => Type::String,
                Value::Boolean(_) => Type::Boolean,
                Value::Char(_) => Type::Char,
                // Value::Array(_) => Type::Array(), // Placeholder
                Value::Identifier(_) => Type::String, // Placeholder
                Value::Function(_) => Type::Function,
                _ => {
                    return Err(CPSError {
                        error_type: crate::errortype::ErrorType::Runtime,
                        message: format!("Cannot determine type of variable '{}'", name),
                        hint: Some("Check the variable's value.".to_string()),
                        line: 0,
                        column: 0,
                        source: None,
                    });
                }
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
