use super::{AnyOperator, FruError, FruValue, Identifier, OperatorIdentifier};
use crate::interpreter::builtins::builtin_operators;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Scope {
    pub variables: RefCell<HashMap<Identifier, FruValue>>,
    pub operators: HashMap<OperatorIdentifier, AnyOperator>,
    pub parent: Option<Rc<Scope>>,
}

impl Scope {
    pub fn new_global() -> Rc<Scope> {
        Rc::new(Scope {
            variables: RefCell::new(HashMap::new()),
            operators: builtin_operators(),
            parent: None,
        })
    }

    pub fn new_with_parent(parent: Rc<Scope>) -> Rc<Scope> {
        Rc::new(Scope {
            variables: RefCell::new(HashMap::new()),
            operators: HashMap::new(),
            parent: Some(parent),
        })
    }

    pub fn get_variable(&self, ident: Identifier) -> Result<FruValue, FruError> {
        match (self.variables.borrow().get(&ident), &self.parent) {
            (Some(var), _) => Ok(var.clone()),
            (_, Some(parent)) => parent.get_variable(ident),
            _ => FruError::new_err(format!("variable {:?} does not exist", ident)),
        }
    }

    pub fn let_variable(&self, ident: Identifier, value: FruValue) -> Result<(), FruError> {
        if self.variables.borrow().contains_key(&ident) {
            return Err(FruError::new(format!(
                "variable {:?} already exists",
                ident
            )));
        }

        self.variables.borrow_mut().insert(ident, value);
        Ok(())
    }

    pub fn set_variable(&self, ident: Identifier, value: FruValue) -> Result<(), FruError> {
        if !self.variables.borrow().contains_key(&ident) {
            return Err(FruError::new(format!(
                "variable {:?} does not exist",
                ident
            )));
        }

        self.variables.borrow_mut().insert(ident, value);
        Ok(())
    }

    pub fn get_operator(&self, ident: OperatorIdentifier) -> Result<AnyOperator, FruError> {
        if let Some(op) = self.operators.get(&ident) {
            Ok(op.clone())
        } else if let Some(parent) = &self.parent {
            parent.get_operator(ident)
        } else {
            Err(FruError::new(format!(
                "operator {:?} does not exist",
                ident
            )))
        }
    }
}
