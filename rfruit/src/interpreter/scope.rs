use super::{
    builtin_functions::builtin_functions, builtin_operators::builtin_operators, AnyOperator,
    FruError, FruValue, Identifier, OperatorIdentifier,
};

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Scope {
    pub variables: RefCell<HashMap<Identifier, FruValue>>,
    pub operators: RefCell<HashMap<OperatorIdentifier, AnyOperator>>,
    pub parent: Option<Rc<Scope>>,
}

impl Scope {
    pub fn new_global() -> Rc<Scope> {
        Rc::new(Scope {
            variables: RefCell::new(builtin_functions()),
            operators: RefCell::new(builtin_operators()),
            parent: None,
        })
    }

    pub fn new_with_parent(parent: Rc<Scope>) -> Rc<Scope> {
        Rc::new(Scope {
            variables: RefCell::new(HashMap::new()),
            operators: RefCell::new(HashMap::new()),
            parent: Some(parent),
        })
    }

    pub fn get_variable(&self, ident: Identifier) -> Result<FruValue, FruError> {
        if let Some(var) = self.variables.borrow().get(&ident) {
            Ok(var.clone())
        } else if let Some(parent) = &self.parent {
            parent.get_variable(ident)
        } else {
            FruError::new_err(format!("variable `{:?}` does not exist", ident))
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

    pub fn set_variable(&self, path: &[Identifier], value: FruValue) -> Result<(), FruError> {
        if let Some(v) = self.variables.borrow_mut().get_mut(&path[0]) {
            if path.len() == 1 {
                *v = value;
            } else {
                v.set_field(&path[1..path.len()], value)?;
            }
        } else {
            return Err(FruError::new(format!(
                "variable {:?} does not exist",
                path[0]
            )));
        }

        Ok(())
    }

    pub fn get_operator(&self, ident: OperatorIdentifier) -> Result<AnyOperator, FruError> {
        if let Some(op) = self.operators.borrow().get(&ident) {
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

    pub fn set_operator(&self, ident: OperatorIdentifier, op: AnyOperator) {
        self.operators.borrow_mut().insert(ident, op);
    }
}
