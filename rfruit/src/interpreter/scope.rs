use crate::{builtins, AnyOperator, FruError, FruValue, Identifier, OperatorIdentifier};

use crate::interpreter::FruObject;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub struct Scope {
    pub variables: RefCell<HashMap<Identifier, FruValue>>,
    pub operators: RefCell<HashMap<OperatorIdentifier, AnyOperator>>,
    parent: ScopeAncestor,
}

enum ScopeAncestor {
    None,
    Parent(Rc<Scope>),
    Object {
        object: FruObject,
        parent: Rc<Scope>,
    },
}

impl Scope {
    pub fn new_global() -> Rc<Scope> {
        Rc::new(Scope {
            variables: RefCell::new(builtins::builtin_functions()),
            operators: RefCell::new(builtins::builtin_operators()),
            parent: ScopeAncestor::None,
        })
    }

    pub fn new_with_parent(parent: Rc<Scope>) -> Rc<Scope> {
        Rc::new(Scope {
            variables: RefCell::new(HashMap::new()),
            operators: RefCell::new(HashMap::new()),
            parent: ScopeAncestor::Parent(parent),
        })
    }

    pub fn new_with_object_then_parent(object: FruObject, parent: Rc<Scope>) -> Rc<Scope> {
        Rc::new(Scope {
            variables: RefCell::new(HashMap::new()),
            operators: RefCell::new(HashMap::new()),
            parent: ScopeAncestor::Object { object, parent },
        })
    }

    pub fn get_variable(&self, ident: Identifier) -> Result<FruValue, FruError> {
        if let Some(var) = self.variables.borrow().get(&ident) {
            Ok(var.clone())
        } else {
            self.parent.get_variable(ident)
        }
    }

    pub fn let_variable(&self, ident: Identifier, value: FruValue) -> Result<(), FruError> {
        if self.variables.borrow().contains_key(&ident) {
            return FruError::new_unit(format!("variable {:?} already exists", ident));
        }

        self.variables.borrow_mut().insert(ident, value);
        Ok(())
    }

    pub fn set_variable(&self, path: &[Identifier], value: FruValue) -> Result<(), FruError> {
        assert_ne!(path.len(), 0);

        let mut to_set;

        if let Some(v) = self.variables.borrow_mut().get_mut(&path[0]) {
            if path.len() == 1 {
                *v = value;
                return Ok(());
            } else {
                to_set = v.clone();
            }
        } else {
            return self.parent.set_variable(path, value);
        }

        to_set.set_field(&path[1..path.len()], value, false)
    }

    pub fn get_operator(&self, ident: OperatorIdentifier) -> Result<AnyOperator, FruError> {
        if let Some(op) = self.operators.borrow().get(&ident) {
            Ok(op.clone())
        } else {
            match &self.parent {
                ScopeAncestor::None => Err(FruError::new(format!(
                    "operator {:?} does not exist",
                    ident
                ))),
                ScopeAncestor::Parent(parent) | ScopeAncestor::Object { parent, .. } => {
                    parent.get_operator(ident)
                }
            }
        }
    }

    pub fn set_operator(&self, ident: OperatorIdentifier, op: AnyOperator) {
        self.operators.borrow_mut().insert(ident, op);
    }
}

impl ScopeAncestor {
    pub fn get_variable(&self, ident: Identifier) -> Result<FruValue, FruError> {
        match self {
            ScopeAncestor::None => {
                FruError::new_val(format!("variable `{:?}` does not exist", ident))
            }
            ScopeAncestor::Parent(parent) => parent.get_variable(ident),
            ScopeAncestor::Object { object, parent } => object
                .get_field(ident)
                .or_else(|_| parent.get_variable(ident)),
        }
    }

    pub fn set_variable(&self, path: &[Identifier], value: FruValue) -> Result<(), FruError> {
        match self {
            ScopeAncestor::None => {
                FruError::new_unit(format!("variable `{:?}` does not exist", path[0]))
            }
            ScopeAncestor::Parent(parent) => parent.set_variable(path, value),
            ScopeAncestor::Object { object, parent } => object
                .set_field(path, value.clone(), true)
                .or_else(|_| parent.set_variable(path, value)),
        }
    }
}
