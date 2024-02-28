use super::{FruError, FruStatement, Identifier, Scope, StatementSignal};
use std::fmt::Debug;

use std::rc::Rc;

pub type TFnBuiltin = dyn Fn(Vec<FruValue>) -> Result<FruValue, FruError>;
pub type TOpBuiltin = dyn Fn(FruValue, FruValue) -> Result<FruValue, FruError>;

#[derive(Clone)]
pub enum FruValue {
    // ---primitives---
    None,
    Number(f64),
    Bool(bool),
    String(String),

    // ---function---
    Function(AnyFunction),
    // ---collections---
    // List(Vec<FruValue>),
    // Map(HashMap<Identifier, FruValue>),
    // Set(HashSet<FruValue>),

    // ---oop---
    // StructType(FruStructType),
    // StructObject(FruStructObject),
}

#[derive(Clone)]
pub enum AnyFunction {
    Function(Rc<FruFunction>),
    BuiltinFunction(Rc<TFnBuiltin>),
}

#[derive(Clone)]
pub enum AnyOperator {
    Operator {
        left_ident: Identifier,
        right_ident: Identifier,
        body: Rc<FruStatement>,
        scope: Rc<Scope>,
    },
    BuiltinOperator(Rc<TOpBuiltin>),
}

pub struct FruFunction {
    pub argument_idents: Vec<Identifier>,
    pub body: Rc<FruStatement>,
    pub scope: Rc<Scope>,
}

impl FruValue {
    pub fn get_type_identifier(&self) -> Identifier {
        match self {
            FruValue::None => Identifier::for_none(),
            FruValue::Number(_) => Identifier::for_number(),
            FruValue::Bool(_) => Identifier::for_bool(),
            FruValue::String(_) => Identifier::for_string(),
            FruValue::Function(_) => Identifier::for_function(),
        }
    }
}

impl FruFunction {
    pub fn call(&self, args: Vec<FruValue>) -> Result<FruValue, FruError> {
        if self.argument_idents.len() != args.len() {
            return FruError::new_err(format!(
                "expected {} arguments, got {}",
                self.argument_idents.len(),
                args.len()
            ));
        }

        let new_scope = Scope::new_with_parent(self.scope.clone());

        for (ident, value) in self.argument_idents.iter().zip(args.iter()) {
            new_scope
                .let_variable(*ident, value.clone())
                .expect("should NEVER happen XD :)");
        }

        let res = self.body.execute(new_scope)?;
        match res {
            StatementSignal::Nah => Ok(FruValue::None),
            StatementSignal::Return(v) => Ok(v),
            other => FruError::new_err(format!("unexpected signal {:?}", other)),
        }
    }
}

impl AnyFunction {
    pub fn call(&self, args: Vec<FruValue>) -> Result<FruValue, FruError> {
        match self {
            AnyFunction::Function(func) => func.call(args),
            AnyFunction::BuiltinFunction(func) => func(args),
        }
    }
}

impl AnyOperator {
    pub fn operate(&self, left_val: FruValue, right_val: FruValue) -> Result<FruValue, FruError> {
        match self {
            AnyOperator::Operator {
                left_ident,
                right_ident,
                body,
                scope,
            } => {
                let mut new_scope = Scope::new_with_parent(scope.clone());
                new_scope.let_variable(*left_ident, left_val)?;
                new_scope.let_variable(*right_ident, right_val)?;

                let res = body.execute(new_scope)?;

                match res {
                    StatementSignal::Nah => Ok(FruValue::None),
                    StatementSignal::Return(v) => Ok(v),
                    other => FruError::new_err(format!("unexpected signal {:?}", other)),
                }
            }
            AnyOperator::BuiltinOperator(op) => op(left_val, right_val),
        }
    }
}

impl Debug for FruValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FruValue::None => write!(f, "None"),
            FruValue::Number(v) => write!(f, "{}", v),
            FruValue::Bool(v) => write!(f, "{}", v),
            FruValue::String(v) => write!(f, "{}", v),
            FruValue::Function(_) => write!(f, "Function"),
        }
    }
}

impl Debug for FruFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Function")
    }
}

impl Debug for AnyOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnyOperator::BuiltinOperator(_) => write!(f, "BuiltinOperator"),
            v => v.fmt(f),
        }
    }
}
