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
    StructType(Rc<FruStructType>),
    StructObject(FruStructObject),
}

#[derive(Clone)]
pub enum AnyFunction {
    // TODO: extract curried function to struct
    Function(Rc<FruFunction>),
    BuiltinFunction(Rc<TFnBuiltin>),
    CurriedFunction {
        saved_args: Vec<FruValue>,
        function: Rc<AnyFunction>,
    },
}

pub struct FruFunction {
    pub argument_idents: Vec<Identifier>,
    pub body: Rc<FruStatement>,
    pub scope: Rc<Scope>,
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

#[derive(Clone)]
pub struct FruStructType {
    pub name: Identifier,
    pub fields: Vec<Identifier>,
    // a lot of expected here :: methods / static functions / watches / etc
}

#[derive(Clone)]
pub struct FruStructObject {
    pub type_: Rc<FruStructType>,
    pub fields: Vec<FruValue>,
}

impl FruValue {
    pub fn get_type_identifier(&self) -> Identifier {
        match self {
            FruValue::None => Identifier::for_none(),
            FruValue::Number(_) => Identifier::for_number(),
            FruValue::Bool(_) => Identifier::for_bool(),
            FruValue::String(_) => Identifier::for_string(),
            FruValue::Function(_) => Identifier::for_function(),
            FruValue::StructType(_) => Identifier::for_struct_type(),
            FruValue::StructObject(_) => Identifier::for_struct_object(),
        }
    }

    pub fn get_field(&self, ident: Identifier) -> Result<FruValue, FruError> {
        match self {
            FruValue::StructObject(obj) => obj.get_field(ident),

            _ => FruError::new_err(format!(
                "expected struct object, got {:?}",
                self.get_type_identifier()
            )),
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
            StatementSignal::BlockReturn(v) => Ok(v),
            other => FruError::new_err(format!("unexpected signal {:?}", other)),
        }
    }
}

impl AnyFunction {
    pub fn call(&self, args: Vec<FruValue>) -> Result<FruValue, FruError> {
        match self {
            AnyFunction::Function(func) => func.call(args),
            AnyFunction::BuiltinFunction(func) => func(args),
            AnyFunction::CurriedFunction {
                saved_args,
                function,
            } => {
                let mut new_args = saved_args.clone();
                new_args.extend(args);
                function.call(new_args)
            }
        }
    }

    pub fn get_arg_count(&self) -> usize {
        match self {
            AnyFunction::Function(func) => func.argument_idents.len(),
            AnyFunction::BuiltinFunction(_) => 0,
            AnyFunction::CurriedFunction {
                saved_args,
                function,
            } => function.get_arg_count() - saved_args.len(),
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
                let new_scope = Scope::new_with_parent(scope.clone());
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

impl FruStructObject {
    pub fn get_field(&self, ident: Identifier) -> Result<FruValue, FruError> {
        for (i, field_ident) in self.type_.fields.iter().enumerate() {
            if *field_ident == ident {
                return Ok(self.fields[i].clone());
            }
        }
        FruError::new_err(format!("field {} not found", ident))
    }
}

impl Debug for FruValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FruValue::None => write!(f, "None"),
            FruValue::Number(v) => write!(f, "{}", v),
            FruValue::Bool(v) => write!(f, "{}", v),
            FruValue::String(v) => write!(f, "{}", v),
            FruValue::Function(fun) => write!(f, "{:?}", fun),
            FruValue::StructType(type_) => write!(f, "{:?}", type_),
            FruValue::StructObject(obj) => write!(f, "{:?}", obj),
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

impl Debug for AnyFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnyFunction::Function(_) | AnyFunction::BuiltinFunction(_) => write!(f, "Function"),
            AnyFunction::CurriedFunction {
                saved_args,
                function,
            } => {
                write!(
                    f,
                    "CurriedFunction({:?}/{:?})",
                    saved_args,
                    function.get_arg_count()
                )
            }
        }
    }
}

impl Debug for FruStructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Debug for FruStructObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}{{", self.type_)?;
        let total_fields = self.type_.fields.len();
        for (i, field) in self.type_.fields.iter().enumerate() {
            write!(f, "{}: {:?}", field, self.fields[i])?;
            if i < total_fields - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, "}}")
    }
}
