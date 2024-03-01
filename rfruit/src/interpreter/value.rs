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

#[derive(Debug, Clone, Copy)]
pub enum ArgCount {
    Exact(i32),
    AtMost(i32),
    Any,
}

#[derive(Clone, Copy)]
pub enum ArgCountError {
    TooManyArgs { expected: ArgCount, got: i32 },
    TooFewArgs { expected: ArgCount, got: i32 },
}

#[derive(Clone)]
pub enum AnyFunction {
    Function(Rc<FruFunction>),
    CurriedFunction(Rc<CurriedFunction>),
    BuiltinFunction(BuiltinFunction),
}

pub struct FruFunction {
    pub argument_idents: Vec<Identifier>,
    pub body: Rc<FruStatement>,
    pub scope: Rc<Scope>,
}

#[derive(Clone)]
pub struct BuiltinFunction {
    pub function: Rc<TFnBuiltin>,
    pub argument_count: ArgCount,
}

pub struct CurriedFunction {
    pub saved_args: Vec<FruValue>,
    pub function: Rc<AnyFunction>,
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
            FruValue::StructObject(obj) => obj.type_.name,
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

impl ArgCount {
    pub fn satisfies(&self, got: i32) -> Result<(), ArgCountError> {
        match self {
            ArgCount::Exact(n) => {
                if *n == got {
                    Ok(())
                } else if *n < got {
                    Err(ArgCountError::TooManyArgs {
                        expected: *self,
                        got,
                    })
                } else {
                    Err(ArgCountError::TooFewArgs {
                        expected: *self,
                        got,
                    })
                }
            }

            ArgCount::AtMost(n) => {
                if *n >= got {
                    Ok(())
                } else {
                    Err(ArgCountError::TooManyArgs {
                        expected: *self,
                        got,
                    })
                }
            }

            ArgCount::Any => Ok(()),
        }
    }
}

impl ArgCountError {
    pub fn to_error(&self) -> FruError {
        FruError::new(format!("{:?}", self))
    }
}

impl AnyFunction {
    pub fn call(&self, args: Vec<FruValue>) -> Result<FruValue, FruError> {
        match self {
            AnyFunction::Function(func) => func.call(args),
            AnyFunction::BuiltinFunction(func) => func.call(args),
            AnyFunction::CurriedFunction(func) => func.call(args),
        }
    }
    pub fn get_arg_count(&self) -> ArgCount {
        match self {
            AnyFunction::Function(func) => func.get_arg_count(),
            AnyFunction::BuiltinFunction(func) => func.get_arg_count(),
            AnyFunction::CurriedFunction(func) => func.get_arg_count(),
        }
    }
}

impl FruFunction {
    pub fn call(&self, args: Vec<FruValue>) -> Result<FruValue, FruError> {
        if let Err(err) = self.get_arg_count().satisfies(args.len() as i32) {
            return Err(err.to_error());
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

    pub fn get_arg_count(&self) -> ArgCount {
        ArgCount::Exact(self.argument_idents.len() as i32)
    }
}

impl BuiltinFunction {
    pub fn call(&self, args: Vec<FruValue>) -> Result<FruValue, FruError> {
        if let Err(err) = self.get_arg_count().satisfies(args.len() as i32) {
            return Err(err.to_error());
        }
        (self.function)(args)
    }

    pub fn get_arg_count(&self) -> ArgCount {
        self.argument_count
    }
}

impl CurriedFunction {
    pub fn call(&self, args: Vec<FruValue>) -> Result<FruValue, FruError> {
        let mut new_args = self.saved_args.clone();
        new_args.extend(args);
        self.function.call(new_args)
    }

    pub fn get_arg_count(&self) -> ArgCount {
        let internal = match &*self.function {
            AnyFunction::Function(func) => func.get_arg_count(),
            AnyFunction::BuiltinFunction(func) => func.get_arg_count(),
            _ => panic!("CurriedFunction should never contain a CurriedFunction"),
        };

        match internal {
            ArgCount::Exact(n) => ArgCount::Exact(n - self.saved_args.len() as i32),
            ArgCount::AtMost(n) => ArgCount::AtMost(n - self.saved_args.len() as i32),
            ArgCount::Any => ArgCount::Any,
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
                    StatementSignal::BlockReturn(v) => Ok(v),
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

impl Debug for ArgCountError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArgCountError::TooManyArgs { expected, got } => {
                write!(
                    f,
                    "too many arguments, expected {:?}, got {}",
                    expected, got
                )
            }

            ArgCountError::TooFewArgs { expected, got } => {
                write!(f, "too few arguments, expected {:?}, got {}", expected, got)
            }
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
            AnyFunction::CurriedFunction(func) => {
                write!(
                    f,
                    "CurriedFunction({:?}/{:?})",
                    func.saved_args,
                    func.function.get_arg_count()
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
