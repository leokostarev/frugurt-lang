use super::{FruError, FruStatement, Identifier, Scope, StatementSignal};
use std::collections::HashMap;
use std::fmt::Debug;
use std::ops::Sub;

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

#[derive(Clone, Copy)]
pub struct FruField {
    pub ident: Identifier,
    pub is_public: bool,
}

#[derive(Clone)]
pub struct FruStructType {
    pub ident: Identifier,
    pub fields: Vec<FruField>,
    pub watches_by_field: HashMap<Identifier, Vec<Rc<FruStatement>>>,
    pub watches: Vec<Rc<FruStatement>>,
    // a lot of expected here :: methods / static functions / etc
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
            FruValue::StructObject(obj) => obj.type_.ident,
        }
    }

    pub fn call(
        &self,
        arg_count: i32,
        get_args: impl Fn() -> Result<Vec<FruValue>, FruError>,
    ) -> Result<FruValue, FruError> {
        match self {
            FruValue::Function(fun) => fun.call(arg_count, get_args),
            _ => FruError::new_err(format!("{:?} is not invokable", self.get_type_identifier())),
        }
    }

    pub fn curry_call(
        &self,
        arg_count: i32,
        get_args: impl Fn() -> Result<Vec<FruValue>, FruError>,
    ) -> Result<FruValue, FruError> {
        match self {
            FruValue::Function(func) => {
                if let Err(err) = func.get_arg_count().satisfies(arg_count) {
                    match err {
                        ArgCountError::TooFewArgs { .. } => {}
                        _ => {
                            return FruError::new_err(format!("{:?}", err));
                        }
                    }
                }

                match func {
                    AnyFunction::CurriedFunction(func) => {
                        let mut new_args = func.saved_args.clone();
                        new_args.extend(get_args()?);

                        Ok(FruValue::Function(AnyFunction::CurriedFunction(Rc::new(
                            CurriedFunction {
                                saved_args: new_args,
                                function: func.function.clone(),
                            },
                        ))))
                    }

                    normal => Ok(FruValue::Function(AnyFunction::CurriedFunction(Rc::new(
                        CurriedFunction {
                            saved_args: get_args()?,
                            function: Rc::new(normal.clone()),
                        },
                    )))),
                }
            }

            _ => FruError::new_err(format!("{:?} is not invokable", self.get_type_identifier())),
        }
    }

    pub fn instantiate(
        &self,
        arg_count: usize,
        get_args: impl Fn() -> Result<Vec<FruValue>, FruError>,
    ) -> Result<FruValue, FruError> {
        match self {
            FruValue::StructType(type_) => {
                if arg_count != type_.fields.len() {
                    return FruError::new_err(format!(
                        "expected {} fields, got {}",
                        type_.fields.len(),
                        arg_count
                    ));
                } // todo fire watches

                Ok(FruValue::StructObject(FruStructObject {
                    type_: type_.clone(),
                    fields: get_args()?,
                }))
            }

            _ => {
                return FruError::new_err(format!(
                    "cannot instantiate {}",
                    self.get_type_identifier()
                ))
            }
        }
    }

    pub fn get_field(&self, ident: Identifier) -> Result<FruValue, FruError> {
        match self {
            FruValue::StructObject(obj) => obj.get_field(ident),

            _ => FruError::new_err(format!(
                "cannot access field of {}",
                self.get_type_identifier()
            )),
        }
    }

    pub fn set_field(
        &mut self,
        path: &[Identifier],
        value: FruValue,
    ) -> Result<FruValue, FruError> {
        match self {
            FruValue::StructObject(obj) => obj.set_field(path, value),

            _ => FruError::new_err(format!(
                "cannot access field of {}",
                self.get_type_identifier()
            )),
        }
    }
}

impl ArgCount {
    pub fn satisfies(&self, got: i32) -> Result<(), ArgCountError> {
        match self {
            ArgCount::Exact(n) => {
                if got == *n {
                    Ok(())
                } else if got > *n {
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
                if got <= *n {
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

impl Sub<i32> for ArgCount {
    type Output = ArgCount;

    fn sub(self, rhs: i32) -> Self::Output {
        match self {
            ArgCount::Exact(n) => ArgCount::Exact(n - rhs),
            ArgCount::AtMost(n) => ArgCount::AtMost(n - rhs),
            ArgCount::Any => ArgCount::Any,
        }
    }
}

impl ArgCountError {
    pub fn to_error(&self) -> FruError {
        FruError::new(format!("{:?}", self))
    }
}

impl AnyFunction {
    pub fn call(
        &self,
        arg_count: i32,
        get_args: impl Fn() -> Result<Vec<FruValue>, FruError>,
    ) -> Result<FruValue, FruError> {
        if let Err(err) = self.get_arg_count().satisfies(arg_count) {
            return Err(err.to_error());
        }

        match self {
            AnyFunction::Function(func) => func.call_unchecked(get_args()?),
            AnyFunction::BuiltinFunction(func) => func.call_unchecked(get_args()?),
            AnyFunction::CurriedFunction(func) => func.call_unchecked(get_args()?),
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
    pub fn call_unchecked(&self, args: Vec<FruValue>) -> Result<FruValue, FruError> {
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
    pub fn call_unchecked(&self, args: Vec<FruValue>) -> Result<FruValue, FruError> {
        (self.function)(args)
    }

    pub fn get_arg_count(&self) -> ArgCount {
        self.argument_count
    }
}

impl CurriedFunction {
    pub fn call_unchecked(&self, args: Vec<FruValue>) -> Result<FruValue, FruError> {
        let mut new_args = self.saved_args.clone();
        new_args.extend(args);

        match &*self.function {
            AnyFunction::Function(func) => func.call_unchecked(new_args),
            AnyFunction::BuiltinFunction(func) => func.call_unchecked(new_args),
            AnyFunction::CurriedFunction(_) => {
                panic!("CurriedFunction should never contain a CurriedFunction")
            }
        }
    }

    pub fn get_arg_count(&self) -> ArgCount {
        let internal = match &*self.function {
            AnyFunction::Function(func) => func.get_arg_count() - self.saved_args.len() as i32,
            AnyFunction::BuiltinFunction(func) => {
                func.get_arg_count() - self.saved_args.len() as i32
            }
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
            if field_ident.ident == ident {
                return Ok(self.fields[i].clone());
            }
        }
        FruError::new_err(format!("field {} not found", ident))
    }

    pub fn set_field(
        &mut self,
        path: &[Identifier],
        value: FruValue,
    ) -> Result<FruValue, FruError> {
        for (i, field_ident) in self.type_.fields.iter().enumerate() {
            if field_ident.ident == path[0] {
                return if path.len() == 1 {
                    self.fields[i] = value;
                    Ok(FruValue::None)
                } else {
                    self.fields[i].set_field(&path[1..path.len()], value)
                };
            }
        }
        FruError::new_err(format!(
            "field {} does not exist in struct {}",
            path[0], self.type_.ident
        ))
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

impl Debug for AnyOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnyOperator::BuiltinOperator(_) => write!(f, "BuiltinOperator"),
            v => v.fmt(f),
        }
    }
}

impl Debug for FruField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_public {
            write!(f, "pub ")?;
        }
        write!(f, "{}", self.ident)
    }
}

impl Debug for FruStructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

impl Debug for FruStructObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}{{", self.type_)?;
        let total_fields = self.type_.fields.len();
        for (i, field) in self.type_.fields.iter().enumerate() {
            write!(f, "{:?}: {:?}", field, self.fields[i])?;
            if i < total_fields - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, "}}")
    }
}
