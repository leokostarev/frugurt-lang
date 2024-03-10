use crate::{
    AnyFunction, ArgCountError, CurriedFunction, FruError, FruObject, FruType, Identifier,
};
use std::{fmt::Debug, rc::Rc};

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

    // ---oop---
    Type(Rc<FruType>),
    Object(FruObject),
}

impl FruValue {
    pub fn get_type_identifier(&self) -> Identifier {
        match self {
            FruValue::None => Identifier::for_none(),
            FruValue::Number(_) => Identifier::for_number(),
            FruValue::Bool(_) => Identifier::for_bool(),
            FruValue::String(_) => Identifier::for_string(),
            FruValue::Function(_) => Identifier::for_function(),
            FruValue::Type(_) => Identifier::for_struct_type(),
            FruValue::Object(obj) => obj.get_type().ident,
        }
    }

    pub fn call(
        &self,
        arg_count: i32,
        get_args: impl Fn() -> Result<Vec<FruValue>, FruError>,
    ) -> Result<FruValue, FruError> {
        match self {
            FruValue::Function(fun) => fun.call(arg_count, get_args),
            _ => FruError::new_val(format!("{:?} is not invokable", self.get_type_identifier())),
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
                            return FruError::new_val(format!("{:?}", err));
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

            _ => FruError::new_val(format!("{:?} is not invokable", self.get_type_identifier())),
        }
    }

    pub fn instantiate(
        &self,
        arg_count: usize,
        get_args: impl Fn() -> Result<Vec<FruValue>, FruError>,
    ) -> Result<FruValue, FruError> {
        match self {
            FruValue::Type(type_) => {
                if arg_count != type_.fields.len() {
                    return FruError::new_val(format!(
                        "expected {} fields, got {}",
                        type_.fields.len(),
                        arg_count
                    ));
                } // todo fire watches

                Ok(FruObject::new(type_.clone(), get_args()?))
            }

            _ => {
                return FruError::new_val(format!(
                    "cannot instantiate {}",
                    self.get_type_identifier()
                ))
            }
        }
    }

    pub fn get_field(&self, ident: Identifier) -> Result<FruValue, FruError> {
        match self {
            FruValue::Object(obj) => obj.get_field(ident),

            _ => FruError::new_val(format!(
                "cannot access field of {}",
                self.get_type_identifier()
            )),
        }
    }

    pub fn set_field(
        &mut self,
        path: &[Identifier],
        value: FruValue,
        ignore_watches: bool,
    ) -> Result<(), FruError> {
        match self {
            FruValue::Object(obj) => obj.set_field(path, value, ignore_watches),

            _ => FruError::new_unit(format!(
                "cannot access field of {}",
                self.get_type_identifier()
            )),
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
            FruValue::Function(fun) => write!(f, "{:?}", fun),
            FruValue::Type(type_) => write!(f, "{:?}", type_),
            FruValue::Object(obj) => write!(f, "{:?}", obj),
        }
    }
}
