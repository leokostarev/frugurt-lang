use crate::{FruError, FruStatement, FruValue, Identifier, Scope, StatementSignal, TFnBuiltin};
use std::{fmt::Debug, ops::Sub, rc::Rc};

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
    pub function: Rc<TFnBuiltin>, // TODO: check is Rc is needed
    pub argument_count: ArgCount,
}

pub struct CurriedFunction {
    pub saved_args: Vec<FruValue>,
    pub function: Rc<AnyFunction>,
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
            other => FruError::new_val(format!("unexpected signal {:?}", other)),
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
