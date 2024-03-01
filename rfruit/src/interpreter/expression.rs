use super::{
    AnyFunction, CurriedFunction, FruError, FruFunction, FruStatement, FruStructObject, FruValue,
    Identifier, OperatorIdentifier, Scope,
};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum FruExpression {
    Literal(FruValue),
    Variable(Identifier),
    Call {
        what: Box<FruExpression>,
        args: Vec<FruExpression>,
    },
    CurryCall {
        what: Box<FruExpression>,
        args: Vec<FruExpression>,
    },
    Binary {
        operator: Identifier,
        left: Box<FruExpression>,
        right: Box<FruExpression>,
    },

    FnDef {
        args: Vec<Identifier>,
        body: Rc<FruStatement>,
    },

    Instantiation {
        what: Box<FruExpression>,
        args: Vec<FruExpression>,
    },

    FieldAccess {
        what: Box<FruExpression>,
        field: Identifier,
    },
}

impl FruExpression {
    pub fn evaluate(&self, scope: Rc<Scope>) -> Result<FruValue, FruError> {
        match self {
            FruExpression::Literal(value) => Ok(value.clone()),

            FruExpression::Variable(ident) => Ok(scope.get_variable(*ident)?),

            FruExpression::Call { what, args } => {
                let callee = what.evaluate(scope.clone())?;
                match callee {
                    FruValue::Function(func) => func.call(
                        args.iter()
                            .map(|arg| arg.evaluate(scope.clone()))
                            .collect::<Result<Vec<FruValue>, FruError>>()?,
                    ),

                    _ => FruError::new_err(format!(
                        "{} is not a function",
                        what.evaluate(scope.clone())?.get_type_identifier()
                    )),
                }
            }

            FruExpression::CurryCall { what, args } => {
                let callee = what.evaluate(scope.clone())?;

                match callee {
                    FruValue::Function(func) => {
                        let args = args
                            .iter()
                            .map(|arg| arg.evaluate(scope.clone()))
                            .collect::<Result<Vec<FruValue>, FruError>>()?;

                        if let Err(err) = func.get_arg_count().satisfies(args.len() as i32) {
                            return FruError::new_err(format!("{:?}",
                               err
                            ));
                        }

                        match func {
                            AnyFunction::CurriedFunction(func) => {
                                let mut new_args = func.saved_args.clone();
                                new_args.extend(args);

                                Ok(FruValue::Function(AnyFunction::CurriedFunction(Rc::new(
                                    CurriedFunction {
                                        saved_args: new_args,
                                        function: func.function.clone(),
                                    },
                                ))))
                            }

                            normal => Ok(FruValue::Function(AnyFunction::CurriedFunction(
                                Rc::new(CurriedFunction {
                                    saved_args: args,
                                    function: Rc::new(normal),
                                }),
                            ))),
                        }
                    }

                    _ => FruError::new_err(format!(
                        "{} is not a function",
                        what.evaluate(scope.clone())?.get_type_identifier()
                    )),
                }
            }

            FruExpression::Binary {
                operator,
                left,
                right,
            } => {
                let left_val = left.evaluate(scope.clone())?;
                let right_val = right.evaluate(scope.clone())?;
                let type_left = left_val.get_type_identifier();
                let type_right = right_val.get_type_identifier();

                let op = scope.get_operator(OperatorIdentifier {
                    op: *operator,
                    left: type_left,
                    right: type_right,
                })?;
                op.operate(left_val, right_val)
            }

            FruExpression::FnDef { args, body } => Ok(FruValue::Function(AnyFunction::Function(
                Rc::new(FruFunction {
                    argument_idents: args.clone(),
                    body: body.clone(),
                    scope: scope.clone(),
                }),
            ))),

            FruExpression::Instantiation { what, args } => {
                let instantiated = what.evaluate(scope.clone())?;

                match instantiated {
                    FruValue::StructType(type_) => {
                        let args = args
                            .iter()
                            .map(|arg| arg.evaluate(scope.clone()))
                            .collect::<Result<Vec<FruValue>, FruError>>()?;

                        if args.len() != type_.fields.len() {
                            return FruError::new_err(format!(
                                "expected {} fields, got {}",
                                type_.fields.len(),
                                args.len()
                            ));
                        }

                        Ok(FruValue::StructObject(FruStructObject {
                            type_: type_,
                            fields: args,
                        }))
                    }

                    _ => {
                        return FruError::new_err(format!(
                            "cannot instantiate {}",
                            instantiated.get_type_identifier()
                        ))
                    }
                }
            }

            FruExpression::FieldAccess { what, field } => {
                let what = what.evaluate(scope.clone())?;

                match what {
                    FruValue::StructObject(obj) => Ok(obj.get_field(*field)?),

                    _ => FruError::new_err(format!(
                        "cannot access field of {}",
                        what.get_type_identifier()
                    )),
                }
            }
        }
    }
}
