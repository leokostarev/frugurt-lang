use super::{
    AnyFunction, FruError, FruFunction, FruStatement, FruValue, Identifier, OperatorIdentifier,
    Scope,
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

                        match func {
                            AnyFunction::CurriedFunction {
                                saved_args,
                                function,
                            } => {
                                let mut new_args = saved_args.clone();
                                new_args.extend(args);

                                Ok(FruValue::Function(AnyFunction::CurriedFunction {
                                    saved_args: new_args,
                                    function,
                                }))
                            }

                            normal => {
                                Ok(FruValue::Function(AnyFunction::CurriedFunction {
                                    saved_args: args,
                                    function: Rc::new(normal),
                                }))
                            },
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

            FruExpression::FnDef { args, body } => {
                return Ok(FruValue::Function(AnyFunction::Function(Rc::new(
                    FruFunction {
                        argument_idents: args.clone(),
                        body: body.clone(),
                        scope: scope.clone(),
                    },
                ))));
            }
        }
    }
}
