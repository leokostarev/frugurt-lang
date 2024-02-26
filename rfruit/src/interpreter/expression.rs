use super::{
    AnyFunction, FruError, FruFunction, FruStatement, FruValue, Identifier, OperatorIdentifier,
    Scope,
};
use crate::fru_progress;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum FruExpression {
    Literal(FruValue),
    Variable(Identifier),
    Call {
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

            FruExpression::Variable(ident) => Ok(scope.get_variable(ident.clone())?),

            FruExpression::Call { what, args } => {
                let var = what.evaluate(scope.clone())?;

                match var {
                    //
                    FruValue::Function(func) => {
                        let args_evaluated = args
                            .iter()
                            .map(|arg| arg.evaluate(scope.clone()))
                            .collect::<Result<Vec<FruValue>, FruError>>()?;
                        fru_progress!("Calling fru-function with args {:?}", args_evaluated);
                        func.call(args_evaluated)
                    }

                    _ => FruError::new_err(format!(
                        "{} is not a function",
                        var.get_type_identifier()
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
                    op: operator.clone(),
                    left: type_left,
                    right: type_right,
                })?;
                op.call(left_val, right_val)
            }

            FruExpression::FnDef { args, body } => {
                return Ok(FruValue::Function(AnyFunction::Function(Rc::new(
                    FruFunction {
                        argument_names: args.clone(),
                        body: body.clone(),
                        scope: scope.clone(),
                    },
                ))));
            }
        }
    }
}
