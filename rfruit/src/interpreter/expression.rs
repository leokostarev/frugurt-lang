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
    Function {
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
macro_rules! args_to_lambda {
    ($args:ident, $scope:ident) => {
        || {
            Ok($args
                .iter()
                .map(|arg| arg.evaluate($scope.clone()))
                .collect::<Result<Vec<FruValue>, FruError>>()?)
        }
    };
}

impl FruExpression {
    pub fn evaluate(&self, scope: Rc<Scope>) -> Result<FruValue, FruError> {
        match self {
            FruExpression::Literal(value) => Ok(value.clone()),

            FruExpression::Variable(ident) => Ok(scope.get_variable(*ident)?),

            FruExpression::Call { what, args } => {
                let callee = what.evaluate(scope.clone())?;
                let args_count = args.len() as i32;
                let args = args_to_lambda!(args, scope);

                callee.call(args_count, args)
            }

            FruExpression::CurryCall { what, args } => {
                let callee = what.evaluate(scope.clone())?;
                let arg_count = args.len() as i32;
                let args = args_to_lambda!(args, scope);

                callee.curry_call(arg_count, args)
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

            FruExpression::Function { args, body } => Ok(FruValue::Function(
                AnyFunction::Function(Rc::new(FruFunction {
                    argument_idents: args.clone(),
                    body: body.clone(),
                    scope: scope.clone(),
                })),
            )),

            FruExpression::Instantiation { what, args } => {
                let instantiated = what.evaluate(scope.clone())?;
                let arg_count = args.len();
                let args = args_to_lambda!(args, scope);

                instantiated.instantiate(arg_count, args)
            }

            FruExpression::FieldAccess { what, field } => {
                let what = what.evaluate(scope.clone())?;
                what.get_field(*field)
            }
        }
    }
}
