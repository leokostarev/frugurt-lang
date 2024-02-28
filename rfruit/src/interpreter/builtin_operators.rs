use super::{
    AnyOperator, FruError, FruValue, Identifier as Id, OperatorIdentifier as OpId, TOpBuiltin,
};
use std::collections::HashMap;
use std::rc::Rc;

macro_rules! builtin_operator {
    ($Name:ident, $L:ident, $R:ident, $Res:ident, $OP:tt) => {
        fn $Name(left: FruValue, right: FruValue) -> Result<FruValue, FruError> {
            if let (FruValue::$L(l), FruValue::$R(r)) = (left, right) {
                return Ok(FruValue::$Res(l $OP r));
            }

            unreachable!();
        }
    };
}

impl Id {
    // builtin types
    pub fn for_none() -> Self {
        return Self::new("None");
    }

    pub fn for_number() -> Self {
        return Self::new("Number");
    }

    pub fn for_bool() -> Self {
        return Self::new("Bool");
    }

    pub fn for_function() -> Self {
        return Self::new("Function");
    }

    // builtin operators
    pub fn for_plus() -> Self {
        return Self::new("+");
    }

    pub fn for_minus() -> Self {
        return Self::new("-");
    }

    pub fn for_multiply() -> Self {
        return Self::new("*");
    }

    pub fn for_divide() -> Self {
        return Self::new("/");
    }

    pub fn for_and() -> Self {
        return Self::new("&&");
    }

    pub fn for_or() -> Self {
        return Self::new("||");
    }

    // builtin operators (comparison)
    pub fn for_less() -> Self {
        return Self::new("<");
    }

    pub fn for_less_eq() -> Self {
        return Self::new("<=");
    }

    pub fn for_greater() -> Self {
        return Self::new(">");
    }

    pub fn for_greater_eq() -> Self {
        return Self::new(">=");
    }

    pub fn for_eq() -> Self {
        return Self::new("==");
    }

    pub fn for_not_eq() -> Self {
        return Self::new("!=");
    }
}

pub fn builtin_operators() -> HashMap<OpId, AnyOperator> {
    let mut res = HashMap::from(
        [
            (Id::for_plus(), &num_plus_num as &TOpBuiltin),
            (Id::for_minus(), &num_minus_num as &TOpBuiltin),
            (Id::for_multiply(), &num_mul_num as &TOpBuiltin),
            (Id::for_divide(), &num_div_num as &TOpBuiltin),
            (Id::for_less(), &num_less_num as &TOpBuiltin),
            (Id::for_less_eq(), &num_less_eq_num as &TOpBuiltin),
            (Id::for_greater(), &num_greater_num as &TOpBuiltin),
            (Id::for_greater_eq(), &num_greater_eq_num as &TOpBuiltin),
            (Id::for_eq(), &num_eq_num as &TOpBuiltin),
            (Id::for_not_eq(), &num_not_eq_num as &TOpBuiltin),
        ]
        .map(|(op, fun)| {
            (
                OpId::new(op, Id::for_number(), Id::for_number()),
                AnyOperator::BuiltinOperator(Rc::new(fun)),
            )
        }),
    );

    res.insert(
        OpId::new(Id::for_and(), Id::for_bool(), Id::for_bool()),
        AnyOperator::BuiltinOperator(Rc::new(bool_and_bool)),
    );

    res.insert(
        OpId::new(Id::for_or(), Id::for_bool(), Id::for_bool()),
        AnyOperator::BuiltinOperator(Rc::new(bool_or_bool)),
    );

    res
}

// number
builtin_operator!(num_plus_num, Number, Number, Number, +);
builtin_operator!(num_minus_num, Number, Number, Number, -);
builtin_operator!(num_mul_num, Number, Number, Number, *);
fn num_div_num(left: FruValue, right: FruValue) -> Result<FruValue, FruError> {
    if let (FruValue::Number(l), FruValue::Number(r)) = (left, right) {
        if r == 0.0 {
            return FruError::new_errs("division by zero");
        }
        return Ok(FruValue::Number(l / r));
    }

    unreachable!();
}
builtin_operator!(num_less_num, Number, Number, Bool, <);
builtin_operator!(num_less_eq_num, Number, Number, Bool, <=);
builtin_operator!(num_greater_num, Number, Number, Bool, >);
builtin_operator!(num_greater_eq_num, Number, Number, Bool, >=);
builtin_operator!(num_eq_num, Number, Number, Bool, ==);
builtin_operator!(num_not_eq_num, Number, Number, Bool, !=);

// bool
builtin_operator!(bool_or_bool, Bool, Bool, Bool, ||);
builtin_operator!(bool_and_bool, Bool, Bool, Bool, &&);
