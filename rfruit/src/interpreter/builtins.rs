use super::{AnyOperator, FruError, FruValue, Identifier, OperatorIdentifier as OpId};
use std::collections::HashMap;
use std::rc::Rc;

#[macro_export]
macro_rules! fru_progress {
    ($($arg:tt)*) => {
        if false {
            print!(">>> ");
            println!($($arg)*);
        }
    };
}

macro_rules! builtin_operator {
    ($Name:ident, $L:ident, $R:ident, $Res:ident, $OP:tt) => {
        fn $Name(left: FruValue, right: FruValue) -> Result<FruValue, FruError> {
            if let (FruValue::$L(l), FruValue::$R(r)) = (left, right) {
                fru_progress!("{} <> {} = {}", l, r, l $OP r);
                return Ok(FruValue::$Res(l $OP r));
            }

            unreachable!();
        }
    };
}

impl Identifier {
    // builtin types
    pub fn for_none() -> Self {
        return Self::new("None");
    }

    pub fn for_int() -> Self {
        return Self::new("Int");
    }

    pub fn for_float() -> Self {
        return Self::new("Float");
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

    pub fn for_less_equal() -> Self {
        return Self::new("<=");
    }

    pub fn for_greater() -> Self {
        return Self::new(">");
    }

    pub fn for_greater_equal() -> Self {
        return Self::new(">=");
    }

    pub fn for_equal() -> Self {
        return Self::new("==");
    }

    pub fn for_not_equal() -> Self {
        return Self::new("!=");
    }
}

pub fn builtin_operators() -> HashMap<OpId, AnyOperator> {
    HashMap::from([
        // int
        (
            OpId::new(
                Identifier::for_plus(),
                Identifier::for_int(),
                Identifier::for_int(),
            ),
            AnyOperator::BuiltinOperator(Rc::new(int_plus_int)),
        ),
        (
            OpId::new(
                Identifier::for_minus(),
                Identifier::for_int(),
                Identifier::for_int(),
            ),
            AnyOperator::BuiltinOperator(Rc::new(int_minus_int)),
        ),
        (
            OpId::new(
                Identifier::for_multiply(),
                Identifier::for_int(),
                Identifier::for_int(),
            ),
            AnyOperator::BuiltinOperator(Rc::new(int_mul_int)),
        ),
        (
            OpId::new(
                Identifier::for_divide(),
                Identifier::for_int(),
                Identifier::for_int(),
            ),
            AnyOperator::BuiltinOperator(Rc::new(int_div_int)),
        ),
        (
            OpId::new(
                Identifier::for_less(),
                Identifier::for_int(),
                Identifier::for_int(),
            ),
            AnyOperator::BuiltinOperator(Rc::new(int_less_int)),
        ),
        (
            OpId::new(
                Identifier::for_less_equal(),
                Identifier::for_int(),
                Identifier::for_int(),
            ),
            AnyOperator::BuiltinOperator(Rc::new(int_less_equal_int)),
        ),
        (
            OpId::new(
                Identifier::for_greater(),
                Identifier::for_int(),
                Identifier::for_int(),
            ),
            AnyOperator::BuiltinOperator(Rc::new(int_greater_int)),
        ),
        (
            OpId::new(
                Identifier::for_greater_equal(),
                Identifier::for_int(),
                Identifier::for_int(),
            ),
            AnyOperator::BuiltinOperator(Rc::new(int_greater_equal_int)),
        ),
        (
            OpId::new(
                Identifier::for_equal(),
                Identifier::for_int(),
                Identifier::for_int(),
            ),
            AnyOperator::BuiltinOperator(Rc::new(int_equal_int)),
        ),
        (
            OpId::new(
                Identifier::for_not_equal(),
                Identifier::for_int(),
                Identifier::for_int(),
            ),
            AnyOperator::BuiltinOperator(Rc::new(int_not_equal_int)),
        ),
        // bool
        (
            OpId::new(
                Identifier::for_and(),
                Identifier::for_bool(),
                Identifier::for_bool(),
            ),
            AnyOperator::BuiltinOperator(Rc::new(bool_and_bool)),
        ),
        (
            OpId::new(
                Identifier::for_or(),
                Identifier::for_bool(),
                Identifier::for_bool(),
            ),
            AnyOperator::BuiltinOperator(Rc::new(bool_or_bool)),
        ),
    ])
}

// int
builtin_operator!(int_plus_int, Int, Int, Int, +);
builtin_operator!(int_minus_int, Int, Int, Int, -);
builtin_operator!(int_mul_int, Int, Int, Int, *);

fn int_div_int(left: FruValue, right: FruValue) -> Result<FruValue, FruError> {
    if let (FruValue::Int(l), FruValue::Int(r)) = (left, right) {
        if r == 0 {
            return FruError::new_errs("division by zero");
        }
        fru_progress!("{} <> {} = {}", l, r, l / r);
        return Ok(FruValue::Int(l / r));
    }

    unreachable!();
}

builtin_operator!(int_less_int, Int, Int, Bool, <);
builtin_operator!(int_less_equal_int, Int, Int, Bool, <=);
builtin_operator!(int_greater_int, Int, Int, Bool, >);
builtin_operator!(int_greater_equal_int, Int, Int, Bool, >=);
builtin_operator!(int_equal_int, Int, Int, Bool, ==);
builtin_operator!(int_not_equal_int, Int, Int, Bool, !=);

// bool
builtin_operator!(bool_or_bool, Bool, Bool, Bool, ||);
builtin_operator!(bool_and_bool, Bool, Bool, Bool, &&);
