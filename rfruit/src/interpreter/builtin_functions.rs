use super::{AnyFunction, FruError, FruValue, Identifier};
use std::collections::HashMap;

pub fn builtin_functions() -> HashMap<Identifier, FruValue> {
    HashMap::from([(
        Identifier::new("print"),
        FruValue::Function(AnyFunction::BuiltinFunction(std::rc::Rc::new(b_print))),
    )])
}

fn b_print(args: Vec<FruValue>) -> Result<FruValue, FruError> {
    for arg in args {
        print!("{:?} ", arg);
    }
    println!();

    Ok(FruValue::None)
}
