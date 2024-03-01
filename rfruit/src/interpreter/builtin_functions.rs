use super::{AnyFunction, ArgCount, BuiltinFunction, FruError, FruValue, Identifier, TFnBuiltin};
use std::collections::HashMap;
use std::rc::Rc;

pub fn builtin_functions() -> HashMap<Identifier, FruValue> {
    HashMap::from([
        (
            Identifier::new("print"),
            FruValue::Function(AnyFunction::BuiltinFunction(BuiltinFunction {
                function: Rc::new(&b_print as &TFnBuiltin),
                argument_count: ArgCount::Any,
            })),
        ),
        (
            Identifier::new("input"),
            FruValue::Function(AnyFunction::BuiltinFunction(BuiltinFunction {
                function: Rc::new(&b_input as &TFnBuiltin),
                argument_count: ArgCount::AtMost(1),
            })),
        ),
    ])
}

fn b_print(args: Vec<FruValue>) -> Result<FruValue, FruError> {
    for arg in args {
        print!("{:?} ", arg);
    }
    println!();

    Ok(FruValue::None)
}

fn b_input(args: Vec<FruValue>) -> Result<FruValue, FruError> {
    if args.len() == 1 {
        print!("{:?}: ", args[0]);
    }
    
    let mut input = String::new();
    std::io::stdin().read_line(&mut input).unwrap();
    Ok(FruValue::String(input.trim().to_string()))
}
