use serde_json::{Value};
use std::env;
use std::process::Command;

mod interpreter;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        println!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];

    let converter_result = Command::new(".\\converter.exe")
        .arg(filename)
        .output()
        .expect("failed to run converter");

    let text = std::str::from_utf8(&converter_result.stdout).unwrap();

    let ast_or_err: Result<Value, _> = serde_json::from_str(text);

    let json_ast = match ast_or_err {
        Ok(x) => x,
        Err(x)=>{
            println!("{}", x);
            std::process::exit(1);
        }
    };

    let ast = interpreter::ast_json_parser::parse(json_ast);

    interpreter::runner::run_program(ast);
}
