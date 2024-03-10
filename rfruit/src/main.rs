mod helpers;
mod interpreter;

use interpreter::*;

use serde_json::Value;
use std::{env, process::Command};

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

    let json_ast_or_err: Result<Value, _> = serde_json::from_str(text);

    let json_ast = match json_ast_or_err {
        Ok(x) => x,
        Err(x) => {
            eprintln!("Json is invalid: {}", x);
            println!("{}", text);
            std::process::exit(1);
        }
    };

    if let Some(x) = json_ast.as_object() {
        if let Some(x) = x.get("error") {
            eprintln!("Error occurred during parsing: {}", x.as_str().unwrap());
            eprintln!("Details: {}", x.get("message").unwrap().as_str().unwrap());
        }
    }

    let ast = ast_json_parser::parse(json_ast);

    runner::run_program(ast);
}
