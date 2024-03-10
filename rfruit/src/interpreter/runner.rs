use crate::{FruStatement, Scope};
use std::time::Instant;

pub fn run_program(program: Box<FruStatement>) {
    let global_scope = Scope::new_global();

    let start = Instant::now();

    let res = program.execute(global_scope);

    println!("{:?}", res);
    println!("Program finished in {}ms", start.elapsed().as_millis());
}
