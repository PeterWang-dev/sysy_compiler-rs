use lalrpop_util::lalrpop_mod;

lalrpop_mod!(sysy);
pub mod ast;
pub mod error;
pub mod ir_gen;

use koopa::back::KoopaGenerator;
use std::env::args;
use std::fs::read_to_string;
use std::io::Result;

fn main() -> Result<()> {
    // get the command line arguments
    let mut args = args();
    args.next();
    let _mode = args.next().unwrap();
    let input = args.next().unwrap();
    args.next();
    let output = args.next().unwrap();

    let input = read_to_string(input)?; // read input file

    // parse the input
    let ast = sysy::CompUnitParser::new().parse(&input).unwrap();
    let program = ir_gen::generate_ir(&ast).unwrap();

    // generate the output
    let mut gen = KoopaGenerator::new(Vec::new());
    gen.generate_on(&program).unwrap();
    let text_form_ir = std::str::from_utf8(&gen.writer()).unwrap().to_string();

    // write the output
    std::fs::write(output, text_form_ir)?;
    Ok(())
}
