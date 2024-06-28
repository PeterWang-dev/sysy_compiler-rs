use lalrpop_util::lalrpop_mod;

lalrpop_mod!(sysy);

pub mod ast;
pub mod error;
pub mod gen;

use gen::asm as asm_gen;
use gen::ir as ir_gen;

use koopa::back::KoopaGenerator;
use std::{env::args, fs::read_to_string, io::Result};

fn main() -> Result<()> {
    // get the command line arguments
    //! Can not use clap because it currently have no support non-UNIX options (like -koopa)
    let mut args = args();
    args.next();
    let mode = args.next().unwrap();
    let input = args.next().unwrap();
    args.next();
    let output = args.next().unwrap();

    let input = read_to_string(input)?; // read input file

    // parse the input
    let ast = sysy::CompUnitParser::new().parse(&input).unwrap();
    let program = ir_gen::generate_on(&ast).unwrap();

    // generate the output conditionally
    let text_from_ir = std::str::from_utf8(
        &(match mode.as_str() {
            "-koopa" => { // generate Koopa IR using koopa::back
                let mut gen = KoopaGenerator::new(Vec::new());
                gen.generate_on(&program).unwrap();
                gen.writer()
            } // generate riscv assembly using crate::gen::asm
            "-riscv" => asm_gen::generate_on(&program).unwrap().into(),
            _ => panic!("Invalid mode"),
        }),
    ) // convert UTF-8 bytes to plain text
    .unwrap()
    .to_string();

    // write the output
    std::fs::write(output, text_from_ir)?;
    Ok(())
}
