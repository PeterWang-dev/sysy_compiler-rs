use koopa::back::LlvmGenerator;
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(sysy);

pub mod ast;
pub mod error;
pub mod gen;

use gen::asm as asm_gen;
use gen::ir as ir_gen;

use koopa::back::KoopaGenerator;
use std::{env::args, fs::read_to_string, io::Result, str::from_utf8};

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
    let mut gen = ir_gen::IrGenerator::new();
    gen.generate_on(&ast).unwrap();
    let program = gen.program();

    // generate the output conditionally
    let text_from_ir = match mode.as_str() {
        "-koopa" => {
            // generate Koopa IR using koopa::back::KoopaGenerator
            let mut gen = KoopaGenerator::new(Vec::new());
            gen.generate_on(&program).unwrap();
            from_utf8(&gen.writer()).unwrap().to_string()
        }
        "-riscv" => {
            // generate riscv assembly using crate::gen::asm
            asm_gen::generate_on(&program).unwrap()
        }
        "-llvm" => {
            // generate llvm IR using koopa::back::LlvmGenerator
            let mut gen = LlvmGenerator::new(Vec::new());
            gen.generate_on(&program).unwrap();
            from_utf8(&gen.writer()).unwrap().to_string()
        }
        _ => panic!("Invalid mode"),
    };

    // write the output
    std::fs::write(output, text_from_ir)?;
    Ok(())
}
