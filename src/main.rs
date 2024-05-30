pub mod sysy;

use std::env::args;
use std::fs::read_to_string;
use std::io::Result;

fn main() -> Result<()>{
    // get the command line arguments
    let mut args = args();
    args.next();
    let mode = args.next().unwrap();
    let input = args.next().unwrap();
    args.next();
    let output = args.next().unwrap();

    let input = read_to_string(input)?; // read input file

    // parse the input
    let ast = sysy::CompUnitParser::new().parse(&input).unwrap();

    // generate the output
    println!("{}", ast);
    Ok(())
}
