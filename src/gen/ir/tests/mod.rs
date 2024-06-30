use super::generate_on;
use crate::sysy::CompUnitParser;
use koopa::back::KoopaGenerator;

mod expr;
mod const_variable;

fn generate_ir_from_input(input: &str) -> String {
    let ast = CompUnitParser::new().parse(input).unwrap();
    let program = generate_on(&ast).unwrap();
    let mut gen = KoopaGenerator::new(Vec::new());
    gen.generate_on(&program).unwrap();
    std::str::from_utf8(&gen.writer()).unwrap().to_string()
}

#[test]
fn test_simple() {
    let input = r#"int main() {
  // This is a comment, should be ignored
  /* This is a block comment,
  should be ignored */
  return 0;
}"#;
    let text_form_ir = generate_ir_from_input(input);
    assert_eq!(
        text_form_ir,
        // Pay attention: new line should not be inserted after `r#"` as it will be included in the string
        r#"fun @main(): i32 {
%entry:
  ret 0
}
"#
    );
}
