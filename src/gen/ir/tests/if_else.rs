use super::*;

#[test]
fn test_if_else() {
    let input = r#"int main() {
  int a = 2;
  if (a) {
    a = 2;
  } else a = 0;
  return a;
}"#;
    let text_form_ir = generate_ir_from_input(input);
    print!("{:#}", text_form_ir);
}