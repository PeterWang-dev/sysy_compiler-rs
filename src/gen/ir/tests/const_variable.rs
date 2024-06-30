use super::*;

#[test]
fn test_const() {
    let input = r#"int main() {
  const int x = 1 + 1;
  return x;
}"#;
    let text_form_ir = generate_ir_from_input(input);
    assert_eq!(
        text_form_ir,
        r#"fun @main(): i32 {
%entry:
  ret 2
}"#
    );
}
