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
}
"#
    );
}

#[test]
fn test_complex_const() {
    let input = r#"int main() {
  const int x = 1 * 2 || 2 * 3 && 3 * 4; // 2 || 6 && 12 => 1
  const int y = x * 3 > 10; // 3 > 10 => 0
  return y + 4 - x;
}"#;
    let text_form_ir = generate_ir_from_input(input);
    assert_eq!(
        text_form_ir,
        r#"fun @main(): i32 {
%entry:
  %0 = add 0, 4
  %1 = sub %0, 1
  ret %1
}
"#
    );
}

#[test]
fn test_variable() {
    let input = r#"int main() {
  int x = 10;
  x = x + 1;
  return x;
}"#;
    let text_form_ir = generate_ir_from_input(input);
    assert_eq!(
        text_form_ir,
        r#"fun @main(): i32 {
%entry:
  %0 = alloc i32
  store 10, %0
  %1 = load %0
  %2 = add %1, 1
  store %2, %0
  %3 = load %0
  ret %3
}
"#
    );
}
