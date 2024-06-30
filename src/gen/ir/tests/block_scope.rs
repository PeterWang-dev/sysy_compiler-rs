use super::*;

#[test]
fn test_block_scope() {
    let input = r#"int main() {
  int a = 1, b = 2;
  {
    int a = 2;
    b = b + a;
  }
  return b;
}"#;
    let text_form_ir = generate_ir_from_input(input);
    print!("{:#}", text_form_ir);
    assert_eq!(
        text_form_ir,
        r#"fun @main(): i32 {
%entry:
  %0 = alloc i32
  store 1, %0
  %1 = alloc i32
  store 2, %1
  %2 = alloc i32
  store 2, %2
  %3 = load %1
  %4 = load %2
  %5 = add %3, %4
  store %5, %1
  %6 = load %1
  ret %6
}
"#
    );
}
