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
    %a = alloca i32
    store i32 1, i32* %a
    %b = alloca i32
    store i32 2, i32* %b
    %a1 = alloca i32
    store i32 2, i32* %a1
    %b2 = load i32, i32* %b
    %a3 = load i32, i32* %a1
    %add = add i32 %b2, %a3
    store i32 %add, i32* %b
    %b4 = load i32, i32* %b
    ret i32 %b4
}
"#
    );
}
