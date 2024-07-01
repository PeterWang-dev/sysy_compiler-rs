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
    assert_eq!(
        text_form_ir,
        r#"fun @main(): i32 {
%entry:
  %0 = alloc i32
  store 2, %0
  %1 = load %0
  br %1, %then, %else

%then:
  store 2, %0
  jump %end

%else:
  store 0, %0
  jump %end

%end:
  %2 = load %0
  ret %2
}
"#
    );
}
