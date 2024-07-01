use super::*;

    #[test]
    fn test_unary() {
        let input = r#"int main() {
  return +(- -!6);  // looks like a smiley face
}"#;
        let text_form_ir = generate_ir_from_input(input);
        assert_eq!(
            text_form_ir,
            r#"fun @main(): i32 {
%entry:
  %0 = eq 6, 0
  %1 = sub 0, %0
  %2 = sub 0, %1
  ret %2
}
"#
        );
    }

    #[test]
    fn test_pos() {
        let input = r#"int main() {
  return +2;
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
    fn test_arithmatic() {
        let input = r#"int main() {
  return 1 + 2 * 3;
}"#;
        let text_form_ir = generate_ir_from_input(input);
        assert_eq!(
            text_form_ir,
            r#"fun @main(): i32 {
%entry:
  %0 = mul 2, 3
  %1 = add 1, %0
  ret %1
}
"#
        );
    }

    #[test]
    fn test_neq() {
        let input = r#"int main() {
  return 1 <= 2;
}
"#;
        let text_form_ir = generate_ir_from_input(input);
        assert_eq!(
            text_form_ir,
            r#"fun @main(): i32 {
%entry:
  %0 = le 1, 2
  ret %0
}
"#
        );
    }

    #[test]
    fn test_lor() {
        let input = r#"int main() {
  return 11 || 0;
}"#;
        let text_form_ir = generate_ir_from_input(input);
        assert_eq!(
            text_form_ir,
            r#"fun @main(): i32 {
%entry:
  %0 = ne 11, 0
  %1 = ne 0, 0
  %2 = or %0, %1
  ret %2
}
"#,
        );
    }

    #[test]
    fn test_land() {
        let input = r#"int main() {
  return 2 && 4;
}
"#;
        let text_form_ir = generate_ir_from_input(input);
        assert_eq!(
            text_form_ir,
            r#"fun @main(): i32 {
%entry:
  %0 = ne 2, 0
  %1 = ne 4, 0
  %2 = and %0, %1
  ret %2
}
"#,
        );
    }

    #[test]
    fn test_complex() {
        let input = r#"int main() {
  return 1 + 2 * (!3 || 4) < 5 != 6 && -7;
}
"#;
        let text_form_ir = generate_ir_from_input(input);
        assert_eq!(
            text_form_ir,
            r#"fun @main(): i32 {
%entry:
  %0 = eq 3, 0
  %1 = ne %0, 0
  %2 = ne 4, 0
  %3 = or %1, %2
  %4 = mul 2, %3
  %5 = add 1, %4
  %6 = lt %5, 5
  %7 = ne %6, 6
  %8 = sub 0, 7
  %9 = ne %7, 0
  %10 = ne %8, 0
  %11 = and %9, %10
  ret %11
}
"#,
        );
    }