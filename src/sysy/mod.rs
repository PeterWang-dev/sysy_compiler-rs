use lalrpop_util::lalrpop_mod;

lalrpop_mod!(lex, "/sysy/lex.rs"); // generated parser

pub use lex::CompUnitParser;

#[cfg(test)]
mod test {
    use super::lex::CompUnitParser;

    #[test]
    fn test_parser() {
        let input = r#"
            int main() {
                return 0;
            }
        "#;

        let ast = CompUnitParser::new().parse(input).unwrap();

        assert_eq!(format!("{}", ast), "int main() { return 0; }");
    }
}