#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}

#[derive(Debug)]
pub struct FuncDef {
    pub ret_type: FuncType,
    pub ident: String,
    pub block: Block,
}

#[derive(Debug)]
pub enum FuncType {
    Int,
}

#[derive(Debug)]
pub struct Block {
    pub stmt: Stmt,
}

#[derive(Debug)]
pub struct Stmt {
    pub num: i32,
}

#[cfg(test)]
mod tests {
    use crate::sysy::CompUnitParser;

    #[test]
    fn test_ast_main() {
        // Pay attention: new line should not be inserted after `r#"` as it will be included in the string
        let input = r#"int main() {
  // This is a comment, should be ignored
  /* This is a block comment,
  should be ignored */
  return 0;
}"#;

        let ast = CompUnitParser::new().parse(input).unwrap();
        assert_eq!(
            format!("{:#?}", ast),
            r#"CompUnit {
    func_def: FuncDef {
        ret_type: Int,
        ident: "main",
        block: Block {
            stmt: Stmt {
                num: 0,
            },
        },
    },
}"#
        );
    }
}
