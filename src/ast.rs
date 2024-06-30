#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}

#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Block,
}

#[derive(Debug, Copy, Clone)]
pub enum FuncType {
    Int,
}

#[derive(Debug)]
pub struct Block {
    pub items: Vec<BlockItem>,
}

#[derive(Debug)]
pub enum BlockItem {
    Decl(Decl),
    Stmt(Stmt),
}

#[derive(Debug)]
pub enum Decl {
    ConstDecl(ConstDecl),
    VarDecl(VarDecl),
}

#[derive(Debug)]
pub struct ConstDecl {
    pub ty: BType,
    pub defs: Vec<ConstDef>,
}

#[derive(Debug, Copy, Clone)]
pub enum BType {
    Int,
}

#[derive(Debug)]
pub struct ConstDef {
    pub ident: String,
    pub init_val: ConstInitVal,
}

#[derive(Debug)]
pub enum ConstInitVal {
    ConstExpr(ConstExpr),
}

#[derive(Debug)]
pub enum ConstExpr {
    Expr(Expr),
}

#[derive(Debug)]
pub struct VarDecl {
    pub ty: BType,
    pub defs: Vec<VarDef>,
}

#[derive(Debug)]
pub enum VarDef {
    Ident(String),
    Init(String, InitVal),
}

#[derive(Debug)]
pub enum InitVal {
    Expr(Expr),
}

#[derive(Debug)]
pub enum Stmt {
    Assign(LVal, Expr),
    Return(Expr),
}

#[derive(Debug)]
pub enum Expr {
    LOrExpr(LOrExpr),
}

#[derive(Debug)]
pub enum PrimaryExpr {
    Expr(Box<Expr>),
    LVal(LVal),
    Number(i32),
}

#[derive(Debug)]
pub enum LVal {
    Ident(String),
}

#[derive(Debug)]
pub enum UnaryExpr {
    PrimaryExpr(PrimaryExpr),
    Unary(UnaryOp, Box<UnaryExpr>),
}

#[derive(Debug)]
pub enum UnaryOp {
    Positive,
    Negative,
    LogicalNot,
}

#[derive(Debug)]
pub enum MulExpr {
    UnaryExpr(UnaryExpr),
    Mul(Box<MulExpr>, MulOp, UnaryExpr),
}

#[derive(Debug)]
pub enum MulOp {
    Multiply,
    Divide,
    Module,
}

#[derive(Debug)]
pub enum AddExpr {
    MulExpr(MulExpr),
    Add(Box<AddExpr>, AddOp, MulExpr),
}

#[derive(Debug)]
pub enum AddOp {
    Add,
    Subtract,
}

#[derive(Debug)]
pub enum RelExpr {
    AddExpr(AddExpr),
    Rel(Box<RelExpr>, RelOp, AddExpr),
}

#[derive(Debug)]
pub enum RelOp {
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

#[derive(Debug)]
pub enum EqExpr {
    RelExpr(RelExpr),
    Eq(Box<EqExpr>, EqOp, RelExpr),
}

#[derive(Debug)]
pub enum EqOp {
    Equal,
    NotEqual,
}

#[derive(Debug)]
pub enum LAndExpr {
    EqExpr(EqExpr),
    LAnd(Box<LAndExpr>, EqExpr),
}

#[derive(Debug)]
pub enum LOrExpr {
    LAndExpr(LAndExpr),
    LOr(Box<LOrExpr>, LAndExpr),
}

#[cfg(test)]
mod tests {
    use crate::sysy::CompUnitParser;

    #[test]
    #[ignore]
    fn print_ast() {
        let input = r#"int main() {
  int x = 10;
  x = x + 1;
  return x;
}
"#;
        let ast = CompUnitParser::new().parse(&input).unwrap();
        println!("{:#?}", ast);
    }
}
