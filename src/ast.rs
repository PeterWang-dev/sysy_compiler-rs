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
    pub expr: Expr,
}

#[derive(Debug)]
pub enum Expr {
    AddExpr(AddExpr),
}

#[derive(Debug)]
pub enum PrimaryExpr {
    Expr(Box<Expr>),
    Number(i32),
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
    Mul(Box<MulExpr>,MulOp, UnaryExpr),
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