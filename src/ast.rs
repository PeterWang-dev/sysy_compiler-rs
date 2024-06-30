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
    LOrExpr(LOrExpr),
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
