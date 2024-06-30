use super::symbol_table::SymbolTable;
use crate::{ast::*, error::Error};

pub trait Evaluate {
    type Output;

    fn eval(&self, sym_table: &SymbolTable) -> Result<Self::Output, Error>;
}

impl Evaluate for ConstExpr {
    type Output = i32;

    fn eval(&self, sym_table: &SymbolTable) -> Result<Self::Output, Error> {
        match self {
            ConstExpr::Expr(e) => e.eval(sym_table),
        }
    }
}

impl Evaluate for Expr {
    type Output = i32;

    fn eval(&self, sym_table: &SymbolTable) -> Result<Self::Output, Error> {
        match self {
            Expr::LOrExpr(e) => e.eval(sym_table),
        }
    }
}

impl Evaluate for LOrExpr {
    type Output = i32;

    fn eval(&self, sym_table: &SymbolTable) -> Result<Self::Output, Error> {
        match self {
            LOrExpr::LAndExpr(e) => e.eval(sym_table),
            LOrExpr::LOr(e1, e2) => {
                let v1 = e1.eval(sym_table)?;
                let res = if v1 != 0 { v1 } else { e2.eval(sym_table)? };

                Ok(res)
            }
        }
    }
}

impl Evaluate for LAndExpr {
    type Output = i32;

    fn eval(&self, sym_table: &SymbolTable) -> Result<Self::Output, Error> {
        match self {
            LAndExpr::EqExpr(e) => e.eval(sym_table),
            LAndExpr::LAnd(e1, e2) => {
                let v1 = e1.eval(sym_table)?;
                let res = if v1 == 0 { v1 } else { e2.eval(sym_table)? };

                Ok(res)
            }
        }
    }
}

impl Evaluate for EqExpr {
    type Output = i32;

    fn eval(&self, sym_table: &SymbolTable) -> Result<Self::Output, Error> {
        match self {
            EqExpr::RelExpr(e) => e.eval(sym_table),
            EqExpr::Eq(e1, op, e2) => {
                let v1 = e1.eval(sym_table)?;
                let v2 = e2.eval(sym_table)?;
                let res = match op {
                    EqOp::Equal => (v1 == v2) as i32,
                    EqOp::NotEqual => (v1 != v2) as i32,
                };

                Ok(res)
            }
        }
    }
}

impl Evaluate for RelExpr {
    type Output = i32;

    fn eval(&self, sym_table: &SymbolTable) -> Result<Self::Output, Error> {
        match self {
            RelExpr::AddExpr(e) => e.eval(sym_table),
            RelExpr::Rel(e1, op, e2) => {
                let v1 = e1.eval(sym_table)?;
                let v2 = e2.eval(sym_table)?;
                let res = match op {
                    RelOp::Less => (v1 < v2) as i32,
                    RelOp::LessEqual => (v1 <= v2) as i32,
                    RelOp::Greater => (v1 > v2) as i32,
                    RelOp::GreaterEqual => (v1 >= v2) as i32,
                };

                Ok(res)
            }
        }
    }
}

impl Evaluate for AddExpr {
    type Output = i32;

    fn eval(&self, sym_table: &SymbolTable) -> Result<Self::Output, Error> {
        match self {
            AddExpr::MulExpr(e) => e.eval(sym_table),
            AddExpr::Add(e1, op, e2) => {
                let v1 = e1.eval(sym_table)?;
                let v2 = e2.eval(sym_table)?;
                let res = match op {
                    AddOp::Add => v1 + v2,
                    AddOp::Subtract => v1 - v2,
                };

                Ok(res)
            }
        }
    }
}

impl Evaluate for MulExpr {
    type Output = i32;

    fn eval(&self, sym_table: &SymbolTable) -> Result<Self::Output, Error> {
        match self {
            MulExpr::UnaryExpr(e) => e.eval(sym_table),
            MulExpr::Mul(e1, op, e2) => {
                let v1 = e1.eval(sym_table)?;
                let v2 = e2.eval(sym_table)?;
                let res = match op {
                    MulOp::Multiply => v1 * v2,
                    MulOp::Divide => v1 / v2,
                    MulOp::Module => v1 % v2,
                };

                Ok(res)
            }
        }
    }
}

impl Evaluate for UnaryExpr {
    type Output = i32;

    fn eval(&self, sym_table: &SymbolTable) -> Result<Self::Output, Error> {
        match self {
            UnaryExpr::PrimaryExpr(e) => e.eval(sym_table),
            UnaryExpr::Unary(op, e) => {
                let v = e.eval(sym_table)?;
                let res = match op {
                    UnaryOp::Positive => v,
                    UnaryOp::Negative => -v,
                    UnaryOp::LogicalNot => (v == 0) as i32,
                };

                Ok(res)
            }
        }
    }
}

impl Evaluate for PrimaryExpr {
    type Output = i32;

    fn eval(&self, sym_table: &SymbolTable) -> Result<Self::Output, Error> {
        let res = match self {
            PrimaryExpr::Number(n) => *n,
            PrimaryExpr::LVal(lval) => lval.eval(sym_table)?,
            PrimaryExpr::Expr(e) => e.eval(sym_table)?,
        };

        Ok(res)
    }
}

impl Evaluate for LVal {
    type Output = i32;

    fn eval(&self, sym_table: &SymbolTable) -> Result<Self::Output, Error> {
        match self {
            LVal::Ident(s) => {
                let v = sym_table
                    .get(s)
                    .ok_or(Error::SemanticError(format!("Undefined variable: {}", s)))?;
                Ok(*v)
            }
        }
    }
}
