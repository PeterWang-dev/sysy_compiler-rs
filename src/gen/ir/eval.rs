use super::symbol_table::{SymbolTable, SymbolValue};
use crate::{ast::*, error::Error};

pub trait EvaluateConstant {
    type Output;

    fn eval(&self, sym_table: &SymbolTable) -> Result<Self::Output, Error>;
}

impl EvaluateConstant for ConstExpr {
    type Output = i32;

    fn eval(&self, sym_table: &SymbolTable) -> Result<Self::Output, Error> {
        match self {
            ConstExpr::Expr(e) => e.eval(sym_table),
        }
    }
}

impl EvaluateConstant for Expr {
    type Output = i32;

    fn eval(&self, sym_table: &SymbolTable) -> Result<Self::Output, Error> {
        match self {
            Expr::LOrExpr(e) => e.eval(sym_table),
        }
    }
}

impl EvaluateConstant for LOrExpr {
    type Output = i32;

    fn eval(&self, sym_table: &SymbolTable) -> Result<Self::Output, Error> {
        match self {
            LOrExpr::LAndExpr(e) => e.eval(sym_table),
            LOrExpr::LOr(e1, e2) => {
                let logic_v1 = e1.eval(sym_table)? != 0;
                let res = if logic_v1 == true {
                    logic_v1
                } else {
                    let logic_v2 = e2.eval(sym_table)? != 0;
                    logic_v2
                };

                Ok(res as i32)
            }
        }
    }
}

impl EvaluateConstant for LAndExpr {
    type Output = i32;

    fn eval(&self, sym_table: &SymbolTable) -> Result<Self::Output, Error> {
        match self {
            LAndExpr::EqExpr(e) => e.eval(sym_table),
            LAndExpr::LAnd(e1, e2) => {
                let logic_v1 = e1.eval(sym_table)? != 0;
                let res = if logic_v1 == false {
                    logic_v1
                } else {
                    let logic_v2 = e2.eval(sym_table)? != 0;
                    logic_v2
                };

                Ok(res as i32)
            }
        }
    }
}

impl EvaluateConstant for EqExpr {
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

impl EvaluateConstant for RelExpr {
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

impl EvaluateConstant for AddExpr {
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

impl EvaluateConstant for MulExpr {
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

impl EvaluateConstant for UnaryExpr {
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

impl EvaluateConstant for PrimaryExpr {
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

impl EvaluateConstant for LVal {
    type Output = i32;

    fn eval(&self, sym_table: &SymbolTable) -> Result<Self::Output, Error> {
        match self {
            LVal::Ident(s) => {
                let v = sym_table
                    .get(s)
                    .ok_or(Error::SemanticError(format!("Undefined symbol: {}", s)))?;

                match v {
                    SymbolValue::Const(n) => Ok(*n),
                    _ => Err(Error::SemanticError(format!("Not a constant: {}", s))),
                }
            }
        }
    }
}
