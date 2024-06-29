use super::scope::Scope;
use crate::{ast::*, error::Error};
use koopa::ir::{self, builder_traits::*, BinaryOp, Program, Type, Value};

/// A trait for generating IR from AST nodes.
trait GenerateIr {
    type Output;

    fn generate(&self, program: &mut Program, scope: Scope) -> Result<Self::Output, Error>;
}

impl GenerateIr for CompUnit {
    type Output = ();

    fn generate(&self, program: &mut Program, scope: Scope) -> Result<Self::Output, Error> {
        self.func_def.generate(program, scope)?;
        Ok(())
    }
}

impl GenerateIr for FuncDef {
    type Output = ();

    fn generate(&self, program: &mut Program, scope: Scope) -> Result<Self::Output, Error> {
        match scope {
            Scope::Program => (),
            _ => unreachable!("Function definition must be in Program scope!"),
        }

        let func_info = ir::FunctionData::new(
            format!("@{}", self.ident),
            vec![],
            match self.func_type {
                FuncType::Int => Type::get_i32(),
            },
        );
        let func = program.new_func(func_info);
        self.block.generate(program, Scope::Function(&func))?;
        Ok(())
    }
}

impl GenerateIr for Block {
    type Output = ();

    fn generate(&self, program: &mut Program, scope: Scope) -> Result<Self::Output, Error> {
        let &func = match scope {
            Scope::Function(f) => f,
            _ => unreachable!("Block must be in a Function scope!"),
        };
        let func_data = program.func_mut(func);

        let entry = func_data
            .dfg_mut()
            .new_bb()
            .basic_block(Some("%entry".into()));

        func_data.layout_mut().bbs_mut().extend([entry]);

        self.stmt
            .generate(program, Scope::BasicBlock(&func, &entry))?;

        Ok(())
    }
}

impl GenerateIr for Stmt {
    type Output = ();

    fn generate(&self, program: &mut Program, scope: Scope) -> Result<Self::Output, Error> {
        let (&func, block) = match scope {
            Scope::BasicBlock(f, b) => (f, b),
            _ => unreachable!("Stmt must be in a BasicBlock scope!"),
        };

        let ret_val = self.expr.generate(program, scope)?;

        let func_data = program.func_mut(func);
        let dfg = func_data.dfg_mut();

        let ret = dfg.new_value().ret(Some(ret_val));

        func_data
            .layout_mut()
            .bb_mut(*block)
            .insts_mut()
            .extend([ret]);

        Ok(())
    }
}

impl GenerateIr for Expr {
    type Output = Value;

    fn generate(&self, program: &mut Program, scope: Scope) -> Result<Self::Output, Error> {
        // no need to check scope here as Expr can only be in Stmt, and check has been done in Stmt
        // also scope not used here
        match self {
            Expr::UnaryExpr(e) => e.generate(program, scope),
        }
    }
}

impl GenerateIr for UnaryExpr {
    type Output = Value;

    fn generate(&self, program: &mut Program, scope: Scope) -> Result<Self::Output, Error> {
        let &func = match scope {
            Scope::BasicBlock(f, _) => f,
            _ => unreachable!("UnaryExpr must be in a BasicBlock scope!"),
        };

        match self {
            UnaryExpr::PrimaryExpr(e) => e.generate(program, scope),
            UnaryExpr::Unary(op, e) => {
                let val = e.generate(program, scope);
                let dfg = program.func_mut(func).dfg_mut();
                match op {
                    UnaryOp::Positive => val,
                    UnaryOp::Negative => {

                        let zero = dfg.new_value().integer(0);
                        Ok(dfg.new_value().binary(BinaryOp::Sub, zero, val?))
                    }
                    UnaryOp::LogicalNot => {
                        let zero = dfg.new_value().integer(0);
                        Ok(dfg.new_value().binary(BinaryOp::Eq, val?, zero))
                    }
                }
            }
        }
    }
}

impl GenerateIr for PrimaryExpr {
    type Output = Value;

    fn generate(&self, program: &mut Program, scope: Scope) -> Result<Self::Output, Error> {
        let dfg = match scope {
            Scope::BasicBlock(f, _) => program.func_mut(*f).dfg_mut(),
            _ => unreachable!("PrimaryExpr must be in a BasicBlock scope!"),
        };

        match self {
            PrimaryExpr::Number(n) => Ok(dfg.new_value().integer(*n)),
            PrimaryExpr::Expr(e) => e.generate(program, scope),
        }
    }
}

pub fn generate_on(ast: &CompUnit) -> Result<Program, Error> {
    let mut program = Program::new();
    ast.generate(&mut program, Scope::Program)?;
    Ok(program)
}

#[cfg(test)]
mod tests {
    use super::*;
    use koopa::back::KoopaGenerator;

    #[test]
    fn test_simple() {
        let ast = CompUnit {
            func_def: FuncDef {
                ident: "main".into(),
                func_type: FuncType::Int,
                block: Block {
                    stmt: Stmt {
                        expr: Expr::UnaryExpr(UnaryExpr::PrimaryExpr(PrimaryExpr::Number(0))),
                    },
                },
            },
        };

        let program = generate_on(&ast).unwrap();

        let mut gen = KoopaGenerator::new(Vec::new());
        gen.generate_on(&program).unwrap();
        let text_form_ir = std::str::from_utf8(&gen.writer()).unwrap().to_string();
        assert_eq!(
            text_form_ir,
            r#"fun @main(): i32 {
%entry:
  ret 0
}
"#
        );
    }
}
