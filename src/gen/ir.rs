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
            Expr::AddExpr(e) => e.generate(program, scope),
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

impl GenerateIr for UnaryExpr {
    type Output = Value;

    fn generate(&self, program: &mut Program, scope: Scope) -> Result<Self::Output, Error> {
        let (&func, &block) = match scope {
            Scope::BasicBlock(f, b) => (f, b),
            _ => unreachable!("UnaryExpr must be in a BasicBlock scope!"),
        };

        match self {
            UnaryExpr::PrimaryExpr(e) => e.generate(program, scope),
            UnaryExpr::Unary(op, e) => {
                let val = e.generate(program, scope)?;
                let func_data = program.func_mut(func);
                let dfg = func_data.dfg_mut();

                let unary = match op {
                    UnaryOp::Positive => val,
                    UnaryOp::Negative => {
                        let zero = dfg.new_value().integer(0);
                        dfg.new_value().binary(BinaryOp::Sub, zero, val)
                    }
                    UnaryOp::LogicalNot => {
                        let zero = dfg.new_value().integer(0);
                        dfg.new_value().binary(BinaryOp::Eq, val, zero)
                    }
                };

                func_data
                    .layout_mut()
                    .bb_mut(block)
                    .insts_mut()
                    .extend([unary]);

                Ok(unary)
            }
        }
    }
}

impl GenerateIr for MulExpr {
    type Output = Value;

    fn generate(&self, program: &mut Program, scope: Scope) -> Result<Self::Output, Error> {
        let (&func, &block) = match scope {
            Scope::BasicBlock(f, b) => (f, b),
            _ => unreachable!("UnaryExpr must be in a BasicBlock scope!"),
        };

        match self {
            MulExpr::UnaryExpr(e) => e.generate(program, scope),
            MulExpr::Mul(mul_expr, op, unary_expr) => {
                let lhs = mul_expr.generate(program, Scope::BasicBlock(&func, &block))?;
                let rhs = unary_expr.generate(program, Scope::BasicBlock(&func, &block))?;

                let func_data = program.func_mut(func);
                let dfg = func_data.dfg_mut();

                let mul = match op {
                    MulOp::Multiply => dfg.new_value().binary(BinaryOp::Mul, lhs, rhs),
                    MulOp::Divide => dfg.new_value().binary(BinaryOp::Div, lhs, rhs),
                    MulOp::Module => dfg.new_value().binary(BinaryOp::Mod, lhs, rhs),
                };

                func_data
                    .layout_mut()
                    .bb_mut(block)
                    .insts_mut()
                    .extend([mul]);

                Ok(mul)
            }
        }
    }
}

impl GenerateIr for AddExpr {
    type Output = Value;

    fn generate(&self, program: &mut Program, scope: Scope) -> Result<Self::Output, Error> {
        let (&func, &block) = match scope {
            Scope::BasicBlock(f, b) => (f, b),
            _ => unreachable!("UnaryExpr must be in a BasicBlock scope!"),
        };

        match self {
            AddExpr::MulExpr(e) => e.generate(program, scope),
            AddExpr::Add(add_expr, op, mul_expr) => {
                let lhs = add_expr.generate(program, Scope::BasicBlock(&func, &block))?;
                let rhs = mul_expr.generate(program, Scope::BasicBlock(&func, &block))?;

                let func_data = program.func_mut(func);
                let dfg = func_data.dfg_mut();

                let mul = match op {
                    AddOp::Add => dfg.new_value().binary(BinaryOp::Add, lhs, rhs),
                    AddOp::Subtract => dfg.new_value().binary(BinaryOp::Sub, lhs, rhs),
                };

                func_data
                    .layout_mut()
                    .bb_mut(block)
                    .insts_mut()
                    .extend([mul]);

                Ok(mul)
            }
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
    use crate::sysy::CompUnitParser;
    use koopa::back::KoopaGenerator;

    #[test]
    fn test_simple() {
        let input = r#"int main() {
  // This is a comment, should be ignored
  /* This is a block comment,
  should be ignored */
  return 0;
}"#;

        let ast = CompUnitParser::new().parse(input).unwrap();
        let program = generate_on(&ast).unwrap();
        let mut gen = KoopaGenerator::new(Vec::new());
        gen.generate_on(&program).unwrap();
        let text_form_ir = std::str::from_utf8(&gen.writer()).unwrap().to_string();

        assert_eq!(
            text_form_ir,
            // Pay attention: new line should not be inserted after `r#"` as it will be included in the string
            r#"fun @main(): i32 {
%entry:
  ret 0
}
"#
        );
    }

    #[test]
    fn test_unary_expr() {
        let input = r#"int main() {
  return +(- -!6);  // looks like a smiley face
}"#;

        let ast = CompUnitParser::new().parse(input).unwrap();
        let program = generate_on(&ast).unwrap();
        let mut gen = KoopaGenerator::new(Vec::new());
        gen.generate_on(&program).unwrap();
        let text_form_ir = std::str::from_utf8(&gen.writer()).unwrap().to_string();

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
    fn test_arithmatic_expr() {
        let input = r#"int main() {
  return 1 + 2 * 3;
}"#;

        let ast = CompUnitParser::new().parse(input).unwrap();
        let program = generate_on(&ast).unwrap();
        let mut gen = KoopaGenerator::new(Vec::new());
        gen.generate_on(&program).unwrap();
        let text_form_ir = std::str::from_utf8(&gen.writer()).unwrap().to_string();

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
}
