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
            Expr::LOrExpr(e) => e.generate(program, scope),
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
                    UnaryOp::Positive => val, // ! Bug: Need further investigation (panicked at 'can not name constants')
                    UnaryOp::Negative => {
                        let zero = dfg.new_value().integer(0);
                        let neg = dfg.new_value().binary(BinaryOp::Sub, zero, val);
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .extend([neg]);
                        neg
                    }
                    UnaryOp::LogicalNot => {
                        let zero = dfg.new_value().integer(0);
                        let l_not = dfg.new_value().binary(BinaryOp::Eq, val, zero);
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .extend([l_not]);
                        l_not
                    }
                };

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
            _ => unreachable!("MulExpr must be in a BasicBlock scope!"),
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
            _ => unreachable!("AddExpr must be in a BasicBlock scope!"),
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

impl GenerateIr for RelExpr {
    type Output = Value;

    fn generate(&self, program: &mut Program, scope: Scope) -> Result<Self::Output, Error> {
        let (&func, &block) = match scope {
            Scope::BasicBlock(f, b) => (f, b),
            _ => unreachable!("RelExpr must be in a BasicBlock scope!"),
        };

        match self {
            RelExpr::AddExpr(e) => e.generate(program, scope),
            RelExpr::Rel(rel_expr, op, add_expr) => {
                let lhs = rel_expr.generate(program, Scope::BasicBlock(&func, &block))?;
                let rhs = add_expr.generate(program, Scope::BasicBlock(&func, &block))?;

                let func_data = program.func_mut(func);
                let dfg = func_data.dfg_mut();

                let rel = match op {
                    RelOp::Less => dfg.new_value().binary(BinaryOp::Lt, lhs, rhs),
                    RelOp::LessEqual => dfg.new_value().binary(BinaryOp::Le, lhs, rhs),
                    RelOp::Greater => dfg.new_value().binary(BinaryOp::Gt, lhs, rhs),
                    RelOp::GreaterEqual => dfg.new_value().binary(BinaryOp::Ge, lhs, rhs),
                };

                func_data
                    .layout_mut()
                    .bb_mut(block)
                    .insts_mut()
                    .extend([rel]);

                Ok(rel)
            }
        }
    }
}

impl GenerateIr for EqExpr {
    type Output = Value;

    fn generate(&self, program: &mut Program, scope: Scope) -> Result<Self::Output, Error> {
        let (&func, &block) = match scope {
            Scope::BasicBlock(f, b) => (f, b),
            _ => unreachable!("EqExpr must be in a BasicBlock scope!"),
        };

        match self {
            EqExpr::RelExpr(e) => e.generate(program, scope),
            EqExpr::Eq(eq_expr, op, rel_expr) => {
                let lhs = eq_expr.generate(program, Scope::BasicBlock(&func, &block))?;
                let rhs = rel_expr.generate(program, Scope::BasicBlock(&func, &block))?;

                let func_data = program.func_mut(func);
                let dfg = func_data.dfg_mut();

                let eq = match op {
                    EqOp::Equal => dfg.new_value().binary(BinaryOp::Eq, lhs, rhs),
                    EqOp::NotEqual => dfg.new_value().binary(BinaryOp::NotEq, lhs, rhs),
                };

                func_data
                    .layout_mut()
                    .bb_mut(block)
                    .insts_mut()
                    .extend([eq]);

                Ok(eq)
            }
        }
    }
}

impl GenerateIr for LAndExpr {
    type Output = Value;

    fn generate(&self, program: &mut Program, scope: Scope) -> Result<Self::Output, Error> {
        let (&func, &block) = match scope {
            Scope::BasicBlock(f, b) => (f, b),
            _ => unreachable!("LAndExpr must be in a BasicBlock scope!"),
        };

        match self {
            LAndExpr::EqExpr(e) => e.generate(program, scope),
            LAndExpr::LAnd(l_and_expr, eq_expr) => {
                let lhs = l_and_expr.generate(program, Scope::BasicBlock(&func, &block))?;
                let rhs = eq_expr.generate(program, Scope::BasicBlock(&func, &block))?;

                let func_data = program.func_mut(func);
                let dfg = func_data.dfg_mut();

                let zero = dfg.new_value().integer(0);
                let logic_lhs = dfg.new_value().binary(BinaryOp::NotEq, lhs, zero);
                let logic_rhs = dfg.new_value().binary(BinaryOp::NotEq, rhs, zero);
                let l_and = dfg.new_value().binary(BinaryOp::And, logic_lhs, logic_rhs);

                func_data
                    .layout_mut()
                    .bb_mut(block)
                    .insts_mut()
                    .extend([logic_lhs, logic_rhs, l_and]);

                Ok(l_and)
            }
        }
    }
}

impl GenerateIr for LOrExpr {
    type Output = Value;

    fn generate(&self, program: &mut Program, scope: Scope) -> Result<Self::Output, Error> {
        let (&func, &block) = match scope {
            Scope::BasicBlock(f, b) => (f, b),
            _ => unreachable!("LOrExpr must be in a BasicBlock scope!"),
        };

        match self {
            LOrExpr::LAndExpr(e) => e.generate(program, scope),
            LOrExpr::LOr(l_or_expr, l_and_expr) => {
                let lhs = l_or_expr.generate(program, Scope::BasicBlock(&func, &block))?;
                let rhs = l_and_expr.generate(program, Scope::BasicBlock(&func, &block))?;

                let func_data = program.func_mut(func);
                let dfg = func_data.dfg_mut();

                // ! Bug: Need further investigation
                let zero = dfg.new_value().integer(0);
                let logic_lhs = dfg.new_value().binary(BinaryOp::NotEq, lhs, zero);
                let logic_rhs = dfg.new_value().binary(BinaryOp::NotEq, rhs, zero);
                let l_or = dfg.new_value().binary(BinaryOp::Or, logic_lhs, logic_rhs);

                func_data
                    .layout_mut()
                    .bb_mut(block)
                    .insts_mut()
                    .extend([logic_lhs, logic_rhs, l_or]);

                Ok(l_or)
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

    fn generate_ir_from_input(input: &str) -> String {
        let ast = CompUnitParser::new().parse(input).unwrap();
        let program = generate_on(&ast).unwrap();
        let mut gen = KoopaGenerator::new(Vec::new());
        gen.generate_on(&program).unwrap();
        std::str::from_utf8(&gen.writer()).unwrap().to_string()
    }

    #[test]
    fn test_simple() {
        let input = r#"int main() {
  // This is a comment, should be ignored
  /* This is a block comment,
  should be ignored */
  return 0;
}"#;
        let text_form_ir = generate_ir_from_input(input);
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

    mod test_expr {
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
            print!("{:#}", text_form_ir);
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
    }
}
