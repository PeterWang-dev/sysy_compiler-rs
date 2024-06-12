use crate::{ast::*, error::Error};
use koopa::ir::{self, builder_traits::*, Program, Type};

enum Scope {
    Program,
    Function(ir::Function),
    BasicBlock(ir::Function, ir::BasicBlock),
}

trait IrGenerator {
    fn generate(&self, program: &mut Program, scope: Scope) -> Result<(), Error>;
}

impl IrGenerator for CompUnit {
    fn generate(&self, program: &mut Program, scope: Scope) -> Result<(), Error> {
        self.func_def.generate(program, scope);
        Ok(())
    }
}

impl IrGenerator for FuncDef {
    fn generate(&self, program: &mut Program, scope: Scope) -> Result<(), Error> {
        match scope {
            Scope::Program => (),
            _ => unreachable!("Function definition must be in program scope!"),
        }

        let func_info = ir::FunctionData::new(
            format!("@{}", self.ident),
            vec![],
            match self.ret_type {
                FuncType::Int => Type::get_i32(),
            },
        );
        let func = program.new_func(func_info);
        self.block.generate(program, Scope::Function(func))?;
        Ok(())
    }
}

impl IrGenerator for Block {
    fn generate(&self, program: &mut Program, scope: Scope) -> Result<(), Error> {
        let func = match scope {
            Scope::Function(f) => f,
            _ => unreachable!("Block must be in a function scope!"),
        };
        let func_data = program.func_mut(func);

        let entry = func_data
            .dfg_mut()
            .new_bb()
            .basic_block(Some("%entry".into()));

        func_data.layout_mut().bbs_mut().extend([entry]);

        self.stmt
            .generate(program, Scope::BasicBlock(func, entry))?;

        Ok(())
    }
}

impl IrGenerator for Stmt {
    fn generate(&self, program: &mut Program, scope: Scope) -> Result<(), Error> {
        let (function, block) = match scope {
            Scope::BasicBlock(t, b) => (t, b),
            _ => unreachable!("Statement must be in a basic block scope!"),
        };
        let func_data = program.func_mut(function);

        let dfg = func_data.dfg_mut();
        let res = dfg.new_value().integer(self.num);
        let ret = dfg.new_value().ret(res.into());

        func_data
            .layout_mut()
            .bb_mut(block)
            .insts_mut()
            .extend([ret]);

        Ok(())
    }
}

pub fn generate_ir(ast: &CompUnit) -> Result<Program, Error> {
    let mut program = Program::new();
    ast.generate(&mut program, Scope::Program)?;
    Ok(program)
}

#[cfg(test)]
mod tests {
    use super::*;
    use koopa::back::KoopaGenerator;

    #[test]
    fn test_generate_ir() {
        let ast = CompUnit {
            func_def: FuncDef {
                ident: "main".into(),
                ret_type: FuncType::Int,
                block: Block {
                    stmt: Stmt { num: 0 },
                },
            },
        };

        let program = generate_ir(&ast).unwrap();

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
