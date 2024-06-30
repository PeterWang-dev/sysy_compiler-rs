use super::scope::Scope;
use crate::error::Error;
use koopa::ir::*;
use layout::BasicBlockNode;

type Asm = String;

trait GenerateAsm {
    // Implement this trait for handles that have information of generating code
    fn generate(&self, program: &Program, scope: Scope) -> Result<Asm, Error>;
}

impl GenerateAsm for Program {
    fn generate(&self, program: &Program, scope: Scope) -> Result<Asm, Error> {
        match scope {
            Scope::Program => (),
            _ => unreachable!("Program must be in program scope!"),
        }

        let mut asm = Asm::new();
        asm.push_str(format!("  .text\n").as_str());
        asm.push_str(format!("  .globl main\n").as_str());

        for func in self.func_layout() {
            let func_asm = func.generate(program, Scope::Function(*func))?;
            asm.push_str(&func_asm);
        }

        Ok(asm)
    }
}

impl GenerateAsm for Function {
    fn generate(&self, program: &Program, scope: Scope) -> Result<Asm, Error> {
        let (func, func_data) = match scope {
            Scope::Function(f) => (f, program.func(f)),
            _ => unreachable!("Function must be in a function scope!"),
        };

        let mut asm = Asm::new();

        // Attention: should skip the first character in IR '@'/'%'
        let name_raw = &func_data.name()[1..];
        asm.push_str(format!("{}:\n", name_raw).as_str());

        for (&bb, node) in func_data.layout().bbs() {
            // BasicBlockNode contains instructions instead of BasicBlock
            let bb_asm = node.generate(program, Scope::BasicBlock(func, bb))?;
            asm.push_str(&bb_asm);
        }

        Ok(asm)
    }
}

impl GenerateAsm for BasicBlockNode {
    fn generate(&self, program: &Program, scope: Scope) -> Result<Asm, Error> {
        let (func, bb) = match scope {
            Scope::BasicBlock(f, b) => (f, b),
            _ => unreachable!("Basic block must be in a basic block scope!"),
        };

        let mut asm = Asm::new();

        for inst in self.insts().keys() {
            let inst_asm = inst.generate(program, Scope::BasicBlock(func, bb))?;
            asm.push_str(&inst_asm);
        }

        Ok(asm)
    }
}

impl GenerateAsm for Value {
    fn generate(&self, program: &Program, scope: Scope) -> Result<Asm, Error> {
        let func_data = match scope {
            Scope::BasicBlock(f, _) => program.func(f),
            _ => unreachable!("Value must be in a basic block scope!"),
        };

        let value_data = func_data.dfg().value(*self);

        let mut asm = Asm::new();
        asm.push_str(
            match value_data.kind() {
                ValueKind::Integer(num) => format!("{}", num.value()),
                ValueKind::ZeroInit(_) => todo!(),
                ValueKind::Undef(_) => todo!(),
                ValueKind::Aggregate(_) => todo!(),
                ValueKind::FuncArgRef(_) => todo!(),
                ValueKind::BlockArgRef(_) => todo!(),
                ValueKind::Alloc(_) => todo!(),
                ValueKind::GlobalAlloc(_) => todo!(),
                ValueKind::Load(_) => todo!(),
                ValueKind::Store(_) => todo!(),
                ValueKind::GetPtr(_) => todo!(),
                ValueKind::GetElemPtr(_) => todo!(),
                ValueKind::Binary(_) => todo!(),
                ValueKind::Branch(_) => todo!(),
                ValueKind::Jump(_) => todo!(),
                ValueKind::Call(_) => todo!(),
                ValueKind::Return(ret) => {
                    (match ret.value() {
                        Some(val) => {
                            let ret_val = val.generate(program, scope)?;
                            format!("  li a0, {}\n", ret_val)
                        }
                        None => format!(""),
                    }) + "  ret\n"
                }
            }
            .as_str(),
        );

        Ok(asm)
    }
}

pub fn generate_on(program: &Program) -> Result<String, Error> {
    program.generate(program, Scope::Program)
}

#[cfg(test)]
mod tests {
    use super::{super::ir, *};
    use crate::sysy::CompUnitParser;

    #[test]
    fn test_asm_main() {
        // Pay attention: new line should not be inserted after `r#"` as it will be included in the string
        let input = r#"int main() {
  // This is a comment, should be ignored
  /* This is a block comment,
  should be ignored */
  return 0;
}"#;

        let ast = CompUnitParser::new().parse(input).unwrap();
        let mut gen = ir::IrGenerator::new();
        gen.generate_on(&ast).unwrap();
        let program = gen.program();
        let asm = generate_on(&program).unwrap();

        assert_eq!(
            asm,
            r#"  .text
  .globl main
main:
  li a0, 0
  ret
"#
        );
    }
}
