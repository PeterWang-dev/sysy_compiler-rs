use koopa::ir;

#[derive(Debug, Clone)]
pub enum Scope {
    Program,
    Function(ir::Function),
    BasicBlock(ir::Function, ir::BasicBlock),
    Decl(ir::Function, ir::BasicBlock, ir::Type),
}
