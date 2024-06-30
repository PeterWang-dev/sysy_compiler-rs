use koopa::ir;

#[derive(Clone)]
pub enum Scope {
    Program,
    Function(ir::Function),
    BasicBlock(ir::Function, ir::BasicBlock),
    Decl(ir::Type),
}
