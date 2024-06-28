use koopa::ir;

pub enum Scope<'a, 'b> {
    Program,
    Function(&'a ir::Function),
    BasicBlock(&'b ir::Function, &'b ir::BasicBlock),
}
