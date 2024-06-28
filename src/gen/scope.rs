use koopa::ir;

pub enum Scope<'a, 'b> {
    Program,
    Function(&'a ir::Function),
    BasicBlock(&'a ir::Function, &'b ir::BasicBlock),
}
