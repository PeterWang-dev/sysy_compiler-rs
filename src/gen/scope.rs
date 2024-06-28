use koopa::ir;

pub enum Scope<'a> {
    Program,
    Function(&'a ir::Function),
    BasicBlock(&'a ir::Function, &'a ir::BasicBlock),
}
