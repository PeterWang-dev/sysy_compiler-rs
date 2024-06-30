use super::scope;

mod eval;
mod gen;
mod symbol_table;

#[cfg(test)]
mod tests;

pub use gen::IrGenerator;
