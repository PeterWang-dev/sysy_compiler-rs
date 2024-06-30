use crate::error::Error;
use koopa::ir::Value;
use std::collections::HashMap;

#[derive(Clone, Copy)]
pub enum SymbolValue {
    Const(i32),
    Var(Value),
}

pub struct SymbolTable {
    table: HashMap<String, SymbolValue>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            table: HashMap::new(),
        }
    }

    pub fn try_insert(&mut self, name: String, value: SymbolValue) -> Result<(), Error> {
        if self.table.contains_key(&name) {
            return Err(Error::SemanticError(format!("{} already exists", name)));
        }

        self.table.insert(name, value);
        Ok(())
    }

    pub fn get(&self, name: &str) -> Option<&SymbolValue> {
        self.table.get(name)
    }
}
