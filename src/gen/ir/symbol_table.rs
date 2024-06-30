use crate::error::Error;
use koopa::ir::Value;
use std::collections::HashMap;

#[derive(Clone, Copy)]
pub enum SymbolValue {
    Const(i32),
    Var(Value),
}

pub struct SymbolTable {
    table_stack: Vec<HashMap<String, SymbolValue>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            table_stack: vec![HashMap::new()],
        }
    }

    pub fn enter_scope(&mut self) {
        self.table_stack.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.table_stack.pop();
    }

    pub fn try_insert(&mut self, name: String, value: SymbolValue) -> Result<(), Error> {
        let top = self.table_stack.last_mut().unwrap();
        if top.contains_key(&name) {
            return Err(Error::SemanticError(format!("{} already exists", name)));
        }
        top.insert(name, value);
        Ok(())
    }

    pub fn get(&self, name: &str) -> Option<&SymbolValue> {
        for table in self.table_stack.iter().rev() {
            if let Some(value) = table.get(name) {
                return Some(value);
            }
        }
        None
    }
}
