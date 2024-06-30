use std::collections::HashMap;

pub struct SymbolTable {
    table: HashMap<String, i32>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            table: HashMap::new(),
        }
    }

    pub fn insert(&mut self, name: String, value: i32) {
        self.table.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<&i32> {
        self.table.get(name)
    }
}
