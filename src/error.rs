#[derive(Debug)]
pub enum Error {
    ParseError(String),
    SemanticError(String),
}