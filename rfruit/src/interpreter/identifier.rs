use std::fmt::Display;
use std::hash::Hash;

#[derive(Debug, Hash, PartialEq, Eq, Clone, PartialOrd, Ord)]
pub struct Identifier {
    // later should hold hash for fast comparison
    name: String,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, PartialOrd, Ord)]
pub struct OperatorIdentifier {
    pub op: Identifier,
    pub left: Identifier,
    pub right: Identifier,
}

impl Identifier {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
        }
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl OperatorIdentifier {
    pub fn new(op: Identifier, left: Identifier, right: Identifier) -> Self {
        Self { op, left, right }
    }
}
