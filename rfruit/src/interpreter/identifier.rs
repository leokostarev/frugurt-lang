use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::fmt::Display;
use std::hash::{DefaultHasher, Hash, Hasher};

// this map is used for Identifier visualization
static mut BACKWARDS_MAP: Lazy<HashMap<u64, String>> = Lazy::new(|| HashMap::new());

#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone, PartialOrd, Ord)]
pub struct Identifier {
    // holds hash for fast comparison and copy
    name: u64,
}

#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone, PartialOrd, Ord)]
pub struct OperatorIdentifier {
    pub op: Identifier,
    pub left: Identifier,
    pub right: Identifier,
}

impl Identifier {
    pub fn new(name: &str) -> Self {
        let mut hasher = DefaultHasher::new();
        name.hash(&mut hasher);
        let hash = hasher.finish();

        unsafe {
            // oh, no I am such a bad boy))))
            if !BACKWARDS_MAP.contains_key(&hash) {
                BACKWARDS_MAP.insert(hash, name.to_string());
            }
        }

        Self { name: hash }
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", unsafe { BACKWARDS_MAP.get(&self.name).unwrap() })
    }
}

impl OperatorIdentifier {
    pub fn new(op: Identifier, left: Identifier, right: Identifier) -> Self {
        Self { op, left, right }
    }
}
