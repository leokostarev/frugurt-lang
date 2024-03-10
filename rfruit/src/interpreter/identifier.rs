use once_cell::sync::Lazy;
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    hash::{DefaultHasher, Hash, Hasher},
};

// this map is used for Identifier visualization
static mut BACKWARDS_MAP: Lazy<HashMap<u64, String>> = Lazy::new(|| HashMap::new());

#[derive(Hash, PartialEq, Eq, Copy, Clone, PartialOrd, Ord)]
pub struct Identifier {
    // holds hash for fast comparison and copy
    hashed_ident: u64,
}

#[derive(Hash, PartialEq, Eq, Copy, Clone, PartialOrd, Ord)]
pub struct OperatorIdentifier {
    pub op: Identifier,
    pub left: Identifier,
    pub right: Identifier,
}

impl Identifier {
    pub fn new(ident: &str) -> Self {
        let mut hasher = DefaultHasher::new();
        ident.hash(&mut hasher);
        let hash = hasher.finish();

        unsafe {
            // oh, no I am such a bad boy))))
            if !BACKWARDS_MAP.contains_key(&hash) {
                BACKWARDS_MAP.insert(hash, ident.to_string());
            }
        }

        Self { hashed_ident: hash }
    }
}
impl OperatorIdentifier {
    pub fn new(op: Identifier, left: Identifier, right: Identifier) -> Self {
        Self { op, left, right }
    }
}

impl Debug for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", unsafe {
            BACKWARDS_MAP.get(&self.hashed_ident).unwrap()
        })
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Debug for OperatorIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Operator({} {} {})", self.left, self.op, self.right)
    }
}
