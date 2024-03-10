use crate::{rc_refcell, FruError, FruStatement, FruValue, Identifier, Scope};
use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
    fmt::Debug,
    rc::Rc,
};

#[derive(Clone, Copy)]
pub struct FruField {
    pub ident: Identifier,
    pub is_public: bool,
}

#[derive(Clone)]
pub struct FruType {
    pub ident: Identifier,
    pub fields: Vec<FruField>,
    pub watches_by_field: HashMap<Identifier, Vec<Rc<FruStatement>>>,
    pub watches: Vec<Rc<FruStatement>>,
    pub scope: Rc<Scope>,
    // a lot of expected here :: methods / static functions / etc
}

#[derive(Clone)]
pub struct FruObject {
    pub internal: Rc<RefCell<FruObjectInternal>>,
}

pub struct FruObjectInternal {
    pub type_: Rc<FruType>,
    pub fields: Vec<FruValue>,
}

impl FruObject {
    pub fn new(type_: Rc<FruType>, fields: Vec<FruValue>) -> FruValue {
        FruValue::Object(FruObject {
            internal: rc_refcell!(FruObjectInternal { type_, fields }),
        })
    }

    pub fn get_type(&self) -> Rc<FruType> {
        self.internal.borrow().type_.clone()
    }

    pub fn get(&self) -> Ref<FruObjectInternal> {
        self.internal.borrow()
    }

    pub fn get_kth_field(&self, i: usize) -> FruValue {
        self.get().fields[i].clone()
    }

    pub fn set_kth_field(&mut self, i: usize, value: FruValue) {
        self.internal.borrow_mut().fields[i] = value
    }

    pub fn get_field(&self, ident: Identifier) -> Result<FruValue, FruError> {
        for (i, field_ident) in self.get_type().fields.iter().enumerate() {
            if field_ident.ident == ident {
                return Ok(self.get().fields[i].clone());
            }
        }
        FruError::new_res(format!("field {} not found", ident))
    }

    pub fn set_field(&mut self, path: &[Identifier], value: FruValue) -> Result<(), FruError> {
        assert_ne!(path.len(), 0);

        let pos = self
            .get_type()
            .fields
            .iter()
            .position(|f| f.ident == path[0]);

        let pos = match pos {
            Some(p) => p,
            None => {
                return FruError::new_res_err(format!(
                    "field {} does not exist in struct {}",
                    path[0],
                    self.get_type().ident
                ));
            }
        };

        if path.len() >= 2 {
            return self
                .get_kth_field(pos)
                .set_field(&path[1..path.len()], value);
        }

        self.set_kth_field(pos, value);

        self.get_type().watches_by_field.get(&path[0]);

        return Ok(());
    }
}

impl Debug for FruField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_public {
            write!(f, "pub ")?;
        }
        write!(f, "{}", self.ident)
    }
}

impl Debug for FruType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

impl Debug for FruObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}{{", self.get_type())?;

        let total_fields = self.get_type().fields.len();

        for (i, field) in self.get_type().fields.iter().enumerate() {
            write!(f, "{:?}: {:?}", field, self.get().fields[i])?;

            if i < total_fields - 1 {
                write!(f, ", ")?;
            }
        }

        write!(f, "}}")
    }
}
