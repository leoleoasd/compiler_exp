use crate::ast::types::Type;
use std::cell::{Ref, RefCell};
use std::error::Error;
use std::fmt::{self, Debug, Display, Formatter};
use std::{
    collections::HashMap,
    marker::PhantomData,
    ops::{Range, Sub},
    rc::Rc,
};

pub struct VariableRedifinationError {
    pub name: String,
    pub previous_index: isize,
}
// TODO: implement error diagnostics about previously defined
impl Error for VariableRedifinationError {}
impl Debug for VariableRedifinationError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "VariableRedifinationError: Variable {} is defined twice",
            self.name
        )
    }
}
impl Display for VariableRedifinationError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        <Self as Debug>::fmt(self, f)
    }
}

#[derive(Debug)]
pub struct Entity {
    pub name: String,
    pub location: Range<isize>,
    pub index: isize,
    pub _type: Type,
}

#[derive(Debug)]
pub struct Scope {
    root: Rc<RefCell<SubScope>>,
    stack: Vec<Rc<RefCell<SubScope>>>,
    all_scopes: Vec<Rc<RefCell<SubScope>>>,
    // make 'scope invariant
    // pd: PhantomData<&'scope mut & 'scope ()>,
}
impl Scope {
    pub fn new() -> Scope {
        let s = Rc::new(RefCell::new(SubScope::new()));
        Scope {
            root: s.clone(),
            stack: vec![s.clone()],
            all_scopes: vec![s],
        }
    }
    pub fn push(&mut self) -> Rc<RefCell<SubScope>> {
        let s = Rc::new(RefCell::new(SubScope::new()));
        self.stack.push(s.clone());
        self.all_scopes.push(s.clone());
        s
    }
    pub fn pop(&mut self) {
        self.stack.pop();
        if self.stack.is_empty() {
            panic!("Top scope is being poped!");
        }
    }
    pub fn get(&mut self, name: &str) -> Option<Rc<Entity>> {
        for s in self.stack.iter().rev() {
            if let Some(_) = s.borrow().get(name) {
                return Some(s.borrow().get(name).unwrap().clone());
            }
        }
        None
    }
    pub fn defineVariable(
        &mut self,
        name: &str,
        location: Range<isize>,
        index: isize,
        _type: Type,
    ) -> Result<Rc<Entity>, VariableRedifinationError> {
        self.stack
            .last()
            .unwrap()
            .borrow_mut()
            .defineVariable(name, location, index, _type)
    }
}
#[derive(Debug)]
pub struct SubScope {
    // parent: &'scope Scope<'scope>,
    children: Vec<SubScope>,
    entities: HashMap<String, Rc<Entity>>,
}
impl SubScope {
    fn new() -> SubScope {
        SubScope {
            children: Vec::new(),
            entities: HashMap::new(),
        }
    }
    fn get(&self, name: &str) -> Option<Rc<Entity>> {
        self.entities.get(name).map(|s| s.to_owned())
    }
    fn defineVariable(
        &mut self,
        name: &str,
        location: Range<isize>,
        index: isize,
        _type: Type,
    ) -> Result<Rc<Entity>, VariableRedifinationError> {
        if let Some(v) = self.entities.get(name) {
            return Err(VariableRedifinationError {
                name: name.to_string(),
                previous_index: v.index,
            });
        }
        self.entities.insert(
            name.to_string(),
            Rc::new(Entity {
                name: name.to_string(),
                location,
                index,
                _type,
            }),
        );
        Ok(self.entities.get(name).unwrap().clone())
    }
}
