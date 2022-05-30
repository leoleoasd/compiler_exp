use crate::ast::types::Type;
use crate::parser::errors::ParserError;
use std::cell::RefCell;
use std::{collections::HashMap, ops::Range, rc::Rc};

#[derive(Debug)]
pub struct Entity {
    pub name: String,
    pub location: Range<usize>,
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
    pub fn define_variable(
        &mut self,
        name: &str,
        location: Range<usize>,
        _type: Type,
    ) -> Result<Rc<Entity>, ParserError> {
        self.stack
            .last()
            .unwrap()
            .borrow_mut()
            .define_variable(name, location, _type)
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
    fn define_variable(
        &mut self,
        name: &str,
        location: Range<usize>,
        _type: Type,
    ) -> Result<Rc<Entity>, ParserError> {
        if let Some(v) = self.entities.get(name) {
            return Err(ParserError::VariableRedefination(
                name.to_string(),
                v.location.clone(),
            ));
        }
        self.entities.insert(
            name.to_string(),
            Rc::new(Entity {
                name: name.to_string(),
                location,
                _type,
            }),
        );
        Ok(self.entities.get(name).unwrap().clone())
    }
}
