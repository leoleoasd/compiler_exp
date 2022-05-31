use crate::ast::types::Type;
use crate::parser::errors::ParserError;
use std::cell::RefCell;
use std::{collections::HashMap, ops::Range, rc::Rc};

#[derive(Debug)]
pub enum Entity {
    Variable {
        name: String,
        location: Range<usize>,
        _type: Type,
    },
    Function {
        name: String,
        location: Range<usize>,
        _type: Type,
        _extern: bool,
    },
}

impl Entity {
    pub fn get_name(&self) -> &str {
        match self {
            Entity::Variable { name, .. } => name,
            Entity::Function { name, .. } => name,
        }
    }
    pub fn get_type(&self) -> &Type {
        match self {
            Entity::Variable { _type, .. } => _type,
            Entity::Function { _type, .. } => _type,
        }
    }
    pub fn get_location(&self) -> &Range<usize> {
        match self {
            Entity::Variable { location, .. } => location,
            Entity::Function { location, .. } => location,
        }
    }
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
    pub fn define_function(
        &mut self,
        name: &str,
        location: Range<usize>,
        _type: Type,
        _extern: bool,
    ) -> Result<Rc<Entity>, ParserError> {
        if self.stack.len() != 1 {
            return Err(ParserError::InvalidFunctionDefination(name.to_string()));
        }
        self.stack
            .last()
            .unwrap()
            .borrow_mut()
            .define_function(name, location, _type, _extern)
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
    fn define_function(
        &mut self,
        name: &str,
        location: Range<usize>,
        _type: Type,
        _extern: bool,
    ) -> Result<Rc<Entity>, ParserError> {
        if let Some(v) = self.entities.get(name) {
            return Err(ParserError::EntityNameConflict(
                name.to_string(),
                v.get_location().clone(),
            ));
        }
        let e = Rc::new(Entity::Function {
            name: name.to_owned(),
            location,
            _type,
            _extern,
        });
        self.entities.insert(name.to_owned(), e.clone());
        Ok(e)
    }
    fn define_variable(
        &mut self,
        name: &str,
        location: Range<usize>,
        _type: Type,
    ) -> Result<Rc<Entity>, ParserError> {
        if let Some(v) = self.entities.get(name) {
            return Err(ParserError::EntityNameConflict(
                name.to_string(),
                v.get_location().clone(),
            ));
        }
        self.entities.insert(
            name.to_string(),
            Rc::new(Entity::Variable {
                name: name.to_string(),
                location,
                _type,
            }),
        );
        Ok(self.entities.get(name).unwrap().clone())
    }
}
