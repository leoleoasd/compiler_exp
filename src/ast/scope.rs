use crate::ast::types::Type;
use crate::parser::errors::ParserError;
use std::cell::RefCell;
use std::{collections::HashMap, ops::Range, sync::Arc};

use super::expr::ExprNode;

#[derive(Debug)]
pub enum Entity {
    Variable {
        name: String,
        location: Range<usize>,
        init_expr: Option<Box<dyn ExprNode>>,
        _type: Arc<Type>,
    },
    Function {
        name: String,
        location: Range<usize>,
        _type: Arc<Type>,
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
    pub fn get_type(&self) -> Arc<Type> {
        match self {
            Entity::Variable { _type, .. } => _type.clone(),
            Entity::Function { _type, .. } => _type.clone(),
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
    root: Arc<RefCell<SubScope>>,
    stack: Vec<Arc<RefCell<SubScope>>>,
    all_scopes: Vec<Arc<RefCell<SubScope>>>,
    // make 'scope invariant
    // pd: PhantomData<&'scope mut & 'scope ()>,
}
impl Scope {
    pub fn new() -> Scope {
        let s = Arc::new(RefCell::new(SubScope::new()));
        Scope {
            root: s.clone(),
            stack: vec![s.clone()],
            all_scopes: vec![s],
        }
    }
    pub fn push(&mut self) -> Arc<RefCell<SubScope>> {
        let s = Arc::new(RefCell::new(SubScope::new()));
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
    pub fn get(&mut self, name: &str) -> Option<Arc<Entity>> {
        for s in self.stack.iter().rev() {
            if s.borrow().get(name).is_some() {
                return Some(s.borrow().get(name).unwrap());
            }
        }
        None
    }
    pub fn define_variable(
        &mut self,
        name: &str,
        location: Range<usize>,
        _type: Arc<Type>,
        expr: Option<Box<dyn ExprNode>>,
    ) -> Result<Arc<Entity>, ParserError> {
        self.stack
            .last()
            .unwrap()
            .borrow_mut()
            .define_variable(name, location, _type, expr)
    }
    pub fn define_function(
        &mut self,
        name: &str,
        location: Range<usize>,
        _type: Arc<Type>,
        _extern: bool,
    ) -> Result<Arc<Entity>, ParserError> {
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
    entities: HashMap<String, Arc<Entity>>,
}
impl SubScope {
    fn new() -> SubScope {
        SubScope {
            children: Vec::new(),
            entities: HashMap::new(),
        }
    }
    fn get(&self, name: &str) -> Option<Arc<Entity>> {
        self.entities.get(name).map(|s| s.to_owned())
    }
    fn define_function(
        &mut self,
        name: &str,
        location: Range<usize>,
        _type: Arc<Type>,
        _extern: bool,
    ) -> Result<Arc<Entity>, ParserError> {
        if let Some(v) = self.entities.get(name) {
            return Err(ParserError::EntityNameConflict(
                name.to_string(),
                v.get_location().clone(),
            ));
        }
        let e = Arc::new(Entity::Function {
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
        _type: Arc<Type>,
        expr: Option<Box<dyn ExprNode>>,
    ) -> Result<Arc<Entity>, ParserError> {
        if let Some(v) = self.entities.get(name) {
            return Err(ParserError::EntityNameConflict(
                name.to_string(),
                v.get_location().clone(),
            ));
        }
        self.entities.insert(
            name.to_string(),
            Arc::new(Entity::Variable {
                name: name.to_string(),
                location,
                _type,
                init_expr: expr,
            }),
        );
        Ok(self.entities.get(name).unwrap().clone())
    }
}
