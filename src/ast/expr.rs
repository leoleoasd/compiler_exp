use super::{node::Node, scope::Entity, types::Type};
use lazy_static::lazy_static;
use std::ops::Range;
use std::rc::Rc;

pub enum ConstValue {
    Int(i32),
    Char(i8),
    String(String),
}

pub trait ExprNode: Node {
    fn get_type(&self) -> &Type;
    fn is_addressable(&self) -> bool;
    fn is_constant(&self) -> bool;
    fn get_const_value(&self) -> Option<ConstValue>;
}
dyn_clone::clone_trait_object!(ExprNode);

#[derive(Debug, Clone)]
struct BaiscExprNode {
    _type: Type,
    addressable: bool,
    constant: bool,
    location: Range<usize>,
}
lazy_static! {
    static ref INT_LITERAL_TYPE: Type = Type::Integer {
        signed: true,
        size: 32,
    };
    static ref CHAR_LITERAL_TYPE: Type = Type::Integer {
        signed: true,
        size: 8,
    };
    static ref STRING_LITERAL_TYPE: Type = Type::Pointer {
        element_type: Box::new(Type::Integer {
            signed: true,
            size: 8,
        })
    };
}

#[derive(Debug, Clone)]
pub struct IntegerLiteralNode {
    value: i32,
    location: Range<usize>,
}
impl Node for IntegerLiteralNode {
    fn get_location(&self) -> &Range<usize> {
        return &self.location;
    }
}
impl ExprNode for IntegerLiteralNode {
    fn get_type(&self) -> &Type {
        return &INT_LITERAL_TYPE;
    }
    fn is_addressable(&self) -> bool {
        false
    }
    fn is_constant(&self) -> bool {
        true
    }
    fn get_const_value(&self) -> Option<ConstValue> {
        Some(ConstValue::Int(self.value))
    }
}
impl IntegerLiteralNode {
    pub fn new(val: i32, location: Range<usize>) -> Self {
        IntegerLiteralNode {
            value: val,
            location,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CharLiteralNode {
    value: i8,
    location: Range<usize>,
}
impl Node for CharLiteralNode {
    fn get_location(&self) -> &Range<usize> {
        &self.location
    }
}
impl ExprNode for CharLiteralNode {
    fn get_type(&self) -> &Type {
        &CHAR_LITERAL_TYPE
    }
    fn is_addressable(&self) -> bool {
        false
    }
    fn is_constant(&self) -> bool {
        true
    }
    fn get_const_value(&self) -> Option<ConstValue> {
        Some(ConstValue::Char(self.value))
    }
}
impl CharLiteralNode {
    pub fn new(val: i8, location: Range<usize>) -> Self {
        CharLiteralNode {
            value: val,
            location,
        }
    }
}

#[derive(Debug, Clone)]
pub struct StringLiteralNode {
    value: String,
    location: Range<usize>,
}
impl Node for StringLiteralNode {
    fn get_location(&self) -> &Range<usize> {
        &self.location
    }
}
impl ExprNode for StringLiteralNode {
    fn get_type(&self) -> &Type {
        &STRING_LITERAL_TYPE
    }
    fn is_addressable(&self) -> bool {
        false
    }
    fn is_constant(&self) -> bool {
        true
    }
    fn get_const_value(&self) -> Option<ConstValue> {
        Some(ConstValue::String(self.value.clone()))
    }
}
impl StringLiteralNode {
    pub fn new(val: String, location: Range<usize>) -> Self {
        StringLiteralNode {
            value: val,
            location,
        }
    }
}
#[derive(Debug, Clone)]
pub struct EntityNode {
    entity: Rc<Entity>,
    location: Range<usize>,
}
impl Node for EntityNode {
    fn get_location(&self) -> &Range<usize> {
        &self.location
    }
}
impl ExprNode for EntityNode {
    fn get_type(&self) -> &Type {
        self.entity.get_type()
    }
    fn is_addressable(&self) -> bool {
        true
    }
    fn is_constant(&self) -> bool {
        false
    }
    fn get_const_value(&self) -> Option<ConstValue> {
        None
    }
}
impl EntityNode {
    pub fn new(entity: Rc<Entity>, location: Range<usize>) -> Self {
        EntityNode { entity, location }
    }
}
