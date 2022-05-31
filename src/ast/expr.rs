use crate::parser::errors::ParserError;

use super::{node::Node, scope::Entity, types::Type};
use lazy_static::lazy_static;
use std::fmt::Debug;
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
        &self.location
    }
}
impl ExprNode for IntegerLiteralNode {
    fn get_type(&self) -> &Type {
        &INT_LITERAL_TYPE
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

#[derive(Debug, Clone)]
pub enum PostOp {
    Inc,
    Dec,
    Index(Box<dyn ExprNode>),
    MemberOf(String),
    MemberOfPointer(String),
    FuncCall(Vec<Box<dyn ExprNode>>),
}

#[derive(Debug, Clone)]
pub struct PostfixExprNode {
    expr: Box<dyn ExprNode>,
    op: PostOp,
    location: Range<usize>,
}
impl Node for PostfixExprNode {
    fn get_location(&self) -> &Range<usize> {
        &self.location
    }
}
impl ExprNode for PostfixExprNode {
    fn get_type(&self) -> &Type {
        match &self.op {
            PostOp::Inc | PostOp::Dec => self.expr.get_type(),
            PostOp::Index(_) => self.expr.get_type().element_type(),
            PostOp::MemberOf(field) => self.expr.get_type().field_type(field).unwrap(),
            PostOp::MemberOfPointer(field) => self
                .expr
                .get_type()
                .element_type()
                .field_type(field)
                .unwrap(),
            PostOp::FuncCall(_) => self.expr.get_type().element_type(),
        }
    }
    fn is_addressable(&self) -> bool {
        match self.op {
            PostOp::Inc | PostOp::Dec => false, // rvalue
            PostOp::Index(_) => true,           // inner expr shoudl be addressable
            PostOp::MemberOf(_) | PostOp::MemberOfPointer(_) => true, // inner expr shoudl be addressable
            PostOp::FuncCall(_) => false,                             // rvalue
        }
    }
    fn is_constant(&self) -> bool {
        false
    }
    fn get_const_value(&self) -> Option<ConstValue> {
        None
    }
}
impl PostfixExprNode {
    pub fn new(expr: Box<dyn ExprNode>, op: PostOp, location: Range<usize>) -> Self {
        PostfixExprNode { expr, op, location }
    }
    pub fn new_inc(expr: Box<dyn ExprNode>, location: Range<usize>) -> Result<Self, ParserError> {
        if matches!(expr.get_type(), Type::Integer { .. }) {
            if expr.is_addressable() {
                Ok(PostfixExprNode {
                    expr,
                    op: PostOp::Inc,
                    location,
                })
            } else {
                Err(ParserError::AddressableOprandRequired)
            }
        } else {
            Err(ParserError::TypeMismatch(
                "Integer".to_string(),
                expr.get_type().name(),
            ))
        }
    }
    pub fn new_dec(expr: Box<dyn ExprNode>, location: Range<usize>) -> Result<Self, ParserError> {
        if matches!(expr.get_type(), Type::Integer { .. }) {
            if expr.is_addressable() {
                Ok(PostfixExprNode {
                    expr,
                    op: PostOp::Dec,
                    location,
                })
            } else {
                Err(ParserError::AddressableOprandRequired)
            }
        } else {
            Err(ParserError::TypeMismatch(
                "Integer".to_string(),
                expr.get_type().name(),
            ))
        }
    }
    pub fn new_index(
        expr: Box<dyn ExprNode>,
        index: Box<dyn ExprNode>,
        location: Range<usize>,
    ) -> Result<Self, ParserError> {
        if matches!(expr.get_type(), Type::Array { .. }) {
            if expr.is_addressable() {
                Ok(PostfixExprNode {
                    expr,
                    op: PostOp::Index(index),
                    location,
                })
            } else {
                Err(ParserError::AddressableOprandRequired)
            }
        } else {
            Err(ParserError::TypeMismatch(
                "Array".to_string(),
                expr.get_type().name(),
            ))
        }
    }
    pub fn new_member_of(
        expr: Box<dyn ExprNode>,
        member: String,
        location: Range<usize>,
    ) -> Result<Self, ParserError> {
        if let Type::Struct {
            location: s_location,
            name,
            ..
        } = expr.get_type()
        {
            if !expr.is_addressable() {
                return Err(ParserError::AddressableOprandRequired);
            }
            if expr.get_type().field_type(&member).is_none() {
                return Err(ParserError::FieldNotFound(
                    member,
                    name.to_string(),
                    s_location.clone(),
                ));
            }
            Ok(PostfixExprNode {
                expr,
                op: PostOp::MemberOf(member),
                location,
            })
        } else {
            Err(ParserError::TypeMismatch(
                "Struct".to_string(),
                expr.get_type().name(),
            ))
        }
    }
    pub fn new_member_of_pointer(
        expr: Box<dyn ExprNode>,
        member: String,
        location: Range<usize>,
    ) -> Result<Self, ParserError> {
        if let Type::Pointer {
            element_type: t, ..
        } = expr.get_type()
        {
            if let Type::Struct {
                location: s_location,
                name,
                ..
            } = &**t
            {
                if !expr.is_addressable() {
                    return Err(ParserError::AddressableOprandRequired);
                }
                if t.field_type(&member).is_none() {
                    return Err(ParserError::FieldNotFound(
                        member,
                        name.to_string(),
                        s_location.clone(),
                    ));
                }
                Ok(PostfixExprNode {
                    expr,
                    op: PostOp::MemberOf(member),
                    location,
                })
            } else {
                Err(ParserError::TypeMismatch(
                    "Pointer of Struct".to_string(),
                    t.name(),
                ))
            }
        } else {
            Err(ParserError::TypeMismatch(
                "Pointer of Struct".to_string(),
                expr.get_type().name(),
            ))
        }
    }
    pub fn new_func_call(
        expr: Box<dyn ExprNode>,
        args: Vec<Box<dyn ExprNode>>,
        location: Range<usize>,
    ) -> Result<Self, ParserError> {
        if let Type::Function {
            return_type,
            parameters,
            variadic,
        } = expr.get_type()
        {
            if parameters.len() != args.len() && !variadic {
                return Err(ParserError::ArgumentCountMismatch(
                    parameters.len(),
                    args.len(),
                ));
            }
            for (index, (exp, act)) in parameters.iter().zip(args.iter()).enumerate() {
                if exp != act.get_type() {
                    return Err(ParserError::ArgumentTypeMismatch(
                        index,
                        exp.name(),
                        act.get_type().name(),
                    ));
                }
            }
            Ok(PostfixExprNode {
                expr,
                op: PostOp::FuncCall(args),
                location,
            })
        } else {
            Err(ParserError::TypeMismatch(
                "Func".to_string(),
                expr.get_type().name(),
            ))
        }
    }
}
