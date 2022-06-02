use crate::parser::errors::ParserError;

use super::{node::Node, scope::Entity, types::Type};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::values::{BasicValue, BasicValueEnum, PointerValue};
use lazy_static::lazy_static;
use std::cell::RefCell;
use std::fmt::Debug;
use std::ops::Range;
use std::sync::Arc;

pub enum ConstValue {
    Int(i32),
    Char(i8),
    String(String),
}

pub trait ExprNode: Node {
    fn get_type(&self) -> Arc<Type>;
    fn is_addressable(&self) -> bool;
    fn is_constant(&self) -> bool;
    fn get_const_value(&self) -> Option<ConstValue>;
    fn value(
        &self,
        context: &'static Context,
        builder: &Builder<'static>,
    ) -> BasicValueEnum<'static> {
        todo!()
    }
    fn addr(&self, context: &'static Context, builder: &Builder<'static>) -> PointerValue<'static> {
        todo!()
    }
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
    static ref INT_LITERAL_TYPE: Arc<Type> = Arc::new(Type::Integer {
        signed: true,
        size: 32,
    });
    static ref CHAR_LITERAL_TYPE: Arc<Type> = Arc::new(Type::Integer {
        signed: true,
        size: 8,
    });
    static ref STRING_LITERAL_TYPE: Arc<Type> = Arc::new(Type::Pointer {
        element_type: Arc::new(Type::Integer {
            signed: true,
            size: 8,
        })
    });
    static ref BOOLEAN_TYPE: Arc<Type> = Arc::new(Type::Integer {
        signed: true,
        size: 8,
    });
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
    fn get_type(&self) -> Arc<Type> {
        INT_LITERAL_TYPE.clone()
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
    fn value(
        &self,
        context: &'static Context,
        builder: &Builder<'static>,
    ) -> BasicValueEnum<'static> {
        self.get_type()
            .to_llvm_type(context)
            .into_int_type()
            .const_int(self.value as u64, false)
            .as_basic_value_enum()
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
    fn get_type(&self) -> Arc<Type> {
        CHAR_LITERAL_TYPE.clone()
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
    fn value(
        &self,
        context: &'static Context,
        builder: &Builder<'static>,
    ) -> BasicValueEnum<'static> {
        self.get_type()
            .to_llvm_type(context)
            .into_int_type()
            .const_int(self.value as u64, false)
            .as_basic_value_enum()
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
    fn get_type(&self) -> Arc<Type> {
        STRING_LITERAL_TYPE.clone()
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
    entity: Arc<RefCell<Entity>>,
    location: Range<usize>,
}
impl Node for EntityNode {
    fn get_location(&self) -> &Range<usize> {
        &self.location
    }
}
impl ExprNode for EntityNode {
    fn get_type(&self) -> Arc<Type> {
        self.entity.borrow().get_type()
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
    fn value(
        &self,
        context: &'static Context,
        builder: &Builder<'static>,
    ) -> BasicValueEnum<'static> {
        let addr = self.addr(context, builder);
        builder.build_load(addr, "load variable")
    }
    fn addr(&self, context: &'static Context, builder: &Builder<'static>) -> PointerValue<'static> {
        // must be variable
        if let Entity::Variable { llvm, .. } = &*self.entity.borrow() {
            llvm.unwrap()
        } else {
            panic!("addr on function");
        }
    }
}
impl EntityNode {
    pub fn new(entity: Arc<RefCell<Entity>>, location: Range<usize>) -> Self {
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
    fn get_type(&self) -> Arc<Type> {
        match &self.op {
            PostOp::Inc | PostOp::Dec => self.expr.get_type(),
            PostOp::Index(_) => Type::element_type(self.expr.get_type()),
            PostOp::MemberOf(field) => Type::field_type(self.expr.get_type(), field).unwrap(),
            PostOp::MemberOfPointer(field) => {
                Type::field_type(Type::element_type(self.expr.get_type()), field).unwrap()
            }
            PostOp::FuncCall(_) => Type::element_type(self.expr.get_type()),
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
        if matches!(*expr.get_type(), Type::Integer { .. }) {
            if expr.is_addressable() {
                Ok(PostfixExprNode {
                    expr,
                    op: PostOp::Inc,
                    location,
                })
            } else {
                Err(ParserError::AddressableOprandRequired(
                    expr.get_location().clone(),
                ))
            }
        } else {
            Err(ParserError::TypeMismatch(
                "Integer".to_string(),
                expr.get_type().name(),
                expr.get_location().clone(),
            ))
        }
    }
    pub fn new_dec(expr: Box<dyn ExprNode>, location: Range<usize>) -> Result<Self, ParserError> {
        if matches!(*expr.get_type(), Type::Integer { .. }) {
            if expr.is_addressable() {
                Ok(PostfixExprNode {
                    expr,
                    op: PostOp::Dec,
                    location,
                })
            } else {
                Err(ParserError::AddressableOprandRequired(
                    expr.get_location().clone(),
                ))
            }
        } else {
            Err(ParserError::TypeMismatch(
                "Integer".to_string(),
                expr.get_type().name(),
                expr.get_location().clone(),
            ))
        }
    }
    pub fn new_index(
        expr: Box<dyn ExprNode>,
        index: Box<dyn ExprNode>,
        location: Range<usize>,
    ) -> Result<Self, ParserError> {
        if matches!(*expr.get_type(), Type::Array { .. }) {
            if expr.is_addressable() {
                Ok(PostfixExprNode {
                    expr,
                    op: PostOp::Index(index),
                    location,
                })
            } else {
                Err(ParserError::AddressableOprandRequired(
                    expr.get_location().clone(),
                ))
            }
        } else {
            println!("{expr:?}");
            // TODO: Allow indexing of pointers
            Err(ParserError::TypeMismatch(
                "Array".to_string(),
                expr.get_type().name(),
                expr.get_location().clone(),
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
        } = &*expr.get_type()
        {
            if !expr.is_addressable() {
                return Err(ParserError::AddressableOprandRequired(
                    expr.get_location().clone(),
                ));
            }
            if Type::field_type(expr.get_type(), &member).is_none() {
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
                expr.get_location().clone(),
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
        } = &*expr.get_type()
        {
            if let Type::Struct {
                location: s_location,
                name,
                ..
            } = &**t
            {
                if !expr.is_addressable() {
                    return Err(ParserError::AddressableOprandRequired(
                        expr.get_location().clone(),
                    ));
                }
                if Type::field_type(t.clone(), &member).is_none() {
                    return Err(ParserError::FieldNotFound(
                        member,
                        name.to_string(),
                        s_location.clone(),
                    ));
                }
                Ok(PostfixExprNode {
                    expr,
                    op: PostOp::MemberOfPointer(member),
                    location,
                })
            } else {
                Err(ParserError::TypeMismatch(
                    "Pointer of Struct".to_string(),
                    t.name(),
                    expr.get_location().clone(),
                ))
            }
        } else {
            Err(ParserError::TypeMismatch(
                "Pointer of Struct".to_string(),
                expr.get_type().name(),
                expr.get_location().clone(),
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
        } = &*expr.get_type()
        {
            if parameters.len() != args.len() && !variadic {
                return Err(ParserError::ArgumentCountMismatch(
                    parameters.len(),
                    args.len(),
                ));
            }
            for (index, (exp, act)) in parameters.iter().zip(args.iter()).enumerate() {
                if *exp != act.get_type() {
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
                expr.get_location().clone(),
            ))
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Inc,
    Dec,
    Add,
    Sub,
    Not,
    LogicalNot,
    Neg,
    Deref,
    Addr,
}
#[derive(Debug, Clone)]
pub struct UnaryExprNode {
    pub expr: Box<dyn ExprNode>,
    pub op: UnaryOp,
    pub location: Range<usize>,
    _type: Option<Arc<Type>>, // for Addr
}
impl Node for UnaryExprNode {
    fn get_location(&self) -> &Range<usize> {
        &self.location
    }
}
impl ExprNode for UnaryExprNode {
    fn get_type(&self) -> Arc<Type> {
        match self.op {
            UnaryOp::Addr => self._type.clone().unwrap(),
            UnaryOp::Deref => Type::element_type(self.expr.get_type()),
            UnaryOp::Inc => self.expr.get_type(),
            UnaryOp::Dec => self.expr.get_type(),
            UnaryOp::Add => self.expr.get_type(),
            UnaryOp::Sub => self.expr.get_type(),
            UnaryOp::LogicalNot => BOOLEAN_TYPE.clone(),
            UnaryOp::Not => self.expr.get_type(),
            UnaryOp::Neg => self.expr.get_type(),
        }
    }
    fn is_addressable(&self) -> bool {
        // Only Deref is addresasable
        matches!(self.op, UnaryOp::Deref)
    }

    fn is_constant(&self) -> bool {
        false
    }

    fn get_const_value(&self) -> Option<ConstValue> {
        None
    }
}
impl UnaryExprNode {
    pub fn new_inc(expr: Box<dyn ExprNode>, location: Range<usize>) -> Result<Self, ParserError> {
        if let Type::Integer { .. } = &*expr.get_type() {
            if !expr.is_addressable() {
                return Err(ParserError::AddressableOprandRequired(
                    expr.get_location().clone(),
                ));
            }
            Ok(UnaryExprNode {
                expr,
                op: UnaryOp::Inc,
                location,
                _type: None,
            })
        } else {
            Err(ParserError::TypeMismatch(
                "Integer".to_string(),
                expr.get_type().name(),
                expr.get_location().clone(),
            ))
        }
    }
    pub fn new_dec(expr: Box<dyn ExprNode>, location: Range<usize>) -> Result<Self, ParserError> {
        if let Type::Integer { .. } = &*expr.get_type() {
            if !expr.is_addressable() {
                return Err(ParserError::AddressableOprandRequired(
                    expr.get_location().clone(),
                ));
            }
            Ok(UnaryExprNode {
                expr,
                op: UnaryOp::Dec,
                location,
                _type: None,
            })
        } else {
            Err(ParserError::TypeMismatch(
                "Integer".to_string(),
                expr.get_type().name(),
                expr.get_location().clone(),
            ))
        }
    }
    pub fn new_add(expr: Box<dyn ExprNode>, location: Range<usize>) -> Result<Self, ParserError> {
        if let Type::Integer { .. } = &*expr.get_type() {
            Ok(UnaryExprNode {
                expr,
                op: UnaryOp::Add,
                location,
                _type: None,
            })
        } else {
            Err(ParserError::TypeMismatch(
                "Integer".to_string(),
                expr.get_type().name(),
                expr.get_location().clone(),
            ))
        }
    }
    pub fn new_neg(expr: Box<dyn ExprNode>, location: Range<usize>) -> Result<Self, ParserError> {
        if let Type::Integer { .. } = &*expr.get_type() {
            Ok(UnaryExprNode {
                expr,
                op: UnaryOp::Neg,
                location,
                _type: None,
            })
        } else {
            Err(ParserError::TypeMismatch(
                "Integer".to_string(),
                expr.get_type().name(),
                expr.get_location().clone(),
            ))
        }
    }
    pub fn new_logical_not(
        expr: Box<dyn ExprNode>,
        location: Range<usize>,
    ) -> Result<Self, ParserError> {
        if let Type::Integer { .. } = &*expr.get_type() {
            Ok(UnaryExprNode {
                expr,
                op: UnaryOp::LogicalNot,
                location,
                _type: None,
            })
        } else {
            Err(ParserError::TypeMismatch(
                "Integer".to_string(),
                expr.get_type().name(),
                expr.get_location().clone(),
            ))
        }
    }
    pub fn new_not(expr: Box<dyn ExprNode>, location: Range<usize>) -> Result<Self, ParserError> {
        if let Type::Integer { .. } = &*expr.get_type() {
            Ok(UnaryExprNode {
                expr,
                op: UnaryOp::Not,
                location,
                _type: None,
            })
        } else {
            Err(ParserError::TypeMismatch(
                "Integer".to_string(),
                expr.get_type().name(),
                expr.get_location().clone(),
            ))
        }
    }
    pub fn new_deref(expr: Box<dyn ExprNode>, location: Range<usize>) -> Result<Self, ParserError> {
        if let Type::Pointer { .. } = &*expr.get_type() {
            Ok(UnaryExprNode {
                expr,
                op: UnaryOp::Deref,
                location,
                _type: None,
            })
        } else {
            Err(ParserError::TypeMismatch(
                "Pointer".to_string(),
                expr.get_type().name(),
                expr.get_location().clone(),
            ))
        }
    }
    pub fn new_addr(expr: Box<dyn ExprNode>, location: Range<usize>) -> Result<Self, ParserError> {
        // have no type constraint at all
        if !expr.is_addressable() {
            return Err(ParserError::AddressableOprandRequired(
                expr.get_location().clone(),
            ));
        }
        let _type = Type::pointer_type(expr.get_type());
        Ok(UnaryExprNode {
            expr,
            op: UnaryOp::Addr,
            location,
            _type: Some(_type),
        })
    }
}

#[derive(Debug, Clone)]
pub struct CastExprNode {
    expr: Box<dyn ExprNode>,
    to_type: Arc<Type>,
    location: Range<usize>,
}
impl Node for CastExprNode {
    fn get_location(&self) -> &Range<usize> {
        &self.location
    }
}
impl ExprNode for CastExprNode {
    fn get_type(&self) -> Arc<Type> {
        self.to_type.clone()
    }
    fn is_addressable(&self) -> bool {
        false
    }
    fn is_constant(&self) -> bool {
        false
    }
    fn get_const_value(&self) -> Option<ConstValue> {
        None
    }
}
impl CastExprNode {
    pub fn new(
        expr: Box<dyn ExprNode>,
        to_type: Arc<Type>,
        location: Range<usize>,
    ) -> Result<Self, ParserError> {
        if expr.get_type().is_compatible(&to_type) {
            Ok(CastExprNode {
                expr,
                to_type,
                location,
            })
        } else {
            Err(ParserError::IncapableTypeCast(
                to_type.name(),
                expr.get_type().name(),
            ))
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Shl,
    Shr,
    And,
    Or,
    Xor,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    LogicalAnd,
    LogicalOr,
    Comma,
}
#[derive(Debug, Clone)]
pub struct BinaryExprNode {
    lhs: Box<dyn ExprNode>,
    rhs: Box<dyn ExprNode>,
    op: BinaryOp,
    location: Range<usize>,
}
impl Node for BinaryExprNode {
    fn get_location(&self) -> &Range<usize> {
        &self.location
    }
}
impl ExprNode for BinaryExprNode {
    fn get_type(&self) -> Arc<Type> {
        // return BOOLEAN_TYPE for logical operators
        match self.op {
            BinaryOp::LogicalAnd | BinaryOp::LogicalOr => BOOLEAN_TYPE.clone(),
            BinaryOp::Comma => self.rhs.get_type(),
            _ => Type::binary_cast(self.lhs.get_type(), self.rhs.get_type()).unwrap(),
        }
    }
    fn is_addressable(&self) -> bool {
        false
    }
    fn is_constant(&self) -> bool {
        // todo: Support constant
        false
    }
    fn get_const_value(&self) -> Option<ConstValue> {
        None
    }
}
impl BinaryExprNode {
    pub fn new_add(
        lhs: Box<dyn ExprNode>,
        rhs: Box<dyn ExprNode>,
        location: Range<usize>,
    ) -> Result<Self, ParserError> {
        // allow adding integer and integer with pointers
        if Type::binary_cast(lhs.get_type(), rhs.get_type()).is_some() {
            return Ok(BinaryExprNode {
                lhs,
                rhs,
                op: BinaryOp::Add,
                location,
            });
        }
        Err(ParserError::InvalidOperation(location))
    }
    pub fn new_sub(
        lhs: Box<dyn ExprNode>,
        rhs: Box<dyn ExprNode>,
        location: Range<usize>,
    ) -> Result<Self, ParserError> {
        // allow subtracting integer and integer with pointers
        if Type::binary_cast(lhs.get_type(), rhs.get_type()).is_some() {
            return Ok(BinaryExprNode {
                lhs,
                rhs,
                op: BinaryOp::Sub,
                location,
            });
        }
        Err(ParserError::InvalidOperation(location))
    }
    pub fn new_mul(
        lhs: Box<dyn ExprNode>,
        rhs: Box<dyn ExprNode>,
        location: Range<usize>,
    ) -> Result<Self, ParserError> {
        // only allow integer
        if lhs.get_type().is_integer() && rhs.get_type().is_integer() {
            return Ok(BinaryExprNode {
                lhs,
                rhs,
                op: BinaryOp::Mul,
                location,
            });
        }
        Err(ParserError::InvalidOperation(location))
    }
    pub fn new_div(
        lhs: Box<dyn ExprNode>,
        rhs: Box<dyn ExprNode>,
        location: Range<usize>,
    ) -> Result<Self, ParserError> {
        // only allow integer
        if lhs.get_type().is_integer() && rhs.get_type().is_integer() {
            return Ok(BinaryExprNode {
                lhs,
                rhs,
                op: BinaryOp::Div,
                location,
            });
        }
        Err(ParserError::InvalidOperation(location))
    }
    pub fn new_mod(
        lhs: Box<dyn ExprNode>,
        rhs: Box<dyn ExprNode>,
        location: Range<usize>,
    ) -> Result<Self, ParserError> {
        // only allow integer
        if lhs.get_type().is_integer() && rhs.get_type().is_integer() {
            return Ok(BinaryExprNode {
                lhs,
                rhs,
                op: BinaryOp::Mod,
                location,
            });
        }
        Err(ParserError::InvalidOperation(location))
    }
    pub fn new_shl(
        lhs: Box<dyn ExprNode>,
        rhs: Box<dyn ExprNode>,
        location: Range<usize>,
    ) -> Result<Self, ParserError> {
        // only allow integer
        if lhs.get_type().is_integer() && rhs.get_type().is_integer() {
            return Ok(BinaryExprNode {
                lhs,
                rhs,
                op: BinaryOp::Shl,
                location,
            });
        }
        Err(ParserError::InvalidOperation(location))
    }
    pub fn new_shr(
        lhs: Box<dyn ExprNode>,
        rhs: Box<dyn ExprNode>,
        location: Range<usize>,
    ) -> Result<Self, ParserError> {
        // only allow integer
        if lhs.get_type().is_integer() && rhs.get_type().is_integer() {
            return Ok(BinaryExprNode {
                lhs,
                rhs,
                op: BinaryOp::Shr,
                location,
            });
        }
        Err(ParserError::InvalidOperation(location))
    }
    pub fn new_and(
        lhs: Box<dyn ExprNode>,
        rhs: Box<dyn ExprNode>,
        location: Range<usize>,
    ) -> Result<Self, ParserError> {
        // only allow integer
        if lhs.get_type().is_integer() && rhs.get_type().is_integer() {
            return Ok(BinaryExprNode {
                lhs,
                rhs,
                op: BinaryOp::And,
                location,
            });
        }
        Err(ParserError::InvalidOperation(location))
    }
    pub fn new_or(
        lhs: Box<dyn ExprNode>,
        rhs: Box<dyn ExprNode>,
        location: Range<usize>,
    ) -> Result<Self, ParserError> {
        // only allow integer
        if lhs.get_type().is_integer() && rhs.get_type().is_integer() {
            return Ok(BinaryExprNode {
                lhs,
                rhs,
                op: BinaryOp::Or,
                location,
            });
        }
        Err(ParserError::InvalidOperation(location))
    }
    pub fn new_xor(
        lhs: Box<dyn ExprNode>,
        rhs: Box<dyn ExprNode>,
        location: Range<usize>,
    ) -> Result<Self, ParserError> {
        // only allow integer
        if lhs.get_type().is_integer() && rhs.get_type().is_integer() {
            return Ok(BinaryExprNode {
                lhs,
                rhs,
                op: BinaryOp::Xor,
                location,
            });
        }
        Err(ParserError::InvalidOperation(location))
    }
    pub fn new_eq(
        lhs: Box<dyn ExprNode>,
        rhs: Box<dyn ExprNode>,
        location: Range<usize>,
    ) -> Result<Self, ParserError> {
        if lhs.get_type() == rhs.get_type()
            || (lhs.get_type().is_integer() && rhs.get_type().is_integer())
        {
            return Ok(BinaryExprNode {
                lhs,
                rhs,
                op: BinaryOp::Eq,
                location,
            });
        }
        Err(ParserError::InvalidOperation(location))
    }
    pub fn new_ne(
        lhs: Box<dyn ExprNode>,
        rhs: Box<dyn ExprNode>,
        location: Range<usize>,
    ) -> Result<Self, ParserError> {
        if lhs.get_type() == rhs.get_type()
            || (lhs.get_type().is_integer() && rhs.get_type().is_integer())
        {
            return Ok(BinaryExprNode {
                lhs,
                rhs,
                op: BinaryOp::Ne,
                location,
            });
        }
        Err(ParserError::InvalidOperation(location))
    }
    pub fn new_lt(
        lhs: Box<dyn ExprNode>,
        rhs: Box<dyn ExprNode>,
        location: Range<usize>,
    ) -> Result<Self, ParserError> {
        if (lhs.get_type().is_integer() && rhs.get_type().is_integer())
            || (lhs.get_type().is_pointer() && rhs.get_type().is_pointer())
        {
            return Ok(BinaryExprNode {
                lhs,
                rhs,
                op: BinaryOp::Lt,
                location,
            });
        }
        Err(ParserError::InvalidOperation(location))
    }
    pub fn new_gt(
        lhs: Box<dyn ExprNode>,
        rhs: Box<dyn ExprNode>,
        location: Range<usize>,
    ) -> Result<Self, ParserError> {
        if (lhs.get_type().is_integer() && rhs.get_type().is_integer())
            || (lhs.get_type().is_pointer() && rhs.get_type().is_pointer())
        {
            return Ok(BinaryExprNode {
                lhs,
                rhs,
                op: BinaryOp::Gt,
                location,
            });
        }
        Err(ParserError::InvalidOperation(location))
    }
    pub fn new_le(
        lhs: Box<dyn ExprNode>,
        rhs: Box<dyn ExprNode>,
        location: Range<usize>,
    ) -> Result<Self, ParserError> {
        if (lhs.get_type().is_integer() && rhs.get_type().is_integer())
            || (lhs.get_type().is_pointer() && rhs.get_type().is_pointer())
        {
            return Ok(BinaryExprNode {
                lhs,
                rhs,
                op: BinaryOp::Le,
                location,
            });
        }
        Err(ParserError::InvalidOperation(location))
    }
    pub fn new_ge(
        lhs: Box<dyn ExprNode>,
        rhs: Box<dyn ExprNode>,
        location: Range<usize>,
    ) -> Result<Self, ParserError> {
        if (lhs.get_type().is_integer() && rhs.get_type().is_integer())
            || (lhs.get_type().is_pointer() && rhs.get_type().is_pointer())
        {
            return Ok(BinaryExprNode {
                lhs,
                rhs,
                op: BinaryOp::Ge,
                location,
            });
        }
        Err(ParserError::InvalidOperation(location))
    }
    pub fn new_logical_and(
        lhs: Box<dyn ExprNode>,
        rhs: Box<dyn ExprNode>,
        location: Range<usize>,
    ) -> Result<Self, ParserError> {
        if lhs.get_type().is_integer() && rhs.get_type().is_integer() {
            return Ok(BinaryExprNode {
                lhs,
                rhs,
                op: BinaryOp::LogicalAnd,
                location,
            });
        }
        Err(ParserError::InvalidOperation(location))
    }
    pub fn new_logical_or(
        lhs: Box<dyn ExprNode>,
        rhs: Box<dyn ExprNode>,
        location: Range<usize>,
    ) -> Result<Self, ParserError> {
        if lhs.get_type().is_integer() && rhs.get_type().is_integer() {
            return Ok(BinaryExprNode {
                lhs,
                rhs,
                op: BinaryOp::LogicalOr,
                location,
            });
        }
        Err(ParserError::InvalidOperation(location))
    }
    pub fn new_comma(
        lhs: Box<dyn ExprNode>,
        rhs: Box<dyn ExprNode>,
        location: Range<usize>,
    ) -> Result<Self, ParserError> {
        // no type constraint at all
        Ok(BinaryExprNode {
            lhs,
            rhs,
            op: BinaryOp::Comma,
            location,
        })
    }
}

#[derive(Debug, Clone)]
pub struct CondExprNode {
    pub cond: Box<dyn ExprNode>,
    pub then_expr: Box<dyn ExprNode>,
    pub else_expr: Box<dyn ExprNode>,
    pub location: Range<usize>,
}
impl Node for CondExprNode {
    fn get_location(&self) -> &std::ops::Range<usize> {
        &self.location
    }
}
impl ExprNode for CondExprNode {
    fn get_type(&self) -> Arc<Type> {
        self.then_expr.get_type()
    }

    fn is_addressable(&self) -> bool {
        // todo
        false
    }

    fn is_constant(&self) -> bool {
        false
    }

    fn get_const_value(&self) -> Option<ConstValue> {
        None
    }
}
impl CondExprNode {
    pub fn new(
        cond: Box<dyn ExprNode>,
        then_expr: Box<dyn ExprNode>,
        else_expr: Box<dyn ExprNode>,
        location: Range<usize>,
    ) -> Result<Self, ParserError> {
        if then_expr.get_type() != else_expr.get_type() {
            return Err(ParserError::TypeMismatch(
                then_expr.get_type().name(),
                else_expr.get_type().name(),
                else_expr.get_location().clone(),
            ));
        }
        Ok(CondExprNode {
            cond,
            then_expr,
            else_expr,
            location,
        })
    }
}
#[derive(Debug, Clone)]
pub enum AssignOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    ShlAssign,
    ShrAssign,
    AndAssign,
    OrAssign,
    XorAssign,
}
#[derive(Debug, Clone)]
pub struct AssignExprNode {
    pub lhs: Box<dyn ExprNode>,
    pub rhs: Box<dyn ExprNode>,
    pub op: AssignOp,
    pub location: Range<usize>,
}
impl Node for AssignExprNode {
    fn get_location(&self) -> &std::ops::Range<usize> {
        &self.location
    }
}
impl ExprNode for AssignExprNode {
    fn get_type(&self) -> Arc<Type> {
        self.lhs.get_type()
    }

    fn is_addressable(&self) -> bool {
        false
    }

    fn is_constant(&self) -> bool {
        false
    }

    fn get_const_value(&self) -> Option<ConstValue> {
        None
    }
}
impl AssignExprNode {
    pub fn new(
        lhs: Box<dyn ExprNode>,
        rhs: Box<dyn ExprNode>,
        op: AssignOp,
        location: Range<usize>,
    ) -> Result<Self, ParserError> {
        if lhs.get_type() != rhs.get_type() {
            return Err(ParserError::TypeMismatch(
                lhs.get_type().name(),
                rhs.get_type().name(),
                rhs.get_location().clone(),
            ));
        }
        match &op {
            AssignOp::AddAssign | AssignOp::SubAssign => {
                if Type::binary_cast(lhs.get_type(), rhs.get_type()).is_none() {
                    return Err(ParserError::InvalidOperation(location));
                }
            }
            AssignOp::MulAssign
            | AssignOp::DivAssign
            | AssignOp::ModAssign
            | AssignOp::ShlAssign
            | AssignOp::ShrAssign
            | AssignOp::AndAssign
            | AssignOp::OrAssign
            | AssignOp::XorAssign => {
                if !(lhs.get_type().is_integer() && rhs.get_type().is_integer()) {
                    return Err(ParserError::InvalidOperation(location));
                }
            }
            AssignOp::Assign => {}
        }
        if !lhs.is_addressable() {
            return Err(ParserError::AddressableOprandRequired(
                lhs.get_location().clone(),
            ));
        }
        Ok(AssignExprNode {
            lhs,
            rhs,
            op,
            location,
        })
    }
}
