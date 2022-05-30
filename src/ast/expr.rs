use std::ops::Range;

use super::{node::Node, types::Type};

pub enum ConstValue {
    Int(i64),
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

macro_rules! expr_impl {
    ($t: ty, $_self: ident, addressable = $is_addressable: block, is_constant = $is_constant: block,get_const_value =  $get_const_value: block) => {
        impl Node for $t {
            fn get_location(& $_self) -> &Range<usize> {
                return & $_self.basic.location;
            }
        }
        impl ExprNode for $t {
            fn get_type(& $_self) -> &Type {
                return & $_self.basic._type;
            }
            fn is_addressable(& $_self) -> bool $is_addressable
            fn is_constant(& $_self) -> bool $is_constant
            fn get_const_value(& $_self) -> Option<ConstValue> $get_const_value
        }
    };
    ($t: ty, $_self: ident) => {
        expr_impl!($t, $_self, addressable = { $_self.basic.addressable }, is_constant = { $_self.basic.constant }, get_const_value = {None});
    };
    ($t: ty, $_self: ident, get_const_value = $get_const_value: block ) => {
        expr_impl!($t, $_self, addressable = { $_self.basic.addressable },
            is_constant = { $_self.basic.constant }, get_const_value = $get_const_value);
    }
}
#[derive(Debug, Clone)]
pub struct IntegerLiteralNode {
    basic: BaiscExprNode,
    value: i64,
}
expr_impl!(
    IntegerLiteralNode,
    self,
    get_const_value = { Some(ConstValue::Int(self.value)) }
);
impl IntegerLiteralNode {
    pub fn new(val: i64, location: Range<usize>) -> Self {
        IntegerLiteralNode {
            value: val,
            basic: BaiscExprNode {
                _type: Type::Integer {
                    signed: true,
                    size: 64,
                },
                addressable: false,
                constant: true,
                location,
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct CharLiteralNode {
    basic: BaiscExprNode,
    value: i8,
}
expr_impl!(
    CharLiteralNode,
    self,
    get_const_value = { Some(ConstValue::Char(self.value)) }
);
impl CharLiteralNode {
    pub fn new(val: i8, location: Range<usize>) -> Self {
        CharLiteralNode {
            value: val,
            basic: BaiscExprNode {
                _type: Type::Integer {
                    signed: true,
                    size: 8,
                },
                addressable: false,
                constant: true,
                location,
            },
        }
    }
}
