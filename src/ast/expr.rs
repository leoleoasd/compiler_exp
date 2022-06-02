use crate::parser::errors::ParserError;

use super::scope::VariableEntity;
use super::{node::Node, scope::Entity, types::Type};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{BasicValue, BasicValueEnum, PointerValue};
use inkwell::{AddressSpace, IntPredicate};
use lazy_static::lazy_static;
use std::any::Any;
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
    fn value(
        &self,
        context: &'static Context,
        module: &'static Module,
        builder: &Builder<'static>,
    ) -> BasicValueEnum<'static> {
        todo!("value of {:?} isn't implemented", self)
    }
    fn addr(
        &self,
        context: &'static Context,
        module: &'static Module,
        builder: &Builder<'static>,
    ) -> PointerValue<'static> {
        todo!("addr of {:?} isn't implemented", self)
    }
    fn cast_value(
        &self,
        context: &'static Context,
        module: &'static Module,
        builder: &Builder<'static>,
        to_type: Arc<Type>,
    ) -> BasicValueEnum<'static> {
        if *self.get_type() == *to_type {
            return self.value(context, module, builder);
        }
        if self.get_type().is_array() && to_type.is_pointer() {
            if Type::element_type(self.get_type()) == Type::element_type(to_type) {
                let value = self.addr(context, module, builder);
                let zero = context.i32_type().const_int(0, false);
                let addr = unsafe { builder.build_gep(value, &[zero, zero], "") };
                return addr.as_basic_value_enum();
            } else {
                panic!("cannot cast array to pointer with another element");
            }
        }
        builder.build_cast(
            self.get_type().cast_op(&*to_type),
            self.value(context, module, builder),
            to_type.to_llvm_type(context),
            "cast",
        )
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
    fn value(
        &self,
        context: &'static Context,
        module: &'static Module,
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
    fn value(
        &self,
        context: &'static Context,
        module: &'static Module,
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
    fn value(
        &self,
        context: &'static Context,
        module: &'static Module,
        builder: &Builder<'static>,
    ) -> BasicValueEnum<'static> {
        let vector = context.const_string(self.value.as_bytes(), true);
        let ptr = module.add_global(vector.get_type(), Some(AddressSpace::Local), "");
        ptr.set_initializer(&vector);
        let ptr = ptr.as_pointer_value();
        let to_ptr_type = context.i8_type().ptr_type(AddressSpace::Generic);
        ptr.const_cast(to_ptr_type).as_basic_value_enum()
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
    fn value(
        &self,
        context: &'static Context,
        module: &'static Module,
        builder: &Builder<'static>,
    ) -> BasicValueEnum<'static> {
        let addr = self.addr(context, module, builder);
        builder.build_load(
            addr,
            &format!("load variable {}", self.entity.borrow().get_name()),
        )
    }
    fn addr(
        &self,
        context: &'static Context,
        module: &'static Module,
        builder: &Builder<'static>,
    ) -> PointerValue<'static> {
        // must be variable
        if let Entity::Variable(VariableEntity { llvm, .. }) = &*self.entity.borrow() {
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
            PostOp::MemberOfPointer(_) => true, // inner expr shoudl be addressable
            PostOp::MemberOf(_) => self.expr.is_addressable(),
            PostOp::FuncCall(_) => false, // rvalue
        }
    }
    fn addr(
        &self,
        context: &'static Context,
        module: &'static Module,
        builder: &Builder<'static>,
    ) -> PointerValue<'static> {
        match &self.op {
            PostOp::MemberOf(field) => {
                assert!(self.expr.is_addressable());
                let addr = self.expr.addr(context, module, builder);
                let index = self.expr.get_type().field_index(field).unwrap();
                builder
                    .build_struct_gep(addr, index, &format!("field {}", field))
                    .unwrap()
            }
            PostOp::MemberOfPointer(field) => {
                let addr = self
                    .expr
                    .value(context, module, builder)
                    .into_pointer_value();
                let index = Type::element_type(self.expr.get_type())
                    .field_index(field)
                    .unwrap();
                builder
                    .build_struct_gep(addr, index, &format!("field {}", field))
                    .unwrap()
            }
            PostOp::Index(index) => {
                assert!(self.expr.is_addressable());
                if self.expr.get_type().is_array() {
                    let addr = self.expr.addr(context, module, builder);
                    let index = index.value(context, module, builder).into_int_value();
                    let zero = context.i32_type().const_zero();
                    unsafe { builder.build_gep(addr, &[zero, index], "") }
                } else {
                    // is pointer
                    let addr = self
                        .expr
                        .value(context, module, builder)
                        .into_pointer_value();
                    let index = index.value(context, module, builder).into_int_value();
                    unsafe { builder.build_gep(addr, &[index], "") }
                }
            }
            _ => unreachable!(),
        }
    }
    fn value(
        &self,
        context: &'static Context,
        module: &'static Module,
        builder: &Builder<'static>,
    ) -> BasicValueEnum<'static> {
        match &self.op {
            PostOp::Inc => {
                let expr = self.expr.value(context, module, builder);
                if expr.is_int_value() {
                    let inc = builder.build_int_add(
                        expr.into_int_value(),
                        expr.get_type().into_int_type().const_int(1, false),
                        "",
                    );
                    builder.build_store(self.expr.addr(context, module, builder), inc);
                } else if expr.is_pointer_value() {
                    let inc = unsafe {
                        builder.build_gep(
                            expr.into_pointer_value(),
                            &[context.i8_type().const_int(1, false)],
                            "",
                        )
                    };
                    builder.build_store(self.expr.addr(context, module, builder), inc);
                } else {
                    unreachable!()
                }
                expr
            }
            PostOp::Dec => {
                let expr = self.expr.value(context, module, builder);
                if expr.is_int_value() {
                    let dec = builder.build_int_sub(
                        expr.into_int_value(),
                        expr.get_type().into_int_type().const_int(1, false),
                        "",
                    );
                    builder.build_store(self.expr.addr(context, module, builder), dec);
                } else if expr.is_pointer_value() {
                    let dec = unsafe {
                        builder.build_gep(
                            expr.into_pointer_value(),
                            &[context.i8_type().const_int(u64::MAX, true)],
                            "",
                        )
                    };
                    builder.build_store(self.expr.addr(context, module, builder), dec);
                } else {
                    unreachable!()
                }
                expr
            }
            PostOp::FuncCall(args) => {
                let func_entity_node = (&*self.expr as &dyn Any)
                    .downcast_ref::<EntityNode>()
                    .unwrap();
                let args_type = func_entity_node.entity.borrow().get_type().param_types();
                let args = args
                    .iter()
                    .enumerate()
                    .map(|(index, arg)| {
                        if index < args_type.len() {
                            arg.cast_value(context, module, builder, args_type[index].clone())
                        } else {
                            arg.value(context, module, builder)
                        }
                        .into()
                    })
                    .collect::<Vec<_>>();
                let ret = builder
                    .build_call(
                        func_entity_node.entity.borrow().as_function().llvm.unwrap(),
                        &args,
                        "",
                    )
                    .try_as_basic_value();
                ret.left()
                    .or_else(|| {
                        // is void
                        // should not be used
                        // so we will return a zero instead
                        Some(context.i32_type().const_zero().as_basic_value_enum())
                    })
                    .unwrap()
            }
            PostOp::MemberOf(field) => {
                let expr = self
                    .expr
                    .value(context, module, builder)
                    .into_struct_value();
                let index = self.expr.get_type().as_ref().field_index(field).unwrap();
                builder.build_extract_value(expr, index as u32, "").unwrap()
            }
            PostOp::MemberOfPointer(field) => {
                let expr = self
                    .expr
                    .value(context, module, builder)
                    .into_pointer_value();
                let index = self.expr.get_type().as_ref().field_index(field).unwrap();
                let addr = builder.build_struct_gep(expr, index as u32, "").unwrap();
                builder.build_load(addr, "")
            }
            PostOp::Index(_) => {
                let addr = self.addr(context, module, builder);
                builder.build_load(addr, "")
            }
        }
    }
}
impl PostfixExprNode {
    pub fn new(expr: Box<dyn ExprNode>, op: PostOp, location: Range<usize>) -> Self {
        PostfixExprNode { expr, op, location }
    }
    pub fn new_inc(expr: Box<dyn ExprNode>, location: Range<usize>) -> Result<Self, ParserError> {
        if expr.get_type().is_integer() || expr.get_type().is_pointer() {
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
                "Integer or Pointer".to_string(),
                expr.get_type().name(),
                expr.get_location().clone(),
            ))
        }
    }
    pub fn new_dec(expr: Box<dyn ExprNode>, location: Range<usize>) -> Result<Self, ParserError> {
        if expr.get_type().is_integer() || expr.get_type().is_pointer() {
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
                "Integer or Pointer".to_string(),
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
        if expr.get_type().is_array() || expr.get_type().is_pointer() {
            // if is array, should be addressable
            if (!expr.get_type().is_array()) || expr.is_addressable() {
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
                if !exp.1.is_compatible(&act.get_type()) {
                    return Err(ParserError::ArgumentTypeMismatch(
                        index,
                        exp.1.name(),
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
    Neg,
    Not,
    LogicalNot,
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
            UnaryOp::LogicalNot => BOOLEAN_TYPE.clone(),
            UnaryOp::Not => self.expr.get_type(),
            UnaryOp::Neg => self.expr.get_type(),
        }
    }
    fn is_addressable(&self) -> bool {
        // Only Deref is addresasable
        matches!(self.op, UnaryOp::Deref)
    }

    fn addr(
        &self,
        context: &'static Context,
        module: &'static Module,
        builder: &Builder<'static>,
    ) -> PointerValue<'static> {
        match &self.op {
            UnaryOp::Deref => {
                let expr = self.expr.value(context, module, builder);
                assert!(expr.is_pointer_value());
                expr.into_pointer_value()
            }
            _ => unreachable!(),
        }
    }

    fn value(
        &self,
        context: &'static Context,
        module: &'static Module,
        builder: &Builder<'static>,
    ) -> BasicValueEnum<'static> {
        match &self.op {
            UnaryOp::Inc => {
                let expr = self.expr.value(context, module, builder);
                let inc;
                if expr.is_int_value() {
                    let _inc = builder.build_int_add(
                        expr.into_int_value(),
                        expr.get_type().into_int_type().const_int(1, false),
                        "",
                    );
                    builder.build_store(self.expr.addr(context, module, builder), _inc);
                    inc = _inc.as_basic_value_enum();
                } else if expr.is_pointer_value() {
                    let _inc = unsafe {
                        builder.build_gep(
                            expr.into_pointer_value(),
                            &[context.i8_type().const_int(1, false)],
                            "",
                        )
                    };
                    builder.build_store(self.expr.addr(context, module, builder), _inc);
                    inc = _inc.as_basic_value_enum();
                } else {
                    unreachable!()
                }
                inc
            }
            UnaryOp::Dec => {
                let expr = self.expr.value(context, module, builder);
                let inc;
                if expr.is_int_value() {
                    let _inc = builder.build_int_add(
                        expr.into_int_value(),
                        expr.get_type().into_int_type().const_int(1, false),
                        "",
                    );
                    builder.build_store(self.expr.addr(context, module, builder), _inc);
                    inc = _inc.as_basic_value_enum();
                } else if expr.is_pointer_value() {
                    let _inc = unsafe {
                        builder.build_gep(
                            expr.into_pointer_value(),
                            &[context.i8_type().const_int(u64::MAX, false)],
                            "",
                        )
                    };
                    builder.build_store(self.expr.addr(context, module, builder), _inc);
                    inc = _inc.as_basic_value_enum();
                } else {
                    unreachable!()
                }
                inc
            }
            UnaryOp::Neg => {
                let expr = self.expr.value(context, module, builder);
                assert!(expr.is_int_value());
                builder
                    .build_int_neg(expr.into_int_value(), "")
                    .as_basic_value_enum()
            }
            UnaryOp::Addr => {
                assert!(self.expr.is_addressable());
                let expr = self.expr.addr(context, module, builder);
                expr.as_basic_value_enum()
            }
            UnaryOp::Deref => {
                let expr = self.expr.value(context, module, builder);
                assert!(expr.is_pointer_value());
                builder
                    .build_load(expr.into_pointer_value(), "")
                    .as_basic_value_enum()
            }
            UnaryOp::Add => {
                // we do nothing
                // unary + is used to ensure int
                self.expr.value(context, module, builder)
            }
            UnaryOp::Not => {
                let expr = self.expr.value(context, module, builder);
                assert!(expr.is_int_value());
                builder
                    .build_not(expr.into_int_value(), "")
                    .as_basic_value_enum()
            }
            UnaryOp::LogicalNot => {
                let expr = self.expr.value(context, module, builder);
                assert!(expr.is_int_value());
                let expr = expr.into_int_value();
                builder
                    .build_int_compare(IntPredicate::EQ, expr, expr.get_type().const_zero(), "")
                    .as_basic_value_enum()
            }
        }
    }
}
impl UnaryExprNode {
    pub fn new_inc(expr: Box<dyn ExprNode>, location: Range<usize>) -> Result<Self, ParserError> {
        if expr.get_type().is_integer() || expr.get_type().is_pointer() {
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
                "Integer or Pointer".to_string(),
                expr.get_type().name(),
                expr.get_location().clone(),
            ))
        }
    }
    pub fn new_dec(expr: Box<dyn ExprNode>, location: Range<usize>) -> Result<Self, ParserError> {
        if expr.get_type().is_integer() || expr.get_type().is_pointer() {
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
                "Integer or Pointer".to_string(),
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

impl BinaryOp {
    fn to_llvm_op(&self, signed: bool) -> IntPredicate {
        match self {
            BinaryOp::Eq => IntPredicate::EQ,
            BinaryOp::Ne => IntPredicate::NE,
            BinaryOp::Lt => {
                if signed {
                    IntPredicate::SLT
                } else {
                    IntPredicate::ULT
                }
            }
            BinaryOp::Le => {
                if signed {
                    IntPredicate::SLE
                } else {
                    IntPredicate::ULE
                }
            }
            BinaryOp::Gt => {
                if signed {
                    IntPredicate::SGT
                } else {
                    IntPredicate::UGT
                }
            }
            BinaryOp::Ge => {
                if signed {
                    IntPredicate::SGE
                } else {
                    IntPredicate::UGE
                }
            }
            _ => unreachable!(),
        }
    }
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
            BinaryOp::LogicalAnd
            | BinaryOp::LogicalOr
            | BinaryOp::Eq
            | BinaryOp::Ne
            | BinaryOp::Ge
            | BinaryOp::Gt
            | BinaryOp::Le
            | BinaryOp::Lt => BOOLEAN_TYPE.clone(),
            BinaryOp::Comma => self.rhs.get_type(),
            _ => Type::binary_cast(self.lhs.get_type(), self.rhs.get_type()).unwrap(),
        }
    }
    fn is_addressable(&self) -> bool {
        false
    }
    fn value(
        &self,
        context: &'static Context,
        module: &'static Module,
        builder: &Builder<'static>,
    ) -> BasicValueEnum<'static> {
        match &self.op {
            BinaryOp::Add => {
                let result_type = self.get_type();
                if result_type.is_integer() {
                    let lhs = self
                        .lhs
                        .cast_value(context, module, builder, result_type.clone())
                        .into_int_value();
                    let rhs = self
                        .rhs
                        .cast_value(context, module, builder, result_type)
                        .into_int_value();
                    builder.build_int_add(lhs, rhs, "").as_basic_value_enum()
                } else {
                    let lhs = self.lhs.value(context, module, builder);
                    let rhs = self.rhs.value(context, module, builder);
                    if lhs.is_pointer_value() {
                        assert!(rhs.is_int_value());
                        unsafe {
                            builder.build_gep(lhs.into_pointer_value(), &[rhs.into_int_value()], "")
                        }
                        .as_basic_value_enum()
                    } else {
                        assert!(lhs.is_int_value());
                        unsafe {
                            builder.build_gep(rhs.into_pointer_value(), &[lhs.into_int_value()], "")
                        }
                        .as_basic_value_enum()
                    }
                }
            }
            BinaryOp::Sub => {
                let result_type = self.get_type();
                if result_type.is_integer() {
                    let lhs = self
                        .lhs
                        .cast_value(context, module, builder, result_type.clone())
                        .into_int_value();
                    let rhs = self
                        .rhs
                        .cast_value(context, module, builder, result_type)
                        .into_int_value();
                    builder.build_int_sub(lhs, rhs, "").as_basic_value_enum()
                } else {
                    let lhs = self.lhs.value(context, module, builder);
                    let rhs = self.rhs.value(context, module, builder);
                    assert!(lhs.is_pointer_value());
                    assert!(rhs.is_int_value());
                    unsafe {
                        builder.build_gep(
                            lhs.into_pointer_value(),
                            &[builder.build_int_neg(rhs.into_int_value(), "")],
                            "",
                        )
                    }
                    .as_basic_value_enum()
                }
            }
            BinaryOp::Mul => {
                let result_type = self.get_type();
                let lhs = self
                    .lhs
                    .cast_value(context, module, builder, result_type.clone())
                    .into_int_value();
                let rhs = self
                    .rhs
                    .cast_value(context, module, builder, result_type)
                    .into_int_value();
                builder.build_int_mul(lhs, rhs, "").as_basic_value_enum()
            }
            BinaryOp::Div => {
                let result_type = self.get_type();
                let lhs = self
                    .lhs
                    .cast_value(context, module, builder, result_type.clone())
                    .into_int_value();
                let rhs = self
                    .rhs
                    .cast_value(context, module, builder, result_type.clone())
                    .into_int_value();
                let signed = match &*result_type {
                    Type::Integer { signed, .. } => signed,
                    _ => unreachable!(),
                };
                if *signed {
                    builder
                        .build_int_signed_div(lhs, rhs, "")
                        .as_basic_value_enum()
                } else {
                    builder
                        .build_int_unsigned_div(lhs, rhs, "")
                        .as_basic_value_enum()
                }
            }
            BinaryOp::Mod => {
                let result_type = self.get_type();
                let lhs = self
                    .lhs
                    .cast_value(context, module, builder, result_type.clone())
                    .into_int_value();
                let rhs = self
                    .rhs
                    .cast_value(context, module, builder, result_type.clone())
                    .into_int_value();
                let signed = match &*result_type {
                    Type::Integer { signed, .. } => signed,
                    _ => unreachable!(),
                };
                if *signed {
                    builder
                        .build_int_signed_rem(lhs, rhs, "")
                        .as_basic_value_enum()
                } else {
                    builder
                        .build_int_unsigned_rem(lhs, rhs, "")
                        .as_basic_value_enum()
                }
            }
            BinaryOp::Shl => {
                let result_type = self.get_type();
                let lhs = self
                    .lhs
                    .cast_value(context, module, builder, result_type.clone())
                    .into_int_value();
                let rhs = self
                    .rhs
                    .cast_value(context, module, builder, result_type.clone())
                    .into_int_value();
                builder.build_left_shift(lhs, rhs, "").as_basic_value_enum()
            }
            BinaryOp::Shr => {
                let result_type = self.get_type();
                let lhs = self
                    .lhs
                    .cast_value(context, module, builder, result_type.clone())
                    .into_int_value();
                let rhs = self
                    .rhs
                    .cast_value(context, module, builder, result_type.clone())
                    .into_int_value();
                let signed = match &*result_type {
                    Type::Integer { signed, .. } => signed,
                    _ => unreachable!(),
                };
                builder
                    .build_right_shift(lhs, rhs, *signed, "")
                    .as_basic_value_enum()
            }
            BinaryOp::And => {
                let result_type = self.get_type();
                let lhs = self
                    .lhs
                    .cast_value(context, module, builder, result_type.clone())
                    .into_int_value();
                let rhs = self
                    .rhs
                    .cast_value(context, module, builder, result_type.clone())
                    .into_int_value();
                builder.build_and(lhs, rhs, "").as_basic_value_enum()
            }
            BinaryOp::Or => {
                let result_type = self.get_type();
                let lhs = self
                    .lhs
                    .cast_value(context, module, builder, result_type.clone())
                    .into_int_value();
                let rhs = self
                    .rhs
                    .cast_value(context, module, builder, result_type.clone())
                    .into_int_value();
                builder.build_or(lhs, rhs, "").as_basic_value_enum()
            }
            BinaryOp::Xor => {
                let result_type = self.get_type();
                let lhs = self
                    .lhs
                    .cast_value(context, module, builder, result_type.clone())
                    .into_int_value();
                let rhs = self
                    .rhs
                    .cast_value(context, module, builder, result_type.clone())
                    .into_int_value();
                builder.build_xor(lhs, rhs, "").as_basic_value_enum()
            }
            BinaryOp::Eq
            | BinaryOp::Ne
            | BinaryOp::Ge
            | BinaryOp::Gt
            | BinaryOp::Le
            | BinaryOp::Lt => {
                let result_type =
                    Type::binary_cast(self.lhs.get_type(), self.rhs.get_type()).unwrap();
                let lhs = self
                    .lhs
                    .cast_value(context, module, builder, result_type.clone())
                    .into_int_value();
                let rhs = self
                    .rhs
                    .cast_value(context, module, builder, result_type.clone())
                    .into_int_value();
                let signed;
                if let Type::Integer {
                    signed: signed1, ..
                } = &*result_type
                {
                    signed = *signed1;
                } else {
                    signed = false;
                }
                builder
                    .build_int_compare(self.op.to_llvm_op(signed), lhs, rhs, "")
                    .as_basic_value_enum()
            }
            BinaryOp::LogicalAnd => {
                let current_block = builder.get_insert_block().unwrap();
                let func = current_block.get_parent().unwrap();
                let rhs_block = context.append_basic_block(func, "rhs");
                let merge_block = context.append_basic_block(func, "merge");

                let lhs = self.lhs.value(context, module, builder).into_int_value();
                let lhs = builder.build_int_compare(
                    IntPredicate::NE,
                    lhs,
                    lhs.get_type().const_zero(),
                    "",
                );
                let lhs = builder.build_int_truncate(lhs, context.custom_width_int_type(1), "");
                builder.build_conditional_branch(lhs, merge_block, rhs_block);
                builder.position_at_end(rhs_block);
                let rhs = self.rhs.value(context, module, builder).into_int_value();
                let rhs = builder.build_int_compare(
                    IntPredicate::NE,
                    rhs,
                    rhs.get_type().const_zero(),
                    "",
                );
                let rhs = builder.build_int_truncate(rhs, context.custom_width_int_type(1), "");
                builder.build_unconditional_branch(merge_block);
                builder.position_at_end(merge_block);
                let phi = builder.build_phi(context.custom_width_int_type(1), "");
                phi.add_incoming(&[(&context.custom_width_int_type(1).const_int(1, false), current_block),
                    (&rhs, rhs_block)]);
                phi.as_basic_value()
            }
            BinaryOp::LogicalOr => {
                let current_block = builder.get_insert_block().unwrap();
                let func = current_block.get_parent().unwrap();
                let rhs_block = context.append_basic_block(func, "rhs");
                let merge_block = context.append_basic_block(func, "merge");

                let lhs = self.lhs.value(context, module, builder).into_int_value();
                let lhs = builder.build_int_compare(
                    IntPredicate::EQ,
                    lhs,
                    lhs.get_type().const_zero(),
                    "",
                );
                let lhs = builder.build_int_truncate(lhs, context.custom_width_int_type(1), "");
                builder.build_conditional_branch(lhs, merge_block, rhs_block);
                builder.position_at_end(rhs_block);
                let rhs = self.rhs.value(context, module, builder).into_int_value();
                let rhs = builder.build_int_compare(
                    IntPredicate::EQ,
                    rhs,
                    rhs.get_type().const_zero(),
                    "",
                );
                let rhs = builder.build_int_truncate(rhs, context.custom_width_int_type(1), "");
                builder.build_unconditional_branch(merge_block);
                builder.position_at_end(merge_block);
                let phi = builder.build_phi(context.custom_width_int_type(1), "");
                phi.add_incoming(&[(&context.custom_width_int_type(1).const_int(0, false), current_block),
                    (&rhs, rhs_block)]);
                phi.as_basic_value()
            },
            BinaryOp::Comma => {
                // calculate lhs, drop
                self.lhs.value(context, module, builder);
                self.rhs.value(context, module, builder)
            }
        }
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
        // allow sub int from pointers
        if (lhs.get_type().is_integer() || lhs.get_type().is_pointer())
            && rhs.get_type().is_integer()
        {
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
        self.then_expr.is_addressable() && self.else_expr.is_addressable()
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

    fn value(
        &self,
        context: &'static Context,
        module: &'static Module,
        builder: &Builder<'static>,
    ) -> BasicValueEnum<'static> {
        match self.op {
            AssignOp::Assign => {
                let rhs = self.rhs.value(context, module, builder);
                let addr = self.lhs.addr(context, module, builder);
                builder.build_store(addr, rhs);
                rhs
            }
            _ => todo!(),
        }
    }
}
impl AssignExprNode {
    pub fn new(
        lhs: Box<dyn ExprNode>,
        rhs: Box<dyn ExprNode>,
        op: AssignOp,
        location: Range<usize>,
    ) -> Result<Self, ParserError> {
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
            AssignOp::Assign => {
                if !lhs.get_type().is_compatible(rhs.get_type().as_ref()) {
                    return Err(ParserError::InvalidOperation(location));
                }
            }
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
