use std::{cmp::max, ops::Range, sync::Arc};

use inkwell::{
    context::{Context, ContextRef},
    types::{AnyType, AnyTypeEnum, BasicType, BasicTypeEnum},
    AddressSpace,
};

use crate::parser::errors::ParserError;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type {
    Void,
    Integer {
        signed: bool,
        size: usize,
    },
    Array {
        size: usize,
        element_type: Arc<Type>,
    },
    Function {
        return_type: Arc<Type>,
        parameters: Vec<Arc<Type>>,
        variadic: bool,
    },
    Pointer {
        element_type: Arc<Type>,
    },
    Struct {
        name: String,
        fields: Vec<(String, Arc<Type>)>,
        location: Range<usize>,
    },
}

impl Type {
    pub fn is_void(&self) -> bool {
        matches!(self, Type::Void)
    }
    pub fn name(&self) -> String {
        match self {
            Type::Void => "void".to_string(),
            Type::Integer { signed, size } => {
                let mut name = "int".to_string();
                if !*signed {
                    name.insert(0, 'u');
                }
                name.push_str(&size.to_string());
                name
            }
            Type::Array { element_type, size } => {
                format!("[{} x {}]", size, element_type.name())
            }
            Type::Function {
                return_type,
                parameters,
                variadic,
            } => {
                let mut name = format!("{} (", return_type.name());
                for (i, parameter) in parameters.iter().enumerate() {
                    if i > 0 {
                        name.push_str(", ");
                    }
                    name.push_str(&parameter.name());
                }
                name.push(')');
                name
            }
            Type::Pointer { element_type } => {
                format!("{}*", element_type.name())
            }
            Type::Struct { name, .. } => format!("struct {name}"),
        }
    }
    pub fn pointer_type(s: Arc<Self>) -> Arc<Type> {
        Arc::new(Type::Pointer { element_type: s })
    }
    pub fn array_type(s: Arc<Self>, size: usize) -> Arc<Type> {
        Arc::new(Type::Array {
            size,
            element_type: s,
        })
    }
    pub fn function_type(s: Arc<Self>, parameters: Vec<Arc<Type>>, variadic: bool) -> Arc<Type> {
        Arc::new(Type::Function {
            return_type: s,
            parameters,
            variadic,
        })
    }
    pub fn element_type(s: Arc<Self>) -> Arc<Type> {
        match &*s {
            Type::Array { element_type, .. } => element_type.clone(),
            Type::Pointer { element_type } => element_type.clone(),
            Type::Function { return_type, .. } => return_type.clone(),
            _ => panic!("Type does not have an element type"),
        }
    }
    pub fn field_type(s: Arc<Self>, field: &str) -> Option<Arc<Type>> {
        match &*s {
            Type::Struct { fields, .. } => fields
                .iter()
                .find(|(name, _)| name == field)
                .map(|(_, t)| t.clone()),
            _ => None,
        }
    }
    pub fn is_compatible(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Void, Type::Void) => true,
            (Type::Integer { .. }, Type::Integer { .. }) => true,
            (Type::Pointer { .. }, Type::Pointer { .. }) => true,
            (
                Type::Array { element_type, .. },
                Type::Pointer {
                    element_type: element_type2,
                    ..
                },
            ) => element_type == element_type2,
            (
                Type::Pointer {
                    element_type: element_type1,
                    ..
                },
                Type::Array {
                    element_type: element_type2,
                    ..
                },
            ) => element_type1 == element_type2,
            (Type::Pointer { .. }, Type::Integer { .. }) => true,
            (Type::Integer { .. }, Type::Pointer { .. }) => true,
            _ => false,
        }
    }
    pub fn binary_cast(lhs: Arc<Type>, rhs: Arc<Type>) -> Option<Arc<Type>> {
        if lhs == rhs {
            return Some(rhs);
        }
        match (&*lhs, &*rhs) {
            (
                Type::Integer {
                    signed: signed1,
                    size: size1,
                },
                Type::Integer {
                    signed: signed2,
                    size: size2,
                },
            ) => Some(Arc::new(Type::Integer {
                signed: *signed1 && *signed2,
                size: max(*size1, *size2),
            })),
            (Type::Pointer { .. }, Type::Integer { .. }) => Some(lhs.clone()),
            (Type::Integer { .. }, Type::Pointer { .. }) => Some(rhs.clone()),
            _ => None,
        }
    }
    pub fn is_integer(&self) -> bool {
        matches!(self, Type::Integer { .. })
    }
    pub fn is_array(&self) -> bool {
        matches!(self, Type::Array { .. })
    }
    pub fn is_function(&self) -> bool {
        matches!(self, Type::Function { .. })
    }
    pub fn is_pointer(&self) -> bool {
        matches!(self, Type::Pointer { .. })
    }
    pub fn is_struct(&self) -> bool {
        matches!(self, Type::Struct { .. })
    }
    pub fn to_llvm_type<'ctx>(&self, context: &'ctx Context) -> BasicTypeEnum<'ctx> {
        match &self {
            Type::Integer { size, .. } => context
                .custom_width_int_type(*size as u32)
                .as_basic_type_enum(),
            Type::Array { size, element_type } => match element_type.to_llvm_type(context) {
                BasicTypeEnum::ArrayType(a) => a.array_type(*size as u32).as_basic_type_enum(),
                BasicTypeEnum::IntType(t) => t.array_type(*size as u32).as_basic_type_enum(),
                BasicTypeEnum::PointerType(t) => t.array_type(*size as u32).as_basic_type_enum(),
                BasicTypeEnum::StructType(t) => t.array_type(*size as u32).as_basic_type_enum(),
                BasicTypeEnum::VectorType(_) => unreachable!(),
                _ => panic!("Array of {:?} isn't allowed!", element_type),
            },
            Type::Pointer { element_type } => {
                if matches!(&**element_type, Type::Void) {
                    context
                        .i8_type()
                        .ptr_type(AddressSpace::Generic)
                        .as_basic_type_enum()
                } else {
                    match element_type.to_llvm_type(context) {
                        BasicTypeEnum::ArrayType(a) => {
                            a.ptr_type(AddressSpace::Generic).as_basic_type_enum()
                        }
                        BasicTypeEnum::IntType(t) => {
                            t.ptr_type(AddressSpace::Generic).as_basic_type_enum()
                        }
                        BasicTypeEnum::PointerType(t) => {
                            t.ptr_type(AddressSpace::Generic).as_basic_type_enum()
                        }
                        BasicTypeEnum::StructType(t) => {
                            t.ptr_type(AddressSpace::Generic).as_basic_type_enum()
                        }
                        BasicTypeEnum::VectorType(_) => unreachable!(),
                        BasicTypeEnum::FloatType(_) => unreachable!(),
                    }
                }
            }
            Type::Struct {
                name,
                fields,
                location,
            } => {
                let mut llvm_fields = Vec::new();
                for (name, field_type) in fields {
                    llvm_fields.push(field_type.to_llvm_type(context));
                }
                context
                    .struct_type(&llvm_fields, false)
                    .as_basic_type_enum()
            }
            _ => {
                panic!("to_llvm_type called for {:?}", self.name());
            }
        }
    }
    pub fn is_legal(&self) -> Result<(), ParserError> {
        match self {
            Type::Void => Err(ParserError::IllegalType(self.name(), None)),
            Type::Integer { .. } => Ok(()),
            Type::Array { size, element_type } => {
                // element must be legal
                if matches!(&**element_type, Type::Void) {
                    Err(ParserError::IllegalType(self.name(), None))
                } else {
                    element_type.is_legal()?;
                    // size must be legal
                    if *size == 0 {
                        Err(ParserError::IllegalType(self.name(), None))
                    } else {
                        Ok(())
                    }
                }
            }
            Type::Function {
                return_type,
                parameters,
                variadic,
            } => {
                // allow return void, check legal for other return type
                if !matches!(&**return_type, Type::Void) && return_type.is_legal().is_err() {
                    return Err(ParserError::IllegalType(self.name(), None));
                }
                if matches!(&**return_type, Type::Array { .. }) {
                    return Err(ParserError::IllegalType(self.name(), None));
                }
                for parameter in parameters {
                    if parameter.is_legal().is_err() {
                        return Err(ParserError::IllegalType(self.name(), None));
                    }
                }
                Ok(())
            }
            Type::Pointer { element_type } => {
                // pointer to arbitrary type is allowed
                Ok(())
            }
            Type::Struct {
                fields, location, ..
            } => {
                // all fields must be legal
                for (name, field_type) in fields {
                    if field_type.is_legal().is_err() {
                        return Err(ParserError::IllegalType(
                            self.name(),
                            Some(location.clone()),
                        ));
                    }
                }
                Ok(())
            }
        }
    }
}

impl Default for Type {
    fn default() -> Self {
        Type::Void
    }
}
