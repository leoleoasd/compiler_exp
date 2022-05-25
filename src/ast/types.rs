use std::{boxed::Box, collections::HashMap};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type {
    Void,
    Integer(IntegerType),
    Array(ArrayType),
    Function(FunctionType),
    Pointer(PointerType),
    Struct(StructType),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IntegerType {
    pub signed: bool,
    pub size: usize,
}
#[derive(Clone, Debug, Eq, PartialEq)]

pub struct ArrayType {
    pub size: usize,
    pub element_type: Box<Type>,
}
#[derive(Clone, Debug, Eq, PartialEq)]

pub struct FunctionType {
    pub return_type: Box<Type>,
    pub parameters: Vec<Type>,
    pub variadic: bool,
}
#[derive(Clone, Debug, Eq, PartialEq)]

pub struct PointerType {
    pub element_type: Box<Type>,
}
#[derive(Clone, Debug, Eq, PartialEq)]

pub struct StructType {
    pub name: String,
    pub fields: Vec<(String, Type)>,
}
#[derive(Clone, Debug, Eq, PartialEq)]

pub struct TypeDefType {
    pub name: String,
    pub inner: Box<Type>,
}
impl From<StructType> for Type {
    fn from(struct_type: StructType) -> Self {
        Type::Struct(struct_type)
    }
}
impl From<IntegerType> for Type {
    fn from(integer_type: IntegerType) -> Self {
        Type::Integer(integer_type)
    }
}
impl From<ArrayType> for Type {
    fn from(array_type: ArrayType) -> Self {
        Type::Array(array_type)
    }
}
impl From<FunctionType> for Type {
    fn from(function_type: FunctionType) -> Self {
        Type::Function(function_type)
    }
}
impl From<PointerType> for Type {
    fn from(pointer_type: PointerType) -> Self {
        Type::Pointer(pointer_type)
    }
}

impl Type {
    pub fn is_void(&self) -> bool {
        matches!(self, Type::Void)
    }
    pub fn name(&self) -> String {
        match self {
            Type::Void => "void".to_string(),
            Type::Integer(integer_type) => {
                let mut name = "int".to_string();
                if integer_type.signed {
                    name.insert(0, 'u');
                }
                name.push_str(&integer_type.size.to_string());
                name
            }
            Type::Array(array_type) => {
                format!("[{} x {}]", array_type.size, array_type.element_type.name())
            }
            Type::Function(function_type) => {
                let mut name = format!("{} (", function_type.return_type.name());
                for (i, parameter) in function_type.parameters.iter().enumerate() {
                    if i > 0 {
                        name.push_str(", ");
                    }
                    name.push_str(&parameter.name());
                }
                name.push(')');
                name
            }
            Type::Pointer(pointer_type) => {
                format!("{}*", pointer_type.element_type.name())
            }
            Type::Struct(struct_type) => struct_type.name.clone(),
        }
    }
    pub fn pointer_type(&self) -> PointerType {
        PointerType {
            element_type: Box::new(self.clone()),
        }
    }
    pub fn array_type(&self, size: usize) -> ArrayType {
        ArrayType {
            size,
            element_type: Box::new(self.clone()),
        }
    }
    pub fn function_type(&self, parameters: Vec<Type>, variadic: bool) -> FunctionType {
        FunctionType {
            return_type: Box::new(self.clone()),
            parameters,
            variadic,
        }
    }
}

impl Default for Type {
    fn default() -> Self {
        Type::Void
    }
}
