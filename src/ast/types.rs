use std::{boxed::Box, collections::HashMap};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type {
    Void,
    Integer {
        signed: bool,
        size: usize,
    },
    Array {
        size: usize,
        element_type: Box<Type>,
    },
    Function {
        return_type: Box<Type>,
        parameters: Vec<Type>,
        variadic: bool,
    },
    Pointer {
        element_type: Box<Type>,
    },
    Struct {
        name: String,
        fields: Vec<(String, Type)>,
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
                if *signed {
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
            Type::Struct { name, fields } => name.clone(),
        }
    }
    pub fn pointer_type(&self) -> Type {
        Type::Pointer {
            element_type: Box::new(self.clone()),
        }
    }
    pub fn array_type(&self, size: usize) -> Type {
        Type::Array {
            size,
            element_type: Box::new(self.clone()),
        }
    }
    pub fn function_type(&self, parameters: Vec<Type>, variadic: bool) -> Type {
        Type::Function {
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
