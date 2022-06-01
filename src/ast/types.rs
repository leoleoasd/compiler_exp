use std::{ops::Range, sync::Arc};

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
            Type::Struct { name, .. } => name.clone(),
        }
    }
    pub fn pointer_type(s: Arc<Self>) -> Arc<Type> {
        Arc::new(Type::Pointer {
            element_type: s.clone(),
        })
    }
    pub fn array_type(s: Arc<Self>, size: usize) -> Arc<Type> {
        Arc::new(Type::Array {
            size,
            element_type: s.clone(),
        })
    }
    pub fn function_type(s: Arc<Self>, parameters: Vec<Arc<Type>>, variadic: bool) -> Arc<Type> {
        Arc::new(Type::Function {
            return_type: s.clone(),
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
}

impl Default for Type {
    fn default() -> Self {
        Type::Void
    }
}
