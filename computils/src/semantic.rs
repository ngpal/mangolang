#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
    Unit,
    Ref(Box<Type>),
}

impl Type {
    pub fn to_str(&self) -> String {
        match self {
            Type::Int => "int".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Unit => "unit".to_string(),
            Type::Ref(inner) => format!("ref {}", inner.to_str()),
        }
    }

    pub fn get_size_words(&self) -> u16 {
        match self {
            Type::Int => 1,
            Type::Bool => 1,
            Type::Unit => 0,
            Type::Ref(_) => 1,
        }
    }
}
