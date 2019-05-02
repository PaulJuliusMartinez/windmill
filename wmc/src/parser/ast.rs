#[derive(Debug)]
pub enum Item<'a> {
    FunctionDef(FunctionDef<'a>),
    StructDef(StructDef<'a>),
    VariantDef(VariantDef<'a>),
    // InterfaceDef,
    // Implementation,
    // StaticDecl,
}

#[derive(Debug)]
pub struct FunctionDef<'a> {
    pub name: &'a str,
    pub generics: Vec<&'a str>,
    pub arguments: Vec<(&'a str, TypeLiteral<'a>)>,
    pub return_type: Option<TypeLiteral<'a>>,
    // pub body: Expression,
}

#[derive(Debug)]
pub enum TypeLiteral<'a> {
    ParameterizedType {
        main_type: &'a str,
        generic_types: Vec<TypeLiteral<'a>>,
    },
    TupleType(Vec<TypeLiteral<'a>>),
    FnType {
        arg_types: Vec<TypeLiteral<'a>>,
        return_type: Box<TypeLiteral<'a>>,
    },
    ArrayType(Box<TypeLiteral<'a>>),
    // AnonStructType(Vec<(&'a str, TypeLiteral<'a>)>),
}

#[derive(Debug)]
pub struct StructDef<'a> {
    fields: Vec<(&'a str, TypeLiteral<'a>)>,
}

#[derive(Debug)]
pub struct VariantDef<'a> {
    name: &'a str,
    kind: VariantKind<'a>,
}

#[derive(Debug)]
pub enum VariantKind<'a> {
    TupleVariant(Vec<TypeLiteral<'a>>),
    StructVariant(Vec<(&'a str, TypeLiteral<'a>)>),
}
