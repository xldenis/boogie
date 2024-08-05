#[derive(Debug, Clone)]
pub struct Program {
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Const(ConstDecl),
    Function(FunctionDecl),
    Axiom(Axiom),
    Type(TypeDecl),
    Datatype(DatatypeDecl),
    GlobalVar(GlobalVarDecl),
    Procedure(ProcedureDecl),
    Implementation(ImplementationDecl),
    Action(ActionDecl),
    YieldInvariant(YieldInvariantDecl),
    YieldProcedure(YieldProcedureDecl),
}

#[derive(Debug, Clone)]
pub struct ConstDecl {
    pub vars: Vec<(String, Type)>,
    pub unique: bool,
    pub axioms: Vec<Axiom>,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub signature: Signature,
    pub body: Option<Expression>,
    pub axioms: Option<Vec<Axiom>>,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, Clone)]
pub struct AxiomDecl {
    pub hideable: bool,
    pub expression: Expression,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub names: Vec<(String, Vec<TypeVariable>)>,
    pub body: Option<Type>,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, Clone)]
pub struct DatatypeDecl {
    pub name: String,
    pub type_params: Vec<TypeVariable>,
    pub constructors: Vec<DatatypeConstructor>,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, Clone)]
pub struct DatatypeConstructor {
    pub name: String,
    pub fields: Vec<Variable>,
}

#[derive(Debug, Clone)]
pub struct GlobalVarDecl {
    pub name: String,
    pub typ: Type,
    pub where_: Option<Expression>,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, Clone)]
pub struct ProcedureDecl {
    pub pure: bool,
    pub signature: Signature,
    pub specifications: Specifications,
    pub body: Option<ImplBlock>,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, Clone)]
pub struct Specifications {
    pub requires: Vec<Requires>,
    pub modifies: Vec<String>,
    pub ensures: Vec<Ensures>,
}

#[derive(Debug, Clone)]
pub struct Signature {
    pub name: String,
    pub type_params: Vec<TypeVariable>,
    pub params: Vec<FormalArg>,
    pub returns: Option<FormalArg>,
}

#[derive(Debug, Clone)]
pub enum FormalArg {
    Anon(Type),
    Named(String, Type),
}

#[derive(Debug, Clone)]
pub struct ImplementationDecl {
    pub signature: Signature,
    pub body: ImplBlock,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, Clone)]
pub struct ActionDecl {
    pub signature: Signature,
    pub body: ImplBlock,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, Clone)]
pub struct YieldInvariantDecl {
    pub name: String,
    pub params: Vec<Variable>,
    pub invariants: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct YieldProcedureDecl {
    pub signature: Signature,
    pub requires: Vec<Requires>,
    pub yield_requires: Vec<Expression>,
    pub yield_ensures: Vec<Expression>,
    pub yield_preserves: Vec<Expression>,
    pub ensures: Vec<Ensures>,
    pub body: Option<ImplBlock>,
}

#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Real,
    Bool,
    Map(Vec<String>, Vec<Type>, Box<Type>),
    TypeVar(String),
    UserDefined(String, Vec<Type>),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Var(String),
    Field(Box<Expression>, String),
    UnaryOp(UnaryOp, Box<Expression>),
    BinaryOp(BinaryOp, Box<Expression>, Box<Expression>),
    FunctionCall(String, Vec<Expression>),
    MapSelect(Box<Expression>, Vec<Expression>),
    BvExtract(Box<Expression>, Box<Expression>, Box<Expression>),
    MapUpdate(Box<Expression>, Box<Expression>, Box<Expression>),
    Old(Box<Expression>),
    Quantifier(
        Quantifier,
        Vec<(String, Type)>,
        Vec<Trigger>,
        Box<Expression>,
    ),
    Lambda(Vec<Variable>, Box<Expression>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
}

#[derive(Debug, Clone)]
pub struct Trigger(pub Vec<Expression>);

#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Real(f64),
    Bool(bool),
    BitVector(i128, u16),
    String(String),
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
    Implies,
    Iff,
    Pow,
    Concat,
}

#[derive(Debug, Clone)]
pub enum Quantifier {
    Forall,
    Exists,
}

#[derive(Debug, Clone)]
pub struct ImplBlock {
    pub local_vars: Vec<(String, Type)>,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub label: String,
    pub statements: Vec<Statement>,
    pub term: Option<Box<Statement>>,
}

#[derive(Debug, Clone)]
pub enum Lhs {
    Simple(String),
    Map(Box<Lhs>, Vec<Expression>),
    Field(Box<Lhs>, String),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Block(BasicBlock),
    Assign(Vec<Lhs>, Vec<Expression>),
    Assert(Vec<Attribute>, Expression),
    Assume(Vec<Attribute>, Expression),
    Havoc(Vec<String>),
    Call(String, Vec<Expression>, Vec<Expression>),
    If(Option<Expression>, ImplBlock, Option<ImplBlock>),
    While(Option<Expression>, Vec<Expression>, ImplBlock),
    Break(Option<String>),
    Goto(Vec<String>),
    Return,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub typ: Type,
    pub where_clause: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct TypeVariable {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct Attribute {
    pub name: String,
    pub params: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct Requires {
    pub free: bool,
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub struct Ensures {
    pub free: bool,
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub struct Axiom {
    pub hideable: bool,
    pub expression: Expression,
    pub attributes: Vec<Attribute>,
}
