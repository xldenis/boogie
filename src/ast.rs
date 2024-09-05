#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct ConstDecl {
    pub vars: Vec<(String, Type)>,
    pub unique: bool,
    pub axioms: Vec<Axiom>,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDecl {
    pub signature: Signature,
    pub body: Option<Expression>,
    pub axioms: Option<Vec<Axiom>>,
    pub attributes: Vec<Attribute>,
}

// #[derive(Debug, Clone, PartialEq)]
// pub struct AxiomDecl {
//     pub hideable: bool,
//     pub expression: Expression,
//     pub attributes: Vec<Attribute>,
// }

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDecl {
    pub names: Vec<(String, Vec<TypeVariable>)>,
    pub body: Option<Type>,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DatatypeDecl {
    pub name: String,
    pub type_params: Vec<TypeVariable>,
    pub constructors: Vec<DatatypeConstructor>,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DatatypeConstructor {
    pub name: String,
    pub fields: Vec<Variable>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GlobalVarDecl {
    pub name: String,
    pub typ: Type,
    pub where_: Option<Expression>,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProcedureDecl {
    pub pure: bool,
    pub signature: Signature,
    pub specifications: Specifications,
    pub body: Option<ImplBlock>,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Specifications {
    pub requires: Vec<Requires>,
    pub modifies: Vec<String>,
    pub ensures: Vec<Ensures>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Signature {
    pub name: String,
    pub type_params: Vec<TypeVariable>,
    pub params: Vec<FormalArg>,
    pub returns: Option<Vec<FormalArg>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FormalArg {
    Anon(Type),
    Named(Vec<Attribute>, String, Type),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImplementationDecl {
    pub signature: Signature,
    pub body: ImplBlock,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Mover {
    Atomic,
    Both,
    Left,
    Right,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ActionDecl {
    pub mover: Option<Mover>,
    pub signature: Signature,
    pub specification: ActionSpecifications,
    pub body: ImplBlock,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct YieldInvariantDecl {
    pub name: String,
    pub params: Vec<FormalArg>,
    pub invariants: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct YieldSpecifications {
    pub requires: Vec<Expression>,
    pub modifies: Vec<String>,
    pub ensures: Vec<Expression>,
    pub refines: Vec<(String, Option<String>)>,
    pub preserves: Vec<CallExp>,
    pub req_calls: Vec<CallExp>,
    pub ens_calls: Vec<CallExp>,
}

type CallExp = (String, Vec<Expression>);

#[derive(Debug, Clone, PartialEq)]
pub struct ActionSpecifications {
    pub requires: Vec<Expression>,
    pub requires_call: Vec<(String, Vec<Expression>)>,
    pub modifies: Vec<String>,
    pub asserts: Vec<Expression>,
    pub refines: Vec<String>,
    pub creates: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct YieldProcedureDecl {
    pub attrs: Vec<Attribute>,
    pub signature: Signature,
    pub spec: YieldSpecifications,
    pub body: Option<ImplBlock>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Real,
    Bool,
    Map(Vec<String>, Vec<Type>, Box<Type>),
    TypeVar(String),
    UserDefined(String, Vec<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(Literal),
    Var(String),
    Field(Box<Expression>, String),
    UnaryOp(UnaryOp, Box<Expression>),
    BinaryOp(BinaryOp, Box<Expression>, Box<Expression>),
    FunctionCall(String, Vec<Expression>),
    MapSelect(Box<Expression>, Vec<Expression>),
    BvExtract(Box<Expression>, Box<Expression>, Box<Expression>),
    MapUpdate(Box<Expression>, Vec<Expression>, Box<Expression>),
    Old(Box<Expression>),
    Quantifier(
        Quantifier,
        Vec<(String, Type)>,
        Vec<Trigger>,
        Box<Expression>,
    ),
    IntCast(Box<Expression>),
    RealCast(Box<Expression>),
    Lambda(Vec<FormalArg>, Box<Expression>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
    Rounding(Rounding),
    Is(Box<Expression>, String),
    Let(
        Vec<(Vec<Attribute>, String)>,
        Vec<Expression>,
        Vec<Attribute>,
        Box<Expression>,
    ),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Rounding {
    RNE,
    RTN,
    RNA,
    RTP,
    RTZ,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Trigger(pub Vec<Expression>);

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Real(f64),
    Bool(bool),
    BitVector(i128, u16),
    String(String),
    Float(String), // TODO: Parse
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[derive(Debug, Clone, Copy, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum Quantifier {
    Forall,
    Exists,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImplBlock {
    pub local_vars: Vec<(String, Type)>,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BasicBlock {
    pub label: String,
    pub statements: Vec<Statement>,
    pub term: Option<Box<Statement>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lhs {
    Simple(String),
    Map(Box<Lhs>, Vec<Expression>),
    Field(Box<Lhs>, String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Block(BasicBlock),
    Assign(Vec<Lhs>, Vec<Expression>),
    Assert(Vec<Attribute>, Expression),
    Assume(Vec<Attribute>, Expression),
    Havoc(Vec<String>),
    Call(Vec<String>, String, Vec<Expression>),
    If(Option<Expression>, ImplBlock, Option<ImplBlock>),
    While(Option<Expression>, Vec<Invariant>, ImplBlock),
    Break(Option<String>),
    Goto(Vec<String>),
    Par(Vec<(Option<Vec<String>>, String, Vec<Expression>)>),
    Return,
    Unpack((String, Vec<String>), Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Invariant {
    Expression(Vec<Attribute>, Expression),
    Call(String, Vec<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub name: String,
    pub typ: Type,
    pub where_clause: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeVariable {
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    pub name: String,
    pub params: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Requires {
    pub free: bool,
    pub expression: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ensures {
    pub free: bool,
    pub expression: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Axiom {
    pub hideable: bool,
    pub expression: Expression,
    pub attributes: Vec<Attribute>,
}
