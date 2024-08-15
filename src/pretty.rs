use pretty::{docs, DocAllocator, Pretty};

use crate::ast::*;

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a Declaration
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        match self {
            Declaration::Const(decl) => decl.pretty(alloc),
            Declaration::Function(decl) => decl.pretty(alloc),
            Declaration::Axiom(decl) => decl.pretty(alloc),
            Declaration::Type(decl) => decl.pretty(alloc),
            Declaration::Datatype(decl) => decl.pretty(alloc),
            Declaration::GlobalVar(decl) => decl.pretty(alloc),
            Declaration::Procedure(decl) => decl.pretty(alloc),
            Declaration::Implementation(decl) => decl.pretty(alloc),
            Declaration::Action(decl) => decl.pretty(alloc),
            Declaration::YieldInvariant(decl) => decl.pretty(alloc),
            Declaration::YieldProcedure(decl) => decl.pretty(alloc),
        }
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a ConstDecl
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        let mut doc = alloc.text("const");

        // Add 'unique' if applicable
        if self.unique {
            doc = doc.append(alloc.space()).append("unique");
        }

        let attrs = alloc.intersperse(self.attributes.iter(), alloc.space());
        // Add attributes
        doc = doc.append(alloc.space()).append(attrs);

        // Add variables
        let vars_doc = alloc.intersperse(
            self.vars.iter().map(|(name, typ)| {
                alloc
                    .text(name)
                    .append(":")
                    .append(alloc.space())
                    .append(typ.pretty(alloc))
            }),
            ", ",
        );
        doc = doc.append(alloc.space()).append(vars_doc);

        // Add semicolon if there are no axioms
        if self.axioms.is_empty() {
            doc = doc.append(";");
        } else {
            // Add 'uses' block with axioms
            doc = doc
                .append(alloc.space())
                .append("uses")
                .append(alloc.space());

            let axioms = alloc
                .intersperse(self.axioms.iter(), alloc.hardline())
                .nest(2)
                .braces();
            doc = doc.append(axioms);
        }

        doc.group()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a Axiom
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        let mut doc = alloc.nil();

        // Add 'hideable' if applicable
        if self.hideable {
            doc = doc.append("hideable").append(alloc.space());
        }

        doc = doc.append("axiom");

        // Add attributes
        let attrs_doc = alloc.intersperse(self.attributes.iter(), alloc.space());
        doc = doc.append(alloc.space()).append(attrs_doc);

        // Add expression
        doc = doc
            .append(alloc.space())
            .append(self.expression.pretty(alloc))
            .append(";");

        doc.group()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a TypeDecl
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        let mut doc = alloc.text("type");

        // Add attributes
        let attrs_doc = alloc.intersperse(self.attributes.iter(), alloc.space());
        doc = doc.append(alloc.space()).append(attrs_doc);

        // Add type names and parameters
        let names_doc = alloc.intersperse(
            self.names.iter().map(|(name, params)| {
                let name_doc = alloc.text(name);
                if params.is_empty() {
                    name_doc
                } else {
                    let params_doc = alloc.intersperse(params.iter(), ", ");
                    name_doc.append(params_doc.angles())
                }
            }),
            ", ",
        );
        doc = doc.append(alloc.space()).append(names_doc);

        // Add body if present
        if let Some(body) = &self.body {
            doc = doc
                .append(alloc.space())
                .append("=")
                .append(alloc.space())
                .append(body.pretty(alloc));
        }

        doc.append(";").group()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a DatatypeDecl
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        let mut doc = alloc.text("datatype");

        // Add attributes
        let attrs_doc = alloc.intersperse(self.attributes.iter(), alloc.space());
        doc = doc.append(alloc.space()).append(attrs_doc);

        // Add name and type parameters
        doc = doc.append(alloc.space()).append(&self.name);
        if !self.type_params.is_empty() {
            let params_doc = alloc.intersperse(self.type_params.iter(), ", ");
            doc = doc.append(params_doc.angles());
        }

        // Add constructors
        let constructors_doc = alloc.intersperse(
            self.constructors.iter().map(|constructor| {
                let mut cons_doc = alloc.text(&constructor.name);
                if !constructor.fields.is_empty() {
                    let fields_doc = alloc.intersperse(
                        constructor.fields.iter().map(|field| {
                            alloc
                                .text(&field.name)
                                .append(":")
                                .append(alloc.space())
                                .append(field.typ.pretty(alloc))
                        }),
                        ", ",
                    );
                    cons_doc = cons_doc.append(fields_doc.parens());
                }
                cons_doc
            }),
            alloc.text(",").append(alloc.hardline()),
        );

        doc.append(alloc.space())
            .append(constructors_doc.braces().nest(2))
            .group()
    }
}
impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a GlobalVarDecl
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        let mut doc = alloc.text("var");

        // Add attributes
        let attrs_doc = alloc.intersperse(self.attributes.iter(), alloc.space());
        doc = doc.append(alloc.space()).append(attrs_doc);

        // Add name and type
        doc = doc
            .append(alloc.space())
            .append(&self.name)
            .append(":")
            .append(alloc.space())
            .append(self.typ.pretty(alloc));

        // Add where clause if present
        if let Some(where_clause) = &self.where_ {
            doc = doc
                .append(alloc.space())
                .append("where")
                .append(alloc.space())
                .append(where_clause.pretty(alloc));
        }

        doc.append(";").group()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a FunctionDecl
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        let mut doc = alloc.text("function");

        // Add attributes
        let attrs_doc = alloc.intersperse(&self.attributes, alloc.space());
        doc = doc.append(alloc.space()).append(attrs_doc);

        // Add function signature
        doc = doc.append(alloc.space()).append(&self.signature);

        // Add body if present
        if let Some(body) = &self.body {
            doc = doc.append(alloc.space()).append(
                docs![
                    alloc,
                    alloc.line(),
                    body.pretty(alloc).nest(2),
                    alloc.line()
                ]
                .braces(),
            );
        } else {
            doc = doc.append(";");
        }

        // Add axioms if present
        if let Some(axioms) = &self.axioms {
            let axioms_doc = alloc.intersperse(axioms.iter(), alloc.hardline());
            doc = doc
                .append(alloc.space())
                .append("uses")
                .append(alloc.space())
                .append(docs![alloc, alloc.line(), axioms_doc.nest(2), alloc.line()].braces());
        }

        doc.group()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a ProcedureDecl
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        let mut doc = alloc.text(if self.pure {
            "pure procedure"
        } else {
            "procedure"
        });

        // Add attributes
        if !self.attributes.is_empty() {
            let attrs_doc = alloc.intersperse(&self.attributes, alloc.space());
            doc = doc.append(alloc.space()).append(attrs_doc);
        }

        // Add procedure signature
        doc = doc.append(alloc.space()).append(&self.signature);

        // Add specifications
        doc = doc.append(alloc.space()).append(&self.specifications);

        // Add body if present
        if let Some(body) = &self.body {
            doc = doc.append(alloc.space()).append(
                docs![
                    alloc,
                    alloc.line(),
                    body.pretty(alloc).nest(2),
                    alloc.line()
                ]
                .braces(),
            );
        } else {
            doc = doc.append(";");
        }

        doc.group()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a ImplementationDecl
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        let mut doc = alloc.text("implementation");

        // Add attributes
        if !self.attributes.is_empty() {
            let attrs_doc = alloc.intersperse(&self.attributes, alloc.space());
            doc = doc.append(alloc.space()).append(attrs_doc);
        }

        // Add implementation signature
        doc = doc.append(alloc.space()).append(&self.signature);

        // Add implementation body
        doc = doc.append(alloc.space()).append(
            docs![
                alloc,
                alloc.line(),
                self.body.pretty(alloc).nest(2),
                alloc.line()
            ]
            .braces(),
        );

        doc.group()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a ActionDecl
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        let mut doc = alloc.text("action");

        doc = doc.append(alloc.space()).append(&self.mover);

        // Add attributes
        if !self.attributes.is_empty() {
            let attrs_doc = alloc.intersperse(&self.attributes, alloc.space());
            doc = doc.append(alloc.space()).append(attrs_doc);
        }

        // Add action signature
        doc = doc.append(alloc.space()).append(&self.signature);

        // Add action body
        doc = doc.append(alloc.space()).append(
            docs![
                alloc,
                alloc.line(),
                self.body.pretty(alloc).nest(2),
                alloc.line()
            ]
            .braces(),
        );

        doc.group()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a YieldInvariantDecl
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        let mut doc = alloc.text("yield invariant");

        // Add name
        doc = doc.append(alloc.space()).append(&self.name);

        // Add parameters
        let params_doc = alloc.intersperse(&self.params, ", ");
        doc = doc.append(params_doc.parens());

        // Add semicolon
        doc = doc.append(";");

        // Add invariants
        if !self.invariants.is_empty() {
            let invariants_doc = alloc.intersperse(
                self.invariants
                    .iter()
                    .map(|inv| docs![alloc, "invariant", alloc.space(), inv.pretty(alloc), ";"]),
                alloc.line(),
            );
            doc = doc.append(alloc.line()).append(invariants_doc);
        }

        doc.group()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a YieldProcedureDecl
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        let mut doc = alloc.text("yield procedure");

        // Add procedure signature
        doc = doc.append(alloc.space()).append(&self.signature);

        // Add specifications
        doc = doc.append(alloc.space()).append(&self.spec);

        // Add body if present
        if let Some(body) = &self.body {
            doc = doc.append(alloc.space()).append(
                docs![
                    alloc,
                    alloc.line(),
                    body.pretty(alloc).nest(2),
                    alloc.line()
                ]
                .braces(),
            );
        } else {
            doc = doc.append(";");
        }

        doc.group()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a Attribute
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        let mut doc = alloc.text("{:");

        // Add attribute name
        doc = doc.append(&self.name);

        // Add parameters if present
        if !self.params.is_empty() {
            let params_doc = alloc.intersperse(self.params.iter(), ", ");
            doc = doc.append(alloc.space()).append(params_doc);
        }

        // Close the attribute
        doc.append(alloc.text("}"))
    }
}
impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a Type
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        match self {
            Type::Int => alloc.text("int"),
            Type::Real => alloc.text("real"),
            Type::Bool => alloc.text("bool"),
            Type::Map(type_params, arg_types, return_type) => {
                let type_params_doc = if !type_params.is_empty() {
                    alloc
                        .intersperse(type_params, alloc.text(","))
                        .angles()
                        .append(alloc.space())
                } else {
                    alloc.nil()
                };

                let args_doc = alloc.intersperse(arg_types, ", ");

                type_params_doc
                    .append(args_doc.brackets())
                    .append(return_type.pretty(alloc))
            }
            Type::TypeVar(name) => alloc.text(name),
            Type::UserDefined(name, type_args) => {
                let name_doc = alloc.text(name);
                if type_args.is_empty() {
                    name_doc
                } else {
                    let args_doc = alloc.intersperse(type_args, ", ");
                    name_doc.append(args_doc.parens())
                }
            }
        }
    }
}

impl Expression {
    // TODO: Review definition
    fn precedence(&self) -> i32 {
        match self {
            Expression::Literal(_) | Expression::Var(_) => 100,
            Expression::UnaryOp(op, _) => match op {
                UnaryOp::Not => 90,
                UnaryOp::Neg => 90,
            },
            Expression::BinaryOp(op, _, _) => match op {
                BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => 80,
                BinaryOp::Add | BinaryOp::Sub => 70,
                BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => 60,
                BinaryOp::Eq | BinaryOp::Neq => 50,
                BinaryOp::And => 40,
                BinaryOp::Or => 30,
                BinaryOp::Implies => 20,
                BinaryOp::Iff => 10,
                BinaryOp::Pow => todo!(),
                BinaryOp::Concat => todo!(),
            },
            Expression::FunctionCall(_, _)
            | Expression::Field(_, _)
            | Expression::MapSelect(_, _) => 95,
            Expression::Old(_) => 95,
            Expression::If(_, _, _) => 5,
            Expression::Quantifier(_, _, _, _) => 1,
            Expression::Let(_, _, _, _) => 1,
            _ => 0,
        }
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a BinaryOp
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D> {
        let op_str = match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",
            BinaryOp::Eq => "==",
            BinaryOp::Neq => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::Le => "<=",
            BinaryOp::Gt => ">",
            BinaryOp::Ge => ">=",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
            BinaryOp::Implies => "==>",
            BinaryOp::Iff => "<===>",
            BinaryOp::Pow => "**",
            BinaryOp::Concat => "++",
        };
        alloc.text(op_str)
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a UnaryOp
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D> {
        let op_str = match self {
            UnaryOp::Not => "!",
            UnaryOp::Neg => "-",
        };
        alloc.text(op_str)
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a Expression
where
    D::Doc: Clone,
{
    fn pretty(self, allocator: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        match self {
            Expression::Literal(lit) => lit.pretty(allocator),
            Expression::Var(name) => allocator.text(name),
            Expression::UnaryOp(op, expr) => {
                let expr_doc = if expr.precedence() < self.precedence() {
                    expr.pretty(allocator).parens()
                } else {
                    expr.pretty(allocator)
                };
                docs![allocator, op, expr_doc]
            }
            Expression::BinaryOp(op, left, right) => {
                let left_doc = if left.precedence() < self.precedence() {
                    left.pretty(allocator).parens()
                } else {
                    left.pretty(allocator)
                };
                // TODO: Handle associativity correctly.
                let right_doc = if right.precedence() <= self.precedence() {
                    right.pretty(allocator).parens()
                } else {
                    right.pretty(allocator)
                };
                docs![
                    allocator,
                    left_doc,
                    allocator.space(),
                    op,
                    allocator.space(),
                    right_doc
                ]
                .group()
            }
            Expression::FunctionCall(name, args) => {
                let args_doc = allocator.intersperse(args, ", ");
                docs![allocator, name, args_doc.parens(),].group()
            }
            Expression::Field(expr, field) => {
                docs![allocator, &**expr, ".", field]
            }
            Expression::MapSelect(map, indexes) => {
                let indexes_doc = allocator.intersperse(indexes, ", ");
                docs![allocator, &**map, indexes_doc.brackets()].group()
            }

            Expression::BvExtract(expr, high, low) => {
                let expr_doc = if expr.precedence() < self.precedence() {
                    expr.pretty(allocator).parens()
                } else {
                    expr.pretty(allocator)
                };
                expr_doc
                    .append(docs![allocator, &**high, ":", &**low,].brackets())
                    .group()
            }

            Expression::MapUpdate(map, indexes, value) => {
                let map_doc = if map.precedence() < self.precedence() {
                    map.pretty(allocator).parens()
                } else {
                    map.pretty(allocator)
                };
                let indexes_doc = allocator.intersperse(indexes, ", ");

                map_doc
                    .append(docs![allocator, indexes_doc, ":=", &**value].brackets())
                    .group()
            }
            Expression::Old(expr) => {
                docs![allocator, "old(", &**expr, ")"]
            }
            Expression::Quantifier(quantifier, vars, triggers, body) => {
                let quantifier_doc = match quantifier {
                    Quantifier::Forall => "forall",
                    Quantifier::Exists => "exists",
                };
                let vars_doc = allocator.intersperse(
                    vars.iter()
                        .map(|(name, ty)| docs![allocator, name, ":", allocator.space(), ty]),
                    ", ",
                );
                let triggers_doc = if !triggers.is_empty() {
                    docs![
                        allocator,
                        allocator.space(),
                        allocator.intersperse(triggers, ", ").braces(),
                    ]
                } else {
                    allocator.nil()
                };
                docs![
                    allocator,
                    quantifier_doc,
                    allocator.space(),
                    vars_doc,
                    triggers_doc,
                    "::",
                    allocator.space(),
                    &**body
                ]
                .parens()
                .nest(2)
                .group()
            }
            Expression::IntCast(expr) => {
                docs![allocator, "int(", &**expr, ")"]
            }
            Expression::RealCast(expr) => {
                docs![allocator, "real(", &**expr, ")"]
            }
            Expression::Lambda(_, _) => todo!(),
            Expression::If(cond, then_expr, else_expr) => docs![
                allocator,
                "if",
                allocator.space(),
                &**cond,
                allocator.space(),
                "then",
                allocator.space(),
                &**then_expr,
                allocator.space(),
                "else",
                allocator.space(),
                &**else_expr
            ]
            .nest(2)
            .group(),
            Expression::Rounding => todo!(),
            Expression::Is(expr, type_name) => {
                let expr_doc = if expr.precedence() < self.precedence() {
                    expr.pretty(allocator).parens()
                } else {
                    expr.pretty(allocator)
                };
                docs![allocator, expr_doc, " is ", type_name,].group()
            }

            Expression::Let(vars, exprs, attrs, body) => {
                let var_docs = vars
                    .iter()
                    .map(|(attrs, nm)| allocator.intersperse(attrs, " ").append(nm));
                docs![
                    allocator,
                    "var",
                    allocator.intersperse(var_docs, ", "),
                    ":=",
                    allocator.intersperse(exprs, ", "),
                    ";",
                    allocator.intersperse(attrs, " "),
                    allocator.line(),
                    &**body
                ]
                .nest(2)
                .group()
            }
        }
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a Literal
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D> {
        match self {
            Literal::Int(value) => alloc.text(value.to_string()),
            Literal::Real(value) => alloc.text(value.to_string()),
            Literal::Bool(value) => alloc.text(if *value { "true" } else { "false" }),
            Literal::BitVector(value, size) => {
                use std::fmt::Write;
                let mut s = String::new();
                write!(s, "{}bv{}", value, size).unwrap();
                alloc.text(s)
            }
            Literal::String(value) => {
                let escaped = value.replace('\\', "\\\\").replace('"', "\\\"");
                alloc.text(format!("\"{}\"", escaped))
            }
            Literal::Float(value) => alloc.text(format!("{:e}", value)),
        }
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a Trigger
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        alloc.intersperse(&self.0, ", ").braces()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a FormalArg
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        match self {
            FormalArg::Anon(typ) => typ.pretty(alloc),
            FormalArg::Named(name, typ) => docs![alloc, name, ":", alloc.space(), typ],
        }
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a Signature
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        let mut doc = alloc.text(&self.name);

        // Add type parameters if present
        if !self.type_params.is_empty() {
            let type_params_doc =
                alloc.intersperse(self.type_params.iter().map(|tp| alloc.text(&tp.name)), ", ");
            doc = doc.append(type_params_doc.angles());
        }

        // Add parameters
        let params_doc = alloc.intersperse(&self.params, ", ");
        doc = doc.append(params_doc.parens());

        // Add return type if present
        if let Some(returns) = &self.returns {
            doc = doc
                .append(alloc.space())
                .append("returns")
                .append(alloc.space())
                .append(returns.pretty(alloc).parens());
        }

        doc.group()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a ImplBlock
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        let mut docs = Vec::new();

        // Local variable declarations
        if !self.local_vars.is_empty() {
            let vars_doc = docs![
                alloc,
                "var",
                alloc.space(),
                alloc.intersperse(
                    self.local_vars.iter().map(|(name, typ)| {
                        docs![
                            alloc,
                            alloc.text(name),
                            ":",
                            alloc.space(),
                            typ.pretty(alloc)
                        ]
                    }),
                    ", "
                ),
                ";"
            ];
            docs.push(vars_doc);
        }

        // Statements
        let statements_doc = alloc.intersperse(&self.statements, alloc.line());
        docs.push(statements_doc);

        docs![
            alloc,
            alloc.intersperse(docs, alloc.line()).nest(2),
            alloc.line()
        ]
        .braces()
        .group()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a Requires
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        docs![
            alloc,
            alloc.text(if self.free { "free " } else { "" }),
            alloc.text("requires"),
            alloc.space(),
            self.expression.pretty(alloc),
            ";"
        ]
        .group()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a Ensures
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        docs![
            alloc,
            alloc.text(if self.free { "free " } else { "" }),
            alloc.text("ensures"),
            alloc.space(),
            &self.expression,
            ";"
        ]
        .group()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a TypeVariable
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        alloc.text(&self.name)
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a Mover
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        let mover_str = match self {
            Mover::Atomic => "atomic",
            Mover::Both => "both",
            Mover::Left => "left",
            Mover::Right => "right",
        };
        alloc.text(mover_str)
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a Specifications
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        let mut docs = Vec::new();

        // Requires clauses
        if !self.requires.is_empty() {
            docs.push(alloc.intersperse(&self.requires, alloc.line()));
        }

        // Modifies clause
        if !self.modifies.is_empty() {
            let modifies_doc = docs![
                alloc,
                "modifies",
                alloc.space(),
                alloc.intersperse(&self.modifies, ", "),
                ";"
            ];
            docs.push(modifies_doc);
        }

        // Ensures clauses
        if !self.ensures.is_empty() {
            docs.push(alloc.intersperse(&self.ensures, alloc.line()));
        }

        alloc.intersperse(docs, alloc.line()).group()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a YieldSpecifications
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        let mut docs = Vec::new();

        // Requires clauses
        if !self.requires.is_empty() {
            docs.push(alloc.intersperse(&self.requires, alloc.line()));
        }

        // Modifies clause
        if !self.modifies.is_empty() {
            let modifies_doc = docs![
                alloc,
                "modifies",
                alloc.space(),
                alloc.intersperse(&self.modifies, ", "),
                ";"
            ];
            docs.push(modifies_doc);
        }

        // Ensures clauses
        if !self.ensures.is_empty() {
            docs.push(alloc.intersperse(&self.ensures, alloc.line()));
        }

        // Refines clauses
        if !self.refines.is_empty() {
            let refines_doc = docs![
                alloc,
                "refines",
                alloc.space(),
                alloc.intersperse(&self.refines, ", "),
                ";"
            ];
            docs.push(refines_doc);
        }

        alloc.intersperse(docs, alloc.line()).group()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a Statement
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        match self {
            Statement::Block(block) => block.pretty(alloc),
            Statement::Assign(lhs, rhs) => docs![
                alloc,
                alloc.intersperse(lhs, ", "),
                alloc.space(),
                ":=",
                alloc.space(),
                alloc.intersperse(rhs, ", "),
                ";"
            ]
            .group(),
            Statement::Assert(attrs, expr) => docs![
                alloc,
                alloc.intersperse(attrs, alloc.space()),
                "assert",
                alloc.space(),
                expr.pretty(alloc),
                ";"
            ]
            .group(),
            Statement::Assume(attrs, expr) => docs![
                alloc,
                alloc.intersperse(attrs, alloc.space()),
                "assume",
                alloc.space(),
                expr.pretty(alloc),
                ";"
            ]
            .group(),
            Statement::Havoc(vars) => docs![
                alloc,
                "havoc",
                alloc.space(),
                alloc.intersperse(vars, ", "),
                ";"
            ]
            .group(),
            Statement::Call(name, args, returns) => {
                let mut doc = docs![alloc, "call", alloc.space(),];
                if !returns.is_empty() {
                    doc = docs![
                        alloc,
                        alloc.intersperse(returns, ", "),
                        alloc.space(),
                        alloc.text(":="),
                        alloc.space()
                    ];
                }
                doc = doc
                    .append(name)
                    .append(docs![alloc, alloc.intersperse(args, ", ")].parens())
                    .append(";");
                doc.group()
            }
            Statement::If(cond, then_block, else_block) => {
                let mut doc = docs![
                    alloc,
                    "if",
                    alloc.space(),
                    cond.as_ref().map_or(alloc.text("(*)"), |c| c.pretty(alloc)),
                    alloc.space(),
                    then_block.pretty(alloc)
                ];
                if let Some(else_b) = else_block {
                    doc = doc
                        .append(alloc.space())
                        .append(alloc.text("else"))
                        .append(alloc.space())
                        .append(else_b.pretty(alloc));
                }
                doc.group()
            }
            Statement::While(cond, invariants, body) => {
                let mut doc = docs![
                    alloc,
                    "while",
                    alloc.space(),
                    cond.as_ref().map_or(alloc.text("*"), |c| c.pretty(alloc)),
                    alloc.space(),
                ];
                if !invariants.is_empty() {
                    doc = doc
                        .append(alloc.intersperse(&*invariants, alloc.line()))
                        .append(alloc.line());
                }
                doc.append(body).group()
            }
            Statement::Break(label) => docs![
                alloc,
                "break",
                label
                    .as_ref()
                    .map_or(alloc.nil(), |l| alloc.space().append(alloc.text(l))),
                ";"
            ]
            .group(),
            Statement::Goto(labels) => docs![
                alloc,
                "goto",
                alloc.space(),
                alloc.intersperse(&*labels, ", "),
                ";"
            ]
            .group(),
            Statement::Par => alloc.text("par TODO").append(";"),
            Statement::Return => alloc.text("return TODO?").append(";"),
            Statement::Unpack((name, args), expr) => docs![
                alloc,
                name,
                docs![alloc, alloc.intersperse(&*args, ", ")].parens(),
                alloc.space(),
                ":=",
                alloc.space(),
                expr,
                ";"
            ]
            .group(),
        }
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a BasicBlock
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        let mut doc = docs![alloc, &self.label, ":", alloc.line()];

        // Format statements
        if !self.statements.is_empty() {
            let stmts_doc = alloc.intersperse(&self.statements, alloc.line());
            doc = doc.append(stmts_doc).append(alloc.line());
        }

        // Format terminating statement if present
        if let Some(term) = &self.term {
            doc = doc.append(term.pretty(alloc)).append(alloc.line());
        }

        // Group and nest the entire block
        doc.nest(2).group()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a Lhs
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        match self {
            Lhs::Simple(name) => alloc.text(name),
            Lhs::Map(lhs, indexes) => {
                let lhs_doc = lhs.pretty(alloc);
                let indexes_doc = alloc.intersperse(&**indexes, ", ");
                docs![alloc, lhs_doc, indexes_doc.brackets().group()]
            }
            Lhs::Field(lhs, field) => docs![alloc, &**lhs, "->", field],
        }
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a Invariant
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D> {
        match self {
            Invariant::Expression(expr) => docs![alloc, "invariant", alloc.space(), expr, ";"],
            Invariant::Call(name, args) => {
                let args_doc = alloc.intersperse(&**args, ",");
                docs![
                    alloc,
                    "invariant",
                    alloc.space(),
                    name,
                    "(",
                    args_doc,
                    ")",
                    ";"
                ]
                .group()
            }
        }
    }
}