use pretty::{docs, DocAllocator, Pretty};

use crate::ast::*;

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a Program
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        alloc.intersperse(&self.declarations, alloc.hardline())
    }
}

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

        let attrs = alloc.intersperse(self.attributes.iter(), alloc.space());
        // Add attributes
        doc = doc.append(alloc.space()).append(attrs);

        // Add 'unique' if applicable
        if self.unique {
            doc = doc.append(alloc.space()).append("unique");
        }

        // Add variables
        let vars_doc = alloc.intersperse(
            self.vars
                .iter()
                .map(|(name, typ)| alloc.text(name).append(": ").append(typ)),
            ", ",
        );
        doc = doc.append(alloc.space()).append(vars_doc);

        // Add semicolon if there are no axioms
        if self.axioms.is_empty() {
            doc = doc.append(";");
        } else {
            // Add 'uses' block with axioms
            doc = doc.append(" uses ");

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
            doc = doc.append("hideable ");
        }

        doc = doc.append("axiom");

        // Add attributes
        let attrs_doc = alloc.intersperse(&self.attributes, alloc.space());
        doc = doc.append(alloc.space()).append(attrs_doc);

        // Add expression
        doc = doc
            .append(alloc.space())
            .append(&self.expression)
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
                    let params_doc = alloc.intersperse(params.iter(), " ");
                    name_doc.append(" ").append(params_doc)
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
        if self.body.is_none() {
            doc = doc.append(";");
        }

        // Add specifications
        doc = doc.append(alloc.space()).append(&self.specifications);

        // Add body if present
        if let Some(body) = &self.body {
            doc = doc.append(alloc.space()).append(docs![
                alloc,
                alloc.line(),
                body.pretty(alloc).nest(2),
                alloc.line()
            ]);
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
        doc = doc.append(alloc.space()).append(docs![
            alloc,
            alloc.line(),
            self.body.pretty(alloc).nest(2),
            alloc.line()
        ]);

        doc.group()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a ActionDecl
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        let mover = match &self.mover {
            None => alloc.nil(),
            Some(m) => m.pretty(alloc),
        };
        let mut doc = docs![alloc, mover, " ", "action"];

        // Add attributes
        if !self.attributes.is_empty() {
            let attrs_doc = alloc.intersperse(&self.attributes, alloc.space());
            doc = doc.append(alloc.space()).append(attrs_doc);
        }

        // Add action signature
        doc = doc.append(alloc.space()).append(&self.signature);

        doc = doc.append(&self.specification);
        // Add action body
        doc = doc.append(alloc.space()).append(docs![
            alloc,
            alloc.line(),
            self.body.pretty(alloc).nest(2),
            alloc.line()
        ]);

        doc.group()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a ActionSpecifications
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        let mut docs = Vec::new();

        // Requires clauses
        if !self.requires.is_empty() {
            docs.push(
                alloc.intersperse(
                    self.requires
                        .iter()
                        .map(|r| docs![alloc, "requires ", r, ";"]),
                    alloc.line(),
                ),
            );
        }

        // TODO simplify.
        // Require call clauses
        if !self.requires_call.is_empty() {
            let requires_doc = docs![
                alloc,
                alloc.intersperse(
                    self.requires_call.iter().map(|rc| docs![
                        alloc,
                        "requires call ",
                        &rc.0,
                        alloc.intersperse(&rc.1, ", ").parens()
                    ]),
                    "; "
                ),
                ";"
            ];
            docs.push(requires_doc);
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

        // asserts clauses
        if !self.asserts.is_empty() {
            docs.push(
                alloc.intersperse(
                    self.asserts
                        .iter()
                        .map(|r| docs![alloc, "asserts ", r, ";"]),
                    alloc.line(),
                ),
            );
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

        // Refines clauses
        if !self.creates.is_empty() {
            let refines_doc = docs![
                alloc,
                "creates",
                alloc.space(),
                alloc.intersperse(&self.creates, ", "),
                ";"
            ];
            docs.push(refines_doc);
        }

        alloc.intersperse(docs, alloc.line()).group()
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

        doc = doc.append(alloc.concat(self.attrs.iter().map(|a| a.pretty(alloc).append(" "))));
        // Add procedure signature
        doc = doc.append(alloc.space()).append(&self.signature);

        // Add body if present
        if let Some(body) = &self.body {
            // Add specifications
            doc = doc.append(alloc.space()).append(&self.spec);
            doc = doc.append(alloc.space()).append(docs![
                alloc,
                alloc.line(),
                body.pretty(alloc).nest(2),
                alloc.line()
            ]);
        } else {
            doc = doc.append(";");
            // Add specifications
            doc = doc.append(alloc.space()).append(&self.spec);
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
                    let arg_docs = type_args.iter().map(|arg| arg.pretty(alloc).parens());
                    let args_doc = alloc.intersperse(arg_docs, " ");
                    name_doc.append(" ").append(args_doc)
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
                BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => 50,
                BinaryOp::Eq | BinaryOp::Neq => 50,
                BinaryOp::And => 30,
                BinaryOp::Or => 30,
                BinaryOp::Implies => 20,
                BinaryOp::Iff => 10,
                BinaryOp::Pow => 90,
                BinaryOp::Concat => 70,
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

enum Associativity {
    Left,
    Right,
    None,
}

impl BinaryOp {
    fn associativity(self) -> Associativity {
        use BinaryOp::*;
        match self {
            Mul | Add | Div | Sub => Associativity::Left,
            Eq => Associativity::None,
            Implies => Associativity::Right,
            _ => Associativity::Left,
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
            BinaryOp::Mod => "mod",
            BinaryOp::Eq => "==",
            BinaryOp::Neq => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::Le => "<=",
            BinaryOp::Gt => ">",
            BinaryOp::Ge => ">=",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
            BinaryOp::Implies => "==>",
            BinaryOp::Iff => "<==>",
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
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        match self {
            Expression::Literal(lit) => lit.pretty(alloc),
            Expression::Var(name) => alloc.text(name),
            Expression::UnaryOp(op, expr) => {
                let expr_doc = if expr.precedence() < self.precedence() {
                    expr.pretty(alloc).parens()
                } else {
                    expr.pretty(alloc)
                };
                docs![alloc, op, expr_doc]
            }
            Expression::BinaryOp(op, left, right) => {
                let (left_prec, right_prec) = match op.associativity() {
                    Associativity::Left => (self.precedence(), self.precedence() + 1),
                    Associativity::Right => (self.precedence() + 1, self.precedence()),
                    Associativity::None => (self.precedence() + 1, self.precedence() + 1),
                };
                let left_doc = if left.precedence() < left_prec {
                    left.pretty(alloc).parens()
                } else {
                    left.pretty(alloc)
                };

                let right_doc = if right.precedence() < right_prec {
                    right.pretty(alloc).parens()
                } else {
                    right.pretty(alloc)
                };
                docs![alloc, left_doc, alloc.space(), op, alloc.space(), right_doc].group()
            }
            Expression::FunctionCall(name, args) => {
                let args_doc = alloc.intersperse(args, ", ");
                docs![alloc, name, args_doc.parens(),].group()
            }
            Expression::Field(expr, field) => {
                docs![alloc, &**expr, "->", field]
            }
            Expression::MapSelect(map, indexes) => {
                let indexes_doc = alloc.intersperse(indexes, ", ");
                docs![alloc, &**map, indexes_doc.brackets()].group()
            }

            Expression::BvExtract(expr, high, low) => {
                let expr_doc = if expr.precedence() < self.precedence() {
                    expr.pretty(alloc).parens()
                } else {
                    expr.pretty(alloc)
                };
                expr_doc
                    .append(docs![alloc, &**high, ":", &**low,].brackets())
                    .group()
            }

            Expression::MapUpdate(map, indexes, value) => {
                let map_doc = if map.precedence() < self.precedence() {
                    map.pretty(alloc).parens()
                } else {
                    map.pretty(alloc)
                };
                let indexes_doc = alloc.intersperse(indexes, ", ");

                map_doc
                    .append(docs![alloc, indexes_doc, ":=", &**value].brackets())
                    .group()
            }
            Expression::Old(expr) => {
                docs![alloc, "old(", &**expr, ")"]
            }
            Expression::Quantifier(quantifier, vars, triggers, body) => {
                let quantifier_doc = match quantifier {
                    Quantifier::Forall => "forall",
                    Quantifier::Exists => "exists",
                };
                let vars_doc = alloc.intersperse(
                    vars.iter()
                        .map(|(name, ty)| docs![alloc, name, ":", alloc.space(), ty]),
                    ", ",
                );
                let triggers_doc = if !triggers.is_empty() {
                    docs![alloc, alloc.space(), alloc.intersperse(triggers, " "),]
                } else {
                    alloc.nil()
                };
                docs![
                    alloc,
                    quantifier_doc,
                    alloc.space(),
                    vars_doc,
                    "::",
                    triggers_doc,
                    alloc.space(),
                    &**body
                ]
                .parens()
                .nest(2)
                .group()
            }
            Expression::IntCast(expr) => {
                docs![alloc, "int(", &**expr, ")"]
            }
            Expression::RealCast(expr) => {
                docs![alloc, "real(", &**expr, ")"]
            }
            Expression::Lambda(vars, expr) => docs![
                alloc,
                "lambda ",
                alloc.intersperse(vars, ", "),
                "::",
                &**expr
            ]
            .parens(),
            Expression::If(cond, then_expr, else_expr) => docs![
                alloc,
                "if",
                alloc.space(),
                &**cond,
                alloc.space(),
                "then",
                alloc.space(),
                &**then_expr,
                alloc.space(),
                "else",
                alloc.space(),
                &**else_expr
            ]
            .nest(2)
            .group(),
            Expression::Rounding(r) => match r {
                Rounding::RNE => alloc.text("RNE"),
                Rounding::RTN => alloc.text("RTN"),
                Rounding::RNA => alloc.text("RNA"),
                Rounding::RTP => alloc.text("RTP"),
                Rounding::RTZ => alloc.text("RTZ"),
            },
            Expression::Is(expr, type_name) => {
                let expr_doc = if expr.precedence() < self.precedence() {
                    expr.pretty(alloc).parens()
                } else {
                    expr.pretty(alloc)
                };
                docs![alloc, expr_doc, " is ", type_name,].group()
            }

            Expression::Let(vars, exprs, attrs, body) => {
                let var_docs = vars
                    .iter()
                    .map(|(attrs, nm)| alloc.intersperse(attrs, " ").append(nm));
                docs![
                    alloc,
                    "var ",
                    alloc.intersperse(var_docs, ", "),
                    ":=",
                    alloc.intersperse(exprs, ", "),
                    ";",
                    alloc.intersperse(attrs, " "),
                    alloc.line(),
                    &**body
                ]
                .parens()
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
                // let escaped = value.replace('\\', "\\\\").replace('"', "\\\"");
                alloc.text(format!("{value}"))
            }
            Literal::Float(value) => alloc.text(format!("{}", value)),
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
            FormalArg::Named(attrs, name, typ) => docs![
                alloc,
                alloc.concat(attrs.iter().map(|a| a.pretty(alloc).append(" "))),
                name,
                ":",
                alloc.space(),
                typ
            ],
        }
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a Variable
where
    D::Doc: Clone,
{
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        docs![
            alloc,
            &self.name,
            &self.typ,
            match &self.where_clause {
                Some(wc) => wc.pretty(alloc),
                None => alloc.nil(),
            }
        ]
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
                .append(alloc.intersperse(returns, ", ").parens());
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
                    self.local_vars
                        .iter()
                        .map(|(name, typ)| { docs![alloc, name, ": ", typ] }),
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
            docs.push(
                alloc.intersperse(
                    self.requires
                        .iter()
                        .map(|r| docs![alloc, "requires ", r, ";"]),
                    alloc.line(),
                ),
            );
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
            docs.push(
                alloc.intersperse(
                    self.ensures
                        .iter()
                        .map(|r| docs![alloc, "ensures ", r, ";"]),
                    alloc.line(),
                ),
            );
        }

        // Refines clauses
        if !self.refines.is_empty() {
            let refines_doc = docs![
                alloc,
                "refines",
                alloc.space(),
                alloc.intersperse(self.refines.iter().map(|r| &r.0), ", "),
                ";"
            ];
            docs.push(refines_doc);
        }

        // TODO simplify.
        // Require call clauses
        if !self.req_calls.is_empty() {
            let requires_doc = docs![
                alloc,
                alloc.intersperse(
                    self.req_calls.iter().map(|rc| docs![
                        alloc,
                        "requires call ",
                        &rc.0,
                        alloc.intersperse(&rc.1, ", ").parens()
                    ]),
                    "; "
                ),
                ";"
            ];
            docs.push(requires_doc);
        }

        // ensures call clauses
        if !self.ens_calls.is_empty() {
            let ensures_doc = docs![
                alloc,
                alloc.intersperse(
                    self.ens_calls.iter().map(|rc| docs![
                        alloc,
                        "ensures call ",
                        &rc.0,
                        alloc.intersperse(&rc.1, ", ").parens()
                    ]),
                    "; "
                ),
                ";"
            ];
            docs.push(ensures_doc);
        }

        // preservess call clauses
        if !self.preserves.is_empty() {
            let preserves_doc = docs![
                alloc,
                alloc.intersperse(
                    self.preserves.iter().map(|rc| docs![
                        alloc,
                        "preserves call ",
                        &rc.0,
                        alloc.intersperse(&rc.1, ", ").parens()
                    ]),
                    "; "
                ),
                ";"
            ];
            docs.push(preserves_doc);
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
                "assert",
                " ",
                alloc.intersperse(attrs, alloc.space()),
                " ",
                expr,
                ";"
            ]
            .group(),
            Statement::Assume(attrs, expr) => docs![
                alloc,
                "assume",
                " ",
                alloc.intersperse(attrs, alloc.space()),
                " ",
                expr,
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
            Statement::Call(returns, name, args) => {
                let mut doc = docs![alloc, "call", alloc.space(),];
                if !returns.is_empty() {
                    doc = docs![alloc, doc, alloc.intersperse(returns, ", "), " := ",];
                }
                doc = doc
                    .append(name)
                    .append(alloc.intersperse(args, ", ").parens())
                    .append(";");
                doc.group()
            }
            Statement::If(cond, then_block, else_block) => {
                let mut doc = docs![
                    alloc,
                    "if",
                    alloc.space(),
                    cond.as_ref()
                        .map_or(alloc.text("*"), |c| c.pretty(alloc))
                        .parens(),
                    alloc.space(),
                    then_block.pretty(alloc)
                ];
                if let Some(else_b) = else_block {
                    doc = doc
                        .append(alloc.text(" else "))
                        .append(else_b.pretty(alloc));
                }
                doc.group()
            }
            Statement::While(cond, invariants, body) => {
                let mut doc = docs![
                    alloc,
                    "while",
                    alloc.space(),
                    cond.as_ref()
                        .map_or(alloc.text("*"), |c| c.pretty(alloc))
                        .parens(),
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
            Statement::Par(calls) => {
                let mut doc = alloc.text("par ");

                let calls = calls.iter().map(|call| {
                    let mut one_call = alloc.nil();

                    if let Some(lhs) = &call.0 {
                        one_call = one_call.append(alloc.intersperse(lhs, ", "));
                        one_call = one_call.append(" := ");
                    }

                    one_call = one_call
                        .append(&call.1)
                        .append(alloc.intersperse(&call.2, ", ").parens());

                    one_call
                });

                doc = doc.append(alloc.intersperse(calls, " | ")).append(";");

                doc
            }
            Statement::Return => alloc.text("return;"),
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
            Invariant::Expression(attrs, expr) => docs![
                alloc,
                "invariant ",
                alloc.intersperse(attrs, alloc.space()),
                expr,
                ";"
            ],
            Invariant::Call(name, args) => {
                let args_doc = alloc.intersperse(&**args, ",");
                docs![
                    alloc,
                    "invariant call",
                    alloc.space(),
                    name,
                    args_doc.parens(),
                    ";"
                ]
                .group()
            }
        }
    }
}
