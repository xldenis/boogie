use pretty::{DocAllocator, Pretty};

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
                    .append(alloc.text(":"))
                    .append(alloc.space())
                    .append(typ.pretty(alloc))
            }),
            alloc.text(",").append(alloc.space()),
        );
        doc = doc.append(alloc.space()).append(vars_doc);

        // Add semicolon if there are no axioms
        if self.axioms.is_empty() {
            doc = doc.append(alloc.text(";"));
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
            .append(alloc.text(";"));

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
                    let params_doc =
                        alloc.intersperse(params.iter(), alloc.text(",").append(alloc.space()));
                    name_doc.append(params_doc.angles())
                }
            }),
            alloc.text(",").append(alloc.space()),
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

        doc.append(alloc.text(";")).group()
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
            let params_doc = alloc.intersperse(
                self.type_params.iter(),
                alloc.text(",").append(alloc.space()),
            );
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
                        alloc.text(",").append(alloc.space()),
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

        doc.append(alloc.text(";")).group()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &FunctionDecl {
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        todo!()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &ProcedureDecl {
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        todo!()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &ImplementationDecl {
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        todo!()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &ActionDecl {
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        todo!()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &YieldInvariantDecl {
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        todo!()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &YieldProcedureDecl {
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        todo!()
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
            let params_doc =
                alloc.intersperse(self.params.iter(), alloc.text(",").append(alloc.space()));
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

                let args_doc = alloc.intersperse(arg_types, alloc.text(",").append(alloc.space()));

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
                    let args_doc =
                        alloc.intersperse(type_args, alloc.text(",").append(alloc.space()));
                    name_doc.append(args_doc.parens())
                }
            }
        }
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a Expression
where
    D::Doc: Clone,
{
    fn pretty(self, allocator: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        todo!()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a FormalArg
where
    D::Doc: Clone,
{
    fn pretty(self, allocator: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        todo!()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a Signature
where
    D::Doc: Clone,
{
    fn pretty(self, allocator: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        todo!()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a ImplBlock
where
    D::Doc: Clone,
{
    fn pretty(self, allocator: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        todo!()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a Requires
where
    D::Doc: Clone,
{
    fn pretty(self, allocator: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        todo!()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a Ensures
where
    D::Doc: Clone,
{
    fn pretty(self, allocator: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        todo!()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a TypeVariable
where
    D::Doc: Clone,
{
    fn pretty(self, allocator: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        todo!()
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a Mover
where
    D::Doc: Clone,
{
    fn pretty(self, allocator: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        todo!()
    }
}
