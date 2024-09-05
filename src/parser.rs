use super::ast::*;
use crate::lexer::Token;
use chumsky::extra::Err;
use chumsky::{input::ValueInput, prelude::*};

pub fn program<'a, I>() -> impl Parser<'a, I, Program, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    declaration()
        .repeated()
        .collect()
        .map(|declarations| Program { declarations })
}

fn declaration<'a, I>() -> impl Parser<'a, I, Declaration, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    choice((
        const_decl().map(Declaration::Const),
        function_decl().map(Declaration::Function),
        axiom_decl().map(Declaration::Axiom),
        type_decl().map(Declaration::Type),
        datatype_decl().map(Declaration::Datatype),
        global_var_decl().map(Declaration::GlobalVar),
        procedure_decl().map(Declaration::Procedure),
        implementation_decl().map(Declaration::Implementation),
        action_decl().map(Declaration::Action),
        yield_invariant_decl().map(Declaration::YieldInvariant),
        yield_procedure_decl().map(Declaration::YieldProcedure),
    ))
}

fn const_decl<'a, I>() -> impl Parser<'a, I, ConstDecl, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let attributes = attribute().repeated().collect();
    let is_unique = just(Token::Unique).or_not().map(|x| x.is_some());

    (just(Token::Const)
        .ignore_then(attributes)
        .then(is_unique)
        .then(ids_type())
        .then(
            just(Token::Semicolon).to(None).or(just(Token::Uses)
                .ignore_then(just(Token::LeftBrace))
                .ignore_then(axiom_decl().repeated().collect::<Vec<_>>())
                .then_ignore(just(Token::RightBrace))
                .map(Some)),
        ))
    .map(|(((attrs, unique), vars), axioms)| ConstDecl {
        unique,
        vars,
        axioms: axioms.unwrap_or_default(),
        attributes: attrs,
    })
    .boxed()
}

fn function_decl<'a, I>() -> impl Parser<'a, I, FunctionDecl, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let attributes = attribute().repeated().collect();

    let block = expression().delimited_by(just(Token::LeftBrace), just(Token::RightBrace));

    let body = choice((
        block.map(|e| (Some(e), None)),
        uses().map(|u| (None, Some(u))),
        just(Token::Semicolon).map(|_| (None, None)),
    ));

    just(Token::Function)
        .ignore_then(attributes)
        .then(proc_signature())
        .then(body)
        .map(|((attrs, sig), (body, axioms))| {
            FunctionDecl {
                signature: sig,
                body,
                axioms,
                attributes: attrs, //         axioms: axioms.unwrap_or_default(),
            }
        })
        .boxed()
}

fn uses<'a, I>() -> impl Parser<'a, I, Vec<Axiom>, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    just(Token::Uses)
        .ignore_then(
            axiom_decl()
                .repeated()
                .collect()
                .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
        )
        .boxed()
}

fn axiom_decl<'a, I>() -> impl Parser<'a, I, Axiom, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let attributes = attribute().repeated().collect();
    let is_hideable = just(Token::Hideable).or_not().map(|x| x.is_some());

    is_hideable
        .then_ignore(just(Token::Axiom))
        .then(attributes)
        .then(expression())
        .then_ignore(just(Token::Semicolon))
        .map(|((hideable, attrs), expr)| Axiom {
            hideable,
            expression: expr,
            attributes: attrs,
        })
        .boxed()
}

fn ids_type<'a, I>() -> impl Parser<'a, I, Vec<(String, Type)>, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    ident()
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
        .then_ignore(just(Token::Colon))
        .then(type_expr())
        .map(|(ids, ty)| ids.into_iter().map(|i| (i, ty.clone())).collect::<Vec<_>>())
        .separated_by(just(Token::Comma))
        .collect::<Vec<Vec<(_, _)>>>()
        .map(|vs| vs.into_iter().flatten().collect::<Vec<_>>())
}

fn ident<'a, I>() -> impl Parser<'a, I, String, Err<Rich<'a, Token<'a>>>> + Clone
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    select! {
        Token::Ident(s) => s.into(),
        Token::Atomic => "atomic".into(),
        Token::Both => "both".into(),
        Token::Left => "left".into(),
        Token::Right => "right".into(),
        Token::Reveal => "reveal".into(),
        Token::Hide => "hide".into(),
        Token::Push => "push".into(),
        Token::Pop => "pop".into()
    }
    .labelled("identifier")
}

fn type_decl<'a, I>() -> impl Parser<'a, I, TypeDecl, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let type_param = ident().map(|name| TypeVariable { name });

    let type_params = type_param.repeated().collect();
    let attributes = attribute().repeated().collect();

    let name = ident().then(type_params);

    just(Token::Type)
        .ignore_then(attributes)
        .then(
            name.separated_by(just(Token::Comma))
                .collect::<Vec<(_, _)>>(),
        )
        .then(just(Token::Equals).ignore_then(type_expr()).or_not())
        .then_ignore(just(Token::Semicolon))
        .map(|((attrs, names), body)| TypeDecl {
            names,
            body,
            attributes: attrs,
        })
        .boxed()
}

fn datatype_decl<'a, I>() -> impl Parser<'a, I, DatatypeDecl, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let type_param = select! { Token::Ident(name) => TypeVariable { name: name.to_string() } };

    let type_params = type_param
        .separated_by(just(Token::Comma))
        .collect()
        .delimited_by(just(Token::LessThan), just(Token::GreaterThan))
        .or_not()
        .map(|params| params.unwrap_or_default());

    let field = attribute()
        .repeated()
        .ignore_then(ident())
        .then_ignore(just(Token::Colon))
        .then(type_expr())
        .map(|(name, typ)| Variable {
            name,
            typ,
            where_clause: None,
        });

    let constructor = ident()
        .then(
            field
                .separated_by(just(Token::Comma))
                .collect()
                .delimited_by(just(Token::LeftParen), just(Token::RightParen))
                .or_not()
                .map(|fields| fields.unwrap_or_default()),
        )
        .map(|(name, fields)| DatatypeConstructor { name, fields });

    let constructors = constructor
        .separated_by(just(Token::Comma))
        .collect()
        .delimited_by(just(Token::LeftBrace), just(Token::RightBrace));

    let attributes = attribute().repeated().collect();

    just(Token::Datatype)
        .ignore_then(attributes)
        .then(ident())
        .then(type_params)
        .then(constructors)
        // .then_ignore(just(Token::Semicolon))
        .map(
            |(((attrs, name), type_params), constructors)| DatatypeDecl {
                name,
                type_params,
                constructors,
                attributes: attrs,
            },
        )
        .boxed()
}

fn global_var_decl<'a, I>() -> impl Parser<'a, I, GlobalVarDecl, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let attributes = attribute().repeated().collect();

    let id_type = ident().then_ignore(just(Token::Colon)).then(type_expr());
    let where_clause = just(Token::Where).ignore_then(expression());

    just(Token::Var)
        .ignore_then(attributes)
        .then(id_type.then(where_clause.or_not()))
        .then_ignore(just(Token::Semicolon))
        .map(|(attrs, ((name, typ), where_))| GlobalVarDecl {
            name,
            typ,
            where_,
            attributes: attrs,
        })
        .boxed()
}

fn procedure_decl<'a, I>() -> impl Parser<'a, I, ProcedureDecl, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let attributes = attribute().repeated().collect();
    let is_pure = just(Token::Pure).or_not().map(|x| x.is_some());

    let proc_signature = proc_signature();

    is_pure
        .then_ignore(just(Token::Procedure))
        .then(attributes)
        .then(proc_signature)
        .then(
            just(Token::Semicolon)
                .ignore_then(spec().map(|s| (s, None)))
                .or(spec().then(block().map(|b| Some(b)))),
        )
        .map(
            |(((pure, attributes), signature), (specs, body))| ProcedureDecl {
                pure,
                signature,
                specifications: specs,
                body,
                attributes,
            },
        )
        .boxed()
}

fn implementation_decl<'a, I>() -> impl Parser<'a, I, ImplementationDecl, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let attributes = attribute().repeated().collect::<Vec<_>>();

    just(Token::Implementation)
        .ignore_then(attributes)
        .then(proc_signature())
        .then(block())
        .map(|((attributes, signature), body)| ImplementationDecl {
            signature,
            body,
            attributes,
        })
        .boxed()
}

fn mover<'a, I>() -> impl Parser<'a, I, Mover, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    choice((
        just(Token::Both).map(|_| Mover::Both),
        just(Token::Atomic).map(|_| Mover::Atomic),
        just(Token::Left).map(|_| Mover::Left),
        just(Token::Right).map(|_| Mover::Right),
    ))
}

fn action_decl<'a, I>() -> impl Parser<'a, I, ActionDecl, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let mover_qualifier = mover();

    let attributes = attribute().repeated().collect::<Vec<_>>();
    let is_async = just(Token::Async).or_not().map(|x| x.is_some());
    let is_pure = just(Token::Pure).or_not().map(|x| x.is_some());
    let mover_type = mover_qualifier.or_not();

    (is_pure
        .ignored()
        .then(is_async)
        .then(mover_type)
        .then_ignore(just(Token::Action))
        .then(attributes)
        .then(proc_signature())
        .then(
            just(Token::Semicolon)
                .ignore_then(action_spec().map(|s| (s, None)))
                .or(action_spec().then(block().map(|b| Some(b)))),
        ))
    .map(
        |(((((is_pure, async_), mover), attrs), signature), (specs, _))| ActionDecl {
            // is_async,
            mover,
            signature,
            specification: specs,
            body: ImplBlock {
                local_vars: Default::default(),
                statements: Default::default(),
            },
            attributes: attrs,
        },
    )
    .boxed()
}

fn yield_invariant_decl<'a, I>() -> impl Parser<'a, I, YieldInvariantDecl, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let attributes = attribute().repeated().collect::<Vec<Attribute>>();

    let invariant_ = just(Token::Invariant)
        .then_ignore(attribute().repeated())
        .ignore_then(expression())
        .then_ignore(just(Token::Semicolon));

    just(Token::Yield)
        .then(just(Token::Invariant))
        .ignore_then(attributes)
        .then(ident())
        .then(formal_args().delimited_by(just(Token::LeftParen), just(Token::RightParen)))
        .then_ignore(just(Token::Semicolon))
        .then(invariant_.repeated().collect())
        .map(
            |(((_attrs, name), params), invariants)| YieldInvariantDecl {
                name,
                params,
                invariants,
                // attributes: attrs,
            },
        )
        .boxed()
}

fn yield_procedure_decl<'a, I>() -> impl Parser<'a, I, YieldProcedureDecl, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let attributes = attribute().repeated().collect::<Vec<_>>();
    let mover_type = mover().or_not();

    just(Token::Yield)
        .ignore_then(mover_type)
        .ignore_then(just(Token::Procedure))
        .ignore_then(attributes)
        .then(proc_signature())
        .then(
            (just(Token::Semicolon)
                .ignore_then(yield_spec())
                .map(|spec| (None, spec)))
            .or(yield_spec()
                .then(block())
                .map(|(spc, body)| (Some(body), spc))),
        )
        .map(|((attrs, sig), (body, spec))| YieldProcedureDecl {
            attrs,
            signature: sig,
            spec,
            body,
        })
        .boxed()
}

//
fn formal_args<'a, I>() -> impl Parser<'a, I, Vec<FormalArg>, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    formal_args_inner(expression())
}

fn formal_args_inner<'a, I>(
    expr: impl Parser<'a, I, Expression, Err<Rich<'a, Token<'a>>>> + Clone,
) -> impl Parser<'a, I, Vec<FormalArg>, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let formal_arg = attribute_inner(expr.clone())
        .repeated()
        .collect::<Vec<_>>()
        .then(ident())
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
        .then_ignore(just(Token::Colon))
        .or_not()
        .then(type_expr())
        .then_ignore(just(Token::Where).then(expr.clone()).or_not())
        .map(|(ids, ty)| {
            let ids = ids.unwrap_or_default();
            if ids.is_empty() {
                vec![FormalArg::Anon(ty)]
            } else {
                ids.into_iter()
                    .map(|(attrs, id)| FormalArg::Named(attrs, id, ty.clone()))
                    .collect()
            }
        });

    formal_arg
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
        .map(|v: Vec<_>| v.into_iter().flatten().collect())
}

fn proc_signature<'a, I>() -> impl Parser<'a, I, Signature, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let returns = just(Token::Returns)
        .ignore_then(formal_args().delimited_by(just(Token::LeftParen), just(Token::RightParen)))
        .map(|_| ());

    let anon_return = just(Token::Colon)
        .ignore_then(type_expr())
        .map(FormalArg::Anon)
        .map(|_| ());

    let ty_params = ident()
        .map(|i| TypeVariable { name: i })
        .separated_by(just(Token::Comma))
        .collect()
        .delimited_by(just(Token::LessThan), just(Token::GreaterThan));
    ident()
        .then(ty_params.or_not().map(|tys| tys.unwrap_or_default()))
        .then(formal_args().delimited_by(just(Token::LeftParen), just(Token::RightParen)))
        .then(
            returns.or(anon_return).or_not(), // .map(|a| a.flatten())
        )
        .map(|(((name, type_params), params), _returns)| Signature {
            name,
            type_params,
            params,
            returns: None,
        })
}

fn yield_spec<'a, I>() -> impl Parser<'a, I, YieldSpecifications, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let function_call = ident().then(
        expression()
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
    );

    enum YieldSpec {
        Refn((String, Option<String>)),
        Requires(Expression),
        Ensures(Expression),
        Modifies(Vec<String>),
        ReqCall((String, Vec<Expression>)),
        EnsCall((String, Vec<Expression>)),
        Preserves((String, Vec<Expression>)),
    }

    let refns = just(Token::Refines)
        .ignore_then(attribute().repeated())
        .ignore_then(ident())
        .then((just(Token::Using).ignore_then(ident())).or_not())
        .map(YieldSpec::Refn);
    let req = just(Token::Requires)
        .then(attribute().repeated())
        .ignore_then(expression())
        .map(YieldSpec::Requires);
    let ens = just(Token::Ensures)
        .ignore_then(attribute().repeated())
        .ignore_then(expression())
        .map(YieldSpec::Ensures);
    let modf = just(Token::Modifies)
        .ignore_then(ident().separated_by(just(Token::Comma)).collect())
        .map(YieldSpec::Modifies);
    let req_call = just(Token::Requires)
        .ignore_then(just(Token::Call))
        .ignore_then(function_call.clone())
        .map(YieldSpec::ReqCall);
    let ens_call = just(Token::Ensures)
        .ignore_then(just(Token::Call))
        .ignore_then(function_call.clone())
        .map(YieldSpec::EnsCall);
    let preserves = just(Token::Preserves)
        .ignore_then(just(Token::Call))
        .ignore_then(function_call.clone())
        .map(YieldSpec::Preserves);

    let specs = choice((refns, req, ens, modf, req_call, ens_call, preserves))
        .then_ignore(just(Token::Semicolon))
        .repeated()
        .collect::<Vec<_>>();
    specs
        .map(|refn| {
            let mut specs = YieldSpecifications {
                requires: vec![],
                modifies: vec![],
                ensures: vec![],
                refines: vec![],
                preserves: vec![],
                req_calls: vec![],
                ens_calls: vec![],
            };

            for r in refn {
                match r {
                    YieldSpec::Refn(r) => specs.refines.push(r),
                    YieldSpec::Requires(r) => specs.requires.push(r),
                    YieldSpec::Ensures(e) => specs.ensures.push(e),
                    YieldSpec::Modifies(m) => specs.modifies.extend(m),
                    YieldSpec::ReqCall(rc) => specs.req_calls.push(rc),
                    YieldSpec::EnsCall(ec) => specs.ens_calls.push(ec),
                    YieldSpec::Preserves(pc) => specs.preserves.push(pc),
                }
            }

            specs
        })
        .boxed()
}

fn action_spec<'a, I>() -> impl Parser<'a, I, ActionSpecifications, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let function_call = ident().then(
        expression()
            .separated_by(just(Token::Comma))
            .collect()
            .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
    );
    enum ActionSpec {
        Refines(String, Option<String>),
        Requires(Expression),
        Modifies(Vec<String>),
        ReqCall(String, Vec<Expression>),
        Creates(Vec<String>),
        Asserts(Expression),
    }

    let refns = just(Token::Refines)
        .ignore_then(attribute().or_not())
        .ignore_then(ident())
        .then((just(Token::Using).ignore_then(ident())).or_not())
        .map(|(id, refn)| ActionSpec::Refines(id, refn));

    let req = just(Token::Requires)
        .ignore_then(attribute().repeated())
        .ignore_then(expression())
        .map(ActionSpec::Requires);

    let modf = just(Token::Modifies)
        .ignore_then(ident().separated_by(just(Token::Comma)).collect::<Vec<_>>())
        .map(ActionSpec::Modifies);

    let req_call = just(Token::Requires)
        .ignore_then(just(Token::Call))
        .ignore_then(function_call.clone())
        .map(|(id, args)| ActionSpec::ReqCall(id, args));

    let create = just(Token::Creates)
        .ignore_then(ident().separated_by(just(Token::Comma)).collect::<Vec<_>>())
        .map(ActionSpec::Creates);

    let asserts = just(Token::Asserts)
        .ignore_then(attribute().repeated())
        .ignore_then(expression())
        .map(ActionSpec::Asserts);

    let specs = choice((refns, req, create, asserts, modf, req_call))
        .then_ignore(just(Token::Semicolon))
        .repeated()
        .collect::<Vec<_>>();
    specs
        .map(|contracts| {
            let mut specs = ActionSpecifications {
                requires: vec![],
                requires_call: vec![],
                modifies: vec![],
                refines: vec![],
                asserts: vec![],
                creates: vec![],
            };
            for c in contracts {
                match c {
                    ActionSpec::Refines(i, _u) => specs.refines.push(i),
                    ActionSpec::Requires(e) => specs.requires.push(e),
                    ActionSpec::Modifies(m) => specs.modifies.extend(m),
                    ActionSpec::ReqCall(id, args) => specs.requires_call.push((id, args)),
                    ActionSpec::Creates(c) => specs.creates.extend(c),
                    ActionSpec::Asserts(a) => specs.asserts.push(a),
                }
            }

            specs
        })
        .boxed()
}

fn spec<'a, I>() -> impl Parser<'a, I, Specifications, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    enum Kind {
        Requires(bool, Expression),
        Ensures(bool, Expression),
        Modifies(Vec<String>),
    }

    let semi = just(Token::Semicolon);

    let req = just(Token::Free)
        .or_not()
        .map(|a| a.is_some())
        .then_ignore(just(Token::Requires))
        .then_ignore(attribute().repeated())
        .then(expression())
        .then_ignore(semi)
        .map(|(f, e)| Kind::Requires(f, e));
    let ens = just(Token::Free)
        .or_not()
        .map(|a| a.is_some())
        .then_ignore(just(Token::Ensures))
        .then_ignore(attribute().repeated())
        .then(expression())
        .then_ignore(semi)
        .map(|(f, e)| Kind::Ensures(f, e));
    let modf = just(Token::Modifies)
        .ignore_then(ident().separated_by(just(Token::Comma)).collect())
        .then_ignore(semi)
        .map(|mods| Kind::Modifies(mods));

    choice((req, ens, modf))
        .repeated()
        .collect()
        .map(|clauses: Vec<_>| {
            let mut requires = Vec::new();
            let mut ensures = Vec::new();
            let mut modifies = Vec::new();

            for k in clauses {
                match k {
                    Kind::Requires(f, e) => requires.push(Requires {
                        free: f,
                        expression: e,
                    }),
                    Kind::Ensures(f, e) => ensures.push(Ensures {
                        free: f,
                        expression: e,
                    }),
                    Kind::Modifies(i) => modifies.extend(i),
                }
            }
            Specifications {
                requires,
                modifies,
                ensures,
            }
        })
        .boxed()
}

// fn mover_qualifier<'a, I>() -> impl Parser<'a, I, MoverType, Err<Rich<'a, Token<'a>>>>
// where
//     I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
// {
//     todo()
// }

// fn spec_action<'a, I>() -> impl Parser<'a, I, SpecAction, Err<Rich<'a, Token<'a>>>>
// where
//     I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
// {
//     todo()
// }

// fn proc_formals<'a, I>() -> impl Parser<'a, I, Vec<Formal>, Err<Rich<'a, Token<'a>>>>
// where
//     I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
// {
//     todo()
// }

// fn invariant<'a, I>() -> impl Parser<'a, I, Invariant, Err<Rich<'a, Token<'a>>>>
// where
//     I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
// {
//     todo()
// }

// fn spec_yield_pre_post<'a, I>() -> impl Parser<'a, I, SpecYieldPrePost, Err<Rich<'a, Token<'a>>>>
// where
//     I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
// {
//     todo()
// }

// Additional helper parsers that might be needed:

fn type_expr<'a, I>() -> impl Parser<'a, I, Type, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    recursive(|type_expr| {
        let primitive = select! {
            Token::Int => Type::Int,
            Token::Real => Type::Real,
            Token::Bool => Type::Bool,
        };

        let type_var = select! {
            Token::Ident(name) if name.starts_with('\'') => Type::TypeVar(name.to_string())
        };

        let user_defined = select! {
            Token::Ident(name) if !name.starts_with('\'') => name.to_string()
        }
        .then(type_expr.clone().repeated().collect())
        .map(|(name, type_args)| Type::UserDefined(name, type_args));

        let ty_vars = just(Token::LessThan)
            .ignore_then(ident().separated_by(just(Token::Comma)).collect())
            .then_ignore(just(Token::GreaterThan))
            .or_not()
            .map(|ids| ids.unwrap_or_default());
        let map_type = ty_vars
            .then(
                just(Token::LeftBracket)
                    .ignore_then(type_expr.clone().separated_by(just(Token::Comma)).collect())
                    .then_ignore(just(Token::RightBracket)),
            )
            .then(type_expr.clone())
            .map(|((vars, domain), range)| Type::Map(vars, domain, Box::new(range)));

        let parenthesized = type_expr
            .clone()
            .delimited_by(just(Token::LeftParen), just(Token::RightParen));

        choice((primitive, type_var, user_defined, map_type, parenthesized))
    })
    .labelled("type")
}

fn attribute<'a, I>() -> impl Parser<'a, I, Attribute, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let attribute_name = ident();

    let attribute_param = expression().separated_by(just(Token::Comma)).collect();

    just(Token::LeftBrace)
        .ignore_then(just(Token::Colon))
        .ignore_then(attribute_name)
        .then(attribute_param)
        .then_ignore(just(Token::RightBrace))
        .map(|(name, params)| Attribute { name, params })
}

fn attribute_inner<'a, I>(
    expr: impl Parser<'a, I, Expression, Err<Rich<'a, Token<'a>>>>,
) -> impl Parser<'a, I, Attribute, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let attribute_name = ident();

    let attribute_param = expr.separated_by(just(Token::Comma)).collect();

    just(Token::LeftBrace)
        .ignore_then(just(Token::Colon))
        .ignore_then(attribute_name)
        .then(attribute_param)
        .then_ignore(just(Token::RightBrace))
        .map(|(name, params)| Attribute { name, params })
}

fn block<'a, I>() -> impl Parser<'a, I, ImplBlock, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    (local_var()
        .repeated()
        .collect()
        .map(|v: Vec<_>| v.into_iter().flatten().collect())
        .then(stmt_list()))
    .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
    .map(|(vars, statements)| ImplBlock {
        local_vars: vars,
        statements,
    })
}

fn stmt_list<'a, I>() -> impl Parser<'a, I, Vec<Statement>, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let assert = just(Token::Assert)
        .ignore_then(attribute().repeated().collect())
        .then(expression())
        .map(|(attrs, e)| Statement::Assert(attrs, e));

    let assume = just(Token::Assume)
        .ignore_then(attribute().repeated().collect())
        .then(expression())
        .map(|(attrs, e)| Statement::Assume(attrs, e));

    let call_expr = attribute()
        .or_not()
        .ignore_then(ident())
        .then(attribute().or_not())
        .then(
            expression()
                .separated_by(just(Token::Comma))
                .collect()
                .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
        )
        .boxed();

    let lhs_el = {
        use chumsky::pratt::*;

        let map = expression()
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LeftBracket), just(Token::RightBracket));

        let field = just(Token::Arrow).ignore_then(ident());

        ident()
            .map(Lhs::Simple)
            .pratt((
                postfix(0, map, |l, args| Lhs::Map(Box::new(l), args)),
                postfix(1, field, |l, args| Lhs::Field(Box::new(l), args)),
            ))
            .boxed()
    };

    let unpack_expr = ident()
        .then(
            ident()
                .separated_by(just(Token::Comma))
                .collect()
                .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
        )
        .boxed();

    // let lhs_el = just(Token::Arrow).ignore_then(expression()).or();
    let unpack = unpack_expr
        .then_ignore(just(Token::Assign))
        .then(expression())
        .map(|(i, e)| Statement::Unpack(i, e));

    let assign = lhs_el
        .separated_by(just(Token::Comma))
        .collect()
        .then_ignore(just(Token::Assign))
        .then_ignore(attribute().repeated())
        .then(expression().separated_by(just(Token::Comma)).collect())
        .map(|(i, e)| Statement::Assign(i, e));

    let havoc = just(Token::Havoc)
        .ignore_then(ident().separated_by(just(Token::Comma)).collect())
        .map(|ids| Statement::Havoc(ids));

    let call = just(Token::Async)
        .or_not()
        .ignore_then(just(Token::Call))
        .ignore_then(attribute().or_not())
        .ignore_then(
            (ident()
                .separated_by(just(Token::Comma))
                .collect()
                .then_ignore(just(Token::Assign)))
            .or_not()
            .map(|s| s.unwrap_or_default()),
        )
        .then(call_expr.clone())
        .map(|(lhs, ((name, _attr), args))| Statement::Call(lhs, name, args));
    let par = just(Token::Par)
        .ignore_then(
            ((ident()
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .then_ignore(just(Token::Assign)))
            .or_not()
            .then(call_expr.clone()))
            .separated_by(just(Token::VerticalBar))
            .collect::<Vec<_>>(),
        )
        .map(|calls| {
            let calls = calls
                .into_iter()
                // TODO: Keep attributes
                .map(|(vars, (a, b))| (vars, a.0, b))
                .collect();
            Statement::Par(calls)
        });

    let cmd = choice((assert, assume, unpack, assign, havoc, call, par))
        .then_ignore(just(Token::Semicolon))
        .boxed();

    let goto = just(Token::Goto)
        .ignore_then(ident().separated_by(just(Token::Comma)).collect())
        .map(Statement::Goto);
    let return_ = just(Token::Return).map(|_| Statement::Return);

    let term = choice((goto, return_)).then_ignore(just(Token::Semicolon));

    let block = ident()
        .then_ignore(just(Token::Colon))
        .then(cmd.clone().repeated().collect())
        .then(term.clone().or_not())
        .map(|((id, cmds), term)| BasicBlock {
            label: id,
            statements: cmds,
            term: term.map(|a| Box::new(a)),
        })
        .map(Statement::Block);
    // let cmd = choice(());

    // let term = todo();

    // let structured = choice(());
    let star_or_exp = expression()
        .map(Some)
        .or(just(Token::Asterisk).map(|_| None))
        .delimited_by(just(Token::LeftParen), just(Token::RightParen));

    recursive(|stmt| {
        let stmt_braced = stmt
            .clone()
            .repeated()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LeftBrace), just(Token::RightBrace));
        let if_stmt = just(Token::If)
            .ignore_then(star_or_exp.clone())
            .then(stmt_braced.clone())
            .then_ignore(
                // TODO: unignore this part of if statments
                (just(Token::Else).ignore_then(
                    just(Token::If)
                        .ignore_then(star_or_exp.clone())
                        .then(stmt_braced.clone()),
                ))
                .repeated(),
            )
            .then(
                (just(Token::Else).ignore_then(stmt_braced))
                    .map(|stmts| ImplBlock {
                        local_vars: Vec::new(),
                        statements: stmts,
                    })
                    .or_not(),
            )
            .map(|((e, then), else_)| {
                Statement::If(
                    e,
                    ImplBlock {
                        local_vars: Vec::new(),
                        statements: then,
                    },
                    else_,
                )
            });

        let invariant_ = just(Token::Free)
            .or_not()
            .map(|a| a.is_some())
            .ignore_then(just(Token::Invariant))
            .then_ignore(attribute().repeated())
            .ignore_then(
                expression().map(Invariant::Expression).or(just(Token::Call)
                    .ignore_then(call_expr)
                    .map(|((name, _), args)| Invariant::Call(name, args))),
            )
            .then_ignore(just(Token::Semicolon));

        let while_ = just(Token::While)
            .ignore_then(star_or_exp)
            .then(invariant_.repeated().collect())
            .then(
                stmt.repeated()
                    .collect()
                    .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
            )
            .map(|((e, invs), body)| {
                Statement::While(
                    e,
                    invs,
                    ImplBlock {
                        local_vars: Vec::new(),
                        statements: body,
                    },
                )
            });
        let break_ = just(Token::Break)
            .ignore_then(ident().or_not())
            .map(|i| Statement::Break(i))
            .then_ignore(just(Token::Semicolon));

        choice((block, break_, cmd.or(term), if_stmt, while_)).boxed()
    })
    .repeated()
    .collect()
}

fn local_var<'a, I>() -> impl Parser<'a, I, Vec<(String, Type)>, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let ids_ty_pair = ident()
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
        .then_ignore(just(Token::Colon))
        .then(type_expr())
        .then_ignore(just(Token::Where).then(expression()).or_not())
        .map(|(ids, ty)| ids.into_iter().map(|i| (i, ty.clone())).collect::<Vec<_>>());

    just(Token::Var)
        .then_ignore(attribute().repeated())
        .ignore_then(
            ids_ty_pair
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>(),
        )
        .map(|c| c.into_iter().flatten().collect())
        .then_ignore(just(Token::Semicolon))
}

// pretty bugged out

fn expression<'a, I>() -> impl Parser<'a, I, Expression, Err<Rich<'a, Token<'a>>>> + Clone
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let variable = ident().map(Expression::Var).labelled("variable");

    let literal = select! {
        Token::Digits(n) => Expression::Literal(Literal::Int(n.parse().unwrap())),
        Token::Decimal(n) => Expression::Literal(Literal::Real(n.parse().unwrap())),
        Token::BvLit(n) => {
            let parts: Vec<_> = n.split("bv").collect();

            Expression::Literal(
                Literal::BitVector(parts[0].parse().unwrap(), parts[1].parse().unwrap())
            )
        },
        Token::HexFloat(hex_float) => {
            // TODO: Parse float string
            Expression::Literal(
                Literal::Float(
                    hex_float.to_string()
            ))
        },
        Token::DecFloat(f) => {
            // TODO: Parse float string
            Expression::Literal(
                Literal::Float(
                    f.to_string()
            ))
        },
        Token::BvFloat(f) => {
            // TODO: Parse float string
            Expression::Literal(
                Literal::Float(
                    f.to_string()
            ))
        },
        Token::String(s) => Expression::Literal(Literal::String(s.to_string())),
        Token::True => Expression::Literal(Literal::Bool(true)),
        Token::False => Expression::Literal(Literal::Bool(false)),
    }
    .labelled("literal");

    recursive(|expr| {
        let ty_vars = ident()
            .separated_by(just(Token::Comma))
            .delimited_by(just(Token::LessThan), just(Token::GreaterThan));

        let lambda = {
            just(Token::Lambda)
                .ignore_then(ty_vars.clone().or_not())
                .then(formal_args_inner(expr.clone()))
                .then_ignore(just(Token::DoubleColon))
                .then_ignore(attribute_inner(expr.clone().boxed()).repeated())
                .then(expr.clone())
                .map(|((ty_vars, vars), body)| Expression::Lambda(vars, Box::new(body)))
        };
        let quantifier = {
            let quantifier_type = choice((
                just(Token::Forall).to(Quantifier::Forall),
                just(Token::Exists).to(Quantifier::Exists),
            ));

            let bound_vars = ids_type();

            let trigger = expr
                .clone()
                .separated_by(just(Token::Comma))
                .collect()
                .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
                .map(Trigger);
            let attr = attribute_inner(expr.clone().boxed());
            enum AorT {
                A(Attribute),
                T(Trigger),
            }
            quantifier_type
                .then_ignore(attribute_inner(expr.clone()).repeated())
                .then_ignore(ty_vars.or_not())
                .then(bound_vars)
                .then_ignore(just(Token::DoubleColon))
                .then(
                    attr.map(AorT::A)
                        .or(trigger.map(AorT::T))
                        .repeated()
                        .collect::<Vec<_>>(),
                )
                .then(expr.clone())
                .map(|(((quantifier, vars), aorts), body)| {
                    let mut triggers = Vec::new();
                    let mut attrs = Vec::new();

                    for aort in aorts {
                        match aort {
                            AorT::T(trigger) => triggers.push(trigger),
                            AorT::A(attr) => attrs.push(attr),
                        }
                    }
                    Expression::Quantifier(quantifier, vars, triggers, Box::new(body))
                })
        };

        let let_expr = just(Token::Var)
            .ignore_then(
                attribute_inner(expr.clone())
                    .repeated()
                    .collect()
                    .then(ident())
                    .separated_by(just(Token::Comma))
                    .collect(),
            )
            .then_ignore(just(Token::Assign))
            .then(expr.clone().separated_by(just(Token::Comma)).collect())
            .then_ignore(just(Token::Semicolon))
            .then(attribute_inner(expr.clone()).repeated().collect())
            .then(expr.clone())
            .map(|(((ids, exprs), attrs), body)| {
                Expression::Let(ids, exprs, attrs, Box::new(body))
            });

        let if_ = {
            just(Token::If)
                .ignore_then(expr.clone())
                .then(just(Token::Then).ignore_then(expr.clone()))
                .then(just(Token::Else).ignore_then(expr.clone()))
                .map(|((e, t), f)| Expression::If(Box::new(e), Box::new(t), Box::new(f)))
        };

        let parenthesized = choice((expr.clone(), quantifier, lambda, let_expr))
            .delimited_by(just(Token::LeftParen), just(Token::RightParen));

        let old = just(Token::Old)
            .ignore_then(
                expr.clone()
                    .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
            )
            .map(|e| Expression::Old(Box::new(e)));

        let coercions = choice((
            just(Token::Int)
                .ignore_then(
                    expr.clone()
                        .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
                )
                .map(|e| Expression::IntCast(Box::new(e))),
            just(Token::Real)
                .ignore_then(
                    expr.clone()
                        .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
                )
                .map(|e| Expression::RealCast(Box::new(e))),
        ));

        let function_call = ident()
            .then(
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .collect()
                    .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
            )
            .map(|(name, args)| Expression::FunctionCall(name, args))
            .labelled("function call");

        let rounding = select! {
            Token::RNE => Expression::Rounding(Rounding::RNE),
            Token::RTN => Expression::Rounding(Rounding::RTN),
            Token::RNA => Expression::Rounding(Rounding::RNA),
            Token::RTP => Expression::Rounding(Rounding::RTP),
            Token::RTZ => Expression::Rounding(Rounding::RTZ),
        };

        let atom = choice((
            literal,
            old,
            coercions,
            function_call,
            variable,
            parenthesized,
            if_,
            rounding,
        ));

        enum MapOp {
            Bv(Expression, Expression),
            Map(Vec<Expression>),
            Assign(Vec<Expression>, Expression),
        }
        let bv_extract = expr
            .clone()
            .then_ignore(just(Token::Colon))
            .then(expr.clone())
            .map(|(a, b)| MapOp::Bv(a, b));
        let map_ = expr
            .clone()
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .map(MapOp::Map);

        let map_assign = expr
            .clone()
            .separated_by(just(Token::Comma))
            .collect()
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .map(|(a, b)| MapOp::Assign(a, b));

        let one_map_op = bv_extract
            .clone()
            .or(bv_extract.delimited_by(just(Token::LeftParen), just(Token::RightParen)))
            .or(map_assign)
            .or(map_)
            .delimited_by(just(Token::LeftBracket), just(Token::RightBracket));

        // let map_access = atom
        //     .foldl(
        //         one_map_op.clone().repeated(),
        //         |acc, indices| match indices {
        //             MapOp::Bv(a, b) => {
        //                 Expression::BvExtract(Box::new(acc), Box::new(a), Box::new(b))
        //             }
        //             MapOp::Map(indices) => Expression::MapSelect(Box::new(acc), indices),
        //             MapOp::Assign(a, b) => {
        //                 Expression::MapUpdate(Box::new(acc), Box::new(a), Box::new(b))
        //             }
        //         },
        //     )
        //     .labelled("map access");

        // let atom = map_access;
        use chumsky::pratt::*;

        atom.pratt((
            infix(
                left(0),
                just(Token::DoubleArrow).or(just(Token::LogicalEquivalence)),
                |l, _, r| Expression::BinaryOp(BinaryOp::Iff, Box::new(l), Box::new(r)),
            ),
            infix(right(1), just(Token::Implies), |l, _, r| {
                Expression::BinaryOp(BinaryOp::Implies, Box::new(l), Box::new(r))
            }),
            infix(left(2), just(Token::LogicalOr), |l, _, r| {
                Expression::BinaryOp(BinaryOp::Or, Box::new(l), Box::new(r))
            }),
            infix(left(2), just(Token::LogicalAnd), |l, _, r| {
                Expression::BinaryOp(BinaryOp::And, Box::new(l), Box::new(r))
            }),
            infix(left(3), just(Token::Equality), |l, _, r| {
                Expression::BinaryOp(BinaryOp::Eq, Box::new(l), Box::new(r))
            }),
            infix(left(3), just(Token::NotEqual), |l, _, r| {
                Expression::BinaryOp(BinaryOp::Neq, Box::new(l), Box::new(r))
            }),
            infix(left(3), just(Token::LessThan), |l, _, r| {
                Expression::BinaryOp(BinaryOp::Lt, Box::new(l), Box::new(r))
            }),
            infix(left(3), just(Token::LessThanOrEqual), |l, _, r| {
                Expression::BinaryOp(BinaryOp::Le, Box::new(l), Box::new(r))
            }),
            infix(left(3), just(Token::GreaterThan), |l, _, r| {
                Expression::BinaryOp(BinaryOp::Gt, Box::new(l), Box::new(r))
            }),
            infix(left(3), just(Token::GreaterThanOrEqual), |l, _, r| {
                Expression::BinaryOp(BinaryOp::Ge, Box::new(l), Box::new(r))
            }),
            infix(left(4), just(Token::Increment), |l, _, r| {
                Expression::BinaryOp(BinaryOp::Concat, Box::new(l), Box::new(r))
            }),
            infix(left(5), just(Token::Plus), |l, _, r| {
                Expression::BinaryOp(BinaryOp::Add, Box::new(l), Box::new(r))
            }),
            infix(left(5), just(Token::Minus), |l, _, r| {
                Expression::BinaryOp(BinaryOp::Sub, Box::new(l), Box::new(r))
            }),
            infix(left(6), just(Token::Asterisk), |l, _, r| {
                Expression::BinaryOp(BinaryOp::Mul, Box::new(l), Box::new(r))
            }),
            infix(
                left(6),
                just(Token::Div).or(just(Token::Division)),
                |l, _, r| Expression::BinaryOp(BinaryOp::Div, Box::new(l), Box::new(r)),
            ),
            infix(left(6), just(Token::Mod), |l, _, r| {
                Expression::BinaryOp(BinaryOp::Mod, Box::new(l), Box::new(r))
            }),
            infix(right(7), just(Token::Exponentiation), |l, _, r| {
                Expression::BinaryOp(BinaryOp::Pow, Box::new(l), Box::new(r))
            }),
            prefix(8, just(Token::Minus).labelled("operator"), |_, e| {
                Expression::UnaryOp(UnaryOp::Neg, Box::new(e))
            }),
            prefix(
                8,
                just(Token::NegationSymbol).or(just(Token::ExclamationMark)),
                |_, e| Expression::UnaryOp(UnaryOp::Not, Box::new(e)),
            ),
            postfix(10, just(Token::Arrow).ignore_then(ident()), |l, f| {
                Expression::Field(Box::new(l), f)
            }),
            postfix(10, just(Token::Is).ignore_then(ident()), |l, f| {
                Expression::Is(Box::new(l), f)
            }),
            postfix(
                10,
                just(Token::Colon).ignore_then(type_expr()),
                |op, _ty| {
                    // TODO
                    op
                },
            ),
            postfix(10, one_map_op, |e, map_op| match map_op {
                MapOp::Bv(a, b) => Expression::BvExtract(Box::new(e), Box::new(a), Box::new(b)),
                MapOp::Map(indices) => Expression::MapSelect(Box::new(e), indices),
                MapOp::Assign(a, b) => Expression::MapUpdate(Box::new(e), a, Box::new(b)),
            }),
        ))
        .boxed()
    })
}
