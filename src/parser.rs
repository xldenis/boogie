use super::ast::*;
use crate::lexer::Token;
use chumsky::{extra::Err, pratt};
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
        // action_decl().map(Declaration::Action),
        // yield_invariant_decl().map(Declaration::YieldInvariant),
        // yield_procedure_decl().map(Declaration::YieldProcedure),
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
}

fn function_decl<'a, I>() -> impl Parser<'a, I, FunctionDecl, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let attributes = attribute().repeated().collect();

    (just(Token::Function)
        .ignore_then(attributes)
        .then(proc_signature())
        .then(
            expression()
                .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
                .or_not(),
        )
        .then(uses().or_not())
        .then_ignore(just(Token::Semicolon)))
    .map(|(((attrs, sig), body), axioms)| {
        FunctionDecl {
            signature: sig,
            body,
            axioms,
            attributes: attrs, //         axioms: axioms.unwrap_or_default(),
        }
    })
}

fn uses<'a, I>() -> impl Parser<'a, I, Vec<Axiom>, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    just(Token::Uses).ignore_then(
        axiom_decl()
            .repeated()
            .collect()
            .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
    )
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
}

fn ids_type<'a, I>() -> impl Parser<'a, I, Vec<(String, Type)>, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    ident()
        .then_ignore(just(Token::Colon))
        .then(type_expr())
        .repeated()
        .collect()
}

fn ident<'a, I>() -> impl Parser<'a, I, String, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    select! {
        Token::Ident(s) => s.into()
    }
    .labelled("identifier")
}

fn type_params<'a, I>() -> impl Parser<'a, I, Vec<TypeVariable>, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    todo()
}

fn type_decl<'a, I>() -> impl Parser<'a, I, TypeDecl, Err<Rich<'a, Token<'a>>>>
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

    let attributes = attribute().repeated().collect();

    just(Token::Type)
        .ignore_then(attributes)
        .then(select! { Token::Ident(name) => name.to_string() })
        .then(type_params)
        .then(just(Token::Equals).ignore_then(type_expr()).or_not())
        .then_ignore(just(Token::Semicolon))
        .map(|(((attrs, name), type_params), body)| TypeDecl {
            name,
            type_params,
            body,
            attributes: attrs,
        })
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

    let field = select! { Token::Ident(name) => name.to_string() }
        .then_ignore(just(Token::Colon))
        .then(type_expr())
        .map(|(name, typ)| Variable {
            name,
            typ,
            where_clause: None,
        });

    let constructor = select! { Token::Ident(name) => name.to_string() }
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
        .separated_by(just(Token::VerticalBar))
        .collect()
        .delimited_by(just(Token::LeftBrace), just(Token::RightBrace));

    let attributes = attribute().repeated().collect();

    just(Token::Datatype)
        .ignore_then(attributes)
        .then(select! { Token::Ident(name) => name.to_string() })
        .then(type_params)
        .then(constructors)
        .then_ignore(just(Token::Semicolon))
        .map(
            |(((attrs, name), type_params), constructors)| DatatypeDecl {
                name,
                type_params,
                constructors,
                attributes: attrs,
            },
        )
}

fn global_var_decl<'a, I>() -> impl Parser<'a, I, GlobalVarDecl, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let attributes = attribute().repeated().collect();

    let id_type = ident().then(type_expr());
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
}

// fn action_decl<'a, I>() -> impl Parser<'a, I, ActionDecl, Err<Rich<'a, Token<'a>>>>
// where
//     I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
// {
//     let attributes = attribute().repeated().collect();
//     let is_async = just(Token::Async).or_not().map(|x| x.is_some());
//     let mover_type = mover_qualifier().or_not();

//     (is_async
//         .then(mover_type)
//         .then(just(Token::Action))
//         .ignore_then(attributes)
//         .then(proc_signature())
//         .then(
//             spec_action()
//                 .repeated()
//                 .collect()
//                 .then(impl_body().or_not())
//                 .or(just(Token::Semicolon).to((vec![], None))),
//         ))
//     .map(
//         |((((is_async, mover_type), attrs), signature), (specs, body))| ActionDecl {
//             is_async,
//             mover_type,
//             signature,
//             specifications: specs,
//             body,
//             attributes: attrs,
//         },
//     )
// }

// fn yield_invariant_decl<'a, I>() -> impl Parser<'a, I, YieldInvariantDecl, Err<Rich<'a, Token<'a>>>>
// where
//     I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
// {
//     let attributes = attribute().repeated().collect();

//     just(Token::Yield)
//         .then(just(Token::Invariant))
//         .ignore_then(attributes)
//         .then(select! { Token::Ident(name) => name.to_string() })
//         .then(proc_formals())
//         .then_ignore(just(Token::Semicolon))
//         .then(invariant().repeated().collect())
//         .map(
//             |(((attrs, name), formals), invariants)| YieldInvariantDecl {
//                 name,
//                 formals,
//                 invariants,
//                 attributes: attrs,
//             },
//         )
// }

// fn yield_procedure_decl<'a, I>() -> impl Parser<'a, I, YieldProcedureDecl, Err<Rich<'a, Token<'a>>>>
// where
//     I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
// {
//     let attributes = attribute().repeated().collect();
//     let mover_type = mover_qualifier().or_not();

//     (just(Token::Yield)
//         .then(just(Token::Procedure))
//         .ignore_then(attributes)
//         .then(select! { Token::Ident(name) => name.to_string() })
//         .then(proc_formals())
//         .then(just(Token::Returns).ignore_then(proc_formals()).or_not())
//         .then(
//             spec_yield_pre_post()
//                 .repeated()
//                 .collect()
//                 .then(impl_body().or_not())
//                 .or(just(Token::Semicolon).to((vec![], None))),
//         ))
//     .map(
//         |((((attrs, name), in_params), out_params), (specs, body))| YieldProcedureDecl {
//             name,
//             in_params,
//             out_params,
//             specifications: specs,
//             body,
//             attributes: attrs,
//         },
//     )
// }

fn proc_signature<'a, I>() -> impl Parser<'a, I, Signature, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    ident()
        .then(
            ident()
                .map(|i| TypeVariable { name: i })
                .separated_by(just(Token::Comma))
                .collect()
                .delimited_by(just(Token::LessThan), just(Token::GreaterThan))
                .or_not()
                .map(|tys| tys.unwrap_or_default()),
        )
        .then(
            formal_arg()
                .separated_by(just(Token::Comma))
                .collect()
                .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
        )
        .then(
            just(Token::Returns)
                .ignore_then(
                    formal_arg()
                        .or_not()
                        .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
                )
                .or_not()
                .map(|a| a.flatten()),
        )
        .map(|(((name, type_params), params), returns)| Signature {
            name,
            type_params,
            params,
            returns,
        })
}

fn formal_arg<'a, I>() -> impl Parser<'a, I, FormalArg, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    ident()
        .then_ignore(just(Token::Colon))
        .then(type_expr())
        .map(|(i, ty)| FormalArg::Named(i, ty))
        .or(type_expr().map(FormalArg::Anon))
}

fn spec<'a, I>() -> impl Parser<'a, I, Specifications, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    enum Kind {
        Requires(bool, Expression),
        Ensures(bool, Expression),
        Modifies(String),
    }

    let semi = just(Token::Semicolon);
    let req = just(Token::Requires)
        .ignore_then(just(Token::Free).or_not().map(|a| a.is_some()))
        .then(expression())
        .then_ignore(semi)
        .map(|(f, e)| Kind::Requires(f, e));
    let ens = just(Token::Ensures)
        .ignore_then(just(Token::Free).or_not().map(|a| a.is_some()))
        .then(expression())
        .then_ignore(semi)
        .map(|(f, e)| Kind::Ensures(f, e));
    let modf = just(Token::Modifies)
        .ignore_then(ident())
        .then_ignore(semi)
        .map(Kind::Modifies);

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
                    Kind::Modifies(i) => modifies.push(i),
                }
            }
            Specifications {
                requires,
                modifies,
                ensures,
            }
        })
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
        .then(
            type_expr
                .clone()
                .separated_by(just(Token::Comma))
                .collect()
                .delimited_by(just(Token::LessThan), just(Token::GreaterThan))
                .or_not(),
        )
        .map(|(name, type_args)| Type::UserDefined(name, type_args.unwrap_or_default()));

        let map_type = just(Token::LeftBracket)
            .ignore_then(type_expr.clone())
            .then_ignore(just(Token::RightBracket))
            .then(type_expr.clone())
            .map(|(domain, range)| Type::Map(Box::new(domain), Box::new(range)));

        let parenthesized = type_expr
            .clone()
            .delimited_by(just(Token::LeftParen), just(Token::RightParen));

        choice((primitive, type_var, user_defined, map_type, parenthesized))
    })
}

fn attribute<'a, I>() -> impl Parser<'a, I, Attribute, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let attribute_name = select! { Token::Ident(name) => name.to_string() };

    let attribute_param = expression().separated_by(just(Token::Comma)).collect();

    just(Token::LeftBrace)
        .ignore_then(just(Token::Colon))
        .ignore_then(attribute_name)
        .then(
            attribute_param
                .delimited_by(just(Token::LeftParen), just(Token::RightParen))
                .or_not(),
        )
        .then_ignore(just(Token::RightBrace))
        .map(|(name, params)| Attribute {
            name,
            params: params.unwrap_or_default(),
        })
}

fn block<'a, I>() -> impl Parser<'a, I, ImplBlock, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    (local_vars().then(stmt_list()))
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
    enum Step {
        Terminator(Statement),
        Label(String),
        Structed(Statement),
        Cmd(Statement),
    }

    let assert = just(Token::Assert)
        .ignore_then(expression())
        .then_ignore(just(Token::Semicolon))
        .map(Statement::Assert);

    let assume = just(Token::Assume)
        .ignore_then(expression())
        .then_ignore(just(Token::Semicolon))
        .map(Statement::Assume);

    let lhs_list = ident().separated_by(just(Token::Comma)).collect();
    let assign = lhs_list
        .then_ignore(just(Token::Assign))
        .then(expression().separated_by(just(Token::Comma)).collect())
        .then_ignore(just(Token::Semicolon))
        .map(|(i, e)| Statement::Assign(i, e));

    let cmd = choice((assert, assume, assign)).boxed();

    let goto = just(Token::Goto).ignore_then(ident().separated_by(just(Token::Comma)).collect()).map(Statement::Goto);
    let return_ = just(Token::Return).map(|_| Statement::Return);

    let term = choice((goto, return_)).then_ignore(just(Token::Semicolon));

    let block = ident()
        .then_ignore(just(Token::Colon))
        .then(cmd.clone().repeated().collect())
        .then(term.or_not())
        .map(|((id, cmds), term)| BasicBlock {
            label: id,
            statements: cmds,
            term: term.map(|a| Box::new(a)),
        })
        .map(Statement::Block);
    // let cmd = choice(());

    // let term = todo();

    // let structured = choice(());

    choice((block, cmd)).repeated().collect()
}

fn local_vars<'a, I>() -> impl Parser<'a, I, Vec<(String, Type)>, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    just(Token::Var)
        .ignore_then(ident())
        .then_ignore(just(Token::Colon))
        .then(type_expr())
        .then_ignore(just(Token::Semicolon))
        .repeated()
        .collect()
}

// pretty bugged out

fn expression<'a, I>() -> impl Parser<'a, I, Expression, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let variable = select! {
        Token::Ident(name) => Expression::Var(name.to_string())
    }
    .labelled("variable");

    let literal = select! {
        Token::Digits(n) => Expression::Literal(Literal::Int(n.parse().unwrap())),
        // Token::Decimal(n) => Expression::Literal(Literal::Real(n.parse().unwrap())),
        // Token::BvLit(n) => Expression::Literal(Literal::BitVector(n.to_string())),
        Token::True => Expression::Literal(Literal::Bool(true)),
        Token::False => Expression::Literal(Literal::Bool(false)),
    }
    .labelled("literal");

    let quantifier = {
        let quantifier_type = choice((
            just(Token::Forall).to(Quantifier::Forall),
            just(Token::Exists).to(Quantifier::Exists),
        ));

        let bound_var = select! { Token::Ident(name) => name.to_string() }
            .then_ignore(just(Token::Colon))
            .then(type_expr())
            .map(|(name, typ)| Variable {
                name,
                typ,
                where_clause: None,
            });

        quantifier_type
            .then(bound_var.separated_by(just(Token::Comma)).collect())
            .then_ignore(just(Token::DoubleColon))
            .then(recursive(|expr| expr))
            .map(|((quantifier, vars), body)| {
                Expression::Quantifier(quantifier, vars, Box::new(body))
            })
    };

    let lambda = {
        let bound_var = select! { Token::Ident(name) => name.to_string() }
            .then_ignore(just(Token::Colon))
            .then(type_expr())
            .map(|(name, typ)| Variable {
                name,
                typ,
                where_clause: None,
            });

        just(Token::Lambda)
            .ignore_then(bound_var.separated_by(just(Token::Comma)).collect())
            .then_ignore(just(Token::DoubleColon))
            .then(recursive(|expr| expr))
            .map(|(vars, body)| Expression::Lambda(vars, Box::new(body)))
    };

    recursive(|expr| {
        let parenthesized = expr
            .clone()
            .delimited_by(just(Token::LeftParen), just(Token::RightParen));

        let old = just(Token::Old)
            .ignore_then(
                expr.clone()
                    .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
            )
            .map(|e| Expression::Old(Box::new(e)));

        let function_call = select! { Token::Ident(name) => name.to_string() }
            .then(
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .collect()
                    .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
            )
            .map(|(name, args)| Expression::FunctionCall(name, args));

        let atom = choice((literal, variable, old, function_call, parenthesized));

        let atom = choice((atom.clone(), quantifier, lambda));
        use chumsky::pratt::*;
        let map_access = atom
            // .then(
            //     expr.clone()
            //         .separated_by(just(Token::Comma))
            //         .delimited_by(just(Token::LeftBracket), just(Token::RightBracket))
            //         .repeated()
            // )
            // .foldl(|acc, indices| {
            //     Expression::MapSelect(Box::new(acc), indices)
            // });
            ;
        map_access
            .pratt((
                prefix(0, just(Token::Minus), |_, e| {
                    Expression::UnaryOp(UnaryOp::Neg, Box::new(e))
                }),
                prefix(
                    0,
                    just(Token::NegationSymbol).or(just(Token::ExclamationMark)),
                    |_, e| Expression::UnaryOp(UnaryOp::Not, Box::new(e)),
                ),
                infix(
                    left(1),
                    just(Token::DoubleArrow).or(just(Token::LogicalEquivalence)),
                    |l, _, r| Expression::BinaryOp(BinaryOp::Iff, Box::new(l), Box::new(r)),
                ),
                infix(right(2), just(Token::Implies), |l, _, r| {
                    Expression::BinaryOp(BinaryOp::Implies, Box::new(l), Box::new(r))
                }),
                infix(left(3), just(Token::LogicalOr), |l, _, r| {
                    Expression::BinaryOp(BinaryOp::Or, Box::new(l), Box::new(r))
                }),
                infix(left(4), just(Token::LogicalAnd), |l, _, r| {
                    Expression::BinaryOp(BinaryOp::And, Box::new(l), Box::new(r))
                }),
                infix(left(5), just(Token::Equality), |l, _, r| {
                    Expression::BinaryOp(BinaryOp::Eq, Box::new(l), Box::new(r))
                }),
                infix(left(5), just(Token::NotEqual), |l, _, r| {
                    Expression::BinaryOp(BinaryOp::Neq, Box::new(l), Box::new(r))
                }),
                infix(left(6), just(Token::LessThan), |l, _, r| {
                    Expression::BinaryOp(BinaryOp::Lt, Box::new(l), Box::new(r))
                }),
                infix(left(6), just(Token::LessThanOrEqual), |l, _, r| {
                    Expression::BinaryOp(BinaryOp::Le, Box::new(l), Box::new(r))
                }),
                infix(left(6), just(Token::GreaterThan), |l, _, r| {
                    Expression::BinaryOp(BinaryOp::Gt, Box::new(l), Box::new(r))
                }),
                infix(left(6), just(Token::GreaterThanOrEqual), |l, _, r| {
                    Expression::BinaryOp(BinaryOp::Ge, Box::new(l), Box::new(r))
                }),
                infix(left(7), just(Token::Plus), |l, _, r| {
                    Expression::BinaryOp(BinaryOp::Add, Box::new(l), Box::new(r))
                }),
                infix(left(7), just(Token::Minus), |l, _, r| {
                    Expression::BinaryOp(BinaryOp::Sub, Box::new(l), Box::new(r))
                }),
                infix(left(8), just(Token::Asterisk), |l, _, r| {
                    Expression::BinaryOp(BinaryOp::Mul, Box::new(l), Box::new(r))
                }),
                infix(left(8), just(Token::Div), |l, _, r| {
                    Expression::BinaryOp(BinaryOp::Div, Box::new(l), Box::new(r))
                }),
                infix(left(8), just(Token::Mod), |l, _, r| {
                    Expression::BinaryOp(BinaryOp::Mod, Box::new(l), Box::new(r))
                }),
                infix(right(9), just(Token::Exponentiation), |l, _, r| {
                    Expression::BinaryOp(BinaryOp::Pow, Box::new(l), Box::new(r))
                }),
                // postfix(
                //     10,
                //     just(Token::LeftBracket)
                //         .ignore_then(expr.clone().boxed().separated_by(just(Token::Comma)).collect::<Vec<_>>())
                //         .then_ignore(just(Token::RightBracket)),
                //     |e, op| Expression::MapSelect(Box::new(e), op),
                // ),
            ))
            .boxed()
    })
}
