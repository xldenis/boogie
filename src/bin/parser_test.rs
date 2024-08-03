use std::fs::read_to_string;

use chumsky::{
    input::{Input, Stream},
    Parser,
};
use boogie::{lexer::Token, parser};
use logos::Logos;
// use pretty::{BoxAllocator, Pretty};

fn main() -> color_eyre::eyre::Result<()> {
    color_eyre::install()?;

    let file = read_to_string(std::env::args().nth(1).unwrap())?;
    // let tokens1: Vec<_> = Token::lexer(&file).collect();

    // println!("{tokens1}");
    let tokens = Token::lexer(&file).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, span.into()),
        Err(()) => (Token::Error, span.into()),
    });

    let stream = Stream::from_iter(tokens).spanned((file.len()..file.len()).into());
    let parse = parser::program().parse(stream);

    let result = match parse.into_result() {
        Ok(re) => re,
        Err(errs) => {
            use ariadne::{Color, Label, Report, ReportKind, Source};
            for err in errs {
                Report::build(ReportKind::Error, (), err.span().start)
                    .with_code(3)
                    .with_message(err.to_string())
                    .with_label(
                        Label::new(err.span().into_range())
                            .with_message(err.reason().to_string())
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint(Source::from(&file))
                    .unwrap();
            }
            return Ok(());
        }
    };

    eprintln!("Parse ok!");
    // let alloc = BoxAllocator;
    // let stdout = std::io::stdout();
    // let mut lock = stdout.lock();
    // result.pretty(&alloc).render(120, &mut lock)?;
    Ok(())
}
