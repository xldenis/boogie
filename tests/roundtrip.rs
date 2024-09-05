use boogie::ast::Program;
use boogie::lexer::Token;
use boogie::parser;
use chumsky::input::{Input as _, Stream};
use chumsky::Parser as _;
use color_eyre::eyre::Result;
use logos::Logos as _;
use pretty::BoxAllocator;
use pretty::Pretty;
use std::fs::read_to_string;
use std::path::Path;
use walkdir::WalkDir;

// Import necessary modules from your project
// use crate::{Token, Stream, parser, Pretty, BoxAllocator};

static DENIED: &[&str] = &[
    "boogie-orig/Test/test0/SeparateVerification0.bpl",
    "boogie-orig/Test/test0/AttributeParsingErr.bpl",
    "boogie-orig/Test/test0/LineResolve.bpl",
    "boogie-orig/Test/test0/LineParse.bpl",
    "boogie-orig/Test/inline/codeexpr.bpl",
    "boogie-orig/Test/codeexpr/codeExprBug.bpl",
    "boogie-orig/Test/codeexpr/CodeExpr1.bpl",
    "boogie-orig/Test/codeexpr/CodeExpr0.bpl",
    "boogie-orig/Test/codeexpr/CodeExpr2.bpl",
];

fn main() -> Result<()> {
    color_eyre::install()?;

    let test_dir = Path::new("boogie-orig/Test/");
    let mut failed_tests = Vec::new();
    let mut num_tests = 0;
    for entry in WalkDir::new(test_dir).into_iter().filter_map(|e| e.ok()) {
        let path = entry.path();
        if path.extension().map_or(false, |ext| ext == "bpl") {
            if DENIED.contains(&path.to_str().unwrap()) {
                continue;
            }

            match run_roundtrip_test(path) {
                Ok(_) => println!("Test passed: {}", path.display()),
                Err(e) => {
                    println!("Test failed: {}\nError: {}", path.display(), e);
                    failed_tests.push(path.to_path_buf());
                }
            }
            num_tests += 1;
        }
    }

    if failed_tests.is_empty() {
        println!("All tests passed successfully!");
    } else {
        println!("{} / {} failed tests:", failed_tests.len(), num_tests);
        for test in failed_tests {
            println!("- {}", test.display());
        }
        std::process::exit(1);
    }

    Ok(())
}

fn run_roundtrip_test(path: &Path) -> Result<()> {
    let file_content = read_to_string(path)?;

    // First parse
    let parse_result1 = parse_content(&file_content)?;

    // Pretty print
    let pretty_printed = pretty_print(&parse_result1)?;

    // Second parse
    let parse_result2 = parse_content(&pretty_printed)?;

    // Compare parse results
    if parse_result1 != parse_result2 {
        return Err(color_eyre::eyre::eyre!(
            "Roundtrip test failed: parse results do not match"
        ));
    }

    Ok(())
}

fn parse_content(content: &str) -> Result<Program> {
    let tokens = Token::lexer(content)
        .spanned()
        .map(|(tok, span)| match tok {
            Ok(tok) => (tok, span.into()),
            Err(()) => (Token::Error, span.into()),
        });

    let stream = Stream::from_iter(tokens).spanned((content.len()..content.len()).into());
    let parse = parser::program().parse(stream);

    match parse.into_result() {
        Ok(result) => Ok(result),
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
                    .eprint(Source::from(content))?;
            }
            Err(color_eyre::eyre::eyre!("Parsing failed"))
        }
    }
}

fn pretty_print(ast: &Program) -> Result<String> {
    let alloc = BoxAllocator;
    let mut output = Vec::new();
    ast.pretty(&alloc).render(120, &mut output)?;
    Ok(String::from_utf8(output)?)
}
