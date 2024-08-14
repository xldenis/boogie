use logos::{FilterResult, Lexer, Logos};

#[derive(Logos, Debug, PartialEq, Copy, Clone)]
#[logos(skip r"[ \t\n\f]+")] // Ignore whitespace
#[logos(skip r"//.*")] // Ignore line comments
// #[logos(skip r"\/\*([^*]|\*[^\/])+\*\/")] // Ignore block comments
pub enum Token<'a> {
    #[token("/*", multiline_comment)]
    MultiComment,
    Error,
    #[regex(r"([#$'\.?A-Z^-z~][#$'\.0-9?A-Z^-z~]*)", priority = 2)]
    Ident(&'a str),

    #[regex(r#""([^"\\]|\\.)*""#)]
    String(&'a str),

    #[regex(r"([0-9]+bv[0-9]+)")]
    BvLit(&'a str),

    #[regex(r"[0-9]+")]
    Digits(&'a str),

    #[regex(r"[0-9]+(e-?[0-9]+)?", priority = 3)]
    Decimal(&'a str),

    #[regex(r"([0-9]+\.[0-9]+(e-?[0-9]+)?|0(NaN|nan)[0-9]+e[0-9]+|0[+-]oo[0-9]+e[0-9]+)")]
    DecFloat(&'a str),

    #[regex(r"b[0-9]+e[-+]?[0-9]+")]
    BvFloat(&'a str),

    //  float = [ '-' ] '0' 'x' hexdigit {hexdigit} '.' hexdigit {hexdigit} 'e' [ '-' ] digit {digit} 'f' digit {digit} 'e' digit {digit}
    // | '0' 'N' 'a' 'N' digit {digit} 'e' digit {digit}
    // | '0' 'n' 'a' 'n' digit {digit} 'e' digit {digit}
    // | '0' '+' 'o' 'o' digit {digit} 'e' digit {digit}
    // | '0' '-' 'o' 'o' digit {digit} 'e' digit {digit} .
    // r"(-?0[x][0-9a-fA-F]+\.[0-9a-f]+e-?[0-9]+f[0-9]+e[0-9]+|0(NaN|nan)[0-9]+e[0-9]|[+-]oo[0-9]+e[0-9])"
    #[regex(r"0x[0-9a-fA-F]+\.[\-0-9a-f]+")]
    HexFloat(&'a str),

    // #[regex(r"([0-9]+\.[0-9]+[eE][-+]?[0-9]+[fF])")]
    // Float(&'a str),
    #[token("yield")]
    Yield,
    #[token("var")]
    Var,
    #[token(";")]
    Semicolon,
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("where")]
    Where,
    #[token("int")]
    Int,
    #[token("real")]
    Real,
    #[token("bool")]
    Bool,
    #[token("[")]
    LeftBracket,
    #[token("]")]
    RightBracket,
    #[token("<")]
    LessThan,
    #[token(">")]
    GreaterThan,
    #[token("const")]
    Const,
    #[token("unique")]
    Unique,
    #[token("uses")]
    Uses,
    #[token("{")]
    LeftBrace,
    #[token("}")]
    RightBrace,
    #[token("function")]
    Function,
    #[token("returns")]
    Returns,
    #[token("hideable")]
    Hideable,
    #[token("axiom")]
    Axiom,
    #[token("type")]
    Type,
    #[token("=")]
    Equals,
    #[token("datatype")]
    Datatype,
    #[token("invariant")]
    Invariant,
    #[token("pure")]
    Pure,
    #[token("async")]
    Async,
    #[token("action")]
    Action,
    #[token("creates")]
    Creates,
    #[token("refines")]
    Refines,
    #[token("using")]
    Using,
    #[token("left")]
    Left,
    #[token("right")]
    Right,
    #[token("both")]
    Both,
    #[token("atomic")]
    Atomic,
    #[token("procedure")]
    Procedure,
    #[token("asserts")]
    Asserts,
    #[token("requires")]
    Requires,
    #[token("ensures")]
    Ensures,
    #[token("preserves")]
    Preserves,
    #[token("implementation")]
    Implementation,
    #[token("free")]
    Free,
    #[token("modifies")]
    Modifies,
    #[token("goto")]
    Goto,
    #[token("return")]
    Return,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("while")]
    While,
    #[token("*")]
    Asterisk,
    #[token("break")]
    Break,
    #[token("reveal")]
    Reveal,
    #[token("hide")]
    Hide,
    #[token("pop")]
    Pop,
    #[token("push")]
    Push,
    #[token("assert")]
    Assert,
    #[token("assume")]
    Assume,
    #[token("havoc")]
    Havoc,
    #[token(":=")]
    Assign,
    #[token("->")]
    Arrow,
    #[token("call")]
    Call,
    #[token("par")]
    Par,
    #[token("|")]
    VerticalBar,
    #[token("<==>")]
    DoubleArrow,
    #[token("⇔")]
    LogicalEquivalence,
    #[token("==>")]
    Implies,
    #[token("⇒")]
    ImpliesSymbol,
    #[token("<==")]
    ReverseImplies,
    #[token("⇐")]
    ReverseImpliesSymbol,
    #[token("&&")]
    LogicalAnd,
    #[token("∧")]
    LogicalAndSymbol,
    #[token("||")]
    LogicalOr,
    #[token("∨")]
    LogicalOrSymbol,
    #[token("==")]
    Equality,
    #[token("<=")]
    LessThanOrEqual,
    #[token(">=")]
    GreaterThanOrEqual,
    #[token("!=")]
    NotEqual,
    #[token("≠")]
    NotEqualSymbol,
    #[token("≤")]
    LessThanOrEqualSymbol,
    #[token("≥")]
    GreaterThanOrEqualSymbol,
    #[token("++")]
    Increment,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("div")]
    Div,
    #[token("mod")]
    Mod,
    #[token("/")]
    Division,
    #[token("**")]
    Exponentiation,
    #[token("is")]
    Is,
    #[token("!")]
    ExclamationMark,
    #[token("¬")]
    NegationSymbol,
    #[token("false")]
    False,
    #[token("true")]
    True,
    #[token("RNE")]
    RNE,
    #[token("RNA")]
    RNA,
    #[token("RTP")]
    RTP,
    #[token("RTN")]
    RTN,
    #[token("RTZ")]
    RTZ,
    #[token("old")]
    Old,
    #[token("|{")]
    LeftCodeBlock,
    #[token("}|")]
    RightCodeBlock,
    #[token("then")]
    Then,
    #[token("forall")]
    Forall,
    #[token("∀")]
    UniversalQuantifier,
    #[token("exists")]
    Exists,
    #[token("∃")]
    ExistentialQuantifier,
    #[token("lambda")]
    Lambda,
    #[token("λ")]
    LambdaSymbol,
    #[token("::")]
    DoubleColon,
    #[token("•")]
    Bullet,
}


// TODO: Handle nested ocmments
fn multiline_comment<'a>(lex: &mut Lexer<'a, Token<'a>>) -> FilterResult<(), ()> {
    enum State {
        ExpectStar,
        ExpectSlash,
    }
    let remainder = lex.remainder();
    let (mut state, mut iter) = (State::ExpectStar, remainder.chars());
    while let Some(next_char) = iter.next() {
        match next_char {
            '\n' => {
                // lex.extras.line += 1;
                // lex.extras.line_beg = lex.span().end + (remainder.len() - iter.as_str().len());
                state = State::ExpectStar;
            }
            '*' => state = State::ExpectSlash,
            '/' if matches!(state, State::ExpectSlash) => {
                lex.bump(remainder.len() - iter.as_str().len());
                return FilterResult::Skip;
            }
            _ => state = State::ExpectStar,
        }
    }
    lex.bump(remainder.len());
    FilterResult::Error(())
}

use std::fmt;

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Error => write!(f, "Error"),
            Token::Ident(s) => write!(f, "{}", s),
            Token::BvLit(s) => write!(f, "{s}"),
            Token::Digits(s) => write!(f, "{s}"),
            // TODO: Escape string
            Token::String(s) => write!(f, "{}", s),
            Token::Decimal(s) => write!(f, "{s}"),
            Token::DecFloat(s) => write!(f, "{s}"),
            Token::BvFloat(s) => write!(f, "{s}"),
            Token::HexFloat(s) => write!(f, "{s}"),
            // Token::Float(s) => write!(f, "{s}"),
            Token::Yield => write!(f, "yield"),
            Token::Var => write!(f, "var"),
            Token::Semicolon => write!(f, ";"),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::Colon => write!(f, ":"),
            Token::Comma => write!(f, ","),
            Token::Where => write!(f, "where"),
            Token::Int => write!(f, "int"),
            Token::Real => write!(f, "real"),
            Token::Bool => write!(f, "bool"),
            Token::LeftBracket => write!(f, "["),
            Token::RightBracket => write!(f, "]"),
            Token::LessThan => write!(f, "<"),
            Token::GreaterThan => write!(f, ">"),
            Token::Const => write!(f, "const"),
            Token::Unique => write!(f, "unique"),
            Token::Uses => write!(f, "uses"),
            Token::LeftBrace => write!(f, "{{"),
            Token::RightBrace => write!(f, "}}"),
            Token::Function => write!(f, "function"),
            Token::Returns => write!(f, "returns"),
            Token::Hideable => write!(f, "hideable"),
            Token::Axiom => write!(f, "axiom"),
            Token::Type => write!(f, "type"),
            Token::Equals => write!(f, "="),
            Token::Datatype => write!(f, "datatype"),
            Token::Invariant => write!(f, "invariant"),
            Token::Pure => write!(f, "pure"),
            Token::Async => write!(f, "async"),
            Token::Action => write!(f, "action"),
            Token::Creates => write!(f, "creates"),
            Token::Refines => write!(f, "refines"),
            Token::Using => write!(f, "using"),
            Token::Left => write!(f, "left"),
            Token::Right => write!(f, "right"),
            Token::Both => write!(f, "both"),
            Token::Atomic => write!(f, "atomic"),
            Token::Procedure => write!(f, "procedure"),
            Token::Asserts => write!(f, "asserts"),
            Token::Requires => write!(f, "requires"),
            Token::Ensures => write!(f, "ensures"),
            Token::Preserves => write!(f, "preserves"),
            Token::Implementation => write!(f, "implementation"),
            Token::Free => write!(f, "free"),
            Token::Modifies => write!(f, "modifies"),
            Token::Goto => write!(f, "goto"),
            Token::Return => write!(f, "return"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::While => write!(f, "while"),
            Token::Asterisk => write!(f, "*"),
            Token::Break => write!(f, "break"),
            Token::Reveal => write!(f, "reveal"),
            Token::Hide => write!(f, "hide"),
            Token::Pop => write!(f, "pop"),
            Token::Push => write!(f, "push"),
            Token::Assert => write!(f, "assert"),
            Token::Assume => write!(f, "assume"),
            Token::Havoc => write!(f, "havoc"),
            Token::Assign => write!(f, ":="),
            Token::Arrow => write!(f, "->"),
            Token::Call => write!(f, "call"),
            Token::Par => write!(f, "par"),
            Token::VerticalBar => write!(f, "|"),
            Token::DoubleArrow => write!(f, "<=>"),
            Token::LogicalEquivalence => write!(f, "⇔"),
            Token::Implies => write!(f, "==>"),
            Token::ImpliesSymbol => write!(f, "⇒"),
            Token::ReverseImplies => write!(f, "<=="),
            Token::ReverseImpliesSymbol => write!(f, "⇐"),
            Token::LogicalAnd => write!(f, "&&"),
            Token::LogicalAndSymbol => write!(f, "∧"),
            Token::LogicalOr => write!(f, "||"),
            Token::LogicalOrSymbol => write!(f, "∨"),
            Token::Equality => write!(f, "=="),
            Token::LessThanOrEqual => write!(f, "<="),
            Token::GreaterThanOrEqual => write!(f, ">="),
            Token::NotEqual => write!(f, "!="),
            Token::NotEqualSymbol => write!(f, "≠"),
            Token::LessThanOrEqualSymbol => write!(f, "≤"),
            Token::GreaterThanOrEqualSymbol => write!(f, "≥"),
            Token::Increment => write!(f, "++"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Div => write!(f, "div"),
            Token::Mod => write!(f, "mod"),
            Token::Division => write!(f, "/"),
            Token::Exponentiation => write!(f, "**"),
            Token::Is => write!(f, "is"),
            Token::ExclamationMark => write!(f, "!"),
            Token::NegationSymbol => write!(f, "¬"),
            Token::False => write!(f, "false"),
            Token::True => write!(f, "true"),
            Token::RNE => write!(f, "RNE"),
            Token::RNA => write!(f, "RNA"),
            Token::RTP => write!(f, "RTP"),
            Token::RTN => write!(f, "RTN"),
            Token::RTZ => write!(f, "RTZ"),
            Token::Old => write!(f, "old"),
            Token::LeftCodeBlock => write!(f, "|{{"),
            Token::RightCodeBlock => write!(f, "}}|"),
            Token::Then => write!(f, "then"),
            Token::Forall => write!(f, "forall"),
            Token::UniversalQuantifier => write!(f, "∀"),
            Token::Exists => write!(f, "exists"),
            Token::ExistentialQuantifier => write!(f, "∃"),
            Token::Lambda => write!(f, "lambda"),
            Token::LambdaSymbol => write!(f, "λ"),
            Token::DoubleColon => write!(f, "::"),
            Token::Bullet => write!(f, "•"),
            Token::MultiComment => write!(f, "comment")
        }
    }
}
