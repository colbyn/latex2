use std::iter::FromIterator;
use std::collections::HashSet;
use nom::{
    IResult,
    bytes::complete::{tag, take_while_m_n, take_while},
    combinator::map_res,
    sequence::tuple,
    sequence::delimited,
    character::complete::char,
    bytes::complete::is_not,
    error::ParseError,
    character::complete::multispace0,
    combinator::recognize,
    sequence::pair,
    branch::alt,
    character::complete::{alpha1},
    character::complete::alphanumeric1,
    combinator::{cut, map, opt},
    error::{context, VerboseError},
    multi::{many0, many1},
    sequence::{preceded, terminated},
    character::complete::{digit1, multispace1, one_of},
    multi::separated_list1,
    Parser,
    combinator::all_consuming,
    bytes::complete::take_until,
    combinator::eof,
    multi::many_till,
    combinator::verify,
};
use crate::parser_utils::{identifier, ws, parens};

///////////////////////////////////////////////////////////////////////////////
// AST
///////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub enum Token {
    Text(String),
    MacroCall(String, Vec<Token>),
    MacroCallNoArg(String),
    EnvCall(String, Vec<Token>),
    InlineMath(Vec<Token>),
    BlockMath(Vec<Token>),
    BinOp(String),
}

///////////////////////////////////////////////////////////////////////////////
// AST ROOT PARSERS
///////////////////////////////////////////////////////////////////////////////

pub(crate) fn parse_terms(source: &str) -> Result<(&str, Vec<Token>), nom::Err<nom::error::Error<&str>>> {
    let (source, (ast, _)) = nom::multi::many_till(parse_term, eof)(source)?;
    Ok((source, ast))
}

pub(crate) fn parse_term(source: &str) -> Result<(&str, Token), nom::Err<nom::error::Error<&str>>> {
    alt((
        parse_bin_op,
        parse_block_math,
        parse_inline_math,
        parse_env_call,
        parse_macro_call,
        parse_text,
    ))(source)
}


///////////////////////////////////////////////////////////////////////////////
// TOKEN PARSER HELPERS
///////////////////////////////////////////////////////////////////////////////

pub(crate) fn parse_block(
    source: &str
) -> Result<(&str, Vec<Token>), nom::Err<nom::error::Error<&str>>>
{
    let (source, _) = tag("{")(source)?;
    let (source, (contents, _)) = many_till(parse_term, tag("}"))(source)?;
    let ast = contents;
    Ok((source, ast))
}

pub(crate) fn parse_ident_block(
    source: &str,
) -> Result<(&str, String), nom::Err<nom::error::Error<&str>>>
{
    let (source, ident) = delimited(char('{'), ws(identifier), char('}'))(source)?;
    Ok((source, ident))
}

///////////////////////////////////////////////////////////////////////////////
// TOKEN PARSERS
///////////////////////////////////////////////////////////////////////////////

pub(crate) fn parse_env_call(source: &str) -> Result<(&str, Token), nom::Err<nom::error::Error<&str>>> {
    pub fn parse_body(
        source: &str,
        ident: String,
    ) -> Result<(&str, Vec<Token>), nom::Err<nom::error::Error<&str>>> {
        let end_token = format!("\\end{{{}}}", ident);
        let (source, (ast, _)) = many_till(
            parse_term,
            tag(end_token.as_str()),
        )(source)?;
        Ok((source, ast))
    }
    let (source, _) = tag("\\")(source)?;
    let (source, _) = tag("begin")(source)?;
    let (source, ident) = parse_ident_block(source)?;
    let (source, contents) = parse_body(
        source,
        ident.clone(),
    )?;
    let ast = Token::EnvCall(ident, contents);
    Ok((source, ast))
}

pub(crate) fn parse_macro_call(source: &str) -> Result<(&str, Token), nom::Err<nom::error::Error<&str>>> {
    let is_keyword = |s: &str| match s {
        "begin" => false,
        "end" => false,
        _ => true,
    };
    let (source, _) = tag("\\")(source)?;
    let (source, ident) = verify(identifier, is_keyword)(source)?;
    let (source, contents) = opt(parse_block)(source)?;
    let ast = match contents {
        None => Token::MacroCallNoArg(ident),
        Some(contents) => Token::MacroCall(ident, contents),
    };
    Ok((source, ast))
}

pub(crate) fn parse_text(source: &str) -> Result<(&str, Token), nom::Err<nom::error::Error<&str>>> {
    let end_chars = HashSet::<char>::from_iter(vec![
        '\\',
        '{',
        '}',
        '$',
        '^',
        '*',
    ]);
    let pred = |c: char| !end_chars.contains(&c);
    let (source, contents) = take_while(pred)(source)?;
    let ast = Token::Text(contents.to_owned());
    Ok((source, ast))
}

pub(crate) fn parse_inline_math(source: &str) -> Result<(&str, Token), nom::Err<nom::error::Error<&str>>> {
    let (source, _) = tag("$")(source)?;
    let (source, (tks, _)) = many_till(
        parse_term,
        tag("$"),
    )(source)?;
    Ok((source, Token::InlineMath(tks)))
}

pub(crate) fn parse_block_math(source: &str) -> Result<(&str, Token), nom::Err<nom::error::Error<&str>>> {
    let (source, _) = tag("$$")(source)?;
    let (source, (tks, _)) = many_till(
        parse_term,
        tag("$$"),
    )(source)?;
    Ok((source, Token::BlockMath(tks)))
}

pub(crate) fn parse_bin_op(source: &str) -> Result<(&str, Token), nom::Err<nom::error::Error<&str>>> {
    let (source, tag) = alt((
        tag("^"),
        tag("*"),
    ))(source)?;
    Ok((source, Token::BinOp(tag.to_string())))
}


///////////////////////////////////////////////////////////////////////////////
// DEV
///////////////////////////////////////////////////////////////////////////////


pub fn main() {
    let source_path = "source.tex";
    let source = std::fs::read_to_string(source_path).unwrap();
    let (source, tks) = parse_terms(&source).unwrap();
    // let (source, tks) = parse_env_call(&source).unwrap();
    // let (source, tks) = parse_macro_call(&source).unwrap();
    // let (source, tks) = parse_inline_math(&source).unwrap();
    println!("{:#?}", tks);
}

