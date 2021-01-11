use crate::parser::{Token, parse_terms};

#[derive(Debug, Clone)]
pub enum Ast {
    
}


impl Ast {
    pub fn from_tokens(tks: Vec<Token>) -> Self {
        unimplemented!()
    }
}



///////////////////////////////////////////////////////////////////////////////
// DEV
///////////////////////////////////////////////////////////////////////////////


pub fn main() {
    let source_path = "source.tex";
    let source = std::fs::read_to_string(source_path).unwrap();
    let (_, tks) = parse_terms(&source).unwrap();
    let ast = Ast::from_tokens(tks);
    // let (source, ast) = parse_env_call(&source).unwrap();
    // let (source, ast) = parse_macro_call(&source).unwrap();
    // let (source, ast) = parse_inline_math(&source).unwrap();
    // println!("{:#?}", ast);
}

