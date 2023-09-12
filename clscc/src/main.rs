use common::Tokens;

pub mod common;
pub mod lexer;
pub mod parser;

fn main() {
    simplelog::TermLogger::init(
        #[cfg(debug_assertions)]
        simplelog::LevelFilter::Debug,
        #[cfg(not(debug_assertions))]
        simplelog::LevelFilter::Info,
        simplelog::Config::default(),
        simplelog::TerminalMode::Mixed,
        simplelog::ColorChoice::Auto,
    )
    .unwrap();
    let inp = r#"
void start() {
    int i, j;
    i = 0;
    return;
}
    "#;
    let tokens = lexer::lex(inp).unwrap();
    dbg!(&tokens);
    let ast = parser::AstNode::parse(Tokens::new(&tokens)).unwrap();
    dbg!(ast);
}
