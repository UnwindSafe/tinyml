use analysis::run_passes;
use lexer::Lexer;

mod analysis;
mod lexer;
mod parser;

fn main() {
    // set the environment variable so that it always prints logs, maybe a better way.
    unsafe { std::env::set_var("RUST_LOG", "trace") };

    pretty_env_logger::init();

    // get the code for our example program.
    let code = include_str!("../examples/test.tml");

    let tokens = Lexer::new(code).scan().unwrap();

    // do nothing if there are no tokens.
    if tokens.is_empty() {
        return;
    }

    let mut ast = parser::Parser::new(tokens).parse().unwrap();

    run_passes(&mut ast);

    println!("Hello, world!: {:#?}", ast);
}
