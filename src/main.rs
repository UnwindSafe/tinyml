use lexer::Lexer;

mod lexer;
mod parser;

fn main() {
    // set the environment variable so that it always prints logs, maybe a better way.
    unsafe { std::env::set_var("RUST_LOG", "trace") };

    pretty_env_logger::init();

    // get the code for our example program.
    let code = include_str!("../examples/test.tml");

    let tokens = Lexer::new(code).scan().unwrap();

    let ast = parser::Parser::new(tokens);

    println!("Hello, world!");
}
