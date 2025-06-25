use lexer::Lexer;

mod lexer;

fn main() {
    // get the code for our example program.
    let code = include_str!("../examples/test.tml");

    let tokens = Lexer::new(code).scan();

    println!("{:?}", tokens);

    println!("Hello, world!");
}
