
mod tokens;

fn main() {
    println!("{}{}", tokens::Token::KwProgram, tokens::Token::Ident(String::from("hello")));

    let s1: &'static str = "hello";
    let s2: &'static str = "hello";

    if s1 == s2 {
        println!("same");
    } else {
        println!("not same");
    }
}
