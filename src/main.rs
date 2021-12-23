extern crate clap;
use clap::{App, Arg};

fn main() {
    let matches = App::new("Rust 8cc")
        .version("0.1")
        .arg(
            Arg::with_name("INPUT")
                .help("Sets input file")
                .required(true)
                .index(1),
        )
        .get_matches();

    println!("Using input file: {}", matches.value_of("INPUT").unwrap());
}
