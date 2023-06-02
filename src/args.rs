
use clap::{Parser, ValueEnum};

#[derive(Parser)]
pub struct Args {
    #[arg(value_enum, default_value_t=OutputMode::Gen)]
    pub output_mode: OutputMode
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
pub enum OutputMode {
    Lex,
    Ast,
    Gen,
}

