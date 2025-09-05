use std::{
    fs::File,
    io::{self, Read, Write, stdin, stdout},
    process,
};

mod codegen;
mod error;
mod lexer;
mod parser;
mod type_check;

use clap::{ArgGroup, CommandFactory, Parser};
use lexer::Lexer;

use crate::{
    codegen::{gen_asm, gen_bin, gen_instrs},
    type_check::check_types,
};

#[derive(Parser, Debug)]
#[command(name = "mango")]
#[command(group(
    ArgGroup::new("source")
        .required(true)
        .args(["input", "repl"]),
))]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[arg(value_name = "FILE")]
    input: Option<String>,

    #[arg(short, long)]
    repl: bool,

    /// Emit assembly instead of binary
    #[arg(short = 's', long)]
    asm: bool,

    #[arg(short, long, value_name = "OUTPUT")]
    output: Option<String>,
}

fn run_repl(emit_asm: bool) -> io::Result<()> {
    loop {
        let mut input = String::new();
        print!("> ");
        stdout().flush()?;
        stdin().read_line(&mut input)?;
        let input = input.trim();

        if input == "exit" {
            break;
        }

        match parser::parse(&mut Lexer::new(input).peekable())
            .and_then(|ast| check_types(ast))
            .and_then(|(ast, type_env, var_env)| gen_instrs(ast, type_env, var_env))
        {
            Ok(instrs) => {
                if emit_asm {
                    println!("{}", gen_asm(instrs));
                } else {
                    let bin = gen_bin(instrs);
                    println!("{:02X?}", bin);
                }
            }
            Err(err) => eprintln!("Error: {}", err),
        }
    }
    Ok(())
}

fn compile_file(filename: &str, output_path: &str, emit_asm: bool) -> io::Result<()> {
    let mut code_file = File::open(filename)?;
    let mut code = String::new();
    code_file.read_to_string(&mut code)?;

    match parser::parse(&mut Lexer::new(&code).peekable())
        .and_then(|ast| check_types(ast))
        .and_then(|(ast, type_env, var_env)| gen_instrs(ast, type_env, var_env))
    {
        Ok(instrs) => {
            let mut output = File::create(output_path)?;
            if emit_asm {
                output.write_all(gen_asm(instrs).as_bytes())?;
            } else {
                output.write_all(&gen_bin(instrs))?;
            }
        }
        Err(err) => {
            eprintln!("Compilation failed: {}", err);
            process::exit(1);
        }
    }

    Ok(())
}

fn main() -> io::Result<()> {
    let cli = Cli::parse();

    let emit_asm = cli.asm;
    let output = cli.output.unwrap_or_else(|| {
        if emit_asm {
            "out.masm".to_string()
        } else {
            "out.mbin".to_string()
        }
    });

    if cli.repl {
        run_repl(emit_asm)
    } else if let Some(filename) = cli.input {
        compile_file(&filename, &output, emit_asm)
    } else {
        Cli::command().print_help().unwrap();
        println!();
        process::exit(1);
    }
}
