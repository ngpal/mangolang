use clap::Parser;
use std::{fs, process};

use crate::vm::{Vm, parse_program};
mod error;
mod vm;

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Cli {
    filename: String,

    /// Show only top of stack
    #[arg(short = 't', long)]
    show_top: bool,

    /// Show whole stack
    #[arg(short = 's', long)]
    show_stack: bool,

    /// Show whole locals array
    #[arg(short = 'l', long)]
    show_locals: bool,
}

fn main() {
    let cli = Cli::parse();
    let code = fs::read_to_string(&cli.filename).unwrap_or_else(|err| {
        eprintln!("failed to read {}: {}", cli.filename, err);
        process::exit(1);
    });

    let program = match parse_program(&code) {
        Ok(p) => p,
        Err(err) => {
            eprintln!("{}", err);
            process::exit(1);
        }
    };

    let mut vm = Vm::new();
    if let Err(err) = vm.exec(&program) {
        eprintln!("{}", err);
        process::exit(1);
    }

    if cli.show_top {
        let raw = vm.top();
        println!("top of stack: as u32 = {}, as i32 = {}", raw, raw as i32);
    }

    if cli.show_stack {
        println!("stack top = {}", vm.sp);
        for (i, raw) in vm.stack.iter().enumerate() {
            println!("stack[{}]: as u32 = {}, as i32 = {}", i, raw, *raw as i32);
        }
    }

    if cli.show_locals {
        for (i, raw) in vm.locals.iter().enumerate() {
            println!("locals[{}]: as u32 = {}, as i32 = {}", i, raw, *raw as i32);
        }
    }
}
