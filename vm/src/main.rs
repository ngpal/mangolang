use clap::Parser;
use std::{fs, process};

use crate::{
    debug::Debugger,
    vm::{Vm, parse_program},
};
mod debug;
mod error;
mod vm;

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Cli {
    filename: String,

    // Show only top of stack
    #[arg(short = 't', long)]
    show_top: bool,

    // Show whole stack
    #[arg(short = 's', long)]
    show_stack: bool,

    // Debugger
    #[arg(short = 'd', long)]
    debugger: bool,
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

    let mut vm = Vm::new(&program).expect("memory overflow - too many local variables");
    if cli.debugger {
        println!("Entering debugger");
        if let Err(err) = Debugger::new(vm).debug() {
            println!("{}", err)
        };

        process::exit(0);
    } else {
        if let Err(err) = vm.exec() {
            eprintln!("{}", err);
            process::exit(1);
        }
    }
    if cli.show_top {
        let raw = vm.top_word();
        println!("top of stack: as u16 = {}, as i16 = {}", raw, raw as i16);
    }

    if cli.show_stack {
        println!("stack top = {}", vm.top_word());
        for (i, raw) in vm.memory.iter().enumerate() {
            println!("stack[{}]: as u16 = {}, as i16 = {}", i, raw, *raw as i16);
        }
    }
}
