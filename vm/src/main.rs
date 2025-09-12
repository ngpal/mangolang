mod core;
mod debug;
mod error;
mod instr;
mod video;

use crate::{core::Vm, debug::Debugger};
use clap::Parser;
use std::{fs, process};

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
    let program = fs::read(&cli.filename).unwrap_or_else(|err| {
        eprintln!("failed to read {}: {}", cli.filename, err);
        process::exit(1);
    });

    let mut vm = Vm::new();
    vm.load(program).unwrap_or_else(|err| {
        eprintln!("failed to load program: {}", err);
        process::exit(1);
    });

    if cli.debugger {
        println!("Entering debugger");
        if let Err(err) = Debugger::new(&mut vm).run() {
            println!("{}", err);
        }
        process::exit(0);
    }

    // always run in video mode
    let mut video = video::VideoMemory::new();
    if let Err(err) = video.run(&mut vm) {
        eprintln!("video error: {}", err);
        process::exit(1);
    }

    // optional stack inspection
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
