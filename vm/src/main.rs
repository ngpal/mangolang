mod core;
mod debug;
mod disk;
mod error;
mod instr;
mod video;

use crate::{core::Vm, debug::Debugger, disk::DiskDriver};
use clap::{Parser, Subcommand};
use std::{fs, process};

#[derive(Subcommand, Debug)]
enum Command {
    // Run a program on the VM (default)
    Run {
        #[arg(short = 'd', long)]
        debug: bool,
    },

    // Write data to a disk sector
    Dw {
        #[arg(help = "Path to binary file to write")]
        src: String,
        #[arg(help = "Sector address (hex or decimal)")]
        sector: String,
        #[arg(short = 's', long)]
        strict: bool,
    },

    // Read data from a disk sector
    Dr {
        #[arg(help = "Sector address (hex or decimal)")]
        sector: String,
        #[arg(help = "Output path to save the sector data")]
        out: String,
    },
}

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Command::Run { debug } => {
            let mut vm = Vm::new();
            vm.load().unwrap_or_else(|err| {
                eprintln!("failed to load program: {}", err);
                process::exit(1);
            });

            if debug {
                println!("entering debugger");
                if let Err(err) = Debugger::new(&mut vm).run() {
                    eprintln!("{}", err);
                }
            } else {
                if let Err(err) = vm.run() {
                    eprintln!("{}", err);
                }
            }
        }

        Command::Dw {
            src,
            sector,
            strict,
        } => {
            let addr = parse_sector(&sector);
            let data = fs::read(&src).expect("failed to read source file");

            // check magic number
            if &data[0..4] != b"MBIN" {
                eprintln!("error: file does not start with 'MBIN' magic number");
                process::exit(1);
            }

            let payload = &data[4..];
            let mut disk = DiskDriver::new();

            if strict {
                // strict: must fit within one sector
                if payload.len() > 256 {
                    eprintln!(
                        "strict mode: file too large ({} bytes > 256)",
                        payload.len()
                    );
                    process::exit(1);
                }

                let mut buf = [0u8; 256];
                buf[..payload.len()].copy_from_slice(payload);
                if disk.write_sector(&buf, addr) {
                    disk.persist_table();
                    println!("wrote {} bytes to sector 0x{:04X}", payload.len(), addr);
                } else {
                    eprintln!("failed to write sector 0x{:04X}", addr);
                }
            } else {
                // flexible: spill across multiple sectors
                let total_sectors = (payload.len() + 255) / 256;
                for i in 0..total_sectors {
                    let start = i * 256;
                    let end = (start + 256).min(payload.len());
                    let mut buf = [0u8; 256];
                    buf[..end - start].copy_from_slice(&payload[start..end]);
                    if !disk.write_sector(&buf, addr + i as u16) {
                        eprintln!("failed to write sector 0x{:04X}", addr + i as u16);
                        process::exit(1);
                    }
                }
                disk.persist_table();
                println!(
                    "wrote {} sector(s) ({} bytes total) starting from 0x{:04X}",
                    total_sectors,
                    payload.len(),
                    addr
                );
            }
        }

        Command::Dr { sector, out } => {
            let addr = parse_sector(&sector);
            let disk = DiskDriver::new();
            let data = disk.get_sector(addr);
            fs::write(&out, &data).expect("failed to write output");
            println!("dumped sector 0x{:04X} -> {}", addr, out);
        }
    }
}

fn parse_sector(s: &str) -> u16 {
    if s.starts_with("0x") {
        u16::from_str_radix(&s[2..], 16).unwrap()
    } else {
        s.parse::<u16>().unwrap()
    }
}
