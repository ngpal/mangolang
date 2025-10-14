mod error;

use clap::{Parser, command};
use std::{collections::HashMap, fs, io};

use crate::error::{LinkerError, LinkerResult};

const OBJ_FILE_VERSION: u16 = 2;
const VERSION_OFST: usize = 4;
const INSTR_LEN_OFST: usize = 6;
const DATA_LEN_OFST: usize = 8;
const SYM_LEN_OFST: usize = 10;
const RELOC_LEN_OFST: usize = 12;
const INSTR_BEGIN_OFST: usize = 14;

fn read_u16(object: &[u8], idx: usize) -> u16 {
    object[idx] as u16 | ((object[idx + 1] as u16) << 8)
}

pub fn link_objects(objects: Vec<Vec<u8>>, base: u16) -> LinkerResult<Vec<u8>> {
    let mut instr_blob = Vec::new(); // instructions only
    let mut data_blob = Vec::new();
    let mut symbols: HashMap<String, u16> = HashMap::new();
    let mut relocs: Vec<(usize, String, u8)> = Vec::new();

    let mut instr_base = 0usize;
    let mut data_base = 0usize;

    for object in objects {
        // validate object
        if &object[0..VERSION_OFST] != b"MOBJ" {
            return Err(LinkerError("invalid object file (missing MOBJ)".into()));
        }
        let version = read_u16(&object, VERSION_OFST);
        if version != OBJ_FILE_VERSION {
            return Err(LinkerError(format!(
                "object file version ({}) does not match supported version ({})",
                version, OBJ_FILE_VERSION
            )));
        }

        let instr_len = read_u16(&object, INSTR_LEN_OFST) as usize;
        let sym_len = read_u16(&object, SYM_LEN_OFST) as usize;
        let reloc_len = read_u16(&object, RELOC_LEN_OFST) as usize;

        // copy instructions
        let obj_instrs = &object[INSTR_BEGIN_OFST..INSTR_BEGIN_OFST + instr_len];
        instr_blob.extend_from_slice(obj_instrs);

        // copy data
        let data_len = read_u16(&object, DATA_LEN_OFST) as usize;
        let data_start = INSTR_BEGIN_OFST + instr_len;
        let obj_data = &object[data_start..data_start + data_len];
        data_blob.extend_from_slice(obj_data);

        // read symbols
        let mut obj_symbols = Vec::new();
        let mut ofs = 0;
        let sym_start = data_start + data_len;
        while ofs < sym_len {
            let mut name = String::new();
            while object[sym_start + ofs] != 0 {
                name.push(object[sym_start + ofs] as char);
                ofs += 1;
            }
            ofs += 1; // skip \0

            let mut addr = read_u16(&object, sym_start + ofs);
            ofs += 2;
            if addr != 0xFFFF {
                // its a
                if addr >= instr_len as u16 {
                    let offset = addr - instr_len as u16;
                    addr = data_base as u16 + offset;
                } else {
                    addr += instr_base as u16;
                }
            }
            obj_symbols.push((name, addr));
        }

        for (name, addr) in &obj_symbols {
            symbols.insert(name.clone(), *addr);
        }

        // read relocations
        ofs = 0;
        let reloc_start = sym_start + sym_len;
        while ofs < reloc_len {
            let instr_ofst = read_u16(&object, reloc_start + ofs) as usize;
            ofs += 2;

            let sym_idx = read_u16(&object, reloc_start + ofs) as usize;
            ofs += 2;

            let kind = object[reloc_start + ofs];
            ofs += 1;

            let global_instr_ofst = instr_ofst + instr_base;
            let sym_name = obj_symbols[sym_idx].0.clone();
            relocs.push((global_instr_ofst, sym_name, kind));
        }

        instr_base += instr_len;
        data_base += data_len;
    }

    // apply relocations with debug check
    for (instr_ofst, sym_name, kind) in &relocs {
        let addr = symbols
            .get(sym_name)
            .ok_or(LinkerError(format!("unresolved symbol {}", sym_name)))?;

        if *addr == 0xFFFF {
            return Err(LinkerError(format!("symbol {} unresolved", sym_name)));
        }

        if *kind == 0 {
            // Abs16
            let bytes = (addr + base).to_le_bytes();
            instr_blob[*instr_ofst] = bytes[0];
            instr_blob[*instr_ofst + 1] = bytes[1];
        } else if *kind == 1 {
            // Rel16
            let next_addr = *instr_ofst + 2; // 2 byte operand
            let rel = (*addr as isize - next_addr as isize) as i16;
            let offset = rel.to_le_bytes();
            instr_blob[*instr_ofst] = offset[0];
            instr_blob[*instr_ofst + 1] = offset[1];
        } else if *kind == 2 {
            // Data16
            let bytes = (addr + instr_base as u16 + base).to_le_bytes();
            instr_blob[*instr_ofst] = bytes[0];
            instr_blob[*instr_ofst + 1] = bytes[1];
        } else {
            unreachable!("unknown kind {}", *kind)
        }
    }

    // prepend MBIN header
    let mut mbin = b"MBIN".to_vec();
    mbin.extend_from_slice(&instr_blob);
    mbin.extend(&data_blob);

    Ok(mbin)
}

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Cli {
    // input object files
    #[arg(value_name = "FILES")]
    input: Vec<String>,

    // output binary filename
    #[arg(short, long)]
    output: Option<String>,

    // base address modification
    #[arg(short, long)]
    base: Option<String>,
}

fn parse_base(base: &str) -> u16 {
    if let Some(hex) = base.strip_prefix("0x") {
        u16::from_str_radix(hex, 16).unwrap_or_else(|_| {
            eprintln!("invalid hex base address: {}", base);
            std::process::exit(1);
        })
    } else {
        base.parse::<u16>().unwrap_or_else(|_| {
            eprintln!("invalid decimal base address: {}", base);
            std::process::exit(1);
        })
    }
}

fn main() -> io::Result<()> {
    let cli = Cli::parse();

    if cli.input.is_empty() {
        eprintln!("linker requires at least one input object file");
        std::process::exit(1);
    }

    let mut objects = Vec::new();
    for path in &cli.input {
        let buf = fs::read(path)?;
        objects.push(buf);
    }

    let base = if let Some(ofst) = cli.base {
        parse_base(&ofst)
    } else {
        // user code base
        0x0910
    };

    let binary = link_objects(objects, base).unwrap_or_else(|e| {
        eprintln!("Link error: {:?}", e);
        std::process::exit(1);
    });

    // default output: a.out
    let output = cli.output.unwrap_or_else(|| "a.out".to_string());
    fs::write(&output, binary)?;
    println!("wrote binary to {}; base offset 0x{:04X}", output, base);

    Ok(())
}
