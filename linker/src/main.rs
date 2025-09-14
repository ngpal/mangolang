use clap::{Parser, command};
use computils::error::{LinkerError, LinkerResult};
use std::{collections::HashMap, fs, io};

const OBJ_FILE_VERSION: u16 = 1;
const VERSION_OFST: usize = 4;
const INSTR_LEN_OFST: usize = 6;
const SYM_LEN_OFST: usize = 8;
const RELOC_LEN_OFST: usize = 10;
const INSTR_BEGIN_OFST: usize = 12;

fn read_u16(object: &[u8], idx: usize) -> u16 {
    object[idx] as u16 | ((object[idx + 1] as u16) << 8)
}

pub fn link_objects(objects: Vec<Vec<u8>>) -> LinkerResult<Vec<u8>> {
    let mut instr_blob = Vec::new(); // instructions only
    let mut symbols: HashMap<String, u16> = HashMap::new();
    let mut relocs: Vec<(usize, String, u8)> = Vec::new();

    let mut instr_base = 0usize;

    for object in objects {
        // --- validate object ---
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

        // --- copy instructions ---
        let obj_instrs = &object[INSTR_BEGIN_OFST..INSTR_BEGIN_OFST + instr_len];
        instr_blob.extend_from_slice(obj_instrs);

        // --- read symbols ---
        let mut obj_symbols = Vec::new();
        let mut ofs = 0;
        let sym_start = INSTR_BEGIN_OFST + instr_len;
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
                addr += instr_base as u16;
            }
            obj_symbols.push((name, addr));
        }

        for (name, addr) in &obj_symbols {
            symbols.insert(name.clone(), *addr);
        }

        // --- read relocations ---
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
    }

    // --- apply relocations with debug check ---
    for (instr_ofst, sym_name, kind) in &relocs {
        let addr = symbols
            .get(sym_name)
            .ok_or(LinkerError(format!("unresolved symbol {}", sym_name)))?;

        if *addr == 0xFFFF {
            return Err(LinkerError(format!("symbol {} still unresolved", sym_name)));
        }

        // let placeholder = instr_blob[*instr_ofst];
        // if placeholder != 0xFF {
        //     return Err(LinkerError(format!(
        //         "expected placeholder 0xFF at offset {}, found 0x{:02X}",
        //         instr_ofst, placeholder
        //     )));
        // }

        if *kind == 1 {
            let offset = (*addr as isize - (*instr_ofst + 1) as isize) as i8;
            instr_blob[*instr_ofst] = offset as u8;
        } else {
            let bytes = addr.to_le_bytes();
            instr_blob[*instr_ofst] = bytes[0];
            instr_blob[*instr_ofst + 1] = bytes[1];
        }
    }

    // --- prepend MBIN header ---
    let mut mbin = b"MBIN".to_vec();
    // optionally append version or instruction length here if needed
    mbin.extend_from_slice(&instr_blob);

    Ok(mbin)
}

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Cli {
    /// input object files
    #[arg(value_name = "FILES")]
    input: Vec<String>,

    /// output binary filename
    #[arg(short, long)]
    output: Option<String>,
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

    let binary = link_objects(objects).unwrap_or_else(|e| {
        eprintln!("Link error: {:?}", e);
        std::process::exit(1);
    });

    // default output: a.out
    let output = cli.output.unwrap_or_else(|| "a.out".to_string());
    fs::write(&output, binary)?;
    println!("wrote binary to {}", output);

    Ok(())
}
