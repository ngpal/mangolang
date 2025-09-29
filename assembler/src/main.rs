mod error;
mod parser;
mod parser_old;
mod tokenizer;

use clap::{Parser, command};
use std::{collections::HashMap, fs, io};

use crate::{
    error::{AssemblerError, AssemblerResult},
    parser::{Assembly, Format, Instr, Mnemonic, parse_assembly},
};

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum RelocType {
    Abs16 = 0,
    Rel8 = 1, // relative 8-bit displacement (e.g. jmp/jlt/jgt/jeq)
    Data = 2,
}

#[derive(Debug, Clone)]
pub struct Reloc {
    pub offset: u16,      // byte offset into emitted code where operand bytes start
    pub sym_name: String, // symbolic name to resolve at link time
    pub kind: RelocType,
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub val: u16,
}

type Symbols = Vec<Symbol>;
type Relocs = Vec<Reloc>;

#[derive(Debug)]
pub struct Object {
    pub instrs: Vec<Instr>, // Changed from Vec<Mnemonic> to Vec<Instr>
    pub data: Vec<(String, String)>,
    pub symbols: Symbols,
    pub relocs: Relocs,
}

const OBJ_FILE_VERSION: u16 = 2;

impl Object {
    pub fn to_bin(&self) -> Vec<u8> {
        let mut out = Vec::new();

        // header: magic + version
        out.extend([b'M', b'O', b'B', b'J']);
        out.extend(OBJ_FILE_VERSION.to_le_bytes());

        // instructions bytes
        let instr_bytes = gen_bin(&self.instrs);
        out.extend((instr_bytes.len() as u16).to_le_bytes());

        // data bytes
        let mut data_bytes: Vec<u8> = Vec::new();
        for (_, value) in &self.data {
            data_bytes.extend(value.bytes());
        }
        out.extend((data_bytes.len() as u16).to_le_bytes());

        // symtable bytes (name\0 + u16 addr)
        let mut symtab_bytes = Vec::new();
        for Symbol { name, val } in &self.symbols {
            symtab_bytes.extend(name.bytes());
            symtab_bytes.push(0u8);
            symtab_bytes.extend(val.to_le_bytes());
        }
        out.extend((symtab_bytes.len() as u16).to_le_bytes());

        // reloctable bytes: need to convert sym_name -> sym_index
        let mut reloctab_bytes = Vec::new();
        for Reloc {
            offset,
            sym_name,
            kind,
        } in &self.relocs
        {
            let sym_index = self
                .symbols
                .iter()
                .position(|s| s.name == *sym_name)
                .expect("symbol referenced in reloc missing from symbol table")
                as u16;
            reloctab_bytes.extend(offset.to_le_bytes());
            reloctab_bytes.extend(sym_index.to_le_bytes());
            reloctab_bytes.push(*kind as u8);
        }
        out.extend((reloctab_bytes.len() as u16).to_le_bytes());

        // payload
        out.extend(instr_bytes);
        out.extend(data_bytes);
        out.extend(symtab_bytes);
        out.extend(reloctab_bytes);

        out
    }
}

impl Mnemonic {
    fn byte_len(&self) -> usize {
        match self {
            // All standard instructions are 16-bit (2 bytes) in the new ISA
            Mnemonic::Add
            | Mnemonic::Sub
            | Mnemonic::And
            | Mnemonic::Or
            | Mnemonic::Xor
            | Mnemonic::Shl
            | Mnemonic::Shr
            | Mnemonic::Mov
            | Mnemonic::AddI
            | Mnemonic::SubI
            | Mnemonic::AndI
            | Mnemonic::OrI
            | Mnemonic::XorI
            | Mnemonic::ShlI
            | Mnemonic::ShrI
            | Mnemonic::MovI
            | Mnemonic::Ldw
            | Mnemonic::Stw
            | Mnemonic::Ldb
            | Mnemonic::Stb
            | Mnemonic::Jmp
            | Mnemonic::Jeq
            | Mnemonic::Jlt
            | Mnemonic::Jgt
            | Mnemonic::Ret
            | Mnemonic::NoOp
            | Mnemonic::Halt => 2,

            // E-Type instructions are 2 words (4 bytes total)
            Mnemonic::JmpW | Mnemonic::JeqW | Mnemonic::JltW | Mnemonic::JgtW | Mnemonic::Call => 4,

            // Pseudo instructions have no size or resolve to their concrete counterparts
            Mnemonic::Lbl(_) => 0,
            Mnemonic::CallLbl(_) => 4, // becomes CALL (E-type)
            Mnemonic::JmpLbl(_) => 2,  // becomes JMP (B-type)
            Mnemonic::JltLbl(_) => 2,  // becomes JLT (B-type)
            Mnemonic::JgtLbl(_) => 2,  // becomes JGT (B-type)
            Mnemonic::JeqLbl(_) => 2,  // becomes JEQ (B-type)
            Mnemonic::JmpWLbl(_) => 4, // becomes JMPW (E-type)
            Mnemonic::JltWLbl(_) => 4, // becomes JLTW (E-type)
            Mnemonic::JgtWLbl(_) => 4, // becomes JGTW (E-type)
            Mnemonic::JeqWLbl(_) => 4, // becomes JEQW (E-type)
            Mnemonic::Data(_, _) => 4, // becomes CALL (E-type) for data reference
        }
    }
}

// Now gen_bin can access both mnemonic and format information
pub fn gen_bin(instrs: &Vec<Instr>) -> Vec<u8> {
    let mut code = Vec::new();

    for instr in instrs {
        let bytes = match (&instr.mnemonic, &instr.format) {
            // R-Type instructions
            (Mnemonic::Add, Format::Rfmt { reserved, rd, rs }) => {
                let opcode = 0b00001u16;
                let instr_word =
                    (opcode << 11) | ((*reserved as u16) << 6) | ((*rd as u16) << 3) | (*rs as u16);
                instr_word.to_le_bytes().to_vec()
            }
            (Mnemonic::Sub, Format::Rfmt { reserved, rd, rs }) => {
                let opcode = 0b00001u16;
                let instr_word =
                    (opcode << 11) | ((*reserved as u16) << 6) | ((*rd as u16) << 3) | (*rs as u16);
                instr_word.to_le_bytes().to_vec()
            }
            (Mnemonic::And, Format::Rfmt { reserved, rd, rs }) => {
                let opcode = 0b00010u16;
                let instr_word =
                    (opcode << 11) | ((*reserved as u16) << 6) | ((*rd as u16) << 3) | (*rs as u16);
                instr_word.to_le_bytes().to_vec()
            }
            (Mnemonic::Or, Format::Rfmt { reserved, rd, rs }) => {
                let opcode = 0b00010u16;
                let instr_word =
                    (opcode << 11) | ((*reserved as u16) << 6) | ((*rd as u16) << 3) | (*rs as u16);
                instr_word.to_le_bytes().to_vec()
            }
            (Mnemonic::Xor, Format::Rfmt { reserved, rd, rs }) => {
                let opcode = 0b00010u16;
                let instr_word =
                    (opcode << 11) | ((*reserved as u16) << 6) | ((*rd as u16) << 3) | (*rs as u16);
                instr_word.to_le_bytes().to_vec()
            }
            (Mnemonic::Shl, Format::Rfmt { reserved, rd, rs }) => {
                let opcode = 0b00011u16;
                let instr_word =
                    (opcode << 11) | ((*reserved as u16) << 6) | ((*rd as u16) << 3) | (*rs as u16);
                instr_word.to_le_bytes().to_vec()
            }
            (Mnemonic::Shr, Format::Rfmt { reserved, rd, rs }) => {
                let opcode = 0b00011u16;
                let instr_word =
                    (opcode << 11) | ((*reserved as u16) << 6) | ((*rd as u16) << 3) | (*rs as u16);
                instr_word.to_le_bytes().to_vec()
            }
            (Mnemonic::Mov, Format::Rfmt { reserved, rd, rs }) => {
                let opcode = 0b00100u16;
                let instr_word =
                    (opcode << 11) | ((*reserved as u16) << 6) | ((*rd as u16) << 3) | (*rs as u16);
                instr_word.to_le_bytes().to_vec()
            }

            // I-Type instructions
            (Mnemonic::AddI, Format::Ifmt { rd, imm }) => {
                let opcode = 0b00101u16;
                let instr_word = (opcode << 11) | ((*rd as u16) << 8) | (*imm as u16);
                instr_word.to_le_bytes().to_vec()
            }
            (Mnemonic::SubI, Format::Ifmt { rd, imm }) => {
                let opcode = 0b00110u16;
                let instr_word = (opcode << 11) | ((*rd as u16) << 8) | (*imm as u16);
                instr_word.to_le_bytes().to_vec()
            }
            (Mnemonic::AndI, Format::Ifmt { rd, imm }) => {
                let opcode = 0b00111u16;
                let instr_word = (opcode << 11) | ((*rd as u16) << 8) | (*imm as u16);
                instr_word.to_le_bytes().to_vec()
            }
            (Mnemonic::OrI, Format::Ifmt { rd, imm }) => {
                let opcode = 0b01000u16;
                let instr_word = (opcode << 11) | ((*rd as u16) << 8) | (*imm as u16);
                instr_word.to_le_bytes().to_vec()
            }
            (Mnemonic::XorI, Format::Ifmt { rd, imm }) => {
                let opcode = 0b01001u16;
                let instr_word = (opcode << 11) | ((*rd as u16) << 8) | (*imm as u16);
                instr_word.to_le_bytes().to_vec()
            }
            (Mnemonic::ShlI, Format::Ifmt { rd, imm }) => {
                let opcode = 0b01010u16;
                let instr_word = (opcode << 11) | ((*rd as u16) << 8) | (*imm as u16);
                instr_word.to_le_bytes().to_vec()
            }
            (Mnemonic::ShrI, Format::Ifmt { rd, imm }) => {
                let opcode = 0b01011u16;
                let instr_word = (opcode << 11) | ((*rd as u16) << 8) | (*imm as u16);
                instr_word.to_le_bytes().to_vec()
            }
            (Mnemonic::MovI, Format::Ifmt { rd, imm }) => {
                let opcode = 0b01100u16;
                let instr_word = (opcode << 11) | ((*rd as u16) << 8) | (*imm as u16);
                instr_word.to_le_bytes().to_vec()
            }

            // M-Type instructions
            (Mnemonic::Ldw, Format::Mfmt { rd, rs, imm }) => {
                let opcode = 0b01101u16;
                let instr_word =
                    (opcode << 11) | ((*rd as u16) << 8) | ((*rs as u16) << 5) | (*imm as u16);
                instr_word.to_le_bytes().to_vec()
            }
            (Mnemonic::Stw, Format::Mfmt { rd, rs, imm }) => {
                let opcode = 0b01110u16;
                let instr_word =
                    (opcode << 11) | ((*rd as u16) << 8) | ((*rs as u16) << 5) | (*imm as u16);
                instr_word.to_le_bytes().to_vec()
            }
            (Mnemonic::Ldb, Format::Mfmt { rd, rs, imm }) => {
                let opcode = 0b01111u16;
                let instr_word =
                    (opcode << 11) | ((*rd as u16) << 8) | ((*rs as u16) << 5) | (*imm as u16);
                instr_word.to_le_bytes().to_vec()
            }
            (Mnemonic::Stb, Format::Mfmt { rd, rs, imm }) => {
                let opcode = 0b10000u16;
                let instr_word =
                    (opcode << 11) | ((*rd as u16) << 8) | ((*rs as u16) << 5) | (*imm as u16);
                instr_word.to_le_bytes().to_vec()
            }

            // B-Type instructions
            (Mnemonic::Jmp, Format::Bfmt { cond, imm }) => {
                let opcode = 0b10001u16;
                let instr_word = (opcode << 11) | ((*cond as u16) << 9) | (*imm as u16);
                instr_word.to_le_bytes().to_vec()
            }
            (Mnemonic::Jeq, Format::Bfmt { cond, imm }) => {
                let opcode = 0b10001u16;
                let instr_word = (opcode << 11) | ((*cond as u16) << 9) | (*imm as u16);
                instr_word.to_le_bytes().to_vec()
            }
            (Mnemonic::Jlt, Format::Bfmt { cond, imm }) => {
                let opcode = 0b10001u16;
                let instr_word = (opcode << 11) | ((*cond as u16) << 9) | (*imm as u16);
                instr_word.to_le_bytes().to_vec()
            }
            (Mnemonic::Jgt, Format::Bfmt { cond, imm }) => {
                let opcode = 0b10001u16;
                let instr_word = (opcode << 11) | ((*cond as u16) << 9) | (*imm as u16);
                instr_word.to_le_bytes().to_vec()
            }

            // E-Type instructions (2-word instructions)
            (Mnemonic::JmpW, Format::Efmt { reserved }) => {
                let opcode = 0b10010u16;
                let instr_word = (opcode << 11) | *reserved;
                let mut bytes = instr_word.to_le_bytes().to_vec();
                bytes.extend(0u16.to_le_bytes()); // Second word filled by linker
                bytes
            }
            (Mnemonic::JeqW, Format::Efmt { reserved }) => {
                let opcode = 0b10010u16;
                let instr_word = (opcode << 11) | *reserved;
                let mut bytes = instr_word.to_le_bytes().to_vec();
                bytes.extend(0u16.to_le_bytes());
                bytes
            }
            (Mnemonic::JltW, Format::Efmt { reserved }) => {
                let opcode = 0b10010u16;
                let instr_word = (opcode << 11) | *reserved;
                let mut bytes = instr_word.to_le_bytes().to_vec();
                bytes.extend(0u16.to_le_bytes());
                bytes
            }
            (Mnemonic::JgtW, Format::Efmt { reserved }) => {
                let opcode = 0b10010u16;
                let instr_word = (opcode << 11) | *reserved;
                let mut bytes = instr_word.to_le_bytes().to_vec();
                bytes.extend(0u16.to_le_bytes());
                bytes
            }
            (Mnemonic::Call, Format::Efmt { reserved }) => {
                let opcode = 0b10011u16;
                let instr_word = (opcode << 11) | *reserved;
                let mut bytes = instr_word.to_le_bytes().to_vec();
                bytes.extend(0u16.to_le_bytes());
                bytes
            }

            // S-Type instructions
            (Mnemonic::Ret, Format::Sfmt { reserved }) => {
                let opcode = 0b10100u16;
                let instr_word = (opcode << 11) | *reserved;
                instr_word.to_le_bytes().to_vec()
            }
            (Mnemonic::NoOp, Format::Sfmt { reserved }) => {
                let opcode = 0b00000u16;
                let instr_word = (opcode << 11) | *reserved;
                instr_word.to_le_bytes().to_vec()
            }
            (Mnemonic::Halt, Format::Sfmt { reserved }) => {
                let opcode = 0b11111u16;
                let instr_word = (opcode << 11) | *reserved;
                instr_word.to_le_bytes().to_vec()
            }

            // These should never appear in gen_bin - they should be resolved by assemble_object
            (Mnemonic::Lbl(_), _)
            | (Mnemonic::CallLbl(_), _)
            | (Mnemonic::JmpLbl(_), _)
            | (Mnemonic::JltLbl(_), _)
            | (Mnemonic::JgtLbl(_), _)
            | (Mnemonic::JeqLbl(_), _)
            | (Mnemonic::JmpWLbl(_), _)
            | (Mnemonic::JltWLbl(_), _)
            | (Mnemonic::JgtWLbl(_), _)
            | (Mnemonic::JeqWLbl(_), _)
            | (Mnemonic::Data(_, _), _) => {
                unreachable!(
                    "gen_bin called on unresolved symbolic instruction: {:?}",
                    instr.mnemonic
                )
            }

            _ => unreachable!(
                "Invalid mnemonic/format combination: {:?} {:?}",
                instr.mnemonic, instr.format
            ),
        };

        code.extend(bytes);
    }

    code
}

pub fn assemble_object(assembly: &Assembly) -> AssemblerResult<Vec<u8>> {
    let mut out_instrs = Vec::new();
    let mut byte_pos: u16 = 0;
    let mut symbols = HashMap::new();
    let mut relocs = Vec::new();

    for instr in &assembly.text {
        match &instr.mnemonic {
            Mnemonic::Lbl(name) => {
                let rec = symbols.insert(name.clone(), byte_pos);
                if rec.is_some() && rec.unwrap() != 0xFFFF {
                    return Err(AssemblerError {
                        msg: format!("symbol {} is defined multiple times", name),
                        line: None,
                    });
                }
                // Labels don't generate instructions
                continue;
            }

            // B-Type label instructions (8-bit relative jumps)
            Mnemonic::JmpLbl(name)
            | Mnemonic::JltLbl(name)
            | Mnemonic::JgtLbl(name)
            | Mnemonic::JeqLbl(name) => {
                let target_addr = match symbols.get(name) {
                    // Back reference - calculate relative offset
                    Some(addr) => (*addr as isize - (byte_pos as isize + 2)) as i8,
                    // Forward reference - add relocation
                    None => {
                        symbols.insert(name.to_string(), 0xFFFF);
                        relocs.push(Reloc {
                            offset: byte_pos + 1, // Second byte of B-type instruction contains offset
                            sym_name: name.to_string(),
                            kind: RelocType::Rel8,
                        });
                        0 // Placeholder
                    }
                };

                let (concrete_mnemonic, cond) = match &instr.mnemonic {
                    Mnemonic::JmpLbl(_) => (Mnemonic::Jmp, 0),
                    Mnemonic::JltLbl(_) => (Mnemonic::Jlt, 2),
                    Mnemonic::JgtLbl(_) => (Mnemonic::Jgt, 3),
                    Mnemonic::JeqLbl(_) => (Mnemonic::Jeq, 1),
                    _ => unreachable!(),
                };

                let resolved_instr = Instr {
                    mnemonic: concrete_mnemonic,
                    format: Format::Bfmt {
                        cond,
                        imm: target_addr as u8,
                    },
                };
                out_instrs.push(resolved_instr);
            }

            // E-Type label instructions (16-bit absolute addresses)
            Mnemonic::JmpWLbl(name)
            | Mnemonic::JltWLbl(name)
            | Mnemonic::JgtWLbl(name)
            | Mnemonic::JeqWLbl(name) => {
                let _target_addr = match symbols.get(name) {
                    Some(addr) => *addr,
                    None => {
                        symbols.insert(name.to_string(), 0xFFFF);
                        0xFFFF
                    }
                };
                relocs.push(Reloc {
                    offset: byte_pos + 2, // Second word of E-type instruction
                    sym_name: name.to_string(),
                    kind: RelocType::Abs16,
                });

                let (concrete_mnemonic, reserved) = match &instr.mnemonic {
                    Mnemonic::JmpWLbl(_) => (Mnemonic::JmpW, 0),
                    Mnemonic::JltWLbl(_) => (Mnemonic::JltW, 2),
                    Mnemonic::JgtWLbl(_) => (Mnemonic::JgtW, 3),
                    Mnemonic::JeqWLbl(_) => (Mnemonic::JeqW, 1),
                    _ => unreachable!(),
                };

                let resolved_instr = Instr {
                    mnemonic: concrete_mnemonic,
                    format: Format::Efmt { reserved },
                };
                out_instrs.push(resolved_instr);
            }

            Mnemonic::CallLbl(name) => {
                let _target_addr = match symbols.get(name) {
                    Some(addr) => *addr,
                    None => {
                        symbols.insert(name.to_string(), 0xFFFF);
                        0xFFFF
                    }
                };
                relocs.push(Reloc {
                    offset: byte_pos + 2, // Second word of E-type instruction
                    sym_name: name.to_string(),
                    kind: RelocType::Abs16,
                });

                let resolved_instr = Instr {
                    mnemonic: Mnemonic::Call,
                    format: Format::Efmt { reserved: 0 },
                };
                out_instrs.push(resolved_instr);
            }

            Mnemonic::Data(_rd, name) => {
                relocs.push(Reloc {
                    offset: byte_pos + 2, // Second word of E-type instruction
                    sym_name: name.to_string(),
                    kind: RelocType::Data,
                });

                // Data reference becomes CALL for compatibility, but we'll need to modify this
                // to actually load into the specified register
                let resolved_instr = Instr {
                    mnemonic: Mnemonic::Call,
                    format: Format::Efmt { reserved: 0 },
                };
                out_instrs.push(resolved_instr);
            }

            // Regular instructions - just copy them
            _concrete => {
                out_instrs.push(instr.clone());
            }
        }
        byte_pos += instr.mnemonic.byte_len() as u16;
    }

    // Handle data section symbols
    for (name, value) in &assembly.data {
        let rec = symbols.insert(name.clone(), byte_pos);
        if rec.is_some() && rec.unwrap() != 0xFFFF {
            return Err(AssemblerError {
                msg: format!("symbol {} is defined multiple times", name),
                line: None,
            });
        }
        byte_pos += value.len() as u16
    }

    Ok(Object {
        instrs: out_instrs,
        data: assembly.data.clone(),
        symbols: symbols
            .iter()
            .map(|(name, val)| Symbol {
                name: name.to_string(),
                val: *val,
            })
            .collect(),
        relocs,
    }
    .to_bin())
}

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Cli {
    /// input assembly file
    #[arg(value_name = "FILE")]
    input: String,

    /// output object file
    #[arg(short, long)]
    output: Option<String>,
}

fn main() -> io::Result<()> {
    let cli = Cli::parse();

    let asm_code = fs::read_to_string(&cli.input)?;

    // parse assembly → instrs
    let instrs = parse_assembly(&asm_code).unwrap_or_else(|e| {
        eprintln!("Assembly parse error: {:?}", e);
        std::process::exit(1);
    });

    // assemble into object bytes
    let object_bytes = assemble_object(&instrs).unwrap_or_else(|e| {
        eprintln!("Assembly error: {:?}", e);
        std::process::exit(1);
    });

    // decide output filename
    let output = cli.output.unwrap_or_else(|| {
        let stem = std::path::Path::new(&cli.input)
            .file_stem()
            .unwrap_or_default()
            .to_string_lossy()
            .into_owned();
        format!("{}.mobj", stem)
    });

    fs::write(&output, object_bytes)?;
    println!("wrote object file to {}", output);

    Ok(())
}
