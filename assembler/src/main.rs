mod error;
mod parser;

use clap::{Parser, command};
use computils::instr::Instr;
use std::{collections::HashMap, fs, io};

use crate::{
    error::{AssemblerError, AssemblerResult},
    parser::{Assembly, parse_assembly},
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
    pub instrs: Vec<Instr>,
    pub data: Vec<(String, String)>,
    pub symbols: Symbols,
    pub relocs: Relocs, // assembler-time relocations with byte offsets
}

const OBJ_FILE_VERSION: u16 = 2;

impl Object {
    /// Serialize object into binary:
    /// [ "MOBJ" (4) ][version u16]
    /// [instr_bytes_len u16][data_bytes_len u16]
    /// [symtable_len u16][reloctable_len u16]
    /// [instr_bytes...][data_bytes...]
    /// [symtable...][reloctable...]
    ///
    /// symtable: repeated (name bytes, 0u8, u16 addr)
    /// reloctable: repeated (u16 offset, u16 sym_index, u8 kind)

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

pub fn resolve_conv_instrs(instrs: Vec<Instr>) -> Vec<Instr> {
    let mut out = Vec::new();
    for instr in instrs {
        out.extend(match instr {
            Instr::Ldr(rs, imm) => vec![
                Instr::Pushr(rs),
                Instr::Push(imm as i16 as u16),
                Instr::Add,
                Instr::Ldw,
            ],
            // a little complex because value is on top of the stack
            Instr::Str(rd, imm) => vec![
                Instr::Pushr(rd),
                Instr::Push(imm as i16 as u16),
                Instr::Add,
                Instr::Stw,
            ],
            Instr::Sub => vec![Instr::Not, Instr::Push(1), Instr::Add, Instr::Add],
            Instr::Mul => vec![
                Instr::Popr(2),
                Instr::Popr(1),
                Instr::CallLbl("__imul".to_string()),
                Instr::Pushr(0),
            ],
            Instr::Div => vec![
                Instr::Popr(3),
                Instr::Popr(2),
                Instr::CallLbl("__idivmod".to_string()),
                Instr::Pushr(0),
            ],
            Instr::Mod => vec![
                Instr::Popr(3),
                Instr::Popr(2),
                Instr::CallLbl("__idivmod".to_string()),
                Instr::Pushr(1),
            ],
            Instr::Neg => vec![Instr::Not, Instr::Push(1), Instr::Add],
            _ => vec![instr],
        })
    }
    out
}

pub fn gen_bin(instrs: &Vec<Instr>) -> Vec<u8> {
    let mut code = Vec::new();

    for instr in instrs {
        code.extend(match instr {
            Instr::Push(val) => vec![0x01, (val & 0xFF) as u8, (val >> 8) as u8],
            Instr::Halt => vec![0x0F],
            Instr::Ldw => vec![0x12],
            Instr::Stw => vec![0x13],
            Instr::Ldb => vec![0x16],
            Instr::Stb => vec![0x17],
            Instr::Jmp(displ) => vec![0x20, *displ as u8],
            Instr::Jlt(displ) => vec![0x21, *displ as u8],
            Instr::Jgt(displ) => vec![0x22, *displ as u8],
            Instr::Jeq(displ) => vec![0x23, *displ as u8],
            Instr::Call(addr) => vec![0x24, (*addr & 0xFF) as u8, (*addr >> 8) as u8],
            Instr::Ret => vec![0x25],
            Instr::Add => vec![0x30],
            Instr::Cmp => vec![0x35],
            Instr::Mod => vec![0x36],
            Instr::Not => vec![0x40],
            Instr::And => vec![0x41],
            Instr::Or => vec![0x42],
            Instr::Xor => vec![0x43],
            Instr::Shft => vec![0x44],
            Instr::Mov(rd, rs) => vec![0x50, (rd << 4) | rs],
            Instr::Pushr(rs) => vec![0x51, *rs],
            Instr::Popr(rd) => vec![0x52, *rd],
            Instr::Lbl(_)
            | Instr::CallLbl(_)
            | Instr::JmpLbl(_)
            | Instr::JltLbl(_)
            | Instr::JgtLbl(_)
            | Instr::JeqLbl(_)
            | Instr::Data(_) => {
                unreachable!("gen_bin called on unresolved symbolic instruction")
            }
            Instr::Sub
            | Instr::Mul
            | Instr::Div
            | Instr::Neg
            | Instr::Ldr(_, _)
            | Instr::Str(_, _) => {
                unreachable!(
                    "gen_bin called on unresolved convenience instructions {:?}",
                    &instr
                )
            }
            Instr::Int(int) => vec![0x70, *int],
            Instr::Iret => vec![0x71],
            Instr::Bkpt => vec![0x72],
        });
    }

    code
}

pub fn assemble_object(assembly: &Assembly) -> AssemblerResult<Vec<u8>> {
    let mut out_instr = Vec::new();
    let mut byte_pos: u16 = 0;
    let mut symbols = HashMap::new();
    let mut relocs = Vec::new();

    for instr in &assembly.text {
        match instr {
            Instr::Lbl(name) => {
                let rec = symbols.insert(name.clone(), byte_pos);
                if rec.is_some() && rec.unwrap() != 0xFFFF {
                    return Err(AssemblerError {
                        msg: format!("symbol {} is defined multiple times", name),
                        line: None,
                    });
                }
            }

            Instr::JmpLbl(name)
            | Instr::JltLbl(name)
            | Instr::JgtLbl(name)
            | Instr::JeqLbl(name) => {
                let target_addr = match symbols.get(name) {
                    // Back reference
                    Some(addr) => {
                        (*addr as isize - (byte_pos as isize + instr.clone().byte_len() as isize))
                            as i8
                    }
                    // Forward reference
                    None => {
                        symbols.insert(name.to_string(), 0xFFFF);
                        relocs.push(Reloc {
                            offset: byte_pos + 1,
                            sym_name: name.to_string(),
                            kind: RelocType::Rel8,
                        });
                        -1 // 0xFF
                    }
                };

                let unlabelled = match instr {
                    Instr::JmpLbl(_) => Instr::Jmp(target_addr),
                    Instr::JltLbl(_) => Instr::Jlt(target_addr),
                    Instr::JgtLbl(_) => Instr::Jgt(target_addr),
                    Instr::JeqLbl(_) => Instr::Jeq(target_addr),
                    _ => unreachable!(),
                };

                out_instr.push(unlabelled);
            }

            Instr::CallLbl(name) => {
                let target_addr = match symbols.get(name) {
                    Some(addr) => addr,
                    None => {
                        symbols.insert(name.to_string(), 0xFFFF);
                        &0xFFFF
                    }
                };
                relocs.push(Reloc {
                    offset: byte_pos + 1,
                    sym_name: name.to_string(),
                    kind: RelocType::Abs16,
                });
                out_instr.push(Instr::Call(*target_addr));
            }

            Instr::Data(name) => {
                relocs.push(Reloc {
                    offset: byte_pos + 1,
                    sym_name: name.to_string(),
                    kind: RelocType::Data,
                });
                out_instr.push(Instr::Push(0xFFFF));
            }

            concrete => out_instr.push(concrete.clone()),
        }
        byte_pos += instr.byte_len() as u16;
    }

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
        instrs: out_instr,
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
    let assembly = parse_assembly(&asm_code).unwrap_or_else(|e| {
        eprintln!("Assembly parse error: {:?}", e);
        std::process::exit(1);
    });

    // assemble into object bytes
    let object_bytes = assemble_object(&assembly).unwrap_or_else(|e| {
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
