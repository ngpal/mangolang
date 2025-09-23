mod error;

use clap::{Parser, command};
use computils::instr::Instr;
use std::{collections::HashMap, fs, io};

use crate::error::{AssemblerError, AssemblerResult};

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum RelocType {
    Abs16 = 0,
    Rel8 = 1, // relative 8-bit displacement (e.g. jmp/jlt/jgt/jeq)
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

pub struct Object {
    pub instrs: Vec<Instr>,
    pub symbols: Symbols,
    pub relocs: Relocs, // assembler-time relocations with byte offsets
}

const OBJ_FILE_VERSION: u16 = 1;

impl Object {
    /// Serialize object into binary:
    /// [ "MOBJ" (4) ][version u16][instr_bytes_len u16]
    /// [symtable_len u16][reloctable_len u16]
    /// [instr_bytes...][symtable...][reloctable...]
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
        out.extend(symtab_bytes);
        out.extend(reloctab_bytes);

        out
    }
}

fn parse_reg(token: Option<&str>, lineno: usize) -> AssemblerResult<u8> {
    let tok = token.ok_or_else(|| AssemblerError {
        msg: "missing register operand".into(),
        line: Some(lineno + 1),
    })?;

    match &tok.to_ascii_lowercase().as_str()[..2] {
        "r0" => Ok(0),
        "r1" => Ok(1),
        "r2" => Ok(2),
        "r3" => Ok(3),
        "r4" | "sp" => Ok(4),
        "r5" | "fp" => Ok(5),
        _ => Err(AssemblerError {
            msg: format!("invalid register `{}`", tok),
            line: Some(lineno + 1),
        }),
    }
}

fn parse_imm8(token: Option<&str>, lineno: usize) -> AssemblerResult<u8> {
    let tok = token.ok_or_else(|| AssemblerError {
        msg: "missing 8-bit immediate".into(),
        line: Some(lineno + 1),
    })?;

    let val = if let Some(hex) = tok.strip_prefix("0x").or_else(|| tok.strip_prefix("0X")) {
        u8::from_str_radix(hex, 16)
    } else {
        tok.parse::<u8>()
    };

    val.map_err(|_| AssemblerError {
        msg: format!("invalid 8-bit immediate `{}`", tok),
        line: Some(lineno + 1),
    })
}

fn parse_imm8_signed(token: Option<&str>, lineno: usize) -> AssemblerResult<i8> {
    let tok = token.ok_or_else(|| AssemblerError {
        msg: "missing signed 8-bit immediate".into(),
        line: Some(lineno + 1),
    })?;

    let val = if let Some(hex) = tok.strip_prefix("0x").or_else(|| tok.strip_prefix("0X")) {
        // parse as u8 first, then reinterpret as i8
        u8::from_str_radix(hex, 16).map(|n| n as i8)
    } else {
        tok.parse::<i8>()
    };

    val.map_err(|_| AssemblerError {
        msg: format!("invalid signed 8-bit immediate `{}`", tok),
        line: Some(lineno + 1),
    })
}

fn parse_imm16(token: Option<&str>, lineno: usize) -> AssemblerResult<u16> {
    let tok = token.ok_or_else(|| AssemblerError {
        msg: "missing 16-bit immediate".into(),
        line: Some(lineno + 1),
    })?;

    let val = if let Some(hex) = tok.strip_prefix("0x").or_else(|| tok.strip_prefix("0X")) {
        u16::from_str_radix(hex, 16)
    } else {
        tok.parse::<u16>()
    };

    val.map_err(|_| AssemblerError {
        msg: format!("invalid 16-bit immediate `{}`", tok),
        line: Some(lineno + 1),
    })
}

fn parse_label(token: Option<&str>, lineno: usize) -> AssemblerResult<String> {
    Ok(token
        .ok_or_else(|| AssemblerError {
            msg: "missing label operand".into(),
            line: Some(lineno + 1),
        })?
        .to_string())
}

pub fn parse_assembly(input: &str) -> AssemblerResult<Vec<Instr>> {
    let mut instrs = Vec::new();

    for (lineno, line) in input.lines().enumerate() {
        let line = line.trim();

        // skip blank lines and comments
        if line.is_empty() || line.starts_with(';') {
            continue;
        }

        // label definition
        if let Some(label) = line.strip_suffix(':') {
            instrs.push(Instr::Lbl(label.to_string()));
            continue;
        }

        // split into mnemonic + rest
        let mut parts = line.splitn(2, ' ');
        let mnemonic = parts.next().unwrap().to_ascii_lowercase();
        let rest = parts.next().unwrap_or("").trim();

        let instr = match mnemonic.as_str() {
            // stack & control
            "push16" => Instr::Push(parse_imm16(Some(rest), lineno)?),
            "halt" => Instr::Halt,
            "ret" => Instr::Ret,

            // memory
            "load8" => Instr::Load(parse_imm8(Some(rest), lineno)?),
            "store8" => Instr::Store(parse_imm8(Some(rest), lineno)?),
            "loadp" => Instr::Loadp,
            "storep" => Instr::Storep,

            "loadr" => {
                let inner = rest
                    .strip_prefix('[')
                    .and_then(|s| s.strip_suffix(']'))
                    .ok_or_else(|| AssemblerError {
                        msg: "expected [reg, imm]".into(),
                        line: Some(lineno + 1),
                    })?;

                let mut inner_parts = inner.split(',');
                let reg_str = inner_parts
                    .next()
                    .ok_or_else(|| AssemblerError {
                        msg: "missing register".into(),
                        line: Some(lineno + 1),
                    })?
                    .trim();
                let imm_str = inner_parts
                    .next()
                    .ok_or_else(|| AssemblerError {
                        msg: "missing offset".into(),
                        line: Some(lineno + 1),
                    })?
                    .trim();

                let reg = parse_reg(Some(reg_str), lineno)?;
                let imm = parse_imm8_signed(Some(imm_str), lineno)?;
                Instr::Loadr(reg, imm)
            }

            "storer" => {
                let inner = rest
                    .strip_prefix('[')
                    .and_then(|s| s.strip_suffix(']'))
                    .ok_or_else(|| AssemblerError {
                        msg: "expected [reg, imm]".into(),
                        line: Some(lineno + 1),
                    })?;

                let mut inner_parts = inner.split(',');
                let reg_str = inner_parts
                    .next()
                    .ok_or_else(|| AssemblerError {
                        msg: "missing register".into(),
                        line: Some(lineno + 1),
                    })?
                    .trim();
                let imm_str = inner_parts
                    .next()
                    .ok_or_else(|| AssemblerError {
                        msg: "missing offset".into(),
                        line: Some(lineno + 1),
                    })?
                    .trim();

                let reg = parse_reg(Some(reg_str), lineno)?;
                let imm = parse_imm8_signed(Some(imm_str), lineno)?;
                Instr::Storer(reg, imm)
            }

            // jumps & calls (label forms)
            "jmp" => Instr::JmpLbl(parse_label(Some(rest), lineno)?),
            "jmp8" => Instr::Jmp(parse_imm8_signed(Some(rest), lineno)?),

            "jlt" => Instr::JltLbl(parse_label(Some(rest), lineno)?),
            "jlt8" => Instr::Jlt(parse_imm8_signed(Some(rest), lineno)?),

            "jgt" => Instr::JgtLbl(parse_label(Some(rest), lineno)?),
            "jgt8" => Instr::Jgt(parse_imm8_signed(Some(rest), lineno)?),

            "jeq" => Instr::JeqLbl(parse_label(Some(rest), lineno)?),
            "jeq8" => Instr::Jeq(parse_imm8_signed(Some(rest), lineno)?),
            "call" => Instr::CallLbl(parse_label(Some(rest), lineno)?),

            // integer arithmetic
            "add" => Instr::Add,
            "sub" => Instr::Sub,
            "mul" => Instr::Mul,
            "div" => Instr::Div,
            "neg" => Instr::Neg,
            "cmp" => Instr::Cmp,
            "mod" => Instr::Mod,

            // bitwise
            "not" => Instr::Not,
            "and" => Instr::And,
            "or" => Instr::Or,
            "xor" => Instr::Xor,
            "shft" => Instr::Shft,

            // register ops
            "mov" => {
                let mut ops = rest.split_whitespace();
                let rd = parse_reg(ops.next(), lineno)?;
                let rs = parse_reg(ops.next(), lineno)?;
                Instr::Mov(rd, rs)
            }
            "pushr" => {
                let mut ops = rest.split_whitespace();
                let rs = parse_reg(ops.next(), lineno)?;
                Instr::Pushr(rs)
            }
            "popr" => {
                let mut ops = rest.split_whitespace();
                let rd = parse_reg(ops.next(), lineno)?;
                Instr::Popr(rd)
            }

            // video
            "print" => Instr::Print,
            "mvcur" => Instr::MvCur(parse_imm8(Some(rest), lineno)? as i8),

            _ => {
                return Err(AssemblerError {
                    msg: format!("unknown mnemonic `{}`", mnemonic),
                    line: Some(lineno + 1),
                });
            }
        };

        instrs.push(instr);
    }

    Ok(instrs)
}

pub fn gen_bin(instrs: &Vec<Instr>) -> Vec<u8> {
    let mut code = Vec::new();

    for instr in instrs {
        code.extend(match instr {
            Instr::Push(val) => vec![0x01, (val & 0xFF) as u8, (val >> 8) as u8],
            Instr::Halt => vec![0x0F],
            Instr::Load(addr) => vec![0x10, *addr],
            Instr::Store(addr) => vec![0x11, *addr],
            Instr::Loadp => vec![0x12],
            Instr::Storep => vec![0x13],
            Instr::Jmp(displ) => vec![0x20, *displ as u8],
            Instr::Jlt(displ) => vec![0x21, *displ as u8],
            Instr::Jgt(displ) => vec![0x22, *displ as u8],
            Instr::Jeq(displ) => vec![0x23, *displ as u8],
            Instr::Call(addr) => vec![0x24, (*addr & 0xFF) as u8, (*addr >> 8) as u8],
            Instr::Ret => vec![0x25],
            Instr::Add => vec![0x30],
            Instr::Sub => vec![0x31],
            Instr::Mul => vec![0x32],
            Instr::Div => vec![0x33],
            Instr::Neg => vec![0x34],
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
            Instr::Print => vec![0x60],
            Instr::MvCur(ofst) => vec![0x61, *ofst as u8],
            Instr::Lbl(_)
            | Instr::CallLbl(_)
            | Instr::JmpLbl(_)
            | Instr::JltLbl(_)
            | Instr::JgtLbl(_)
            | Instr::JeqLbl(_) => {
                unreachable!("gen_bin called on unresolved symbolic instruction")
            }
            Instr::Loadr(rd, ofst) => vec![0x14, *rd, *ofst as u8],
            Instr::Storer(rs, ofst) => vec![0x15, *rs, *ofst as u8],
        });
    }

    code
}

pub fn assemble_object(instrs: &Vec<Instr>) -> AssemblerResult<Vec<u8>> {
    let mut out_instr = Vec::new();
    let mut byte_pos: u16 = 0;
    let mut symbols = HashMap::new();
    let mut relocs = Vec::new();

    for instr in instrs {
        match instr {
            Instr::Lbl(name) => {
                let rec = symbols.insert(name, byte_pos);
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
                        (*addr as isize - (byte_pos as isize + instr.byte_len() as isize)) as i8
                    }
                    // Forward reference
                    None => {
                        symbols.insert(name, 0xFFFF);
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
                        symbols.insert(name, 0xFFFF);
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

            concrete => out_instr.push(concrete.clone()),
        }
        byte_pos += instr.byte_len() as u16;
    }

    Ok(Object {
        instrs: out_instr,
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
