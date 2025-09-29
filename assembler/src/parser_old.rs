use std::collections::HashMap;

use chumsky::prelude::*;

use crate::error::{AssemblerError, AssemblerResult};

#[derive(Clone, Debug)]
pub enum Mnemonic {
    // R-Type instructions
    Add,
    Sub,
    And,
    Or,
    Xor,
    Shl,
    Shr,
    Mov,

    // I-Type instructions
    AddI,
    SubI,
    AndI,
    OrI,
    XorI,
    ShlI,
    ShrI,
    MovI,

    // M-Type instructions
    Ldw,
    Stw,
    Ldb,
    Stb,

    // B-Type instructions
    Jmp,
    Jeq,
    Jlt,
    Jgt,

    // E-Type instructions
    JmpW,
    JeqW,
    JltW,
    JgtW,
    Call,

    // S-Type instructions
    Ret,
    NoOp,
    Halt,

    // Pseudo instructions for labels
    Lbl(String),
    CallLbl(String),
    JmpLbl(String),
    JltLbl(String),
    JgtLbl(String),
    JeqLbl(String),
    JmpWLbl(String),
    JltWLbl(String),
    JgtWLbl(String),
    JeqWLbl(String),
    Data(u8, String), // (rd, label) - load data address into register rd
}

fn parser<'a>() -> impl Parser<'a, &'a str, Vec<Instr>> {}

#[derive(Debug, Clone)]
pub enum Format {
    Rfmt { reserved: u8, rd: u8, rs: u8 },
    Ifmt { rd: u8, imm: u8 },
    Mfmt { rd: u8, rs: u8, imm: u8 },
    Bfmt { cond: u8, imm: u8 },
    Efmt { reserved: u16 },
    Sfmt { reserved: u16 },
}

#[derive(Debug, Clone)]
pub struct Instr {
    pub mnemonic: Mnemonic,
    pub format: Format,
}

impl Instr {
    fn new_r(mnemonic: Mnemonic, reserved: u8, rd: u8, rs: u8) -> Self {
        Self {
            mnemonic,
            format: Format::Rfmt { reserved, rd, rs },
        }
    }

    fn new_i(mnemonic: Mnemonic, rd: u8, imm: u8) -> Self {
        Self {
            mnemonic,
            format: Format::Ifmt { rd, imm },
        }
    }

    fn new_m(mnemonic: Mnemonic, rd: u8, rs: u8, imm: u8) -> Self {
        Self {
            mnemonic,
            format: Format::Mfmt { rd, rs, imm },
        }
    }

    fn new_b(mnemonic: Mnemonic, cond: u8, imm: u8) -> Self {
        Self {
            mnemonic,
            format: Format::Bfmt { cond, imm },
        }
    }

    fn new_e(mnemonic: Mnemonic, reserved: u16) -> Self {
        Self {
            mnemonic,
            format: Format::Efmt { reserved },
        }
    }

    fn new_s(mnemonic: Mnemonic, reserved: u16) -> Self {
        Self {
            mnemonic,
            format: Format::Sfmt { reserved },
        }
    }

    fn new_lbl(label: String) -> Self {
        Self {
            mnemonic: Mnemonic::Lbl(label),
            format: Format::Sfmt { reserved: 0 },
        }
    }

    fn new_pseudo(mnemonic: Mnemonic) -> Self {
        Self {
            mnemonic,
            format: Format::Sfmt { reserved: 0 },
        }
    }
}

enum Section {
    Text,
    Data,
}

pub struct Assembly {
    pub text: Vec<Instr>,
    pub data: Vec<(String, String)>,
}

// Extract constants from lines like: @define CONST = 42
fn extract_constants(input: &str) -> HashMap<String, String> {
    let mut macros = HashMap::new();
    for line in input.lines() {
        let line = line.trim();
        if let Some(rest) = line.strip_prefix("@define") {
            if let Some((name, val)) = rest.split_once('=') {
                let name = name.trim();
                let val = val.trim();
                if !name.is_empty() && !val.is_empty() {
                    macros.insert(name.to_string(), val.to_string());
                }
            }
        }
    }
    macros
}

fn substitute_constants(s: &str, macros: &HashMap<String, String>) -> String {
    let mut result = s.to_string();
    for (name, val) in macros {
        let key = format!("={}", name);
        result = result.replace(&key, val);
    }
    result
}

fn parse_reg(token: Option<&str>, lineno: usize) -> AssemblerResult<u8> {
    let tok = token.ok_or_else(|| AssemblerError {
        msg: "missing register operand".into(),
        line: Some(lineno + 1),
    })?;

    let reg_str = tok.to_ascii_lowercase();
    match reg_str.as_str() {
        "r0" => Ok(0),
        "r1" => Ok(1),
        "r2" => Ok(2),
        "r3" => Ok(3),
        "r4" => Ok(4),
        "r5" => Ok(5),
        "sp" | "r6" => Ok(6),
        "fp" | "r7" => Ok(7),
        _ => Err(AssemblerError {
            msg: format!("invalid register `{}` (valid: r0-r7, sp, fp)", tok),
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
        msg: format!("invalid 8-bit immediate `{}` (range: 0-255)", tok),
        line: Some(lineno + 1),
    })
}

fn parse_imm8_signed(token: Option<&str>, lineno: usize) -> AssemblerResult<i8> {
    let tok = token.ok_or_else(|| AssemblerError {
        msg: "missing signed 8-bit immediate".into(),
        line: Some(lineno + 1),
    })?;

    let val = if let Some(hex) = tok.strip_prefix("0x").or_else(|| tok.strip_prefix("0X")) {
        u8::from_str_radix(hex, 16).map(|n| n as i8)
    } else {
        tok.parse::<i8>()
    };

    val.map_err(|_| AssemblerError {
        msg: format!(
            "invalid signed 8-bit immediate `{}` (range: -128 to 127)",
            tok
        ),
        line: Some(lineno + 1),
    })
}

fn parse_imm5(token: Option<&str>, lineno: usize) -> AssemblerResult<u8> {
    let tok = token.ok_or_else(|| AssemblerError {
        msg: "missing 5-bit immediate".into(),
        line: Some(lineno + 1),
    })?;

    let val = if let Some(hex) = tok.strip_prefix("0x").or_else(|| tok.strip_prefix("0X")) {
        u8::from_str_radix(hex, 16)
    } else {
        tok.parse::<u8>()
    };

    let val = val.map_err(|_| AssemblerError {
        msg: format!("invalid 5-bit immediate `{}`", tok),
        line: Some(lineno + 1),
    })?;

    if val > 31 {
        Err(AssemblerError {
            msg: format!("5-bit immediate `{}` out of range (0-31)", val),
            line: Some(lineno + 1),
        })
    } else {
        Ok(val)
    }
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
        msg: format!("invalid 16-bit immediate `{}` (range: 0-65535)", tok),
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

fn unescape_string(s: &str) -> String {
    let mut result = String::new();
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            if let Some(&next) = chars.peek() {
                let escaped = match next {
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    '\\' => '\\',
                    '"' => '"',
                    '0' => '\0',
                    other => other, // unknown escapes treated literally
                };
                result.push(escaped);
                chars.next(); // consume the escaped character
            } else {
                result.push('\\');
            }
        } else {
            result.push(c);
        }
    }

    result
}

pub fn parse_assembly(input: &str) -> AssemblerResult<Assembly> {
    let consts = extract_constants(input);
    let mut instrs = Vec::new();
    let mut cur_sec = Section::Text;
    let mut data = HashMap::new();

    for (lineno, line) in input.lines().enumerate() {
        let line = line.trim();
        let line = substitute_constants(line, &consts);

        // Skip blank lines and comments
        if line.is_empty() || line.starts_with(';') || line.starts_with("@define") {
            continue;
        }

        // Section switches
        if line.starts_with("@section") {
            match line.strip_prefix("@section").unwrap().trim() {
                "data" => {
                    cur_sec = Section::Data;
                    continue;
                }
                "text" => {
                    cur_sec = Section::Text;
                    continue;
                }
                other => {
                    return Err(AssemblerError {
                        msg: format!("unknown section `{}` (valid: text, data)", other),
                        line: Some(lineno + 1),
                    });
                }
            }
        }

        // Handle data section
        if let Section::Data = cur_sec {
            if let Some((label, string)) = line.split_once('=') {
                let label = label.trim().to_string();
                let string = string.trim();

                if !(string.starts_with('"') && string.ends_with('"') && string.len() >= 2) {
                    return Err(AssemblerError {
                        msg: format!("data string for `{}` must be quoted", label),
                        line: Some(lineno + 1),
                    });
                }

                let raw = &string[1..string.len() - 1];
                let mut string = unescape_string(raw);
                string.push('\0');
                data.insert(label, string);
                continue;
            } else {
                return Err(AssemblerError {
                    msg: "expected `label = \"string\"` in data section".into(),
                    line: Some(lineno + 1),
                });
            }
        }

        // Label definition
        if let Some(label) = line.strip_suffix(':') {
            instrs.push(Instr::new_lbl(label.to_string()));
            continue;
        }

        // Parse instruction
        let mut parts = line.splitn(2, ' ');
        let mnemonic = parts.next().unwrap().to_ascii_lowercase();
        let rest = parts.next().unwrap_or("").trim();

        let instr = match mnemonic.as_str() {
            // R-Type instructions (rd, rs)
            "add" => {
                let mut ops = rest.split_whitespace();
                let rd = parse_reg(ops.next(), lineno)?;
                let rs = parse_reg(ops.next(), lineno)?;
                Instr::new_r(Mnemonic::Add, 0, rd, rs)
            }
            "sub" => {
                let mut ops = rest.split_whitespace();
                let rd = parse_reg(ops.next(), lineno)?;
                let rs = parse_reg(ops.next(), lineno)?;
                Instr::new_r(Mnemonic::Sub, 1, rd, rs)
            }
            "and" => {
                let mut ops = rest.split_whitespace();
                let rd = parse_reg(ops.next(), lineno)?;
                let rs = parse_reg(ops.next(), lineno)?;
                Instr::new_r(Mnemonic::And, 0, rd, rs)
            }
            "or" => {
                let mut ops = rest.split_whitespace();
                let rd = parse_reg(ops.next(), lineno)?;
                let rs = parse_reg(ops.next(), lineno)?;
                Instr::new_r(Mnemonic::Or, 1, rd, rs)
            }
            "xor" => {
                let mut ops = rest.split_whitespace();
                let rd = parse_reg(ops.next(), lineno)?;
                let rs = parse_reg(ops.next(), lineno)?;
                Instr::new_r(Mnemonic::Xor, 2, rd, rs)
            }
            "shl" => {
                let mut ops = rest.split_whitespace();
                let rd = parse_reg(ops.next(), lineno)?;
                let rs = parse_reg(ops.next(), lineno)?;
                Instr::new_r(Mnemonic::Shl, 0, rd, rs)
            }
            "shr" => {
                let mut ops = rest.split_whitespace();
                let rd = parse_reg(ops.next(), lineno)?;
                let rs = parse_reg(ops.next(), lineno)?;
                Instr::new_r(Mnemonic::Shr, 1, rd, rs)
            }
            "mov" => {
                let mut ops = rest.split_whitespace();
                let rd = parse_reg(ops.next(), lineno)?;
                let rs = parse_reg(ops.next(), lineno)?;
                Instr::new_r(Mnemonic::Mov, 0, rd, rs)
            }

            // I-Type instructions (rd, imm8)
            "addi" => {
                let mut ops = rest.split_whitespace();
                let rd = parse_reg(ops.next(), lineno)?;
                let imm = parse_imm8(ops.next(), lineno)?;
                Instr::new_i(Mnemonic::AddI, rd, imm)
            }
            "subi" => {
                let mut ops = rest.split_whitespace();
                let rd = parse_reg(ops.next(), lineno)?;
                let imm = parse_imm8(ops.next(), lineno)?;
                Instr::new_i(Mnemonic::SubI, rd, imm)
            }
            "andi" => {
                let mut ops = rest.split_whitespace();
                let rd = parse_reg(ops.next(), lineno)?;
                let imm = parse_imm8(ops.next(), lineno)?;
                Instr::new_i(Mnemonic::AndI, rd, imm)
            }
            "ori" => {
                let mut ops = rest.split_whitespace();
                let rd = parse_reg(ops.next(), lineno)?;
                let imm = parse_imm8(ops.next(), lineno)?;
                Instr::new_i(Mnemonic::OrI, rd, imm)
            }
            "xori" => {
                let mut ops = rest.split_whitespace();
                let rd = parse_reg(ops.next(), lineno)?;
                let imm = parse_imm8(ops.next(), lineno)?;
                Instr::new_i(Mnemonic::XorI, rd, imm)
            }
            "shli" => {
                let mut ops = rest.split_whitespace();
                let rd = parse_reg(ops.next(), lineno)?;
                let imm = parse_imm8(ops.next(), lineno)?;
                Instr::new_i(Mnemonic::ShlI, rd, imm)
            }
            "shri" => {
                let mut ops = rest.split_whitespace();
                let rd = parse_reg(ops.next(), lineno)?;
                let imm = parse_imm8(ops.next(), lineno)?;
                Instr::new_i(Mnemonic::ShrI, rd, imm)
            }
            "movi" => {
                let mut ops = rest.split_whitespace();
                let rd = parse_reg(ops.next(), lineno)?;
                let imm = parse_imm8(ops.next(), lineno)?;
                Instr::new_i(Mnemonic::MovI, rd, imm)
            }

            // M-Type instructions (rd, rs, imm5)
            "ldw" => {
                let mut ops = rest.split_whitespace();
                let rd = parse_reg(ops.next(), lineno)?;
                let rs = parse_reg(ops.next(), lineno)?;
                let imm = parse_imm5(ops.next(), lineno)?;
                Instr::new_m(Mnemonic::Ldw, rd, rs, imm)
            }
            "stw" => {
                let mut ops = rest.split_whitespace();
                let rd = parse_reg(ops.next(), lineno)?;
                let rs = parse_reg(ops.next(), lineno)?;
                let imm = parse_imm5(ops.next(), lineno)?;
                Instr::new_m(Mnemonic::Stw, rd, rs, imm)
            }
            "ldb" => {
                let mut ops = rest.split_whitespace();
                let rd = parse_reg(ops.next(), lineno)?;
                let rs = parse_reg(ops.next(), lineno)?;
                let imm = parse_imm5(ops.next(), lineno)?;
                Instr::new_m(Mnemonic::Ldb, rd, rs, imm)
            }
            "stb" => {
                let mut ops = rest.split_whitespace();
                let rd = parse_reg(ops.next(), lineno)?;
                let rs = parse_reg(ops.next(), lineno)?;
                let imm = parse_imm5(ops.next(), lineno)?;
                Instr::new_m(Mnemonic::Stb, rd, rs, imm)
            }

            // B-Type instructions with immediate offsets
            "jmp" => {
                if rest.chars().any(|c| c.is_alphabetic()) {
                    // It's a label
                    Instr::new_pseudo(Mnemonic::JmpLbl(parse_label(Some(rest), lineno)?))
                } else {
                    // It's an immediate
                    let imm = parse_imm8_signed(Some(rest), lineno)?;
                    Instr::new_b(Mnemonic::Jmp, 0, imm as u8)
                }
            }
            "jeq" => {
                if rest.chars().any(|c| c.is_alphabetic()) {
                    Instr::new_pseudo(Mnemonic::JeqLbl(parse_label(Some(rest), lineno)?))
                } else {
                    let imm = parse_imm8_signed(Some(rest), lineno)?;
                    Instr::new_b(Mnemonic::Jeq, 1, imm as u8)
                }
            }
            "jlt" => {
                if rest.chars().any(|c| c.is_alphabetic()) {
                    Instr::new_pseudo(Mnemonic::JltLbl(parse_label(Some(rest), lineno)?))
                } else {
                    let imm = parse_imm8_signed(Some(rest), lineno)?;
                    Instr::new_b(Mnemonic::Jlt, 2, imm as u8)
                }
            }
            "jgt" => {
                if rest.chars().any(|c| c.is_alphabetic()) {
                    Instr::new_pseudo(Mnemonic::JgtLbl(parse_label(Some(rest), lineno)?))
                } else {
                    let imm = parse_imm8_signed(Some(rest), lineno)?;
                    Instr::new_b(Mnemonic::Jgt, 3, imm as u8)
                }
            }

            // E-Type instructions (16-bit jumps and calls)
            "jmpw" => {
                if rest.chars().any(|c| c.is_alphabetic()) {
                    Instr::new_pseudo(Mnemonic::JmpWLbl(parse_label(Some(rest), lineno)?))
                } else {
                    let _imm = parse_imm16(Some(rest), lineno)?;
                    Instr::new_e(Mnemonic::JmpW, 0)
                }
            }
            "jeqw" => {
                if rest.chars().any(|c| c.is_alphabetic()) {
                    Instr::new_pseudo(Mnemonic::JeqWLbl(parse_label(Some(rest), lineno)?))
                } else {
                    let _imm = parse_imm16(Some(rest), lineno)?;
                    Instr::new_e(Mnemonic::JeqW, 1)
                }
            }
            "jltw" => {
                if rest.chars().any(|c| c.is_alphabetic()) {
                    Instr::new_pseudo(Mnemonic::JltWLbl(parse_label(Some(rest), lineno)?))
                } else {
                    let _imm = parse_imm16(Some(rest), lineno)?;
                    Instr::new_e(Mnemonic::JltW, 2)
                }
            }
            "jgtw" => {
                if rest.chars().any(|c| c.is_alphabetic()) {
                    Instr::new_pseudo(Mnemonic::JgtWLbl(parse_label(Some(rest), lineno)?))
                } else {
                    let _imm = parse_imm16(Some(rest), lineno)?;
                    Instr::new_e(Mnemonic::JgtW, 3)
                }
            }
            "call" => {
                if rest.chars().any(|c| c.is_alphabetic()) {
                    Instr::new_pseudo(Mnemonic::CallLbl(parse_label(Some(rest), lineno)?))
                } else {
                    let _imm = parse_imm16(Some(rest), lineno)?;
                    Instr::new_e(Mnemonic::Call, 0)
                }
            }

            // S-Type instructions (no operands)
            "ret" => Instr::new_s(Mnemonic::Ret, 0),
            "noop" => Instr::new_s(Mnemonic::NoOp, 0),
            "halt" => Instr::new_s(Mnemonic::Halt, 0),

            // Data reference pseudo-instruction
            "data" => {
                let mut ops = rest.split_whitespace();
                let rd = parse_reg(ops.next(), lineno)?;
                let label = parse_label(ops.next(), lineno)?;
                Instr::new_pseudo(Mnemonic::Data(rd, label))
            }

            _ => {
                return Err(AssemblerError {
                    msg: format!("unknown mnemonic `{}`", mnemonic),
                    line: Some(lineno + 1),
                });
            }
        };

        instrs.push(instr);
    }

    Ok(Assembly {
        text: instrs,
        data: data.into_iter().collect(),
    })
}
