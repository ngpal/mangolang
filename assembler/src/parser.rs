use std::collections::HashMap;

use computils::instr::Instr;

use crate::{
    error::{AssemblerError, AssemblerResult},
    resolve_conv_instrs,
};

enum Section {
    Text,
    Data,
}

pub struct Assembly {
    pub text: Vec<Instr>,
    pub data: Vec<(String, String)>,
}

// extract constants from lines like: @define CONST = 42
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

    match &tok.to_ascii_lowercase().as_str()[..2] {
        "r0" => Ok(0),
        "r1" => Ok(1),
        "r2" => Ok(2),
        "r3" => Ok(3),
        "r4" => Ok(4),
        "r5" => Ok(5),
        "r6" => Ok(6),
        "r7" => Ok(7),
        "r8" | "sp" => Ok(8),
        "r9" | "fp" => Ok(9),
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

fn parse_imm16_signed(token: Option<&str>, lineno: usize) -> AssemblerResult<i16> {
    let tok = token.ok_or_else(|| AssemblerError {
        msg: "missing 16-bit immediate".into(),
        line: Some(lineno + 1),
    })?;

    let val = if let Some(hex) = tok.strip_prefix("0x").or_else(|| tok.strip_prefix("0X")) {
        i16::from_str_radix(hex, 16)
    } else {
        tok.parse::<i16>()
    };

    val.map_err(|_| AssemblerError {
        msg: format!("invalid 16-bit signed immediate `{}`", tok),
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
                    '\\' => '\\',
                    '"' => '"',
                    other => other, // unknown escapes treated literally
                };
                result.push(escaped);
                chars.next(); // consume the escaped character
            } else {
                // lone backslash at end, keep as is
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

        // skip blank lines and comments
        if line.is_empty() || line.starts_with(';') || line.starts_with("@define") {
            continue;
        }

        // section switches
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
                        msg: format!("unknown section `{}`", other),
                        line: Some(lineno + 1),
                    });
                }
            }
        }

        if let Section::Data = cur_sec {
            if let Some((label, string)) = line.split_once('=') {
                let label = label.trim().to_string();
                let string = string.trim();

                // enforce quotes
                if !(string.starts_with('"') && string.ends_with('"') && string.len() >= 2) {
                    return Err(AssemblerError {
                        msg: format!("data string for `{}` must be quoted", label),
                        line: Some(lineno + 1),
                    });
                }

                // strip quotes
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
            "ldw" => Instr::Ldw,
            "stw" => Instr::Stw,
            "ldb" => Instr::Ldb,
            "stb" => Instr::Stb,

            "ldr" => {
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
                Instr::Ldr(reg, imm)
            }

            "str" => {
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
                Instr::Str(reg, imm)
            }

            // jumps & calls (label forms)
            "jmp" => Instr::JmpLbl(parse_label(Some(rest), lineno)?),
            "jmp16" => Instr::Jmp(parse_imm16_signed(Some(rest), lineno)?),

            "jlt" => Instr::JltLbl(parse_label(Some(rest), lineno)?),
            "jlt16" => Instr::Jlt(parse_imm16_signed(Some(rest), lineno)?),

            "jgt" => Instr::JgtLbl(parse_label(Some(rest), lineno)?),
            "jgt16" => Instr::Jgt(parse_imm16_signed(Some(rest), lineno)?),

            "jeq" => Instr::JeqLbl(parse_label(Some(rest), lineno)?),
            "jeq16" => Instr::Jeq(parse_imm16_signed(Some(rest), lineno)?),
            "call" => Instr::CallLbl(parse_label(Some(rest), lineno)?),

            // integer arithmetic
            "add" => Instr::Add,
            "sub" => Instr::Sub,
            "mul" => Instr::Mul,
            "div" => Instr::Div,
            "neg" => Instr::Neg,
            "cmp" => Instr::Cmp,
            "mod" => Instr::Mod,

            "addi" => Instr::AddI(parse_imm16(Some(rest), lineno)?),
            "subi" => Instr::SubI(parse_imm16(Some(rest), lineno)?),
            "muli" => Instr::MulI(parse_imm16(Some(rest), lineno)?),
            "divi" => Instr::DivI(parse_imm16(Some(rest), lineno)?),
            "negi" => Instr::NegI(parse_imm16(Some(rest), lineno)?),
            "cmpi" => Instr::CmpI(parse_imm16(Some(rest), lineno)?),
            "modi" => Instr::ModI(parse_imm16(Some(rest), lineno)?),

            // bitwise
            "not" => Instr::Not,
            "and" => Instr::And,
            "or" => Instr::Or,
            "xor" => Instr::Xor,
            "shl" => Instr::Shl,
            "shr" => Instr::Shr,

            "noti" => Instr::NotI(parse_imm16(Some(rest), lineno)?),
            "andi" => Instr::AndI(parse_imm16(Some(rest), lineno)?),
            "ori" => Instr::OrI(parse_imm16(Some(rest), lineno)?),
            "xori" => Instr::XorI(parse_imm16(Some(rest), lineno)?),
            "shli" => Instr::ShlI(parse_imm16(Some(rest), lineno)?),
            "shri" => Instr::ShrI(parse_imm16(Some(rest), lineno)?),

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

            // data
            "data" => Instr::Data(parse_label(Some(rest), lineno)?),

            // int
            "int" => Instr::Int(parse_imm8(Some(rest), lineno)?),
            "iret" => Instr::Iret,
            "bkpt" => Instr::Bkpt,

            _ => {
                return Err(AssemblerError {
                    msg: format!("unknown mnemonic `{}`", mnemonic),
                    line: Some(lineno + 1),
                });
            }
        };

        instrs.push(instr);
    }

    instrs = resolve_conv_instrs(instrs);
    Ok(Assembly {
        text: instrs,
        data: data.into_iter().collect(),
    })
}
