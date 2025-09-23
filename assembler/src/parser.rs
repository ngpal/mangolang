use std::collections::HashMap;

use computils::instr::Instr;

use crate::error::{AssemblerError, AssemblerResult};

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

fn substitute_constants<'a>(s: &'a str, macros: &HashMap<String, String>) -> String {
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
    let consts = extract_constants(input);
    let mut instrs = Vec::new();

    for (lineno, line) in input.lines().enumerate() {
        let line = line.trim();
        let line = substitute_constants(line, &consts);

        // skip blank lines and comments
        if line.is_empty() || line.starts_with(';') || line.starts_with("@") {
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
