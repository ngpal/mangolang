use crate::error::{RuntimeError, RuntimeResult};

#[repr(u16)]
#[derive(Debug, Copy, Clone)]
pub enum Opcode {
    Arith = 0x01,
    Logical = 0x02,
    Shift = 0x03,
    Mov = 0x04,
    AddI = 0x05,
    SubI = 0x06,
    AndI = 0x07,
    OrI = 0x08,
    XorI = 0x09,
    ShlI = 0x0A,
    ShrI = 0x0B,
    MovI = 0x0C,
    Ldw = 0x0D,
    Stw = 0x0E,
    Ldb = 0x0F,
    Stb = 0x10,
    J = 0x11,
    JW = 0x12,
    Call = 0x13,
    Ret = 0x14,
    Noop = 0x00,
    Halt = 0x1F,
}

impl Opcode {
    pub fn from_u8(op: u8) -> RuntimeResult<Opcode> {
        Ok(match op {
            0x01 => Opcode::Arith,
            0x02 => Opcode::Logical,
            0x03 => Opcode::Shift,
            0x04 => Opcode::Mov,
            0x05 => Opcode::AddI,
            0x06 => Opcode::SubI,
            0x07 => Opcode::AndI,
            0x08 => Opcode::OrI,
            0x09 => Opcode::XorI,
            0x0A => Opcode::ShlI,
            0x0B => Opcode::ShrI,
            0x0C => Opcode::MovI,
            0x0D => Opcode::Ldw,
            0x0E => Opcode::Stw,
            0x0F => Opcode::Ldb,
            0x10 => Opcode::Stb,
            0x11 => Opcode::J,
            0x12 => Opcode::JW,
            0x13 => Opcode::Call,
            0x14 => Opcode::Ret,
            0x00 => Opcode::Noop,
            0x1F => Opcode::Halt,
            _ => return Err(RuntimeError(format!("unknown opcode 0x{:2X}", op))),
        })
    }
}

pub fn reg_name(reg: u8) -> String {
    match reg {
        4 => "sp".into(),
        5 => "fp".into(),
        n => format!("r{}", n),
    }
}

#[derive(Debug)]
pub struct Instr {
    pub opcode: Opcode,
    pub format: Format,
}

impl Instr {
    // pub fn to_raw(&self) -> u16 {
    //     let mut instr = self.opcode as u16;

    //     instr |= match self.format {
    //         Format::Rfmt { reserved, rd, rs } => {
    //             (reserved as u16) << 5 | (rd as u16) << 10 | (rs as u16) << 13
    //         }
    //         Format::Ifmt { rd, imm } => (rd as u16) << 5 | (imm as u16) << 8,
    //         Format::Mfmt { rd, rs, imm } => {
    //             (rd as u16) << 5 | (rs as u16) << 8 | (imm as u16) << 11
    //         }
    //         Format::Bfmt { cond, imm } => (cond as u16) << 5 | (imm as u16) << 8,
    //         Format::Efmt { reserved } => reserved << 5,
    //         Format::Sfmt { reserved } => reserved << 5,
    //     };

    //     instr
    // }

    pub fn disassemble(&self) -> Option<String> {
        use Format::*;
        use Opcode::*;

        let s = match (&self.opcode, &self.format) {
            // --- R-Type ---
            (Arith, Rfmt { reserved, rd, rs }) => match reserved {
                0 => format!("ADD {}, {}", reg_name(*rd), reg_name(*rs)),
                1 => format!("SUB {}, {}", reg_name(*rd), reg_name(*rs)),
                _ => return None,
            },
            (Logical, Rfmt { reserved, rd, rs }) => match reserved {
                0 => format!("AND {}, {}", reg_name(*rd), reg_name(*rs)),
                1 => format!("OR {}, {}", reg_name(*rd), reg_name(*rs)),
                2 => format!("XOR {}, {}", reg_name(*rd), reg_name(*rs)),
                _ => return None,
            },
            (Shift, Rfmt { reserved, rd, rs }) => match reserved {
                0 => format!("SHL {}, {}", reg_name(*rd), reg_name(*rs)),
                1 => format!("SHR {}, {}", reg_name(*rd), reg_name(*rs)),
                _ => return None,
            },
            (Mov, Rfmt { rd, rs, .. }) => format!("MOV {}, {}", reg_name(*rd), reg_name(*rs)),

            // --- I-Type ---
            (AddI, Ifmt { rd, imm }) => format!("ADDI {}, {}", reg_name(*rd), imm),
            (SubI, Ifmt { rd, imm }) => format!("SUBI {}, {}", reg_name(*rd), imm),
            (AndI, Ifmt { rd, imm }) => format!("ANDI {}, {}", reg_name(*rd), imm),
            (OrI, Ifmt { rd, imm }) => format!("ORI {}, {}", reg_name(*rd), imm),
            (XorI, Ifmt { rd, imm }) => format!("XORI {}, {}", reg_name(*rd), imm),
            (ShlI, Ifmt { rd, imm }) => format!("SHLI {}, {}", reg_name(*rd), imm),
            (ShrI, Ifmt { rd, imm }) => format!("SHRI {}, {}", reg_name(*rd), imm),
            (MovI, Ifmt { rd, imm }) => format!("MOVI {}, {}", reg_name(*rd), imm),

            // --- M-Type ---
            (Ldw, Mfmt { rd, rs, imm }) => {
                format!("LDW {}, [{}+{}]", reg_name(*rd), reg_name(*rs), imm)
            }
            (Stw, Mfmt { rd, rs, imm }) => {
                format!("STW [{}+{}], {}", reg_name(*rd), imm, reg_name(*rs))
            }
            (Ldb, Mfmt { rd, rs, imm }) => {
                format!("LDB {}, [{}+{}]", reg_name(*rd), reg_name(*rs), imm)
            }
            (Stb, Mfmt { rd, rs, imm }) => {
                format!("STB [{}+{}], {}", reg_name(*rd), imm, reg_name(*rs))
            }

            // --- B-Type ---
            (J, Bfmt { cond, imm }) => match cond {
                0 => format!("JMP {}", imm),
                1 => format!("JEQ {}", imm),
                2 => format!("JLT {}", imm),
                3 => format!("JGT {}", imm),
                _ => return None,
            },

            // --- E-Type ---
            (JW, Efmt { reserved }) => match reserved {
                0 => "JMPW".to_string(),
                1 => "JEQW".to_string(),
                2 => "JLTW".to_string(),
                3 => "JGTW".to_string(),
                _ => return None,
            },
            (Call, Efmt { .. }) => "CALL".to_string(),

            // --- S-Type ---
            (Ret, Sfmt { .. }) => "RET".to_string(),
            (Noop, Sfmt { .. }) => "NOOP".to_string(),
            (Halt, Sfmt { .. }) => "HALT".to_string(),

            _ => return None,
        };

        Some(s)
    }
}

#[derive(Debug)]
pub enum Format {
    Rfmt { reserved: u8, rd: u8, rs: u8 },
    Ifmt { rd: u8, imm: u8 },
    Mfmt { rd: u8, rs: u8, imm: u8 },
    Bfmt { cond: u8, imm: u8 },
    Efmt { reserved: u16 },
    Sfmt { reserved: u16 },
}

// inclusive
pub fn get_bits(val: u16, start: u8, stop: u8) -> u16 {
    let len = 1 + stop - start;
    let mask = (0x1 << len) - 1;
    (val >> (15 - stop)) & mask
}

pub fn parse_instr(instr: u16) -> RuntimeResult<Instr> {
    let opcode = get_bits(instr, 11, 15) as u8;

    let format = match opcode {
        0x01..=0x04 => Format::Rfmt {
            reserved: get_bits(instr, 11, 15) as u8,
            rd: get_bits(instr, 3, 5) as u8,
            rs: get_bits(instr, 0, 3) as u8,
        },
        0x05..=0x0C => Format::Ifmt {
            rd: get_bits(instr, 8, 10) as u8,
            imm: get_bits(instr, 0, 7) as u8,
        },
        0x0D..=0x10 => Format::Mfmt {
            rd: get_bits(instr, 8, 10) as u8,
            rs: get_bits(instr, 5, 7) as u8,
            imm: get_bits(instr, 0, 7) as u8,
        },
        0x11 => Format::Bfmt {
            cond: get_bits(instr, 9, 10) as u8,
            imm: get_bits(instr, 0, 7) as u8,
        },
        0x12 | 0x13 => Format::Efmt {
            reserved: get_bits(instr, 0, 10),
        },
        0x00 | 0xFF | 0x14 => Format::Sfmt {
            reserved: get_bits(instr, 0, 10),
        },
        _ => return Err(RuntimeError(format!("invalid instruction 0x{:4X}", instr))),
    };

    Ok(Instr {
        opcode: Opcode::from_u8(opcode)?,
        format,
    })
}
