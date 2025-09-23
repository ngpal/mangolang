use std::cmp;

use crate::{
    error::{RuntimeError, RuntimeResult},
    instr::Instr,
};

const MBIN_MAGIC: &[u8; 4] = b"MBIN";
pub const MEM_SIZE: usize = 0x10000;
const MAX_PROGRAM_SIZE: usize = 32 * 1024; // 32KB

pub const VIDEO_WIDTH: usize = 40;
pub const VIDEO_HEIGHT: usize = 12;
pub const VIDEO_BASE: usize = 0x8000;
pub const VIDEO_SIZE: usize = VIDEO_WIDTH * VIDEO_HEIGHT;
pub const CURSOR_ADDR: usize = VIDEO_BASE + VIDEO_SIZE;
pub const INFO_HEIGHT: usize = 5;

#[derive(Default)]
pub struct Flags {
    pub n: bool,
    pub z: bool,
    pub v: bool,
}

pub struct Registers([u16; 6]); // 4 general purpose, sp, fp

impl Registers {
    pub fn new() -> Self {
        Self([0, 0, 0, 0, 0xFFFE, 0xFFFE]) // Stack and fram pointers are at the bottom
    }

    // general purpose
    pub fn get_reg(&self, id: u8) -> RuntimeResult<u16> {
        Ok(match id {
            0..=3 => self.0[id as usize],
            4 => self.get_sp(),
            5 => self.get_fp(),
            _ => return Err(RuntimeError(format!("invalid general register r{}", id))),
        })
    }

    pub fn set_reg(&mut self, id: u8, val: u16) -> RuntimeResult<()> {
        match id {
            0..=3 => self.0[id as usize] = val,
            4 => self.set_sp(val),
            5 => self.set_fp(val),
            _ => return Err(RuntimeError(format!("invalid general register r{}", id))),
        }

        Ok(())
    }

    // stack pointer
    pub fn get_sp(&self) -> u16 {
        self.0[4]
    }

    pub fn set_sp(&mut self, val: u16) {
        self.0[4] = val;
    }

    pub fn inc_sp(&mut self, amt: u16) {
        self.0[4] = self.0[4].wrapping_add(amt);
    }

    pub fn dec_sp(&mut self, amt: u16) {
        self.0[4] = self.0[4].wrapping_sub(amt);
    }

    // frame pointer
    pub fn get_fp(&self) -> u16 {
        self.0[5]
    }

    pub fn set_fp(&mut self, val: u16) {
        self.0[5] = val;
    }
}

pub struct Vm {
    pub memory: [u8; MEM_SIZE],
    pub flags: Flags,
    pub registers: Registers,
    pub ip: u16,
    pub program_end: usize,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            memory: [0; MEM_SIZE],
            flags: Flags::default(),
            registers: Registers::new(),
            ip: 0,
            program_end: 0,
        }
    }

    pub fn load(&mut self, program: Vec<u8>) -> RuntimeResult<()> {
        if program.len() < 4 {
            return Err(RuntimeError("Program too small".into()));
        }

        // check header
        if &program[0..4] != MBIN_MAGIC {
            return Err(RuntimeError(
                "Invalid binary format (missing MBIN header)".into(),
            ));
        }

        if program.len() - 4 > MAX_PROGRAM_SIZE {
            return Err(RuntimeError(format!(
                "Program exceeds size of {} bytes",
                MAX_PROGRAM_SIZE
            )));
        }

        // copy into memory, skipping header
        for (addr, byte) in program[4..].iter().enumerate() {
            self.memory[addr] = *byte;
        }

        // address after end of program
        self.program_end = program.len() - 4;

        self.allocate_locals()?;

        Ok(())
    }

    fn allocate_locals(&mut self) -> RuntimeResult<()> {
        let mut max_local = 0;
        let mut ip = 0;
        let program_start = 0; // memory index 0 now contains first instruction

        while ip < self.program_end {
            let instr_byte = self.memory[program_start + ip];
            if instr_byte == Instr::Store8 as u8 {
                max_local = cmp::max(max_local, self.memory[program_start + ip + 1]);
            }

            ip += Instr::from_u8(instr_byte)
                .ok_or(RuntimeError(format!(
                    "formatting issue on byte {} in the instructions",
                    ip
                )))?
                .byte_len();
        }

        let space_needed = 2 * (max_local as usize + 1);
        if (self.registers.get_sp() as usize) < space_needed + self.program_end {
            return Err(RuntimeError(
                "memory overflow - too many local variables".into(),
            ));
        }

        self.registers.dec_sp(space_needed as u16);
        Ok(())
    }

    fn read_word(&self, addr: u16) -> u16 {
        let lo = self.memory[addr as usize];
        let hi = self.memory[addr as usize + 1];
        u16::from_le_bytes([lo, hi])
    }

    fn write_word(&mut self, addr: u16, word: u16) -> RuntimeResult<()> {
        if (addr as usize) < self.program_end {
            return Err(RuntimeError(
                "invalid attempt to write to memory in text area".to_string(),
            ));
        }

        self.memory[addr as usize] = (word & 0xFF) as u8;
        self.memory[addr as usize + 1] = (word >> 8) as u8;

        Ok(())
    }

    fn push_word(&mut self, val: u16) -> RuntimeResult<()> {
        if self.registers.get_sp() < 2 {
            return Err(RuntimeError("stack overflow".into()));
        }

        self.write_word(self.registers.get_sp(), val)?;
        self.registers.dec_sp(2);
        Ok(())
    }

    fn pop_word(&mut self) -> RuntimeResult<u16> {
        if (self.registers.get_sp() as usize) + 2 > MEM_SIZE {
            return Err(RuntimeError("stack underflow".into()));
        }

        self.registers.inc_sp(2);
        let val = self.read_word(self.registers.get_sp());
        Ok(val)
    }

    fn read_byte(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn write_byte(&mut self, addr: u16, val: u8) -> RuntimeResult<()> {
        if (addr as usize) < self.program_end {
            return Err(RuntimeError(
                "invalid attempt to write to memory in text area".to_string(),
            ));
        }

        self.memory[addr as usize] = val;
        Ok(())
    }

    fn fetch_byte(&mut self) -> u8 {
        let ret = self.read_byte(self.ip);
        self.ip += 1;
        ret
    }

    fn fetch_word(&mut self) -> u16 {
        let ret = self.read_word(self.ip);
        self.ip += 2;
        ret
    }

    fn jump_rel(&mut self, cond: bool) -> Result<bool, RuntimeError> {
        let offset = self.fetch_byte() as i8 as isize;
        let base = self.ip as isize;
        if cond {
            let new_ip = base.wrapping_add(offset);

            if new_ip < 0 || (new_ip as usize) >= self.program_end {
                return Err(RuntimeError(format!("invalid jump target {}", new_ip)));
            }

            self.ip = new_ip as u16;
        }

        Ok(false)
    }

    // Return halt signal
    pub fn exec_instruction(&mut self) -> RuntimeResult<bool> {
        let byte = self.fetch_byte();
        let instr = Instr::from_u8(byte)
            .ok_or(RuntimeError(format!("unknown instruction: 0x{:2X}", byte)))?;

        match instr {
            Instr::Push16 => {
                let word = self.fetch_word();
                self.push_word(word)?;
            }
            Instr::Halt => return Ok(true),
            Instr::Add
            | Instr::Sub
            | Instr::Mul
            | Instr::Div
            | Instr::Cmp
            | Instr::Mod
            | Instr::And
            | Instr::Or
            | Instr::Xor
            | Instr::Shft => {
                let right = self.pop_word()?;
                let left = self.pop_word()?;

                let (result, overflow) = match instr {
                    Instr::Add => left.overflowing_add(right),
                    Instr::Sub | Instr::Cmp => left.overflowing_sub(right),
                    Instr::Mul => left.overflowing_mul(right),
                    Instr::Div => {
                        if right == 0 {
                            return Err(RuntimeError("division by zero".into()));
                        }
                        let (res, overflow) = (left as i16).overflowing_div(right as i16);
                        (res as u16, overflow)
                    }
                    Instr::Mod => {
                        if right == 0 {
                            return Err(RuntimeError("modulo by zero".into()));
                        }
                        let (res, overflow) = (left as i16).overflowing_rem(right as i16);
                        (res as u16, overflow)
                    }
                    Instr::And => (left & right, false),
                    Instr::Or => (left | right, false),
                    Instr::Xor => (left ^ right, false),
                    Instr::Shft => {
                        let sh = right as i16;

                        let amt = sh.unsigned_abs() as usize;
                        let amt = if amt >= 16 { 15 } else { amt };

                        let word_bits = 16;
                        let res = if sh >= 0 {
                            if amt as u32 >= word_bits {
                                0
                            } else {
                                left << (amt as u32)
                            }
                        } else if (-sh) as u32 >= word_bits {
                            0
                        } else {
                            left >> ((-sh) as u32)
                        };
                        (res, false)
                    }
                    _ => unreachable!(),
                };

                if !(matches!(instr, Instr::Cmp)) {
                    self.push_word(result)?;
                }

                self.set_flags(result, overflow);
            }
            Instr::Neg => {
                let operand = self.pop_word()?;
                self.push_word((!operand).wrapping_add(1))?
            }
            Instr::Store8 => {
                let offset = self.fetch_byte() as u16;
                let addr = self.registers.get_fp() - offset * 2;
                let val = self.pop_word()?;
                self.write_word(addr, val)?;
            }
            Instr::Load8 => {
                let offset = self.fetch_byte() as u16;
                let addr = self.registers.get_fp() - offset * 2;
                self.push_word(self.read_word(addr))?;
            }
            Instr::Jmp8 => return self.jump_rel(true),
            Instr::Jlt8 => return self.jump_rel(self.flags.n),
            Instr::Jgt8 => return self.jump_rel(!self.flags.z && !self.flags.n),
            Instr::Jeq8 => return self.jump_rel(self.flags.z),
            Instr::Not => {
                let val = self.pop_word()?;
                self.push_word(!val)?;
            }
            Instr::Mov => {
                let inp = self.fetch_byte();

                let rd = inp >> 4;
                let rs = inp & 0xF;

                self.registers.set_reg(rd, self.registers.get_reg(rs)?)?;
            }
            Instr::Pushr => {
                let rs = self.fetch_byte();
                self.push_word(self.registers.get_reg(rs)?)?;
            }
            Instr::Popr => {
                let rd = self.fetch_byte();
                let word = self.pop_word()?;

                self.registers.set_reg(rd, word)?;
            }
            Instr::Loadp => {
                let addr = self.pop_word()?;
                self.push_word(self.read_word(addr))?;
            }
            Instr::Storep => {
                let val = self.pop_word()?;
                let addr = self.pop_word()?;
                self.write_word(addr, val)?;
            }
            Instr::Loadpb => {
                let addr = self.pop_word()?;
                self.push_word(self.read_byte(addr) as u16)?;
            }
            Instr::Storepb => {
                let val = self.pop_word()? as u8;
                let addr = self.pop_word()?;
                self.write_byte(addr, val)?;
            }
            Instr::Call => {
                let addr = self.fetch_word();
                self.push_word(self.ip)?;
                self.ip = addr;
            }
            Instr::Ret => {
                let addr = self.pop_word()?;
                self.ip = addr;
            }
            Instr::Print => {
                let ch = self.pop_word()? as u8;

                // read cursor position
                let mut pos = self.read_word(CURSOR_ADDR as u16) as usize;

                if ch == b'\n' {
                    // move cursor to start of next line
                    let row = pos / VIDEO_WIDTH;
                    let next_row = (row + 1) % VIDEO_HEIGHT;
                    pos = next_row * VIDEO_WIDTH;
                } else if pos < VIDEO_SIZE {
                    self.memory[VIDEO_BASE + pos] = ch;
                    // advance cursor
                    pos += 1;
                    if pos >= VIDEO_SIZE {
                        pos = 0;
                    }
                }

                self.write_word(CURSOR_ADDR as u16, pos as u16)?;
            }
            Instr::MvCur => {
                let offset = self.fetch_byte() as i8;
                let cur = self.read_word(CURSOR_ADDR as u16) as i16;
                let new_cur = (cur + offset as i16).clamp(0, (VIDEO_SIZE - 1) as i16);
                self.write_word(CURSOR_ADDR as u16, new_cur as u16)?;
            }
            Instr::Loadr => {
                let reg_byte = self.fetch_byte();
                let reg = self.registers.get_reg(reg_byte)?;
                let offset = self.fetch_byte() as i8;
                self.push_word(self.read_word((reg as i8 + offset) as u16))?
            }
            Instr::Storer => {
                let reg_byte = self.fetch_byte();
                let reg = self.registers.get_reg(reg_byte)?;
                let offset = self.fetch_byte() as i8;
                let word = self.pop_word()?;
                self.write_word((reg as i8 + offset) as u16, word)?;
            }
        }

        Ok(false)
    }

    pub fn top_word(&self) -> u16 {
        let sp = self.registers.get_sp() as usize;
        u16::from_le_bytes([self.memory[sp], self.memory[sp + 1]])
    }

    fn set_flags(&mut self, result: u16, overflow: bool) {
        self.flags.z = result == 0;
        self.flags.n = (result & 0x8000) != 0;
        self.flags.v = overflow;
    }

    pub fn reg_name(reg: u8) -> String {
        match reg {
            4 => "sp".into(),
            5 => "fp".into(),
            n => format!("r{}", n),
        }
    }

    pub fn disassemble(&self, start: usize, end: usize) -> Vec<String> {
        let mut addr = start;
        let mut output = Vec::new();

        while addr < end && addr < self.program_end {
            let byte = self.memory[addr];
            let instr = Instr::from_u8(byte);
            let mut size = 1;
            let line = match instr {
                Some(Instr::Push16) => {
                    if addr + 2 < self.memory.len() {
                        let imm =
                            u16::from_le_bytes([self.memory[addr + 1], self.memory[addr + 2]]);
                        size = 3;
                        format!("0x{:04X}: PUSH16 {}", addr, imm)
                    } else {
                        format!("0x{:04X}: PUSH16 ???", addr)
                    }
                }
                Some(Instr::Halt) => format!("0x{:04X}: HALT", addr),
                Some(Instr::Add) => format!("0x{:04X}: ADD", addr),
                Some(Instr::Sub) => format!("0x{:04X}: SUB", addr),
                Some(Instr::Mul) => format!("0x{:04X}: MUL", addr),
                Some(Instr::Div) => format!("0x{:04X}: DIV", addr),
                Some(Instr::Neg) => format!("0x{:04X}: NEG", addr),
                Some(Instr::Cmp) => format!("0x{:04X}: CMP", addr),
                Some(Instr::Mod) => format!("0x{:04X}: MOD", addr),
                Some(Instr::Not) => format!("0x{:04X}: NOT", addr),
                Some(Instr::And) => format!("0x{:04X}: AND", addr),
                Some(Instr::Or) => format!("0x{:04X}: OR", addr),
                Some(Instr::Xor) => format!("0x{:04X}: XOR", addr),
                Some(Instr::Shft) => format!("0x{:04X}: SHFT", addr),
                Some(Instr::Load8) => {
                    size = 2;
                    format!("0x{:04X}: LOAD8 {}", addr, self.memory[addr + 1])
                }
                Some(Instr::Store8) => {
                    size = 2;
                    format!("0x{:04X}: STORE8 {}", addr, self.memory[addr + 1])
                }
                Some(Instr::Loadp) => format!("0x{:04X}: LOADP", addr),
                Some(Instr::Storep) => format!("0x{:04X}: STOREP", addr),
                Some(Instr::Loadpb) => format!("0x{:04X}: LOADPB", addr),
                Some(Instr::Storepb) => format!("0x{:04X}: STOREPB", addr),
                Some(Instr::Loadr) => {
                    size = 3;
                    let reg = self.memory[addr + 1];
                    let offset = self.memory[addr + 2] as i8;
                    format!(
                        "0x{:04X}: LOADR [{}, {}{}]",
                        addr,
                        Self::reg_name(reg),
                        if offset >= 0 { "+" } else { "" },
                        offset
                    )
                }
                Some(Instr::Storer) => {
                    size = 3;
                    let reg = self.memory[addr + 1];
                    let offset = self.memory[addr + 2] as i8;
                    format!(
                        "0x{:04X}: STORER [{}, {}]",
                        addr,
                        Self::reg_name(reg),
                        offset
                    )
                }
                Some(Instr::Jmp8) => {
                    size = 2;
                    format!("0x{:04X}: JMP8 {}", addr, self.memory[addr + 1] as i8)
                }
                Some(Instr::Jlt8) => {
                    size = 2;
                    format!("0x{:04X}: JLT8 {}", addr, self.memory[addr + 1] as i8)
                }
                Some(Instr::Jgt8) => {
                    size = 2;
                    format!("0x{:04X}: JGT8 {}", addr, self.memory[addr + 1] as i8)
                }
                Some(Instr::Jeq8) => {
                    size = 2;
                    format!("0x{:04X}: JEQ8 {}", addr, self.memory[addr + 1] as i8)
                }
                Some(Instr::Call) => {
                    size = 3;
                    let target = u16::from_le_bytes([self.memory[addr + 1], self.memory[addr + 2]]);
                    format!("0x{:04X}: CALL 0x{:04X}", addr, target)
                }
                Some(Instr::Ret) => format!("0x{:04X}: RET", addr),
                Some(Instr::Mov) => {
                    size = 2;
                    let byte = self.memory[addr + 1];
                    let rd = byte >> 4;
                    let rs = byte & 0xF;
                    format!(
                        "0x{:04X}: MOV {} {}",
                        addr,
                        Self::reg_name(rd),
                        Self::reg_name(rs)
                    )
                }
                Some(Instr::Pushr) => {
                    size = 2;
                    format!("0x{:04X}: PUSHR r{}", addr, self.memory[addr + 1])
                }
                Some(Instr::Popr) => {
                    size = 2;
                    format!("0x{:04X}: POPR r{}", addr, self.memory[addr + 1])
                }
                Some(Instr::Print) => format!("0x{:04X}: PRINT", addr),
                Some(Instr::MvCur) => {
                    size = 2;
                    format!("0x{:04X}: MVCUR {}", addr, self.memory[addr + 1] as i8)
                }
                // Some(_) => format!("0x{:04X}: <instr 0x{:02X}>", addr, byte),
                None => format!("0x{:04X}: DB 0x{:02X}", addr, byte),
            };

            output.push(line);
            addr += size;
        }

        output
    }
}
