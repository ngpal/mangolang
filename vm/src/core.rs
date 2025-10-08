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
pub const INFO_HEIGHT: usize = 5;

#[derive(Default)]
pub struct Flags {
    pub n: bool,
    pub z: bool,
    pub v: bool,
}

pub struct Registers([u16; 10]); // 8 general purpose, sp, fp

impl Registers {
    pub fn new() -> Self {
        Self([0, 0, 0, 0, 0, 0, 0, 0, 0xFFFE, 0xFFFE]) // Stack and fram pointers are at the bottom
    }

    // general purpose
    pub fn get_reg(&self, id: u8) -> RuntimeResult<u16> {
        Ok(match id {
            0..=7 => self.0[id as usize],
            8 => self.get_sp(),
            9 => self.get_fp(),
            _ => return Err(RuntimeError(format!("invalid general register r{}", id))),
        })
    }

    pub fn set_reg(&mut self, id: u8, val: u16) -> RuntimeResult<()> {
        match id {
            0..=7 => self.0[id as usize] = val,
            8 => self.set_sp(val),
            9 => self.set_fp(val),
            _ => return Err(RuntimeError(format!("invalid general register r{}", id))),
        }

        Ok(())
    }

    // stack pointer
    pub fn get_sp(&self) -> u16 {
        self.0[8]
    }

    pub fn set_sp(&mut self, val: u16) {
        self.0[8] = val;
    }

    pub fn inc_sp(&mut self, amt: u16) {
        self.0[8] = self.0[8].wrapping_add(amt);
    }

    pub fn dec_sp(&mut self, amt: u16) {
        self.0[8] = self.0[8].wrapping_sub(amt);
    }

    // frame pointer
    pub fn get_fp(&self) -> u16 {
        self.0[9]
    }

    pub fn set_fp(&mut self, val: u16) {
        self.0[9] = val;
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

        Ok(())
    }

    // fn allocate_locals(&mut self) -> RuntimeResult<()> {
    //     let mut max_local = 0;
    //     let mut ip = 0;
    //     let program_start = 0; // memory index 0 now contains first instruction

    //     while ip < self.program_end {
    //         let instr_byte = self.memory[program_start + ip];
    //         if instr_byte == Instr::Store8 as u8 {
    //             max_local = cmp::max(max_local, self.memory[program_start + ip + 1]);
    //         }

    //         ip += Instr::from_u8(instr_byte)
    //             .ok_or(RuntimeError(format!(
    //                 "formatting issue on byte {} (0x{:X}) in the instructions",
    //                 ip, instr_byte
    //             )))?
    //             .byte_len();
    //     }

    //     let space_needed = 2 * (max_local as usize + 1);
    //     if (self.registers.get_sp() as usize) < space_needed + self.program_end {
    //         return Err(RuntimeError(
    //             "memory overflow - too many local variables".into(),
    //         ));
    //     }

    //     self.registers.dec_sp(space_needed as u16);
    //     Ok(())
    // }

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
            Instr::Add | Instr::Cmp | Instr::And | Instr::Or | Instr::Xor | Instr::Shft => {
                let right = self.pop_word()?;
                let left = self.pop_word()?;

                let (result, overflow) = match instr {
                    Instr::Add => left.overflowing_add(right),
                    Instr::Cmp => left.overflowing_sub(right),
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
            Instr::Ldw => {
                let addr = self.pop_word()?;
                self.push_word(self.read_word(addr))?;
            }
            Instr::Stw => {
                let val = self.pop_word()?;
                let addr = self.pop_word()?;
                self.write_word(addr, val)?;
            }
            Instr::Ldb => {
                let addr = self.pop_word()?;
                self.push_word(self.read_byte(addr) as u16)?;
            }
            Instr::Stb => {
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
            instr => {
                return Err(RuntimeError(format!(
                    "Encountered depricated instruction 0x{:4X}",
                    instr as u8,
                )));
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
                Some(Instr::Cmp) => format!("0x{:04X}: CMP", addr),
                Some(Instr::Not) => format!("0x{:04X}: NOT", addr),
                Some(Instr::And) => format!("0x{:04X}: AND", addr),
                Some(Instr::Or) => format!("0x{:04X}: OR", addr),
                Some(Instr::Xor) => format!("0x{:04X}: XOR", addr),
                Some(Instr::Shft) => format!("0x{:04X}: SHFT", addr),
                Some(Instr::Ldw) => format!("0x{:04X}: LOADP", addr),
                Some(Instr::Stw) => format!("0x{:04X}: STOREP", addr),
                Some(Instr::Ldb) => format!("0x{:04X}: LOADPB", addr),
                Some(Instr::Stb) => format!("0x{:04X}: STOREPB", addr),
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
                Some(_) => format!("0x{:04X}: <instr 0x{:02X}>", addr, byte),
                None => format!("0x{:04X}: DB 0x{:02X}", addr, byte),
            };

            output.push(line);
            addr += size;
        }

        output
    }
}
