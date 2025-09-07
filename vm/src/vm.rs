use std::cmp;

use crate::error::{RuntimeError, RuntimeResult};

pub const MEM_SIZE: usize = 0x10000;
const MAX_PROGRAM_SIZE: usize = 32 * 1024; // 32KB

#[repr(u8)]
pub enum Instr {
    Push16 = 0x01,
    Halt = 0x0F,
    Load8 = 0x10,
    Store8 = 0x11,
    Jmp8 = 0x20,
    Jlt8 = 0x21,
    Jgt8 = 0x22,
    Jeq8 = 0x23,
    Iadd = 0x30,
    Isub = 0x31,
    Imul = 0x32,
    Idiv = 0x33,
    Neg = 0x34,
    Icmp = 0x35,
    Not = 0x40,
    And = 0x41,
    Or = 0x42,
    Xor = 0x43,
    Shft = 0x44,
}

impl Instr {
    pub fn from_u8(byte: u8) -> Option<Self> {
        match byte {
            0x01 => Some(Instr::Push16),
            0x0F => Some(Instr::Halt),
            0x10 => Some(Instr::Load8),
            0x11 => Some(Instr::Store8),
            0x20 => Some(Instr::Jmp8),
            0x21 => Some(Instr::Jlt8),
            0x22 => Some(Instr::Jgt8),
            0x23 => Some(Instr::Jeq8),
            0x30 => Some(Instr::Iadd),
            0x31 => Some(Instr::Isub),
            0x32 => Some(Instr::Imul),
            0x33 => Some(Instr::Idiv),
            0x34 => Some(Instr::Neg),
            0x35 => Some(Instr::Icmp),
            0x40 => Some(Instr::Not),
            0x41 => Some(Instr::And),
            0x42 => Some(Instr::Or),
            0x43 => Some(Instr::Xor),
            0x44 => Some(Instr::Shft),
            _ => None,
        }
    }
}

#[derive(Default)]
pub struct Flags {
    pub n: bool,
    pub z: bool,
    pub v: bool,
}

pub struct Vm {
    pub memory: [u8; MEM_SIZE],
    pub flags: Flags,
    pub sp: u16,
    pub ip: u16,
    pub fp: u16,
    pub program_end: usize,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            memory: [0; MEM_SIZE],
            flags: Flags::default(),
            ip: 0,
            fp: 0xFFFE,
            sp: 0xFFFE,
            program_end: 0,
        }
    }

    pub fn load(&mut self, program: Vec<u8>) -> RuntimeResult<()> {
        if program.len() > MAX_PROGRAM_SIZE {
            return Err(RuntimeError(format!(
                "Program exceeds size of {} bytes",
                MAX_PROGRAM_SIZE
            )));
        }

        for (addr, byte) in program.iter().enumerate() {
            self.memory[addr] = *byte;
        }

        // address after end of program
        self.program_end = program.len();

        self.allocate_locals()?;

        Ok(())
    }

    fn allocate_locals(&mut self) -> RuntimeResult<()> {
        let mut max_local = 0;
        for ip in 0..MAX_PROGRAM_SIZE {
            if self.memory[ip] == Instr::Store8 as u8 {
                max_local = cmp::max(max_local, self.memory[ip + 1]);
            }
        }

        let space_needed = 2 * (max_local as usize + 1);
        if (self.sp as usize) < space_needed + self.program_end {
            return Err(RuntimeError(
                "memory overflow - too many local variables".into(),
            ));
        }

        self.sp -= space_needed as u16;
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
        if self.sp < 2 {
            return Err(RuntimeError("stack overflow".into()));
        }
        self.sp -= 2;
        let bytes = val.to_le_bytes(); // little-endian
        self.memory[self.sp as usize] = bytes[0];
        self.memory[self.sp as usize + 1] = bytes[1];
        Ok(())
    }

    fn pop_word(&mut self) -> RuntimeResult<u16> {
        if (self.sp as usize) + 2 > MEM_SIZE {
            return Err(RuntimeError("stack underflow".into()));
        }
        let val = u16::from_le_bytes(
            self.memory[self.sp as usize..self.sp as usize + 2]
                .try_into()
                .unwrap(),
        );
        self.sp += 2;
        Ok(val)
    }

    fn jump_rel(&mut self, cond: bool) -> Result<bool, RuntimeError> {
        if cond {
            let offset = self.memory[self.ip as usize + 1] as i8 as isize;
            let base = self.ip as isize + 2;
            let new_ip = base.wrapping_add(offset);

            if new_ip < 0 || (new_ip as usize) >= self.program_end {
                return Err(RuntimeError(format!("invalid jump target {}", new_ip)));
            }

            self.ip = new_ip as u16;
        } else {
            // skip jump and offset bytes
            self.ip = self.ip.wrapping_add(2);
        }

        Ok(false)
    }

    // Return halt signal
    pub fn exec_instruction(&mut self) -> RuntimeResult<bool> {
        let byte = self.memory[self.ip as usize];
        let instr = Instr::from_u8(byte)
            .ok_or(RuntimeError(format!("unknown instruction: 0x{:2X}", byte)))?;

        match instr {
            Instr::Push16 => {
                let word = self.read_word(self.ip + 1);
                self.push_word(word)?;
                self.ip += 2;
            }
            Instr::Halt => return Ok(true),
            Instr::Iadd
            | Instr::Isub
            | Instr::Imul
            | Instr::Idiv
            | Instr::Icmp
            | Instr::And
            | Instr::Or
            | Instr::Xor
            | Instr::Shft => {
                let right = self.pop_word()?;
                let left = self.pop_word()?;

                let (result, overflow) = match instr {
                    Instr::Iadd => left.overflowing_add(right),
                    Instr::Isub | Instr::Icmp => left.overflowing_sub(right),
                    Instr::Imul => left.overflowing_mul(right),
                    Instr::Idiv => {
                        if right == 0 {
                            return Err(RuntimeError("division by zero".into()));
                        }
                        let (res, overflow) = (left as i16).overflowing_div(right as i16);
                        (res as u16, overflow)
                    }
                    Instr::And => (left & right, false),
                    Instr::Or => (left | right, false),
                    Instr::Xor => (left ^ right, false),
                    Instr::Shft => {
                        let sh = right as i16;

                        let amt = sh.abs() as usize;
                        let amt = if amt >= 16 { 15 } else { amt };

                        let word_bits = 16;
                        let res = if sh >= 0 {
                            if amt as u32 >= word_bits {
                                0
                            } else {
                                left << (amt as u32)
                            }
                        } else {
                            if (-sh) as u32 >= word_bits {
                                0
                            } else {
                                left >> ((-sh) as u32)
                            }
                        };
                        (res, false)
                    }
                    _ => unreachable!(),
                };

                if !(matches!(instr, Instr::Icmp)) {
                    self.push_word(result)?;
                }

                self.set_flags(result, overflow);
            }
            Instr::Neg => {
                let operand = self.pop_word()?;
                self.push_word((!operand).wrapping_add(1))?
            }
            Instr::Store8 => {
                let offset = self.memory[self.ip as usize + 1] as u16;
                self.ip += 1;

                let addr = self.fp - offset * 2;
                let val = self.pop_word()?;
                self.write_word(addr, val)?;
            }
            Instr::Load8 => {
                let offset = self.memory[self.ip as usize + 1] as u16;
                self.ip += 1;

                let addr = self.fp - offset * 2;
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
        }

        self.ip += 1;
        Ok(false)
    }

    // Execute instructions
    pub fn exec(&mut self) -> RuntimeResult<()> {
        let mut halt = false;
        while !halt && (self.ip as usize) < self.program_end {
            halt = self.exec_instruction()?;
        }

        Ok(())
    }

    pub fn top_word(&self) -> u16 {
        let sp = self.sp as usize;
        u16::from_le_bytes([self.memory[sp], self.memory[sp + 1]])
    }

    fn set_flags(&mut self, result: u16, overflow: bool) {
        self.flags.z = result == 0;
        self.flags.n = (result & 0x8000) != 0;
        self.flags.v = overflow;
    }
}
