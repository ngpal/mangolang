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
    Loadp = 0x12,
    Storep = 0x13,
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
    Mov = 0x50,
    Pushr = 0x51,
    Popr = 0x52,
}

impl Instr {
    pub fn from_u8(byte: u8) -> Option<Self> {
        match byte {
            0x01 => Some(Instr::Push16),
            0x0F => Some(Instr::Halt),
            0x10 => Some(Instr::Load8),
            0x11 => Some(Instr::Store8),
            0x12 => Some(Instr::Loadp),
            0x13 => Some(Instr::Storep),
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
            0x50 => Some(Instr::Mov),
            0x51 => Some(Instr::Pushr),
            0x52 => Some(Instr::Popr),
            _ => None,
        }
    }

    pub fn byte_len(&self) -> usize {
        match self {
            Instr::Push16 => 3,
            Instr::Load8 => 2,
            Instr::Store8 => 2,
            Instr::Jmp8 => 2,
            Instr::Jlt8 => 2,
            Instr::Jgt8 => 2,
            Instr::Jeq8 => 2,
            Instr::Mov => 2,
            Instr::Pushr => 2,
            Instr::Popr => 2,
            Instr::Iadd
            | Instr::Isub
            | Instr::Imul
            | Instr::Idiv
            | Instr::Neg
            | Instr::Not
            | Instr::Icmp
            | Instr::Halt => 1,
            Instr::And => 1,
            Instr::Or => 1,
            Instr::Shft => 1,
            Instr::Xor => 1,
            Instr::Loadp => 1,
            Instr::Storep => 1,
        }
    }
}

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
        let mut ip = 0;

        while ip < self.program_end {
            if self.memory[ip] == Instr::Store8 as u8 {
                max_local = cmp::max(max_local, self.memory[ip + 1]);
            }

            ip += Instr::from_u8(self.memory[ip])
                .ok_or(RuntimeError(format!(
                    "formatting issue on byte {} in the instructions",
                    ip
                )))?
                .byte_len()
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
        self.registers.dec_sp(2);
        let bytes = val.to_le_bytes(); // little-endian
        self.memory[self.registers.get_sp() as usize] = bytes[0];
        self.memory[self.registers.get_sp() as usize + 1] = bytes[1];
        Ok(())
    }

    fn pop_word(&mut self) -> RuntimeResult<u16> {
        if (self.registers.get_sp() as usize) + 2 > MEM_SIZE {
            return Err(RuntimeError("stack underflow".into()));
        }
        let val = u16::from_le_bytes(
            self.memory[self.registers.get_sp() as usize..self.registers.get_sp() as usize + 2]
                .try_into()
                .unwrap(),
        );
        self.registers.inc_sp(2);
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

                let addr = self.registers.get_fp() - offset * 2;
                let val = self.pop_word()?;
                self.write_word(addr, val)?;
            }
            Instr::Load8 => {
                let offset = self.memory[self.ip as usize + 1] as u16;
                self.ip += 1;

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
                self.ip += 1;
                let inp = self.memory[self.ip as usize];

                let rd = inp >> 4;
                let rs = inp & 0xF;

                self.registers.set_reg(rd, self.registers.get_reg(rs)?)?;
            }
            Instr::Pushr => {
                self.ip += 1;
                let rs = self.memory[self.ip as usize];

                self.push_word(self.registers.get_reg(rs)?)?;
            }
            Instr::Popr => {
                self.ip += 1;
                let rd = self.memory[self.ip as usize];
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
        let sp = self.registers.get_sp() as usize;
        u16::from_le_bytes([self.memory[sp], self.memory[sp + 1]])
    }

    fn set_flags(&mut self, result: u16, overflow: bool) {
        self.flags.z = result == 0;
        self.flags.n = (result & 0x8000) != 0;
        self.flags.v = overflow;
    }
}
