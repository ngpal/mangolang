use crate::{
    error::{RuntimeError, RuntimeResult},
    instr::Instr,
    video::Video,
};

const MBIN_MAGIC: &[u8; 4] = b"MBIN";
const STACK_START: usize = 0xFF00;
// const HEAP_START: usize = 0x4A10;
const MMIO_START: usize = 0x4910;
const USER_CODE_START: usize = 0x0910;
const USER_CODE_SIZE: usize = MMIO_START - USER_CODE_START;
// const INT_HANDLER_START: usize = 0x0110;
const IVT_START: usize = 0x0100;
// const ROM_START: usize = 0x0000;
pub const MEM_SIZE: usize = 0x10000;

#[derive(Default)]
pub struct Flags {
    pub n: bool,
    pub z: bool,
    pub v: bool,
    pub k: bool, // kernel mode
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

        if program.len() - 4 > USER_CODE_SIZE {
            return Err(RuntimeError(format!(
                "Program exceeds size of {} bytes",
                USER_CODE_SIZE
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

    fn read_word(&self, addr: u16) -> u16 {
        let lo = self.memory[addr as usize];
        let hi = self.memory[addr as usize + 1];
        u16::from_le_bytes([lo, hi])
    }

    fn write_word(&mut self, addr: u16, word: u16) -> RuntimeResult<()> {
        if (addr as usize) < STACK_START && !self.flags.k {
            return Err(RuntimeError(
                "attempt to write outside of stack in user mode".to_string(),
            ));
        } else if (addr as usize) < IVT_START {
            return Err(RuntimeError("attempt to write to ROM".to_string()));
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
        if (addr as usize) < STACK_START && !self.flags.k {
            return Err(RuntimeError(
                "attemtpt to write outside of stack in user mode".to_string(),
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
    pub fn exec_instruction(&mut self, video: &mut Video) -> RuntimeResult<bool> {
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
                let addr = self.pop_word()?;
                let val = self.pop_word()?;
                self.write_word(addr, val)?;
            }
            Instr::Ldb => {
                let addr = self.pop_word()?;
                self.push_word(self.read_byte(addr) as u16)?;
            }
            Instr::Stb => {
                let addr = self.pop_word()?;
                let val = self.pop_word()? as u8;
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
            Instr::Int => {
                let int = self.fetch_byte();
                self.exec_interrupt(int, video)?;
                self.flags.k = true;
            }
            Instr::Iret => {
                if !self.flags.k {
                    return Err(RuntimeError(
                        "Attempted to run privilaged instruction in user mode".to_string(),
                    ));
                }

                let addr = self.pop_word()?;
                self.ip = addr;
                self.flags.k = false;
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
            8 => "SP".into(),
            9 => "FP".into(),
            n => format!("r{}", n),
        }
    }

    pub fn disassemble(&self, start: usize, end: usize) -> Vec<String> {
        let mut addr = start;
        let mut output = Vec::new();

        while addr < end && addr < self.program_end {
            let byte = self.memory[addr];
            let instr = Instr::from_u8(byte);
            let line = match instr {
                Some(Instr::Push16) => {
                    if addr + 2 < self.memory.len() {
                        let imm =
                            u16::from_le_bytes([self.memory[addr + 1], self.memory[addr + 2]]);
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
                    format!("0x{:04X}: JMP8 {}", addr, self.memory[addr + 1] as i8)
                }
                Some(Instr::Jlt8) => {
                    format!("0x{:04X}: JLT8 {}", addr, self.memory[addr + 1] as i8)
                }
                Some(Instr::Jgt8) => {
                    format!("0x{:04X}: JGT8 {}", addr, self.memory[addr + 1] as i8)
                }
                Some(Instr::Jeq8) => {
                    format!("0x{:04X}: JEQ8 {}", addr, self.memory[addr + 1] as i8)
                }
                Some(Instr::Call) => {
                    let target = u16::from_le_bytes([self.memory[addr + 1], self.memory[addr + 2]]);
                    format!("0x{:04X}: CALL 0x{:04X}", addr, target)
                }
                Some(Instr::Ret) => format!("0x{:04X}: RET", addr),
                Some(Instr::Mov) => {
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
                    format!("0x{:04X}: PUSHR r{}", addr, self.memory[addr + 1])
                }
                Some(Instr::Popr) => {
                    format!("0x{:04X}: POPR r{}", addr, self.memory[addr + 1])
                }
                Some(Instr::Int) => {
                    format!("0x{:04X}: INT {}", addr, self.memory[addr + 1])
                }
                Some(Instr::Iret) => {
                    format!("0x{:04X}: IRET", addr)
                }
                None => format!("0x{:04X}: DB 0x{:02X}", addr, byte),
            };

            let size = instr.map(|x| x.byte_len()).unwrap_or(1);
            output.push(line);
            addr += size;
        }

        output
    }

    fn exec_interrupt(&mut self, int: u8, video: &mut Video) -> RuntimeResult<()> {
        match int {
            // print to video
            0 => {
                video.put_char(self.registers.get_reg(0)? as u8);
            }
            _ => return Err(RuntimeError(format!("unkown interrupt {}", int))),
        }

        // hack for now
        self.flags.k = false;
        Ok(())
    }
}
