use crate::{
    error::{RuntimeError, RuntimeResult},
    instr::{Format, Opcode, parse_instr},
};

const MBIN_MAGIC: &[u8; 4] = b"MBIN";
const MAX_PROGRAM_SIZE: usize = 0x1000;

pub const MEM_SIZE: usize = 0x10000;
pub const VIDEO_WIDTH: usize = 40;
pub const VIDEO_HEIGHT: usize = 12;
pub const VIDEO_BASE: usize = 0x8000;
// pub const VIDEO_SIZE: usize = VIDEO_WIDTH * VIDEO_HEIGHT;
// pub const CURSOR_ADDR: usize = VIDEO_BASE + VIDEO_SIZE;
pub const INFO_HEIGHT: usize = 5;

#[derive(Default)]
pub struct Flags {
    pub n: bool,
    pub z: bool,
    pub v: bool,
}

pub struct Registers([u16; 8]); // 6 general purpose, sp, fp

impl Registers {
    pub fn new() -> Self {
        Self([0, 0, 0, 0, 0, 0, 0xFFFE, 0xFFFE]) // Stack and fram pointers are at the bottom
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
    pub lr: u16,
    pub program_end: usize,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            memory: [0; MEM_SIZE],
            flags: Flags::default(),
            registers: Registers::new(),
            ip: 0,
            lr: 0,
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

    fn fetch_word(&mut self) -> u16 {
        let ret = self.read_word(self.ip);
        self.ip += 2;
        ret
    }

    fn jump_rel(&mut self, cond: bool, offset: isize) -> Result<(), RuntimeError> {
        let offset = offset;
        let base = self.ip as isize;
        if cond {
            let new_ip = base.wrapping_add(offset);

            if new_ip < 0 || (new_ip as usize) >= self.program_end {
                return Err(RuntimeError(format!("invalid jump target {}", new_ip)));
            }

            self.ip = new_ip as u16;
        }

        Ok(())
    }

    // Return halt signal
    pub fn exec_instruction(&mut self) -> RuntimeResult<bool> {
        let instr_word = self.fetch_word();
        let instr = parse_instr(instr_word)?;

        match instr.format {
            Format::Rfmt { reserved, rd, rs } => {
                // Could be Arith, Logical, Shift or Move
                let (result, overflow) = match (instr.opcode, reserved) {
                    // add
                    (Opcode::Arith, 0) => self
                        .registers
                        .get_reg(rd)?
                        .overflowing_add(self.registers.get_reg(rs)?),

                    // sub
                    (Opcode::Arith, 1) => self
                        .registers
                        .get_reg(rd)?
                        .overflowing_add(!self.registers.get_reg(rs)? + 1),

                    // and
                    (Opcode::Logical, 0) => {
                        let res = self.registers.get_reg(rd)? & self.registers.get_reg(rs)?;
                        (res, false)
                    }

                    // or
                    (Opcode::Logical, 1) => {
                        let res = self.registers.get_reg(rd)? | self.registers.get_reg(rs)?;
                        (res, false)
                    }

                    // xor
                    (Opcode::Logical, 2) => {
                        let res = self.registers.get_reg(rd)? ^ self.registers.get_reg(rs)?;
                        (res, false)
                    }

                    // shl
                    (Opcode::Shift, 0) => {
                        let res = self.registers.get_reg(rd)? << self.registers.get_reg(rs)?;
                        (res, false)
                    }

                    // shr
                    (Opcode::Shift, 1) => {
                        let res = self.registers.get_reg(rd)? >> self.registers.get_reg(rs)?;
                        (res, false)
                    }

                    // mov
                    (Opcode::Mov, 0) => {
                        let res = self.registers.get_reg(rs)?;
                        (res, false)
                    }

                    _ => {
                        return Err(RuntimeError(format!(
                            "unknown R-format instruction {:4X}",
                            instr_word
                        )));
                    }
                };

                self.set_flags(result, overflow);
                self.registers.set_reg(rd, result)?
            }
            Format::Ifmt { rd, imm } => {
                // addi, subi, andi, ori, xori, shli, shri, movi
                match instr.opcode {
                    Opcode::AddI => self
                        .registers
                        .set_reg(rd, self.registers.get_reg(rd)? + imm as u16)?,
                    Opcode::SubI => self
                        .registers
                        .set_reg(rd, self.registers.get_reg(rd)? + !(imm as u16) + 1)?,
                    Opcode::AndI => self
                        .registers
                        .set_reg(rd, self.registers.get_reg(rd)? & imm as u16)?,
                    Opcode::OrI => self
                        .registers
                        .set_reg(rd, self.registers.get_reg(rd)? | imm as u16)?,
                    Opcode::XorI => self
                        .registers
                        .set_reg(rd, self.registers.get_reg(rd)? ^ imm as u16)?,
                    Opcode::ShlI => self
                        .registers
                        .set_reg(rd, self.registers.get_reg(rd)? << imm as u16)?,
                    Opcode::ShrI => self
                        .registers
                        .set_reg(rd, self.registers.get_reg(rd)? >> imm as u16)?,
                    Opcode::MovI => self.registers.set_reg(rd, imm as u16)?,
                    code => unreachable!("ifmt, opcode {:?}", code),
                }
            }
            Format::Mfmt { rd, rs, imm } => {
                // ldw, stw, ldb, stb
                match instr.opcode {
                    Opcode::Ldw => self
                        .registers
                        .set_reg(rd, self.read_word(self.registers.get_reg(rs)? + imm as u16))?,
                    Opcode::Stw => self.write_word(
                        self.registers.get_reg(rd)? + imm as u16,
                        self.registers.get_reg(rs)?,
                    )?,
                    Opcode::Ldb => self.registers.set_reg(
                        rd,
                        self.read_byte(self.registers.get_reg(rs)? + imm as u16) as u16,
                    )?,
                    Opcode::Stb => self.write_byte(
                        self.registers.get_reg(rd)? + imm as u16,
                        self.registers.get_reg(rs)? as u8,
                    )?,
                    code => unreachable!("mfmt, opcode {:?}", code),
                }
            }
            Format::Bfmt { cond, imm } => {
                // Could only be jump instructions
                match (instr.opcode, cond) {
                    // jmp
                    (Opcode::J, 0) => self.jump_rel(true, imm as i8 as isize)?,

                    // jeq
                    (Opcode::J, 1) => self.jump_rel(self.flags.z == true, imm as i8 as isize)?,

                    // jlt
                    (Opcode::J, 2) => self.jump_rel(self.flags.n == true, imm as i8 as isize)?,

                    // jgt
                    (Opcode::J, 3) => {
                        self.jump_rel(!self.flags.z && !self.flags.n, imm as i8 as isize)?
                    }
                    code => unreachable!("bfmt, opcode {:?}", code),
                }
            }
            Format::Efmt { rd, reserved } => {
                let imm = self.fetch_word();
                // Could only be jump word, movw, or call
                match (instr.opcode, reserved) {
                    // jmpw
                    (Opcode::JW, 0) => self.jump_rel(true, imm as i16 as isize)?,

                    // jeqw
                    (Opcode::JW, 1) => self.jump_rel(self.flags.z == true, imm as i16 as isize)?,

                    // jltw
                    (Opcode::JW, 2) => self.jump_rel(self.flags.n == true, imm as i16 as isize)?,

                    // jgtw
                    (Opcode::JW, 3) => {
                        self.jump_rel(!self.flags.z && !self.flags.n, imm as i16 as isize)?
                    }

                    // call
                    (Opcode::Call, 0) => {
                        self.lr = self.ip;
                        self.ip = imm;
                    }

                    // movw
                    (Opcode::MovW, 0) => {
                        self.registers.set_reg(rd, imm)?;
                    }

                    code => unreachable!("efmt, opcode {:?}", code),
                }
            }
            Format::Sfmt { reserved } => {
                if reserved != 0 {
                    unreachable!("sfmt, invalid reserved {:?}", instr)
                }

                // Could only be ret, noop, halt
                match instr.opcode {
                    Opcode::Ret => self.ip = self.lr,
                    Opcode::Noop => {}
                    Opcode::Halt => return Ok(true),
                    code => unreachable!("sfmt, opcode {:?}", code),
                }
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

    // disassembly + skip next byte
    pub fn disassemble_at(&self, addr: u16) -> (String, bool) {
        let instr_raw = self.read_word(addr);
        let mut extra = false;

        let disassembly = if let Ok(instr) = parse_instr(instr_raw) {
            if matches!(instr.format, Format::Efmt { .. }) {
                extra = true;
            }

            if let Some(mut disassembly) = instr.disassemble() {
                if extra {
                    disassembly += &format!(" 0x{:4X}", self.read_word(addr + 2));
                }
                disassembly
            } else {
                format!("DB 0x{:4X}", instr_raw)
            }
        } else {
            format!("DB 0x{:4X}", instr_raw)
        };

        (disassembly, extra)
    }

    pub fn disassemble(&self, start: usize, end: usize) -> Vec<String> {
        let mut addr = start;
        let mut output = Vec::new();

        while addr < end && addr < self.program_end {
            let (disassembly, skip_next) = self.disassemble_at(addr as u16);
            output.push(disassembly);

            if skip_next {
                addr += 2
            }
            addr += 2;
        }

        output
    }
}
