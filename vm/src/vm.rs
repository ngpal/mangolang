use std::cmp::max;

use crate::error::{RuntimeError, RuntimeResult};

const MEM_SIZE: usize = 0x10000;

#[derive(Debug)]
pub enum Instruction {
    Push(u16),
    Load(u16),
    Store(u16),
    Icmp,
    Jmp(i16),
    Jlt(i16),
    Jgt(i16),
    Jeq(i16),
    Not,
    Iadd,
    Isub,
    Imul,
    Idiv,
    Neg,
    Halt,
}

#[derive(Default)]
pub struct Flags {
    pub n: bool,
    pub z: bool,
    pub v: bool,
}

pub struct Vm<'ip> {
    pub memory: [u8; MEM_SIZE],
    pub flags: Flags,
    pub sp: u16,
    pub ip: u16,
    pub fp: u16,
    pub program: &'ip [Instruction],
}

impl<'ip> Vm<'ip> {
    pub fn new(program: &'ip [Instruction]) -> RuntimeResult<Self> {
        let mut ret = Self {
            memory: [0; MEM_SIZE],
            flags: Flags::default(),
            ip: 0,
            fp: 0xFFFE,
            sp: 0xFFFE,
            program,
        };

        ret.allocate_locals(program)?;
        Ok(ret)
    }

    fn allocate_locals(&mut self, program: &[Instruction]) -> RuntimeResult<()> {
        let mut max_local = 0;
        for instr in program {
            if let Instruction::Store(x) = instr {
                max_local = max(*x, max_local)
            }
        }

        let space_needed = 2 * (max_local + 1);
        if self.sp < space_needed {
            return Err(RuntimeError(
                "memory overflow - too many local variables".into(),
            ));
        }

        self.sp -= space_needed;
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

    // Return halt signal
    pub fn exec_instruction(&mut self) -> RuntimeResult<bool> {
        let instr = &self.program[self.ip as usize];

        match instr {
            Instruction::Push(val) => self.push_word(*val)?,
            Instruction::Iadd
            | Instruction::Isub
            | Instruction::Imul
            | Instruction::Idiv
            | Instruction::Icmp => {
                let right = self.pop_word()?;
                let left = self.pop_word()?;

                let (result, overflow) = match instr {
                    Instruction::Iadd => left.overflowing_add(right),
                    Instruction::Isub | Instruction::Icmp => left.overflowing_sub(right),
                    Instruction::Imul => left.overflowing_mul(right),
                    Instruction::Idiv => {
                        if right == 0 {
                            return Err(RuntimeError("division by zero".into()));
                        }
                        let (res, overflow) = (left as i16).overflowing_div(right as i16);
                        (res as u16, overflow)
                    }
                    _ => unreachable!(),
                };

                if !(matches!(instr, Instruction::Icmp)) {
                    self.push_word(result)?;
                }

                self.set_flags(result, overflow);
            }
            Instruction::Neg => {
                let operand = self.pop_word()?;
                self.push_word((!operand).wrapping_add(1))?
            }
            Instruction::Store(offset) => {
                let addr = self.fp - offset * 2;
                let val = self.pop_word()?;
                if addr as usize + 1 >= MEM_SIZE {
                    return Err(RuntimeError(format!(
                        "invalid memory access 0x{:04X}",
                        addr
                    )));
                }
                let bytes = val.to_le_bytes();
                self.memory[addr as usize] = bytes[0];
                self.memory[addr as usize + 1] = bytes[1];
            }
            Instruction::Load(offset) => {
                let addr = self.fp - offset * 2;
                if addr as usize + 1 >= MEM_SIZE {
                    return Err(RuntimeError(format!(
                        "invalid memory access 0x{:04X}",
                        addr
                    )));
                }
                let val = u16::from_le_bytes(
                    self.memory[addr as usize..addr as usize + 2]
                        .try_into()
                        .unwrap(),
                );
                self.push_word(val)?;
            }
            Instruction::Jmp(offset) => {
                let new_ip = (self.ip as isize).wrapping_add(*offset as isize);
                if new_ip < 0 || new_ip as usize >= self.program.len() {
                    return Err(RuntimeError(format!("invalid jump target {}", new_ip)));
                }
                self.ip = new_ip as u16;
                return Ok(false);
            }
            Instruction::Jlt(offset) => {
                if self.flags.n {
                    let new_ip = (self.ip as isize).wrapping_add(*offset as isize);
                    if new_ip < 0 || new_ip as usize >= self.program.len() {
                        return Err(RuntimeError(format!("invalid jump target {}", new_ip)));
                    }
                    self.ip = new_ip as u16;
                    return Ok(false);
                }
            }
            Instruction::Jgt(offset) => {
                if !self.flags.z && !self.flags.n {
                    let new_ip = (self.ip as isize).wrapping_add(*offset as isize);
                    if new_ip < 0 || new_ip as usize >= self.program.len() {
                        return Err(RuntimeError(format!("invalid jump target {}", new_ip)));
                    }
                    self.ip = new_ip as u16;
                    return Ok(false);
                }
            }
            Instruction::Jeq(offset) => {
                if self.flags.z {
                    let new_ip = (self.ip as isize).wrapping_add(*offset as isize);
                    if new_ip < 0 || new_ip as usize >= self.program.len() {
                        return Err(RuntimeError(format!("invalid jump target {}", new_ip)));
                    }
                    self.ip = new_ip as u16;
                    return Ok(false);
                }
            }
            Instruction::Not => {
                let val = self.pop_word()?;
                let result = if val == 0 { 1 } else { 0 };
                self.push_word(result)?;
                self.set_flags(result, false);
            }
            Instruction::Halt => return Ok(true),
        }

        self.ip += 1;
        Ok(false)
    }

    // Execute instructions
    pub fn exec(&mut self) -> RuntimeResult<()> {
        let mut halt = false;
        while !halt && (self.ip as usize) < self.program.len() {
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

pub fn parse_program(source: &str) -> RuntimeResult<Vec<Instruction>> {
    source
        .lines()
        .map(|line| {
            let words: Vec<&str> = line.split_whitespace().collect();

            match words.get(0) {
                Some(cmd) => {
                    let cmd_upper = cmd.to_uppercase();
                    match cmd_upper.as_str() {
                        "PUSH" => {
                            if let Some(val) = words.get(1) {
                                Ok(Instruction::Push(val.parse().map_err(|_| {
                                    RuntimeError(format!("Invalid number for PUSH: {}", val))
                                })?))
                            } else {
                                Err(RuntimeError("Missing argument for PUSH".into()))
                            }
                        }
                        "LOAD" => {
                            if let Some(val) = words.get(1) {
                                Ok(Instruction::Load(val.parse().map_err(|_| {
                                    RuntimeError(format!("Invalid index for LOAD: {}", val))
                                })?))
                            } else {
                                Err(RuntimeError("Missing argument for LOAD".into()))
                            }
                        }
                        "STOR" => {
                            if let Some(val) = words.get(1) {
                                Ok(Instruction::Store(val.parse().map_err(|_| {
                                    RuntimeError(format!("Invalid index for STORE: {}", val))
                                })?))
                            } else {
                                Err(RuntimeError("Missing argument for STORE".into()))
                            }
                        }
                        "ICMP" => Ok(Instruction::Icmp),
                        "JMP" => {
                            if let Some(val) = words.get(1) {
                                Ok(Instruction::Jmp(val.parse().map_err(|_| {
                                    RuntimeError(format!("Invalid offset for JMP: {}", val))
                                })?))
                            } else {
                                Err(RuntimeError("Missing argument for JMP".into()))
                            }
                        }
                        "JLT" => {
                            if let Some(val) = words.get(1) {
                                Ok(Instruction::Jlt(val.parse().map_err(|_| {
                                    RuntimeError(format!("Invalid offset for JLT: {}", val))
                                })?))
                            } else {
                                Err(RuntimeError("Missing argument for JLT".into()))
                            }
                        }
                        "JGT" => {
                            if let Some(val) = words.get(1) {
                                Ok(Instruction::Jgt(val.parse().map_err(|_| {
                                    RuntimeError(format!("Invalid offset for JGT: {}", val))
                                })?))
                            } else {
                                Err(RuntimeError("Missing argument for JGT".into()))
                            }
                        }
                        "JEQ" => {
                            if let Some(val) = words.get(1) {
                                Ok(Instruction::Jeq(val.parse().map_err(|_| {
                                    RuntimeError(format!("Invalid offset for JEQ: {}", val))
                                })?))
                            } else {
                                Err(RuntimeError("Missing argument for JEQ".into()))
                            }
                        }
                        "NOT" => Ok(Instruction::Not),
                        "IADD" => Ok(Instruction::Iadd),
                        "ISUB" => Ok(Instruction::Isub),
                        "IMUL" => Ok(Instruction::Imul),
                        "IDIV" => Ok(Instruction::Idiv),
                        "NEG" => Ok(Instruction::Neg),
                        "HALT" => Ok(Instruction::Halt),
                        _ => Err(RuntimeError(format!("Unknown command: {}", cmd))),
                    }
                }
                None => Ok(Instruction::Halt), // blank line fallback
            }
        })
        .collect()
}
