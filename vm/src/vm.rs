use crate::error::{RuntimeError, RuntimeResult};

const STACK_SIZE: usize = 16;
const LOCALS_SIZE: usize = 16;

#[derive(Debug)]
pub enum Instruction {
    Push(u32),
    Load(u8),
    Store(u8),
    Icmp,
    Jmp(i8),
    Jlt(i8),
    Jgt(i8),
    Jeq(i8),
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
    n: bool,
    z: bool,
    v: bool,
}

pub struct Vm {
    pub stack: [u32; STACK_SIZE],
    pub locals: [u32; LOCALS_SIZE],
    pub flags: Flags,
    pub sp: usize,
    ip: usize,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            stack: [0; STACK_SIZE],
            locals: [0; LOCALS_SIZE],
            flags: Flags::default(),
            ip: 0,
            sp: 0,
        }
    }

    pub fn exec(&mut self, program: &[Instruction]) -> RuntimeResult<()> {
        while self.ip < program.len() {
            let instr = &program[self.ip];
            match instr {
                Instruction::Push(val) => {
                    self.stack[self.sp] = *val;
                    self.inc_sp()?;
                }
                Instruction::Iadd
                | Instruction::Isub
                | Instruction::Imul
                | Instruction::Idiv
                | Instruction::Icmp => {
                    let right = self.top();
                    self.dec_sp()?;
                    let left = self.top();
                    self.dec_sp()?;

                    let (result, overflow) = match instr {
                        Instruction::Iadd => left.overflowing_add(right),
                        Instruction::Isub | Instruction::Icmp => left.overflowing_sub(right),
                        Instruction::Imul => left.overflowing_mul(right),
                        Instruction::Idiv => {
                            if right == 0 {
                                return Err(RuntimeError("division by zero".into()));
                            }
                            let (res, overflow) = (left as i32).overflowing_div(right as i32);
                            (res as u32, overflow)
                        }
                        _ => unreachable!(),
                    };

                    if !(matches!(instr, Instruction::Icmp)) {
                        self.stack[self.sp] = result;
                        self.inc_sp()?;
                    }

                    self.set_flags(result, overflow);
                }
                Instruction::Neg => {
                    let operand = self.top();
                    self.stack[self.sp - 1] = (!operand).wrapping_add(1);
                }
                Instruction::Store(s) => {
                    if *s as usize >= LOCALS_SIZE {
                        return Err(RuntimeError(format!(
                            "Locals array index {} is out of bounds",
                            s
                        )));
                    }
                    self.locals[*s as usize] = self.pop()?;
                }
                Instruction::Load(s) => {
                    if *s as usize >= LOCALS_SIZE {
                        return Err(RuntimeError(format!("Load index {} out of bounds", s)));
                    }
                    self.push(self.locals[*s as usize])?
                }
                Instruction::Jmp(offset) => {
                    // offset is signed relative to current ip
                    let new_ip = (self.ip as isize).wrapping_add(*offset as isize);
                    if new_ip < 0 || new_ip as usize >= program.len() {
                        return Err(RuntimeError(format!("invalid jump target {}", new_ip)));
                    }
                    self.ip = new_ip as usize;
                }
                Instruction::Jlt(offset) => {
                    if self.flags.n {
                        let new_ip = (self.ip as isize).wrapping_add(*offset as isize);
                        if new_ip < 0 || new_ip as usize >= program.len() {
                            return Err(RuntimeError(format!("invalid jump target {}", new_ip)));
                        }
                        self.ip = new_ip as usize;
                    }
                }
                Instruction::Jgt(offset) => {
                    if !self.flags.z && !self.flags.n {
                        let new_ip = (self.ip as isize).wrapping_add(*offset as isize);
                        if new_ip < 0 || new_ip as usize >= program.len() {
                            return Err(RuntimeError(format!("invalid jump target {}", new_ip)));
                        }
                        self.ip = new_ip as usize;
                    }
                }
                Instruction::Jeq(offset) => {
                    if self.flags.z {
                        let new_ip = (self.ip as isize).wrapping_add(*offset as isize);
                        if new_ip < 0 || new_ip as usize >= program.len() {
                            return Err(RuntimeError(format!("invalid jump target {}", new_ip)));
                        }
                        self.ip = new_ip as usize;
                    }
                }
                Instruction::Not => {
                    let val = self.top();
                    self.stack[self.sp - 1] = if val == 0 { 1 } else { 0 };
                    self.set_flags(self.top(), false);
                }
                Instruction::Halt => return Ok(()),
            }

            self.ip += 1;
        }
        Ok(())
    }

    fn inc_sp(&mut self) -> RuntimeResult<()> {
        if self.sp >= STACK_SIZE {
            Err(RuntimeError("stack overflow".into()))
        } else {
            self.sp += 1;
            Ok(())
        }
    }

    fn dec_sp(&mut self) -> RuntimeResult<()> {
        if self.sp == 0 {
            Err(RuntimeError("stack underflow".into()))
        } else {
            self.sp -= 1;
            Ok(())
        }
    }

    pub fn top(&self) -> u32 {
        if self.sp == 0 {
            0
        } else {
            self.stack[self.sp - 1]
        }
    }

    pub fn pop(&mut self) -> RuntimeResult<u32> {
        let ret = self.top();
        self.dec_sp()?;
        Ok(ret)
    }

    pub fn push(&mut self, val: u32) -> RuntimeResult<()> {
        self.stack[self.sp] = val;
        self.inc_sp()?;
        Ok(())
    }

    fn set_flags(&mut self, result: u32, overflow: bool) {
        self.flags.z = result == 0;
        self.flags.n = (result & 0x8000_0000) != 0;
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
