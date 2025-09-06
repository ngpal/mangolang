use std::io::{self, Write};

use crate::error::RuntimeResult;
use crate::vm::{Instr, MEM_SIZE, Vm};

pub struct Debugger {
    vm: Vm,
}

impl Debugger {
    pub fn new(vm: Vm) -> Self {
        Self { vm }
    }

    fn read_word(mem: &[u8], addr: usize) -> Option<u16> {
        if addr + 1 < mem.len() {
            Some(u16::from_le_bytes([mem[addr], mem[addr + 1]]))
        } else {
            None
        }
    }

    fn disasm_at(vm: &Vm) -> String {
        let ip = vm.ip as usize;
        if ip >= vm.program_end {
            return "EOF".to_string();
        }

        let op = vm.memory[ip];
        match Instr::from_u8(op) {
            Some(Instr::Push16) => match Self::read_word(&vm.memory, ip + 1) {
                Some(imm) => format!("PUSH16 {}", imm),
                None => "PUSH16 <truncated>".into(),
            },
            Some(Instr::Load8) => {
                let v = vm.memory.get(ip + 1).copied().unwrap_or(0);
                format!("LOAD8 {}", v)
            }
            Some(Instr::Store8) => {
                let v = vm.memory.get(ip + 1).copied().unwrap_or(0);
                format!("STORE8 {}", v)
            }
            Some(Instr::Jmp8) => {
                let off = vm.memory.get(ip + 1).copied().unwrap_or(0) as i8;
                format!("JMP8 {}", off)
            }
            Some(Instr::Jlt8) => {
                let off = vm.memory.get(ip + 1).copied().unwrap_or(0) as i8;
                format!("JLT8 {}", off)
            }
            Some(Instr::Jgt8) => {
                let off = vm.memory.get(ip + 1).copied().unwrap_or(0) as i8;
                format!("JGT8 {}", off)
            }
            Some(Instr::Jeq8) => {
                let off = vm.memory.get(ip + 1).copied().unwrap_or(0) as i8;
                format!("JEQ8 {}", off)
            }
            Some(Instr::Iadd) => "IADD".into(),
            Some(Instr::Isub) => "ISUB".into(),
            Some(Instr::Imul) => "IMUL".into(),
            Some(Instr::Idiv) => "IDIV".into(),
            Some(Instr::Neg) => "NEG".into(),
            Some(Instr::Icmp) => "ICMP".into(),
            Some(Instr::Not) => "NOT".into(),
            Some(Instr::Halt) => "HALT".into(),
            None => format!("DB 0x{:02X}", op),
        }
    }

    pub fn debug(&mut self) -> RuntimeResult<()> {
        loop {
            let next = Self::disasm_at(&self.vm);
            let top = if self.vm.sp < 0xFFFE {
                Some(self.vm.top_word())
            } else {
                None
            };

            println!(
                "IP: 0x{:04X} | SP: 0x{:04X} | FP: 0x{:04X} | Flags [Z:{} N:{} V:{}] | Top: {:?} | Next: {}",
                self.vm.ip,
                self.vm.sp,
                self.vm.fp,
                self.vm.flags.z,
                self.vm.flags.n,
                self.vm.flags.v,
                top,
                next
            );

            // prompt
            print!("(debug) ");
            io::stdout().flush().unwrap();

            // read command
            let mut input = String::new();
            io::stdin().read_line(&mut input).unwrap();
            let cmd = input.trim();

            match cmd {
                "step" | "s" => {
                    let halt = self.vm.exec_instruction()?;
                    if halt {
                        println!("program halted");
                        break;
                    }
                }
                "continue" | "c" => {
                    self.vm.exec()?;
                    println!("program finished");
                    break;
                }
                "stack" | "stk" => {
                    println!("Stack snapshot (top → bottom):");
                    let mut sp = self.vm.sp as usize;
                    while sp + 1 < MEM_SIZE && sp < 0xFFFE {
                        let val = u16::from_le_bytes([self.vm.memory[sp], self.vm.memory[sp + 1]]);
                        println!("  [0x{:04X}] = {}", sp, val);
                        sp += 2;
                    }
                }
                "flags" | "f" => {
                    println!(
                        "Flags -> Z:{} N:{} V:{}",
                        self.vm.flags.z, self.vm.flags.n, self.vm.flags.v
                    );
                }
                "ip" => println!("IP: 0x{:04X}", self.vm.ip),
                "mem" => {
                    let ip = self.vm.ip as usize;
                    let end = (ip + 8).min(self.vm.program_end);
                    print!("mem[0x{:04X}..0x{:04X}]:", ip, end);
                    for i in ip..end {
                        print!(" {:02X}", self.vm.memory[i]);
                    }
                    println!();
                }
                "help" | "h" => {
                    println!("commands:");
                    println!("  step | s      - execute next instruction");
                    println!("  continue | c  - run until halt");
                    println!("  stack | stk   - show stack contents");
                    println!("  flags | f     - show flags");
                    println!("  ip            - show instruction pointer");
                    println!("  mem           - show next few bytes of memory");
                    println!("  help | h      - show this message");
                    println!("  quit | q      - exit debugger");
                }
                "quit" | "q" => break,
                "" => continue, // empty line, just loop
                _ => println!("unknown command: '{}', try 'help'", cmd),
            }
        }

        Ok(())
    }
}
