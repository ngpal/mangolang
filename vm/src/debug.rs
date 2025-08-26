use std::io::{self, Write};

use crate::{
    error::RuntimeResult,
    vm::{Instruction, Vm},
};

pub struct Debugger<'ip> {
    vm: Vm<'ip>,
}

impl<'ip> Debugger<'ip> {
    pub fn new(vm: Vm<'ip>) -> Self {
        Self { vm }
    }

    pub fn debug(&mut self) -> RuntimeResult<()> {
        loop {
            let ip = self.vm.ip as usize;
            let instr = if ip < self.vm.program.len() {
                &self.vm.program[ip]
            } else {
                &Instruction::Halt
            };
            let top = if self.vm.sp < 0xFFFE {
                Some(self.vm.top_word())
            } else {
                None
            };
            println!(
                "IP: {} | SP: {} | FP: {} | Flags: [Z:{} N:{} V:{}] | Top of stack: {:?} | Next instr: {:?}",
                self.vm.ip,
                self.vm.sp,
                self.vm.fp,
                self.vm.flags.z,
                self.vm.flags.n,
                self.vm.flags.v,
                top,
                instr
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
                    println!("Stack snapshot (top to bottom):");
                    let mut sp = self.vm.sp as usize;
                    while sp < 0xFFFE {
                        let val = u16::from_le_bytes([self.vm.memory[sp], self.vm.memory[sp + 1]]);
                        println!("  0x{:04X}: {}", sp, val);
                        sp += 2;
                    }
                }
                "flags" | "f" => {
                    println!(
                        "Flags -> Z:{} N:{} V:{}",
                        self.vm.flags.z, self.vm.flags.n, self.vm.flags.v
                    );
                }
                "ip" => println!("IP: {}", self.vm.ip),
                "help" | "h" => {
                    println!("commands:");
                    println!("  step | s      - execute next instruction");
                    println!("  continue | c  - run until halt");
                    println!("  stack | stk   - show stack contents");
                    println!("  flags | f     - show flags");
                    println!("  ip            - show instruction pointer");
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
