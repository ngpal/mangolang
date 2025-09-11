use crossterm::{
    cursor::{Hide, MoveTo, Show},
    event::{self, Event, KeyCode},
    execute,
    style::Print,
    terminal::{self, Clear, ClearType, EnterAlternateScreen, LeaveAlternateScreen},
};
use std::io::{self, Write, stdout};

use crate::vm::Vm;

pub struct Debugger<'a> {
    vm: &'a mut Vm,
}

impl<'a> Debugger<'a> {
    pub fn new(vm: &'a mut Vm) -> Self {
        Self { vm }
    }

    pub fn run(&mut self) -> io::Result<()> {
        let mut stdout = stdout();
        execute!(stdout, EnterAlternateScreen, Hide)?;
        terminal::enable_raw_mode()?;

        let mut info_lines: Vec<String> = vec!["hit 'q' to quit debugger".into()];
        let mut running = false; // true if 'c' pressed

        let result = (|| {
            loop {
                execute!(stdout, MoveTo(0, 0), Clear(ClearType::All))?;

                // draw panels
                self.draw_disassembly(&mut stdout)?;
                self.draw_registers(&mut stdout)?;
                self.draw_stack(&mut stdout)?;
                self.draw_memory(&mut stdout, 0, 8)?; // first 8 rows of memory
                self.draw_screen(&mut stdout)?;
                self.draw_info(&mut stdout, &info_lines)?;

                stdout.flush()?;

                // handle VM execution
                if running {
                    match self.vm.exec_instruction() {
                        Ok(true) => info_lines.push("program halted".into()),
                        Ok(false) => {}
                        Err(e) => {
                            info_lines.push(format!("runtime error: {}", e));
                            running = false;
                        }
                    }
                }

                // poll keyboard
                if event::poll(std::time::Duration::from_millis(50))? {
                    if let Event::Key(key) = event::read()? {
                        match key.code {
                            KeyCode::Char('q') => break Ok(()),
                            KeyCode::Char('s') => {
                                // single step
                                match self.vm.exec_instruction() {
                                    Ok(true) => info_lines.push("program halted".into()),
                                    Ok(false) => {}
                                    Err(e) => info_lines.push(format!("runtime error: {}", e)),
                                }
                            }
                            KeyCode::Char('c') => running = true, // continue
                            _ => {}
                        }
                    }
                }
            }
        })();

        terminal::disable_raw_mode()?;
        execute!(stdout, Show, LeaveAlternateScreen)?;

        result
    }

    fn draw_memory(
        &self,
        stdout: &mut std::io::Stdout,
        start_addr: usize,
        rows: usize,
    ) -> io::Result<()> {
        execute!(stdout, Print("\r\nMemory (hex dump):\r\n"))?;
        let mem = &self.vm.memory;
        for r in 0..rows {
            let addr = start_addr + r * 16;
            if addr >= mem.len() {
                break;
            }
            let line_bytes: Vec<String> = mem[addr..(addr + 16).min(mem.len())]
                .iter()
                .map(|b| format!("{:02X}", b))
                .collect();
            let ascii: String = mem[addr..(addr + 16).min(mem.len())]
                .iter()
                .map(|b| {
                    if b.is_ascii_graphic() || *b == b' ' {
                        *b as char
                    } else {
                        '.'
                    }
                })
                .collect();
            execute!(
                stdout,
                Print(format!(
                    "{:04X}: {:<48}  {}\r\n",
                    addr,
                    line_bytes.join(" "),
                    ascii
                ))
            )?;
        }
        Ok(())
    }

    fn draw_screen(&self, stdout: &mut std::io::Stdout) -> io::Result<()> {
        const VIDEO_BASE: usize = 0x8000; // VM video memory start
        const WIDTH: usize = 32;
        const HEIGHT: usize = 16;

        execute!(stdout, Print("\r\nScreen:\r\n"))?;
        for y in 0..HEIGHT {
            for x in 0..WIDTH {
                let idx = VIDEO_BASE + y * WIDTH + x;
                if idx >= self.vm.memory.len() {
                    break;
                }
                let ch = self.vm.memory[idx];
                let display = if ch == 0 { ' ' } else { ch as char };
                execute!(stdout, Print(display))?;
            }
            execute!(stdout, Print("\r\n"))?;
        }
        Ok(())
    }

    fn draw_disassembly(&mut self, stdout: &mut std::io::Stdout) -> io::Result<()> {
        let mut addr = self.vm.ip as usize;
        let lines = 10; // number of instructions to display

        execute!(
            stdout,
            MoveTo(0, 0),
            Print("┏━━ Disassembly ━━━━━━━━━━━━━━━━━━━━━━━┓\r\n")
        )?;

        for _ in 0..lines {
            let (instr, size) = self.disasm_at(addr);
            let marker = if addr == self.vm.ip as usize {
                ">"
            } else {
                " "
            };
            execute!(
                stdout,
                Print(format!("{} 0x{:04X}: {:<20}\r\n", marker, addr, instr))
            )?;
            addr += size;
        }

        execute!(
            stdout,
            Print("┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\r\n")
        )?;
        Ok(())
    }

    fn disasm_at(&mut self, addr: usize) -> (String, usize) {
        let op = self.vm.memory.get(addr).copied().unwrap_or(0);
        let mut size = 1;
        let instr_str = match op {
            0x01 => {
                // PUSH16
                let imm = if addr + 2 < self.vm.memory.len() {
                    u16::from_le_bytes([self.vm.memory[addr + 1], self.vm.memory[addr + 2]])
                } else {
                    0
                };
                size = 3;
                format!("PUSH16 {}", imm)
            }
            0x0F => "HALT".into(),
            0x10 => {
                size = 2;
                format!("LOAD8 {}", self.vm.memory[addr + 1])
            }
            0x11 => {
                size = 2;
                format!("STORE8 {}", self.vm.memory[addr + 1])
            }
            0x12 => "LOADP".into(),
            0x13 => "STOREP".into(),
            0x20 => {
                size = 2;
                format!("JMP8 {}", self.vm.memory[addr + 1] as i8)
            }
            0x21 => {
                size = 2;
                format!("JLT8 {}", self.vm.memory[addr + 1] as i8)
            }
            0x22 => {
                size = 2;
                format!("JGT8 {}", self.vm.memory[addr + 1] as i8)
            }
            0x23 => {
                size = 2;
                format!("JEQ8 {}", self.vm.memory[addr + 1] as i8)
            }
            0x24 => {
                size = 3;
                let addr16 =
                    u16::from_le_bytes([self.vm.memory[addr + 1], self.vm.memory[addr + 2]]);
                format!("CALL 0x{:04X}", addr16)
            }
            0x25 => {
                size = 2;
                format!("RET")
            } // ignore rel8 for simplicity
            0x30 => "ADD".into(),
            0x31 => "SUB".into(),
            0x32 => "MUL".into(),
            0x33 => "DIV".into(),
            0x34 => "NEG".into(),
            0x35 => "CMP".into(),
            0x40 => "NOT".into(),
            0x41 => "AND".into(),
            0x42 => "OR".into(),
            0x43 => "XOR".into(),
            0x44 => "SHFT".into(),
            0x50 => {
                // MOV rd, rs
                size = 2;
                let byte = self.vm.memory[addr + 1];
                let rd = byte >> 4;
                let rs = byte & 0xF;
                format!("MOV r{} r{}", rd, rs)
            }
            0x51 => {
                size = 2;
                format!("PUSHR r{}", self.vm.memory[addr + 1])
            }
            0x52 => {
                size = 2;
                format!("POPR r{}", self.vm.memory[addr + 1])
            }
            0x60 => "PRINT".into(),
            0x61 => {
                size = 2;
                format!("MVCUR {}", self.vm.memory[addr + 1] as i8)
            }
            _ => format!("DB 0x{:02X}", op),
        };

        (instr_str, size)
    }

    fn draw_registers(&self, stdout: &mut std::io::Stdout) -> io::Result<()> {
        let r = |i| self.vm.registers.get_reg(i).unwrap_or(0);
        let sp = self.vm.registers.get_sp();
        let fp = self.vm.registers.get_fp();
        let ip = self.vm.ip;
        let f = &self.vm.flags;

        execute!(
            stdout,
            Print(format!(
                "\r\nRegisters + Flags:\r\nr0={:04X} r1={:04X} r2={:04X} r3={:04X} SP={:04X} FP={:04X} IP={:04X} Flags: Z={} N={} V={}\r\n",
                r(0),
                r(1),
                r(2),
                r(3),
                sp,
                fp,
                ip,
                f.z,
                f.n,
                f.v
            ))
        )?;
        Ok(())
    }

    fn draw_stack(&self, stdout: &mut std::io::Stdout) -> io::Result<()> {
        execute!(stdout, Print("\r\nStack (top -> bottom):\r\n"))?;
        let mut sp = self.vm.registers.get_sp() as usize;
        let mem = &self.vm.memory;

        for _ in 0..16 {
            if sp + 1 >= mem.len() {
                break;
            }
            let val = u16::from_le_bytes([mem[sp], mem[sp + 1]]);
            execute!(stdout, Print(format!("[0x{:04X}] = {:04X}\r\n", sp, val)))?;
            sp += 2;
        }
        Ok(())
    }

    fn draw_info(&self, stdout: &mut std::io::Stdout, info_lines: &[String]) -> io::Result<()> {
        execute!(stdout, Print("\r\nInfo / Messages:\r\n"))?;
        for line in info_lines.iter().rev().take(5) {
            // show last 5 messages
            execute!(stdout, Print(line), Print("\r\n"))?;
        }
        Ok(())
    }
}
