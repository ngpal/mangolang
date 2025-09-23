use ratatui::{
    Terminal,
    backend::CrosstermBackend,
    crossterm::{
        cursor::{Hide, Show},
        event::{self, Event, KeyCode},
        execute,
        terminal::{EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode},
    },
    layout::{Constraint, Direction, Layout},
    style::{Color, Modifier, Style},
    text::{Line, Span, Text},
    widgets::{Block, Borders, Paragraph, Wrap},
};
use std::io::{self, stdout};

use crate::core::{VIDEO_BASE, VIDEO_HEIGHT, VIDEO_WIDTH, Vm};

pub struct Debugger<'a> {
    vm: &'a mut Vm,
    mem_offset: usize,
    speed: u64, // milliseconds per instruction
}

impl<'a> Debugger<'a> {
    pub fn new(vm: &'a mut Vm) -> Self {
        Self {
            vm,
            mem_offset: 0,
            speed: 100,
        }
    }

    pub fn run(&mut self) -> io::Result<()> {
        enable_raw_mode()?; // disable line buffering / echo
        execute!(stdout(), EnterAlternateScreen, Hide)?; // alt screen + hide cursor

        let backend = CrosstermBackend::new(stdout());
        let mut terminal = Terminal::new(backend)?;

        let mut running = false;
        let mut halted = false;
        let mut info_lines = Vec::new();

        loop {
            terminal.draw(|f| {
                let area = f.area();

                // vertical split: top (screen) / bottom (other panels)
                let vertical_chunks = Layout::default()
                    .direction(Direction::Vertical)
                    .constraints([
                        Constraint::Length(VIDEO_HEIGHT as u16 + 2), // top panel height
                        Constraint::Min(0), // bottom panel takes remaining space
                    ])
                    .split(area);

                // center the screen horizontally by adding left/right empty space
                let top_chunks = Layout::default()
                    .direction(Direction::Horizontal)
                    .constraints([
                        Constraint::Percentage(
                            50 - ((VIDEO_WIDTH as u16 + 2) * 100 / area.width) / 2,
                        ), // left padding
                        Constraint::Length(VIDEO_WIDTH as u16 + 2), // video
                        Constraint::Percentage(
                            50 - ((VIDEO_WIDTH as u16 + 2) * 100 / area.width) / 2,
                        ), // right padding
                    ])
                    .split(vertical_chunks[0]);

                // bottom row: horizontal split for the remaining panels
                let bottom_chunks = Layout::default()
                    .direction(Direction::Horizontal)
                    .constraints([
                        Constraint::Length(40), // disassembly
                        Constraint::Length(20), // stack
                        Constraint::Min(0),     // memory+registers
                        Constraint::Length(25), // info+help
                    ])
                    .split(vertical_chunks[1]);

                // render widgets
                f.render_widget(self.render_video(), top_chunks[1]);
                f.render_widget(self.render_disassembly(bottom_chunks[0]), bottom_chunks[0]);
                f.render_widget(self.render_stack(bottom_chunks[1]), bottom_chunks[1]);
                f.render_widget(self.render_memory_registers(), bottom_chunks[2]);
                f.render_widget(self.render_info(&info_lines), bottom_chunks[3]);
            })?;

            if event::poll(std::time::Duration::from_millis(self.speed))? {
                if let Event::Key(key) = event::read()? {
                    match key.code {
                        KeyCode::Char('q') => break, // quit debugger
                        KeyCode::Char('s') => {
                            // single step
                            if !halted && !running {
                                match self.vm.exec_instruction() {
                                    Ok(true) => {
                                        info_lines.push("program halted".into());
                                        halted = true;
                                    }
                                    Ok(false) => {}
                                    Err(e) => info_lines.push(format!("runtime error: {}", e)),
                                }
                            }
                        }
                        KeyCode::Char('c') => running = true, // start continuous execution
                        KeyCode::Char('x') => running = false, // stop continuous execution
                        KeyCode::Char('+') => {
                            // scroll memory down
                            if self.mem_offset + 0x80 < self.vm.memory.len() {
                                self.mem_offset += 0x80;
                            }
                        }
                        KeyCode::Char('-') => {
                            // scroll memory up
                            if self.mem_offset >= 0x80 {
                                self.mem_offset -= 0x80;
                            } else {
                                self.mem_offset = 0;
                            }
                        }
                        KeyCode::Char('>') => {
                            if self.speed > 10 {
                                // minimum 10ms
                                self.speed -= 10;
                            }
                        }
                        KeyCode::Char('<') => {
                            if self.speed < 2000 {
                                // max 2s
                                self.speed += 10;
                            }
                        }
                        _ => {}
                    }
                }
            }

            if running && !halted {
                match self.vm.exec_instruction() {
                    Ok(true) => {
                        info_lines.push("program halted".into());
                        halted = true;
                        running = false;
                    }
                    Ok(false) => {}
                    Err(e) => {
                        info_lines.push(format!("runtime error: {}", e));
                        running = false;
                    }
                }
            }
        }

        disable_raw_mode()?;
        execute!(stdout(), LeaveAlternateScreen, Show)?;
        Ok(())
    }

    fn render_video(&self) -> Paragraph<'_> {
        let inner_width = VIDEO_WIDTH; // matches panel inner width
        let screen_text = (0..VIDEO_HEIGHT)
            .map(|y| {
                let mut line = String::with_capacity(inner_width);
                for x in 0..VIDEO_WIDTH {
                    let idx = VIDEO_BASE + y * VIDEO_WIDTH + x;
                    let ch = if idx < self.vm.memory.len() {
                        let byte = self.vm.memory[idx];
                        if byte.is_ascii_graphic() || byte == b' ' {
                            byte as char
                        } else {
                            ' '
                        }
                    } else {
                        ' '
                    };
                    line.push(ch);
                }
                line
            })
            .collect::<Vec<_>>()
            .join("");

        Paragraph::new(Text::from(screen_text))
            .block(Block::default().title("VIDEO").borders(Borders::ALL))
            .wrap(Wrap { trim: false })
    }

    fn render_disassembly(&mut self, area: ratatui::layout::Rect) -> Paragraph<'_> {
        let max_lines = area.height.saturating_sub(2);

        // find 2 instructions before ip for context
        let mut context_addrs = Vec::new();
        let mut addr = 0;
        let mut collected = 0;

        // simple scan to find addresses of 2 instructions before IP
        while addr < self.vm.memory.len() && collected < 2 {
            let (_, size) = self.disasm_at(addr);
            if addr + size > self.vm.ip as usize {
                break;
            }
            context_addrs.push(addr);
            addr += size;
            if addr >= self.vm.ip as usize {
                collected += 1;
            }
        }

        // start addr is either the last 2 instructions before IP or 0
        let start_addr = if context_addrs.len() >= 2 {
            context_addrs[context_addrs.len() - 2]
        } else if !context_addrs.is_empty() {
            context_addrs[0]
        } else {
            self.vm.ip as usize
        };

        let mut lines: Vec<Line> = Vec::new();
        let mut addr = start_addr;

        while lines.len() < max_lines as usize && addr < self.vm.memory.len() {
            let (instr, size) = self.disasm_at(addr);
            let is_current = addr == self.vm.ip as usize;

            let style = if is_current {
                Style::default()
                    .fg(Color::Black)
                    .bg(Color::Yellow)
                    .add_modifier(Modifier::BOLD)
            } else {
                Style::default().fg(Color::White)
            };

            let marker = if is_current { ">" } else { " " };

            let line = Line::from(vec![Span::styled(
                format!("{} 0x{:04X}: {}", marker, addr, instr),
                style,
            )]);

            lines.push(line);
            addr += size;
        }

        Paragraph::new(Text::from(lines))
            .block(Block::default().title("DISASSEMBLY").borders(Borders::ALL))
            .wrap(Wrap { trim: false })
    }

    fn render_stack(&self, area: ratatui::layout::Rect) -> Paragraph<'_> {
        let mem = &self.vm.memory;
        let sp = self.vm.registers.get_sp() as usize;
        let fp = self.vm.registers.get_fp() as usize;

        // collect stack entries from sp upward
        let mut stack_entries = Vec::new();
        let mut addr = sp;
        while addr + 1 < mem.len() {
            let val = u16::from_le_bytes([mem[addr], mem[addr + 1]]);
            stack_entries.push((addr, val));
            addr += 2;
        }

        // only keep as many as fit in panel
        let max_lines = area.height as usize;
        let start = if stack_entries.len() > max_lines {
            stack_entries.len() - max_lines
        } else {
            0
        };

        let mut lines: Vec<Line> = Vec::new();

        for &(addr, val) in &stack_entries[start..] {
            // determine row style
            let row_style = if addr == sp {
                Style::default()
                    .bg(Color::Cyan)
                    .fg(Color::Black)
                    .add_modifier(Modifier::BOLD)
            } else if addr == fp {
                Style::default()
                    .bg(Color::Magenta)
                    .fg(Color::Black)
                    .add_modifier(Modifier::BOLD)
            } else {
                Style::default().fg(Color::White)
            };

            // build the line
            let spans = vec![
                Span::raw(" "), // left padding
                Span::styled(format!("[0x{:04X}] = {:04X}", addr, val), row_style),
                Span::raw(" "), // right padding
            ];

            lines.push(Line::from(spans));
        }

        // reverse so top of stack appears at bottom of panel
        lines.reverse();

        Paragraph::new(Text::from(lines))
            .block(Block::default().title("STACK").borders(Borders::ALL))
            .wrap(Wrap { trim: false })
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
            0x14 => {
                size = 3;
                let reg = self.vm.memory[addr + 1];
                let offset = self.vm.memory[addr + 2] as i8;
                format!("LOADR [{}, {}]", Vm::reg_name(reg), offset)
            }
            0x15 => {
                size = 3;
                let reg = self.vm.memory[addr + 1];
                let offset = self.vm.memory[addr + 2] as i8;
                format!("STORER [{}, {}]", Vm::reg_name(reg), offset)
            }
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
                size = 1;
                "RET".to_string()
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
                size = 1;
                let byte = self.vm.memory[addr + 1];
                let rd = byte >> 4;
                let rs = byte & 0xF;
                format!("MOV {} {}", Vm::reg_name(rd), Vm::reg_name(rs))
            }
            0x51 => {
                size = 2;
                format!("PUSHR {}", Vm::reg_name(self.vm.memory[addr + 1]))
            }
            0x52 => {
                size = 2;
                format!("POPR {}", Vm::reg_name(self.vm.memory[addr + 1]))
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

    fn render_memory_registers(&mut self) -> Paragraph<'_> {
        let mut lines: Vec<Line> = Vec::new();

        // registers
        let r = |i| self.vm.registers.get_reg(i).unwrap_or(0);
        let sp = self.vm.registers.get_sp();
        let fp = self.vm.registers.get_fp();
        let ip = self.vm.ip;
        let f = &self.vm.flags;

        // line 1: r0-r3
        lines.push(Line::from(vec![
            Span::raw(format!("r0={:04X}  ", r(0))),
            Span::raw(format!("r1={:04X}  ", r(1))),
            Span::raw(format!("r2={:04X}  ", r(2))),
            Span::raw(format!("r3={:04X}", r(3))),
        ]));

        // line 2: SP, FP, IP, flags
        lines.push(Line::from(vec![
            Span::raw("SP="),
            Span::styled(format!("{:04X}  ", sp), Style::default().fg(Color::Cyan)),
            Span::raw("FP="),
            Span::styled(format!("{:04X}  ", fp), Style::default().fg(Color::Magenta)),
            Span::raw("IP="),
            Span::styled(
                format!("{:04X}  ", ip),
                Style::default()
                    .fg(Color::Yellow)
                    .add_modifier(Modifier::BOLD),
            ),
            Span::raw("Z="),
            Span::styled(
                if f.z { "1  " } else { "0  " },
                Style::default().fg(if f.z { Color::Green } else { Color::Red }),
            ),
            Span::raw("N="),
            Span::styled(
                if f.n { "1  " } else { "0  " },
                Style::default().fg(if f.n { Color::Green } else { Color::Red }),
            ),
            Span::raw("V="),
            Span::styled(
                if f.v { "1" } else { "0" },
                Style::default().fg(if f.v { Color::Green } else { Color::Red }),
            ),
        ]));

        lines.push(Line::from("")); // empty line

        // get instruction range
        let (_, instr_size) = self.disasm_at(ip as usize);
        let instr_start = ip as usize;
        let instr_end = instr_start + instr_size;

        // memory hex dump
        let mem = &self.vm.memory;
        let rows = 16;
        for r in 0..rows {
            let addr = self.mem_offset + r * 16;
            if addr >= mem.len() {
                break;
            }

            let mut line_spans: Vec<Span> = Vec::new();
            line_spans.push(Span::raw(format!("{:04X}: ", addr)));

            // hex bytes
            for i in 0..16 {
                let byte_addr = addr + i;
                if byte_addr >= mem.len() {
                    break;
                }

                let style = if byte_addr >= instr_start && byte_addr < instr_end {
                    Style::default()
                        .fg(Color::Black)
                        .bg(Color::Yellow)
                        .add_modifier(Modifier::BOLD)
                } else {
                    Style::default().fg(Color::White)
                };

                line_spans.push(Span::styled(format!("{:02X} ", mem[byte_addr]), style));
            }

            line_spans.push(Span::raw("  "));

            // ascii
            for i in 0..16 {
                let byte_addr = addr + i;
                if byte_addr >= mem.len() {
                    break;
                }

                let ch = if mem[byte_addr].is_ascii_graphic() || mem[byte_addr] == b' ' {
                    mem[byte_addr] as char
                } else {
                    '.'
                };

                let style = if byte_addr >= instr_start && byte_addr < instr_end {
                    Style::default()
                        .fg(Color::Black)
                        .bg(Color::Yellow)
                        .add_modifier(Modifier::BOLD)
                } else {
                    Style::default()
                };

                line_spans.push(Span::styled(ch.to_string(), style));
            }

            lines.push(Line::from(line_spans));
        }

        Paragraph::new(Text::from(lines))
            .block(
                Block::default()
                    .title("MEMORY + REGISTERS")
                    .borders(Borders::ALL),
            )
            .wrap(Wrap { trim: false })
    }

    fn render_info(&self, info_lines: &Vec<String>) -> Paragraph<'_> {
        let shortcuts = vec![
            String::new(),
            "q - quit".into(),
            "s - step".into(),
            "c - continue".into(),
            "x - stop".into(),
            "+ - scroll mem down".into(),
            "- - scroll mem up".into(),
            "> - faster".into(),
            "< - slower".into(),
        ];

        // display last 15 info messages
        let mut display_lines: Vec<Line> = info_lines
            .iter()
            .rev()
            .take(15)
            .rev()
            .map(|msg| Line::from(Span::raw(msg.clone())))
            .collect();

        // add shortcuts
        for sc in shortcuts {
            display_lines.push(Line::from(Span::styled(
                sc,
                Style::default().fg(Color::Blue),
            )));
        }

        // show current speed at the top
        display_lines.insert(
            0,
            Line::from(Span::styled(
                format!("SPEED: {}ms", self.speed),
                Style::default()
                    .fg(Color::Yellow)
                    .add_modifier(Modifier::BOLD),
            )),
        );

        Paragraph::new(Text::from(display_lines))
            .block(
                Block::default()
                    .title("INFO / MESSAGES")
                    .borders(Borders::ALL),
            )
            .wrap(Wrap { trim: false })
    }
}
