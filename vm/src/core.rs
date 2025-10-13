use std::{
    io::{Write, stdout},
    time::Duration,
};

use crossterm::{
    cursor::{self, MoveTo},
    event::{self, Event, KeyCode},
    execute,
    terminal::{self, Clear, ClearType, size},
};

use crate::{
    disk::DiskDriver,
    error::{RuntimeError, RuntimeResult},
    instr::Instr,
    video::{VIDEO_HEIGHT, VIDEO_WIDTH, Video},
};

// const MBIN_MAGIC: &[u8; 4] = b"MBIN";
const STACK_START: usize = 0xFF00;
// const HEAP_START: usize = 0x4A10;
const MMIO_START: usize = 0x4910;
const USER_CODE_START: usize = 0x0910;
// const USER_CODE_SIZE: usize = MMIO_START - USER_CODE_START;
// const INT_HANDLER_START: usize = 0x0110;
const IVT_START: usize = 0x0100;
// const ROM_START: usize = 0x0000;
pub const MEM_SIZE: usize = 0x10000;

pub const MMIO_PRINT: usize = 0x4910;

pub const MMIO_DSK_SEC: usize = 0x4920;
pub const MMIO_DSK_ADR: usize = 0x4922;
pub const MMIO_DSK_CMD: usize = 0x4924;
pub const MMIO_DSK_STS: usize = 0x4925;

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
    pub disk: DiskDriver,
    pub video: Video,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            memory: [0; MEM_SIZE],
            flags: Flags::default(),
            registers: Registers::new(),
            ip: 0,
            disk: DiskDriver::new(),
            video: Video::new(),
        }
    }

    pub fn load(&mut self) -> RuntimeResult<()> {
        // load BIOS
        for (addr, byte) in self.disk.get_sector(0x0000).iter().enumerate() {
            self.memory[addr] = *byte;
        }

        // enter kernel mode
        self.flags.k = true;
        // set disk status ready
        self.memory[MMIO_DSK_STS] = 0;

        Ok(())
    }

    pub fn read_word(&self, addr: u16) -> u16 {
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

    pub fn read_byte(&self, addr: u16) -> u8 {
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
        let offset = self.fetch_word() as i16 as isize;
        let base = self.ip as isize;
        if cond {
            let new_ip = base.wrapping_add(offset);

            if !self.flags.k
                && ((new_ip as usize) < USER_CODE_START || (new_ip as usize) > MMIO_START)
            {
                return Err(RuntimeError(format!("invalid jump target {}", new_ip)));
            }

            self.ip = new_ip as u16;
        }

        Ok(false)
    }

    // Return halt signal
    pub fn exec_instruction(&mut self) -> RuntimeResult<bool> {
        let byte = self.fetch_byte();
        let instr = Instr::from_u8(byte)
            .ok_or(RuntimeError(format!("unknown instruction: 0x{:2X}", byte)))?;

        match instr {
            Instr::Push16 => {
                let word = self.fetch_word();
                self.push_word(word)?;
            }
            Instr::Halt => return Ok(true),
            Instr::Add
            | Instr::Cmp
            | Instr::And
            | Instr::Or
            | Instr::Xor
            | Instr::Shl
            | Instr::Shr => {
                let right = self.pop_word()?;
                let left = self.pop_word()?;

                let (result, overflow) = match instr {
                    Instr::Add => left.overflowing_add(right),
                    Instr::Cmp => left.overflowing_sub(right),
                    Instr::And => (left & right, false),
                    Instr::Or => (left | right, false),
                    Instr::Xor => (left ^ right, false),
                    Instr::Shl => left.overflowing_shl(right as u32),
                    Instr::Shr => left.overflowing_shr(right as u32),
                    _ => unreachable!(),
                };

                if !(matches!(instr, Instr::Cmp)) {
                    self.push_word(result)?;
                }

                self.set_flags(result, overflow);
            }
            Instr::Jmp => return self.jump_rel(true),
            Instr::Jlt => return self.jump_rel(self.flags.n),
            Instr::Jgt => return self.jump_rel(!self.flags.z && !self.flags.n),
            Instr::Jeq => return self.jump_rel(self.flags.z),
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
                if !self.flags.k
                    && ((addr as usize) < USER_CODE_START || (addr as usize) > MMIO_START)
                {
                    return Err(RuntimeError(
                        "attempt to jump outside of user code in user mode".to_string(),
                    ));
                }
                self.push_word(self.ip)?;
                self.ip = addr;
            }
            Instr::Ret => {
                let addr = self.pop_word()?;
                self.ip = addr;
            }
            Instr::Int => {
                let int = self.fetch_byte();
                self.exec_interrupt(int)?;
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
            // do nothing
            Instr::Bkpt => {}
        }

        self.update_terminal();
        self.update_disk()?;
        Ok(false)
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

    pub fn update_terminal(&mut self) {
        if self.memory[MMIO_PRINT] != 0 {
            self.video.put_char(self.memory[MMIO_PRINT]);
            self.memory[MMIO_PRINT] = 0
        }
    }

    pub fn update_disk(&mut self) -> RuntimeResult<()> {
        // only read supported rn
        if self.memory[MMIO_DSK_STS] == 0 && self.memory[MMIO_DSK_CMD] == 1 {
            self.memory[MMIO_DSK_STS] = 1;
            self.memory[MMIO_DSK_CMD] = 0;

            let sec = self.disk.get_sector(self.read_word(MMIO_DSK_SEC as u16));
            let addr = self.read_word(MMIO_DSK_ADR as u16);

            for (i, byte) in sec.iter().enumerate() {
                self.write_byte(addr + i as u16, *byte)?;
            }

            self.memory[MMIO_DSK_STS] = 0;
        }

        Ok(())
    }
    fn exec_interrupt(&mut self, int: u8) -> RuntimeResult<()> {
        match int {
            // print to video
            0..=7 => {
                let addr = self.read_word(IVT_START as u16 + int as u16 * 2);
                self.push_word(self.ip)?;
                self.ip = addr;
            }
            _ => return Err(RuntimeError(format!("unkown interrupt {}", int))),
        }

        // hack for now
        self.flags.k = false;
        Ok(())
    }

    pub fn run(&mut self) -> RuntimeResult<()> {
        fn wrap_err<E: std::fmt::Display>(err: E) -> RuntimeError {
            RuntimeError(err.to_string())
        }

        let mut running = true;
        let mut stdout = stdout();
        terminal::enable_raw_mode().map_err(wrap_err)?;
        execute!(stdout, terminal::EnterAlternateScreen, cursor::Hide).map_err(wrap_err)?;

        // draw static UI once
        self.draw_static_ui().map_err(wrap_err)?;

        let mut error_msg: Option<String> = None;

        loop {
            // quit check
            if event::poll(Duration::from_millis(0)).map_err(wrap_err)? {
                if let Event::Key(key) = event::read().map_err(wrap_err)? {
                    if let KeyCode::Char('q') = key.code {
                        break;
                    }
                }
            }

            // execute one instruction (may mark video as dirty)
            if running {
                match self.exec_instruction() {
                    Err(e) => {
                        running = false;
                        error_msg = Some(format!("Error: {}", e))
                    }
                    Ok(true) => {
                        running = false;
                        error_msg = Some("Machine halted".to_string())
                    }
                    _ => {}
                }
            }

            self.draw_footer(error_msg.as_deref()).map_err(wrap_err)?;

            // only redraw if needed
            if self.video.is_dirty() {
                self.update_video().map_err(wrap_err)?;
                self.video.reset_dirty();
            }
        }

        execute!(stdout, cursor::Show, terminal::LeaveAlternateScreen).map_err(wrap_err)?;
        terminal::disable_raw_mode().map_err(wrap_err)?;
        Ok(())
    }

    fn draw_static_ui(&self) -> std::io::Result<()> {
        let mut out = stdout();
        let (term_width, term_height) = size()?;
        let frame_width = VIDEO_WIDTH + 2;
        let frame_height = VIDEO_HEIGHT + 3;

        let x_offset = (term_width as usize - frame_width).max(0) / 2;
        let y_offset = (term_height as usize - frame_height).max(0) / 2;

        execute!(out, Clear(ClearType::All))?;

        let title = " VM VIDEO OUTPUT ";
        let title_start = x_offset + (frame_width.saturating_sub(title.len())) / 2;
        execute!(out, MoveTo(title_start as u16, y_offset as u16))?;
        writeln!(out, "{}", title)?;

        let top = format!("┌{}┐", "─".repeat(VIDEO_WIDTH));
        execute!(out, MoveTo(x_offset as u16, (y_offset + 1) as u16))?;
        writeln!(out, "{}", top)?;

        for row in 0..VIDEO_HEIGHT {
            execute!(out, MoveTo(x_offset as u16, (y_offset + 2 + row) as u16))?;
            writeln!(out, "│{}│", " ".repeat(VIDEO_WIDTH))?;
        }

        let bottom = format!("└{}┘", "─".repeat(VIDEO_WIDTH));
        execute!(
            out,
            MoveTo(x_offset as u16, (y_offset + 2 + VIDEO_HEIGHT) as u16)
        )?;
        writeln!(out, "{}", bottom)?;
        out.flush()?;
        Ok(())
    }

    fn draw_footer(&self, error: Option<&str>) -> std::io::Result<()> {
        let mut out = stdout();
        let (term_width, term_height) = size()?;

        let frame_width = VIDEO_WIDTH + 2;
        let frame_height = VIDEO_HEIGHT + 3;
        let x_offset = (term_width as usize - frame_width).max(0) / 2;
        let y_offset = (term_height as usize - frame_height).max(0) / 2;

        // footer position: below the video frame
        let footer_y = y_offset + frame_height;
        execute!(out, MoveTo(x_offset as u16, footer_y as u16))?;
        write!(out, "Press 'q' to quit")?;

        if let Some(msg) = error {
            execute!(out, MoveTo(x_offset as u16, (footer_y + 1) as u16))?;
            write!(out, "{}", msg)?;
        }

        out.flush()?;
        Ok(())
    }

    fn update_video(&self) -> std::io::Result<()> {
        let mut out = stdout();
        let (term_width, term_height) = size()?;

        let frame_width = VIDEO_WIDTH + 2;
        let frame_height = VIDEO_HEIGHT + 3;

        let x_offset = (term_width as usize - frame_width).max(0) / 2;
        let y_offset = (term_height as usize - frame_height).max(0) / 2;

        for row in 0..VIDEO_HEIGHT {
            execute!(
                out,
                MoveTo((x_offset + 1) as u16, (y_offset + 2 + row) as u16)
            )?;
            for col in 0..VIDEO_WIDTH {
                let ch = self.video.get_char(row * VIDEO_WIDTH + col);
                let ch = if ch == 0 { ' ' } else { ch as char };
                write!(out, "{}", ch)?;
            }
        }

        out.flush()?;
        Ok(())
    }
}
