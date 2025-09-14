use crossterm::{
    cursor::{Hide, MoveTo, Show},
    event::{self, Event, KeyCode},
    execute,
    style::Print,
    terminal::{self, Clear, ClearType, EnterAlternateScreen, LeaveAlternateScreen},
};
use std::io::{self, Write, stdout};

use crate::core::{INFO_HEIGHT, VIDEO_BASE, VIDEO_HEIGHT, VIDEO_WIDTH, Vm};

pub struct VideoMemory;

impl VideoMemory {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run(&mut self, vm: &mut Vm) -> io::Result<()> {
        let mut stdout = stdout();

        // setup terminal
        execute!(stdout, EnterAlternateScreen, Hide)?;
        terminal::enable_raw_mode()?;

        let mut info = vec!["hit enter to quit".to_string()];
        let mut halted = false;
        let result = loop {
            // run some vm steps
            if !halted {
                match vm.exec_instruction() {
                    Ok(true) => {
                        info.push("machine has halted".into());
                        halted = true;
                    }
                    Ok(false) => {}
                    Err(e) => {
                        eprintln!("runtime error: {e}");
                        break Err(io::Error::other("vm error"));
                    }
                }
            }

            // always show quit message
            self.render(vm, &info)?;

            // poll keyboard
            if event::poll(std::time::Duration::from_millis(50))?
                && let Event::Key(key) = event::read()?
            {
                if key.code == KeyCode::Enter {
                    break Ok(());
                }
            }
        };

        // cleanup terminal
        terminal::disable_raw_mode()?;
        execute!(stdout, Show, LeaveAlternateScreen)?;

        result
    }

    pub fn render(&self, vm: &Vm, info_lines: &[String]) -> io::Result<()> {
        let mut stdout = stdout();
        execute!(stdout, MoveTo(0, 0), Clear(ClearType::All))?;

        // top border
        execute!(stdout, Print("┏"))?;
        for _ in 0..VIDEO_WIDTH {
            execute!(stdout, Print("━"))?;
        }
        execute!(stdout, Print("┓\r\n"))?;

        // video area
        for y in 0..VIDEO_HEIGHT {
            execute!(stdout, Print("┃"))?;
            for x in 0..VIDEO_WIDTH {
                let ch = vm.memory[VIDEO_BASE + y * VIDEO_WIDTH + x];
                let ch = if ch == 0 { b' ' } else { ch };
                execute!(stdout, Print(ch as char))?;
            }
            execute!(stdout, Print("┃\r\n"))?;
        }

        // bottom border
        execute!(stdout, Print("┗"))?;
        for _ in 0..VIDEO_WIDTH {
            execute!(stdout, Print("━"))?;
        }
        execute!(stdout, Print("┛\r\n"))?;

        // info lines
        for line in info_lines.iter().take(INFO_HEIGHT) {
            execute!(stdout, Print(line), Print("\r\n"))?;
        }

        // hex dump of video memory
        execute!(stdout, Print("\r\nvideo memory hex dump:\r\n"))?;
        for y in 0..VIDEO_HEIGHT {
            // print address
            execute!(
                stdout,
                Print(format!("{:04x}: ", VIDEO_BASE + y * VIDEO_WIDTH))
            )?;

            // print hex bytes
            for x in 0..VIDEO_WIDTH {
                let byte = vm.memory[VIDEO_BASE + y * VIDEO_WIDTH + x];
                execute!(stdout, Print(format!("{:02X} ", byte)))?;
            }

            // optional ascii representation at the end
            execute!(stdout, Print("  "))?;
            for x in 0..VIDEO_WIDTH {
                let byte = vm.memory[VIDEO_BASE + y * VIDEO_WIDTH + x];
                let ch = if byte.is_ascii_graphic() || byte == b' ' {
                    byte as char
                } else {
                    '.'
                };
                execute!(stdout, Print(ch))?;
            }

            execute!(stdout, Print("\r\n"))?;
        }

        stdout.flush()?;
        Ok(())
    }
}
