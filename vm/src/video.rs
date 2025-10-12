pub const VIDEO_WIDTH: usize = 40;
pub const VIDEO_HEIGHT: usize = 12;

pub struct Video {
    mem: [u8; VIDEO_HEIGHT * VIDEO_WIDTH],
    cursor: usize,
    dirty: bool,
}

impl Video {
    pub fn new() -> Video {
        Video {
            mem: [0; VIDEO_HEIGHT * VIDEO_WIDTH],
            cursor: 0,
            dirty: true,
        }
    }

    pub fn get_char(&self, idx: usize) -> u8 {
        self.mem[idx]
    }

    pub fn get_cur(&self) -> usize {
        self.cursor
    }

    pub fn set_cur(&mut self, val: usize) {
        self.cursor = val % (VIDEO_HEIGHT * VIDEO_WIDTH)
    }

    pub fn inc_cur(&mut self) {
        self.set_cur(self.get_cur() + 1);
    }

    pub fn put_char(&mut self, ch: u8) {
        if ch == b'\n' {
            self.set_cur(self.get_cur() + VIDEO_WIDTH);
            self.inc_cur();
        } else if ch == b'\r' {
            self.set_cur((self.get_cur() / VIDEO_WIDTH) * VIDEO_WIDTH);
        } else {
            self.mem[self.get_cur()] = ch;
            self.inc_cur();
        }

        self.dirty = true;
    }

    pub fn reset_dirty(&mut self) {
        self.dirty = false;
    }

    pub fn is_dirty(&self) -> bool {
        self.dirty
    }
}
