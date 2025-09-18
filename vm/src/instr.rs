#[repr(u8)]
pub enum Instr {
    Push16 = 0x01,
    Halt = 0x0F,
    Load8 = 0x10,
    Store8 = 0x11,
    Loadp = 0x12,
    Storep = 0x13,
    Loadr = 0x14,
    Storer = 0x15,
    Jmp8 = 0x20,
    Jlt8 = 0x21,
    Jgt8 = 0x22,
    Jeq8 = 0x23,
    Call = 0x24,
    Ret = 0x25,
    Add = 0x30,
    Sub = 0x31,
    Mul = 0x32,
    Div = 0x33,
    Neg = 0x34,
    Cmp = 0x35,
    Not = 0x40,
    And = 0x41,
    Or = 0x42,
    Xor = 0x43,
    Shft = 0x44,
    Mov = 0x50,
    Pushr = 0x51,
    Popr = 0x52,
    Print = 0x60,
    MvCur = 0x61,
}

impl Instr {
    pub fn from_u8(byte: u8) -> Option<Self> {
        match byte {
            0x01 => Some(Instr::Push16),
            0x0F => Some(Instr::Halt),
            0x10 => Some(Instr::Load8),
            0x11 => Some(Instr::Store8),
            0x12 => Some(Instr::Loadp),
            0x13 => Some(Instr::Storep),
            0x14 => Some(Instr::Loadr),
            0x15 => Some(Instr::Storer),
            0x20 => Some(Instr::Jmp8),
            0x21 => Some(Instr::Jlt8),
            0x22 => Some(Instr::Jgt8),
            0x23 => Some(Instr::Jeq8),
            0x24 => Some(Instr::Call),
            0x25 => Some(Instr::Ret),
            0x30 => Some(Instr::Add),
            0x31 => Some(Instr::Sub),
            0x32 => Some(Instr::Mul),
            0x33 => Some(Instr::Div),
            0x34 => Some(Instr::Neg),
            0x35 => Some(Instr::Cmp),
            0x40 => Some(Instr::Not),
            0x41 => Some(Instr::And),
            0x42 => Some(Instr::Or),
            0x43 => Some(Instr::Xor),
            0x44 => Some(Instr::Shft),
            0x50 => Some(Instr::Mov),
            0x51 => Some(Instr::Pushr),
            0x52 => Some(Instr::Popr),
            0x60 => Some(Instr::Print),
            0x61 => Some(Instr::MvCur),
            _ => None,
        }
    }

    pub fn byte_len(&self) -> usize {
        match self {
            Instr::Push16 => 3,
            Instr::Load8 => 2,
            Instr::Store8 => 2,
            Instr::Jmp8 => 2,
            Instr::Jlt8 => 2,
            Instr::Jgt8 => 2,
            Instr::Jeq8 => 2,
            Instr::Mov => 2,
            Instr::Pushr => 2,
            Instr::Popr => 2,
            Instr::Add
            | Instr::Sub
            | Instr::Mul
            | Instr::Div
            | Instr::Neg
            | Instr::Not
            | Instr::Cmp
            | Instr::Halt => 1,
            Instr::And => 1,
            Instr::Or => 1,
            Instr::Shft => 1,
            Instr::Xor => 1,
            Instr::Loadp => 1,
            Instr::Storep => 1,
            Instr::Call => 3,
            Instr::Ret => 1,
            Instr::Print => 1,
            Instr::MvCur => 2,
            Instr::Loadr => 3,
            Instr::Storer => 3,
        }
    }
}
