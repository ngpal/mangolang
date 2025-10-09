#[repr(u8)]
pub enum Instr {
    Push16 = 0x01,
    Halt = 0x0F,

    Ldw = 0x12,
    Stw = 0x13,

    Ldb = 0x16,
    Stb = 0x17,

    Jmp8 = 0x20,
    Jlt8 = 0x21,
    Jgt8 = 0x22,
    Jeq8 = 0x23,
    Call = 0x24,
    Ret = 0x25,

    Add = 0x30,
    Cmp = 0x35,

    Not = 0x40,
    And = 0x41,
    Or = 0x42,
    Xor = 0x43,
    Shft = 0x44,
    Mov = 0x50,
    Pushr = 0x51,
    Popr = 0x52,

    Int = 0x70,
}

impl Instr {
    pub fn from_u8(byte: u8) -> Option<Self> {
        match byte {
            0x01 => Some(Instr::Push16),
            0x0F => Some(Instr::Halt),
            0x12 => Some(Instr::Ldw),
            0x13 => Some(Instr::Stw),
            0x16 => Some(Instr::Ldb),
            0x17 => Some(Instr::Stb),
            0x20 => Some(Instr::Jmp8),
            0x21 => Some(Instr::Jlt8),
            0x22 => Some(Instr::Jgt8),
            0x23 => Some(Instr::Jeq8),
            0x24 => Some(Instr::Call),
            0x25 => Some(Instr::Ret),
            0x30 => Some(Instr::Add),
            0x35 => Some(Instr::Cmp),
            0x40 => Some(Instr::Not),
            0x41 => Some(Instr::And),
            0x42 => Some(Instr::Or),
            0x43 => Some(Instr::Xor),
            0x44 => Some(Instr::Shft),
            0x50 => Some(Instr::Mov),
            0x51 => Some(Instr::Pushr),
            0x52 => Some(Instr::Popr),
            0x70 => Some(Instr::Int),
            _ => None,
        }
    }

    pub fn byte_len(&self) -> usize {
        match self {
            Instr::Push16 => 3,
            Instr::Jmp8 => 2,
            Instr::Jlt8 => 2,
            Instr::Jgt8 => 2,
            Instr::Jeq8 => 2,
            Instr::Mov => 2,
            Instr::Pushr => 2,
            Instr::Popr => 2,
            Instr::Add | Instr::Not | Instr::Cmp | Instr::Halt => 1,
            Instr::And => 1,
            Instr::Or => 1,
            Instr::Shft => 1,
            Instr::Xor => 1,
            Instr::Ldw => 1,
            Instr::Stw => 1,
            Instr::Ldb => 1,
            Instr::Stb => 1,
            Instr::Call => 3,
            Instr::Ret => 1,
            Instr::Int => 2,
        }
    }
}
