#[derive(Clone, Debug)]
pub enum Instr {
    Halt,
    Push(u16),
    Ldw,
    Stw,
    Ldb,
    Stb,
    Ldr(u8, i8),
    Str(u8, i8),

    Jmp(i8),
    Jlt(i8),
    Jgt(i8),
    Jeq(i8),

    Call(u16),
    Ret,

    Cmp,
    Add,
    Sub,
    Mul,
    Div,
    Neg,
    Mod,

    Not,

    And,
    Or,
    Xor,
    Shft,

    Mov(u8, u8),
    Pushr(u8),
    Popr(u8),

    // pseudo instructions
    Lbl(String),
    CallLbl(String),
    JmpLbl(String),
    JltLbl(String),
    JgtLbl(String),
    JeqLbl(String),
    Data(String), // translates to PUSH16 addr
}

impl Instr {
    pub fn byte_len(&self) -> usize {
        match self {
            Instr::Push(_) | Instr::Data(_) => 3,
            Instr::Jmp(_) => 2,
            Instr::Jlt(_) => 2,
            Instr::Jgt(_) => 2,
            Instr::Jeq(_) => 2,
            Instr::Mov(_, _) => 2,
            Instr::Pushr(_) => 2,
            Instr::Popr(_) => 2,
            Instr::Add | Instr::Not | Instr::Halt | Instr::Cmp => 1,
            Instr::Lbl(_) => 0,
            Instr::JmpLbl(_) => 2,
            Instr::JltLbl(_) => 2,
            Instr::JgtLbl(_) => 2,
            Instr::JeqLbl(_) => 2,
            Instr::CallLbl(_) => 3,
            Instr::And => 1,
            Instr::Or => 1,
            Instr::Shft => 1,
            Instr::Xor => 1,
            Instr::Call(_) => 3,
            Instr::Ret => 1,
            Instr::Ldw => 1,
            Instr::Stw => 1,
            Instr::Ldb => 1,
            Instr::Stb => 1,
            Instr::Ldr(_, _)
            | Instr::Str(_, _)
            | Instr::Sub
            | Instr::Mul
            | Instr::Div
            | Instr::Neg
            | Instr::Mod => unreachable!("byte_len called on convenience instruction {:?}", self),
        }
    }
}
