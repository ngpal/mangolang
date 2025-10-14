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

    Jmp(i16),
    Jlt(i16),
    Jgt(i16),
    Jeq(i16),

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
    Shl,
    Shr,

    CmpI(u16),
    AddI(u16),
    SubI(u16),
    MulI(u16),
    DivI(u16),
    NegI(u16),
    ModI(u16),

    NotI(u16),

    AndI(u16),
    OrI(u16),
    XorI(u16),
    ShlI(u16),
    ShrI(u16),

    Mov(u8, u8),
    Pushr(u8),
    Popr(u8),

    Int(u8),
    Iret,
    Bkpt,

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
            Instr::Int(_) => 2,
            Instr::Jmp(_) => 3,
            Instr::Jlt(_) => 3,
            Instr::Jgt(_) => 3,
            Instr::Jeq(_) => 3,
            Instr::Mov(_, _) => 2,
            Instr::Pushr(_) => 2,
            Instr::Popr(_) => 2,
            Instr::Add | Instr::Not | Instr::Halt | Instr::Cmp => 1,
            Instr::Lbl(_) => 0,
            Instr::JmpLbl(_) => 3,
            Instr::JltLbl(_) => 3,
            Instr::JgtLbl(_) => 3,
            Instr::JeqLbl(_) => 3,
            Instr::CallLbl(_) => 3,
            Instr::And => 1,
            Instr::Or => 1,
            Instr::Shl | Instr::Shr => 1,
            Instr::Xor => 1,
            Instr::Call(_) => 3,
            Instr::Ret => 1,
            Instr::Ldw => 1,
            Instr::Stw => 1,
            Instr::Ldb => 1,
            Instr::Stb => 1,
            Instr::Iret => 1,
            Instr::Bkpt => 1,
            Instr::CmpI(_)
            | Instr::AddI(_)
            | Instr::SubI(_)
            | Instr::MulI(_)
            | Instr::DivI(_)
            | Instr::NegI(_)
            | Instr::ModI(_)
            | Instr::NotI(_)
            | Instr::AndI(_)
            | Instr::OrI(_)
            | Instr::XorI(_)
            | Instr::ShlI(_)
            | Instr::ShrI(_)
            | Instr::Ldr(_, _)
            | Instr::Str(_, _)
            | Instr::Sub
            | Instr::Mul
            | Instr::Div
            | Instr::Neg
            | Instr::Mod => unreachable!("byte_len called on convenience instruction {:?}", self),
        }
    }
}
