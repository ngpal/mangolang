use chumsky::prelude::*;

#[derive(Clone, Debug)]
pub enum Mnemonic {
    // R-Type instructions
    Add,
    Sub,
    And,
    Or,
    Xor,
    Shl,
    Shr,
    Mov,

    // I-Type instructions
    AddI,
    SubI,
    AndI,
    OrI,
    XorI,
    ShlI,
    ShrI,
    MovI,

    // M-Type instructions
    Ldw,
    Stw,
    Ldb,
    Stb,

    // B-Type instructions
    Jmp,
    Jeq,
    Jlt,
    Jgt,

    // E-Type instructions
    JmpW,
    JeqW,
    JltW,
    JgtW,
    Call,

    // S-Type instructions
    Ret,
    NoOp,
    Halt,

    // Pseudo instructions for labels
    Lbl(String),
    CallLbl(String),
    JmpLbl(String),
    JltLbl(String),
    JgtLbl(String),
    JeqLbl(String),
    JmpWLbl(String),
    JltWLbl(String),
    JgtWLbl(String),
    JeqWLbl(String),
    Data(u8, String), // (rd, label) - load data address into register rd
}

pub enum Section {
    Text,
    Data,
}

#[derive(Debug, Clone)]
pub struct Assembly {
    pub text: Vec<Instr>,
    pub data: Vec<(String, String)>,
}

#[derive(Debug, Clone)]
pub enum Instr {
    R {
        mnemonic: String,
        rd: u8,
        rs: u8,
    },
    I {
        mnemonic: String,
        rd: u8,
        imm: i8,
    },
    M {
        mnemonic: String,
        rd: u8,
        rs: u8,
        imm: u8,
    },
    B {
        mnemonic: String,
        cond: u8,
        imm: u8,
    },
    E {
        mnemonic: String,
        imm: u16,
    },
    S {
        mnemonic: String,
    },
    Pseudo {
        mnemonic: String,
        args: Vec<String>,
    },
}

fn register<'a>() -> impl Parser<'a, &'a str, u8> {
    text::ident().map(|s: &str| match s.to_ascii_uppercase().as_str() {
        "R0" => 0,
        "R1" => 1,
        "R2" => 2,
        "R3" => 3,
        "R4" => 4,
        "R5" => 5,
        "SP" | "R6" => 6,
        "FP" | "R7" => 7,
        _ => panic!("invalid register: {}", s),
    })
}

fn imm8<'a>() -> impl Parser<'a, &'a str, i8> {
    // hex: starts with 0x
    let hex = just("0x")
        .ignore_then(text::int(16))
        .map(|s: &str| i8::from_str_radix(s, 16).unwrap());

    // decimal
    let dec = text::int(10).map(|s: &str| s.parse::<i8>().unwrap());

    hex.or(dec)
}

fn imm16<'a>() -> impl Parser<'a, &'a str, u16> {
    let hex = just("0x")
        .ignore_then(text::int(16))
        .map(|s: &str| u16::from_str_radix(s, 16).unwrap());

    let dec = text::int(10).map(|s: &str| s.parse::<u16>().unwrap());

    hex.or(dec)
}

fn label<'a>() -> impl Parser<'a, &'a str, String> {
    text::ident().map(|s: &str| s.to_string())
}

fn parse_const<'a>() -> impl Parser<'a, &'a str, String> {
    just("=").ignore_then(text::ident().map(|s: &str| s.to_string()))
}

fn mnemonic<'a>() -> impl Parser<'a, &'a str, Mnemonic> {
    choice([
        // R-Type
        just("ADD").to(Mnemonic::Add),
        just("SUB").to(Mnemonic::Sub),
        just("AND").to(Mnemonic::And),
        just("OR").to(Mnemonic::Or),
        just("XOR").to(Mnemonic::Xor),
        just("SHL").to(Mnemonic::Shl),
        just("SHR").to(Mnemonic::Shr),
        just("MOV").to(Mnemonic::Mov),
        // I-Type
        just("ADDI").to(Mnemonic::AddI),
        just("SUBI").to(Mnemonic::SubI),
        just("ANDI").to(Mnemonic::AndI),
        just("ORI").to(Mnemonic::OrI),
        just("XORI").to(Mnemonic::XorI),
        just("SHLI").to(Mnemonic::ShlI),
        just("SHRI").to(Mnemonic::ShrI),
        just("MOVI").to(Mnemonic::MovI),
        // M-Type
        just("LDW").to(Mnemonic::Ldw),
        just("STW").to(Mnemonic::Stw),
        just("LDB").to(Mnemonic::Ldb),
        just("STB").to(Mnemonic::Stb),
        // B-Type
        just("JMP").to(Mnemonic::Jmp),
        just("JEQ").to(Mnemonic::Jeq),
        just("JLT").to(Mnemonic::Jlt),
        just("JGT").to(Mnemonic::Jgt),
        // E-Type
        just("JMPW").to(Mnemonic::JmpW),
        just("JEQW").to(Mnemonic::JeqW),
        just("JLTW").to(Mnemonic::JltW),
        just("JGTW").to(Mnemonic::JgtW),
        just("CALL").to(Mnemonic::Call),
        // S-Type
        just("RET").to(Mnemonic::Ret),
        just("NOOP").to(Mnemonic::NoOp),
        just("HALT").to(Mnemonic::Halt),
    ])
}
