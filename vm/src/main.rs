use clap::Parser;
use std::{error::Error, fmt::Display, fs, process};

const STACK_SIZE: usize = 16;

#[derive(Debug)]
pub enum Instruction {
    Push(u32),
    Pop,
    Add,
    Sub,
    Mul,
    Div,
    Neg,
    Halt,
}

#[derive(Debug)]
pub struct RuntimeError(pub String);
impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "RuntimeError: {}", self.0)
    }
}
impl Error for RuntimeError {}
pub type RuntimeResult<T> = Result<T, RuntimeError>;

pub struct Vm {
    pub stack: [u32; STACK_SIZE],
    sp: usize,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            stack: [0; STACK_SIZE],
            sp: 0,
        }
    }

    pub fn exec(&mut self, program: &[Instruction]) -> RuntimeResult<()> {
        for instr in program {
            match instr {
                Instruction::Push(val) => {
                    self.stack[self.sp] = *val;
                    self.inc_sp()?;
                }
                Instruction::Pop => self.dec_sp()?,
                Instruction::Add | Instruction::Sub | Instruction::Mul | Instruction::Div => {
                    let right = self.top();
                    self.dec_sp()?;
                    let left = self.top();
                    self.dec_sp()?;

                    self.stack[self.sp] = match instr {
                        Instruction::Add => left.wrapping_add(right),
                        Instruction::Sub => left.wrapping_sub(right),
                        Instruction::Mul => left.wrapping_mul(right),
                        Instruction::Div => {
                            if right == 0 {
                                return Err(RuntimeError("division by zero".into()));
                            }
                            left.wrapping_div(right)
                        }
                        _ => unreachable!(),
                    };
                    self.inc_sp()?;
                }
                Instruction::Neg => {
                    let operand = self.top();
                    self.stack[self.sp - 1] = (!operand).wrapping_add(1);
                }
                Instruction::Halt => return Ok(()),
            }
        }
        Ok(())
    }

    fn inc_sp(&mut self) -> RuntimeResult<()> {
        if self.sp >= STACK_SIZE - 1 {
            Err(RuntimeError("stack overflow".into()))
        } else {
            self.sp += 1;
            Ok(())
        }
    }

    fn dec_sp(&mut self) -> RuntimeResult<()> {
        if self.sp == 0 {
            Err(RuntimeError("stack underflow".into()))
        } else {
            self.sp -= 1;
            Ok(())
        }
    }

    pub fn top(&self) -> u32 {
        if self.sp == 0 {
            0
        } else {
            self.stack[self.sp - 1]
        }
    }
}

fn parse_program(source: &str) -> RuntimeResult<Vec<Instruction>> {
    source
        .lines()
        .map(|line| {
            let words: Vec<&str> = line.split_whitespace().collect();

            match words.get(0) {
                Some(cmd) => {
                    let cmd_upper = cmd.to_uppercase();
                    match cmd_upper.as_str() {
                        "PUSH" => {
                            if let Some(val) = words.get(1) {
                                Ok(Instruction::Push(val.parse().map_err(|_| {
                                    RuntimeError(format!("Invalid number: {}", val))
                                })?))
                            } else {
                                Err(RuntimeError("Missing argument for PUSH".into()))
                            }
                        }
                        "POP" => Ok(Instruction::Pop),
                        "ADD" => Ok(Instruction::Add),
                        "SUB" => Ok(Instruction::Sub),
                        "MUL" => Ok(Instruction::Mul),
                        "DIV" => Ok(Instruction::Div),
                        "NEG" => Ok(Instruction::Neg),
                        "HALT" => Ok(Instruction::Halt),
                        _ => Err(RuntimeError(format!("Unknown command {}", cmd))),
                    }
                }
                None => Ok(Instruction::Halt), // empty line -> ignore
            }
        })
        .collect()
}

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Cli {
    filename: String,
    #[arg(short, long)]
    show_top: bool,
}

fn main() {
    let cli = Cli::parse();
    let code = fs::read_to_string(&cli.filename).unwrap_or_else(|err| {
        eprintln!("failed to read {}: {}", cli.filename, err);
        process::exit(1);
    });

    let program = match parse_program(&code) {
        Ok(p) => p,
        Err(err) => {
            eprintln!("{}", err);
            process::exit(1);
        }
    };

    let mut vm = Vm::new();
    if let Err(err) = vm.exec(&program) {
        eprintln!("{}", err);
        process::exit(1);
    }

    if cli.show_top {
        println!("top of stack: {}", vm.top());
    }
}
