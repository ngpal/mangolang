use clap::Parser;
use std::{error::Error, fmt::Display, fs, process};

const STACK_SIZE: usize = 16;

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
    pub stack: [isize; STACK_SIZE],
    sp: usize,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            stack: [0; STACK_SIZE],
            sp: 0,
        }
    }

    pub fn exec(&mut self, code: String) -> RuntimeResult<()> {
        for line in code.split("\n") {
            let words: Vec<&str> = line.split(" ").collect();
            match words[0] {
                "PUSH" => {
                    self.stack[self.sp] = words[1].parse().unwrap();
                    self.inc_sp()?;
                }
                "POP" => self.dec_sp()?,
                "ADD" | "SUB" | "MUL" | "DIV" => {
                    let right = self.top();
                    self.dec_sp()?;
                    let left = self.top();
                    self.dec_sp()?;

                    self.stack[self.sp] = match words[0] {
                        "ADD" => left + right,
                        "SUB" => left - right,
                        "MUL" => left * right,
                        "DIV" => {
                            if right == 0 {
                                return Err(RuntimeError("division by zero".into()));
                            }
                            left / right
                        }
                        _ => unreachable!(),
                    };
                    self.inc_sp()?;
                }
                "NEG" => {
                    let operand = self.top();
                    self.stack[self.sp - 1] = -operand;
                }
                "HALT" => return Ok(()),
                _ => return Err(RuntimeError(format!("Unknown command {}", words[0]))),
            }
        }

        Ok(())
    }

    fn inc_sp(&mut self) -> RuntimeResult<()> {
        if self.sp >= STACK_SIZE - 1 {
            Err(RuntimeError(String::from("stack overflow")))
        } else {
            self.sp += 1;
            Ok(())
        }
    }

    fn dec_sp(&mut self) -> RuntimeResult<()> {
        if self.sp <= 0 {
            Err(RuntimeError(String::from("stack underflow")))
        } else {
            self.sp -= 1;
            Ok(())
        }
    }

    pub fn top(&self) -> isize {
        if self.sp == 0 {
            return 0;
        }
        self.stack[self.sp - 1]
    }
}

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Cli {
    /// input program file
    filename: String,

    /// show the top of the stack after execution
    #[arg(short, long)]
    show_top: bool,
}

fn main() {
    let cli = Cli::parse();

    // read file
    let code = fs::read_to_string(&cli.filename).unwrap_or_else(|err| {
        eprintln!("failed to read {}: {}", cli.filename, err);
        process::exit(1);
    });

    // run vm
    let mut vm = Vm::new();
    if let Err(err) = vm.exec(code) {
        eprintln!("{}", err);
        process::exit(1);
    }

    if cli.show_top {
        println!("top of stack: {}", vm.top());
    }
}
