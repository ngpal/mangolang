#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Mnemonic(String),
    Register(String),
    Immediate(String),
    Label(String),
    Punct(char), // , [ ] =
}

pub fn tokenize(line: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut buf = String::new();
    let mut chars = line.chars().peekable();

    while let Some(&c) = chars.peek() {
        match c {
            // punctuation always separate tokens
            ',' | '[' | ']' | '=' => {
                if !buf.is_empty() {
                    tokens.push(classify(&buf));
                    buf.clear();
                }
                tokens.push(Token::Punct(c));
                chars.next();
            }
            // whitespace → flush buffer
            c if c.is_whitespace() => {
                if !buf.is_empty() {
                    tokens.push(classify(&buf));
                    buf.clear();
                }
                chars.next();
            }
            // part of an identifier / number
            _ => {
                buf.push(c);
                chars.next();
            }
        }
    }

    if !buf.is_empty() {
        tokens.push(classify(&buf));
    }

    tokens
}

fn classify(s: &str) -> Token {
    let upper = s.to_ascii_uppercase();

    if upper == "SP" || upper == "FP" {
        Token::Register(upper)
    } else if upper.starts_with('R') && upper[1..].chars().all(|c| c.is_ascii_digit()) {
        Token::Register(upper)
    } else if upper.starts_with("0X")
        || upper.starts_with('-')
        || upper.chars().all(|c| c.is_ascii_digit())
    {
        Token::Immediate(s.to_string())
    } else if is_mnemonic(&upper) {
        Token::Mnemonic(upper)
    } else {
        Token::Label(s.to_string())
    }
}

fn is_mnemonic(s: &str) -> bool {
    matches!(
        s,
        "ADD"
            | "SUB"
            | "AND"
            | "OR"
            | "XOR"
            | "SHL"
            | "SHR"
            | "MOV"
            | "ADDI"
            | "SUBI"
            | "ANDI"
            | "ORI"
            | "XORI"
            | "SHLI"
            | "SHRI"
            | "MOVI"
            | "LDW"
            | "STW"
            | "LDB"
            | "STB"
            | "JMP"
            | "JEQ"
            | "JLT"
            | "JGT"
            | "JMPW"
            | "JEQW"
            | "JLTW"
            | "JGTW"
            | "CALL"
            | "MOVIW"
            | "RET"
            | "NOOP"
            | "HALT"
    )
}
