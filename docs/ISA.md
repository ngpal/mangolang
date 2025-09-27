# Instruction Set Architecture

- 6 general puprose registers r0-r5
- r6 is stack pointer
- r7 is frame pointer
- special registers: instruction pointer, link register, flag register (nzcv)
- 16-bit word
- 16-bit fixed length instructions
  - 5-bit opcodes, 2^5 = 32 instructions in total

## Instruction Formats

### R-Type (Register)

| Bits  | Purpose  |
| ----- | -------- |
| 15:11 | opcode   |
| 10:6  | reserved |
| 5:3   | rd       |
| 2:0   | rs       |

### I-Type (Immediate)

| Bits  | Purpose |
| ----- | ------- |
| 15:11 | opcode  |
| 10:8  | rd      |
| 7:0   | imm8    |

### M-Type (Memory)

| Bits  | Purpose |
| ----- | ------- |
| 15:11 | opcode  |
| 10:8  | rd      |
| 7:5   | rs      |
| 4:0   | imm5    |

### B-Type (Branch)

| Bits  | Purpose |
| ----- | ------- |
| 15:11 | opcode  |
| 10:9  | cond    |
| 7:0   | imm8    |

### E-Type (Extension type)

| Bits    | Purpose  |
| ------- | -------- |
| 15:11   | opcode   |
| 10:0    | reserved |
| +1 word | extra    |

### S-Type (Single)

| Bits  | Purpose  |
| ----- | -------- |
| 15:11 | opcode   |
| 10:0  | reserved |

## Instructions

### R-Type (rd, rs)

| Mnemonic | Opcode | Reserved | Description   |
| -------- | ------ | -------- | ------------- |
| ADD      | 00001  | 00000    | rd = rd + rs  |
| SUB      | 00001  | 00001    | rd = rd - rs  |
| AND      | 00010  | 00000    | rd = rd & rs  |
| OR       | 00010  | 00001    | rd = rd \| rs |
| XOR      | 00010  | 00010    | rd = rd ^ rs  |
| SHL      | 00011  | 00000    | rd = rd << rs |
| SHR      | 00011  | 00001    | rd = rd >> rs |
| MOV      | 00100  | 00000    | rd = rs       |

### I-Type (rd, imm8)

| Mnemonic | Opcode | Description     |
| -------- | ------ | --------------- |
| ADDI     | 00101  | rd = rd + imm8  |
| SUBI     | 00110  | rd = rd - imm8  |
| ANDI     | 00111  | rd = rd & imm8  |
| ORI      | 01000  | rd = rd \| imm8 |
| XORI     | 01001  | rd = rd ^ imm8  |
| SHLI     | 01010  | rd = rd << imm8 |
| SHRI     | 01011  | rd = rd >> imm8 |
| MOVI     | 01100  | rd = imm8       |

### M-Type (rd, rs, imm5)

| Mnemonic | Opcode | Description            |
| -------- | ------ | ---------------------- |
| LDW      | 01101  | rd = \[rs + imm5]      |
| STW      | 01110  | \[rd + imm5] = rs      |
| LDB      | 01111  | rd = \[rs + imm5] byte |
| STB      | 10000  | \[rd + imm5] = rs byte |

### B-Type (cond, imm8)

| Mnemonic | Opcode | Cond | Description          |
| -------- | ------ | ---- | -------------------- |
| JMP      | 10001  | 00   | Unconditional jump   |
| JEQ      | 10001  | 01   | Jump if equal        |
| JLT      | 10001  | 10   | Jump if less than    |
| JGT      | 10001  | 11   | Jump if greater than |

### E-Type (imm16)

| Mnemonic | Opcode | Reserved    | Description              |
| -------- | ------ | ----------- | ------------------------ |
| JMPW     | 10010  | 00000000000 | Unconditional jump       |
| JEQW     | 10010  | 00000000001 | Jump if equal            |
| JLTW     | 10010  | 00000000010 | Jump if less than        |
| JGTW     | 10010  | 00000000011 | Jump if greater than     |
| CALL     | 10011  | 00000000000 | Jump to imm16 setting LR |

### S-Type

| Mnemonic | Opcode | Reserved    | Description  |
| -------- | ------ | ----------- | ------------ |
| RET      | 10100  | 00000000000 | Jump to LR   |
| NOOP     | 00000  | 00000000000 | Do nothing   |
| HALT     | 11111  | 00000000000 | Halt program |
