Instruction Set (VM v0.1.1)

- 8 general purpose registers - r0 to r7
- sp, fp are r8, r9
- r0-r5 is volatile,
- 2 register IDs may take up 1 byte low 4 bits one reg, high 4 bits the second

### Interrupt table

| code | name  | arguments | description               |
| ---- | ----- | --------- | ------------------------- |
| 0x00 | print | r0 - imm8 | prints r0 to the terminal |

### **0x0\_ — Stack & Control**

| Opcode | Mnemonic | Operands | Notes             |
| ------ | -------- | -------- | ----------------- |
| 0x01   | PUSH16   | imm16    | push 16-bit value |
| 0x0F   | HALT     | -        | stop execution    |

### **0x1\_ — Memory**

| Opcode | Mnemonic            | Operands | Notes                                              |
| ------ | ------------------- | -------- | -------------------------------------------------- |
| 0x10   | LOAD8 (depricated)  | addr8    | load from memory                                   |
| 0x11   | STORE8 (depricated) | addr8    | store to memory                                    |
| 0x12   | LDW                 | -        | pops addr and pushes mem\[addr] to stack           |
| 0x13   | STW                 | -        | pops addr and value from stack, stores val at addr |
| 0x16   | LDB                 | -        | pops addr and pushes byte from addr to stack       |
| 0x17   | STB                 | -        | pops addr and word, writes lower bytes to stack    |
| 0x14   | LDR (convenience)   | rs+imm   | push val @ \[rs + imm] (uses LDW under the hood)   |
| 0x15   | STR (convenience)   | rd+imm   | \[rd + imm] = pop (uses STW under the hood)        |

### **0x2\_ — Jumps & Branches**

| Opcode | Mnemonic          | Operands | Notes                                  |
| ------ | ----------------- | -------- | -------------------------------------- |
| 0x20   | JMP8 (depricated) | rel8     | jump relative (signed 8-bit)           |
| 0x21   | JLT8 (depricated) | rel8     | jump if less-than                      |
| 0x22   | JGT8 (depricated) | rel8     | jump if greater-than                   |
| 0x23   | JEQ8 (depricated) | rel8     | jump if equal                          |
| 0x24   | CALL              | addr16   | pushes ip onto stack and jumps to addr |
| 0x25   | RET               | -        | pops from stack into ip                |
| 0x26   | JMP16             | rel16    | jump relative (signed 16-bit)          |
| 0x27   | JLT16             | rel16    | jump if less-than                      |
| 0x28   | JGT16             | rel16    | jump if greater-than                   |
| 0x29   | JEQ16             | rel16    | jump if equal                          |

### **0x3\_ — Integer Arithmetic**

| Opcode | Mnemonic          | Operands | Notes                                         |
| ------ | ----------------- | -------- | --------------------------------------------- |
| 0x30   | ADD               | -        | integer addition                              |
| 0x31   | SUB (convenience) | -        | integer subtraction (add with 2's compliment) |
| 0x32   | MUL (convenience) | -        | integer multiplication                        |
| 0x33   | DIV (convenience) | -        | integer division                              |
| 0x34   | NEG (convenience) | -        | negate integer                                |
| 0x35   | CMP               | -        | compare two ints (sub with flags)             |
| 0x36   | MOD (convenience) | -        | integer modulus                               |

- Convenience instructions for all these accepting 1 immediate

### **0x4\_ - Bitwise Ops**

| Opcode | Mnemonic | Operands | Notes               |
| ------ | -------- | -------- | ------------------- |
| 0x40   | NOT      | -        | bitwise not         |
| 0x41   | AND      | -        | bitwise and         |
| 0x42   | OR       | -        | bitwise or          |
| 0x43   | XOR      | -        | bitwise xor         |
| 0x44   | SHL      | -        | bitwise shift left  |
| 0x45   | SHR      | -        | bitwise shift right |

- Convenience instructions for all these accepting 1 immediate

### **0x5\_ - Register Ops**

| Opcode | Mnemonic | Operands | Notes                          |
| ------ | -------- | -------- | ------------------------------ |
| 0x50   | MOV      | rd, rs   | rd = rs                        |
| 0x51   | PUSHR    | rs       | push contents of rs onto stack |
| 0x52   | POPR     | rd       | pop stack top into rd          |

### **0x6\_ - Video Ops**

| Opcode | Mnemonic           | Operands | Notes                                    |
| ------ | ------------------ | -------- | ---------------------------------------- |
| 0x60   | PRINT (depricated) | -        | pop from stack and write byte at cursor  |
| 0x61   | MVCUR (depricated) | rel8     | move cursor relative to current position |

### **0x7\_ - Interrupts**

| Opcode | Mnemonic          | Operands | Notes                                |
| ------ | ----------------- | -------- | ------------------------------------ |
| 0x70   | INT               | imm8     | executes said interrupt              |
| 0x71   | IRET (privilaged) | -        | same as ret but sets k flag to false |
| 0x72   | BKPT              | -        | Stops the debugger                   |
