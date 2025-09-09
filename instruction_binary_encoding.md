# Instruction Set (Current)

- 4 general purpose registers - r0 to r3
- 2 register IDs may take up 1 byte low 4 bits one reg, high 4 bits the second

### **0x0\_ — Stack & Control**

| Opcode | Mnemonic | Operands | Notes             |
| ------ | -------- | -------- | ----------------- |
| 0x01   | PUSH16   | imm16    | push 16-bit value |
| 0x0F   | HALT     | —        | stop execution    |

---

### **0x1\_ — Memory**

| Opcode | Mnemonic | Operands | Notes            |
| ------ | -------- | -------- | ---------------- |
| 0x10   | LOAD8    | addr8    | load from memory |
| 0x11   | STORE8   | addr8    | store to memory  |

---

### **0x2\_ — Jumps & Branches**

| Opcode | Mnemonic | Operands | Notes                        |
| ------ | -------- | -------- | ---------------------------- |
| 0x20   | JMP8     | rel8     | jump relative (signed 8-bit) |
| 0x21   | JLT8     | rel8     | jump if less-than            |
| 0x22   | JGT8     | rel8     | jump if greater-than         |
| 0x23   | JEQ8     | rel8     | jump if equal                |

---

### **0x3\_ — Integer Arithmetic**

| Opcode | Mnemonic | Operands | Notes                  |
| ------ | -------- | -------- | ---------------------- |
| 0x30   | ADD      | —        | integer addition       |
| 0x31   | SUB      | —        | integer subtraction    |
| 0x32   | MUL      | —        | integer multiplication |
| 0x33   | DIV      | —        | integer division       |
| 0x34   | NEG      | —        | negate integer         |
| 0x35   | CMP      | —        | compare two ints       |

---

### **0x4\_ - Bitwise Ops**

| Opcode | Mnemonic | Operands | Notes         |
| ------ | -------- | -------- | ------------- |
| 0x40   | NOT      | —        | bitwise not   |
| 0x41   | AND      | —        | bitwise and   |
| 0x42   | OR       | —        | bitwise or    |
| 0x43   | XOR      | —        | bitwise xor   |
| 0x44   | SHFT     | —        | bitwise shift |

---

### **0x5\_ - Register Ops**

| Opcode | Mnemonic | Operands | Notes                          |
| ------ | -------- | -------- | ------------------------------ |
| 0x50   | MOV      | rd, rs   | rd = rs                        |
| 0x51   | PUSHR    | rs       | push contents of rs onto stack |
| 0x52   | POPR     | rd       | pop stack top into rd          |
