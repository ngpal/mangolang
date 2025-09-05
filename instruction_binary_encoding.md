# Instruction Set (Current)

### **0x0_ — Stack & Control**
| Opcode | Mnemonic | Operands | Notes            |
|--------|----------|----------|------------------|
| 0x01   | PUSH16   | imm16    | push 16-bit value|
| 0x0F   | HALT     | —        | stop execution   |

---

### **0x1_ — Memory**
| Opcode | Mnemonic | Operands | Notes                  |
|--------|----------|----------|------------------------|
| 0x10   | LOAD8    | addr8    | load from memory       |
| 0x11   | STORE8   | addr8    | store to memory        |

---

### **0x2_ — Jumps & Branches**
| Opcode | Mnemonic | Operands | Notes                         |
|--------|----------|----------|-------------------------------|
| 0x20   | JMP8     | rel8     | jump relative (signed 8-bit)  |
| 0x21   | JLT8     | rel8     | jump if less-than             |
| 0x22   | JGT8     | rel8     | jump if greater-than          |
| 0x23   | JEQ8     | rel8     | jump if equal                 |

---

### **0x3_ — Integer Arithmetic**
| Opcode | Mnemonic | Operands | Notes                |
|--------|----------|----------|----------------------|
| 0x30   | IADD     | —        | integer addition     |
| 0x31   | ISUB     | —        | integer subtraction  |
| 0x32   | IMUL     | —        | integer multiplication|
| 0x33   | IDIV     | —        | integer division     |
| 0x34   | NEG      | —        | negate integer       |
| 0x35   | ICMP     | —        | compare two ints     |

---

### **0x4_ — Logical Ops**
| Opcode | Mnemonic | Operands | Notes        |
|--------|----------|----------|--------------|
| 0x40   | NOT      | —        | bitwise not  |
