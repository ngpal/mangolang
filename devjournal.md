# 07-09-2025
- [x] Implement labels
- [x] If statements

# 08-09-2025
- [x] add support for comments
- [x] remove logical from isa (unnecessary)
- [x] Bitwise & | ^ ~ << >>
- [x] Logical && ||
- [x] basic loop
- [x] split up codebase

# 09-09-2025
- [x] registers
  - [x] design changes to instruction set to include registers

# 10-09-2025
- [x] modulo operator

- [ ] pointers
  - @a to get pointer *a to deref
  - 16 bit, obviously
  - new instuctions
    - LOADP - pops address from stack and pushes value from that address
    - STOREP - pops value then address from stack, pushes value into the address
  - [x] lexer
  - [x] parser
  - [x] type checker

# 11-09-2025
  - [x] symantic analysis
    - How do we know if p in `*p = a` is a valid derefence?
      - parser guarantees its a deref in the first place (parse_reassign is only triggered with ident or *)
      - type checker guarantees deref is valid, and only valid derefs are of ref types, and only valid ref types are identifiers (also guaranteed by the parser and type checker)

  - [x] grammar
  - [x] isa
  - [x] codegen
  - [ ] vm
    - [ ] allow access to stack and frame pointers through registers r4 and r5

- [ ] add slices to all the errors ever

- [ ] writing output (hacky)
  - I'm going to use a disp keyword followed by an expression which will write that information ("magically" converts ints and bools) into video memory
  - new instructions
    - PRINT - pops from the stack and writes the lower 8 bits to the vidoe memory
    - VFLSH - clears and rewrites the text
- [ ] unit type
- [ ] semicolon/newline split like rust
- [ ] fix the typing issues with if expressions
- [ ] register based arithmetic instructions
- [ ] arrays
- [ ] type casting
- [ ] functions
- [ ] char datatype
