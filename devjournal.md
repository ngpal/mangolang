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

- [x] pointers
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
  - [x] vm
    - [x] allow access to stack and frame pointers through registers r4 and r5

- [x] add slices to all the errors ever

- [ ] writing output (hacky)
  - I'm going to use a disp keyword followed by an expression which will write that information ("magically" converts ints and bools) into video memory
  - new instructions
    - PRINT - pops from the stack and writes the lower 8 bits to the video memory at cursor
    - MVCUR i8 - move the cursor relative to current position
  - [x] lexer
  - [x] parser
  - [x] type checker
  - [x] semantics (not really needed)
  - [x] isa and grammar
  - [ ] codegen

    ### Sidequest alert!
    - [ ] call and ret instructions with print subroutine at the bottom of the file
    - [ ] assmebler emiting object files
    - [ ] linker
    
    - [ ] compiler
    - [ ] binary gen

  - [ ] vm
    - [ ] must inc ip as soon as instruction is fetched for ret and call purposes
    - [ ] implement video memory
    - [ ] implement print video memory slice up until the cursor
    - [ ] implement instructions

- [ ] split compiler into compiler, assembler and linker
- [ ] rewrite hardcoded print subroutine in assembly
- [ ] unit type
- [ ] semicolon/newline split like rust
- [ ] fix the typing issues with if expressions
- [ ] register based arithmetic instructions
- [ ] arrays
- [ ] type casting
- [ ] functions
- [ ] char datatype
