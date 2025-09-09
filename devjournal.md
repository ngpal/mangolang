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
