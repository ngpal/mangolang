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

- [x] writing output (hacky)
  - I'm going to use a disp keyword followed by an expression which will write that information ("magically" converts ints and bools) into video memory
  - new instructions
    - PRINT - pops from the stack and writes the lower 8 bits to the video memory at cursor
    - MVCUR i8 - move the cursor relative to current position
  - [x] lexer
  - [x] parser
  - [x] type checker
  - [x] semantics (not really needed)
  - [x] isa and grammar
  - [x] codegen

    ### Sidequest alert!
    - [x] call and ret instructions with print subroutine at the bottom of the file
    - [x] assmebler emiting object files
    - [x] linker
    
    - [x] compiler
    - [x] binary gen

  - [x] vm
    - [x] must inc ip as soon as instruction is fetched for ret and call purposes
    - [x] new instructions 
    - [x] implement video memory
    - [x] implement print video memory with crossterm
    - [x] implement instructions

# 12-09-2025
- [x] debugger upgrade

# 13-09-2025
- [x] refactor assembler
- [x] refactor linker

# 14-09-2025
- [x] split compiler into compiler, assembler and linker
- [x] enable vm to check for magic number in binary
- [x] rewrite hardcoded print subroutine in assembly

# 15-09-2025
- [x] functions
  - [x] func def
    - [x] parsing
    - [x] type checking - disallow redefs
    - [x] semantincs

# 16-09-2025
  - [x] func calling
    - [x] parsing
    - [x] type checking
    - [x] symantics
  - [x] instructions for memory access wrt pointer registers
  - [x] get rid of the whole slots situation actually, use stack pointer offsets

  ## Major deprication in the ISA
    - LOAD8 and STORE8 will no longer be used

  ## New conundrum
    - what if the stack is dirty when calling ret?
    - return values, where do they go?

  ## The answer?
    - Calling convention!

  - [x] codegen

# 18-09-2025
  - [x] give main function special thingies like
    - [x] no params
    - [x] no return
    - [x] no RET, straight to HALT
  - [x] assembler
  - [x] linker?
  - [x] implement new ops on vm
  - [x] require main function

# 19-09-2025
- [x] fix bug in typechecker that ignores returns nested inside other structures
  - took out hunting for a return statement in the function. that wont be good long term

# 20-09-2025
- [[what-do-the-semantic-passes-handle-currently|check for notes on the type checker]]

## Plan of action
- [x] renames slices to span
- [x] modify AST to handle slices properly and accurately

# 21-09-2025
- [x] handle return type problem (does not even check for returns in a function now)

### The `RetStatus` Situation
  - Allow `Maybe`s when type checking, throw errors at it in the analyzer run

  - [x] modify AST to carry types, set by type checker
  - [x] `analyzer.rs` should handle main function checking, breaks/continues, reference validation

- [ ] seperating if statements and if expressions
- [ ] fix loop print 1-10 code bug
- [ ] Unit testingggggg aaaaaaaa
- [ ] implement an IR for base level optimizations
  - [ ] implement IR
  - [ ] constant folding
  - [ ] dead code analysis
- [ ] semicolon/newline split like rust
- [ ] char datatype
- [ ] functions as parameters
- [ ] arrays
  - [ ] messes everything up bc first datatype thats not just 1 word long
- [ ] Strings!
- [ ] fix the typing issues with if expressions
- [ ] register based arithmetic instructions with mod
- [ ] type casting
