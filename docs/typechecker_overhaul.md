# What do the semantic passes handle currently?
## `type_check.rs`
1. type checking expressions
2. assign variable slots on the local scope
3. store function signatures
4. type checking function definitions (partially)
5. type checking function calls
6. keeps track of the loop stack to match break statements
7. keeps track of type names

### NOTES
- some of this work shouldn't really be handled by the type checker (2)
- major bugs in the type checking of function definitions, return statements are kinda iffy
  - funcdef does not need to have a return statement even if a return type if given
  - return types and expression types arent handled seperately, especially in control flow cases,leading to ambiguity in the code that could bite me in th ass later
- does not embed the AST with type information yet, which will cause issues in later passes for recomputation

## `analyzer.rs`
- checks main function for existence, 0 params, and unit return
- checks wether breaks and continues are inside the loops or not
- checks refernces to see if they are values you can actually reference (variables)

### NOTES
- main function signature validation can be cut down drastically by just matching to a template signature
- i need to split things up more nicely
- typed AST

