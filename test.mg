var a: int = 10;
var b: int = 20;

var p: @int = @a;  // p is a reference to a
var q: @@int = @p; // q is a reference to p (nested reference)

// modify a through p
*p = *p + b;

// modify a through q
**q = **q + 5;

// read value via both a and p
b = a + *p;

// final expression -> should be on stack
b
