var n = 3;
var p1 = 1;
var p2 = 1;
var p3 = 0;
var i = 0;

var fib = loop {
	p3 = p1 + p2
	p1 = p2
	p2 = p3

	if i >= n {
		break p3
	} else {
		i = i + 1
	}
}

fib
