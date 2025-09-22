fn fib(n: int) -> int {
    if n <= 1 {
        return 1;
    }
    return fib(n - 1) + fib(n - 2);
}

fn main() {
	var i = 0;
	loop {
		if i > 11 {
			break;
		}

		disp fib(i);
		i = i + 1;
	}
}
