fn print_num(n: int) {
	var q = n;
	var digits[5: char];
	var c = 0;

	if q < 0 {
		disp '-';
		q = -q
	}

	loop {
		if q == 0 {
			break;
		}

		digits[c] = ((q % 10) + ('0' as int)) as char;
		c = c + 1;
		q = q / 10;
	}

	loop {
		c = c - 1;
		if c < 0 {
			break;
		}

		disp digits[c];
	}
}

fn fib(n: int) -> int {
    if n <= 1 {
        return 1;
    }
    return fib(n - 1) + fib(n - 2);
}

fn main() {
	var i = 0;
	loop {
		if i > 23 {
			break;
		}

		print_num(fib(i));
		disp '\n';
		disp '\r';
		i = i + 1;
	}
}
