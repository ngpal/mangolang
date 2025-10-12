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

fn fib(mem: [int], n: int) -> int {
    if n <= 1 {
        return 1;
    } else if mem[n] != 0 {
		return mem[n];
	}

    var res = fib(mem, n - 1) + fib(mem, n - 2);
	mem[n] = res;

	return res;
}

fn main() {
	var i = 0;
	var nums = 3;
	var mem[3: int];

	// initialize mem to 0s
	loop {
		if i > nums - 1 {
			break;
		}

		mem[i] = 0;
		i = i + 1;
	}
	
	loop {
		if i > 23 {
			break;
		}

		print_num(fib(mem, i));
		disp '\n';
		disp '\r';
		i = i + 1;
	}
}
