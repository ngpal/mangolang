fn print_num(n: int) {
	var q = n;
	var digits[5: char];
	var c = 0;

	if q < 0 {
	 	disp '-';
	 	q = -q;
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
		mem[n] = 1;
    }

	if mem[n] != 0 {
		return mem[n];
	}

    var res = fib(mem, n - 1) + fib(mem, n - 2);
	mem[n] = res;

	return res;
}

fn print_string(str: [char]) {
	var i = 0;
	loop {
		if str[i] as int == 0 {
			break;
		}

		disp str[i];
		i = i + 1;
	}
}

fn main() {
	var i = 0;
	var nums = 23;
	var mem[23: int];

	// initialize mem to 0s
	loop {
		if i > nums - 1 {
			break;
		}

		mem[i] = 0;
		i = i + 1;
	}

	i = 0;
	loop {
		if i == nums {
			break;
		}
	
		print_num(fib(mem, i));
		print_string("\r\n");
		i = i + 1;
	}

	print_string("power of memoization!")
}
