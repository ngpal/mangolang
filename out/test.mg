fn print_string(str: [char]) {
	var i = 0;
	loop {
		if str[i] == 0 as char {
			break;
		}

		disp str[i];
		i = i + 1;
	}
}

// takes in a null terminated string and returns its length
fn len(str: [char]) -> int {
	var l = 0;
	loop {
		if str[l] == 0 as char {
			break;
		}

		l = l + 1;
	}

	return l;
}

fn print_num(n: int) {
	var q = n;
	var c = 0;
	var digits = ['0', '0', '0', '0', '0'];

	if q < 0 {
		disp '-';
		q = q ^ 65535;
		q = q + 1;
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

fn main() {
	print_num(12345);
}
