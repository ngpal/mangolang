fn print_num(n: int) {
	var q = n;
	var digits[5: int];
	var c = 0;

	loop {
		if q == 0 {
			break;
		}

		digits[c] = ((q % 10) + ('0' as int));
		c = c + 1;
		q = q / 10;
	}

	loop {
		if c < 0 {
			break;
		}

		disp digits[c] as char;
		c = c - 1;
	}
}

fn main() {
	print_num(9920)
}
