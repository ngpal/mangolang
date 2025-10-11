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

fn main() {
	print_num(-29876)
}
