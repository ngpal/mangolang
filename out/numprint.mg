fn print_num(n: int) {
	var q = n;
	loop {
		if q == 0 {
			break;
		}

		disp ((q % 10) + ('0' as int)) as char;
		q = q / 10;
	}
}

fn main() {
	print_num(9920)
}
