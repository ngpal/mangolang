fn get_biggest(a: int, b: int) -> int {
	if a > b {
		return a;
	}

	return b;
}

fn main() {
	var a = 0;
	loop {
		if a > 22 {
			break;
		}

		disp get_biggest(a, a + 1);
		a = a + 1;
	}
}
