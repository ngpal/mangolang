fn print_string(str: [char], len: int) {
	var i = 0;
	loop {
		if i >= len {
			break;
		}

		disp str[i];
		i = i + 1;
	}
}

fn main() {
	var a = "Hello, World!\n";
	print_string(a, 13);
}
