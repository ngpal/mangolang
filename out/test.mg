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

fn main() {
	var a = "Hello, World!\n";
	print_string(a);
}
