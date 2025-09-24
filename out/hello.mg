fn print_string(str: [char]) {
	var i = 0;
	loop {
		// null terminated
		if str[i] as int == 0 {
			break;
		}

		disp str[i];
		i = i + 1;
	}
}

fn main() {
	var string = "hello!";
	print_string(string)
}
