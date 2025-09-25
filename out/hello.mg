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
	print_string("this won't work all the time! (strings straight in an argument)")
}
