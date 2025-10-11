fn print_string(s: [char]) {
	var i = 0;
	loop {
		if s[i] as int == 0 {
			break
		}

		disp s[i]
		i = i + 1
	}
}

fn main() {
	print_string("hello, world!");
}
