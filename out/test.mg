fn main() {
	var a = ['h', 'i', '!', '\n', ':', ')'];
	var i = 0;
	loop {
		if i >= 9 {
			break
		}

		disp a[i];
		i = i + 1;
	}
}
