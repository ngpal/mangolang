fn main() {
	var i = 100;
	while i > 1 {
		if i % 2 == 0 {
			disp ((i + '0' as int) as char);
			disp '\n';
			disp '\r';
		}
		i = i - 1;
	}
}
