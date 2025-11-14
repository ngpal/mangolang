fn main() {
	var i = 39;
	while i < 1 {
		var j = i; 
		loop {
			if j < 1 {
				break;
			}

			disp '*'
			j = j - 1;
		}
		disp '\r'
		disp '\n'

		i = i - 2;
	}
}
