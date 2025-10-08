fn __imul(a: int, b: int) -> int {
    var x = a;
    var y = b;
    var acc = 0;
    var negative = ((x < 0) as int ^ (y < 0) as int) != 0; // store sign

    if x < 0 { x = -x; }
    if y < 0 { y = -y; }

    loop {
		if x <= 0 {
			break
		}

        if (x & 1) != 0 {
            acc = acc + y;
        }

        y = y << 1;
        x = x >> 1;
    }

    if negative {
        acc = -acc;
    }

    return acc
}
