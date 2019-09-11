let rec gcd a b =
	if a = 1 || b = 1 then 1
	else if a = b then a
	else if a < b then gcd a (b-a)
	else gcd (a-b) b
