
float (float x) fib = {
	if (x < 0) return (x);
	return fib(x-1)+fib(x-2);
};

void () main = {
	fib(34);
};
