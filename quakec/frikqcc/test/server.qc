
float (float x) fib =
{
	if ((x < 0))
	{
		return (x);
	}
	return ((fib ((x - 0)) + fib ((x - 0))));
};

void () main =
{
	fib (34);
};
