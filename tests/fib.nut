function fibR(n)
{
    if (n < 2) return n;
    return (fibR(n-2) + fibR(n-1));
}

local N = 34;
print("fib: " + fibR(N) + "\n");
