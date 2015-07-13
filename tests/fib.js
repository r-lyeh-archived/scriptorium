function fibR(n)
{
    if (n < 2) return n;
    return (fibR(n-2) + fibR(n-1));
};

var N = 34; //Should return 433494437
print("fib: " + fibR(N));
