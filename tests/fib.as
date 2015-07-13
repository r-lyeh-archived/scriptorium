int fibR(int n)
{
    if (n < 2) return n;
    return (fibR(n-2) + fibR(n-1));
}

int main(int unused)
{
    const int N = 34;
    print("fib: ");
    print(fibR(N));
    print("\n");
    return 0;
}
