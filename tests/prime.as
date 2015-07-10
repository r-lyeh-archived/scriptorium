bool isprime(int n)
{
    int i;
    for (i = 2; i < n; ++i)
        if (n % i == 0)
            return false;
    return true;
}

int primes(int n)
{
    int count = 0;
    int i;
    for (i = 2; i <= n; ++i)
        if (isprime(i))
            ++count;
    return count;
}

void main()
{
    const int N = 200000;
    PrintString("primes: ");
    PrintInt(primes(N));
    PrintString("\n");
}
