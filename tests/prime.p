bool:isprime(n)
{
    new i
    for (i = 2; i < n; ++i)
        if (n % i == 0)
            return false
    return true
}



primes(n)
{
    new count = 0
    new i
    for (i = 2; i <= n; ++i)
        if (isprime(i))
            ++count
    return count
}



main()
{
    new N = 200000
    printf("primes: %d\n", primes(N))
}
