//#include <console.inc>

fibR(n)
{
    if (n < 2) return n
    return (fibR(n-2) + fibR(n-1))
}


fibI(n)
{
    new last = 0
    new cur = 1
    n = n - 1
    while(n)
    {
        --n
        new tmp = cur
        cur = last + cur
        last = tmp
    }
    return cur
}

main()
{
    new N = 34 //Should return 433494437
    printf("fib: %d = %d\n", fibR(N), fibI(N))
}

