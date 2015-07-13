//#include <console.inc>

fibR(n)
{
    if (n < 2) return n
    return (fibR(n-2) + fibR(n-1))
}


main()
{
    new N = 34 //Should return 433494437
    printf("fib: %d\n", fibR(N))
}

