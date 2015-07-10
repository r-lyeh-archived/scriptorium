#include <stdio.h>

int isprime(int n)
{
    int i;
    for (i = 2; i < n; ++i)
        if (n % i == 0)
            return 0;
    return 1;
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



int main()
{
    int N = 200000;
    printf("primes: %d\n", primes(N));
}
