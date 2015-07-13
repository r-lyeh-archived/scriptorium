#include <stdio.h>

int fibR(int n)
{
    if (n < 2) return n;
    return (fibR(n-2) + fibR(n-1));
}


int fibI(int n)
{
    int last, cur, tmp;
    last = 0;
    cur = 1;
    n = n - 1;
    while(n)
    {
        --n;
        tmp = cur;
        cur = last + cur;
        last = tmp;
    }
    return cur;
}


int main()
{
    int N;
    N = 34; //Should return 433494437
    printf("fib: %d = %d\n", fibR(N), fibI(N));
    return 0;
}
