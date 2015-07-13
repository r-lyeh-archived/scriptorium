function fibR(n)
{
    if (n < 2) return n;
    return (fibR(n-2) + fibR(n-1));
};


function fibI(n)
{
    var last, cur, tmp;
    last = 0;
    cur = 1;
    n = n - 1;
    while(n)
    {
        --n;
        tmp = cur;
        cur = last + cur;
        last = tmp;
    };
    return cur;
};


var N = 34; //Should return 433494437
print("fib: " + fibR(N) + "=" + fibI(N));
