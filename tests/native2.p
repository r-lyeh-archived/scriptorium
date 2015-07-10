const iterations = 10000000

native StrLen(s[])
forward @m(s[])

@m(s[])
{
    new length = 0, i
    for (i = 0; i < iterations; ++i)
        length = StrLen(s)


    printf("Length: %d\n", iterations * length)
}
