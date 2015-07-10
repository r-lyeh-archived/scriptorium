const iterations = 1000000000

native AddOne(n)

main()
{
    new count = 0, i
    for (i = 0; i < iterations; ++i)
        count = AddOne(count)


    printf("Count: %d\n", count)
}
