const int iterations = 1000000000;

void main()
{
    int count = 0, i;
    for (i = 0; i < iterations; ++i)
        count = AddOne(count);

    PrintString("Count: ");
    PrintInt(count);
    PrintString("\n");
}
