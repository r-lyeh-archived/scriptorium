void past8(float a, float b, float c, float d, ...count)
{
    float i;
    print("out:");
    for (i = 0; i < count; ++i)
        print(" ", ftos(...(i, float)), "");
    print("\n");
}

void main() {
    past8(1, 2, 3, 4, 10, 20, 30, 40, 50, 60, 70, 80);
}
