float glob1[7] = { 10, 20, 30, 40, 50, 60, 70 };
float glob2[7] = { 100, 200, 300, 400, 500, 600 };
string globs[] = { "Hello ", "World" };

void main() {
    float i;
    print(ftos(glob1[0]));
    for (i = 1; i != 7; ++i)
        print(" ", ftos(glob1[i]));
    print("\n");

    print(ftos(glob2[0]));
    for (i = 1; i != 7; ++i)
        print(" ", ftos(glob2[i]));
    print("\n");

    print(globs[0], globs[1], "\n");
}
