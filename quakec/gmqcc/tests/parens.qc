float arr[2];

string gets() { return "S\n"; }
void main(float x) {
    string s;

    s = gets();                // 0 params
    print(s);                  // 1 param
    print("A ", "B\n");        // 2 params
    print("A ", "B ", "C\n");  // more params
    print(gets());             // 0-param call in call
    print(gets(), "next\n");   // 0-param call and another
    print("-> ", gets());      // param + 0-param call
    print(ftos(x), "\n");      // param-call + another
    print(x ? "xA\n" : "xB\n");      // ternary in PAREN_FUNC
    print(!x ? "xA\n" : "xB\n");      // ternary in PAREN_FUNC
    // PAREN_INDEX
    arr[0] = 10;
    arr[1] = 11;
    // PAREN_TERNARY + PAREN_INDEX
    arr[x ? 0 : 1] += 100;
    print(ftos(arr[0]), "\n");
    print(ftos(arr[1]), "\n");
    print(ftos(arr[x ? 0 : 1]), "\n");
    print(ftos(arr[!x ? 0 : 1]), "\n");

    // loops with comma operators
    float i, j;
    for (i = 0, j = 0; i < x; ++i)
        print("-");
    print("\n");

    // if + PAREN_TERNARY2
    if (x ? 1 : 0)
        print("OK\n");
    if (x ? 0 : 1)
        print("NO\n");

    // PAREN_FUNC in PAREN_EXPR
    print(("Is this wrong ", "now?\n"));
}
