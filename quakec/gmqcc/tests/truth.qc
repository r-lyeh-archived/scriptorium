void test(string s) {
    print(ftos(!s));
    if (s)   print(" on");
    if (!s)  print(" off");
    if (!!s) print(" !!on");
    print("\n");
}

void test2(string s) {
    print(ftos(!s));
    while (s)   { print(" on");   break; }
    while (!s)  { print(" off");  break; }
    while (!!s) { print(" !!on"); break; }
    print("\n");
}

string str_nul;

void main(vector v) {
    test("foo");
    test2("foo");
    test("");
    test2("");
    test(str_nul);
    test2(str_nul);
}
