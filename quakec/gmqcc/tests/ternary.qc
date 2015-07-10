void test(float cond, float v1, float v2, float a) {
    print(ftos(cond ? v1 : v2), " ");
    print( (cond ? v1 : v2) ? ( (a == 1) ? "a=1"
                              : (a == 2) ? "a=2"
                              : "a=other"
                              )
                            : "not met",
          "\n");
}

void select_a(float x) {
    print("select_a: ", ftos(x), "\n");
}
void select_b(float x) {
    print("select_b: ", ftos(x), "\n");
}

void main() {
    float a, b;
    test(0, -99, 1, 1);
    test(0, -99, 1, 2);
    test(0, -99, 1, 3);
    test(0, -99, 0, 1);
    test(0, -99, 0, 2);
    test(0, -99, 0, 3);
    test(1, 1, -99, 1);
    test(1, 1, -99, 2);
    test(1, 1, -99, 3);
    test(1, 0, -99, 1);
    test(1, 0, -99, 2);
    test(1, 0, -99, 3);

    b = 5;
    a = b ? 5 : 6;
    print(ftos(a), "\n");
    b ? a = 9 : a = 10;
    print(ftos(a), "\n");
    !b ? a = 9 : a = 10;
    print(ftos(a), "\n");

    ((1) ? select_a : select_b) (1);
    ((0) ? select_a : select_b) (0);
}
