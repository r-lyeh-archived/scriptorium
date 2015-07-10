void test(float brkat, float contat) {
    float i;

    for (i = 0; i < 10; i += 1) {
        if (i == contat) {
            print("ct ");
            continue;
        }
        print(ftos(i), " ");
        if (i == brkat) {
            print("brk ");
            break;
        }
    }
    print("end\n");
}

void main() {
    test(-1, -1);
    test( 3, -1);
    test(-1,  3);
    test( 5,  2);
}
