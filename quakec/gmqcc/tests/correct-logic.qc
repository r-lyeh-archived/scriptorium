float test_s_not  (vector s)           { return !s; }
float test_s_and  (vector s, vector t) { return s && t; }
float test_s_or   (vector s, vector t) { return s || t; }
float test_s_if   (vector s)           { if (s) return 1; return 0; }
float test_s_ifnot(vector s)           { if not (s) return 1; return 0; }

void test(vector s, vector t) {
    print(ftos(!!test_s_not  (s)), " ");
    print(ftos(!!test_s_and  (s, t)), " ");
    print(ftos(!!test_s_or   (s, t)), " ");
    print(ftos(!!test_s_if   (s)), " ");
    print(ftos(!!test_s_ifnot(s)), "\n");
}

void main() {
    print("        ! & | i N\n");
    print("0, 0 -> "); test('0 0 0', '0 0 0');
    print("0, x -> "); test('0 0 0', '1 0 0');
    print("x, 0 -> "); test('1 0 0', '0 0 0');
    print("x, x -> "); test('1 0 0', '1 0 0');
    print("0, y -> "); test('0 0 0', '0 1 0');
    print("y, 0 -> "); test('0 1 0', '0 0 0');
    print("y, y -> "); test('0 1 0', '0 1 0');
}
