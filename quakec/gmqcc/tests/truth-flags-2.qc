float test_s_not  (string s)           { return !s; }
float test_s_and  (string s, string t) { return s && t; }
float test_s_or   (string s, string t) { return s || t; }
float test_s_if   (string s)           { if (s) return 1; return 0; }
float test_s_ifnot(string s)           { if not (s) return 1; return 0; }

void test(string s, string t) {
    print(ftos(!!test_s_not  (s)), " ");
    print(ftos(!!test_s_and  (s, t)), " ");
    print(ftos(!!test_s_or   (s, t)), " ");
    print(ftos(!!test_s_if   (s)), " ");
    print(ftos(!!test_s_ifnot(s)), "\n");
}

string nuls;
void main() {
    print("                ! & | i N\n");
    print("'str', 'str' -> "); test("FULL", "FULL");
    print("'str', ''    -> "); test("FULL", ""    );
    print("'str', 0     -> "); test("FULL", nuls  );
    print("'',    'str' -> "); test("",     "FULL");
    print("'',    ''    -> "); test("",     ""    );
    print("'',    0     -> "); test("",     nuls  );
    print("0,     'str' -> "); test(nuls,   "FULL");
    print("0,     ''    -> "); test(nuls,   ""    );
    print("0,     0     -> "); test(nuls,   nuls  );
}
