// getter to work around future -O
vector get(vector v) {
    return v;
}

void test(vector in) {
    vector v = get(in);
    vector b = v * v_x;
    print(vtos(b), "\n");
}

void main() {
    test('20 40 80');
}
