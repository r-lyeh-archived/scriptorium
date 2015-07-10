void test(vector a, vector b) {
    print(ftos((a && b) + (a && b)), " ");
    print(ftos((a || b) + (a || b)), " ");
    print(ftos((a && b) + (a || b)), "\n");
}

void main() {
    print("X               & | B\n");
    print("0 0 0, 0 0 0 :: "); test('0 0 0', '0 0 0');
    print("0 0 0, 5 0 0 :: "); test('0 0 0', '5 0 0');
    print("5 0 0, 0 0 0 :: "); test('5 0 0', '0 0 0');
    print("5 0 0, 5 0 0 :: "); test('5 0 0', '5 0 0');
    print("Y               & | B\n");
    print("0 0 0, 0 0 0 :: "); test('0 0 0', '0 0 0');
    print("0 0 0, 0 5 0 :: "); test('0 0 0', '0 5 0');
    print("0 5 0, 0 0 0 :: "); test('0 5 0', '0 0 0');
    print("0 5 0, 0 5 0 :: "); test('0 5 0', '0 5 0');
}
