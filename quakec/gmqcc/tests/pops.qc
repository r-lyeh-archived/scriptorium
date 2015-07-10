void main() {
    /* so far only one perl operator is implemented */
    float x = 100;
    float y = 200;
    float z = 300;

    /* to ensure runtime */
    x += 1;
    y += 1;
    z += 1;

    float test_x = (x <=> x + 1); // -1 less than
    float test_y = (x <=> x);     //  0 equal
    float test_z = (x <=> x - 1); //  1 greater than

    print(ftos(test_x), "\n");
    print(ftos(test_y), "\n");
    print(ftos(test_z), "\n");
}
