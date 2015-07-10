vector swap(float x, float y) {
    vector ret = '0 0 0';
    // everyone knows this trick
    ret.x = x;
    ret.y = y;

    ret.x = ret.x ^ ret.y;
    ret.y = ret.y ^ ret.x;
    ret.x = ret.x ^ ret.y;

    return ret;
}

float f(vector b) {
    return b.x+b.y+b.z;
}

void main() {
    float x = 5;
    float y = 3;
    float z = x ^ y; // 6

    float a = 2;
    float b = 10;
    float c = a ^ b; // 8

    print(ftos(z), "\n");
    print(ftos(c), "\n");

    // commutative?
    if (x ^ y == y ^ x)
        print("commutative\n");

    // assocative?
    if (x ^ (y ^ z) == (x ^ y) ^ z)
        print("assocative\n");

    // elements are their own inverse?
    if (x ^ 0 == x)
        print("inverse\n");

    // vector ^ vector
    // vector ^ float
    // are legal in constant expressions (currently)
    vector v1 = '5 2 5';
    vector v2 = '3 10 3';

    print("vv: ", vtos(v1 ^ v2), "\n");
    print("vf: ", vtos(v1 ^ 10), "\n");

    const vector v3 = '5 2 5' ^ '3 10 3';
    const vector v4 = '5 2 5' ^ 10;

    print("vv: ", vtos(v3), "\n");
    print("vf: ", vtos(v4), "\n");

    // good olde xor swap test too
    float swap_x = 100;
    float swap_y = 200;
    vector swaps = swap(swap_x, swap_y);
    print("100:200 swapped is: ", ftos(swaps.x), ":", ftos(swaps.y), "\n");

    // good olde xor swap test too
    vector swap_u = '1 2 3';
    vector swap_v = '4 5 6';
    swap_u ^= swap_v;
    swap_v ^= swap_u;
    swap_u ^= swap_v;
    print("'1 2 3':'4 5 6' swapped is: ", vtos(swap_u), ":", vtos(swap_v), "\n");

    // the one that showed us overlap bugs
    print(vtos('1 2 3' ^ f('3 2 1') ^ f('1 1 1')), "\n");
    print(vtos('1 2 3' ^ f('3 2 1') ^ 3),          "\n");
    print(vtos('1 2 3' ^ 6          ^ 3),          "\n");
    print(vtos('1 2 3' ^ 6          ^ f('1 1 1')), "\n");
    print(vtos('1 2 3' ^ 5),                       "\n");
}
