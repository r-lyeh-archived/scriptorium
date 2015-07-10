float f_float() {
    return = 100.0f;
    return = 200.0f;
    return;
}

vector f_vector() {
    vector foo;
    foo.x = f_float();
    foo.y = f_float();
    foo.z = f_float();

    return = foo;
    return;
}

string f_string() {
#ifndef FAIL_TEST
    return = "hello";
    return = "world";
#endif
    return;
}

float factorial(float n) {
    if (n == 0) return = 1;
    else        return = n * factorial(n - 1);
}

void main() {
    print(ftos(f_float()), "\n");  // 200.0f
    print(vtos(f_vector()), "\n"); // '1 2 3'
    print(f_string(), "\n");       // world
    print(ftos(factorial(4)), "\n"); // 24
}
