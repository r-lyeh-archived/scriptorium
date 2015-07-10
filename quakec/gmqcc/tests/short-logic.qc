float glob1;
float glob2;
float glob3;

float side_effect_1(float r) {
    glob1 += 3;
    return r;
}

float side_effect_2(float r) {
    glob2 += 3;
    return r;
}

float side_effect_3(float r) {
    glob3 += 3;
    return r;
}

void main() {
    glob1 = 10;
    glob2 = 20;
    glob3 = 30;

    if (side_effect_1(0) || side_effect_2(1))
        print(ftos(glob1), "=13 ", ftos(glob2), "=23 OK\n");
    else
        print("Fail\n");

    if (side_effect_3(1) || side_effect_1(1))
        print(ftos(glob1), "=13 ", ftos(glob3), "=33 OK\n");
    else
        print("Fail\n");

    if (side_effect_1(0) && side_effect_3(1))
        print("Fail\n");
    else
        print(ftos(glob1), "=16 ", ftos(glob3), "=33 OK\n");

    if (side_effect_2(1) && side_effect_3(1))
        print(ftos(glob2), "=26 ", ftos(glob3), "=36 OK\n");
    else
        print("Fail\n");

    print(ftos(glob1), "=16 ", ftos(glob2), "=26 ", ftos(glob3), "=36 OK\n");
}
