float(float x, float y, float z) sum = {
    return x + y + z;
};

void(float a, float b, float c) main = {
    local float f;
    f = sum(sum(a, sum(a, b, c), c),
            sum(sum(sum(a, b, c), b, sum(a, b, c)), b, sum(a, b, sum(a, b, c))),
            sum(sum(a, b, c), b, c));
    print(ftos(f), "\n");

};
