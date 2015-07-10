void p10(float a, float b, float c, float d, float e, float f, float g, float h,
         float e1, float e2)
{
    print(ftos(a),  " ", ftos(b), " ", ftos(c), " ");
    print(ftos(d),  " ", ftos(e), " ", ftos(f), " ");
    print(ftos(g),  " ", ftos(h), " ");
    print(ftos(e1), " ", ftos(e2), "\n");
}

void overwrite(float a, float b, float c, float d, float e, float f, float g, float h,
               float e1, float e2)
{
    // call >8 param functions within a >8 param function and see if the locals
    // are actual copies
    p10(a, b, c, d, e, f, g, h, e1, e2);
    p10(1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
    p10(a, b, c, d, e, f, g, h, e1, e2);
}

void main() {
    overwrite(10, 20, 30, 40, 50, 60, 70, 80, 90, 100);
}
