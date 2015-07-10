const string a = "hello world"; // 11
float b[] = { 1, 2, 3 };        // 3
float c[5] = { 5, 4, 3, 2, 1 }; // 5
const float d[] = { 1 };        // 1

void main() {
    print(ftos(_length a), "\n"); // 11
    print(ftos(_length b), "\n"); // 3
    print(ftos(_length c), "\n"); // 5
    print(ftos(_length d), "\n"); // 1

    static float al = _length(a);
    static float bl = _length(b);
    static float cl = _length(c);
    static float dl = _length(d);

    print(ftos(al), "\n"); // 11
    print(ftos(bl), "\n"); // 3
    print(ftos(cl), "\n"); // 5
    print(ftos(dl), "\n"); // 1

    print(ftos(_length "hello world"), "\n"); // 11
}
