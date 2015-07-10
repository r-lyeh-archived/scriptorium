enum {
    // this behaviour is confusing, but I like that
    // we support it.
    __ = (__ - 1),
    A  = (__ + 1),

    B,
    C
};

enum {
    D = C + B,
    E = C + C,
    F = C + D,
};

enum {
    G = (B + F), H = (C + F),
    I = (D + F), J = (B + I)
};
enum {
    K = A + B - C + D - E + F *
        G - H + I - J + A - B -
        J + A,
    L,
    M,
    N
};

enum : flag {
    F1, /* = 1 << 1 */
    F2, /* = 1 << 2 */
    F3  /* = 1 << 3 */
};

/* reversed enumeration */
enum : reverse {
    R1, // 3
    R2, // 2
    R3, // 1
    R4  // 0
};

void main() {
    print(ftos(A), "\n");
    print(ftos(B), "\n");
    print(ftos(C), "\n");
    print(ftos(D), "\n");
    print(ftos(E), "\n");
    print(ftos(F), "\n");
    print(ftos(G), "\n");
    print(ftos(H), "\n");
    print(ftos(I), "\n");
    print(ftos(J), "\n");
    print(ftos(K), "\n");
    print(ftos(L), "\n");
    print(ftos(M), "\n");
    print(ftos(N), "\n");

    print(ftos(F1), "\n");
    print(ftos(F2), "\n");
    print(ftos(F3), "\n");

    print(ftos(R1), "\n");
    print(ftos(R2), "\n");
    print(ftos(R3), "\n");
    print(ftos(R4), "\n");
};
