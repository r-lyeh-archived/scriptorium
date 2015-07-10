void main() {
    float a; a = 1;
    float b; b = 1;
    float c; c = 1;
    float d; d = 1;
    vector e; e = '1 1 1';
    vector f; f = '1 1 1';

#ifdef __STD_FTEQCC__
    a &~= 1; // 0
#else
    a &= ~1; // 0
#endif
#ifdef __STD_GMQCC__
    b &= ~1; // 0
    c &= ~d; // 0
#else
    b &~= 1; // 0
    c &~= 1; // 0
#endif
#ifdef __STD_FTEQCC__
    f &~= e; // '0 0 0'
#else
    f &= ~e; // '0 0 0'
#endif
#ifdef __STD_GMQCC__
    e &= ~e; // '0 0 0'
#else
    e &~= e; // '0 0 0'
#endif

    print("a: ", ftos(a), "\nb: ",
                 ftos(b), "\nc: ",
                 ftos(c), "\n");
    print("e: ", vtos(e), "\n");
    print("f: ", vtos(f), "\n");
}
