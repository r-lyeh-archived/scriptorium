void main() {
    vector v;

    //v_x = 0;
    //v_y = 0;
    //v_z = 0;
#ifdef INIT_BY_VECTOR
    v = '3 4 5';
#elifdef INIT_BY_COMPONENTS
    v_x = 3;
    v_y = 4;
    v_z = 5;
#elifdef INIT_PARTS
    v_z = 5;
#endif

    print(ftos(v_y), "\n");
    print(ftos(v_x), "\n");
    print(ftos(v_z), "\n");
    print(vtos(v), "\n");
}
