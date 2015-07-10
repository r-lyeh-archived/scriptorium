vector main(float a, vector vin) {
    vector v;

    if (a < 4) {
        v = vin;
        v_x += 1;
        v_y += 1;
        v_z += 1;
        return v;
    }
    else if (a < 5) {
        v_x = 3;
        v_y = 3;
#ifdef UNINIT
        print(vtos(v), "\n");
#endif
        v_z = 3;
        return v;
    }
    return '0 0 0';
}
