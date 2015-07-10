float  glob[7];

.float above;
.float flds[6];
.float below;

void main() {
    float loc[6];

    loc[0] = 1000;
    loc[1] = 1100;
    loc[2] = 1200;
    loc[3] = 1300;
    loc[4] = 1400;
    loc[5] = 1500;

    float i;

    for (i = 0; i < 6; i += 1)
        loc[i] += 1;
    for (i = 0; i < 5; i += 1)
        print(ftos(loc[i]), " ");
    print(ftos(loc[i]), "\n");

    glob[0] = 1000;
    glob[1] = 1100;
    glob[2] = 1200;
    glob[3] = 1300;
    glob[4] = 1400;
    glob[5] = 1500;
    glob[6] = 1600;
    for (i = 0; i < 7; i += 1)
        glob[i] += 1;
    for (i = 0; i < 6; i += 1)
        print(ftos(glob[i]), " ");
    print(ftos(glob[i]), "\n");

    entity e = spawn();
    e.above = 7777;
    e.below = 9999;
    e.flds[0] = 1000;
    e.flds[1] = 1100;
    e.flds[2] = 1200;
    e.flds[3] = 1300;
    e.flds[4] = 1400;
    e.flds[5] = 1500;
    for (i = 0; i < 6; i += 1)
        e.flds[i] += 1;
    for (i = 0; i < 5; i += 1)
        print(ftos(e.flds[i]), " ");
    print(ftos(e.flds[i]), "\n");
}
