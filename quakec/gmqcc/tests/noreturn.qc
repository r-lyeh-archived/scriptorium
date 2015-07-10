#ifndef NORETURN
#define NORETURN [[noreturn]]
#endif

void   (...)            print     = #1;
string (float val)      ftos      = #2;

NORETURN void error(...)  = #6;

#if TEST == 1
void test1(float a) {
    float x;

    if (a == 1)       x = 1337;
    else if (a == 2)  x = 1338;
    else
        error("Error\n");
    print("Is this initialized: ", ftos(x), "\n");
}

#else

float test2(float it) {
    switch (it) {
        case 1: return 0;
        case 2: return 1;
        default: error("End of non-void not reachable...\n");
    }
}
#endif
