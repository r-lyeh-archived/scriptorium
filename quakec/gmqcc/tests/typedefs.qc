typedef void(...)       ptype;
typedef string(float a) funcsf;

ptype print = #1;
funcsf ftos = #2;

void main() {
    typedef float funcsf;
    funcsf a;
    a = 0;
    print("A typedeffed function, 0=", ftos(a), "\n");
}
