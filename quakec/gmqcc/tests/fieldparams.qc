.string a;
.string b;
..string ps;

void(entity e, .string s) callout = {
    print(e.s, "\n");
};

void() main = {
    local entity e;
    e = spawn();
    e.a = "foo";
    e.b = "bar";
    callout(e, b);
    e.ps = a;
    print(e.(e.ps), "\n");
};
