string() getter = {
    return "correct";
};

void(string() f) printer = {
    print(f(), "\n");
};

void() main = {
    printer(getter);
};
