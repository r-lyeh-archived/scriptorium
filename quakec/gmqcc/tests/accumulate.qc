#define ACCUMULATE_FUNCTION(FUNC) \
    [[accumulate]] void FUNC ()

ACCUMULATE_FUNCTION(foo) {
    print("hello ");
}

ACCUMULATE_FUNCTION(foo) {
    print("accumulation ");
}

ACCUMULATE_FUNCTION(foo) {
    print("world\n");
}

void main() {
    foo();
}
