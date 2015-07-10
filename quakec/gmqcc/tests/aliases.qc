float alias_1 = 3.14;
void  alias_2() {
    print("alias_2\n");
}

[[alias("alias_2")]] void  alias_2_aliased();
[[alias("alias_1")]] float alias_1_aliased;


// alias to an alias?
vector alias_3;
[[alias("alias_3")]] vector alias_3_aliased;

// expected output
// alias_2
// 3.14
void main() {
    alias_2_aliased();

    alias_3_aliased= '1 2 3';

    print(
        ftos(
            alias_1_aliased
        ),
        "\n"
    );

    print(
        "x ", ftos(alias_3_aliased_x), "\n",
        "y ", ftos(alias_3_aliased_y), "\n",
        "z ", ftos(alias_3_aliased_z), "\n"
    );
}
