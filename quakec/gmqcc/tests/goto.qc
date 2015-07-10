// correct execution order:
// label_3
// label_2
// label_4
// label_3
// label_1
// label_5
void main() {
    float x = 1;
    float y = 2;

    goto label_3;

    :label_1; print("label_1", "\n"); goto label_5;
    :label_2; print("label_2", "\n"); goto label_4;
    :label_3; print("label_3", "\n");

    // will goto label_2
    goto (x == y) ? label_1 : label_2;

    :label_4; print("label_4", "\n");
    {
        x = 1;
        y = 1;

        // will goto label_1
        // then goes label_5
        goto label_3;
    }

    :label_5; print("label_5", "\n");
}
