void nbva(float a, string...count) {
    print("You gave me ", ftos(count), " additional parameters\n");
    print("First: ", ...(0, string), "\n");
    print("You chose: ", ...(a, string), "\n");
    for (a = 0; a < count; ++a)
        print("Vararg ", ftos(a), " = ", ...(a, string), "\n");
}

var void unstable(...);
void stability(float a, float b, ...count)
{
    print("Got: ", ftos(count), "\n");
}

void main() {
    nbva(1, "Hello", "You", "There");
    stability(1, 2, 3, 4, 5);
    unstable = stability;
    unstable(1, 2, 3, 4, 5);
}
