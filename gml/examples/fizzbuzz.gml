m = [ :nil, "Fizz", "Buzz", "FizzBuzz" ];
a = 810092048;

for i in range(1, 101) {
    c = a & 3;
    if c => println(m[c]);
    else => println(i);
    a = a >> 2 | c << 28;
}
