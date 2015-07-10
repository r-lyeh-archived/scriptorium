const iterations = 10000000;


local length = 0;
for (local i = 0; i < iterations; ++i)
    length = ::StrLen(::s);


print("Length: " + (iterations * length) + "\n");
