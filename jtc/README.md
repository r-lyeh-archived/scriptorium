# jtc

A toy programming language interpreter

## building

the jtc interpreter has no dependencies and sits in a single C++ file.
C++11 capable compilers should compile it without extra options other
than enabling C++11.

example: `g++ -o jtc jtc.cpp -std=c++0x -O3`
