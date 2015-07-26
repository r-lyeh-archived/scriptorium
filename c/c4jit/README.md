c4 - C in four functions
========================

An exercise in minimalism.

Try the following:

    gcc -o c4 c4.c  (you may need the -m32 option on 64bit machines)
    ./c4 hello.c
    ./c4 -s hello.c
    
    ./c4 c4.c hello.c
    ./c4 c4.c c4.c hello.c


c4x86 - JIT compiler for x86 in 86 lines
========================================

An exercise in bit-twiddling masochism.

x86 only, not self-hosted!

    gcc -m32 c4x86.c -o c4x86
    ./c4x86 hello.c
    ./c4x86 c4.c hello.c
