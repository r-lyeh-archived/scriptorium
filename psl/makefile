
CC     = g++
CFLAGS = -m32

all: PSL.out PSLc.out sample/brainfuck.out
	./PSL.out sample/helloworld.psl

PSL.out: PSL.cpp
	$(CC) $(CFLAGS) -o $@ $<

PSLc.out: PSLc.cpp
	$(CC) $(CFLAGS) -o $@ $<

sample/brainfuck.out: PSLc.out sample/brainfuck.psl
	./PSLc.out sample/brainfuck.psl
	chmod 764 sample/brainfuck.out
	sample/brainfuck.out sample/hw.bf
