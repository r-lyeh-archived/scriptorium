#  Makefile for miniscm
#
#  This defaults to using ANSI C on 4.3 BSD-flavoured UNIX (which is
#  compatible with many modern Unices, including Linux).  You may select a
#  different flavour of UNIX, or a pre-ANSI version of C, by telling make
#  to override the CC and/or CFLAGS variables.
#  Please see source and/or README for system definition #define's.
#
#  Examples:
#    CFLAGS = -g -DSYSV -traditional -traditional-cpp -Wid-clash-8
#    CFLAGS = -O -DSYSV

CC ?= gcc
CFLAGS ?= -O -ansi -pedantic -DBSD -DCMDLINE

all :	miniscm

miniscm : miniscm.c Makefile
	$(CC) $(CFLAGS) -o miniscm miniscm.c

clean :
	rm -f core *.o miniscm *~

