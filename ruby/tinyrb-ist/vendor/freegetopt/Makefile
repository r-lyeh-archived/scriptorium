##############################################################################
# $Header: /cvsroot/freegetopt/freegetopt/Makefile,v 1.2 2003/10/26 03:10:19 vindaci Exp $

CC=gcc
OBJECTS=getopt.o
BINARIES=test


##############################################################################
# PHONIES

all: test

clean:
	rm -f $(BINARIES) *.o


##############################################################################
# FILES

test: test.o $(OBJECTS)
	$(CC) $(LDFLAGS) test.o $(OBJECTS) -o $@ $(LDLIBS)

test.o: test.c
	$(CC) $(CFLAGS) $< -o $@ -c

getopt.o: getopt.c
	$(CC) $(CFLAGS) $< -o $@ -c

