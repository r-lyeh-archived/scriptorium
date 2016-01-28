#
# Makefile for lil. It requires GNU Make.
#

PREFIX = /usr/local
LIL_PROGRAM = lil
LIL_LIBRARY = liblil.a
BININSTALL = $(PREFIX)/bin
MANINSTALL = $(PREFIX)/man/man1
CFLAGS ?= -g3 -std=c99 -pedantic -Wall -Wextra -Wno-long-long -Wno-unused-parameter
LDFLAGS ?= -g -L.

LIL_SOURCES = lil.c
LIL_PROGRAM_SOURCES = main.c

LIL_OBJECTS = $(patsubst %.c,%.o,$(LIL_SOURCES))
LIL_PROGRAM_OBJECTS = $(patsubst %.c,%.o,$(LIL_PROGRAM_SOURCES))

HEADERS = $(wildcard *.h)

.PHONY: all
all: $(LIL_LIBRARY) $(LIL_PROGRAM)

$(LIL_LIBRARY): $(LIL_OBJECTS)
	$(AR) rcs $(LIL_LIBRARY) $(LIL_OBJECTS)

$(LIL_PROGRAM): $(LIL_PROGRAM_OBJECTS) $(LIL_LIBRARY)
	$(CC) $(LDFLAGS) -o $(LIL_PROGRAM) $(LIL_PROGRAM_OBJECTS) -llil -lm

$(LIL_PROGRAM_OBJECTS) $(LIL_OBJECTS): %.o: %.c $(HEADERS)
	$(CC) -c $(CFLAGS) $< -o $@

.PHONY: clean
clean:
	$(RM) -f $(LIL_PROGRAM_OBJECTS) $(LIL_OBJECTS)
	$(RM) -f $(LIL_LIBRARY) $(LIL_PROGRAM) $(LILCGI_PROGRAM)
