CC = gcc
CFLAGS = -std=c99 -Wall -Wextra -D_XOPEN_SOURCE -DDEBUG -g ${OPTIMIZE}
INCS = -Ivm -Ivendor/gc/include -Ivendor/pcre -Ivendor
LIBS = ${GC} ${PCRE}
GC = vendor/gc/.libs/libgc.a
PCRE = vendor/pcre/.libs/libpcre.a
LEG = vendor/peg/leg
FREEGETOPT = vendor/freegetopt/getopt.o

# Optimizations
ifndef DEV
OPTIMIZE = -O3 -funroll-loops -fomit-frame-pointer -fstrict-aliasing
endif

ifdef COMPAT
CFLAGS += -pedantic -DTR_COMPAT_MODE
endif

# Plateform specific libs
SYS = $(shell uname -s)
ifneq ($(SYS),Linux)
LIBS += ${FREEGETOPT}
endif

SRC = vm/string.c vm/number.c vm/range.c vm/regexp.c vm/primitive.c vm/proc.c vm/array.c vm/hash.c vm/class.c vm/error.c vm/kernel.c vm/object.c vm/block.c vm/compiler.c vm/grammar.c vm/vm.c vm/tr.c
OBJ = ${SRC:.c=.o}
OBJ_MIN = vm/tr.o

all: tinyrb

.c.o:
	@echo "   cc $<"
	@${CC} -c ${CFLAGS} ${INCS} -o $@ $<

tinyrb: ${LIBS} ${OBJ}
	@echo " link tinyrb"
	@${CC} ${CFLAGS} ${OBJ_POTION} ${OBJ} ${LIBS} ${PKG_LIBS} -o tinyrb

vm/grammar.c: ${LEG} vm/grammar.leg
	@echo "  leg vm/grammar.leg"
	@${LEG} -ovm/grammar.c vm/grammar.leg

vm/vm.o: vm/call.h

${FREEGETOPT}:

${LEG}:
	@echo " make peg/leg"
	@cd vendor/peg && make -s

${GC}:
	@echo " make gc"
	@cd vendor/gc && ./configure --disable-threads -q && make -s

${PCRE}:
	@echo " make pcre"
	@cd vendor/pcre && ./configure -q && make -s

test: tinyrb
	@ruby test/runner

sloc: clean
	@cp vm/grammar.leg vm/grammar.leg.c
	@sloccount vm lib
	@rm vm/grammar.leg.c

size: clean
	@ruby -e 'puts "%0.2fK" % (Dir["vm/*.{c,leg,h}"].inject(0) {|s,f| s += File.size(f)} / 1024.0)'

clean:
	$(RM) vm/*.o vm/grammar.c

rebuild: clean tinyrb

site:
	scp site/* macournoyer@code.macournoyer.com:code.macournoyer.com/tinyrb

.PHONY: all sloc size clean rebuild test site
