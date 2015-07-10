#
# This is the Makefile for the BSD flavor
#
.include "include.mk"

GITTEST  != git describe --always 2>/dev/null
VALTEST  != valgrind --version 2>/dev/null
GITINFO  :=

.if $(GITTEST)
    GITINFO != git describe --always
.endif

.if $(CC) == clang
    CFLAGS +=   -Weverything\
                -Wno-padded\
                -Wno-format-nonliteral\
                -Wno-disabled-macro-expansion\
                -Wno-conversion\
                -Wno-float-equal\
                -Wno-unknown-warning-option\
                -Wno-cast-align\
                -Wno-assign-enum\
                -Wno-empty-body\
                -Wno-date-time\
                -pedantic-errors
.else
.    if $(CC) != g++
       CFLAGS += -Wmissing-prototypes -Wstrict-prototypes
.    endif

.    if $(CC) != tcc
       CFLAGS += -pedantic-errors
.    else
       CFLAGS += -Wno-pointer-sign -fno-common
.    endif
.endif

.if !$(VALTEST)
    CFLAGS += -DNVALGRIND
.endif


CFLAGS += -DGMQCC_GITINFO=\"$(GITINFO)\" $(OPTIONAL)
DEPS != for i in $(OBJ_C) $(OBJ_P) $(OBJ_T) $(OBJ_X); do echo $$i; done | sort | uniq

QCVM      = qcvm
GMQCC     = gmqcc
TESTSUITE = testsuite
PAK       = gmqpak

#standard rules
c.o: ${.IMPSRC}
	$(CC) -c ${.IMPSRC} -o ${.TARGET} $(CFLAGS) $(CPPFLAGS)

$(QCVM): $(OBJ_X)
	$(CC) -o ${.TARGET} ${.IMPSRC} $(LDFLAGS) $(LIBS) $(OBJ_X)

$(GMQCC): $(OBJ_C)
	$(CC) -o ${.TARGET} ${.IMPSRC} $(LDFLAGS) $(LIBS) $(OBJ_C)

$(TESTSUITE): $(OBJ_T)
	$(CC) -o ${.TARGET} ${.IMPSRC} $(LDFLAGS) $(LIBS) $(OBJ_T)

$(PAK): $(OBJ_P)
	$(CC) -o ${.TARGET} ${.IMPSRC} $(LDFLAGS) $(OBJ_P)

all: $(GMQCC) $(QCVM) $(TESTSUITE) $(PAK)

check: all
	@ ./$(TESTSUITE)
test: all
	@ ./$(TESTSUITE)

strip: $(GMQCC) $(QCVM) $(TESTSUITE)
	strip $(GMQCC)
	strip $(QCVM)
	strip $(TESTSUITE)

clean:
	rm -rf *.o $(GMQCC) $(QCVM) $(TESTSUITE) $(PAK) *.dat gource.mp4 *.exe gm-qcc.tgz ./cov-int

depend:
	@makedepend -Y -f BSDmakefile -w 65536 2> /dev/null ${DEPS:C/\.o/.c/g}

coverity:
	@cov-build --dir cov-int $(MAKE) -f BSDmakefile
	@tar czf gm-qcc.tgz cov-int
	@rm -rf cov-int
	@echo gm-qcc.tgz generated, submit for analysis

install: install-gmqcc install-qcvm install-gmqpak install-doc
install-gmqcc: $(GMQCC)
	install -d -m755              $(DESTDIR)$(BINDIR)
	install    -m755 $(GMQCC)     $(DESTDIR)$(BINDIR)/$(GMQCC)
install-qcvm: $(QCVM)
	install -d -m755              $(DESTDIR)$(BINDIR)
	install    -m755 $(QCVM)      $(DESTDIR)$(BINDIR)/$(QCVM)
install-gmqpak: $(PAK)
	install -d -m755              $(DESTDIR)$(BINDIR)
	install    -m755 $(PAK)       $(DESTDIR)$(BINDIR)/$(PAK)
install-doc:
	install -d -m755              $(DESTDIR)$(MANDIR)/man1
	install    -m644 doc/gmqcc.1  $(DESTDIR)$(MANDIR)/man1/
	install    -m644 doc/qcvm.1   $(DESTDIR)$(MANDIR)/man1/
	install    -m644 doc/gmqpak.1 $(DESTDIR)$(MANDIR)/man1/

# DO NOT DELETE

ansi.o: platform.h gmqcc.h opts.def
ast.o: gmqcc.h opts.def ast.h ir.h parser.h lexer.h
code.o: gmqcc.h opts.def
conout.o: gmqcc.h opts.def
correct.o: gmqcc.h opts.def
exec.o: gmqcc.h opts.def
fold.o: ast.h ir.h gmqcc.h opts.def parser.h lexer.h
fs.o: gmqcc.h opts.def platform.h
ftepp.o: gmqcc.h opts.def lexer.h
hash.o: gmqcc.h opts.def
intrin.o: parser.h gmqcc.h opts.def lexer.h ast.h ir.h
ir.o: gmqcc.h opts.def ir.h
lexer.o: gmqcc.h opts.def lexer.h
main.o: gmqcc.h opts.def lexer.h
opts.o: gmqcc.h opts.def
pak.o: gmqcc.h opts.def
parser.o: parser.h gmqcc.h opts.def lexer.h ast.h ir.h
stat.o: gmqcc.h opts.def
test.o: gmqcc.h opts.def platform.h
utf8.o: gmqcc.h opts.def
util.o: gmqcc.h opts.def platform.h
