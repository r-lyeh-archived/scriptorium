include include.mk

UNAME  ?= $(shell uname)
CYGWIN  = $(findstring CYGWIN, $(UNAME))
MINGW   = $(findstring MINGW,  $(UNAME))

# turn on tons of warnings if clang is present
# but also turn off the STUPID ONES
ifeq ($(CC), clang)
	CFLAGS +=                              \
	    -Weverything                       \
	    -Wno-padded                        \
	    -Wno-format-nonliteral             \
	    -Wno-disabled-macro-expansion      \
	    -Wno-conversion                    \
	    -Wno-float-equal                   \
	    -Wno-unknown-warning-option        \
	    -Wno-cast-align                    \
	    -Wno-assign-enum                   \
	    -Wno-empty-body                    \
	    -Wno-date-time                     \
	    -pedantic-errors
else
	ifneq ($(CC), g++)
		CFLAGS += -Wmissing-prototypes -Wstrict-prototypes
	endif

	ifneq ($(CC), tcc)
		CFLAGS += -pedantic-errors
	else
		CFLAGS += -Wno-pointer-sign -fno-common
	endif
endif

ifneq ($(shell git describe --always 2>/dev/null),)
	CFLAGS += -DGMQCC_GITINFO="\"$(shell git describe --always)\""
endif

ifeq ($(shell valgrind --version 2>/dev/null),)
	CFLAGS += -DNVALGRIND
endif

# do this last otherwise there is whitespace in the command output and
# it makes my OCD act up
CFLAGS += $(OPTIONAL_CFLAGS)
LDFLAGS += $(OPTIONAL_LDFLAGS)

#we have duplicate object files when dealing with creating a simple list
#for dependinces. To combat this we use some clever recrusive-make to
#filter the list and remove duplicates which we use for make depend
RMDUP = $(if $1,$(firstword $1) $(call RMDUP,$(filter-out $(firstword $1),$1)))
DEPS := $(call RMDUP, $(OBJ_P) $(OBJ_T) $(OBJ_C) $(OBJ_X))

ifneq ("$(CYGWIN)", "")
	#nullify the common variables that
	#most *nix systems have (for windows)
	PREFIX   :=
	BINDIR   :=
	DATADIR  :=
	MANDIR   :=
	QCVM      = qcvm.exe
	GMQCC     = gmqcc.exe
	TESTSUITE = testsuite.exe
	PAK       = gmqpak.exe
	CFLAGS   += -DNVALGRIND
else
ifneq ("$(MINGW)", "")
	#nullify the common variables that
	#most *nix systems have (for windows)
	PREFIX   :=
	BINDIR   :=
	DATADIR  :=
	MANDIR   :=
	QCVM      = qcvm.exe
	GMQCC     = gmqcc.exe
	TESTSUITE = testsuite.exe
	PAK       = gmqpak.exe
	CFLAGS   += -DNVALGRIND
else
	QCVM      = qcvm
	GMQCC     = gmqcc
	TESTSUITE = testsuite
	PAK       = gmqpak
endif
endif

#standard rules
c.o:
	$(CC) -c $< -o $@ $(CFLAGS) $(CPPFLAGS)

$(QCVM): $(OBJ_X)
	$(CC) -o $@ $^ $(LDFLAGS) $(LIBS)

$(GMQCC): $(OBJ_C) $(OBJ_D)
	$(CC) -o $@ $^ $(LDFLAGS) $(LIBS)

$(TESTSUITE): $(OBJ_T)
	$(CC) -o $@ $^ $(LDFLAGS) $(LIBS)

$(PAK): $(OBJ_P)
	$(CC) -o $@ $^ $(LDFLAGS)

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
	@ makedepend -Y -w 65536 2> /dev/null $(subst .o,.c,$(DEPS))


coverity:
	@cov-build --dir cov-int $(MAKE)
	@tar czf gm-qcc.tgz cov-int
	@rm -rf cov-int
	@echo gm-qcc.tgz generated, submit for analysis

#install rules
install: install-gmqcc install-qcvm install-gmqpak install-doc
install-gmqcc: $(GMQCC)
	install -d -m755               $(DESTDIR)$(BINDIR)
	install    -m755  $(GMQCC)     $(DESTDIR)$(BINDIR)/$(GMQCC)
install-qcvm: $(QCVM)
	install -d -m755               $(DESTDIR)$(BINDIR)
	install    -m755  $(QCVM)      $(DESTDIR)$(BINDIR)/$(QCVM)
install-gmqpak: $(PAK)
	install -d -m755               $(DESTDIR)$(BINDIR)
	install    -m755  $(PAK)       $(DESTDIR)$(BINDIR)/$(PAK)
install-doc:
	install -d -m755               $(DESTDIR)$(MANDIR)/man1
	install    -m644  doc/gmqcc.1  $(DESTDIR)$(MANDIR)/man1/
	install    -m644  doc/qcvm.1   $(DESTDIR)$(MANDIR)/man1/
	install    -m644  doc/gmqpak.1 $(DESTDIR)$(MANDIR)/man1/

# DO NOT DELETE

ansi.o: platform.h gmqcc.h opts.def
util.o: gmqcc.h opts.def platform.h
hash.o: gmqcc.h opts.def
stat.o: gmqcc.h opts.def
fs.o: gmqcc.h opts.def platform.h
opts.o: gmqcc.h opts.def
conout.o: gmqcc.h opts.def
pak.o: gmqcc.h opts.def
test.o: gmqcc.h opts.def platform.h
main.o: gmqcc.h opts.def lexer.h
lexer.o: gmqcc.h opts.def lexer.h
parser.o: parser.h gmqcc.h opts.def lexer.h ast.h ir.h
code.o: gmqcc.h opts.def
ast.o: gmqcc.h opts.def ast.h ir.h parser.h lexer.h
ir.o: gmqcc.h opts.def ir.h
ftepp.o: gmqcc.h opts.def lexer.h
utf8.o: gmqcc.h opts.def
correct.o: gmqcc.h opts.def
fold.o: ast.h ir.h gmqcc.h opts.def parser.h lexer.h
intrin.o: parser.h gmqcc.h opts.def lexer.h ast.h ir.h
exec.o: gmqcc.h opts.def
