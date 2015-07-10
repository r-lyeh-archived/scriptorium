# Scheme 9 from Empty Space
# Makefile (obviously)
# By Nils M Holm, 2007-2015
# In the public domain

# Change at least this line:
PREFIX= /u

# Base version and Release
BASE=		20150612
RELEASE=	20150701

# Override default compiler and flags
# CC=	cc
CFLAGS=	-g -Wall -ansi -pedantic -O2

# Which OS are we using (unix or plan9)?
OSDEF=	-Dunix

# Uncomment these to include the Unix extensions
EXTRA_SCM+=	-l ext/unix.scm -l ext/unix-tools.scm
EXTRA_OBJS+=	unix.o
EXTRA_INIT+=	sys_init();
EXTRA_LIBS+=

# Uncomment these to include the Curses extensions
EXTRA_SCM+=	-l ext/curses.scm
EXTRA_OBJS+=	curses.o
EXTRA_INIT+=	curs_init();
EXTRA_LIBS+=	-lncurses

# Options to be added to $(DEFS)
#	-DBITS_PER_WORD_64	# use 64-bit bignum arithmetics
#	-DLIBRARY_PATH="\"dir:...\""
#				# search path for LOCATE-FILE, etc
#	-DNETWORK		# include socket code in the Unix extension
#	-DCURSES_COLOR		# enable the CURS:SET-COLOR primitive
#	-DCURSES_RESET		# automatically run CURS:ENDWIN on the REPL
#				# (requires the Curses extension)

DEFS=	$(OSDEF) \
	-DLIBRARY_PATH="\".:~/s9fes:$(S9DIR)\"" \
	-DEXTENSIONS="$(EXTRA_INIT)" \
	-DNETWORK \
	-DCURSES_COLOR \
	-DCURSES_RESET

# Where to install the stuff
S9DIR=	$(PREFIX)/share/s9fes
BINDIR=	$(PREFIX)/bin
INCDIR=	$(PREFIX)/include
LIBDIR=	$(PREFIX)/lib
MANDIR=	$(PREFIX)/man/man1

# Set up environment to be used during the build process
BUILD_ENV=	env S9FES_LIBRARY_PATH=.:lib:ext:contrib

SETPREFIX=	sed -e "s|^\#! /usr/local|\#! $(PREFIX)|"

default:	s9 s9.image s9.1.gz s9.1.txt s9core.a # s9core.pdf

all:	default

s9:	s9.o s9core.o s9core.h $(EXTRA_OBJS)
	$(CC) -o s9 $(LDFLAGS) s9.o s9core.o $(EXTRA_OBJS) $(EXTRA_LIBS)

s9.o:	s9.c s9core.h
	$(CC) -o s9.o $(CFLAGS) $(DEFS) -c s9.c

s9core.o:	s9core.c s9core.h
	$(CC) -o s9core.o $(CFLAGS) $(DEFS) -c s9core.c

s9.image:	s9 s9.scm ext/unix.scm ext/curses.scm config.scm
	$(BUILD_ENV) ./s9 -i - $(EXTRA_SCM) -l config.scm -d s9.image

s9core.a: s9core.o
	ar q s9core.a s9core.o

s9.1.gz:	s9.1
	sed -e "s,@S9DIR@,$(S9DIR)," <s9.1 |gzip -9 >s9.1.gz

unix.o:	ext/unix.c s9core.h
	$(CC) $(CFLAGS) $(DEFS) -I . -o unix.o -c ext/unix.c

curses.o:	ext/curses.c s9core.h
	$(CC) $(CFLAGS) $(DEFS) -I . -o curses.o -c ext/curses.c

s9core.ps:	s9core.tr util/book
	groff -e -p -t -Tps -P-p9i,6i s9core.tr >s9core.ps 2>_meta
	sed -nf util/mktoc.sed <_meta | sed -e 's/  *	/	/' >_toc.tr
	sed -ne 's/^R;\(.*\)/\1/p' <_meta >_xref.tr
	sed -e 's/\.nr \(.*\) \(.*\)/\\f\[CB]\1\\fP \\n\[\1\]/' <_xref.tr \
		| sort -f >_ndx.tr
	groff -e -p -t -Tps -P-p9i,6i s9core.tr >s9core.ps 2>/dev/null

s9core.pdf:	s9core.ps
	pdfwrite -p 432 648 s9core.ps

lint:
	cc -g -Wall -ansi -pedantic -O3 s9.c s9core.c && rm a.out

test:	s9 test.image
	$(BUILD_ENV) ./s9 -i test -f util/test.scm

libtest:	s9 test.image
	$(BUILD_ENV) sh util/libtest.sh

systest:	s9 test.image s9.image
	$(BUILD_ENV) ./s9 -i test -f util/systest.scm

srtest:	s9 test.image
	$(BUILD_ENV) ./s9 -i test -f util/srtest.scm

realtest:	s9 test.image
	$(BUILD_ENV) ./s9 -i test -f util/realtest.scm

test.image:	s9 s9.scm
	$(BUILD_ENV) ./s9 -i - $(EXTRA_SCM) -d test.image

tests: test realtest srtest libtest systest

install:	install-s9 install-util

install-all:	install-s9 install-util install-progs

# old version of install(1) may need -c
#C=-c
install-s9:	s9 s9.scm s9.image s9.1.gz
	install -d -m 0755 $(S9DIR)
	install -d -m 0755 $(S9DIR)/help
	install -d -m 0755 $(BINDIR)
	install -d -m 0755 $(LIBDIR)
	install -d -m 0755 $(INCDIR)
	install -d -m 0755 $(MANDIR)
	install $C -m 0755 s9 $(BINDIR)
	strip $(BINDIR)/s9
	install $C -m 0644 s9.scm $(S9DIR)
	install $C -m 0644 s9.image $(S9DIR)
	install $C -m 0644 lib/* $(S9DIR)
	install $C -m 0644 ext/*.scm $(S9DIR)
	install $C -m 0644 contrib/* $(S9DIR)
	install $C -m 0644 s9.1.gz $(MANDIR)
	install $C -m 0644 help/* $(S9DIR)/help
	install $C -m 0644 s9core.a $(LIBDIR)
	install $C -m 0644 s9core.h $(INCDIR)
	install $C -m 0755 util/make-help-links $(S9DIR)
	(cd $(S9DIR) && ./make-help-links && rm make-help-links)

install-util:
	$(SETPREFIX) <prog/s9help.scm >$(BINDIR)/s9help
	$(SETPREFIX) <prog/s9resolve.scm >$(BINDIR)/s9resolve
	$(SETPREFIX) <prog/scm2html1.scm >$(BINDIR)/scm2html
	$(SETPREFIX) <prog/scmpp.scm >$(BINDIR)/scmpp
	-chmod +x $(BINDIR)/s9help	\
		  $(BINDIR)/s9resolve	\
		  $(BINDIR)/scm2html	\
		  $(BINDIR)/scmpp

install-progs:
	$(SETPREFIX) <prog/advgen.scm >$(BINDIR)/advgen
	$(SETPREFIX) <prog/c2html1.scm >$(BINDIR)/c2html
	$(SETPREFIX) <prog/cols.scm >$(BINDIR)/cols
	$(SETPREFIX) <prog/dupes.scm >$(BINDIR)/dupes
	$(SETPREFIX) <prog/edoc.scm.edoc >$(BINDIR)/edoc
	$(SETPREFIX) <prog/htmlify.scm >$(BINDIR)/htmlify
	$(SETPREFIX) <prog/s9hts.scm >$(BINDIR)/s9hts
	$(SETPREFIX) <prog/soccat.scm >$(BINDIR)/soccat
	-chmod +x $(BINDIR)/advgen	\
		  $(BINDIR)/c2html	\
		  $(BINDIR)/cols	\
		  $(BINDIR)/dupes	\
		  $(BINDIR)/edoc	\
		  $(BINDIR)/htmlify	\
		  $(BINDIR)/s9hts	\
		  $(BINDIR)/soccat

deinstall:	deinstall-s9 deinstall-util deinstall-progs

deinstall-s9:
	rm -f $(S9DIR)/help/* && rmdir $(S9DIR)/help
	rm -f $(S9DIR)/* && rmdir $(S9DIR)
	rm -f $(BINDIR)/s9
	-rmdir $(BINDIR)
	-rmdir $(MANDIR)

deinstall-util:
	rm -f $(BINDIR)/s9help		\
	      $(BINDIR)/s9resolve	\
	      $(BINDIR)/scm2html	\
	      $(BINDIR)/scmpp

deinstall-progs:
	rm -f $(BINDIR)/advgen		\
	      $(BINDIR)/c2html		\
	      $(BINDIR)/cols		\
	      $(BINDIR)/dupes		\
	      $(BINDIR)/edoc		\
	      $(BINDIR)/htmlify		\
	      $(BINDIR)/s9hts		\
	      $(BINDIR)/soccat

tabs:
	@find . -name \*.scm -exec grep -l "	" {} \;

cd:
	./s9 -f util/check-descr.scm

clean:
	rm -f s9 s9.image s9core.a test.image s9.1.gz *.o *.core \
		CATEGORIES.html HACKING.html core s9fes-$(RELEASE).tgz \
		s9fes-$(BASE).tgz s9core-$(RELEASE).tgz __testfile__ \
		s9core.ps s9core.pdf _meta _toc.tr _xref.tr _ndx.tr

new-version:
	vi Makefile s9.c CHANGES
	make s9.c

update-library:
	vi util/make-docs
	util/make-docs
	vi util/make-help-links \
		util/descriptions \
		util/categories.html
	cd help && s9 -f ../util/procedures.scm >INDEX
	@echo
	@echo "Now copy the new help pages from help-new to help"
	@echo "and run util/make-help-links."

s9.1.txt:	s9.1
	$(CC) -o rpp util/rpp.c
	nroff -c -mdoc s9.1 | ./rpp -a >s9.1.txt
	rm -f rpp

docs:	lib ext contrib
	util/make-docs

webdump:
	util/make-html -r $(RELEASE)

advdump:	prog/advgen.scm prog/adventure.adv prog/adventure.intro
	sed -e 's/@dir/quest/' -e 's/@file/index/g' <util/pagehead >pagehead
	prog/advgen.scm -rv \
		-P terminal:session \
		-p pagehead \
		-e util/pagetail \
		-i prog/adventure.intro \
		-t "The Quest for S9fES" \
		-y s9.css \
		prog/adventure.adv
	rm -f pagehead
	cp MASCOT.png advdump
	sed -e 's/^A:link/A/' -e '/^A:visited/,+3d' \
		<util/s9.css >advdump/s9.css

csums:
	csum -u <_csums >_csums.new
	mv _csums.new _csums

mksums:	clean
	find . -type f | grep -v _csums | csum >_csums

dist:	clean s9.1.txt
	make s9core.pdf && mv s9core.pdf _s9core.pdf
	make clean
	mv -f _s9core.pdf s9core.pdf
	cd .. && \
		tar cf - s9 | gzip -9 > s9fes-$(RELEASE).tgz && \
		mv s9fes-$(RELEASE).tgz s9
	rm -f s9core.pdf
	ls -l s9fes-$(RELEASE).tgz | awk '{print int($$5/1024+.5)}'

cdist:
	make s9core.pdf
	tar cf - s9core.[ch] s9core.pdf README.s9core \
		| gzip -9 > s9core-$(RELEASE).tgz 
	rm -f s9core.pdf

arc:	clean s9.1.txt
	cd .. && tar cf - s9 | gzip -9 > s9fes-$(BASE).tgz && \
		mv s9fes-$(BASE).tgz s9
	ls -l s9fes-$(BASE).tgz | awk '{print int($$5/1024+.5)}'
