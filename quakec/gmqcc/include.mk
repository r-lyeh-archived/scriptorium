# default directories and paths
DESTDIR :=
PREFIX  := /usr/local
BINDIR  := $(PREFIX)/bin
DATADIR := $(PREFIX)/share
MANDIR  := $(DATADIR)/man

# default flags
CFLAGS  += -Wall -Wextra -Werror -Wstrict-aliasing -Wno-attributes -O2

# compiler
CC      ?= clang

# linker flags and optional additional libraries if required
LDFLAGS +=
LIBS    += -lm

#common objects
COMMON   = ansi.o util.o hash.o stat.o fs.o opts.o conout.o

#optional flags
OPTIONAL_CFLAGS  :=
OPTIONAL_LDFLAGS :=

#objects
OBJ_C = $(COMMON) main.o lexer.o parser.o code.o ast.o ir.o ftepp.o utf8.o correct.o fold.o intrin.o
OBJ_P = $(COMMON) pak.o
OBJ_T = $(COMMON) test.o
OBJ_X = $(COMMON) exec.o

#gource flags
GOURCEFLAGS =                 \
    --date-format "%d %B, %Y" \
    --seconds-per-day 0.01    \
    --auto-skip-seconds 1     \
    --title "GMQCC"           \
    --key                     \
    --camera-mode overview    \
    --highlight-all-users     \
    --file-idle-time 0        \
    --hide progress,mouse     \
    --stop-at-end             \
    --max-files 99999999999   \
    --max-file-lag 0.000001   \
    --bloom-multiplier 1.3    \
    --logo doc/html/gmqcc.png \
    -1280x720

#ffmpeg flags for gource
FFMPEGFLAGS=                  \
    -y                        \
    -r 60                     \
    -f image2pipe             \
    -vcodec ppm               \
    -i -                      \
    -vcodec libx264           \
    -preset ultrafast         \
    -crf 1                    \
    -threads 0                \
    -bf 0

#splint flags
SPLINTFLAGS =                 \
    -preproc                  \
    -redef                    \
    -noeffect                 \
    -nullderef                \
    -usedef                   \
    -type                     \
    -mustfreeonly             \
    -nullstate                \
    -varuse                   \
    -mustfreefresh            \
    -compdestroy              \
    -compmempass              \
    -nullpass                 \
    -onlytrans                \
    -predboolint              \
    -boolops                  \
    -incondefs                \
    -macroredef               \
    -retvalint                \
    -nullret                  \
    -predboolothers           \
    -globstate                \
    -dependenttrans           \
    -branchstate              \
    -compdef                  \
    -temptrans                \
    -usereleased              \
    -warnposix                \
    +charindex                \
    -kepttrans                \
    -unqualifiedtrans         \
    +matchanyintegral         \
    +voidabstract             \
    -nullassign               \
    -unrecog                  \
    -casebreak                \
    -retvalbool               \
    -retvalother              \
    -mayaliasunique           \
    -realcompare              \
    -observertrans            \
    -abstract                 \
    -statictrans              \
    -castfcnptr               \
    -shiftimplementation      \
    -shiftnegative            \
    -boolcompare              \
    -infloops                 \
    -sysunrecog

#always the right rule
default: all

#uninstall rule
uninstall:
	rm -f $(DESTDIR)$(BINDIR)/gmqcc
	rm -f $(DESTDIR)$(BINDIR)/qcvm
	rm -f $(DESTDIR)$(BINDIR)/gmqpak
	rm -f $(DESTDIR)$(MANDIR)/man1/gmqcc.1
	rm -f $(DESTDIR)$(MANDIR)/man1/qcvm.1
	rm -f $(DESTDIR)$(MANDIR)/man1/gmqpak.1

#style rule
STYLE_MATCH = \( -name '*.[ch]' -or -name '*.def' -or -name '*.qc' \)

# splint cannot parse the MSVC source
SPLINT_MATCH = \( -name '*.[ch]' -and ! -name 'msvc.c' -and ! -path './doc/*' \)

style:
	find . -type f $(STYLE_MATCH) -exec sed -i 's/ *$$//' '{}' ';'
	find . -type f $(STYLE_MATCH) -exec sed -i -e '$$a\' '{}' ';'
	find . -type f $(STYLE_MATCH) -exec sed -i 's/\t/    /g' '{}' ';'

splint:
	@splint $(SPLINTFLAGS) `find . -type f $(SPLINT_MATCH)`

gource:
	@gource $(GOURCEFLAGS)

gource-record:
	@gource $(GOURCEFLAGS) -o - | ffmpeg $(FFMPEGFLAGS) gource.mp4
