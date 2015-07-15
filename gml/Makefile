CC        ?= clang
COMPILER   = $(shell $(CC) --version | head -1)
OS         = $(shell uname)
CFLAGS     = -std=gnu99 -Wall -Wextra -g3 -Ilinenoise -DGML_COMPILER="\"$(COMPILER)\"" -DGML_OS="\"$(OS)\"" -DGML_TYPE="\"development\""
LDFLAGS    = -lm
SOURCES    = lex.c list.c parse.c runtime.c builtin.c gml.c linenoise/linenoise.c
OBJECTS    = $(SOURCES:.c=.o)
EXECUTABLE = gml
PREFIX     = /usr

all: $(SOURCES) $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	$(CC) $(LDFLAGS) $(OBJECTS) -o $@

.c.o:
	$(CC) -c $(CFLAGS) $< -o $@

clean:
	rm -rf $(OBJECTS) $(EXECUTABLE)

install: $(EXECUTABLE)
	install $(EXECUTABLE) $(PREFIX)/bin/

uninstall:
	rm -rf $(PREFIX)/bin/$(EXECUTABLE)
