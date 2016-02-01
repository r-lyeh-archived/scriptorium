SOURCES = $(wildcard *.pas)
OUTPUT = lil
FPC = fpc

.PHONY: all
all: $(OUTPUT)

$(OUTPUT): $(SOURCES)
	$(FPC) lil.pas

.PHONY: clean
clean:
	rm -f $(OUTPUT) *.o *.ppu
