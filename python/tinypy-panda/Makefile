CC=gcc
RM=rm -f
CFLAGS=-ggdb -O0
CPPFLAGS=-DTP_NORMALMAKE -DTP_SANDBOX
WFLAGS=-std=c89 -Wall

PY2BC_OBJ=tinypy/tokenize.tpc tinypy/parse.tpc tinypy/encode.tpc tinypy/py2bc.tpc
TINYPY_OBJ=tinypy/mymain.o
VM_OBJ=tinypy/vmmain.o
CORE_SRC=$(addprefix tinypy/,builtins.c dict.c gc.c list.c misc.c ops.c sandbox.c string.c vm.c)
CORE_OBJ=$(CORE_SRC:.c=.o)
AUTOHEADERS=$(addsuffix _auto.h,$(basename $(CORE_SRC:.c=.h)))
AUTODEPS=$(CORE_OBJ:.o=.d)

all: build/tinypy build/vm $(PY2BC_OBJ)

-include $(AUTODEPS)

build:
	@mkdir -p build

build/tinypy: build $(TINYPY_OBJ) $(CORE_OBJ)
	$(CC) $(WFLAGS) $(CFLAGS) -o $@ $(TINYPY_OBJ) $(CORE_OBJ)
	
build/vm: build $(VM_OBJ) $(CORE_OBJ)
	$(CC) $(WFLAGS) $(CFLAGS) -o $@ $(VM_OBJ) $(CORE_OBJ)

$(AUTODEPS): $(AUTOHEADERS)

clean:
	-$(RM) -rf build/
	-$(RM) tinypy/*.o
	-$(RM) tinypy/*.tpc
	-$(RM) tinypy/*.pyc
	-$(RM) $(AUTODEPS)
	-$(RM) $(AUTOHEADERS)

%.tpc: %.py
	cd tinypy && python py2bc.py ../$< ../$@

$(addsuffix _auto.h,$(basename %.h)): %.c
	@echo "#ifndef HEADER_$(subst .h,_H,$(subst /,_,$(addsuffix _auto.h,$(basename $@))))" > $@
	@echo "#define HEADER_$(subst .h,_H,$(subst /,_,$(addsuffix _auto.h,$(basename $@))))" >> $@
	@echo "#include \"tp.h\"" >> $@
	@grep -E '^[a-zA-Z0-9_]+ \*?[a-zA-Z0-9_]+\([^)]+\)' $< | sed -e 's/[ \t]*{[ \t]*/;/' >> $@
	@echo "#endif" >> $@
	
%.o: %.c
	$(CC) $(WFLAGS) $(CFLAGS) $(CPPFLAGS) -o $@ -c $<

%.d: %.c
	@/usr/X11/bin/makedepend $(CPPFLAGS) -f - $< | sed 's,\($*\.o\)[ :]*,\1 $@ : ,g' > $@
