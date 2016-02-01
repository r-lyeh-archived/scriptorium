# SGScript makefile


include core.mk


# APP FLAGS
OUTFILE_STATIC=lib/libsgscript.a
OUTFILE_DYNAMIC=$(OUTDIR)/$(LIBPFX)sgscript$(LIBEXT)
OUTFLAGS_STATIC=-Llib -lsgscript
OUTFLAGS_DYNAMIC=-L$(OUTDIR) -lsgscript
ifneq ($(static),)
	PREFLAGS=-DBUILDING_SGS=1
	OUTFILE=$(OUTFILE_STATIC)
	OUTFLAGS=$(OUTFLAGS_STATIC)
else
	PREFLAGS=-DBUILDING_SGS=1 -DSGS_DLL=1
	OUTFILE=$(OUTFILE_DYNAMIC)
	OUTFLAGS=$(OUTFLAGS_DYNAMIC)
endif
CFLAGS=-fwrapv -Wall -Wconversion -Wshadow -Wpointer-arith -Wcast-qual -Wcast-align \
	$(call fnIF_RELEASE,-O3,-D_DEBUG -g) $(call fnIF_COMPILER,gcc,-static-libgcc,) \
	$(call fnIF_ARCH,x86,-m32,$(call fnIF_ARCH,x64,-m64,)) -Isrc \
	$(call fnIF_OS,windows,,-fPIC -D_FILE_OFFSET_BITS=64) \
	$(call fnIF_OS,android,-DSGS_PF_ANDROID,)
COMFLAGS=$(CFLAGS)
BINFLAGS=$(CFLAGS) $(OUTFLAGS) -lm \
	$(call fnIF_OS,android,-ldl -Wl$(comma)-rpath$(comma)'$$ORIGIN' -Wl$(comma)-z$(comma)origin,) \
	$(call fnIF_OS,windows,-lkernel32,) \
	$(call fnIF_OS,osx,-ldl -Wl$(comma)-rpath$(comma)'@executable_path/.',) \
	$(call fnIF_OS,linux,-ldl -lrt -Wl$(comma)-rpath$(comma)'$$ORIGIN' -Wl$(comma)-z$(comma)origin,)
MODULEFLAGS=-DSGS_COMPILE_MODULE $(BINFLAGS) -shared
EXEFLAGS=$(BINFLAGS)
THREADLIBS=$(call fnIF_OS,windows,,$(call fnIF_OS,android,,-lpthread))
SGS_INSTALL_TOOL = $(call fnIF_OS,osx,install_name_tool \
	-change $(OUTDIR)/libsgscript.so @rpath/libsgscript.so $1,)


# BUILD INFO
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),clean_obj)
ifneq ($(MAKECMDGOALS),clean_objbin)
$(info -------------------)
$(info SGScript build info)
$(info -------------------)
$(info OS - $(cOS))
$(info ARCH - $(cARCH))
$(info COMPILER - $(cCOMPILER))
$(info TARGET - $(target_os)/$(target_arch))
$(info MODE - $(call fnIF_RELEASE,release,debug))
$(info OUT.LIB. - $(OUTFILE))
$(info TODO - $(MAKECMDGOALS))
$(info -------------------)
endif
endif
endif


# TARGETS
## the library (default target)
.PHONY: make
make: $(OUTFILE)

DEPS = $(patsubst %,src/%.h,sgs_int sgs_regex sgs_util sgscript)
OBJ = $(patsubst %,obj/sgs_%.o,bcg ctx fnt proc regex std stdL tok util xpc)

lib/libsgscript.a: $(OBJ)
	ar rcs $@ $(OBJ)
$(OUTDIR)/$(LIBPFX)sgscript$(LIBEXT): $(OBJ)
	$(CC) $(PREFLAGS) -o $@ $(OBJ) $(CFLAGS) -shared -lm $(call fnIF_OS,osx,-dynamiclib -install_name @rpath/$(LIBPFX)sgscript$(LIBEXT),)
obj/%.o: src/%.c $(DEPS)
	$(CC) $(PREFLAGS) -c -o $@ $< $(COMFLAGS)

## the libraries
$(OUTDIR)/sgsxgmath$(LIBEXT): ext/sgsxgmath.c ext/sgsxgmath.h $(OUTFILE)
	$(CC) -o $@ $< $(MODULEFLAGS) $(call fnIF_OS,osx,-dynamiclib -install_name @rpath/sgsxgmath$(LIBEXT),)
$(OUTDIR)/sgsjson$(LIBEXT): ext/sgsjson.c $(OUTFILE)
	$(CC) -o $@ $< $(MODULEFLAGS) $(call fnIF_OS,osx,-dynamiclib -install_name @rpath/sgsjson$(LIBEXT),)
$(OUTDIR)/sgspproc$(LIBEXT): ext/sgspproc.c $(OUTFILE)
	$(CC) -o $@ $< $(MODULEFLAGS) $(THREADLIBS) $(call fnIF_OS,osx,-dynamiclib -install_name @rpath/sgspproc$(LIBEXT),)
$(OUTDIR)/sgssockets$(LIBEXT): ext/sgssockets.c $(OUTFILE)
	$(CC) -o $@ $< $(MODULEFLAGS) $(call fnIF_OS,windows,-lws2_32,) $(call fnIF_OS,osx,-dynamiclib -install_name @rpath/sgssockets$(LIBEXT),)
$(OUTDIR)/sgsmeta$(LIBEXT): ext/sgsmeta.c $(OUTFILE)
	$(CC) -o $@ $< $(MODULEFLAGS) $(call fnIF_OS,osx,-dynamiclib -install_name @rpath/sgsmeta$(LIBEXT),)

## the tools
$(OUTDIR)/sgstest$(BINEXT): ext/sgstest.c $(OUTFILE)
	$(CC) -o $@ $< $(EXEFLAGS)
$(OUTDIR)/sgsapitest$(BINEXT): ext/sgsapitest.c ext/sgs_prof.c $(OUTFILE)
	$(CC) -o $@ $(subst $(OUTFILE),,$^) $(EXEFLAGS)
$(OUTDIR)/sgsvm$(BINEXT): ext/sgsvm.c ext/sgs_idbg.c ext/sgs_prof.c $(OUTFILE)
	$(CC) -o $@ $(subst $(OUTFILE),,$^) $(EXEFLAGS)
$(OUTDIR)/sgsc$(BINEXT): ext/sgsc.c $(OUTFILE)
	$(CC) -o $@ $< $(EXEFLAGS)

## library/tool aliases
.PHONY: xgmath
.PHONY: json
.PHONY: pproc
.PHONY: sockets
.PHONY: meta
.PHONY: build_test
.PHONY: build_apitest
.PHONY: vm
.PHONY: c
.PHONY: test
.PHONY: apitest
xgmath: $(OUTDIR)/sgsxgmath$(LIBEXT)
json: $(OUTDIR)/sgsjson$(LIBEXT)
pproc: $(OUTDIR)/sgspproc$(LIBEXT)
sockets: $(OUTDIR)/sgssockets$(LIBEXT)
meta: $(OUTDIR)/sgsmeta$(LIBEXT)
build_test: $(OUTDIR)/sgstest$(BINEXT) json xgmath meta
build_apitest: $(OUTDIR)/sgsapitest$(BINEXT)
vm: $(OUTDIR)/sgsvm$(BINEXT)
c: $(OUTDIR)/sgsc$(BINEXT)

# test tool aliases
test: build_test
	$(OUTDIR)/sgstest --dir tests
apitest: build_apitest
	$(OUTDIR)/sgsapitest

.PHONY: tools
tools: xgmath json pproc sockets meta build_test build_apitest vm c

## other stuff
## - cppbc testing
.PHONY: cppbctest
.PHONY: build_cppbctest
build_cppbctest: $(OUTDIR)/sgscppbctest$(BINEXT)
cppbctest: build_cppbctest
	$(OUTDIR)/sgscppbctest
$(OUTDIR)/sgscppbctest$(BINEXT): ext/sgscppbctest.cpp obj/cppbc_test.cpp ext/sgscppbctest.h ext/sgs_cppbc.h $(OUTFILE)
	$(CXX) -o $@ $< $(word 2,$^) $(EXEFLAGS) -I. -std=c++03 -Wno-shadow
	$(call SGS_INSTALL_TOOL,$@)
.PHONY: cppbctest11
.PHONY: build_cppbctest11
build_cppbctest11: $(OUTDIR)/sgscppbctest11$(BINEXT)
cppbctest11: build_cppbctest11
	$(OUTDIR)/sgscppbctest11
$(OUTDIR)/sgscppbctest11$(BINEXT): ext/sgscppbctest.cpp obj/cppbc_test.cpp ext/sgscppbctest.h ext/sgs_cppbc.h $(OUTFILE)
	$(CXX) -o $@ $< $(word 2,$^) $(EXEFLAGS) -I. -std=c++11 -Wno-shadow
	$(call SGS_INSTALL_TOOL,$@)
obj/cppbc_test.cpp: ext/sgscppbctest.h $(OUTDIR)/sgsvm$(BINEXT)
	$(OUTDIR)/sgsvm -p ext/cppbc.sgs $< -o $@
## - multithreaded testing
.PHONY: mttest
.PHONY: build_mttest
build_mttest: $(OUTDIR)/sgstest_mt$(BINEXT)
mttest: build_mttest
	$(OUTDIR)/sgstest_mt
$(OUTDIR)/sgstest_mt$(BINEXT): examples/sgstest_mt.c $(OUTFILE)
	$(CC) -o $@ $^ $(EXEFLAGS) $(THREADLIBS)
	$(call SGS_INSTALL_TOOL,$@)
## - sgs2exe tool
.PHONY: sgsexe
sgsexe: ext/sgsexe.c $(OUTFILE_STATIC)
	$(CC) -o $(OUTDIR)/sgsexe.exe $^ $(CFLAGS) -lm
	copy /B $(OUTDIR)\sgsexe.exe + ext\stubapp.bin $(OUTDIR)\sgsexe.tmp
	del /Q $(OUTDIR)\sgsexe.exe
	cmd /c move /Y $(OUTDIR)\sgsexe.tmp $(OUTDIR)\sgsexe.exe
## - binary archive preparation
.PHONY: binarch
binarch: clean vm
	$(OUTDIR)/sgsvm build/prep.sgs
## - documentation preparation
.PHONY: docs
docs: vm json
	cd docs && $(call fnFIX_PATH,../$(OUTDIR)/sgsvm) -p docgen -e
## - single header versions
.PHONY: shv
shv: vm
	$(OUTDIR)/sgsvm -p build/hgen -o $(OUTDIR)/sgscript-impl.h
	$(OUTDIR)/sgsvm -p build/hgen -m -o $(OUTDIR)/sgscript-no-stdlib-impl.h

## clean build data
.PHONY: clean_obj
clean_obj:
	-$(fnREMOVE_ALL) $(call fnFIX_PATH,obj/*.o)

.PHONY: clean_objbin
clean_objbin:
	-$(fnREMOVE_ALL) $(call fnFIX_PATH,obj/*.o $(OUTDIR)/sgs* $(OUTDIR)/libsgs*)

.PHONY: clean
clean:
	-$(fnREMOVE_ALL) $(call fnFIX_PATH,obj/*.o lib/*.a lib/*.lib $(OUTDIR)/sgs* $(OUTDIR)/libsgs*)

