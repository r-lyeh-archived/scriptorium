
TARGET = daomake$(EXESUFFIX)

DAO_SRC_DIR =../../../kernel

INCS = -I$(DAO_SRC_DIR)
CFLAGS = $(DEBUG) -O0 $(SYSCFLAGS) -DDAO_WITH_REGEX
LFLAGS = $(SYSLIBS)


HEADERS = $(DAO_SRC_DIR)/dao.h $(DAO_SRC_DIR)/daoBase.h $(DAO_SRC_DIR)/daoGC.h \
		  $(DAO_SRC_DIR)/daoObject.h $(DAO_SRC_DIR)/daoRoutine.h \
		  $(DAO_SRC_DIR)/daoThread.h $(DAO_SRC_DIR)/daoList.h \
		  $(DAO_SRC_DIR)/daoLexer.h $(DAO_SRC_DIR)/daoOptimizer.h \
		  $(DAO_SRC_DIR)/daoInferencer.h \
		  $(DAO_SRC_DIR)/daoStdlib.h $(DAO_SRC_DIR)/daoType.h \
		  $(DAO_SRC_DIR)/daoParser.h $(DAO_SRC_DIR)/daoStdtype.h \
		  $(DAO_SRC_DIR)/daoValue.h $(DAO_SRC_DIR)/daoBytecode.h \
		  $(DAO_SRC_DIR)/daoMap.h $(DAO_SRC_DIR)/daoPlatforms.h \
		  $(DAO_SRC_DIR)/daoStream.h $(DAO_SRC_DIR)/daoVmcode.h \
		  $(DAO_SRC_DIR)/daoClass.h $(DAO_SRC_DIR)/daoNamespace.h \
		  $(DAO_SRC_DIR)/daoProcess.h $(DAO_SRC_DIR)/daoString.h \
		  $(DAO_SRC_DIR)/daoVmspace.h $(DAO_SRC_DIR)/daoConst.h \
		  $(DAO_SRC_DIR)/daoNumtype.h $(DAO_SRC_DIR)/daoRegex.h \
		  $(DAO_SRC_DIR)/daoInterface.h $(DAO_SRC_DIR)/daoTasklet.h


first: all

daoList-$(PLAT).o: $(HEADERS) $(DAO_SRC_DIR)/daoList.c 
	$(CC) -c $(CFLAGS) $(INCS) $(DAO_SRC_DIR)/daoList.c -o daoList-$(PLAT).o

daoMap-$(PLAT).o: $(HEADERS) $(DAO_SRC_DIR)/daoMap.c
	$(CC) -c $(CFLAGS) $(INCS) $(DAO_SRC_DIR)/daoMap.c -o daoMap-$(PLAT).o

daoType-$(PLAT).o: $(HEADERS) $(DAO_SRC_DIR)/daoType.c
	$(CC) -c $(CFLAGS) $(INCS) $(DAO_SRC_DIR)/daoType.c -o daoType-$(PLAT).o

daoInterface-$(PLAT).o: $(HEADERS) $(DAO_SRC_DIR)/daoInterface.c
	$(CC) -c $(CFLAGS) $(INCS) $(DAO_SRC_DIR)/daoInterface.c -o daoInterface-$(PLAT).o

daoValue-$(PLAT).o: $(HEADERS) $(DAO_SRC_DIR)/daoValue.c
	$(CC) -c $(CFLAGS) $(INCS) $(DAO_SRC_DIR)/daoValue.c -o daoValue-$(PLAT).o

daoProcess-$(PLAT).o: $(HEADERS) $(DAO_SRC_DIR)/daoProcess.c
	$(CC) -c $(CFLAGS) $(INCS) $(DAO_SRC_DIR)/daoProcess.c -o daoProcess-$(PLAT).o

daoRoutine-$(PLAT).o: $(HEADERS) $(DAO_SRC_DIR)/daoRoutine.c
	$(CC) -c $(CFLAGS) $(INCS) $(DAO_SRC_DIR)/daoRoutine.c -o daoRoutine-$(PLAT).o

daoGC-$(PLAT).o: $(HEADERS) $(DAO_SRC_DIR)/daoGC.c
	$(CC) -c $(CFLAGS) $(INCS) $(DAO_SRC_DIR)/daoGC.c -o daoGC-$(PLAT).o

daoStdtype-$(PLAT).o: $(HEADERS) $(DAO_SRC_DIR)/daoStdtype.c
	$(CC) -c $(CFLAGS) $(INCS) $(DAO_SRC_DIR)/daoStdtype.c -o daoStdtype-$(PLAT).o

daoNamespace-$(PLAT).o: $(HEADERS) $(DAO_SRC_DIR)/daoNamespace.c
	$(CC) -c $(CFLAGS) $(INCS) $(DAO_SRC_DIR)/daoNamespace.c -o daoNamespace-$(PLAT).o

daoString-$(PLAT).o: $(HEADERS) $(DAO_SRC_DIR)/daoString.c
	$(CC) -c $(CFLAGS) $(INCS) $(DAO_SRC_DIR)/daoString.c -o daoString-$(PLAT).o

daoStdlib-$(PLAT).o: $(HEADERS) $(DAO_SRC_DIR)/daoStdlib.c
	$(CC) -c $(CFLAGS) $(INCS) $(DAO_SRC_DIR)/daoStdlib.c -o daoStdlib-$(PLAT).o

daoLexer-$(PLAT).o: $(HEADERS) $(DAO_SRC_DIR)/daoLexer.c
	$(CC) -c $(CFLAGS) $(INCS) $(DAO_SRC_DIR)/daoLexer.c -o daoLexer-$(PLAT).o

daoParser-$(PLAT).o: $(HEADERS) $(DAO_SRC_DIR)/daoParser.c
	$(CC) -c $(CFLAGS) $(INCS) $(DAO_SRC_DIR)/daoParser.c -o daoParser-$(PLAT).o

daoBytecode-$(PLAT).o: $(HEADERS) $(DAO_SRC_DIR)/daoBytecode.c
	$(CC) -c $(CFLAGS) $(INCS) $(DAO_SRC_DIR)/daoBytecode.c -o daoBytecode-$(PLAT).o

daoOptimizer-$(PLAT).o: $(HEADERS) $(DAO_SRC_DIR)/daoOptimizer.c
	$(CC) -c $(CFLAGS) $(INCS) $(DAO_SRC_DIR)/daoOptimizer.c -o daoOptimizer-$(PLAT).o

daoInferencer-$(PLAT).o: $(HEADERS) $(DAO_SRC_DIR)/daoInferencer.c
	$(CC) -c $(CFLAGS) $(INCS) $(DAO_SRC_DIR)/daoInferencer.c -o daoInferencer-$(PLAT).o

daoThread-$(PLAT).o: $(HEADERS) $(DAO_SRC_DIR)/daoThread.c
	$(CC) -c $(CFLAGS) $(INCS) $(DAO_SRC_DIR)/daoThread.c -o daoThread-$(PLAT).o

daoPlatforms-$(PLAT).o: $(HEADERS) $(DAO_SRC_DIR)/daoPlatforms.c
	$(CC) -c $(CFLAGS) $(INCS) $(DAO_SRC_DIR)/daoPlatforms.c -o daoPlatforms-$(PLAT).o

daoNumtype-$(PLAT).o: $(HEADERS) $(DAO_SRC_DIR)/daoNumtype.c
	$(CC) -c $(CFLAGS) $(INCS) $(DAO_SRC_DIR)/daoNumtype.c -o daoNumtype-$(PLAT).o

daoClass-$(PLAT).o: $(HEADERS) $(DAO_SRC_DIR)/daoClass.c
	$(CC) -c $(CFLAGS) $(INCS) $(DAO_SRC_DIR)/daoClass.c -o daoClass-$(PLAT).o

daoConst-$(PLAT).o: $(HEADERS) $(DAO_SRC_DIR)/daoConst.c
	$(CC) -c $(CFLAGS) $(INCS) $(DAO_SRC_DIR)/daoConst.c -o daoConst-$(PLAT).o

daoObject-$(PLAT).o: $(HEADERS) $(DAO_SRC_DIR)/daoObject.c
	$(CC) -c $(CFLAGS) $(INCS) $(DAO_SRC_DIR)/daoObject.c -o daoObject-$(PLAT).o

daoTasklet-$(PLAT).o: $(HEADERS) $(DAO_SRC_DIR)/daoTasklet.c
	$(CC) -c $(CFLAGS) $(INCS) $(DAO_SRC_DIR)/daoTasklet.c -o daoTasklet-$(PLAT).o

daoStream-$(PLAT).o: $(HEADERS) $(DAO_SRC_DIR)/daoStream.c
	$(CC) -c $(CFLAGS) $(INCS) $(DAO_SRC_DIR)/daoStream.c -o daoStream-$(PLAT).o

daoVmcode-$(PLAT).o: $(HEADERS) $(DAO_SRC_DIR)/daoVmcode.c
	$(CC) -c $(CFLAGS) $(INCS) $(DAO_SRC_DIR)/daoVmcode.c -o daoVmcode-$(PLAT).o

daoVmspace-$(PLAT).o: $(HEADERS) $(DAO_SRC_DIR)/daoVmspace.c
	$(CC) -c $(CFLAGS) $(INCS) $(DAO_SRC_DIR)/daoVmspace.c -o daoVmspace-$(PLAT).o

daoRegex-$(PLAT).o: $(HEADERS) $(DAO_SRC_DIR)/daoRegex.c
	$(CC) -c $(CFLAGS) $(INCS) $(DAO_SRC_DIR)/daoRegex.c -o daoRegex-$(PLAT).o

daoMake-$(PLAT).o: $(HEADERS) ../source/daoMake.c
	$(CC) -c $(CFLAGS) $(INCS) ../source/daoMake.c -o daoMake-$(PLAT).o


OBJECTS = daoConst-$(PLAT).o daoVmcode-$(PLAT).o daoInterface-$(PLAT).o \
		  daoString-$(PLAT).o daoList-$(PLAT).o daoMap-$(PLAT).o \
		  daoValue-$(PLAT).o daoStdtype-$(PLAT).o daoNumtype-$(PLAT).o \
		  daoRoutine-$(PLAT).o daoClass-$(PLAT).o daoObject-$(PLAT).o \
		  daoProcess-$(PLAT).o daoNamespace-$(PLAT).o daoVmspace-$(PLAT).o \
		  daoLexer-$(PLAT).o daoParser-$(PLAT).o daoBytecode-$(PLAT).o \
		  daoType-$(PLAT).o daoOptimizer-$(PLAT).o daoStdlib-$(PLAT).o \
		  daoInferencer-$(PLAT).o \
		  daoStream-$(PLAT).o daoRegex-$(PLAT).o daoGC-$(PLAT).o \
		  daoThread-$(PLAT).o daoTasklet-$(PLAT).o daoPlatforms-$(PLAT).o \
		  daoMake-$(PLAT).o



all: $(TARGET)

$(TARGET): $(OBJECTS) daoMake-$(PLAT).o
	$(CC) $(OBJECTS) -o $(TARGET) $(LFLAGS)

clean:
	./daomake remove $(OBJECTS)



linux:
	$(MAKE) DEBUG=$(DEBUG) SYSCFLAGS="-DUNIX -DLINUX -fPIC" SYSLIBS="-lm -ldl"

macosx:
	$(MAKE) DEBUG=$(DEBUG) SYSCFLAGS="-DUNIX -DMACOSX -fPIC" SYSLIBS=""

ios:
	$(MAKE) DEBUG=$(DEBUG) SYSCFLAGS="-DUNIX -DMACOSX -DIOS -fPIC" SYSLIBS=""

freebsd:
	$(MAKE) DEBUG=$(DEBUG) SYSCFLAGS="-DUNIX -DFREEBSD -fPIC" SYSLIBS="-lm"

openbsd:
	$(MAKE) DEBUG=$(DEBUG) SYSCFLAGS="-DUNIX -DOPENBSD -fPIC" SYSLIBS="-lm"

mingw:
	$(MAKE) DEBUG=$(DEBUG) SYSCFLAGS="-DWIN32 -DMINGW" SYSLIBS="-lwinmm" EXESUFFIX=".exe"

minix:
	$(MAKE) DEBUG=$(DEBUG) SYSCFLAGS="-DUNIX -DMINIX -fPIC" SYSLIBS="-lm"

beos haiku:
	$(MAKE) DEBUG=$(DEBUG) SYSCFLAGS="-DUNIX -DBEOS -fPIC" SYSLIBS=""
