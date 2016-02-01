
# MAKE
comma := ,
space :=
space +=


# BASIC VARIABLES
ifeq ($(CC),cc) # no such compiler on Windows
	CC=
endif
ifeq ($(CC),)
	CC=gcc
endif
ifeq ($(CXX),)
	CXX=g++
endif
ifeq ($(OUTDIR),)
	OUTDIR=bin
endif


# UTILITIES
cOS=os_unknown
cARCH=arch_unknown
cCOMPILER=compiler_unknown
ifeq ($(OS),Windows_NT)
	fnREMOVE_ALL = del /F /S /Q
	fnCOPY_FILE = copy
	fnFIX_PATH = $(subst /,\,$1)
	cOS=windows
	ifeq ($(PROCESSOR_ARCHITECTURE),x86)
		cARCH=x86
	endif
	ifeq ($(PROCESSOR_ARCHITECTURE),AMD64)
		cARCH=x64
	endif
else
	fnREMOVE_ALL = rm -rf
	fnCOPY_FILE = cp
	fnFIX_PATH = $1
	UNAME_S := $(shell uname -s)
	ifeq ($(UNAME_S),Linux)
		cOS=linux
	endif
	ifeq ($(UNAME_S),Darwin)
		cOS=osx
	endif
	UNAME_P := $(shell uname -p)
	ifneq ($(filter %86,$(UNAME_P)),)
		cARCH=x86
	endif
	ifeq ($(UNAME_P),x86_64)
		cARCH=x64
	endif
	ifneq ($(filter arm%,$(UNAME_P)),)
		cARCH=arm
	endif
endif
CC_V := $(shell $(CC) 2>&1)
ifneq ($(findstring clang,$(CC_V)),)
	cCOMPILER=clang
endif
ifneq ($(findstring gcc,$(CC_V)),)
	cCOMPILER=gcc
endif
cOS_ARCH=$(cOS)-$(cARCH)


ifeq ($(target),)
	target=$(cOS_ARCH)
endif
target_os=$(word 1,$(subst -, ,$(target)))
target_arch=$(word 2,$(subst -, ,$(target)))
ifeq ($(target_os),)
$(error Target OS not specified (windows/linux/osx))
endif
ifeq ($(target_arch),)
$(error Target CPU type not specified (x86/x64/arm/..))
endif
cIF_RELEASE=$(findstring release,$(mode))
fnIF_RELEASE=$(if $(cIF_RELEASE),$1,$2)
fnIF_OS=$(if $(findstring $1,$(target_os)),$2,$3)
fnIF_ARCH=$(if $(findstring $1,$(target_arch)),$2,$3)
fnIF_OS_ARCH=$(if $(findstring $1,$(target)),$2,$3)
fnIF_COMPILER=$(if $(findstring $1,$(cCOMPILER)),$2,$3)


# PLATFORM SPECIFICS
ifeq ($(target_os),windows)
	BINEXT=.exe
	LIBPFX=
	LIBEXT=.dll
else
	BINEXT=
	LIBPFX=lib
	LIBEXT=.so
endif
