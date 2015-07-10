# This makefile is for Opus MAKE
# Build with:
#       make -famx32
#
# For release version, use
#       make -a -famx32 NDEBUG=

DEFGEN = f:\tools\defgen
LIBASE = f:\tools\libase
MSLIB   =f:\tools\lib32
BCLIB   =d:\programm\bc5
LCCLIB  =e:\lcc\bin
SVNREV  =p:\tools\svnrev

!ifdef NDEBUG
  # retail version
  C_DEBUG=-dNDEBUG
  L_DEBUG=d all op symf
!else
  # "work in progress"
  C_DEBUG=-hc
  L_DEBUG=d codeview op cvpack
!endif

INCLUDE=..\amx

C_OPTS=-w4 -e25 $(C_DEBUG) -ei -zq -of+s -d2 -bd -bm -5s -bt=nt -mf -damx_Init=amx_InitAMX -dAMXAPI=__stdcall -dAMX_NATIVE_CALL=__stdcall
L_OPTS=$(L_DEBUG) SYS win95 dll op m exp =amx32 op maxe=25 op q

%.obj : %.c
    wcc386 $(.SOURCE) -i=$(INCLUDE) $(C_OPTS)

all : amx32.dll amx32m.lib amx32b.lib amx32l.lib

amxdll.obj : amxdll.c balloon.h

amx.obj : ..\amx\amx.c
  wcc386 $(.SOURCE) -i=$(INCLUDE) $(C_OPTS) -dASM32 -dSTACKARGS

amxcore.obj : ..\amx\amxcore.c

amxcons.obj : ..\amx\amxcons.c
  wcc386 $(.SOURCE) -i=$(INCLUDE) $(C_OPTS) -dAMX_TERMINAL

amxdbg.obj : ..\amx\amxdbg.c

amxexec.obj : ..\amx\amxexec.asm
  wasm $(.SOURCE) -i=$(INCLUDE) -d1 -mf -3s -w4 -zq -dSTDECL -dSTACKARGS

fixed.obj : ..\amx\fixed.c

termwin.obj: ..\amx\termwin.c

pawndbg.obj : ..\amx\pawndbg.c
  wcc386 $(.SOURCE) -i=$(INCLUDE) $(C_OPTS) -dNO_REMOTE

balloon.obj : balloon.c balloon.h

##### Linker files and LIB files #####

amx32.def + amx32.lbc + amx32.imp : amxdll.fed
    $(DEFGEN)\defgen -we=$(.TARGET,2) -g -win32 -o$(.TARGET,1) $(.SOURCE)
    $(DEFGEN)\defgen -wi=$(.TARGET,3) -s -win32 -oNUL $(.SOURCE)

amx32l.lib + amx32.exp : amxdll.fed
    $(DEFGEN)\defgen -s -win32 -li=$(.TARGET,2) -oNUL $(.SOURCE)
    $(LCCLIB)\buildlib $(.TARGET,2) $(.TARGET,1)

amx32m.lib + amx32m.def : amxdll.fed
  $(DEFGEN)\defgen -g -win32 -o$(.TARGET,2) $(.SOURCE)
  $(MSLIB)\lib /machine:ix86 /def:$(.TARGET,2) /out:$(.TARGET,1)

amx32b.lib : amx32.dll
  $(BCLIB)\bin\implib $(.TARGET) amx32.dll

amx32.dll : amxdll.obj amx.obj amxcore.obj amxcons.obj amxdbg.obj amxexec.obj \
            fixed.obj termwin.obj pawndbg.obj balloon.obj amxdll.rc amx32.def
  wlink $(L_OPTS) @<<
    NAME $(.TARGET)
    FIL $(.SOURCES,M"*.obj",W\,)
    LIB user32.lib,gdi32.lib,kernel32.lib
  <<
  wrc -bt=nt -dWIN32 -i=$(INCLUDE) -q amxdll.rc $(.TARGET)
  $(LIBASE)\libase $(.TARGET)
  wlib -n -b -ii -fo -io amx32w.lib @amx32.imp
!ifdef NDEBUG
    upx $(.TARGET)
!endif

