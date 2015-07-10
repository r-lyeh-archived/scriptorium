CDIR  =d:\programm\bc5
BINDIR=$(CDIR)\bin

INCDIR=..\amx;$(CDIR)\include
LIBDIR=$(CDIR)\lib
DLL_MODEL=l

DEFGEN = f:\tools\defgen\defgen

!ifndef NDEBUG
  C_DEBUG=-DSTRICT -v
  L_DEBUG=-v
!else
  C_DEBUG=-DSTRICT;NDEBUG
  L_DEBUG=
!endif

RC      =$(BINDIR)\brc -I$(CDIR)\include
DLL_CC  =$(BINDIR)\bcc -2 -c $(C_DEBUG) -I$(INCDIR) -ml -Od -w -WDE -y
DLL_LINK=$(BINDIR)\tlink /c /L$(LIBDIR) $(L_DEBUG) /l /m /d /Twd /C
DLL_OBJ =$(LIBDIR)\c0dl.obj
DLL_LIBS=cwl import #mathwl


##### Inference Rules #####
%.obj : %.c
    $(DLL_CC) @c_defs.inc $*.c

%.res : %.rc
    $(RC) -r $*.rc


amx16.dll : amxdll.obj amx.obj amxcore.obj amxcons.obj amxdbg.obj fixed.obj \
            termwin.obj pawndbg.obj amx16.def amxdll.res
    $(DLL_LINK) @<<
      $(DLL_OBJ) $(.SOURCES,M"*.obj"), +
      $(.TARGET), +
      , +
      $(DLL_LIBS), +
      $(.SOURCES,M"*.def")
    <<
    $(RC) -t amxdll.res $(.TARGET)
    implib $*.lib $(.TARGET)
!ifdef NDEBUG
    pklite -w -k $(.TARGET)
!endif

amx16.def : amxdll.fed
    $(DEFGEN) -be -g -win16 -o$*.def -DCDECL $(.SOURCE)

amxdll.res : amxdll.rc

amxdll.obj: amxdll.c c_defs.inc
    $(DLL_CC) @c_defs.inc $(.SOURCES,M"*.c")

amx.obj: ..\amx\amx.c c_defs.inc
    $(DLL_CC) @c_defs.inc $(.SOURCES,M"*.c")

amxcore.obj: ..\amx\amxcore.c c_defs.inc
    $(DLL_CC) @c_defs.inc $(.SOURCES,M"*.c")

amxcons.obj: ..\amx\amxcons.c c_defs.inc
    $(DLL_CC) -DAMX_TERMINAL @c_defs.inc $(.SOURCES,M"*.c")

amxdbg.obj: ..\amx\amxdbg.c c_defs.inc
    $(DLL_CC) @c_defs.inc $(.SOURCES,M"*.c")

fixed.obj : ..\amx\fixed.c c_defs.inc
    $(DLL_CC) @c_defs.inc $(.SOURCES,M"*.c")

termwin.obj: ..\amx\termwin.c c_defs.inc
    $(DLL_CC) @c_defs.inc $(.SOURCES,M"*.c")

pawndbg.obj : ..\amx\pawndbg.c c_defs.inc
    $(DLL_CC) @c_defs.inc -DNO_REMOTE $(.SOURCES,M"*.c")

c_defs.inc: amx16.mak
    @echo -Damx_Init=amx_InitAMX;AMXAPI="__pascal __export";AMX_NATIVE_CALL="__far __pascal" > $*.inc

