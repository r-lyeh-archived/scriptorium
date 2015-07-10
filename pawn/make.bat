set OPTS=/Zi /Oy- /MD /DDEBUG
set OPTS=/Ox /Oy /MT /DNDEBUG 

echo.>alloca.h
set FLAGS=-DSTDECL -D_CRT_SECURE_NO_WARNINGS -DENABLE_BINRELOC
set FLAGS=-DHAVE_ALLOCA_H -Dalloca=_alloca -I.
set FLAGS=-DFLOATPOINT -DFIXEDPOINT

set CORE=pawn\source\amx\amx.c pawn\source\amx\amxaux.c

set EXTRAS=pawn\source\amx\amxcore.c pawn\source\amx\amxpool.c pawn\source\amx\amxdbg.c user32.lib -DPAWN_USE_CORE
set EXTRAS=

set TERMINAL=
set TERMINAL=pawn\source\amx\amxcons.c -DPAWN_USE_CONSOLE && REM pawn\source\amx\termwin.c gdi32.lib -DAMX_TERMINAL 

set ASM=
set JIT=

cl %* /Fepawn.exe *.cpp -I pawn\source %CORE% %EXTRAS% %TERMINAL% %FLAGS% %ASM% %JIT% %OPTS%
del *.obj

set ASM=-DAMX_ASM amxexec.obj
ml /c /DCDECL /DSTACKARGS /Cx /coff pawn\source\amx\amxexec.asm
cl %* /Fepawn-asm.exe *.cpp -I pawn\source %CORE% %EXTRAS% %TERMINAL% %FLAGS% %ASM% %JIT% %OPTS%

set JIT=-DAMX_JIT -DAMX_JIT_CALLCONV=__cdecl -Damx_jit_compile=amx_jit_compile_ -Damx_jit_run=amx_jit_run_ -Damx_jit_list=amx_jit_list_ amxjitr.obj 
jwasm\jwasm -coff -Gd -DSTACKARGS -Cx pawn\source\amx\amxjitr.asm
cl %* /Fepawn-jit.exe *.cpp -I pawn\source %CORE% %EXTRAS% %TERMINAL% %FLAGS% %ASM% %JIT% %OPTS% 

del *.obj

