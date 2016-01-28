@ECHO OFF

REM
REM  This batch file builds LIL using cl.exe and lib.exe
REM  from Microsoft C/C++ Compiler command line tools
REM  available from Visual Studio and/or Windows SDK
REM

ECHO.
ECHO Making the msvcbuild directory
mkdir msvcbuild
cd msvcbuild
 
ECHO.
ECHO Building the library (lil.lib)

cl /nologo /Ox /TC /c /DWIN32 ..\lil.c /Folil
lib /nologo /out:lil.lib lil.obj

ECHO.
ECHO Building the lil.exe example
cl /nologo /Ox /TC /Felil /DWIN32 ..\main.c /link lil.lib

ECHO.
ECHO Removing unnecessary files
del *.obj

ECHO.

dir

ECHO.
ECHO Done
ECHO.
cd ..
