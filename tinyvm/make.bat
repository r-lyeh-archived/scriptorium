set OPTS=/Ox /Oy /MT /DNDEBUG

cl /Fetdb.exe tdb\*.c -I include libtvm\*.c %OPTS%
cl /Fetvm.exe src\*.c -I include libtvm\*.c %OPTS%

del *.obj
