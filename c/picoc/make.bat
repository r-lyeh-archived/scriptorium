set OPTS=/Ox /Oy /MT /DNDEBUG
set FLAGS=/DWIN32

cl /Fepicoc.exe *.c cstdlib\*.c platform\*_msvc.c %FLAGS% %OPTS%

del *.obj
