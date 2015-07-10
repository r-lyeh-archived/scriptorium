set OPTS=/Ox /Oy /MT /DNDEBUG
set OPTS=/Oy- /Zi /MDd /DDEBUG

set FLAGS=-I msvc
set FLAGS=-DWIN32 -DWIN32GUI user32.lib
set FLAGS=-DWIN32

cl /Feqcc.exe   compiler\*.c   %FLAGS% %OPTS%
cl /Feunqcc.exe decompiler\*.c %FLAGS% %OPTS%

del *.obj