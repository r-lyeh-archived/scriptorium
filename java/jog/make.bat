set OPTS=/Ox /Oy /MT /DNDEBUG
set FLAGS=/EHsc

cl /Fejog.exe test.cpp libraries\jog\*.c* -I libraries -I libraries\jog %OPTS% %FLAGS%

del *.obj
