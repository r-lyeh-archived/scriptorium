cl /Fewren.exe src\cli\*.c -I src\include src\vm\*.c /Ox /Oy /MT /DNDEBUG 

del *.obj
