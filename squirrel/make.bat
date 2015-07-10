cl /Fesq3.exe squirrel3\squirrel\*.cpp -I squirrel3\include squirrel3\sqstdlib\*.cpp squirrel3\sq\*.c /Ox /Oy /MT /DNDEBUG 
del *.obj