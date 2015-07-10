rem  vs2013 && cl /Felily.exe run\*.c -I src src\*.c /Ox /Oy /MT /DNDEBUG /Dinline= /Dsnprintf=_snprintf /Dstrerror_r= psapi.lib
echo vs2015 && cl /Felily.exe run\*.c -I src src\*.c /Ox /Oy /MT /DNDEBUG /Dinline= /Dstrerror_r= psapi.lib
del *.obj
