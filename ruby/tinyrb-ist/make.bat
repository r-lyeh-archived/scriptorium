set GC=-I vendor\gc\include vendor\gc\*.c -DGC_NOT_DLL -I vendor\gc\include\private 
set PCRE=-I vendor\pcre vendor\pcre\pcre_*.c -DHAVE_CONFIG_H -DPCRE_STATIC

set OPTS=/Zi /Oy- /MDd /DDEBUG
set OPTS=/Ox /Oy /MT /DNDEBUG

cl /c vm\*.cc -I vendor vendor\freegetopt\getopt.c %PCRE% %OPTS% %GC% %*

del setjmp_t.obj
del gcname.obj
del threadlibs.obj
del add_gc_prefix.obj

cl /Fetinyrb.exe *.obj user32.lib

del *.obj
