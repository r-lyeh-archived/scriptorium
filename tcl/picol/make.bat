REM  vs2013 && cl picol.c /Ox /Oy /DNDEBUG /MT /Dsnprintf=_snprintf 
echo vs2015 && cl picol.c /Ox /Oy /DNDEBUG /MT 
del *.obj
