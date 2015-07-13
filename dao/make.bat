set OPTS=-DDAO_WITH_THREAD
set OPTS=main.c
set OPTS=

rem if exist dao\kernel\daomain.c move dao\kernel\daomain.c dao\kernel\daomain.c.bak 
cl /Fedao.exe -I dao\kernel /Ox /Oy /MT /DNDEBUG dao\kernel\*.c -DWIN32 winmm.lib %OPTS% 

del *.obj


