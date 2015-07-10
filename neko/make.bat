del *.obj
set OPTS=/Ox /Oy /DNDEBUG /MT %*
cl /c %OPTS% -I gc-7.2 -I gc-7.2\gc gc-7.2\*.c /DGC_NOT_DLL /DMSVC_DBG_DLL=0 /DGC_WIN32_THREADS -I gc-7.2\libatomic_ops\src gc-7.2\libatomic_ops\src\*.c && REM -DAO_HAVE_compare_and_swap 
lib /out:gc.lib *.obj 

del *.obj
cl /c %OPTS% -I src\vm src\vm\*.c -DNEKO_STANDALONE -I gc-7.2 -I gc-7.2\gc /DGC_NOT_DLL 
del main.obj
lib /out:nekovm.lib *.obj

cl neko.c %OPTS% -DNEKO_STANDALONE -DGC_NOT_DLL -I src\vm -I gc-7.2\gc nekovm.lib gc.lib user32.lib
del *.obj
