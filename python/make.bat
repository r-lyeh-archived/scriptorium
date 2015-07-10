set OPTS=/Ox /Oy /MT /DNDEBUG
set FLAGS=-D__USE_MINGW_ANSI_STDIO=1 
cl %OPTS% %FLAGS% /Fempython.exe micropython\py\*.c -I micropython -I micropython\windows -I micropython\windows\build -I micropython\windows\msvc micropython\windows\msvc\*.c -DM_PI=3.14159265358979323846 -DM_E=2.7182818284590452354 micropython\windows\*.c micropython\extmod\*.c micropython\unix\main.c micropython\unix\file.c micropython\unix\input.c micropython\unix\modos.c micropython\unix\modtime.c micropython\unix\gccollect.c micropython\lib\mp-readline\readline.c %* 
del *.obj
