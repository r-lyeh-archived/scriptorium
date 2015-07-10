@echo on 
setlocal enabledelayedexpansion

set OPTS=/Zi /Oy- /DDEBUG /MDd
set OPTS=/Ox /Oy /DNDEBUG /MT

del *.obj
for /R mruby\build\host %%i in (*.c) do cl /c /Fo!RANDOM!-!RANDOM!-%%~ni.obj %%i %OPTS% -I mruby\include %OPTS%
for /R mruby\mrbgems\mruby-array %%i in (*.c) do cl /c /Fo!RANDOM!-!RANDOM!-%%~ni.obj %%i %OPTS% -I mruby\include %OPTS%
for /R mruby\mrbgems\mruby-array-ext %%i in (*.c) do cl /c /Fo!RANDOM!-!RANDOM!-%%~ni.obj %%i %OPTS% -I mruby\include %OPTS%
for /R mruby\mrbgems\mruby-compiler %%i in (*.c) do cl /c /Fo!RANDOM!-!RANDOM!-%%~ni.obj %%i %OPTS% -I mruby\include %OPTS%
for /R mruby\mrbgems\mruby-eval %%i in (*.c) do cl /c /Fo!RANDOM!-!RANDOM!-%%~ni.obj %%i %OPTS% -I mruby\include %OPTS%
for /R mruby\mrbgems\mruby-fiber %%i in (*.c) do cl /c /Fo!RANDOM!-!RANDOM!-%%~ni.obj %%i %OPTS% -I mruby\include %OPTS%
for /R mruby\mrbgems\mruby-hash-ext %%i in (*.c) do cl /c /Fo!RANDOM!-!RANDOM!-%%~ni.obj %%i %OPTS% -I mruby\include %OPTS%
for /R mruby\mrbgems\mruby-kernel-ext %%i in (*.c) do cl /c /Fo!RANDOM!-!RANDOM!-%%~ni.obj %%i %OPTS% -I mruby\include %OPTS%
for /R mruby\mrbgems\mruby-math %%i in (*.c) do cl /c /Fo!RANDOM!-!RANDOM!-%%~ni.obj %%i %OPTS% -I mruby\include %OPTS%
for /R mruby\mrbgems\mruby-numeric-ext %%i in (*.c) do cl /c /Fo!RANDOM!-!RANDOM!-%%~ni.obj %%i %OPTS% -I mruby\include %OPTS%
for /R mruby\mrbgems\mruby-object-ext %%i in (*.c) do cl /c /Fo!RANDOM!-!RANDOM!-%%~ni.obj %%i %OPTS% -I mruby\include %OPTS%
for /R mruby\mrbgems\mruby-objectspace %%i in (*.c) do cl /c /Fo!RANDOM!-!RANDOM!-%%~ni.obj %%i %OPTS% -I mruby\include %OPTS%
for /R mruby\mrbgems\mruby-print %%i in (*.c) do cl /c /Fo!RANDOM!-!RANDOM!-%%~ni.obj %%i %OPTS% -I mruby\include %OPTS%
for /R mruby\mrbgems\mruby-proc %%i in (*.c) do cl /c /Fo!RANDOM!-!RANDOM!-%%~ni.obj %%i %OPTS% -I mruby\include %OPTS%
for /R mruby\mrbgems\mruby-proc-ext %%i in (*.c) do cl /c /Fo!RANDOM!-!RANDOM!-%%~ni.obj %%i %OPTS% -I mruby\include %OPTS%
for /R mruby\mrbgems\mruby-random %%i in (*.c) do cl /c /Fo!RANDOM!-!RANDOM!-%%~ni.obj %%i %OPTS% -I mruby\include %OPTS%
for /R mruby\mrbgems\mruby-range-ext %%i in (*.c) do cl /c /Fo!RANDOM!-!RANDOM!-%%~ni.obj %%i %OPTS% -I mruby\include %OPTS%
for /R mruby\mrbgems\mruby-sprintf %%i in (*.c) do cl /c /Fo!RANDOM!-!RANDOM!-%%~ni.obj %%i %OPTS% -I mruby\include %OPTS%
for /R mruby\mrbgems\mruby-string-ext %%i in (*.c) do cl /c /Fo!RANDOM!-!RANDOM!-%%~ni.obj %%i %OPTS% -I mruby\include %OPTS%
for /R mruby\mrbgems\mruby-struct %%i in (*.c) do cl /c /Fo!RANDOM!-!RANDOM!-%%~ni.obj %%i %OPTS% -I mruby\include %OPTS%
for /R mruby\mrbgems\mruby-symbol %%i in (*.c) do cl /c /Fo!RANDOM!-!RANDOM!-%%~ni.obj %%i %OPTS% -I mruby\include %OPTS%
for /R mruby\mrbgems\mruby-symbol-ext %%i in (*.c) do cl /c /Fo!RANDOM!-!RANDOM!-%%~ni.obj %%i %OPTS% -I mruby\include %OPTS%
for /R mruby\mrbgems\mruby-time %%i in (*.c) do cl /c /Fo!RANDOM!-!RANDOM!-%%~ni.obj %%i %OPTS% -I mruby\include %OPTS%
for /R mruby\src %%i in (*.c) do cl /c /Fo!RANDOM!-!RANDOM!-%%~ni.obj %%i %OPTS% -I mruby\include %OPTS%

cl /Femruby.exe %OPTS% repl.c -I mruby\include -I mruby\mrbgems\mruby-compiler\core *.obj mruby\build\host\mrbgems\mruby-compiler\core\y.tab.c

del *.obj