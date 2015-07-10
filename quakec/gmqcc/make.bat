rem ansi.c  main.c pak.c test.c exec.c
set CORE= && REM util.c 
set CORE=util.c hash.c stat.c fs.c opts.c conout.c
set PLATFORM=msvc.c && REM ansi.c

set OPTS=/Oy- /Zi /MDd /DDEBUG
set OPTS=/Ox /Oy /MT /DNDEBUG

cl /Fegmqcc.exe /DNVALGRIND main.c ast.c code.c correct.c ftepp.c ir.c lexer.c utf8.c parser.c fold.c intrin.c %CORE% %PLATFORM% %OPTS%
cl /Feqcvm.exe  /DNVALGRIND exec.c %CORE% %PLATFORM% %OPTS%

del *.obj
