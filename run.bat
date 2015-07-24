@echo off

rem @echo.
rem @echo lisp/toylisp
rem @pushd lisp\toylisp
rem @..\..\bench 5 toylisp ..\..\tests\fib.py
rem @popd

@echo.
@echo as/angelscript
@bench 5 angelscript\as tests\fib.as //[as/angelscript](http://www.angelcode.com/angelscript/)

@echo.
@echo as/angelscript-jit
@bench 5 angelscript\asjit tests\fib.as //[as/angelscript-jit](https://github.com/BlindMindStudios/AngelScript-JIT-Compiler)

@echo.
@echo c/c4
@bench 5 c\c4\c4 tests\fib.c //[c/c4](https://github.com/rswier/c4)

@echo.
@echo c/tcc
@bench 5 c\tcc-0.9.26\win32\tcc tests\fib.c -run //[c/libtcc](http://bellard.org/tcc/)

@echo.
@echo c/vc
@cl tests\fib.c /Ox /Oy /MT /DNDEBUG /nologo > nul
@bench 5 fib //[c/vc](https://www.visualstudio.com/)

@echo.
@echo chai/chaiscript
@bench 1 chaiscript\chai tests\fib.chai //[chaiscript](https://github.com/ChaiScript/ChaiScript)

@echo.
@echo dao/dao
@bench 5 dao\dao tests\fib.dao //[dao](https://github.com/daokoder/dao)

@echo.
@echo gm/gamemonkey
@bench 5 gamemonkey\bin\gme64 tests\fib.gm //[gamemonkey](http://www.gmscript.com/)

@echo.
@echo gml
@bench 1 gml\gml tests\fib.gml --no-history //[gml](https://github.com/graphitemaster/gml)

@echo.
@echo js/42tiny-js
@bench 1 javascript\42tiny-js\42tinyjs tests\fib.js //[js/42tiny-js](https://github.com/ardi69/42tiny-js)

@echo.
@echo js/duktape
@bench 5 javascript\duktape\duktape tests\fib.js //[js/duktape](https://github.com/svaarala/duktape)

@echo.
@echo jtc/jtc
@bench 1 jtc\jtc tests\fib.jtc //[jtc](https://github.com/progschj/jtc)

@echo.
@echo jx9
@bench 5 jx9\jx9 tests\fib.jx9 //[jx9](http://jx9.symisc.net/)

@echo.
@echo lily/lily
@bench 5 lily\lily tests\fib.lly //[lily](https://github.com/jesserayadkins/lily)

@echo.
@echo lisp/lispy90
@bench 1 lisp\lispy90\lispy90 tests\fib.l //[lisp/lispy90](http://howtowriteaprogram.blogspot.com.es/2010/11/lisp-interpreter-in-90-lines-of-c.html)

@echo.
@echo lisp/minilisp
@bench 1 lisp\minilisp\minilisp < tests\fib.l //[lisp/minilisp](https://github.com/rui314/minilisp)

@echo.
@echo lisp/paren
@pushd lisp\paren\
@..\..\bench 5 paren-1.9.6 ..\..\tests\fib.pn //[lisp/paren](https://bitbucket.org/ktg/paren)
@popd

@echo.
@echo lua/lua
@bench 5 lua tests\fib.lua //[lua](https://github.com/LuaDist/lua)

@echo.
@echo lua/luajit
@bench 5 lua\luajit-2.0\src\luajit tests\fib.lua //[lua/luajit](https://github.com/LuaDist/luajit)

@echo.
@echo neko/nekovm
@neko\bin\nekoc tests\fib.neko
@bench 5 neko\neko tests\fib.n //[neko/nekovm](https://github.com/HaxeFoundation/neko)

@echo.
@echo os/objectscript
@bench 5 os\os tests\fib.os //[objectscript](https://github.com/unitpoint/objectscript)

@echo.
@echo pawn
@pawn\pawn\bin\pawncc tests\fib.p -d0 -O3 -v2 > nul
@bench 5 pawn\pawn fib.amx //[pawn](http://www.compuphase.com/pawn/pawn.htm)

@echo.
@echo pawn-asm
@pawn\pawn\bin\pawncc tests\fib.p -d0 -O3 -v2 > nul
@bench 5 pawn\pawn-asm fib.amx //[pawn/asm](http://www.compuphase.com/pawn/pawn.htm)

@echo.
@echo psl/psl
@bench 1 psl\psl tests\fib.psl //[psl](https://github.com/Silica/PSL)

@echo.
@echo python/micropython
@bench 5 python\mpython tests\fib.py //[python/micropython](https://github.com/micropython/micropython)

@echo.
@echo python/tinypy
@bench 5 python\tinypy-panda\build\tinypy tests\fib.py //[python/tinypy(panda)](https://github.com/dkasak/tinypy-panda)

@echo.
@echo quakec/gmqcc
@pushd tests\quakec
@..\..\quakec\gmqcc\gmqcc -O3 > nul
@popd
@bench 5 quakec\gmqcc\qcvm tests\quakec\progs.dat //[quakec/gmqcc](https://github.com/graphitemaster/gmqcc)

@echo.
@echo ruby/mruby
@bench 5 ruby\mruby tests\fib.rb //[ruby/mruby](https://github.com/mruby/mruby)

@echo.
@echo ruby/tinyrb
@pushd ruby\tinyrb-ist
@..\..\bench 5 tinyrb ..\..\tests\fib.rb //[ruby/tinyrb(ist)](https://github.com/sanchapereira/tinyrb-ist)
@popd

@echo.
@echo scheme/chibi
@pushd scheme\chibi-scheme\
@..\..\bench 5 chibi ..\..\tests\fib.scm //[scheme/chibi](https://github.com/ashinn/chibi-scheme)
@popd

@echo.
@echo scheme/s7
@pushd scheme\s7\
@..\..\bench 5 s7 ..\..\tests\fib.scm //[scheme/s7](https://ccrma.stanford.edu/software/snd/snd/s7.html)
@popd

@echo.
@echo scheme/s9
@pushd scheme\s9
@..\..\bench 1 s9 ..\..\tests\fib.scm //[scheme/s9](http://www.t3x.org/s9fes/)
@popd

@echo.
@echo scheme/tinyscheme
@pushd scheme\tinyscheme-1.41
@..\..\bench 1 scheme ..\..\tests\fib.scm //[scheme/tinyscheme](http://tinyscheme.sourceforge.net/home.html)
@popd

@echo.
@echo squirrel/squirrel3
@bench 5 squirrel\sq3 tests\fib.nut //[squirrel](http://squirrel-lang.org/)

@echo.
@echo tcl/picol
@bench 1 tcl\picol\picol tests\fib.tcl //[tcl/picol](http://wiki.tcl.tk/17893)

@echo.
@echo wren
@bench 5 wren\wren tests\fib.wren //[wren](https://github.com/munificent/wren)

@echo.
@echo c/picoc
@bench 5 c\picoc\picoc tests\fib.c //[C/picoC](https://github.com/zsaleeba/picoc)

@echo.
@echo c/oc
@c\oc\parse < tests\fib.oc > tests\fib.ooc 
@bench 5 c\oc\interp < tests\fib.ooc //[C/OC](http://exmortis.narod.ru/src_pcode_eng.html)
