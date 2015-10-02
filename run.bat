@echo off

rem @echo.
rem @echo lisp/toylisp
rem @pushd lisp\toylisp
rem @..\..\bench 5 toylisp ..\..\tests\fib.py
rem @popd

@echo.
@echo as/angelscript
@bench 5 angelscript\as tests\fib.as //Angelscript,[angelscript](http://www.angelcode.com/angelscript/)

@echo.
@echo as/angelscript-jit
@bench 5 angelscript\asjit tests\fib.as //Angelscript,[angelscript-jit](https://github.com/BlindMindStudios/AngelScript-JIT-Compiler)

@echo.
@echo c/c4
@bench 5 c\c4\c4 tests\fib.c //C,[c4](https://github.com/rswier/c4)

@echo.
@echo c/c4-jit
@bench 5 c\c4jit\c4jit tests\fib.c //C,[c4-jit](https://github.com/EarlGray/c4)

@echo.
@echo c/picoc
@bench 5 c\picoc\picoc tests\fib.c //C,[picoC](https://github.com/zsaleeba/picoc)

@echo.
@echo c/tcc
@bench 5 c\tcc-0.9.26\win32\tcc tests\fib.c -run //C,[libtcc](http://bellard.org/tcc/)

@echo.
@echo c/vc
@cl tests\fib.c /Ox /Oy /MT /DNDEBUG /nologo > nul
@bench 5 fib //C,[vc](https://www.visualstudio.com/)

@echo.
@echo chai/chaiscript
@bench 1 chaiscript\chai tests\fib.chai //ChaiScript,[chaiscript](https://github.com/ChaiScript/ChaiScript)

@echo.
@echo dao/dao
@bench 5 dao\dao tests\fib.dao //Dao,[dao](https://github.com/daokoder/dao)

@echo.
@echo gm/gamemonkey
@bench 5 gamemonkey\bin\gme64 tests\fib.gm //GameMonkey,[gamemonkey](http://www.gmscript.com/)

@echo.
@echo gml
@bench 1 gml\gml tests\fib.gml --no-history //GML,[gml](https://github.com/graphitemaster/gml)

@echo.
@echo java/jog
@pushd java\jog
@..\..\bench 5 jog ..\..\tests\fib.java //Java,[Jog](https://code.google.com/p/jog-interpreter)
@popd 

@echo.
@echo jetscript
@bench 5 jetscript\jet tests\fib.jet //JetScript,[JetScript](https://github.com/matt-attack/JetScript)

@echo.
@echo js/42tiny-js
@bench 1 javascript\42tiny-js\42tinyjs tests\fib.js //JavaScript,[42tiny-js](https://github.com/ardi69/42tiny-js)

@echo.
@echo js/duktape
@bench 5 javascript\duktape\duktape tests\fib.js //JavaScript,[duktape](https://github.com/svaarala/duktape)

@echo.
@echo js/v7
@bench 1 javascript\v7\v7 tests\fib.js //JavaScript,[v7](https://github.com/cesanta/v7)

@echo.
@echo jtc/jtc
@bench 1 jtc\jtc tests\fib.jtc //JTC,[jtc](https://github.com/progschj/jtc)

@echo.
@echo jx9
@bench 5 jx9\jx9 tests\fib.jx9 //JX9,[jx9](http://jx9.symisc.net/)

@echo.
@echo lily/lily
@bench 5 lily\lily tests\fib.lly //Lily,[lily](https://github.com/jesserayadkins/lily)

@echo.
@echo lisp/lispy90
@bench 1 lisp\lispy90\lispy90 tests\fib.l //Lisp,[lispy90](http://howtowriteaprogram.blogspot.com.es/2010/11/lisp-interpreter-in-90-lines-of-c.html)

@echo.
@echo lisp/minilisp
@bench 1 lisp\minilisp\minilisp < tests\fib.l //Lisp,[minilisp](https://github.com/rui314/minilisp)

@echo.
@echo lisp/paren
@pushd lisp\paren\
@..\..\bench 5 paren-1.9.6 ..\..\tests\fib.pn //Lisp,[paren](https://bitbucket.org/ktg/paren)
@popd

@echo.
@echo lua/lua
@bench 5 lua tests\fib.lua //Lua,[lua](https://github.com/LuaDist/lua)

@echo.
@echo lua/luajit
@bench 5 lua\luajit-2.0\src\luajit tests\fib.lua //Lua,[luajit](https://github.com/LuaDist/luajit)

@echo.
@echo neko/nekovm
@neko\bin\nekoc tests\fib.neko
@bench 5 neko\neko tests\fib.n //Neko,[nekovm](https://github.com/HaxeFoundation/neko)

@echo.
@echo os/objectscript
@bench 5 os\os tests\fib.os //ObjectScript,[objectscript](https://github.com/unitpoint/objectscript)

@echo.
@echo pawn
@pawn\pawn\bin\pawncc tests\fib.p -d0 -O3 -v2 > nul
@bench 5 pawn\pawn fib.amx //Pawn,[pawn](http://www.compuphase.com/pawn/pawn.htm)

@echo.
@echo pawn-asm
@pawn\pawn\bin\pawncc tests\fib.p -d0 -O3 -v2 > nul
@bench 5 pawn\pawn-asm fib.amx //Pawn,[pawn-asm](http://www.compuphase.com/pawn/pawn.htm)

@echo.
@echo php/ph7
@bench 1 php\ph7\ph7 tests\fib.php //PHP,[ph7](https://github.com/symisc/PH7)

@echo.
@echo psl/psl
@bench 1 psl\psl tests\fib.psl //PSL,[psl](https://github.com/Silica/PSL)

@echo.
@echo python/micropython
@bench 5 python\mpython tests\fib.py //Python,[micropython](https://github.com/micropython/micropython)

@echo.
@echo python/tinypy
@bench 5 python\tinypy-panda\build\tinypy tests\fib.py //Python,[tinypy(panda)](https://github.com/dkasak/tinypy-panda)

@echo.
@echo quakec/gmqcc
@pushd tests\quakec
@..\..\quakec\gmqcc\gmqcc -O3 > nul
@popd
@bench 5 quakec\gmqcc\qcvm tests\quakec\progs.dat //QuakeC,[gmqcc](https://github.com/graphitemaster/gmqcc)

@echo.
@echo ruby/mruby
@bench 5 ruby\mruby tests\fib.rb //Ruby,[mruby](https://github.com/mruby/mruby)

@echo.
@echo ruby/tinyrb
@pushd ruby\tinyrb-ist
@..\..\bench 5 tinyrb ..\..\tests\fib.rb //Ruby,[tinyrb(ist)](https://github.com/sanchapereira/tinyrb-ist)
@popd

@echo.
@echo scheme/chibi
@pushd scheme\chibi-scheme\
@..\..\bench 5 chibi ..\..\tests\fib.scm //Scheme,[chibi](https://github.com/ashinn/chibi-scheme)
@popd

@echo.
@echo scheme/s7
@pushd scheme\s7\
@..\..\bench 5 s7 ..\..\tests\fib.scm //Scheme,[s7](https://ccrma.stanford.edu/software/snd/snd/s7.html)
@popd

@echo.
@echo scheme/s9
@pushd scheme\s9
@..\..\bench 1 s9 ..\..\tests\fib.scm //Scheme,[s9](http://www.t3x.org/s9fes/)
@popd

@echo.
@echo scheme/tinyscheme
@pushd scheme\tinyscheme-1.41
@..\..\bench 1 scheme ..\..\tests\fib.scm //Scheme,[tinyscheme](http://tinyscheme.sourceforge.net/home.html)
@popd

@echo.
@echo squirrel/squirrel3
@bench 5 squirrel\sq3 tests\fib.nut //Squirrel,[squirrel](http://squirrel-lang.org/)

@echo.
@echo tcl/picol
@bench 1 tcl\picol\picol tests\fib.tcl //Tcl,[picol](http://wiki.tcl.tk/17893)

@echo.
@echo tcl/jim
@bench 5 tcl\jim\jimsh tests\fib.tcl //Tcl,[jim](https://github.com/antirez/Jim)

@echo.
@echo tinyvm
@bench 5 tinyvm\tvm tests\fib.tvm //TinyVM,[tinyvm](https://github.com/jakogut/tinyvm)

@echo.
@echo wren
@bench 5 wren\wren tests\fib.wren //Wren,[wren](https://github.com/munificent/wren)




@echo.
@echo c/oc
@c\oc\parse < tests\fib.oc > tests\fib.ooc 
@rem bench 5 c\oc\interp < tests\fib.ooc //C,[OC](http://exmortis.narod.ru/src_pcode_eng.html)

