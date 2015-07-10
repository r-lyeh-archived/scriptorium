@echo off

@echo.
@echo as/angelscript
@bench angelscript\as.exe tests\fib.as

@echo.
@echo as/angelscript-jit
@bench angelscript\asjit.exe tests\fib.as

@echo.
@echo c/c4
@bench c\c4\c4 tests\fib.c

@echo.
@echo c/tcc
@bench c\tcc-0.9.26\win32\tcc tests\fib.c -run

@echo.
@echo c/vc
@cl tests\fib.c /Ox /Oy /MT /DNDEBUG /nologo > nul
@bench fib.exe

@echo.
@echo chai/chaiscript
@bench chaiscript\chai tests\fib.chai

@echo.
@echo gm/gamemonkey
@bench gamemonkey\bin\gme64.exe tests\fib.gm

@echo.
@echo js/42tiny-js
@bench javascript\42tiny-js\42tinyjs.exe tests\fib.js

@echo.
@echo js/duktape
@bench javascript\duktape\duktape.exe tests\fib.js

@echo.
@echo jtc/jtc
@bench jtc\jtc tests\fib.jtc

@echo.
@echo jx9
@bench jx9\jx9 tests\fib.jx9

@echo.
@echo lily/lily
@bench lily\lily tests\fib.lly

@echo.
@echo lisp/lispy90
@bench lisp\lispy90\lispy90 tests\fib.l

@echo.
@echo lisp/minilisp
@bench lisp\minilisp\minilisp < tests\fib.l

@echo.
@echo lisp/paren
@pushd lisp\paren\
@..\..\bench paren-1.9.6 ..\..\tests\fib.pn
@popd

@echo.
@echo lua/lua
@bench lua tests\fib.lua

@echo.
@echo lua/luajit
@bench lua\luajit-2.0\src\luajit.exe tests\fib.lua

@echo.
@echo neko/nekovm
@neko\bin\nekoc tests\fib.neko
@bench neko\neko tests\fib.n

@echo.
@echo pawn
@pawn\pawn\bin\pawncc tests\fib.p -d0 -O3 -v2 > nul
@bench pawn\pawn fib.amx

@echo.
@echo pawn-asm
@pawn\pawn\bin\pawncc tests\fib.p -d0 -O3 -v2 > nul
@bench pawn\pawn-asm fib.amx

@echo.
@echo psl/psl
@bench psl\psl tests\fib.psl

@echo.
@echo python/micropython
@bench python\mpython tests\fib.py

@echo.
@echo quakec/gmqcc
@pushd tests\quakec
@..\..\quakec\gmqcc\gmqcc -O3 > nul
@popd
@bench quakec\gmqcc\qcvm tests\quakec\progs.dat

@echo.
@echo ruby/mruby
@bench ruby\mruby tests\fib.rb

@echo.
@echo scheme/s9
@pushd scheme\s9
@..\..\bench s9 ..\..\tests\fib.scm
@popd

@echo.
@echo scheme/tinyscheme
@pushd scheme\tinyscheme-1.41
@..\..\bench scheme ..\..\tests\fib.scm
@popd

@echo.
@echo squirrel/squirrel3
@bench squirrel\sq3 tests\fib.nut

@echo.
@echo tcl/picol
@bench tcl\picol\picol tests\fib.tcl

@echo.
@echo wren
@bench wren\wren tests\fib.wren
