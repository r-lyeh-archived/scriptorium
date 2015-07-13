@echo off

if exist bench.csv (
del bench.csv
)

if exist BENCH.md (
del BENCH.md
)

@echo.
@echo as/angelscript
@bench 5 angelscript\as tests\fib.as

@echo.
@echo as/angelscript-jit
@bench 5 angelscript\asjit tests\fib.as

@echo.
@echo c/c4
@bench 5 c\c4\c4 tests\fib.c

@echo.
@echo c/tcc
@bench 5 c\tcc-0.9.26\win32\tcc tests\fib.c -run

@echo.
@echo c/vc
@cl tests\fib.c /Ox /Oy /MT /DNDEBUG /nologo > nul
@bench 5 fib

@echo.
@echo chai/chaiscript
@bench 1 chaiscript\chai tests\fib.chai

@echo.
@echo dao/dao
@bench 5 dao\dao tests\fib.dao

@echo.
@echo gm/gamemonkey
@bench 5 gamemonkey\bin\gme64 tests\fib.gm

@echo.
@echo js/42tiny-js
@bench 1 javascript\42tiny-js\42tinyjs tests\fib.js

@echo.
@echo js/duktape
@bench 5 javascript\duktape\duktape tests\fib.js

@echo.
@echo jtc/jtc
@bench 1 jtc\jtc tests\fib.jtc

@echo.
@echo jx9
@bench 5 jx9\jx9 tests\fib.jx9

@echo.
@echo lily/lily
@bench 5 lily\lily tests\fib.lly

@echo.
@echo lisp/lispy90
@bench 1 lisp\lispy90\lispy90 tests\fib.l

@echo.
@echo lisp/minilisp
@bench 1 lisp\minilisp\minilisp < tests\fib.l

@echo.
@echo lisp/paren
@pushd lisp\paren\
@..\..\bench 5 paren-1.9.6 ..\..\tests\fib.pn
@popd

@echo.
@echo lua/lua
@bench 5 lua tests\fib.lua

@echo.
@echo lua/luajit
@bench 5 lua\luajit-2.0\src\luajit tests\fib.lua

@echo.
@echo neko/nekovm
@neko\bin\nekoc tests\fib.neko
@bench 5 neko\neko tests\fib.n

@echo.
@echo os/objectscript
@bench 5 os\os tests\fib.os

@echo.
@echo pawn
@pawn\pawn\bin\pawncc tests\fib.p -d0 -O3 -v2 > nul
@bench 5 pawn\pawn fib.amx

@echo.
@echo pawn-asm
@pawn\pawn\bin\pawncc tests\fib.p -d0 -O3 -v2 > nul
@bench 5 pawn\pawn-asm fib.amx

@echo.
@echo psl/psl
@bench 1 psl\psl tests\fib.psl

@echo.
@echo python/micropython
@bench 5 python\mpython tests\fib.py

@echo.
@echo quakec/gmqcc
@pushd tests\quakec
@..\..\quakec\gmqcc\gmqcc -O3 > nul
@popd
@bench 5 quakec\gmqcc\qcvm tests\quakec\progs.dat

@echo.
@echo ruby/mruby
@bench 5 ruby\mruby tests\fib.rb

@echo.
@echo scheme/s9
@pushd scheme\s9
@..\..\bench 1 s9 ..\..\tests\fib.scm
@popd

@echo.
@echo scheme/tinyscheme
@pushd scheme\tinyscheme-1.41
@..\..\bench 1 scheme ..\..\tests\fib.scm
@popd

@echo.
@echo squirrel/squirrel3
@bench 5 squirrel\sq3 tests\fib.nut

@echo.
@echo tcl/picol
@bench 1 tcl\picol\picol tests\fib.tcl

@echo.
@echo wren
@bench 5 wren\wren tests\fib.wren
