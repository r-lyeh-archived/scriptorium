scriptorium
===========

- quick and dirty benchmark suite (dont take it too serious)
- benchmarked latest versions at the time of writing (Jul 2015)

### languages/implementations evaluated (33)
- angelscript
- angelscript (angelscript-jit)
- c (c4)
- c (native)
- c (tcclib)
- chaiscript
- dao
- gamemonkey 
- javascript (42tiny-js)
- javascript (duktape)
- jtc
- jx9
- lily
- lisp (lispy)
- lisp (minilisp)
- lisp (paren)
- lua (lua)
- lua (lua-jit)
- neko (nekovm)
- objectscript
- pawn (pawn)
- pawn (pawn-asm)
- psl
- python (micropython)
- python (tinypy)
- quakec (gmqcc)
- ruby (mruby)
- ruby (tinyrb)
- scheme (s9)
- scheme (tinyscheme)
- squirrel (squirrel3)
- tcl (picol)
- wren

### language requirements
- must embed from C++.
- must compile on vs2015 (or vs2013 at least).
- must link statically.
- must not require (heavy) makefiles/cygwin/build-systems to build.
- must use jit/optimizations if available.
- must compare fair to other languages. for example:
  - must not use yield/coroutines on recursive fibonacci test.
  - must disable threading if possible (not all languages are thread-safe)
  - etc

### results
|Language|Time|Relative Lua speed|Score|
|:-------|---:|:----------------:|----:|
|c/vc|0.0655255 s.|![100%](http://progressed.io/bar/100)|2323 pt|
|lua/luajit-2.0/src/luajit|0.0906679 s.|![100%](http://progressed.io/bar/100)|1679 pt|
|c/tcc-0.9.26/win32/tcc|0.127398 s.|![100%](http://progressed.io/bar/100)|1195 pt|
|pawn/pawn-asm|0.460578 s.|![100%](http://progressed.io/bar/100)|330 pt|
|pawn/pawn|0.858627 s.|![100%](http://progressed.io/bar/100)|177 pt|
|neko/neko|1.3005 s.|![100%](http://progressed.io/bar/100)|117 pt|
|lua|1.52243 s.|![100%](http://progressed.io/bar/100)|100 pt|
|tinyrb|1.55396 s.|![97.971%](http://progressed.io/bar/97)|97 pt|
|gamemonkey/bin/gme64|1.71352 s.|![88.8481%](http://progressed.io/bar/88)|88 pt|
|angelscript/as|1.96192 s.|![77.599%](http://progressed.io/bar/77)|77 pt|
|angelscript/asjit|2.00607 s.|![75.8912%](http://progressed.io/bar/75)|75 pt|
|lily/lily|2.03696 s.|![74.7403%](http://progressed.io/bar/74)|74 pt|
|wren/wren|2.24041 s.|![67.9532%](http://progressed.io/bar/67)|67 pt|
|ruby/mruby|2.40104 s.|![63.4071%](http://progressed.io/bar/63)|63 pt|
|c/c4/c4|2.88247 s.|![52.8169%](http://progressed.io/bar/52)|52 pt|
|dao/dao|2.90203 s.|![52.4609%](http://progressed.io/bar/52)|52 pt|
|squirrel/sq3|3.1019 s.|![49.0806%](http://progressed.io/bar/49)|49 pt|
|quakec/gmqcc/qcvm|3.44478 s.|![44.1953%](http://progressed.io/bar/44)|44 pt|
|python/mpython|3.564 s.|![42.7169%](http://progressed.io/bar/42)|42 pt|
|os/os|3.7224 s.|![40.8992%](http://progressed.io/bar/40)|40 pt|
|lisp/minilisp/minilisp|7.4961 s.|![20.3096%](http://progressed.io/bar/20)|20 pt|
|javascript/duktape/duktape|9.21308 s.|![16.5247%](http://progressed.io/bar/16)|16 pt|
|psl/psl|19.4708 s.|![7.81904%](http://progressed.io/bar/7)|7 pt|
|python/tinypy-panda/build/tinypy|22.3401 s.|![6.81479%](http://progressed.io/bar/6)|6 pt|
|s9|36.9632 s.|![4.11877%](http://progressed.io/bar/4)|4 pt|
|jx9/jx9|41.3571 s.|![3.68118%](http://progressed.io/bar/3)|3 pt|
|jtc/jtc|49.5935 s.|![3.06982%](http://progressed.io/bar/3)|3 pt|
|scheme|72.6125 s.|![2.09665%](http://progressed.io/bar/2)|2 pt|
|paren-1.9.6|78.547 s.|![1.93824%](http://progressed.io/bar/1)|1 pt|
|lisp/lispy90/lispy90|101.345 s.|![1.50223%](http://progressed.io/bar/1)|1 pt|
|tcl/picol/picol|163.352 s.|![0.931993%](http://progressed.io/bar/0)|0 pt|
|chaiscript/chai|185.719 s.|![0.819749%](http://progressed.io/bar/0)|0 pt|
|javascript/42tiny-js/42tinyjs|232.703 s.|![0.654237%](http://progressed.io/bar/0)|0 pt|

- AMD A10 3.8 GHz, 8 GiB, Windows 7 64bit.
- Compiled on VS2015 RC if possible, VS2013 elsewhere.

### to add (soon)
- @todo {
- add exe size
- add iteration benchmarks
- add string benchmarks
- add script->native round trip
- add native->script round trip
- add memory consumption
- add memory leaks
- add time to set up
- add final thoughts
- also, create a score chart, based on:
  - small
  - clean
  - type (interpreted/bytecode/jit)
  - fast
  - containers
  - OO
  - closures
  - bindings
  - 32/64bit
  - threading
  - thread-safety
  - coroutines/greenlets
  - debug capabilities
  - zero-configuration based (drag-n-drop files on solution/project)
  - ...
- }

### possible output 
```lisp
as/angelscript
fib: 5702887
1.96192 s.

as/angelscript-jit
fib: 5702887
2.00607 s.

c/c4
fib: 5702887
exit(0) cycle = 332188733
2.88247 s.

c/tcc
fib: 5702887
0.127398 s.

c/vc
fib: 5702887
0.0655255 s.

chai/chaiscript
fib: 5702887
185.719 s.

dao/dao
5702887
2.90203 s.

gm/gamemonkey
fib: 5702887
1.71352 s.

js/42tiny-js
> fib: 5702887
232.703 s.

js/duktape
fib: 5702887
9.21308 s.

jtc/jtc
5.70289e+06
49.5935 s.

jx9
5702886
41.3571 s.

lily/lily
5702887
2.03696 s.

lisp/lispy90
101.345 s.

lisp/minilisp
><function>
>5702887
()
>7.4961 s.

lisp/paren
5702887
78.547 s.

lua/lua
fib: 5702887
elapsed: 1.37400000
1.52243 s.

lua/luajit
fib: 5702887
elapsed: 0.09300000
0.0906679 s.

neko/nekovm
5702887
1.3005 s.

os/objectscript
5702887
3.7224 s.

pawn
tests\fib.p(13) : warning 237: recursive function "fibR"
fib: 5702887
0.858627 s.

pawn-asm
tests\fib.p(13) : warning 237: recursive function "fibR"
fib: 5702887
0.460578 s.

psl/psl
5702887
19.4708 s.

python/micropython
5702887
3.564 s.

python/tinypy
5702887
22.3401 s.

quakec/gmqcc
3.44478 s.

ruby/mruby
5702887
2.40104 s.

ruby/tinyrb
5702887
1.55396 s.

scheme/s9
fib: 5702887          36.9632 s.

scheme/tinyscheme
fib: 5702887          72.6125 s.

squirrel/squirrel3
fib: 5702887
3.1019 s.

tcl/picol
5702887
163.352 s.

wren
5702887
elapsed: 2.128
2.24041 s.
```

### upcoming
- creating a class to handle them all ([relevant](https://xkcd.com/927/))
- http://qore.org/downloads ?
- https://code.google.com/p/tart/
- https://github.com/aardappel/lobster
- https://github.com/andyfischer/circa
- https://github.com/chameco/Solid
- https://github.com/clever-lang/clever
- https://github.com/evanw/skew
- https://github.com/ex/Killa
- https://github.com/graphitemaster/gml
- https://github.com/gregtour/duck-lang
- https://github.com/H2CO3/Sparkling
- https://github.com/JarrettBillingsley/Croc
- https://github.com/jeorgun/Vivaldi
- https://github.com/kimtg/arcadia
- https://github.com/kimtg/ToyLisp
- https://github.com/LukasBanana/XieXie-2
- https://github.com/nim-lang/Nim (or https://github.com/nim-lang/csources)
- https://github.com/noct/expr
- https://github.com/perl11/potion
- https://github.com/proglangclass/vm
- https://github.com/red/red
- https://github.com/scross99/locic
- https://github.com/stevedekorte/io
- https://github.com/timburks/nu
- https://github.com/zeux/aike
- https://github.com/dkasak/tinypy-panda (or https://github.com/kjk/tinypy-kjk)

### license
- initial tests by [Lewis Van Winkle (2009)](http://codeplea.com/game-scripting-languages) (unlicensed?)
- code & other tests by r-lyeh, unlicensed
