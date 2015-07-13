scriptorium
===========

- quick and dirty benchmark suite (dont take it too serious)
- benchmarked latest versions (at the time of writing, Jul 2015)
- based on original tests by [Lewis Van Winkle (2009)](http://codeplea.com/game-scripting-languages)

### results: tests/fibonacci(34)
|Language|Time|Relative Lua speed|Score|
|:-------|---:|:----------------:|----:|
|c/vc|0.0553765 s.|![100%](http://progressed.io/bar/100)|2580 pt|
|lua/luajit-2.0/src/luajit|0.0875843 s.|![100%](http://progressed.io/bar/100)|1631 pt|
|c/tcc-0.9.26/win32/tcc|0.115078 s.|![100%](http://progressed.io/bar/100)|1241 pt|
|pawn/pawn-asm|0.391965 s.|![100%](http://progressed.io/bar/100)|364 pt|
|pawn/pawn|0.718365 s.|![100%](http://progressed.io/bar/100)|198 pt|
|neko/neko|1.17825 s.|![100%](http://progressed.io/bar/100)|121 pt|
|lua|1.4289 s.|![100%](http://progressed.io/bar/100)|100 pt|
|gamemonkey/bin/gme64|1.84531 s.|![77.4341%](http://progressed.io/bar/77)|77 pt|
|angelscript/asjit|2.00508 s.|![71.264%](http://progressed.io/bar/71)|71 pt|
|wren/wren|2.0346 s.|![70.23%](http://progressed.io/bar/70)|70 pt|
|lily/lily|2.12398 s.|![67.2746%](http://progressed.io/bar/67)|67 pt|
|angelscript/as|2.13701 s.|![66.8644%](http://progressed.io/bar/66)|66 pt|
|ruby/mruby|2.23981 s.|![63.7956%](http://progressed.io/bar/63)|63 pt|
|squirrel/sq3|2.28947 s.|![62.4118%](http://progressed.io/bar/62)|62 pt|
|c/c4/c4|2.67952 s.|![53.3267%](http://progressed.io/bar/53)|53 pt|
|dao/dao|2.95773 s.|![48.3107%](http://progressed.io/bar/48)|48 pt|
|quakec/gmqcc/qcvm|3.17975 s.|![44.9375%](http://progressed.io/bar/44)|44 pt|
|python/mpython|3.18645 s.|![44.843%](http://progressed.io/bar/44)|44 pt|
|os/os|3.35543 s.|![42.5847%](http://progressed.io/bar/42)|42 pt|
|lisp/minilisp/minilisp|7.28049 s.|![19.6264%](http://progressed.io/bar/19)|19 pt|
|javascript/duktape/duktape|9.66474 s.|![14.7847%](http://progressed.io/bar/14)|14 pt|
|psl/psl|18.4611 s.|![7.74006%](http://progressed.io/bar/7)|7 pt|
|s9|35.4354 s.|![4.03241%](http://progressed.io/bar/4)|4 pt|
|jx9/jx9|39.8986 s.|![3.58133%](http://progressed.io/bar/3)|3 pt|
|jtc/jtc|50.5903 s.|![2.82445%](http://progressed.io/bar/2)|2 pt|
|scheme|70.072 s.|![2.03919%](http://progressed.io/bar/2)|2 pt|
|paren-1.9.6|77.1733 s.|![1.85155%](http://progressed.io/bar/1)|1 pt|
|lisp/lispy90/lispy90|100.415 s.|![1.42299%](http://progressed.io/bar/1)|1 pt|
|tcl/picol/picol|161.644 s.|![0.88398%](http://progressed.io/bar/0)|0 pt|
|chaiscript/chai|189.937 s.|![0.752302%](http://progressed.io/bar/0)|0 pt|
|javascript/42tiny-js/42tinyjs|243.497 s.|![0.586824%](http://progressed.io/bar/0)|0 pt|

- AMD A10 3.8 GHz, 8 GiB, Windows 7 64bit.
- Compiled on VS2015 RC if possible, VS2013 elsewhere.

### language requirements
- must embed from C++.
- must compile on vs2013/vs2015.
- must link statically.
- must not require makefiles/cygwin/build-systems to build.
- must use jit/optimizations if possible.
- must compare fair to other languages. for example:
  - must not use yield/coroutines on recursive fibonacci test.
  - must disable threading if possible (not all languages are thread-safe)
  - etc

### languages evaluated (31)
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
- quakec (gmqcc)
- ruby (mruby)
- scheme (s9)
- scheme (tinyscheme)
- squirrel (squirrel3)
- tcl (picol)
- wren

### todo (soon)
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
2.13701 s.

as/angelscript-jit
fib: 5702887
2.00508 s.

c/c4
fib: 5702887
exit(0) cycle = 332188733
2.67952 s.

c/tcc
fib: 5702887
0.115078 s.

c/vc
fib: 5702887
0.0553765 s.

chai/chaiscript
fib: 5702887
189.937 s.

dao/dao
5702887
2.95773 s.

gm/gamemonkey
fib: 5702887
1.84531 s.

js/42tiny-js
> fib: 5702887
243.497 s.

js/duktape
fib: 5702887
9.66474 s.

jtc/jtc
5.70289e+06
50.5903 s.

jx9
5702886
46.8986 s.

lily/lily
5702887
2.12398 s.

lisp/lispy90
100.415 s.

lisp/minilisp
><function>
>5702887
()
>7.28049 s.

lisp/paren
5702887
77.1733 s.

lua/lua
fib: 5702887
elapsed: 1.34000000
1.4289 s.

lua/luajit
fib: 5702887
elapsed: 0.09200000
0.0875843 s.

neko/nekovm
5702887
1.17825 s.

os/objectscript
5702887
3.35543 s.

pawn
tests\fib.p(13) : warning 237: recursive function "fibR"
fib: 5702887
0.718365 s.

pawn-asm
tests\fib.p(13) : warning 237: recursive function "fibR"
fib: 5702887
0.391965 s.

psl/psl
5702887
18.4611 s.

python/micropython
5702887
3.18645 s.

quakec/gmqcc
3.17975 s.

ruby/mruby
5702887
elapsed: 2.243128
2.23981 s.

scheme/s9
fib: 5702887          35.4354 s.

scheme/tinyscheme
fib: 5702887          70.072 s.

squirrel/squirrel3
fib: 5702887
2.28947 s.

tcl/picol
5702887
161.644 s.

wren
5702887
elapsed: 2.039
2.0346 s.
```

### upcoming
- https://github.com/andyfischer/circa
- https://github.com/chameco/Solid
- https://github.com/perl11/potion
- https://github.com/aardappel/lobster
- https://github.com/gregtour/duck-lang
- https://github.com/clever-lang/clever
- http://qore.org/downloads ?
- https://code.google.com/p/tart/
- creating a class to handle them all ([relevant](https://xkcd.com/927/))
