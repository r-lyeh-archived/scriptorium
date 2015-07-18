scriptorium
===========

- quick and dirty benchmark suite (dont take it too serious)
- benchmarked latest versions at the time of writing (Jul 2015)

### Languages or implementations evaluated: 34
- angelscript
- angelscript (angelscript-jit)
- c (c4)
- c (native)
- c (tcclib)
- chaiscript
- dao
- gamemonkey 
- gml
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

### Ranking
|Language|Time|Relative Lua speed|Score|
|:-------|---:|:----------------:|----:|
|[c/vc](https://www.visualstudio.com/)| 0.074 s.|![100%](http://progressed.io/bar/100?title=x18.0)|1808 pt|
|[lua/luajit](https://github.com/LuaDist/luajit)| 0.111 s.|![100%](http://progressed.io/bar/100?title=x12.0)|1203 pt|
|[c/libtcc](http://bellard.org/tcc/)| 0.151 s.|![100%](http://progressed.io/bar/100?title=x08.8)|886 pt|
|[pawn/asm](http://www.compuphase.com/pawn/pawn.htm)| 0.384 s.|![100%](http://progressed.io/bar/100?title=x03.4)|349 pt|
|[pawn](http://www.compuphase.com/pawn/pawn.htm)| 0.719 s.|![100%](http://progressed.io/bar/100?title=x01.8)|186 pt|
|[neko/nekovm](https://github.com/HaxeFoundation/neko)| 1.104 s.|![100%](http://progressed.io/bar/100?title=x01.2)|121 pt|
|[lua](https://github.com/LuaDist/lua)| 1.341 s.|![100%](http://progressed.io/bar/100)|100 pt|
|[ruby/tinyrb(ist)](https://github.com/sanchapereira/tinyrb-ist)| 1.441 s.|![93.0103%](http://progressed.io/bar/93)|93 pt|
|[gamemonkey](http://www.gmscript.com/)| 1.691 s.|![79.2907%](http://progressed.io/bar/79)|79 pt|
|[as/angelscript-jit](https://github.com/BlindMindStudios/AngelScript-JIT-Compiler)| 1.859 s.|![72.1132%](http://progressed.io/bar/72)|72 pt|
|[wren](https://github.com/munificent/wren)| 1.997 s.|![67.1361%](http://progressed.io/bar/67)|67 pt|
|[lily](https://github.com/jesserayadkins/lily)| 2.005 s.|![66.8582%](http://progressed.io/bar/66)|66 pt|
|[as/angelscript](http://www.angelcode.com/angelscript/)| 2.039 s.|![65.7295%](http://progressed.io/bar/65)|65 pt|
|[ruby/mruby](https://github.com/mruby/mruby)| 2.098 s.|![63.893%](http://progressed.io/bar/63)|63 pt|
|[squirrel](http://squirrel-lang.org/)| 2.126 s.|![63.0622%](http://progressed.io/bar/63)|63 pt|
|[c/c4](https://github.com/rswier/c4)| 2.538 s.|![52.8101%](http://progressed.io/bar/52)|52 pt|
|[python/micropython](https://github.com/micropython/micropython)| 2.842 s.|![47.1675%](http://progressed.io/bar/47)|47 pt|
|[dao](https://github.com/daokoder/dao)| 2.876 s.|![46.6166%](http://progressed.io/bar/46)|46 pt|
|[quakec/gmqcc](https://github.com/graphitemaster/gmqcc)| 3.060 s.|![43.806%](http://progressed.io/bar/43)|43 pt|
|[objectscript](https://github.com/unitpoint/objectscript)| 3.108 s.|![43.1278%](http://progressed.io/bar/43)|43 pt|
|[lisp/minilisp](https://github.com/rui314/minilisp)| 6.951 s.|![19.2855%](http://progressed.io/bar/19)|19 pt|
|[js/duktape](https://github.com/svaarala/duktape)| 9.544 s.|![14.0463%](http://progressed.io/bar/14)|14 pt|
|[gml](https://github.com/graphitemaster/gml)|16.443 s.|![8.15268%](http://progressed.io/bar/8)|8 pt|
|[psl](https://github.com/Silica/PSL)|17.645 s.|![7.59738%](http://progressed.io/bar/7)|7 pt|
|[python/tinypy(panda)](https://github.com/dkasak/tinypy-panda)|21.799 s.|![6.14937%](http://progressed.io/bar/6)|6 pt|
|[scheme/s9](http://www.t3x.org/s9fes/)|33.160 s.|![4.04257%](http://progressed.io/bar/4)|4 pt|
|[jx9](http://jx9.symisc.net/)|43.598 s.|![3.07476%](http://progressed.io/bar/3)|3 pt|
|[jtc](https://github.com/progschj/jtc)|47.021 s.|![2.8509%](http://progressed.io/bar/2)|2 pt|
|[scheme/tinyscheme](http://tinyscheme.sourceforge.net/home.html)|65.398 s.|![2.04979%](http://progressed.io/bar/2)|2 pt|
|[lisp/paren](https://bitbucket.org/ktg/paren)|72.901 s.|![1.83883%](http://progressed.io/bar/1)|1 pt|
|[lisp/lispy90](http://howtowriteaprogram.blogspot.com.es/2010/11/lisp-interpreter-in-90-lines-of-c.html)|91.767 s.|![1.46079%](http://progressed.io/bar/1)|1 pt|
|[tcl/picol](http://wiki.tcl.tk/17893)|151.527 s.|![0.884674%](http://progressed.io/bar/0)|0 pt|
|[chaiscript](https://github.com/ChaiScript/ChaiScript)|175.038 s.|![0.765845%](http://progressed.io/bar/0)|0 pt|
|[js/42tiny-js](https://github.com/ardi69/42tiny-js)|227.170 s.|![0.590096%](http://progressed.io/bar/0)|0 pt|

- AMD A10 3.8 GHz, 8 GiB, Windows 7 64bit.
- Compiled on VS2015 RC if possible, VS2013 elsewhere.

### Language requirements
- must embed from C++.
- must compile on vs2015 (or vs2013 at least).
- must link statically.
- must not require (heavy) makefiles/cygwin/build-systems to build.
- must be self-contained (no LLVM).
- must use jit/optimizations if available.
- must compare fair to other languages. for example:
  - must not use yield/coroutines on recursive fibonacci test.
  - must disable threading if possible (not all languages are thread-safe)
  - etc

### To add (soon)
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

### Possible output 
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

### Upcoming
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

### License
- initial tests by [Lewis Van Winkle (2009)](http://codeplea.com/game-scripting-languages) (unlicensed?)
- makefiles, bench code & other tests put into public domain (@r-lyeh)
