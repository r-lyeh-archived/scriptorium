scriptorium
===========

- pretty much wip benchmark suite (dont take it too serious)
- made it while evaluating embeddable scripting languages (many of them not listed here)
- original benchmarks found at codeplea.com, then extended
- no fancy stats/charts yet (come back soon)
- benchmarking latest versions (at the time of writing, Jul 2015)

### languages evaluated (30)
- angelscript
- angelscript (angelscript-jit)
- c (c4)
- c (native)
- c (tcclib)
- chaiscript
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
- os (objectscript)
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

### why? applications
- to learn about new projects
- to learn about how to compile them
- to learn about how to execute them
- to learn about how to bind native functions in them 
- to create a common class to handle them all ([relevant](https://xkcd.com/927/))

### requirements at evaluation
- must embed from C++
- must compile on vs2013/vs2015.
- must link statically
- must not require makefiles/cygwin/build-systems to build
- must use jit/optimizations if possible
- must play fair on tests 
  - do not optimize tests in your favorite language. I want recursive fibonacci.
  - do not use multithreading and/or coroutines.
  - start benchmarking after initialization is finished (@todo)

### fancy stats and charts (@todo, soon)
- reminder {
- exe size
- iteration benchmarks
- string benchmarks
- script->native round trip
- native->script round trip
- debug capabilities
- memory consumption
- memory leaks
- time to set up
- create a score chart, based on:
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
  - threadsafeness
  - coroutines/greenlets
  - zero-configuration based (drag-n-drop files on solution/project)
  - ...
- }

### final thoughts (@todo, soon)

### possible output (tests/fibonacci(34))

- AMD A10 3.8 GHz, 8 GiB, Windows 7 64bit.
- Compiled on VS2015 RC if possible, VS2013 elsewhere.

```lisp
as/angelscript
fib: 5702887 = 5702887
2.09129 s.

as/angelscript-jit
fib: 5702887 = 5702887
1.89204 s.

c/c4
fib: 5702887 = 5702887
exit(0) cycle = 332189752
2.64008 s.

c/tcc
fib: 5702887 = 5702887
0.145295 s.

c/vc
fib: 5702887 = 5702887
0.0486864 s.

chai/chaiscript
fib: 5702887=5702887
199.453 s.

gm/gamemonkey
fib: 5702887 = 5702887
1.75391 s.

js/42tiny-js
> fib: 5702887=5702887
250.004 s.

js/duktape
fib: 5702887=5702887
9.32712 s.

jtc/jtc
5.70289e+06
51.5686 s.

jx9
5702886
39.9112 s.

lily/lily
5702887
2.26246 s.

lisp/lispy90
97.9488 s.

lisp/minilisp
><function>
>5702887
()
>6.88687 s.

lisp/paren
5702887
74.5846 s.

lua/lua
fib: 5702887 = 5702887
elapsed: 1.33600000
1.35021 s.

lua/luajit
fib: 5702887 = 5702887
elapsed: 0.11200000
0.342629 s.

neko/nekovm
5702887
1.13424 s.

os/objectscript
5702887
3.03817 s.

pawn
tests\fib.p(28) : warning 237: recursive function "fibR"
fib: 5702887 = 5702887
0.745357 s.

pawn-asm
tests\fib.p(28) : warning 237: recursive function "fibR"
fib: 5702887 = 5702887
0.377765 s.

psl/psl
5702887
18.2699 s.

python/micropython
5702887
2.86286 s.

quakec/gmqcc
3.32087 s.

ruby/mruby
5702887
elapsed: 2.211127
2.24547 s.

scheme/s9
fib: 5702887 = 5702887          35.423 s.

scheme/tinyscheme
fib: 5702887 = 5702887          70.043 s.

squirrel/squirrel3
fib: 5702887 = 5702887
2.38788 s.

tcl/picol
5702887
5702887
162.193 s.

wren
5702887
elapsed: 2.102
2.10978 s.
```
