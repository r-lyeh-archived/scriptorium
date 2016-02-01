
**1.0.4** (2016/01/20):

- some API cleanup (removed error codes and failure cases)
- various bugfixes
- fixed profiling (coroutine-awareness)
- **added thread syntax and API (thread, subthread, sync, race, end_on)**
- improved __call metamethod behavior
- implemented thread pooling, improved stack frame pooling
- added map literal
- added variable keys to map/dict literals - "{ [a + b] = 5 }"
- implemented tests for various features (improved future stability)
- simplified SGS-CPPBC coroutine interaction
- compacted the code structure (less files and folders)
- **restored OS X support**
- fixed unary operator parsing issues
- added pseudo-globals _F (current function) and _T (current thread)
- optimized memory usage
- **implemented advanced HTML5 single-file documentation**
- created a vector graphics logo

**1.0.1** (2015/11/17):

- major API cleanup - replaced many error codes with internal messaging
- various bugfixes
- improved closure support (can now call them directly, aborting passes through and coroutine pausing works)
- dict literal - mapping shortcut "{ a, b }" = "{ a = a, b = b }"
- cppbc: interface upgrades - VALIDATE/SOURCE, coroutine-aware methods, method aliases, improved parsing
- symbol serialization
- coroutine serialization
- optimized AST heap usage (more efficient eval/code compilation in general)
- restored the old getindex/setindex/expr object interface
- added persistent registry tables (root, symbols, includes)

**0.9.7** (2015/06/03):

- partial Windows Phone 8 support
- bugfixes: mostly in cppbc
- virtual file system for script sources
- *coroutines!*
- metaobjects and source-configurable metamethods (can link any object to any backing object)
- object interface cache for simplified metaobject handling
- fixed string table, now all strings are interned
- removed source code conflicts for easier integration
- fast variable debug dumping functions
- single header implementation generator
- removed the string32 optimization interface
- free function presorting (can now use them before definition)
- cppbc: interface upgrades, auto-dumping/gcref generators
- first-non-null (??, ??=) operators
- xgmath upgrades (aabb3, mat4, quaternion, intersection tests)
- fixed MinGW linking issue

**0.9.5** (2014/05/25, updated 2014/07/11):

- full Mac OS X support
- bugfixes: lots of those (regex is finally stable)
- memory usage profiler
- map object
- syntax/VM upgrades (multi-index/property set, compatible call, unescaped strings, error suppression, named anonymous and short functions, var.arg. support)
- stdlib upgrades (core, utf8, io)
- xgmath (floatarray, matrix), sockets module upgrades
- C API upgrades (major refactor for variable pointer support, output system)
- almost fully documented the whole system, upgraded documentation system
- cppbc upgraded and fully tested with heavy C++ projects (sgs-box2d) + documented
- sgs2c experimental script developed to maximum bytecode compatibility
- code is mostly warning-free on all supported platforms

**0.9.0** (2013/11/10):

- bugfixes: empty subexpr, unary op chaining, expression writes
- implemented hooks interface
- created two kinds of profilers
- major internal refactoring (object interface & C-side handling)
- major stdlib upgrades (OS, date, I/O, formatting, iterables, system, regex)
- serialization
- the meta-global, environment switching
- unoptimized pseudo-empty statements due to possible side effects (1+funccall("x") etc.)
- documented stdlib / C API
- multiple return value capture via expression writes
- if(cond,true,false) pseudo-function (ternary op substitute)
- real closures

**0.8.5** (2013/05/01): 

- sorted MANY things out (lots of fixes and API changes)
- utf8<->utf32, big math/type lib. changes, debug inspector, json addon DLL,
- upgraded the test framework, created the monkey test, doc.gen. from MD

**0.8.2** (2013/04/15): 

- implicit "this", bytecode serialization, upgraded AST code,
- DLL loading system (Windows-only ATM), core I/O API, variable cloning,
- extended array API that is made to be sequencing-friendly, int/int=>real

**0.8.1** (2013/04/04): 

- important stability and main API improvements
- the engine is finally tested to be production-code-ready

**0.8** (2013/03/29): 

- most of string API, type API, closures, API test framework,
- improved debugging, fixed: boolean logic, div/0 error and other things

**0.7** (2013/01/28): 

- object orientation (w/ operator overloading), do-while,
- optimizations, classes, eval, foreach, fixed comparisons

**0.6** - skipped

**0.5** (2013/01/06): cleanup, lambdas, literals, "dict" container

**0.4** (2012/12/26): the initial release

