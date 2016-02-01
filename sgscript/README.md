# SGScript v1.0.4

## Usage

- MinGW/GNU Make/GCC/Clang users:
    * compile the makefile (add mode=release to get the release build)
    * link with -lsgscript from the bin/ directory (-Lbin)
- VC10+ users: project file is in build/vc10/SGScript
- XCode users: project file is in build/xcode
- include ```src/sgscript.h``` in your project and link with ```libsgscript.a```

## Sample Code and Documentation

Look in ```examples/```, ```tests/``` and ```docs/``` directories.

To build local HTML5 documentation, use "make docs".

More sample code can be found in documentation: http://www.sgscript.org/docs/sgscript.docs/code-samples-sgscript

## Features

- A C-like syntax
- The usual stuff (while/do-while/for/foreach, expressions, variables etc.)
- Highly optimized, register-based virtual machine
- Mixed memory management (ref.counting + GC)
- Extensive native debugging features
- **Coroutines, threads, advanced sync features**
- **Interactive debug inspector add-on**
- **Function/instruction execution time and memory usage profiler add-on**
- 10 data types (with lots of space for extensions):
    * null, bool, int, real, string, function, C function, object, pointer, thread
- Tests:
    * testing framework is in ext/sgstest.c => bin/sgstest ("make test" to run)
    * API testing framework is in ext/sgsapitest.c => bin/sgsapitest ("make apitest" to run)
    * C++/BC testing framework is in ext/sgscppbctest.cpp/h => bin/sgscppbctest ("make cppbctest" or "make cppbctest11" to run)
- Object-oriented constructs (dict, class, closure, "compatible call", overloadable operators)

## Bugs

If you think you've found a bug, please create a new issue on GitHub.

Don't forget to include a test sample, as small as possible!

## Future plans

- many additions have been implemented so there's nothing for now...
- got a suggestion? write some sample code (in the form of a test) and send it here

## Credits

- developer: Arvīds Kokins (snake5)
    * I can be reached at snake5creator [at] GMail

