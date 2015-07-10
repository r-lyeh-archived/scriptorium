Angelscript JIT Compiler
========================
A Just-In-Time Compiler for use with AngelScript.

Currently supports x86 and x86_64 processors on both Windows (using MSVC 2010 or later) and Linux (using GCC 4.6.2 or later)

Last made compatible with a version of 2.29 of the AngelScript library.

License
-------

Copyright (C) 2012-2014 Blind Mind Studios

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Utilizing the JIT
-----------------

The JIT makes extensive use of C++11 additions, such as Lambdas and the auto keyword. For GCC, use "-std=c++11" to force the new standard. MSVC 2010 is compatible with all C++11 features utilized.

This short example shows the basics of utilizing the JIT. The folder containing "angelscript.h" should be an include path in the project.
When including files into the project, choose one of "virtual_asm_windows.cpp" and "virtual_asm_linux.cpp" depending on your intended platform.

    #include "angelscript.h"
    #include "as_jit.h"

    int main() {
        asIScriptEngine* engine = asCreateScriptEngine(ANGELSCRIPT_VERSION);

        //Create the JIT Compiler. The build flags are explained below,
        //as well as in as_jit.h
        asCJITCompiler* jit = new asCJITCompiler(0);

        //Enable JIT helper instructions; without these,
        //the JIT will not be invoked
        engine->SetEngineProperty(asEP_INCLUDE_JIT_INSTRUCTIONS, 1);

        //Bind the JIT compiler to the engine
        engine->SetJITCompiler(jit);

        //Load your scripts. The JIT will allocate code pages and build
        //native code; note that some native execution will occur
        //(e.g. for global variables)
        //The JIT is thread-safe, so multiple engines can use the same
        //JIT Compiler, and multiple engines can be compiling at once
        LoadAndCompileScripts();

        //Optionally, you can finalize the JIT's code pages,
        //preventing any alteration to the native code
        jit->finalizePages();

        //Now that the JIT is in place, the scripts will be executed
        //almost entirely in native code
        RunScripts();

        //Clean up your engine. Code pages will automatically be cleared
        //by the JIT when the engine is released.
        DiscardModules();
        engine->Release();
        delete jit;

        return 0;
    }

Build Flags
-----------

*JIT_NO_SUSPEND*

The JIT will not check for suspend events. Even if the AngelScript engine is set for fewer suspensions, some will remain, so this option is still useful.

*JIT_SYSCALL_FPU_NORESET*

Disables the FPU reset around functions for platforms that always clean up the FPU. MSVC appears to work fine without FPU resets, and the result will be slightly faster.

*JIT_SYSCALL_NO_ERRORS*

If system functions never set exceptions on a script context, this produces a smaller and faster output. Setting exceptions with this option enabled will likely result in crashes.

*JIT_ALLOC_SIMPLE*

When using simple allocation (e.g. default new/delete or malloc/free) that does not read any script states, this produces smaller and faster outputs.

*JIT_NO_SWITCHES*

Disables native switch statements in the JIT. Disable this option for a smaller, but slower, output.

*JIT_NO_SCRIPT_CALLS*

Disables native script calls in the JIT. Native script calls are slightly faster, but may break on angelscript updates; disable this as a temporary workaround if they do.

*JIT_FAST_REFCOUNT*

Reduces overhead involved in reference counting. No reference counting function may alter or inspect script contexts.