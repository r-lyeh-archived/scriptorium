cl /Feasjit.exe /DUSE_JIT -I jit jit\as_jit.cpp jit\virtual_asm_windows.cpp jit\virtual_asm_x86.cpp main.cpp -I sdk\angelscript\include -I sdk\add_on\ sdk\angelscript\source\*.cpp /EHsc /DAS_USE_STLNAMES=1 sdk\add_on\scriptstdstring\*.cpp sdk\add_on\scriptarray\*.cpp /Ox /Oy /MT /DNDEBUG winmm.lib 
cl /Feas.exe main.cpp -I sdk\angelscript\include -I sdk\add_on\ sdk\angelscript\source\*.cpp /EHsc /DAS_USE_STLNAMES=1 sdk\add_on\scriptstdstring\*.cpp sdk\add_on\scriptarray\*.cpp /Ox /Oy /MT /DNDEBUG winmm.lib 

del *.obj
