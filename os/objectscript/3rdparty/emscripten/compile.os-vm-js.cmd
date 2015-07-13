rem OS_eval = Module.cwrap('OS_eval', null, ['string'])
rem OS_eval("for(var i=0; i < 3; i++){ var a = LangTokenizer.parseText('print 108'); print(a, \"\\n\", a[0].str, a[0].type, a[1].str, a[1].type, \"\\n\", a[0].str, a[0].type, a[1].str, a[1].type, \"\\n\", gc.allocatedBytes, gc.usedBytes, gc.numObjects) }")

rem set EMCC_DEBUG=2
rem DISABLE_EXCEPTION_CATCHING=0
rem -s OUTLINING_LIMIT=100000 
rem -s QUANTUM_SIZE=1
rem -DOS_DEBUG -s SAFE_HEAP=1 -s DOUBLE_MODE=1 -s USE_TYPED_ARRAYS=2 -s EMCC_DEBUG=1 
rem -s TOTAL_STACK=10000000 -s TOTAL_MEMORY=67108864 

"c:\Program Files\Emscripten\emscripten\1.13.0\em++.bat" -DOS_EMSCRIPTEN -DOS_NUMBER_NAN_TRICK_DISABLED -Oz --llvm-opts 3 --closure 1 -s OUTLINING_LIMIT=1000000 -s DISABLE_EXCEPTION_CATCHING=1 -s NO_EXIT_RUNTIME=1 -s EXPORTED_FUNCTIONS="['_OS_create','_OS_eval','_OS_evalFakeFile','_OS_release']" -Wswitch-enum --memory-init-file 0 -o os-vm-js.html -I..\..\src ..\..\os.cpp ..\..\src\objectscript.cpp ..\..\src\os-heap.cpp -I..\..\src\ext-url ..\..\src\ext-url\os-url.cpp -I..\..\src\ext-base64 ..\..\src\ext-base64\cdecode.cpp ..\..\src\ext-base64\cencode.cpp ..\..\src\ext-base64\os-base64.cpp -I..\..\src\ext-json ..\..\src\ext-json\os-json.cpp -I..\..\src\ext-datetime ..\..\src\ext-datetime\os-datetime.cpp