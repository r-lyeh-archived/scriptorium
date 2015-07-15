set OPTS=/Zi /Oy- /MD /DDEBUG
set OPTS=/Ox /Oy /MT /DNDEBUG

cl /Fegml.exe *.c -I linenoise -DGML_COMPILER="vs2015" -DGML_OS="%OS%" -DGML_TYPE="\"development\"" linenoise\linenoise.c linenoise\utf8.c  %OPTS%

del *.obj