cl /Fe42tinyjs.exe Script.cpp tinyjs*.cpp pool_allocator.cpp /DNO_THREADING /Ox /Oy /DNDEBUG /MT 
del *.obj
