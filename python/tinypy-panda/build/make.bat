pushd ..
python setup.py blob math
popd

echo you might need to tweak tinypy.c at this point
pause 

cl /Fetinypy.exe sample.c tinypy.c /Ox /Oy /MT /DNDEBUG

del *.obj

