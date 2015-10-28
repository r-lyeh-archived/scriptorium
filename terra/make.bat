echo requires 64bit cmd
cl terra.c -I terra\include\terra terra\lib\*.lib /Ox /Oy /MT /DNDEBUG
unzip -o terra\bin\bin.zip
