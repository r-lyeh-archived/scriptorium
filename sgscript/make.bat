cl /Fesgs.exe src\*.c ext\sgsvm.c -I src ext\*_i*.c ext\*_prof*.c /Ox /Oy /MT /DNDEBUG 
sgs fib.sgs 
