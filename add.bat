@echo.
@echo c/c4-jit
@bench 5 c\c4jit\c4jit tests\fib.c //[c/c4-jit](https://github.com/EarlGray/c4)

@echo.
@echo c/oc
@c\oc\parse < tests\fib.oc > tests\fib.ooc 
rem @bench 5 c\oc\interp //[C/OC](http://exmortis.narod.ru/src_pcode_eng.html) < tests\fib.ooc 
