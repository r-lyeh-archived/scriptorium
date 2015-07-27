@echo.
@echo tinyvm
@bench 5 tinyvm\tvm tests\fib.tvm //TinyVM,[tinyvm](https://github.com/jakogut/tinyvm)

@echo.
@echo c/oc
@c\oc\parse < tests\fib.oc > tests\fib.ooc 
rem @bench 5 c\oc\interp //[C/OC](http://exmortis.narod.ru/src_pcode_eng.html) < tests\fib.ooc 
