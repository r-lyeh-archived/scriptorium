@echo.
@echo scheme/chibi
@pushd scheme\chibi-scheme\
@..\..\bench 5 chibi ..\..\tests\fib.scm //[scheme/chibi](https://github.com/ashinn/chibi-scheme)
@popd

@echo.
@echo scheme/s7
@pushd scheme\s7\
@..\..\bench 5 s7 ..\..\tests\fib.scm //[scheme/s7](https://ccrma.stanford.edu/software/snd/snd/s7.html)
@popd


