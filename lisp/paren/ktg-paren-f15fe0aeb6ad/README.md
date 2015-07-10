# Paren: The Paren Programming Language #

(C) 2013-2014 Kim, Taegyoon

Paren is a dialect of Lisp. It is designed to be an embedded language.

## Run ##
```
Usage: paren [OPTIONS...] [FILES...]

OPTIONS:
    -h    print this screen.
    -v    print version.
```

## Reference ##
```
Predefined Symbols:
 ! != #f #t % && * + ++ - -- / < <= = == > >= E PI ^ and apply begin ceil ceilin
g char-at chr cons dec def define display double eval exit expt false filter flo
or fn fold if inc int join lambda length list ln log10 map modulo newline not nt
h or pop-back! pr prn push-back! quote rand range read-line read-string set set!
 slurp spit sqrt strcat string string-append string-length string-ref strlen sys
tem thread true type while ||
Macros:
 block defn for setfn when
```

## Files ##
* libparen.h libparen.cpp: Paren language library
* paren.cpp: Paren REPL executable
* library.paren: The standard library

## Examples ##
### Hello, World! ###
```
(prn "Hello, World!")
```

### Comment ###
```
#!/usr/bin/paren
; comment
```

### Function ###

In a function, [lexical scoping](http://en.wikipedia.org/wiki/Lexical_scoping#Lexical_scoping) is used.

```
> ((fn (x y) (+ x y)) 1 2)
3 : int
> ((fn (x) (* x 2)) 3)
6 : int
> (setfn sum (x y) (+ x y))
 : nil
> (sum 1 2)
3 : int
> (fold sum (range 1 10 1))
55 : int
> (set even? (fn (x) (== 0 (% x 2))))
 : nil
> (even? 3)
false : bool
> (even? 4)
true : bool
> (apply + (list 1 2 3))
6 : int
> (map sqrt (list 1 2 3 4))
(1 1.4142135623730951 1.7320508075688772 2) : list
> (filter even? (list 1 2 3 4 5))
(2 4) : list
> (set x 1) ((fn (x) (prn x) (set x 3) (prn x)) 4) (prn x) ; lexical scoping
4
3
1
 : nil
> (set adder (fn (amount) (fn (x) (+ x amount)))) (set add3 (adder 3)) (add3 4); lexical scoping
7 : int
> (cons 1 (list 2 3))
(1 2 3) : list
```

#### Recursion ####
```
> (set factorial (fn (x) (if (<= x 1) x (* x (factorial (dec x))))))
 : nil
> (for i 1 5 1 (prn i (factorial i)))
1 1
2 2
3 6
4 24
5 120
 : nil
```

### List ###
```
> (nth 1 (list 2 4 6))
4 : int
> (length (list 1 2 3))
3 : int
```

### Macro ###
```
> (defmacro infix (a op ...) (op a ...)) (infix 3 + 4 5)
12 : int
```

### Thread ###
```
> (set t1 (thread (for i 1 10 1 (pr "" i)))) (set t2 (thread (for j 11 20 1 (pr "" j)))) (join t1) (join t2)
 1 2 3 4 5 6 11 12 13 14 15 16 17 18 7 8 9 10 19 20
```

### System Command (Shell) ###
```
(system "notepad" "a.txt") ; compatible with ParenJ
(system "notepad a.txt") ; same as above; not compatible with ParenJ
```

### Embedding ###
Compile libparen.cpp
```
#include "libparen.h"

int main() {
    libparen::init();
    cout << libparen::eval_string("(+ 1 2)")->v_int << endl; // evaluate code and get return value
    libparen::eval_string("(set a 1)"); // evaluate code
    cout << libparen::get("a")->v_int << endl; // get variable
    libparen::set("a", string("Hello")); // set variable
    cout << libparen::get("a")->v_string << endl; // get variable
}

```
=>
```
3
1
Hello
```

### More Examples ###

* [Project Euler solutions in Paren](https://bitbucket.org/ktg/euler-paren) (Some of them are for [ParenJ](https://bitbucket.org/ktg/parenj).)

* Paren [Examples](https://bitbucket.org/ktg/paren/wiki/Examples/)

* [Learn Paren in Y minutes](https://bitbucket.org/ktg/paren/wiki/Learn_Paren_in_Y_minutes/)

### [99 Bottles of Beer](http://en.wikipedia.org/wiki/99_Bottles_of_Beer) ###
```
(for i 99 1 -1
  (prn i "bottles of beer on the wall," i "bottles of beer.")
  (prn "Take one down and pass it around," (dec i) "bottle of beer on the wall."))
```

## Memory Management ##
Paren uses [Reference Counting](http://en.wikipedia.org/wiki/Reference_counting). The main advantage of reference counting over tracing garbage collection is that objects are reclaimed as soon as they can no longer be referenced, and in an incremental fashion, without long pauses for collection cycles and with clearly defined lifetime of every object. In real-time applications or systems with limited memory, this is important to maintain responsiveness.

## Alternative Implementations ##
* [Paren](https://bitbucket.org/ktg/paren) (Paren running natively)
* [ParenJ](https://bitbucket.org/ktg/parenj) (Paren running on the Java Virtual Machine)
* [ParenJS](https://bitbucket.org/ktg/parenjs) (Paren compiler targeting JavaScript)
* [Paren#](https://bitbucket.org/ktg/parensharp) (Paren running on the .Net Framework)

## License ##

   Copyright 2013-2014 Kim, Taegyoon

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

   [http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
