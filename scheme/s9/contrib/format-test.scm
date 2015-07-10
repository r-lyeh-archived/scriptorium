; FORMAT Version 2.2 conformance test
;
; Apparently by Dirk Lutzebaeck, Ken Dickey, and Aubrey Jaffer.
;
; Test run: (load "format.scm") (load "format-test.scm")

(define fails 0)
(define total 0)
(define show-test #f)    ; set this to #t if you like

(define (test format-args out-str)
  (set! total (+ total 1))
  (let ((format-out (apply format `(#f ,@format-args))))
    (if (string=? out-str format-out)
        (if show-test
            (begin
              (display "Verified ")
              (write format-args)
              (newline)))
        (begin
          (set! fails (+ fails 1))
          (if (not show-test) (newline))
          (display "Failed ")
          (write format-args)
          (display " returns ")
          (write format-out)
          (display " instead of ")
          (write out-str)
          (newline)))))

; any object test

(test '("abc") "abc")
(test '("~a" 10) "10")
(test '("~a" -1.2) "-1.2")
(test '("~a" a) "a")
(test '("~a" #t) "#t")
(test '("~a" #f) "#f")
(test '("~a" "abc") "abc")
(test '("~a" #(1 2 3)) "#(1 2 3)")
(test '("~a" ()) "()")
(test '("~a" (a)) "(a)")
(test '("~a" (a b)) "(a b)")
(test '("~a" (a (b c) d)) "(a (b c) d)")
(test '("~a" (a . b)) "(a . b)")
(test '("~a" (a (b c . d))) "(a (b c . d))")
(test `("~a" ,display) "#<procedure>")
(test `("~a" ,(current-input-port)) "#<input-port>")
(test `("~a" ,(current-output-port)) "#<output-port>")

; # argument test

(test '("~a ~a" 10 20) "10 20")
(test '("~a abc ~a def" 10 20) "10 abc 20 def")

; numerical test

(test '("~d" 100) "100")
(test '("~x" 100) "64")
(test '("~o" 100) "144")
(test '("~b" 100) "1100100")
(test '("~@d" 100) "#d100")
(test '("~@x" 100) "#x64")
(test '("~@o" 100) "#o144")
(test '("~@b" 100) "#b1100100")
(test '("~10d" 100) "       100")
(test '("~:d" 12345678) "12,345,678")
(test '("~-6d" 12345678) "<45678")
(test '("~10,'*d" 100) "*******100")
(test '("~10,,'|:d" 12345678) "12|345|678")
(test '("~10,,,2:d" 12345678) "12,34,56,78")
(test '("~14,'*,'|,4:@d" 12345678) "***#d1234|5678")
(test '("~10r" 100) "100")
(test '("~2r" 100) "1100100")
(test '("~8r" 100) "144")
(test '("~16r" 100) "64")
(test '("~16,10,'*r" 100) "********64")

; character test

(test '("~c" #\a) "a")
(test '("~@c" #\a) "#\\a")
(test `("~@c" ,(integer->char 32)) "#\\space")
(test `("~@c" ,(integer->char 0)) "#\\nul")
(test `("~@c" ,(integer->char 27)) "#\\esc")
(test `("~@c" ,(integer->char 127)) "#\\del")
(test `("~@c" ,(integer->char 128)) "#\\200")
(test '("~65c") "A")
(test '("~7@c") "#\\bel")

; plural test

(test '("test~p" 1) "test")
(test '("test~p" 2) "tests")
(test '("test~p" 0) "tests")
(test '("tr~@p" 1) "try")
(test '("tr~@p" 2) "tries")
(test '("tr~@p" 0) "tries")
(test '("~a test~:p" 10) "10 tests")
(test '("~a test~:p" 1) "1 test")

; tilde test

(test '("~~~~") "~~")
(test '("~3~") "~~~")

; whitespace character test

(test '("~%") "
")
(test '("~3%") "


")
(test '("~|") "")
(test '("~_~_~_") "   ")
(test '("~3_") "   ")
(test '("~t") "	")
(test '("~3t") "			")

; indirection test

(test '("~a ~? ~a" 10 "~a ~a" (20 30) 40) "10 20 30 40")
(test '("~a ~@? ~a" 10 "~a ~a" 20 30 40) "10 20 30 40")

; minimum field test

(test '("~10a" "abc") "abc       ")
(test '("~10@a" "abc") "       abc")
(test '("~10a" "0123456789abc") "0123456789abc")
(test '("~10@a" "0123456789abc") "0123456789abc")

; maximum field test

(test '("~-10a" "abc") "abc       ")
(test '("~-10a" "0123456789abc") "012345678>")
(test '("~-10a" "0123456789") "0123456789")
(test '("~-10@a" "0123456789abc") "<456789abc")
(test '("~-10@a" "0123456789") "0123456789")
(test '("~-10a" (a (b c (d e) f) g)) "(a (b c (>")

; pad character field test

(test '("~10,,,'*a" "abc") "abc*******")
(test '("~10,,,'Xa" "abc") "abcXXXXXXX")
(test '("~10,,,42a" "abc") "abc*******")
(test '("~10,,,'*@a" "abc") "*******abc")
(test '("~-10,,,'*a" "0123456789abc") "012345678>")
(test '("~10,,3,'*a" "abc") "abc*******")
(test '("~10,,3,'*a" "0123456789abc") "0123456789abc***") ; min. padchar length
(test '("~10,,3,'*@a" "0123456789abc") "***0123456789abc")
(test '("~-10,,3,'*a" "0123456789abc") "012345678>")
(test '("~-10,,3,'*@a" "0123456789abc") "<456789abc")
(test '("~10,99,3,'*a" "abc") "abc*******") ; 2nd parameter has no effect yet

; slashify test

(test '("~s" "abc") "\"abc\"")
(test '("~s" "abc \\ abc") "\"abc \\\\ abc\"")
(test '("~a" "abc \\ abc") "abc \\ abc")
(test '("~s" "abc \" abc") "\"abc \\\" abc\"")
(test '("~a" "abc \" abc") "abc \" abc")
(test '("~s" #\space) "#\\space")
(test '("~s" #\newline) "#\\newline")
(test `("~s" ,(integer->char 9)) "#\\ht")
(test '("~s" #\a) "#\\a")
(test '("~a" (a "b" c)) "(a \"b\" c)")

; read proof test

(test `("~:s" ,display) "\"#<procedure>\"")
(test `("~:a" ,display) "\"#<procedure>\"")
(test `("~:a" (1 2 ,display)) "(1 2 \"#<procedure>\")")
(test '("~:a" "abc") "abc")

; continuation line test

(test '("abc~
         123") "abc123")
(test '("abc~
123") "abc123")
(test '("abc~
") "abc")
(test '("abc~:
         def") "abc         def")
(test '("abc~@
         def")
"abc
def")

; string case conversion

(test '("~a ~(~a~) ~a" "abc" "HELLO WORLD" "xyz") "abc hello world xyz")
(test '("~a ~:(~a~) ~a" "abc" "HELLO WORLD" "xyz") "abc Hello World xyz")
(test '("~a ~@(~a~) ~a" "abc" "HELLO WORLD" "xyz") "abc Hello world xyz")
(test '("~a ~:@(~a~) ~a" "abc" "hello world" "xyz") "abc HELLO WORLD xyz")
(test '("~:@(~a~)" (a b c)) "(A B C)")
(test '("~:@(~x~)" 255) "FF")
(test '("~:@(~p~)" 2) "S")
(test `("~:@(~a~)" ,display) "#<PROCEDURE>")
(test '("~:(~a ~a ~a~) ~a" "abc" "xyz" "123" "world") "Abc Xyz 123 world")

; variable parameter

(test '("~va" 10 "abc") "abc       ")
(test '("~v,,,va" 10 42 "abc") "abc*******")

; number of remaining arguments as parameter

(test '("~#,,,'*@a ~a ~a ~a" 1 1 1 1) "***1 1 1 1")

; argument jumping

(test '("~a ~* ~a" 10 20 30) "10  30")
(test '("~a ~2* ~a" 10 20 30 40) "10  40")
(test '("~a ~:* ~a" 10) "10  10")
(test '("~a ~a ~2:* ~a ~a" 10 20) "10 20  10 20")
(test '("~a ~a ~@* ~a ~a" 10 20) "10 20  10 20")
(test '("~a ~a ~4@* ~a ~a" 10 20 30 40 50 60) "10 20  50 60")

; conditionals

(test '("~[abc~;xyz~]" 0) "abc")
(test '("~[abc~;xyz~]" 1) "xyz")
(test '("~[abc~;xyz~:;456~]" 99) "456")
(test '("~0[abc~;xyz~:;456~]") "abc")
(test '("~1[abc~;xyz~:;456~] ~a" 100) "xyz 100")
(test '("~#[no arg~;~a~;~a and ~a~;~a, ~a and ~a~]") "no arg")
(test '("~#[no arg~;~a~;~a and ~a~;~a, ~a and ~a~]" 10) "10")
(test '("~#[no arg~;~a~;~a and ~a~;~a, ~a and ~a~]" 10 20) "10 and 20")
(test '("~#[no arg~;~a~;~a and ~a~;~a, ~a and ~a~]" 10 20 30) "10, 20 and 30")
(test '("~:[hello~;world~] ~a" #t 10) "world 10")
(test '("~:[hello~;world~] ~a" #f 10) "hello 10")
(test '("~@[~a tests~]" #f) "")
(test '("~@[~a tests~]" 10) "10 tests")
(test '("~@[~a test~:p~] ~a" 10 done) "10 tests done")
(test '("~@[~a test~:p~] ~a" 1 done) "1 test done")
(test '("~@[~a test~:p~] ~a" 0 done) "0 tests done")
(test '("~@[~a test~:p~] ~a" #f done) " done")
(test '("~@[ level = ~d~]~@[ length = ~d~]" #f 5) " length = 5")
(test '("~[abc~;~[4~;5~;6~]~;xyz~]" 0) "abc")   ; nested conditionals (irrghh)
(test '("~[abc~;~[4~;5~;6~]~;xyz~]" 2) "xyz")
(test '("~[abc~;~[4~;5~;6~]~;xyz~]" 1 2) "6")

; iteration

(test '("~{ ~a ~}" (a b c)) " a  b  c ")
(test '("~{ ~a ~}" ()) "")
(test '("~{ ~a ~5,,,'*a~}" (a b c d)) " a b**** c d****")
(test '("~{ ~a,~a ~}" (a 1 b 2 c 3)) " a,1  b,2  c,3 ")
(test '("~2{ ~a,~a ~}" (a 1 b 2 c 3)) " a,1  b,2 ")
(test '("~3{~a ~} ~a" (a b c d e) 100) "a b c  100")
(test '("~0{~a ~} ~a" (a b c d e) 100) " 100")
(test '("~:{ ~a,~a ~}" ((a b) (c d e f) (g h))) " a,b  c,d  g,h ")
(test '("~2:{ ~a,~a ~}" ((a b) (c d e f) (g h))) " a,b  c,d ")
(test '("~@{ ~a,~a ~}" a 1 b 2 c 3) " a,1  b,2  c,3 ")
(test '("~2@{ ~a,~a ~} <~a|~a>" a 1 b 2 c 3) " a,1  b,2  <c|3>")
(test '("~:@{ ~a,~a ~}" (a 1) (b 2) (c 3)) " a,1  b,2  c,3 ")
(test '("~2:@{ ~a,~a ~} ~a" (a 1) (b 2) (c 3)) " a,1  b,2  (c 3)")
(test '("~{~}" "<~a,~a>" (a 1 b 2 c 3)) "<a,1><b,2><c,3>")
(test '("~{ ~a ~{<~a>~}~} ~a" (a (1 2) b (3 4)) 10) " a <1><2> b <3><4> 10")

; up and out

(test '("abc ~^ xyz") "abc ")
(test '("~@(abc ~^ xyz~) ~a" 10) "ABC  xyz 10")
(test '("done. ~^ ~d warning~:p. ~^ ~d error~:p.") "done. ")
(test '("done. ~^ ~d warning~:p. ~^ ~d error~:p." 10) "done.  10 warnings. ")
(test '("done. ~^ ~d warning~:p. ~^ ~d error~:p." 10 1)
      "done.  10 warnings.  1 error.")
(test '("~{ ~a ~^<~a>~} ~a" (a b c d e f) 10) " a <b> c <d> e <f> 10")
(test '("~{ ~a ~^<~a>~} ~a" (a b c d e) 10) " a <b> c <d> e  10")
(test '("abc~0^ xyz") "abc")
(test '("abc~9^ xyz") "abc xyz")
(test '("abc~7,4^ xyz") "abc xyz")
(test '("abc~7,7^ xyz") "abc")
(test '("abc~3,7,9^ xyz") "abc")
(test '("abc~8,7,9^ xyz") "abc xyz")
(test '("abc~3,7,5^ xyz") "abc xyz")

; complexity tests (oh my god, I hardly understand them myself (see CL std))

(define fmt "Items:~#[ none~; ~a~; ~a and ~a~:;~@{~#[~; and~] ~a~^,~}~].")

(test `(,fmt ) "Items: none.")
(test `(,fmt foo) "Items: foo.")
(test `(,fmt foo bar) "Items: foo and bar.")
(test `(,fmt foo bar baz) "Items: foo, bar, and baz.")
(test `(,fmt foo bar baz zok) "Items: foo, bar, baz, and zok.")

(format #t "~%~a Test~:p completed. (~a failure~:p)~2%" total fails)
