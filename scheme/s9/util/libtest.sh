#!/bin/sh

# Scheme 9 from Empty Space
# By Nils M Holm, 2008-2012
# Generate Library Test Suite

testfile=util/libtest.scm

trap '
	cleanup
	exit 1
' 1 2 3 15

cleanup() {
	rm -f $testfile
}

cat >$testfile <<EOT
; Automatically generated test suite,
; See libtest.sh for details.

(load-from-library "syntax-rules.scm")

(define Errors 0)

(define (check src expr result)
;  (write src) (display " ==> ") (write expr) (newline)
  (if (not (equal? expr result))
      (begin (write src)
             (display " FAILED!")
             (newline)
             (display "Expected: ")
             (write result)
             (newline)
             (display "But got:  ")
             (write expr)
             (newline)
             (set! Errors (+ 1 Errors)))))

(define-syntax %test
  (syntax-rules (==>)
    ((_) #t)
    ((_ expr ==> result)
       (check 'expr expr 'result))
    ((_ expr ==> result . more)
       (begin (check 'expr expr 'result)
              (%test . more)))))

EOT

cases="lib/*.scm contrib/*.scm"
if echo '*extensions*' | ./s9 | grep sys-unix >/dev/null; then
	cases="$cases ext/*.scm"
fi

for f in $cases; do
	if grep '^; Example: ' $f >/dev/null 2>&1; then
		echo "(load-from-library \"`basename $f`\")" >>$testfile
		echo "(%test" >>$testfile
		sed -ne '/^; Example: /,/^$/p' <$f | \
			sed -e '/^$/d' | \
			sed -e 's/^;..........//' >>$testfile
		echo ")" >>$testfile
		echo "" >>$testfile
	fi
done

cat >>$testfile <<EOT
(if (= 0 Errors)
    (begin (display "Everything fine!")
           (newline)))
EOT

trap '
	exit 1
' 1 2 3 15

./s9 -i test -f $testfile
