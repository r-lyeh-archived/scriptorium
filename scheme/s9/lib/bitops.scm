; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010,2012
; Placed in the Public Domain
;
; (bit0  integer1 integer2 ...)  ==>  integer | #f
; (bit*  integer1 integer2 ...)  ==>  integer | #f
; (bit*c integer1 integer2 ...)  ==>  integer | #f
; (bita  integer1 integer2 ...)  ==>  integer | #f
; (bitc* integer1 integer2 ...)  ==>  integer | #f
; (bitb  integer1 integer2 ...)  ==>  integer | #f
; (bitn= integer1 integer2 ...)  ==>  integer | #f
; (bit+  integer1 integer2 ...)  ==>  integer | #f
; (bitn+ integer1 integer2 ...)  ==>  integer | #f
; (bit=  integer1 integer2 ...)  ==>  integer | #f
; (bitcb integer1 integer2 ...)  ==>  integer | #f
; (bit+c integer1 integer2 ...)  ==>  integer | #f
; (bitca integer1 integer2 ...)  ==>  integer | #f
; (bitc+ integer1 integer2 ...)  ==>  integer | #f
; (bitn* integer1 integer2 ...)  ==>  integer | #f
; (bitsl integer1 integer2 ...)  ==>  integer | #f
; (bitsr integer1 integer2 ...)  ==>  integer | #f
;
; (load-from-library "bitops.scm")
;
; Abbreviations for small-magnitude bit operations (see BIT-OP).
; The naming Scheme for these operators works as follows (the
; operation is performed pairwise on the bits of the operands):
;
; 0 indicates that a bit is set to 0 (CLEAR).
; 1 indicates that a bit is set to 1 (SET).
; * indicates a bitwise product (AND).
; + indicates a bitwise sum (OR).
; = indicates bitwise equivalence (NXOR).
; A indicates the first operand (IDENTITY).
; B indicates the second operand (IDENTITY).
; N indicates negation of the result (NOT).
; C indicates a complement.
;   When before the operator: not(A) op B
;   When after the operator:  A op not(B)
;
; Table of operations:
;
; OP    | Description
; ---------------------------------
; BIT0  | CLEAR
; BIT*  | AND
; BIT*C | AND with not(B)
; BITA  | A
; BITC* | AND with not(A)
; BITB  | B
; BITN= | XOR
; BIT+  | OR
; BITN+ | NOR
; BIT=  | NXOR, EQV
; BITCB | not(B)
; BIT+C | OR with not(B)
; BITCA | not(A)
; BITC+ | OR with not(A)
; BITN* | NAND
; BIT1  | SET
; BITSL | A shifted left by B bits
; BITSR | A shifted right by B bits
; 
; Logical NOT of X can be expressed with (BITCA x 0) or (BITCB 0 x).
; The other operand will be ignored.
;
; See the description of the BIT-OP operator for restrictions of
; these procedures.
;
; Example:   (bit0  123 456)  ==>  0
;            (bit*  127  99)  ==>  99
;            (bit+   63  64)  ==>  127
;            (bita  123 456)  ==>  123
;            (bitb  123 456)  ==>  456
;            (bitsl 123   1)  ==>  246

(load-from-library "hof.scm")

(define bit0  (curry bit-op  0))
(define bit*  (curry bit-op  1))
(define bit*c (curry bit-op  2))
(define bita  (curry bit-op  3))
(define bitc* (curry bit-op  4))
(define bitb  (curry bit-op  5))
(define bitn= (curry bit-op  6))
(define bit+  (curry bit-op  7))
(define bitn+ (curry bit-op  8))
(define bit=  (curry bit-op  9))
(define bitcb (curry bit-op 10))
(define bit+c (curry bit-op 11))
(define bitca (curry bit-op 12))
(define bitc+ (curry bit-op 13))
(define bitn* (curry bit-op 14))
(define bit1  (curry bit-op 15))
(define bitsl (curry bit-op 16))
(define bitsr (curry bit-op 17))
