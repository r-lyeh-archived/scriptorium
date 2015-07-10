; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010,2012
; Placed in the Public Domain
;
; (curs:attron integer)   ==>  unspecific
; (curs:attroff integer)  ==>  unspecific
; (curs:standout)         ==>  unspecific
; (curs:standend)         ==>  unspecific
;
; An interface to some curses routines.

(require-extension curses)

(load-from-library "bitops.scm")

(define curs:attr-normal    (curs:get-magic-value "A_NORMAL"))
(define curs:attr-standout  (curs:get-magic-value "A_STANDOUT"))
(define curs:attr-underline (curs:get-magic-value "A_UNDERLINE"))
(define curs:attr-bold      (curs:get-magic-value "A_BOLD"))

(define curs:key-backspace (curs:get-magic-value "KEY_BACKSPACE"))
(define curs:key-dc        (curs:get-magic-value "KEY_DC"))
(define curs:key-ic        (curs:get-magic-value "KEY_IC"))
(define curs:key-up        (curs:get-magic-value "KEY_UP"))
(define curs:key-down      (curs:get-magic-value "KEY_DOWN"))
(define curs:key-left      (curs:get-magic-value "KEY_LEFT"))
(define curs:key-right     (curs:get-magic-value "KEY_RIGHT"))
(define curs:key-home      (curs:get-magic-value "KEY_HOME"))
(define curs:key-end       (curs:get-magic-value "KEY_END"))
(define curs:key-ppage     (curs:get-magic-value "KEY_PPAGE"))
(define curs:key-npage     (curs:get-magic-value "KEY_NPAGE"))

(define curs:color-black   0)
(define curs:color-blue    1)
(define curs:color-green   2)
(define curs:color-cyan    3)
(define curs:color-red     4)
(define curs:color-magenta 5)
(define curs:color-yellow  6)
(define curs:color-gray    7)

(define old-attrset curs:attrset)
(define curs:attributes 0)

(define curs:attrset
  (let ((old-attrset old-attrset))
    (lambda (attr)
      (old-attrset attr)
      (set! curs:attributes attr))))

(define (curs:attron attr)
  (curs:attrset (bit+ attr curs:attributes)))

(define (curs:attroff attr)
  (curs:attrset (bit*c curs:attributes attr)))

(define (curs:standout)
  (curs:attrset curs:attr-standout))

(define (curs:standend)
  (curs:attrset curs:attr-normal))

(define (curses:curses) #t)
