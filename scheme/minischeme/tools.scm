;;;;	A Very Tiny Pretty Printer (VtPP) for Mini-Scheme
;;;
;;;	Date written    28-Nov-1989 by Akira Kida
;;;     Date revised    24-Jan-1990 by Atsushi Moriwaki
;;;	Date revised	17-May-1994 by Akira Kida
;;;

;; Columns of display device.
(define *pp-display-width* 80)

;; Margin of display-width
;;  8 means 80% of *pp-display-width*, i.e., if *pp-display-width* is
;;  set to 80, the result is 64. The prety-print procedure will watch
;;  for the current output column, and if the output seem to exceed
;;  this limit, it tries to insert newlines somewhere in the current
;;  sub-list. However, sometimes this may fail, and output may get even
;;  longer than *pp-display-width*.  This is a feature, not a bug. :-)
(define *pp-display-margin* 8)

;; Number of elements will possibly be displayed in one line.
;; pretty-print will never display more then this number of elements
;; on a single physical line.  There is no feature around this. :-)
(define *pp-display-elements* 12)


;;; print n spaces
(define (spaces n)
  (if (positive? n)
    (begin
      (display " ")
      (spaces (- n 1)))))


;;; get definition of a procedure or a macro
(define (getd symbol)
  (if (not (symbol? symbol))
      (error "getd: expects symbol value"))
  (let ((code (eval symbol)))
    (cond
     ;; since a closure is also a macro, we should check macro first.
     ((macro? code)
	(let ((def (get-closure-code code)))
	  (cons 'macro (list symbol def))))
     ((closure? code)
	(let ((def (get-closure-code code)))
	  (cons
	   'define
	   (cons
	    (cons symbol (car (cdr def)))
	    (cdr (cdr def))))))
     (else
      ;; if symbol is not a macro nor closure, 
      ;; we shall generate error function call code.
      (list 'error "Not a S-Expression procedure:" (list 'quote symbol))))))


;;; pretty printer main procedure
;;;
(define (pretty-print a-list)
  ; List of procedures which need exceptional handling.
  ; Structure or each element in the list is
  ; 
  ;     (name . special-indentation)
  ;
  ;     where name is a symbol and
  ;     special-indentation is an integer.
  ;
  ; #1 Standard format, non special case.
  ;  (proc
  ;    arg1
  ;    arg2
  ;    arg3)
  ;
  ; #2 Format for special-indentation == 0
  ;  (proc arg1
  ;    arg2
  ;    arg3)
  ;
  ; #3 Format for special-indentation == 1
  ;  (proc arg1
  ;        arg2
  ;        arg3)
  ;
  ; #4 Format for let style = 2
  ;  (let ((x .....)
  ;        (y .....))
  ;    <....>
  ;    <....>)
  ;
  (define exception
    '((lambda . 0) (if . 0) (and . 1)
      (or . 1) (let . 2) (case . 0)
      (define . 0) (macro . 0)
      (map . 0) (apply . 0)
      (eq? . 1) (eqv? . 1) (set! . 0)
      (let* . 2) (letrec . 2)
      (* . 1) (/ . 1) (+ . 1) (- . 1)
      (= . 1) (< . 1) (> . 1) (<= . 1) (>= . 1)
      (do . 2)
      (call-with-input-file . 0) (call-with-output-file . 0)))
  ; special quote abbrev.
  (define special
    '((quote 1 . "'") (quasiquote 1 . "`")
      (unquote 2 . ",") (unquote-splicing 2 . ",@"))) 
  ; calculate appropriate margins
  (define pp-margin (/ (* *pp-display-width* *pp-display-margin*) 10))
  ; check whether the number of elements exceeds n or not.
  (define (less-than-n-elements? a-list n)
    ; count elements in a-list at most (n+1)
    (define (up-to-nth a-list n c)
      (cond
        ((null? a-list) c)
        ((pair? a-list)
          (set! c (up-to-nth (car a-list) n c))
          (if (< n c)
            c
            (up-to-nth (cdr a-list) n c)))
        (else (+ c 1))))
    (< (up-to-nth a-list n 0) n))
  ; check if the length is fit within n columns or not.
  (define (fit-in-n-width? a-list n)
    (< (print-width a-list) n))
  ; indent and pretty-print
  (define (do-pp a-list col)
    (spaces col)
    (pp-list a-list col 2))
  ;; main logic.
  (define (pp-list a-list col step)
    (cond
      ((atom? a-list) (write a-list))     ; atom
      ((and (assq (car a-list) special)
            (pair? (cdr a-list))
            (null? (cddr a-list)))   ; check for proper quote etc.
        (let ((s (assq (car a-list) special)))
          (display (cddr s))             ; display using abbrev.
          (pp-list
            (cadr a-list)
            (+ col (- (print-width (cddr s)) 2))
            (cadr s))))
      ((and (less-than-n-elements? a-list *pp-display-elements*)
	    (fit-in-n-width? a-list (- pp-margin col)))
	(display "(")
	(do-pp (car a-list) 0)
	(pp-args #f (cdr a-list) 1))
      (else                               ; long list.
        (let* ((sym (car a-list))
               (ex-col (assq sym exception)))
          (if (pair? ex-col)              ; check for exception.,
            (case (cdr ex-col)
              ((0 1)
                (display "(")
                (write sym)
                (display " ")
                (pp-list (cadr a-list) (+ col 2 (print-width sym)) 2)
                (pp-args
		  #t
                  (cdr (cdr a-list))
                  (+ col 2 (if (zero? (cdr ex-col)) 0 (print-width sym)))))
              ((2)
                (display "(")
                (write sym)
                (display " ")
                (if (symbol? (cadr a-list))
                  (begin ; named let
                    (write (cadr a-list))
                    (display " ")
                    (pp-list
                      (caddr a-list)
                      (+ col 3 (print-width sym) (print-width (cadr a-list)))
                      1)
                    (pp-args #t (cdddr a-list) (+ col 2)))
                  (begin ; usual let
                    (pp-list (cadr a-list) (+ col 2 (print-width sym)) 1)
                    (pp-args #t (cddr a-list) (+ col 2)))))
              (else
                (error "Illegal exception")))
            (begin                        ; normal case.
              (display "(")
              (pp-list (car a-list) (+ col 1) 2)
              (pp-args #t (cdr a-list) (+ col step))))))))
  ;; display arguments
  (define (pp-args nl a-list col)
    (cond
      ((null? a-list) (display ")"))
      ((pair? a-list)
	(if nl (newline))
        (do-pp (car a-list) col)
        (pp-args nl (cdr a-list) col))
      (else
        (display " . ")
        (write a-list)
        (display ")"))))
  ;;
  ;; main of pretty-print begins here.
  ;;
  (do-pp a-list 0)
  (newline))



;;; pretty print procedure(s)/macro(s).
;;;   (pretty 'a-symbol)        ; pretty print a procedure or macro
;;;   (pretty '(sym1 sym2 ...)) ; pretty print procedures and/or macros
(define (pretty symbols)
  (if (pair? symbols)
    (for-each
      (lambda (x) (pretty-print (getd x)) (newline))
      symbols)
    (pretty-print (getd symbols))))



;;; pretty print user-interface
;;;
;;; usage:
;;;  (pp sym1 sym2 ...)  ; obtain procedure/macro definitions in sequence
;;;
;;; Note: pp never evaluate its argument, so you do not have to specify
;;;       (pp 'proc-name). Use (pp proc-name) instead.
;;;
(macro pp (lambda (pp-macro)
  `(pretty ',(cdr pp-macro))))

