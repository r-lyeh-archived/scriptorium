; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (name->file-name string)  ==>  string
;
; (load-from-library "name-to-file-name.scm")
;
; Map the given symbol name to a name that is a valid file name
; on most computer file systems. In particular, the procedure
; replaces the following characters:
; 
;         character(s)  becomes
;            @          at
;            +          plus
;            *          star
;            /          slash
;            ?          p (predicate)
;            !          b (bang)
;            =          eq
;            ->         -to-
;            <          lt
;            <=         le
;            >          gt
;            >=         ge
; 
; In addition a name that consists of a minus sign exclusively
; ("-") is replaced with "minus". All other special characters
; are replaced with an underscore. Non-special characters include
; the letters a-z, the digits 0-9, the minus sign, and the dot.
; 
; Example:    (name->file-name "sys:stat-pipe?")   ==>  "sys_stat-pipep"
;             (name->file-name "a->b")             ==>  "a-to-b"
;             (name->file-name "*foo*")            ==>  "starfoostar"

(define (name->file-name name)
  (let xlate ((in  (string->list name))
        (out '()))
    (cond ((null? in)
            (if (string=? name "-")
                "minus"
                (apply string-append (reverse! out))))
          ((char=? #\@ (car in))
            (xlate (cdr in) (cons "at" out)))
          ((char=? #\+ (car in))
            (xlate (cdr in) (cons "plus" out)))
          ((char=? #\* (car in))
            (xlate (cdr in) (cons "star" out)))
          ((char=? #\/ (car in))
            (xlate (cdr in) (cons "slash" out)))
          ((char=? #\? (car in))
            (xlate (cdr in) (cons "p" out)))
          ((char=? #\! (car in))
            (xlate (cdr in) (cons "b" out)))
          ((char=? #\= (car in))
            (xlate (cdr in) (cons "eq" out)))
          ((and (char=? #\- (car in))
                (pair? (cdr in))
                (char=? #\> (cadr in)))
            (xlate (cddr in) (cons "-to-" out)))
          ((char=? #\< (car in))
            (if (and (pair? (cdr in))
                     (char=? #\= (cadr in)))
                (xlate (cddr in) (cons "le" out))
                (xlate (cdr in) (cons "lt" out))))
          ((char=? #\> (car in))
            (if (and (pair? (cdr in))
                     (char=? #\= (cadr in)))
                (xlate (cddr in) (cons "ge" out))
                (xlate (cdr in) (cons "gt" out))))
          ((or (char-numeric? (car in))
               (char-alphabetic? (car in))
               (memv (car in) '(#\- #\.)))
            (xlate (cdr in) (cons (string (car in)) out)))
          (else
            (xlate (cdr in) (cons "_" out))))))

