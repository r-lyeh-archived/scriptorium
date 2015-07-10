; Fast STRING-LOCATE procedure
; By Nils M Holm, 2009,2010
; Placed in the Public Domain
;
; (string-locate string1 string2)     ==>  offset | #f
; (string-ci-locate string1 string2)  ==>  offset | #f
;
; (load-from-library "string-locate.scm")
;
; Attempt to locate the pattern STRING1 in the text STRING2. Return
; the offset of the first occurrence of STRING1 in STRING2 or #F,
; if STRING2 does not contain STRING1.
;
; This program is based on "A Very Fast Substring Search Algorithm",
; Daniel M. Sunday, CACM v33, #8, August 1990 and the
; SUBSTRING-SEARCH-MAKER procedure by Ken Dickey (1990).
;
; Example:   (string-locate "test" "This is a test string")     ==>  10
;            (string-locate "TEST" "This is a test string")     ==>  #f

(define (string-locate pattern text)
  (letrec
    ((charset-len 256)
     (make-shift-table
       (lambda (pattern k)
         (let* ((shift-table (make-vector charset-len (+ 1 k)))
                (max (- k 1)))
           (let loop ((i 0))
             (vector-set! shift-table
                          (char->integer (string-ref pattern i))
                          (- k i))
             (if (< i max)
                 (loop (+ 1 i))
                 shift-table)))))
     (lookup-offset
       (lambda (tbl n)
         (vector-ref tbl (char->integer (string-ref text n))))))
    (let ((kp (string-length pattern))
          (kt (string-length text)))
      (let ((shift-table (make-shift-table pattern kp))
            (p_max (- kp 1))
            (t_max (- kt 1)))
        (let locate ((i 0))
          (if (> (+ kp i) kt)
              #f
              (let compare ((pi 0)
                            (ti i))
                (cond
                  ((> pi p_max)
                    #f)
                  ((char=? (string-ref pattern pi)
                           (string-ref text ti))
                    (if (= pi p_max)
                        i
                        (compare (+ pi 1) (+ ti 1))))
                  ((> (+ kp i) t_max)
                    #f)
                  (else
                    (locate (+ i (lookup-offset
                                   shift-table
                                   (+ i kp)))))))))))))
