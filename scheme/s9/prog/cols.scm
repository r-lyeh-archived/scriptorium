#! /usr/local/bin/s9 -f

; cols -- format input in two columns
; by Nils M Holm, 2010
; Placed in the Public Domain
;
; Usage: cols [-l] [-s separator] [file ...]

(load-from-library "parse-optionsb.scm")
(load-from-library "displaystar.scm")
(load-from-library "read-file.scm")
(load-from-library "split.scm")
(load-from-library "appendb.scm")

(define pad-left  (option #\l #f))
(define separator (option #\s 'string " "))
(define show-help (option #\h #f))
(define options   `(,pad-left
                    ,separator
                    ,show-help))

(define (pad k s)
  (make-string (- k (string-length s)) #\space))

(define (cols)
  (let* ((col* (split (read-file)))
         (kl   (apply max (cons 0 (map string-length (car col*)))))
         (kr   (apply max (cons 0 (map string-length (cadr col*))))))
    (if (> (length (car col*))
           (length (cadr col*)))
        (if (null? (cadr col*))
            (set-car! (cdr col*) '(""))
            (append! (cadr col*) '(""))))
    (for-each (lambda (left right)
                (if (opt-val pad-left)
                    (display* (pad kl left)
                              left
                              (opt-val separator)
                              (pad kr right)
                              right
                              #\newline)
                    (display* left
                              (pad kl left)
                              (opt-val separator)
                              right
                              (pad kr right)
                              #\newline)))
              (car col*)
              (cadr col*))))

(define (usage)
  (display "Usage: cols [-l] [-s separator] [file ...]")
  (newline))

(let ((files (parse-options! (sys:command-line) options usage)))
  (if (opt-val show-help)
      (begin (display-usage
               `(""
                 ,usage
                 ""
                 "Format input in two columns"
                 ""
                 "-l         pad on the left (default: right)"
                 "-s string  column separator (default: \" \")"
                 ""))
      (sys:exit 0)))
  (if (null? files)
      (cols)
      (let loop ((files files))
        (if (not (null? files))
            (begin (with-input-from-file
                     (car files)
                     cols)
                   (loop (cdr files)))))))
