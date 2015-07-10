; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010,2012
; Placed in the Public Domain
;
; (runtime-stats procedure <option> ...)  ==>  list
;
; Measure the runtime of the unary PROCEDURE when applied to
; a range of values (see below). RUNTIME-STATS returns a list
; containing the data it gathered. Each entry of the list has
; the following form:
;
;       (value (seconds microsecs) reductions conses nodes gcs)
;
; VALUE is the valued passed to the procedure. The sum of SECONDS
; and MICROSECS is the time the procedure took to complete.
; REDUCTIONS is the number of reductions (i.e.: primitive S9
; operations) that the compuation of the procedure value took.
; CONSES and NODES are the number of cons cells and the total
; amount of storage allocated, respectively. GCs is the number
; of garbage collections performed during the computation.
;
; The following options are used to pass ranges to RUNTIME-STATS:
;
; 'START: INTEGER  The smallest value to be measured.
; 'END: INTEGER    The largest value to be measured.
; 'STEP: INTEGER   The increment between values.
; 'SET: LIST       A set of values.
;
; There options are used to control the output of RUNTIME-STATS:
;
; 'PLOT: SYMBOL    Instead of returning the data, plot them.
;                  SYMBOL is used to specify the field to plot:
;                  'VALUE, 'TIME, 'REDUCTIONS, 'CONSES, 'STORAGE
;                  'GC.
; 'WIDTH: INTEGER  The width and height of the graph printed by
; 'HEIGHT: INTEGER the 'PLOT: option (default: h x w = 77x22 plus
;                  border).
; 'TABLE: #T       Instead of returning the data, print a nicely
;                  formatted table containing the values.
; 'COMPRESS: #F    By default RUNTIME-STATS will compress the curve
;                  so that the graph will always fill the entire
;                  X-range. Setting this value to #F will disable
;                  compression.
;
; (Example): (runtime-stats (lambda (x) (expt 2 x))
;                           'start: 1000 'end: 10000 'step: 1000)
;              ==>  (( 1000 (0  23755) 276   96237   97242  0)
;                    ( 2000 (0  86518) 297  353246  354328  3)
;                    ( 3000 (0 177415) 322  783333  784501  6)
;                    ( 4000 (0 301546) 318 1354943 1356102 11)
;                    ( 5000 (0 477758) 335 2140334 2141561 18)
;                    ( 6000 (0 681763) 343 2935950 2937195 25)
;                    ( 7000 (0 923639) 343 4145271 4146516 36)
;                    ( 8000 (1 180418) 339 5293779 5295015 46)
;                    ( 9000 (1 519193) 356 6806279 6807583 60)
;                    (10000 (1 855957) 356 8327850 8329154 73))

(require-extension sys-unix)

(load-from-library "package.scm")
(load-from-library "keyword-value.scm")
(load-from-library "char-plot.scm")
(load-from-library "position.scm")
(load-from-library "count.scm")
(load-from-library "format.scm")

(package *runtime-stats*

  (:import keyword-value
           accept-keywords
           char-plot
           posq
           count
           format)

  (:export runtime-stats)

  (:make-aliases)

  (define data-names '(value time reductions conses storage gcs))

  (define (map-data-ref data what)
    (let* ((pos  (posq what data-names))
           (data (map (lambda (x)
                        (list-ref x pos))
                      data)))
      (if (eq? 'time what)
          (map (lambda (time)
                 (+ (cadr time)
                    (* 1000000 (car time))))
               data)
          data)))

  (define (print-stats data)
    (letrec
      ((digits
         (lambda (x)
           (let loop ((i 1)
                      (v 9))
             (if (< v x)
                 (loop (+ 1 i) (+ 9 (* 10 v)))
                 (+ i (quotient i 3)))))))
      (let* ((size   #f)
             (data*  (map (lambda (name)
                            (map-data-ref data name))
                          data-names))
             (data*  (map (lambda (rec)
                            (map (lambda (x)
                                   (if (pair? x)
                                       (begin (set! size #t)
                                              (count x))
                                       x))
                                 rec))
                          data*))
             (width* (map (lambda (x)
                            (max 4 (digits (apply max x))))
                          data*)))
        (for-each (lambda (v w)
                    (format #t "~V@A" (+ 2 w) v))
                  (list (if size "SIZE" "VAL") "USEC" "REDN" "CONS"
                        "STOR" "GCS")
                  width*)
        (newline)
        (for-each (lambda (v*)
                    (for-each (lambda (v w)
                                (format #t "~V:D" (+ 2 w) v))
                              v*
                              width*)
                    (newline))
                  (transpose data*)))))

  (define (run-stats proc val)
    (letrec
      ((sval->integer
         (lambda (sval)
           (let loop ((sval sval)
                      (int  0))
             (if (null? sval)
                 int
                 (loop (cdr sval)
                       (+ (* 1000 int)
                          (car sval)))))))
       (seconds
         (lambda (t0 tn)
           (let ((d (- (car tn) (car t0))))
             (if (< (cadr tn) (cadr t0))
                 (- d 1)
                 d))))
       (useconds
         (lambda (t0 tn)
           (if (< (cadr tn) (cadr t0))
               (- 1000000 (- (cadr t0) (cadr tn)))
               (- (cadr tn) (cadr t0))))))
      (let* ((t0    (sys:gettimeofday))
             (sval* (cdr (stats `(,proc ',val))))
             (tn    (sys:gettimeofday))
             (time  (list (seconds t0 tn)
                          (useconds t0 tn))))
        `(,val ,time ,@(map sval->integer sval*)))))

  (define (runtime-stats proc . opts)
    (accept-keywords "runtime-stats"
                     opts
                     '(start: end: step: set: plot: table:
                       height: width: compress:))
    (let* ((start  (keyword-value opts 'start: 1))
           (end    (keyword-value opts 'end: 10))
           (step   (keyword-value opts 'step: 1))
           (set    (keyword-value opts 'set: #f))
           (plot   (keyword-value opts 'plot: #f))
           (height (keyword-value opts 'height: 22))
           (width  (keyword-value opts 'width: 77))
           (table  (keyword-value opts 'table: #f))
           (compr  (keyword-value opts 'compress: #t))
           (data   (if set
                       (let loop ((set set)
                                  (res '()))
                         (if (null? set)
                             (reverse! res)
                               (loop (cdr set)
                                     (cons (run-stats proc (car set))
                                           res))))
                       (let loop ((i   start)
                                  (res '()))
                         (if (> i end)
                             (reverse! res)
                               (loop (+ i step)
                                     (cons (run-stats proc i)
                                           res)))))))
      (cond (plot
              (char-plot (map-data-ref data plot)
                         plot
                         height
                         width
                         compr))
            (table
              (print-stats data))
            (else
              data)))))
