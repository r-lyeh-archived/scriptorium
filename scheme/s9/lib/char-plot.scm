; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2012
; Placed in the Public Domain
;
; (char-plot list symbol integer1 integer2 boolean)  ==>  unspecific
;
; CHAR-PLOT creates a character canvas (see MAKE-CANVAS), marks the
; data points in LIST with #\X and draws a line through the points
; with #\-. SYMBOL will be used to label the X axis (on which the
; data points will be distributed).
;
; INTEGER1 (height) and INTEGER2 ; (width) specify the physical
; dimenions of the char canvas. Its virtual dimensions will be
; computed in such a way that all data points can be displayed.
;
; When the BOOLEAN (compression) argument is set to #T, then the
; X axis will start at the magnitude of the least data point
; instead of zero, so that the entire width of the canvas is
; available for distributing the supplied data points.
; 
;
; (Example): (char-plot '(0 1 2 3 4 5 6 7 8 9) 'foo 7 35 #f)  ==>  unspecific
;            ; output: ----------- foo --> -----------------
;            ;         |                                 --|
;            ;         |                             --X-  |
;            ;         |                     --X---X-      |
;            ;         |                 --X-              |
;            ;         |         --X---X-                  |
;            ;         |     --X-                          |
;            ;         |X--X-                              |
;            ;         ----------- foo --> -----------------

(load-from-library "char-canvas.scm")
(load-from-library "format.scm")

(define (char-plot data label height width compr)
  (let* ((d-size  (length data))
         (d-max   (apply max data))
         (d-min   (if compr
                      (apply min data)
                      0))
         (d-range (max 1 (+ 1 (- d-max d-min))))
         (canvas  (make-canvas width height d-range d-size))
         (data    (map (lambda (x)
                         (- x d-min))
                       data)))
    (let plot1 ((px #f)
                (py #f)
                (x* data)
                (y  0))
      (if (not (null? x*))
          (begin (if (not px)
                     (canvas-plot canvas (car x*) y #\-)
                     (canvas-plot-line canvas px py (car x*) y #\-))
                 (plot1 (car x*) y (cdr x*) (+ 1 y)))))
    (let plot2 ((px #f)
                (py #f)
                (x* data)
                (y  0))
      (if (not (null? x*))
          (begin (canvas-plot canvas (car x*) y #\X)
                 (plot2 (car x*) y (cdr x*) (+ 1 y)))))
    (let* ((mid   (- (quotient width 2) 5))
           (line  (make-string (+ 2 width) #\-))
           (label (symbol->string label))
           (s1    (substring line 1 mid))
           (s2    (substring line mid (- width
                                         (string-length label)
                                         3)))
           (line  (string-append s1 " " label " --> " s2)))
      (format #t "~A~%" line)
      (let* ((bars (make-string height #\|))
             (bars (if (>= height 8)
                       (let* ((mid  (- (quotient height 2) 3))
                              (s1   (substring bars 0 mid))
                              (s2   (substring bars
                                               (- height mid)
                                               height)))
                         (string-append s1 "^input" s2))
                       bars)))
        (for-each (lambda (b x)
                    (format #t "~C~A~2:*~C~1*~%" b x))
                  (string->list bars)
                  (vector->list (canvas-dump canvas))))
      (format #t "~A" line)
      (newline))))
