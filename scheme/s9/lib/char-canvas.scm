; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010,2012
; Placed in the Public Domain
;
; (canvas-draw canvas integer-X integer-Y char)       ==>  unspecific
; (canvas-draw-string canvas int-X int-Y string)      ==>  unspecific
; (canvas-dump canvas)                                ==>  vector
; (canvas-plot canvas integer-X integer-Y char)       ==>  unspecific
; (canvas-plot-line canvas X Y DX DY char)            ==>  unspecific
; (make-canvas int-X int-Y int-W int-H)               ==>  canvas
;
; (load-from-library "char-canvas.scm")
;
; This is a set of routines for drawing characters and lines on
; a scaled, character-based (a.k.a. "ASCII Art") canvas.
;
; MAKE-CANVAS creates a char canvas with a physical size of
; x=INT-X times y=INT-Y characters. The virtual size of the
; canvas is INT-W (width) times INT-H (height) "pixels". "Real
; coordinates" relate to the physical size of the canvas.
; "Virtual coordinates" are translated to real coordinates by
; scaling. Both types of coordinates are specified in X/Y
; notation. The origin 0/0 is at the lower left corner of the
; canvas. The new canvas will be filled with blanks initially.
;
; CANVAS-DRAW draws character CHAR at position INTEGER-X/INTEGER-Y.
; It uses real coordinates. CANVAS-DRAWSTRING draws a string
; instead of a single character. When the X or Y coordinate is
; outside of the canvas, C will not be drawn. When STRING extends
; beyond the limits of the canvas, it will be clipped.
;
; CANVAS-PLOT draws the character CHAR at the virtual position
; INTEGER-X/INTEGER-Y. CANVAS-PLOT-LINE draws a line from the
; virtual position X/Y to DX/DY using the character CHAR. All
; arguments must be integers. Lines originating or extending
; outside of the canvas will be clipped.
;
; CANVAS-DUMP returns a vector of strings that contain the
; characters written to the canvas. The vector indexes are the
; Y-coordinates, the string offsets the X-coordinates.
;
; Example:   (let ((c (make-canvas 10 5 10 10)))
;              (canvas-plot-line c 0 9 9 0 #\#)
;              (canvas-plot-line c 0 0 9 9 #\*)
;              (canvas-dump c))                   ==>  #("##      **"
;                                                        "  ##  **  "
;                                                        "    **    "
;                                                        "  **  ##  "
;                                                        "**      ##")

(load-from-library "package.scm")
(load-from-library "define-structure.scm")
(load-from-library "setters.scm")

(define-structure canvas x-scale y-scale cmap)

(define make-canvas* make-canvas)

(define canvas-dump canvas-cmap)

(package char-canvas

  (:import make-canvas*
           canvas-cmap
           canvas-x-scale
           canvas-y-scale)

  (:export canvas-draw
           canvas-draw-string
           canvas-plot
           canvas-plot-line
           make-canvas)

  (:make-aliases)

  (define (canvas-draw canvas x y c)
    (let* ((cmap (canvas-cmap canvas))
           (k    (vector-length cmap)))
      (if (and (<= 0 y (- k 1))
               (<= 0 x (- (string-length (vector-ref cmap 0)) 1)))
          (string-set! (vector-ref cmap (- k y 1)) x c))))
  
  (define (canvas-draw-string canvas x y s)
    (let* ((ks (string-length s))
           (line (vector-ref (canvas-cmap canvas) 
                             (- (vector-length (canvas-cmap canvas))
                                y
                                1)))
           (kl (string-length line)))
      (do ((x x (+ 1 x))
           (i 0 (+ 1 i)))
          ((or (>= i ks)
               (>= x kl)))
        (string-set! line x (string-ref s i)))))
  
  (define (canvas-plot canvas x y c)
    (let ((x (quotient (* x (car (canvas-x-scale canvas)))
                       (cadr (canvas-x-scale canvas))))
          (y (quotient (* y (car (canvas-y-scale canvas)))
                       (cadr (canvas-y-scale canvas)))))
      (canvas-draw canvas x y c)))
  
  (define (ratio x y)
    (if (zero? y)
        '(0 0)
        (let ((g (gcd x y)))
          (list (quotient x g) (quotient y g)))))
  
  (define (rat+ x y)
    (let ((den  (* (cadr x) (cadr y)))
          (numx (* (car x) (cadr y)))
          (numy (* (car y) (cadr x))))
      (ratio (+ numx numy)
             den)))

  (define (rat>=1/2 x)
    (>= (* 2 (car x)) (cadr x)))

  (define (canvas-plot-line canvas x0 y0 xn yn c)
    (let ((x0 (quotient (* x0 (car (canvas-x-scale canvas)))
                        (cadr (canvas-x-scale canvas))))
          (y0 (quotient (* y0 (car (canvas-y-scale canvas)))
                        (cadr (canvas-y-scale canvas))))
          (xn (quotient (* xn (car (canvas-x-scale canvas)))
                        (cadr (canvas-x-scale canvas))))
          (yn (quotient (* yn (car (canvas-y-scale canvas)))
                        (cadr (canvas-y-scale canvas))))
          (steep (> (abs (- yn y0))
                    (abs (- xn x0)))))
      (if steep
          (begin (swap! x0 y0)
                 (swap! xn yn)))
      (if (> x0 xn)
          (begin (swap! x0 xn)
                 (swap! y0 yn)))
      (let ((dx (- xn x0))
            (dy (abs (- yn y0))))
        (let ((de (ratio dy dx))
              (ys (if (< y0 yn) 1 -1)))
          (let plot ((x x0)
                     (y y0)
                     (e '(0 1)))
            (if (<= x xn)
                (begin (if steep
                           (canvas-draw canvas y x c)
                           (canvas-draw canvas x y c))
                (let ((e (rat+ e de)))
                  (if (rat>=1/2 e)
                      (begin (set! y (+ y ys))
                             (set! e (rat+ e '(-1 1)))))
                  (plot (+ 1 x)
                        y
                        e)))))))))
  
  (define (make-canvas x-max y-max v-x-max v-y-max)
    (let* ((x-scale (ratio x-max v-x-max))
           (y-scale (ratio y-max v-y-max)))
      (make-canvas* x-scale
                    y-scale
                    (let ((v (make-vector y-max)))
                      (do ((i 0 (+ 1 i)))
                          ((= i y-max))
                        (vector-set! v i (make-string x-max #\space)))
                      v)))))
