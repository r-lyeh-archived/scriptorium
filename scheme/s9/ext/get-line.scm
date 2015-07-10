; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010,2012
; Placed in the Public Domain
;
; (get-line integer1 integer2 string1 string2)     ==>  string | #f
; (get-line integer1 integer2 string1 string2 #t)  ==>  string | #f
;
; GET-LINE edits a single line of text interactively.
;
; INTEGER1 (y) and INTEGER2 (x) specify the coordinates of the
; visual editing buffer on the screen. STRING1 is the initial
; content of the buffer, and STRING2 is a prompt that will be
; displayed in front of the buffer. The length of the buffer
; is unlimited; its visual representation extends to the end
; of the row on the screen. GET-LINE returns a new string with
; the edited content or #F when editing is aborted.
;
; When an additional argument of #T is passed to GET-LINE, it
; will implement a "smart default". I.e., when the first key
; pressed under the control of GET-LINE is not a motion command,
; then the text in the line buffer will be deleted and replaced
; with the character corresponding to that key.
;
; GET-LINE renders the initial content and places the cursor
; at the end of the buffer. Characters typed will be inserted
; into the buffer at cursor position. In addition, GET-LINE
; accepts the following editing commands ([^A] = [control]+[A]):
;
;       [^A]        go to beginning of buffer.       (also [Home])
;       [^E]        go to end of buffer.             (also [End])
;       [^B]        move back one character.         (also [Left])
;       [^D]        delete character under cursor.   (also [Del])
;       [^F]        move forward one character.      (also [Right])
;       [ESC]       end editing, return string.      (also [Enter])
;       [Backspace] delete character to the left.    (also [^H])
;       [^U]        delete all characters in buffer. (also [^K])
;       [^C]        Abort editing, return #F.        (also [^G])
;
; (Example): (begin (curs:initscr)
;                   (curs:raw)
;                   (curs:noecho)
;                   (curs:nonl)
;                   (get-line 0 0 "" "Enter text here: "))

(require-extension curses)

(define (get-line y x buf prompt . dflt)
  (let* ((lim  256)
         (cols (- (curs:cols) x))
         (rk   0)
         (s    buf)
         (o    (string-length prompt))
         (i    (string-length s))
         (z    i)
         (t    0)
         (dfl  (if (not (null? dflt)) 2 0))
         (spcs (make-string cols #\space))
         (clrtoeol
           (lambda (x)
             (curs:mvaddstr y x (substring spcs 0 (- cols x)))
             (curs:move y x))))
    (curs:move y x)
    (clrtoeol x)
    (curs:addstr prompt)
    (let loop ()
      (if (> (- i t) (- cols o 2))
          (set! t (- i (- cols o 2))))
      (if (< i t)
          (set! t i))
      (clrtoeol o)
      (curs:mvaddstr y o (substring s t (+ t (min (- z t)
                                                  (- cols o 2)))))
      (curs:move y (+ o (- i t)))
      (if (positive? dfl)
          (set! dfl (- dfl 1)))
      (let ((k (curs:getch)))
        (cond ((or (= k 27)
                   (= k 13))
                (curs:move y x)
                (clrtoeol x)
                s)
              ((and (<= 32 k 126)
                    (< z (- lim 1)))
                (if (positive? dfl)
                    (begin (set! i 0)
                           (set! z 0)
                           (set! s "")))
                (set! s (string-append (substring s 0 i)
                                       (string (integer->char k))
                                       (substring s i z)))
                (set! i (+ 1 i))
                (set! z (+ 1 z))
                (loop))
              ((or (= k 8)
                   (= k curs:key-backspace))
                (cond ((zero? i)
                        (cond ((zero? z)
                                (curs:move y x)
                                (curs:clrtoeol)
                                #f)
                              (else
                                (curs:beep)
                                (loop))))
                      (else
                        (set! i (- i 1))
                        (set! s (string-append (substring s 0 i)
                                               (substring s (+ 1 i) z)))
                        (set! z (- z 1))
                        (loop))))
              ((or (= k 4)
                   (= k curs:key-dc))
                (cond ((>= i z)
                        (curs:beep)
                        (loop))
                      (else
                        (set! s (string-append (substring s 0 i)
                                               (substring s (+ 1 i) z)))
                        (set! z (- z 1))
                        (loop))))
              ((or (= k 1)
                   (= k curs:key-home))
                (set! i 0)
                (loop))
              ((or (= k 5)
                   (= k curs:key-end))
                (set! i z)
                (loop))
              ((or (= k 3)
                   (= k 7))
                #f)
              ((or (= k 21)
                   (= k 11))
                (set! i 0)
                (set! z 0)
                (set! s "")
                (loop))
              ((and (< i z)
                    (or (= k curs:key-right)
                        (= k 6)))
                (set! i (+ 1 i))
                (loop))
              ((and (positive? i)
                    (or (= k curs:key-left)
                        (= k 2)))
                (set! i (- i 1))
                (loop))
              (else
                (curs:beep)
                (loop)))))))
