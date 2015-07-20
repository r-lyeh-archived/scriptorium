;;; a repl
;;;
;;; (load "repl.scm") ((*repl* 'run))

(provide 'repl.scm)
(require libc.scm)

(unless (defined? '*repl*)
  (define *repl*                    ; environment that holds the REPL functions
    (let ((prompt #f)               ; function to get/set prompt
	  (keymap #f)               ; function to get/set keymap entries
	  (history #f)              ; function to get/set history buffer entries
	  (history-size #f)         ; function to get/set history buffer size
	  (save-history #f)         ; function to save the current history buffer entries in a file
	  (restore-history #f)      ; function to restore history buffer entries from a file
	  (helpers ())              ; list of functions displaying help strings 
	  (run #f)                  ; function that fires up a REPL
	  (top-level-let (sublet (rootlet) :** #f)) ; environment in which evaluation takes place
	  (repl-let                 ; environment for keymap functions to access all the REPL innards (cursor-position etc)
	   
      (with-let (sublet *libc*)

	;; -------- completion --------
	(define (symbol-completion text)
	  (let ((st (symbol-table))
		(text-len (length text))
		(match #f))
	    (call-with-exit
	     (lambda (return)
	       (for-each
		(lambda (symbol)
		  (let* ((sym (symbol->string symbol))
			 (sym-len (length sym)))
		    (when (and (>= sym-len text-len)
			       (string=? text (substring sym 0 text-len)))
		      (if match
			  (return text)
			  (set! match sym)))))
		st)
	       (or match text)))))
	
	(define (filename-completion text)
	  (let ((g (glob.make)))
	    (glob (string-append text "*")
		  (logior (if (and (defined? 'GLOB_TILDE)
				   (char=? (text 0) #\~))
			      GLOB_TILDE
			      0)
			  GLOB_MARK)
		  g)
	    (let ((files (map (lambda (f) ; get rid of emacs' *~ files
				(if (and (> (length f) 1)
					 (char=? #\~ (f (- (length f) 1))))
				    (values)
				    f))
			      (glob.gl_pathv g))))
	      (globfree g) 
	      (if (or (null? files)
		      (not (null? (cdr files))))
		  text
		  (car files)))))
	
	
	;; -------- history --------
	(let ((histbuf (make-vector 100 ""))
	      (histsize 100)
	      (histpos 0)
	      (m-p-pos 0)
	      (histtop ())
	      (histtail ()))

	  (define (push-line line)
	    (if (null? histtop)
		(begin
		  (set! histtop (list line))
		  (set! histtail histtop))
		(begin
		  (set-cdr! histtail (list line))
		  (set! histtail (cdr histtail)))))

	  (define (history-member line)
	    (do ((i 0 (+ i 1)))
		((or (= i histsize)
		     (string=? (vector-ref histbuf i) line))
		 (and (< i histsize) i))))
	  
	  (define history-size (dilambda
				(lambda ()
				  histsize)
				(lambda (new-size)
				  (unless (= new-size histsize)
				    (if (<= new-size 0)
					(error 'out-of-range "new history buffer size must be positive")
					(let ((new-hist (make-vector new-size ""))
					      (new-end (min (- new-size 1) histpos)))
					  (let loop ((oldpos histpos)
						     (newpos new-end))
					    (set! (new-hist newpos) (histbuf oldpos))
					    (if (zero? newpos)
						(set! newpos (- new-size 1))
						(set! newpos (- newpos 1)))
					    (unless (= newpos new-end)
					      (if (zero? oldpos)
						  (set! oldpos (- histsize 1))
						  (set! oldpos (- oldpos 1)))
					      (unless (= oldpos histpos)
						(loop oldpos newpos))))
					  (set! histsize new-size)
					  (set! histpos new-end)
					  (set! histbuf new-hist)
					  new-size))))))

	  (define history (dilambda 
			   (lambda (back)
			     (let ((i (+ histpos back)))
			       (if (< i 0)
				   (histbuf (+ histsize i))
				   (if (>= i histsize)
				       (histbuf (- i histsize))
				       (histbuf i)))))

			   (lambda (new-line)
			     (let ((pos (history-member new-line)))
			       (when (integer? pos)                   ; remove the earlier case, circularly compress the buffer
				 (when (>= pos histpos)
				   (do ((i pos (+ i 1)))
				       ((>= i (- histsize 1)))
				     (set! (histbuf i) (histbuf (+ i 1))))
				   (set! (histbuf (- histsize 1)) (histbuf 0))
				   (set! pos 0))

				 (do ((i pos (+ i 1)))
				     ((>= i (- histpos 1)))
				   (set! (histbuf i) (histbuf (+ i 1))))
				 (set! histpos (- histpos 1)))

			       (set! (histbuf histpos) new-line)
			       (set! histpos (+ histpos 1))
			       (if (= histpos histsize)
				   (set! histpos 0))))))
	  
	  (define (history-help)
	    (set! (*repl* 'helpers)
		  (list
		   (lambda (c) 
		     (format #f "size: ~A, pos: ~A" histsize histpos))
		   (lambda (c)
		     (format #f "buf: ~A ~A ~A" (history -1) (history -2) (history -3)))
		   (lambda (c)
		     (format #f "line: ~A" histtop)))))
	  
	  (define (pop-history)            ; throw away most recent addition
	    (set! histpos (- histpos 1))
	    (if (negative? histpos)
		(set! histpos (- histsize 1))))
	  
	  (define* (write-history port (spaces 0))
	    (format port "~NC(set! histpos ~D)~%" spaces #\space histpos)
	    (format port "~NC(set! histsize ~D)~%" spaces #\space histsize)
	    (let ((pl (*s7* 'print-length)))
	      (set! (*s7* 'print-length) (* 2 histsize))
	      (format port "~NC(set! histbuf ~A)" spaces #\space (object->string histbuf))
	      (set! (*s7* 'print-length) pl)))
	  
	  (define* (save-history (file "repl-history.scm"))
	    (call-with-output-file file
	      write-history))
	  
	  (define* (restore-history (file "repl-history.scm"))
	    (load file (curlet)))
	  
	  
	  (let ((prompt-string "<1> ")     ; this doesn't look very good, but I can't find anything better
		(prompt-length 4)          ;    perhaps the line number could go in the terminal's right margin?
		(cur-line "")              ; current expression (can contain newlines)
		(prev-line ())             ; for undo via C-_
		(selection #f)             ; for C-y
		(cursor-pos 0)             ; cursor-pos is the logical position (index into cur-line)
		(cur-row 0)                ; catch window scrolling
		(red-par-pos #f)           ; paren match position (index into cur-line)
		(prompt-row 0)             ; need to tie everything to prompt position (it can move in media res if window scrolls)
		(prompt-col 0)
		(last-row 0)               ; these hold the window bounds when we start
		(last-col 0)
		(input-fd (fileno stdin))  ; source of chars, either tty input or file input
		(terminal-fd (fileno stdin))
		(tab-as-space (make-string 6 #\space))
		(next-char #f)
		(chars 0)                  ; (sigh) a kludge to try to distinguish tab-as-space from tab-as-completion/indentation
		(unbound-case #f)
		(all-done #f))             ; if #t, repl returns to its caller, if any
	    
	    
	    ;; -------- evaluation ---------
	    (define (badexpr h)            ; *missing-close-paren-hook* function for Enter command
	      (set! (h 'result) 'string-read-error))
	    
	    (define (shell? h)             ; *unbound-variable-hook* function, also for Enter
	      ;; examine cur-line -- only call system if the unbound variable matches the first non-whitespace chars
	      ;;   of cur-line, and command -v name returns 0 (indicating the shell thinks it is an executable command)
	      (do ((i 0 (+ i 1)))
		  ((or (= i (length cur-line))
		       (not (char-whitespace? (cur-line i))))
		   (let ((var-name (symbol->string (h 'variable))))
		     (when (and (>= (- (length cur-line) i) (length var-name)) ; var-name might be unrelated to cur-line
				(string=? var-name (substring cur-line i (+ i (length var-name))))
				(zero? (system (string-append "command -v " var-name " >/dev/null"))))
		       (set! unbound-case #t)
		       (if (procedure? ((rootlet) 'system))
			   (begin
			     (set! ((*repl* 'top-level-let) '**) (((rootlet) 'system) cur-line #t))
			     (display ((*repl* 'top-level-let) '**) *stderr*))
			   (set! ((*repl* 'top-level-let) '**) (system cur-line)))
		       (set! (h 'result) (symbol " ")))))))
	    
	    (define (with-repl-let body)
	      ;; for multiline edits, we will use *missing-close-paren-hook* rather than try to parse the input ourselves.
	      (let ((old-badexpr-hook (hook-functions *missing-close-paren-hook*))
		    (old-unbound-var-hook (hook-functions *unbound-variable-hook*))
		    (old-eval #f)
		    (old-eval-string #f)
		    (old-load #f))
		
		;; if the repl's top-level-let is not rootlet, and we load some file into rootlet
		;;   that (for example) defines a function at its top level, that function's definition
		;;   env is rootlet.  If we then define something in the repl, it is in the
		;;   repl's top level.  If we now call the function asking is the new thing defined,
		;;   it looks in rootlet, not the repl top-level, and says no.
		;; so, locally redefine these three to use the repl top-level while we're in the repl.
		;; in addition, for each of these, we need to report missing close parens and so on
		
		(define (repl-hooks)
		  (set! (hook-functions *missing-close-paren-hook*) (cons badexpr old-badexpr-hook))
		  (set! (hook-functions *unbound-variable-hook*) (cons shell? old-unbound-var-hook)))
		
		(define (original-hooks)
		  (set! unbound-case #f)
		  (set! (hook-functions *missing-close-paren-hook*) old-badexpr-hook)
		  (set! (hook-functions *unbound-variable-hook*) old-unbound-var-hook))
		
		(define* (new-load file (e (*repl* 'top-level-let)))
		  (dynamic-wind original-hooks (lambda () (load file e)) repl-hooks))
		
		(define* (new-eval form (e (*repl* 'top-level-let))) 
		  (dynamic-wind original-hooks (lambda () (eval form e)) repl-hooks))
		
		(define* (new-eval-string str (e (*repl* 'top-level-let)))
		  (dynamic-wind original-hooks (lambda () (eval-string str e)) repl-hooks))
		
		(dynamic-wind
		    (lambda ()
		      (repl-hooks)
		      (set! eval new-eval)
		      (set! eval-string new-eval-string)
		      (set! load new-load))
		    body
		    (lambda ()
		      (set! eval old-eval)
		      (set! eval-string old-eval-string)
		      (set! load old-load)
		      (original-hooks)))))
	    
	    
	    ;; -------- match parens --------
	    (define (char-constant? pos)
	      (and (> pos 2)
		   (char=? (cur-line (- pos 1)) #\\)
		   (char=? (cur-line (- pos 2)) #\#)))
	    
	    (define (check-parens)
	      (let ((endpos (- cursor-pos 1)))
		(if (and (> cursor-pos 1)
			 (char=? (cur-line endpos) #\))              ; ")" on left of cursor
			 (not (char-constant? endpos)))                  ; it's not "#\)"
		    (let ((oparens ())
			  (new-red-pos #f))
		      (do ((i 0 (+ i 1)))
			  ((>= i endpos))
			(case (cur-line i)
			  ((#\()
			   (set! oparens (cons i oparens)))
			  
			  ((#\))
			   (if (pair? oparens)
			       (set! oparens (cdr oparens))))
			  
			  ((#\;)
			   (do ((k (+ i 1) (+ k 1)))
			       ((or (>= k endpos)
				    (char=? (cur-line k) #\newline))
				(set! i k)
				(if (>= i endpos)                   ; (f1 "(+ 1 3) should not show first paren as a match (similarly below)
				    (set! oparens ())))))
			  
			  ((#\")
			   (do ((k (+ i 1) (+ k 1)))
			       ((or (>= k endpos)
				    (and (char=? (cur-line k) #\")
					 (not (char=? (cur-line (- k 1)) #\\))))
				(set! i k)
				(if (>= i endpos)
				    (set! oparens ())))))
			  
			  ((#\#)
			   (if (char=? (cur-line (+ i 1)) #\|)
			       (do ((k (+ i 1) (+ k 1)))
				   ((or (>= k endpos)
					(and (char=? (cur-line k) #\|)
					     (char=? (cur-line (+ k 1)) #\#)))
				    (set! i (+ k 1))
				    (if (>= i endpos)
					(set! oparens ()))))))))
		      
		      (if (pair? oparens)
			  (set! new-red-pos (car oparens)))
		      (unless (equal? new-red-pos red-par-pos)
			(if (number? new-red-pos)
			    (set! red-par-pos new-red-pos)
			    (set! red-par-pos #f))))
		    (if (number? red-par-pos)
			(set! red-par-pos #f)))))
	    
	    
	    ;; -------- indentation --------
	    (define (indent pos)
	      (let ((old-red red-par-pos)
		    (old-line (copy cur-line))
		    (old-cursor cursor-pos))
		(set! cur-line (string-append (substring cur-line 0 cursor-pos) ")"))
		(set! cursor-pos (length cur-line))
		(check-parens)
		(set! cur-line old-line)
		(set! cursor-pos old-cursor)
		(let ((new-red red-par-pos))
		  (set! red-par-pos old-red)
		  (let ((col 0))
		    (do ((i 0 (+ i 1)))
			((= i new-red))
		      (if (char=? (cur-line i) #\newline)
			  (set! col 0)
			  (set! col (+ col 1))))
		    (let ((sym (do ((i (+ new-red 1) (+ i 1)))
				   ((not (char-alphabetic? (cur-line i)))
				    (substring cur-line (+ new-red 1) i)))))
		      
		      (let ((spaces (+ col (if (member sym '("or" "and" "cond" "if"))
					       (+ (length sym) 2)
					       2))))
			(if (= cursor-pos (length cur-line))
			    (begin
			      (set! cur-line (format #f "~A~NC" cur-line spaces #\space))
			      (set! cursor-pos (length cur-line)))
			    (begin
			      (set! cur-line (format #f "~A~NC~A" 
						     (substring cur-line 0 cursor-pos)
						     spaces #\space
						     (substring cur-line (+ cursor-pos 1))))
			      (set! cursor-pos (+ cursor-pos spaces))))))))))
	    
	    
	    ;; -------- prompt --------
	    (define (original-prompt num)
	      (set! prompt-string (format #f "<~D> " num))
	      (set! prompt-length (length prompt-string)))
	    
	    
	    ;; -------- vt100 --------
	    (define (bold text) (format #f "~C[1m~A~C[0m" #\escape text #\escape)) 
	    (define (red text) (format #f "~C[31m~A~C[0m" #\escape text #\escape))  ; black=30, green=32, yellow=33, blue=34
	    
	    (define* (rgb text (r 0) (g 0) (b 0) all-colors)
	      (if all-colors
		  (format #f "~C[38;5;~Dm~A~C[0m" #\escape (+ 16 (* 36 (round (* r 5))) (* 6 (round (* g 5))) (round (* b 5))) text #\escape)
		  (format #f "~C[~Dm~A~C[0m" #\escape (+ 30 (ash (round b) 2) (ash (round g) 1) (round r)) text #\escape)))
	    
	    (define (move-cursor y x)
	      (format *stderr* "~C[~D;~DH" #\escape y x))

	    (define (cursor-coords)
	      (let* ((c (string #\null #\null))
		     (cc (string->c-pointer c))
		     (terminal-fd (fileno stdin)))
		(format *stderr* "~C[6n" #\escape)
		(do ((b (read terminal-fd cc 1) (read terminal-fd cc 1)))
		    ((char=? (c 0) #\escape)))
		(read terminal-fd cc 1) 
		(and (char=? (c 0) #\[)
		     (let ((y 0)
			   (x 0))
		       (do ((b (read terminal-fd cc 1) (read terminal-fd cc 1)))
			   ((not (char-numeric? (c 0))))
			 (set! y (+ (* 10 y) (- (char->integer (c 0)) (char->integer #\0)))))
		       (and (char=? (c 0) #\;)
			    (do ((b (read terminal-fd cc 1) (read terminal-fd cc 1)))
				((not (char-numeric? (c 0)))
				 (and (char=? (c 0) #\R)
				      (cons x y)))
			      (set! x (+ (* 10 x) (- (char->integer (c 0)) (char->integer #\0))))))))))
	    
	    (define (cursor-bounds)
	      (let ((coords (cursor-coords)))
		(set! prompt-col (car coords))
		(set! prompt-row (cdr coords))
		(move-cursor 4000 4000)
		(let ((bounds (cursor-coords)))
		  (move-cursor (cdr coords) (car coords))
		  (set! last-col (car bounds))
		  (set! last-row (cdr bounds)))))
	    
	    ;; to enable mouse click coords (in xterm anyway), (format *stderr* "~C[?9h" #\escape)
	    ;;    disable: (format *stderr* "~C[?9l" #\escape)
	    ;; while enabled, mouse selection instead sends coords to repl (so it's annoying)
	    ;;    also it's sticky!  exit repl does not clear this flag so mouse is effectively dead
	    ;; upon click, we get ESC [ M bxy -- need to poll for this?
	    
	    
	    ;; -------- display --------
	    (define (display-prompt)
	      (format *stderr* "~A" prompt-string))
	    
	    (define (new-prompt)
	      (set! cur-line "")
	      (set! cursor-pos 0)
	      (set! cur-row 0)
	      (set! prev-line ())
	      ((*repl* 'prompt) (+ (length histtop) 1))
	      (display-prompt)
	      (cursor-bounds))
	    
	    (define (display-line start end)
	      
	      ;; if a line wraps, it will confuse the redisplay/cursor positioning code. so truncate the display
	      (let ((line-len (+ (- end start) 1 prompt-length)))
		(if (>= line-len last-col)
		    (set! end (- end (- line-len last-col)))))
	      
	      (if (and red-par-pos
		       (<= start red-par-pos)
		       (< red-par-pos end))
		  (string-append
		   (if (zero? start)
		       (format #f "~A" prompt-string)
		       (format #f "~NC" prompt-length #\space))
		   (if (= start red-par-pos)
		       (format #f "~A~A"
			       (bold (red "(")) 
			       (substring cur-line (+ start 1) end))
		       (format #f "~A~A~A"
			       (substring cur-line start red-par-pos) 
			       (bold (red "(")) 
			       (substring cur-line (+ red-par-pos 1) end))))
		  (if (zero? start)
		      (format #f "~A~A" prompt-string (substring cur-line 0 end))
		      (format #f "~NC~A" prompt-length #\space (substring cur-line start end)))))
	    
	    (define (display-cursor)
	      (let ((row 0)
		    (start 0)
		    (len (length cur-line)))
		(do ((i 0 (+ i 1)))
		    ((or (= i len)
			 (= i cursor-pos))
		     (move-cursor (+ prompt-row row) (+ prompt-col (- cursor-pos start))))
		  (when (char=? (cur-line i) #\newline)
		    (set! row (+ row 1))
		    (set! start (+ i 1))))))
	    
	    (define (display-lines)
	      (move-cursor prompt-row 0)
	      (format *stderr* "~C[J" #\escape)
	      (let ((len (length cur-line))
		    (new-line ""))
		(let ((line-end 0))
		  (do ((i 0 (+ line-end 2)))
		      ((> i len))
		    (set! line-end (end-of-line i))
		    (set! new-line (string-append new-line (display-line i (min (+ line-end 2) len))))))
		(format *stderr* "~A" new-line)
		(display-cursor)))
	    
	    
	    ;; -------- help/debugging --------
	    (define (one-line text)
	      (if (string? text)
		  (let ((ntext (copy text)))
		    (do ((i 0 (+ i 1)))
			((= i (length ntext))
			 ntext)
		      (if (char=? (ntext i) #\newline) (set! (ntext i) #\|))))
		  text))
	    
	    (define (help c)
	      (when (pair? (*repl* 'helpers))
		(let ((coords (cursor-coords))
		      (col (floor (/ last-col 2))))
		  (move-cursor 1 col)
		  (format *stderr* "+~NC" (- col 2) #\-)
		  
		  (do ((i 2 (+ i 1))                      ; put box in top right corner so we don't get trailing output as we scroll
		       (lst (*repl* 'helpers) (cdr lst)))
		      ((null? lst))
		    (let ((str ((car lst) c)))
		      (move-cursor i col)
		      (format *stderr* "~C[K| ~A"  #\escape (if (> (length str) col) (substring str 0 (- col 1)) str))))
		  
		  (move-cursor (+ 2 (length (*repl* 'helpers))) col)
		  (format *stderr* "+~NC" (- col 2) #\-)
		  (move-cursor (cdr coords) (car coords)))))
	    
	    (define (debug-help)
	      (set! (*repl* 'helpers)
		    (list
		     (lambda (c) 
		       (format #f "cursor: ~A, ~C, line: ~S"
			       cursor-pos
			       (if (> (length cur-line) 0)
				   (let ((c (cur-line (max 0 (min cursor-pos (- (length cur-line) 1))))))
				     (if (char=? c #\newline) #\| c))
				   #\space)
			       (one-line cur-line)))
		     (lambda (c)
		       (format #f "len: ~D, selection: ~S, previous: ~S" 
			       (length cur-line) 
			       (one-line selection) 
			       (if (pair? prev-line) (one-line (cdar prev-line)) ())))
		     (lambda (c)
		       (format #f "cur-row: ~A, prompt-row: ~A" 
			       cur-row 
			       prompt-row))
		     (lambda (c)
		       (format #f "c: ~S ~D, start: ~A, end: ~A" 
			       (object->string c #t) 
			       (char->integer c)	
			       (start-of-line cursor-pos)	
			       (end-of-line cursor-pos))))))
	    
	    
	    ;; -------- keymap(s) --------
	    (define meta-keymap-functions (make-vector 256))
	    (define keymap-functions (make-vector 256 #f))
	    
	    (define keymap (dilambda
			    (lambda (c)
			      (cond ((char? c) 
				     (keymap-functions (char->integer c)))
				    ((integer? c) 
				     (keymap-functions c))
				    ((string? c)
				     (if (= (length c) 1)
					 (keymap-functions (char->integer (c 0)))
					 (if (and (= (length c) 2)
						  (char=? (c 0) #\escape))
					     (meta-keymap-functions (char->integer (c 1)))
					     (lambda (c) #t))))
				    (else (error 'wrong-type-arg "keymap takes a character or string argument"))))
			    
			    (lambda (c f)
			      (cond ((char? c) 
				     (set! (keymap-functions (char->integer c)) f))
				    ((integer? c) 
				     (set! (keymap-functions c) f))
				    ((string? c)
				     (if (= (length c) 1)
					 (set! (keymap-functions (char->integer (c 0))) f)
					 (if (and (= (length c) 2)
						  (char=? (c 0) #\escape))
					     (set! (meta-keymap-functions (char->integer (c 1))) f))))
				    (else (error 'wrong-type-arg "set! keymap takes a character or string first argument"))))))
	    
	    (define C-a 1)     ; #\x01 etc
	    (define C-b 2)
	    (define C-d 4)
	    (define C-e 5)
	    (define C-f 6)
	    (define C-h 8)
	    (define C-k 11)
	    (define C-l 12)
					;(define C-m 13)    ; #\return -- Enter handles this case (crlf?)
	    (define C-n 14)
	    (define C-o 15)
	    (define C-p 16)
					;(define C-r 18)
	    (define C-t 20)
	    (define C-y 25)
	    (define C-_ 31)
	    (define Tab 9)
	    (define Enter 10)           ; #\linefeed
	    (define Backspace 127)
	    (define Escape 27)          ; #\escape
	    
	    (define (end-of-line pos)             ; prompt or #\newline mark the line boundary
	      (let ((len (length cur-line)))
		(do ((i (max 0 (min pos len)) (+ i 1)))
		    ((or (>= i len)
			 (char=? (cur-line i) #\newline))
		     (if (>= i len) len (max 0 (- i 1)))))))
	    
	    (define (start-of-line pos)
	      (if (<= pos 0)
		  0
		  (do ((i (min pos (- (length cur-line) 1)) (- i 1)))
		      ((or (zero? i)
			   (char=? (cur-line i) #\newline))
		       (if (zero? i) 0 (+ i 1))))))
	    
	    (define (count-newlines line)
	      (let ((len (length line))
		    (newlines 0))
		(do ((i 0 (+ i 1)))
		    ((= i len) newlines)
		  (if (char=? (cur-line i) #\newline)
		      (set! newlines (+ newlines 1))))))
	    
	    (define (append-newline)
	      (set! cur-line (string-append cur-line (string #\space #\newline)))
	      (set! cursor-pos (length cur-line))
	      (when (= last-row (+ prompt-row cur-row))
		(format *stderr* "~%")
		(set! prompt-row (- prompt-row 1)))
	      (set! cur-row (+ cur-row 1)))
	    
	    (define (word-break pos)                 ; assume we're at the start of a word
	      (let ((len (length cur-line)))
		(let loop ((i pos))
		  (if (or (>= i len)
			  (not (or (char-alphabetic? (cur-line i))
				   (char-numeric? (cur-line i)))))
		      i
		      (loop (+ i 1))))))
	    
	    (define (save-line)
	      (set! prev-line (cons (cons cursor-pos (copy cur-line)) prev-line)))
	    
	    (let ((main-keyfunc (lambda (c)
				  (if (<= chars 1) (save-line))
				  (if (= cursor-pos (length cur-line))
				      (set! cur-line (string-append cur-line (string c)))
				      (if (= cursor-pos 0)
					  (set! cur-line (string-append (string c) cur-line))
					  (set! cur-line (string-append 
							  (substring cur-line 0 cursor-pos) 
							  (string c) 
							  (substring cur-line cursor-pos)))))
				  (set! cursor-pos (+ cursor-pos 1))
				  (set! m-p-pos 0)))
		  (no-op-keyfunc (lambda (c) #t)))
	      
	      (do ((i 0 (+ i 1)))
		  ((= i 32))
		(set! (keymap-functions i) no-op-keyfunc))
	      
	      (do ((i 32 (+ i 1)))
		  ((= i 256))
		(set! (keymap-functions i) main-keyfunc))
	      
	      (do ((i 0 (+ i 1)))
		  ((= i 256))
		(set! (meta-keymap-functions i) no-op-keyfunc)))
	    
	    
	    ;; -------- cursor movement 
	    (set! (keymap-functions C-a) 
		  (lambda (c)
		    (set! cursor-pos (start-of-line cursor-pos))
		    'just-cursor))
	    
	    (set! (keymap-functions C-e) 
		  (lambda (c)
		    (set! cursor-pos (end-of-line cursor-pos))
		    'just-cursor))
	    
	    (set! (keymap-functions C-b) 
		  (lambda (c)
		    (when (> cursor-pos 0)
		      (set! cursor-pos (- cursor-pos 1))
		      (if (char=? (cur-line cursor-pos) #\newline)
			  (set! cursor-pos (- cursor-pos 1))))
		    'just-cursor))
	    
	    (set! (keymap-functions C-f) 
		  (lambda (c)
		    (let ((len (length cur-line)))
		      (when (< cursor-pos len)
			(set! cursor-pos (+ cursor-pos 1))
			(if (and (< cursor-pos len)
				 (char=? (cur-line cursor-pos) #\newline))
			    (set! cursor-pos (+ cursor-pos 1)))))
		    'just-cursor))
	    
	    (set! (keymap-functions C-p) 
		  (lambda (c)
		    ;; try to find corresponding column in previous line
		    (let ((start (start-of-line cursor-pos)))
		      (when (positive? start)
			(let ((upstart (start-of-line (- start 2)))
			      (upend (end-of-line (- start 2)))
			      (line-pos (- cursor-pos start)))
			  (set! cursor-pos (min (+ upstart line-pos) upend)))))
		    'just-cursor))
	    
	    (set! (keymap-functions C-n) 
		  (lambda (c)
		    ;; try to find corresponding column in next line
		    (let ((start (start-of-line cursor-pos))
			  (len (length cur-line))
			  (next-start (+ (end-of-line cursor-pos) 1)))      ; should be at #\newline (if any)
		      (if (> len next-start)                                ; not already at last line
			  (let ((next-end (end-of-line (+ next-start 1)))
				(line-pos (- cursor-pos start)))
			    (set! cursor-pos (min (+ next-start 1 line-pos) next-end)))))
		    'just-cursor))
	    
	    (set! (keymap-functions C-l) 
		  (lambda (c)
		    (format *stderr* "~C[H~C[J" #\escape #\escape)
		    (new-prompt)))
	    
	    ;; -------- deletion
	    (set! (keymap-functions C-d) 
		  (lambda (c)
		    (let ((len (length cur-line)))
		      (when (< cursor-pos len)
			(save-line)
			(do ((i cursor-pos (+ i 1)))
			    ((>= i (- len 1)))
			  (set! (cur-line i) (cur-line (+ i 1))))
			(set! cur-line (substring cur-line 0 (- len 1)))))))
	    
	    (set! (keymap-functions C-h) 
		  (lambda (c)
		    (when (positive? cursor-pos)
		      (save-line)
		      (let ((len (length cur-line)))
			(set! cursor-pos (- cursor-pos 1))
			(do ((i cursor-pos (+ i 1)))
			    ((>= i (- len 1)))
			  (set! (cur-line i) (cur-line (+ i 1))))
			(set! cur-line (substring cur-line 0 (- len 1)))))))
	    
	    (set! (keymap-functions Backspace) 
		  (keymap-functions C-h))
	    
	    (set! (keymap-functions C-k) 
		  (lambda (c)
		    (let ((len (length cur-line)))
		      (when (< cursor-pos len)
			(save-line)
			(let ((end (end-of-line cursor-pos)))
			  (if (= end len)
			      (begin
				(set! selection (substring cur-line cursor-pos))
				(set! cur-line (substring cur-line 0 cursor-pos)))
			      (if (or (= cursor-pos end)                            ; delete following newline
				      (char=? (cur-line cursor-pos) #\newline))
				  (set! cur-line (string-append (substring cur-line 0 cursor-pos)
								(substring cur-line (+ cursor-pos 1))))
				  (begin
				    (set! selection (substring cur-line cursor-pos (+ end 1)))
				    (set! cur-line (string-append (substring cur-line 0 cursor-pos)
								  (substring cur-line (+ end 1))))
				    ))))))))
	    
	    ;; -------- undo/selection
	    (set! (keymap-functions C-y) 
		  (lambda (c)
		    (when selection
		      (save-line)
		      (if (zero? cursor-pos)
			  (set! cur-line (string-append selection cur-line))
			  (if (>= cursor-pos (length cur-line))
			      (set! cur-line (string-append cur-line selection))
			      (set! cur-line (string-append (substring cur-line 0 cursor-pos)
							    selection
							    (substring cur-line cursor-pos)))))
		      (set! cursor-pos (+ cursor-pos (length selection))))))
	    
	    (set! (keymap-functions C-_) 
		  (lambda (c)
		    (when (pair? prev-line)
		      (set! cur-line (cdar prev-line))
		      (set! cursor-pos (caar prev-line))
		      (set! prev-line (cdr prev-line)))))
	    
	    ;; -------- add newline
	    (set! (keymap-functions C-o)
		  (lambda (c)
		    (if (= cursor-pos 0)
			(set! cur-line (string-append (string #\space #\newline) cur-line))
			(if (>= cursor-pos (length cur-line))
			    (set! cur-line (string-append cur-line (string #\space #\newline)))
			    (if (char=? (cur-line (- cursor-pos 1)) #\newline)
				(set! cur-line (string-append (substring cur-line 0 cursor-pos)
							      (string #\space #\newline)
							      (substring cur-line cursor-pos)))
				(set! cur-line (string-append (substring cur-line 0 cursor-pos)
							      (if (char=? (cur-line (+ cursor-pos 1)) #\newline)
								  (string #\space #\newline #\space)
								  (string #\space #\newline))
							      (substring cur-line (+ cursor-pos 1)))))))
		    (when (= last-row (+ prompt-row cur-row))
		      (set! prompt-row (- prompt-row 1))
		      (display-lines))))
	    
	    ;; -------- transpose
	    (set! (keymap-functions C-t) 
		  (lambda (c)
		    (let ((end (end-of-line cursor-pos)))
		      (save-line)
		      (let ((cur (if (>= cursor-pos end)
				     (- cursor-pos 1)
				     cursor-pos)))
			(if (positive? cur)
			    (let ((tmp-c (cur-line (- cur 1))))
			      (set! (cur-line (- cur 1)) (cur-line cur))
			      (set! (cur-line cur) tmp-c)
			      (set! cursor-pos (+ cur 1))))))))
	    
	    ;; -------- indentation/completion
	    (set! (keymap-functions Tab) 
		  (lambda (c)
		    ;; if user pastes in a selection, it may have embedded tabs which are just spacing, 
		    ;;   not requests for filename completion!  We'll try to catch this case by assuming
		    ;;   that a selection will not be the single character #\tab
		    (if (> chars 1)
			(begin
			  (set! cur-line (string-append cur-line "    "))
			  (set! cursor-pos (+ cursor-pos 4)))
			(let ((start (start-of-line cursor-pos))
			      (end (end-of-line cursor-pos))
			      (completion #f))
			  (if (and (positive? start)
				   (= end start))
			      (indent start)
			      (if (= cursor-pos end)
				  (let ((loc (do ((i (- end 1) (- i 1)))
						 ((or (< i 0)
						      (char-whitespace? (cur-line i))
						      (member (cur-line i) '(#\( #\' #\" #\)) eqv?))
						  i))))
				    (if (< loc 0)
					(set! completion (symbol-completion cur-line))
					(if (char=? (cur-line loc) #\")
					    (set! completion (filename-completion (substring cur-line (+ loc 1))))
					    (set! completion (symbol-completion (substring cur-line (+ loc 1))))))
				    (when (and completion
					       (> (length completion) (- end loc 1)))
				      (save-line)
				      (if (= end (length cur-line))
					  (set! cur-line (string-append (substring cur-line 0 (+ loc 1)) completion))
					  (set! cur-line (string-append (substring cur-line 0 (+ loc 1)) 
									completion
									(substring cur-line (+ end 1)))))
				      (set! cursor-pos (end-of-line cursor-pos))))))))))
	    
	    ;; -------- evaluation/multiline
	    (set! (keymap-functions Enter) 
		  (lambda (c)
		    
		    (call-with-exit
		     (lambda (return)
		       (let ((len (length cur-line)))
			 
			 (do ((i 0 (+ i 1)))               ; check for just whitespace
			     ((or (= i len)
				  (not (char-whitespace? (cur-line i))))
			      (when (= i len)
				(append-newline)
				(return))))
			 
			 (set! red-par-pos #f)
			 (if (or (= chars 1) 
				 (not (= input-fd terminal-fd)))
			     (display-lines))
			 
			 (catch #t
			   (lambda ()
			     
			     ;; we want to add cur-line (copied) to the history buffer
			     ;;   unless it is an on-going edit (missing close paren)
			     (catch 'string-read-error
			       
			       (lambda ()
				 (set! cursor-pos len)
				 (if (or (= chars 1)
					 (not (= input-fd terminal-fd)))
				     (display-lines))
				 (set! (history) (copy cur-line))
				 (set! m-p-pos 0)
				 
				 (with-repl-let
				  (lambda ()
				    ;; get the newline out if the expression does not involve a read error
				    (let ((form (with-input-from-string cur-line #_read)))    ; not libc's read
				      (newline *stderr*)
				      (let ((val (eval form (*repl* 'top-level-let))))
					(if unbound-case
					    (set! unbound-case #f)
					    (begin
					      (format *stderr* "~S~%" val)
					      (set! ((*repl* 'top-level-let) '**) val))))))))
			       
			       (lambda (type info)
				 (pop-history)               ; remove last history entry
				 (append-newline)
				 (return))))
			   
			   (lambda (type info)
			     (format *stderr* "~A: " (red "error"))
			     (apply format *stderr* info)
			     (newline *stderr*)))
			 
			 (push-line (copy cur-line))
			 (new-prompt))))))
	    
	    ;; -------- escaped (Meta/Alt/arrow) keys
	    (set! (keymap-functions Escape) 
		  (lambda (esc)
		    (let ((chr (next-char)))
		      ((meta-keymap-functions (char->integer chr)) chr))))
	    
	    (set! (meta-keymap-functions (char->integer #\[))
		  (lambda (c)
		    (let ((chr (next-char)))
		      (case chr
			((#\A) ((keymap-functions C-p) C-p))  ; arrow keys
			((#\B) ((keymap-functions C-n) C-n))
			((#\C) ((keymap-functions C-f) C-f))
			((#\D) ((keymap-functions C-b) C-b))))))
	    
	    (let ()
	      ;; Meta key is a problem on the Mac, so I'll package these for easier disposal

	      (define (fixup-new-line)
		(set! cursor-pos (length cur-line))
		(let ((newlines (count-newlines cur-line)))
		  (when (< last-row (+ prompt-row newlines))
		    (format *stderr* "~NC" (- (+ prompt-row newlines) last-row) #\newline)
		    (set! prompt-row (- prompt-row newlines)))
		  (set! cur-row newlines)))

		(define (get-previous-line c)         ; get earlier line indexed by numeric arg
		  (let ((len (length histtop)))
		    (let ((pos (or (string->number cur-line) len)))
		      (set! pos (min len (max pos 1)))
		      (set! cur-line (histtop (- pos 1)))
		      (fixup-new-line))))

		(define (move-forward-in-history c)
		  (set! m-p-pos (min 0 (+ m-p-pos 1)))
		  (when (positive? (length (history m-p-pos)))
		    (set! cur-line (history m-p-pos))
		    (fixup-new-line)))

		(define (move-backward-in-history c)
		  (let ((old-index m-p-pos))
		    (when (and (zero? m-p-pos) 
			       (> (length cur-line) 0))
		      (set! (history) cur-line)
		      (set! m-p-pos -1))
		    (set! m-p-pos (max (- m-p-pos 1) (- histsize)))
		    (if (positive? (length (history m-p-pos)))
			(begin
			  (set! cur-line (history m-p-pos))
			  (fixup-new-line))
			(set! m-p-pos old-index))))
	    
		(define (go-to-start c)
		  (set! cursor-pos 0)
		  'just-cursor)

		(define (go-to-end c)
		  (set! cursor-pos (length cur-line))
		  'just-cursor)

		(define (capitalize c)
		  (let ((len (length cur-line)))
		    (let loop ((i cursor-pos))
		      (if (< i len)
			  (if (char-alphabetic? (cur-line i))
			      (begin
				(save-line)
				(set! (cur-line i) (char-upcase (cur-line i)))
				(set! cursor-pos (word-break i)))
			      (loop (+ i 1)))))))
		
		(define (upper-case c)
		  (let ((len (length cur-line)))
		    (do ((i cursor-pos (+ i 1)))
			((or (= i len)
			     (char-alphabetic? (cur-line i)))
			 (when (< i len)
			   (save-line)
			   (do ((k i (+ k 1)))
			       ((or (= k len)
				    (not (char-alphabetic? (cur-line k))))
				(set! cursor-pos k))
			     (set! (cur-line k) (char-upcase (cur-line k)))))))))
	    
		(define (lower-case c)
		  (let ((len (length cur-line)))
		    (do ((i cursor-pos (+ i 1)))
			((or (= i len)
			     (char-alphabetic? (cur-line i)))
			 (when (< i len)
			   (save-line)
			   (do ((k i (+ k 1)))
			       ((or (= k len)
				    (not (char-alphabetic? (cur-line k))))
				(set! cursor-pos k))
			     (set! (cur-line k) (char-downcase (cur-line k)))))))))
		
		(set! (meta-keymap-functions (char->integer #\p)) move-backward-in-history)
		(set! (meta-keymap-functions (char->integer #\n)) move-forward-in-history)
		(set! (meta-keymap-functions (char->integer #\.)) get-previous-line)
		(set! (meta-keymap-functions (char->integer #\<)) go-to-start)
		(set! (meta-keymap-functions (char->integer #\>)) go-to-end)
		(set! (meta-keymap-functions (char->integer #\c)) capitalize)
		(set! (meta-keymap-functions (char->integer #\u)) upper-case)
		(set! (meta-keymap-functions (char->integer #\l)) lower-case)
		)
	    
	    ;; -------- terminal setup --------
	    (define* (run file)
	      (let ((saved #f)
		    (tty #t))
		
		;; we're in libc here, so exit is libc's exit!
		(define (tty-reset no)
		  (if tty (tcsetattr terminal-fd TCSAFLUSH saved))
		  (if (not (equal? input-fd terminal-fd)) (close input-fd))
		  (#_exit))
		
		(varlet (*repl* 'top-level-let) 
		  :exit (let ((documentation "(exit) resets the repl tty and exits the repl"))
			  (lambda ()
			    (newline *stderr*)
			    (tty-reset 0))))
		
		;; check for dumb terminal
		(if (or (zero? (isatty terminal-fd))        ; not a terminal -- input from pipe probably
			(string=? (getenv "TERM") "dumb"))  ; no vt100 codes -- emacs shell for example
		    (let ((buf (c-pointer->string (calloc 512 1) 512)))
		      (set! tty #f)
		      (format *stderr* "> ")
		      (do ((b (fgets buf 512 stdin) (fgets buf 512 stdin)))
			  ((zero? (length b))
			   (#_exit))
			(let ((len (strlen buf)))
			  (when (positive? len)
			    (do ((i 0 (+ i 1)))
				((or (not (char-whitespace? (buf i)))
				     (= i len))
				 (when (< i len)
				   (with-repl-let
				    (lambda ()
				      (catch #t
					(lambda ()
					  (format *stderr* "~S~%" (eval-string (substring buf 0 (- (strlen buf) 1)) (*repl* 'top-level-let))))
					(lambda (type info)
					  (format *stderr* "error: ")
					  (apply format *stderr* info)
					  (newline *stderr*)))))
				   (format *stderr* "> ")))))))))
		
		;; not a pipe or a dumb terminal -- hopefully all others accept vt100 codes
		(let ((buf (termios.make))
		      (read-size 128))
		  
		  (set! next-char                                     ; this indirection is needed if user pastes the selection into the repl
			(let* ((c (make-string read-size #\null)) 
			       (cc (string->c-pointer c))
			       (ctr 0))
			  (lambda ()
			    (call-with-exit
			     (lambda (return)
			       (when (>= ctr chars)
				 (set! ctr 0)
				 (set! chars (read input-fd cc read-size))
				 (if (= chars 0)
				     (tty-reset terminal-fd))
				 
				 (when (= chars read-size)
				   ;; concatenate buffers until we get the entire selection
				   (let ((str (substring c 0 read-size)))
				     (let reading ((num (read input-fd cc read-size)))
				       (set! str (string-append str (substring c 0 num)))
				       (set! chars (+ chars num))
				       (if (= num read-size)
					   (reading (read input-fd cc read-size))))
				     
				     ;; look for simple cut/paste -- no control chars etc
				     (when (= input-fd terminal-fd)
				       (let ((bcksp (integer->char 127))
					     (ok-chars (list #\newline #\linefeed #\return #\tab)))
					 (do ((i 0 (+ i 1)))
					     ((or (= i chars)
						  (char>=? (str i) bcksp)
						  (and (char<? (str i) #\space)
						       (not (member (str i) ok-chars eq?))))
					      
					      (when (= i chars)
						(let ((search-chars (string #\tab #\return #\newline))
						      (old-pos 0)
						      (start 0)
						      (max-cols 0))
						  (do ((pos (char-position search-chars str 0) (char-position search-chars str (+ pos 1))))
						      ((not pos))
						    (set! cur-line (string-append cur-line 
										  (substring str old-pos pos)
										  (if (char=? (str pos) #\tab) 
										      tab-as-space 
										      (string #\space #\newline))))
						    (set! old-pos (+ pos 1))
						    (unless (char=? (str pos) #\tab)
						      (set! max-cols (max max-cols (- pos start)))
						      (set! start pos)))
						  (if (< (+ old-pos 1) (length str))
						      (set! cur-line (string-append cur-line (substring str (+ old-pos 1)))))
						  
						  ;; if the line is too long, the cursor gets confused, so try to reformat over-long strings
						  ;; this still messes up sometimes

						  (when (> max-cols (- last-col prompt-length))
						    (let ((old-len ((funclet pretty-print) '*pretty-print-length*)))
						      (set! ((funclet pretty-print) '*pretty-print-length*) (- last-col prompt-length 2))
						      (set! cur-line (with-output-to-string
								       (lambda ()
									 (pretty-print (with-input-from-string cur-line #_read)))))
						      (set! ((funclet pretty-print) '*pretty-print-length*) old-len)))
						  
						  (set! cursor-pos (length cur-line))
						  (set! chars 0)
						  (set! ctr 1)
						  (display-lines)
						  (return #\newline)))))))
				     
				     (set! c str)
				     (set! cc (string->c-pointer c))
				     ;; avoid time-consuming redisplays.  We need to use a recursive call on next-char here
				     ;;   since we might have multi-char commands (embedded #\escape -> meta, etc)
				     ;; actually, the time is not the repl's fault -- xterm seems to be waiting
				     ;;   for the window manager or someone to poke it -- if I move the mouse,
				     ;;   I get immediate output.  I also get immediate output in any case in OSX.
				     ;;   valgrind and ps say we're not computing, we're just sitting there.
				     (catch #t
				       (lambda ()
					 (do ((ch (next-char) (next-char)))
					     ((= ctr (- chars 1)) 
					      (set! chars 0)
					      (display-lines)
					      (return ch))
					   ((keymap-functions (char->integer ch)) ch)))
				       
				       (lambda (type info)
					 (set! chars 0)
					 (move-cursor prompt-row prompt-col)
					 (format *stderr* "internal error: ")
					 (apply format *stderr* info)
					 (newline *stderr*)
					 (format *stderr* "line ~A: ~A~%" ((owlet) 'error-line) ((owlet) 'error-code))
					 (set! chars 0)
					 (set! ctr 0)
					 (new-prompt)
					 (return #\null))))))
			       
			       (let ((result (c ctr)))
				 (set! ctr (+ ctr 1))
				 result))))))
		  
		  (set! input-fd (if (not file) 
				     terminal-fd
				     (open file O_RDONLY 0)))
		  
		  (set! saved (termios.make))
		  (tcgetattr terminal-fd saved) 
		  
		  (tcgetattr terminal-fd buf)
		  (termios.set_c_lflag buf (logand (termios.c_lflag buf) (lognot (logior ECHO ICANON))))
		  (termios.set_c_cc buf VMIN 1)
		  (termios.set_c_cc buf VTIME 0)
		  (when (negative? (tcsetattr terminal-fd TCSAFLUSH buf))
		    (tty-reset terminal-fd))
		  
		  
		  ;; -------- the repl --------
		  (display-prompt)
		  (cursor-bounds)
		  ;; (debug-help)
		  ;; (history-help)
		  
		  (do () 
		      (all-done
		       (set! all-done #f))       ; clear for next call
		    (catch #t
		      (lambda ()
			(let ((chr (next-char)))
			  (let ((res ((keymap-functions (char->integer chr)) chr))
				(last-pos red-par-pos))
			    (check-parens)
			    (if (or last-pos red-par-pos 
				    (not (eq? res 'just-cursor)))
				(display-lines)
				(display-cursor)))
			  (help chr)))
		      
		      (lambda (type info)
			(move-cursor prompt-row prompt-col)
			(format *stderr* "internal error: ")
			(apply format *stderr* info)
			(format *stderr* "~%line ~A: ~A~%" ((owlet) 'error-line) ((owlet) 'error-code))
			(set! chars 0)
			(new-prompt)))))))
	    
	    (curlet))))))
      
      (define (save-repl) 
	(call-with-output-file "save.repl" 
	  (lambda (p) 
	    (format p "(for-each~%~NC~
                         (lambda (f)~%~NC~
                           (if (not (provided? f))~%~NC~
                               (let ((autofile (*autoload* f)))~%~NC~
                                 (if (and autofile (file-exists? autofile))~%~NC(load autofile)))))~%~NC~W)~%~%" 
		    2 #\space 4 #\space 8 #\space 10 #\space 14 #\space 2 #\space
		    *features*)
	    (format p "(with-let (*repl* 'repl-let)~%")
	    (((*repl* 'repl-let) 'write-history) p 2)
	    (format p ")~%~%")
	    (format p "~W" (*repl* 'top-level-let)))))
      
      (define (restore-repl) 
	(set! (*repl* 'top-level-let) (load "save.repl")))  
      ;; I think this could be a merge rather than a reset by using (with-let top-level-let (load ...))
      
      
      (set! keymap (repl-let 'keymap))
      (set! history (repl-let 'history))
      (set! history-size (repl-let 'history-size))
      (set! save-history (repl-let 'save-history))
      (set! restore-history (repl-let 'restore-history))
      (set! prompt (repl-let 'original-prompt))
      (set! run (repl-let 'run))
      
      (curlet))))

;; ((*repl* 'run))


;;; --------------------------------------------------------------------------------

(autoload 'lint "lint.scm")
(autoload 'pretty-print "write.scm")

#|
(define pwd
  (let ((pd (lambda args
	      ((*libc* 'getcwd) 
	       (make-string 256 #\null) 256))))
    (openlet (inlet 'object->string pd          ; pwd (repl calls object->string)
		    'let-ref-fallback pd))))    ; (pwd) (repl calls let-ref-fallback method)
;; > pwd
;; /home/bil/cl

(define date
  (let ((pd (lambda args
	      (with-let (sublet *libc*)
		(let ((timestr (make-string 128))) 
		  (let ((len (strftime timestr 128 "%a %d-%b-%Y %H:%M:%S %Z"
				       (localtime 
					(time.make (time (c-pointer 0)))))))
		    (substring timestr 0 len)))))))
    (openlet (inlet 'object->string pd 'let-ref-fallback pd))))
|#

;; cd needs to be implemented
(define cd
  (openlet (inlet 'object->string (lambda args
				    (let ((line ((*repl* 'repl-let) 'cur-line)))
				      ((*libc* 'chdir)
				       (do ((i 3 (+ i 1)))
					   ((or (not (char-whitespace? (line i)))
						(= i (length line)))
					    (substring ((*repl* 'repl-let) 'cur-line) i))))
				      ((*libc* 'getcwd) (make-string 256 #\null) 256)))
		  'let-ref-fallback (lambda (obj str)  ; let-ref-fallback's first arg will be cd: (cd "..")
				      ((*libc* 'chdir) str)
				      ((*libc* 'getcwd) (make-string 256 #\null) 256)))))
;; > cd ..
;; /home/bil
;; > cd cl
;; /home/bil/cl

#|
(define-macro (make-command name)
  `(define ,name
     (openlet 
      (inlet 'object->string (lambda args
			       (system ((*repl* 'repl-let) 'cur-line) #t))))))
;; (make-command ls)
|#


(define-macro (time expr)
  (let ((start (gensym)))
    `(let ((,start ((*libc* 'gettimeofday))))
       ,expr
       (let ((end ((*libc* 'gettimeofday))))
	 (+ (- (car end) (car ,start)) ; seconds
	    (* 0.000001 (- (cadr end) (cadr ,start))))))))


(define* (apropos name (e (*repl* 'top-level-let)))
  
  (define (levenshtein s1 s2)
    (let ((l1 (length s1))
	  (l2 (length s2)))
      (cond ((zero? l1) l2)
	    ((zero? l2) l1)
	    (else (let ((distance (make-vector (list (+ l2 1) (+ l1 1)) 0)))
		    (do ((i 0 (+ i 1)))
			((> i l1))
		      (set! (distance 0 i) i))
		    (do ((i 0 (+ i 1)))
			((> i l2))
		      (set! (distance i 0) i))
		    (do ((i 1 (+ i 1)))
			((> i l2))
		      (do ((j 1 (+ j 1)))
			  ((> j l1))
			(let ((c1 (+ (distance i (- j 1)) 1))
			      (c2 (+ (distance (- i 1) j) 1))
			      (c3 (+ (distance (- i 1) (- j 1)) 
				     (if (char=? (s2 (- i 1)) (s1 (- j 1))) 0 1))))
			  (set! (distance i j) (min c1 c2 c3)))))
		    (distance l2 l1))))))

  (define* (make-full-let-iterator lt (stop (rootlet))) ; walk the entire let chain
    (if (eq? stop lt)
	(make-iterator lt)
	(letrec ((iterloop 
		  (let ((iter (make-iterator lt))
			(iterator? #t))
		    (lambda ()
		      (let ((result (iter)))
			(if (and (eof-object? result)
				 (iterator-at-end? iter))
			    (if (eq? stop (iterator-sequence iter))
				result
				(begin 
				  (set! iter (make-iterator (outlet (iterator-sequence iter))))
				  (iterloop)))
			    result))))))
	    (make-iterator iterloop))))
  
  (let ((ap-name (if (string? name) name 
		     (if (symbol? name) (symbol->string name)
			 (error 'wrong-type-arg "apropos argument 1 should be a string or a symbol"))))
	(ap-env (if (let? e) e 
		    (error 'wrong-type-arg "apropos argument 2 should be an environment"))))
    (let ((strs ())
	  (min2 (floor (log (length ap-name) 2)))
	  (have-orange (string=? ((*libc* 'getenv) "TERM") "xterm-256color")))
      (for-each
       (lambda (binding)
	 (if (pair? binding)
	     (let ((symbol-name (symbol->string (car binding))))
	       (if (string-position ap-name symbol-name)
		   (set! strs (cons (cons binding 0) strs))
		   (let ((distance (levenshtein ap-name symbol-name)))
		     (if (< distance min2)
			 (set! strs (cons (cons binding distance) strs))))))))
       (make-full-let-iterator ap-env))

      (if (pair? strs)
	  (begin
	    (for-each (lambda (b)
			(format *stderr*
				(if (zero? (cdr b)) "~C[1m~A~C[0m: ~S~%"                           ; black if exact match somewhere
				    (if (or (< (cdr b) 2) (not have-orange)) "~C[31m~A~C[0m: ~S~%" ; red for near miss
					"~C[38;5;208m~A~C[0m: ~S~%"))                              ; orange for less likely choices
				#\escape (caar b) #\escape
				(if (procedure? (cdar b))
				    (let ((doc (procedure-documentation (cdar b))))
				      (if (and (string? doc)
					       (positive? (length doc)))
					  doc
					  'procedure))
				    (cdar b))))
		      (sort! strs (lambda (a b)
				    (string<? (symbol->string (caar a)) (symbol->string (caar b))))))
	    '----)
	  'no-match))))


;;; --------------------------------------------------------------------------------
#|
to work in a particular environment:
    (set! (*repl* 'top-level-let) (sublet (rootlet) :** #f)) ; or any other like *libc*
now (define g 43) puts g in the new top-level-let, so ((rootlet) 'g) -> #<undefined>, and ((*repl* 'top-level-let) 'g) -> 43 (= g in repl of course)
to start with a fresh top-level, just set top-level-let to (sublet (rootlet)) again.

to add/change keymap entry (using sublet here to protect against inadvertent changes to the repl).
    (set! ((*repl* 'keymap) (integer->char 17)) ; C-q to quit and return to caller
          (lambda (c)
	    (set! ((*repl* 'repl-let) 'all-done) #t)))

(set! ((*repl* 'keymap) (integer->char 12)) ; C-l will expand to "(lambda " at the cursor
      (lambda (c)
	(with-let (sublet (*repl* 'repl-let))
	  (if (zero? cursor-pos)
	      (set! cur-line (string-append "(lambda " cur-line))
	      (if (>= cursor-pos (length cur-line))
		  (set! cur-line (string-append cur-line "(lambda "))
		  (set! cur-line (string-append (substring cur-line 0 cursor-pos)
						"(lambda "
						(substring cur-line cursor-pos)))))
	  (set! cursor-pos (+ cursor-pos (length "(lambda "))))))

change the prompt:
(set! (*repl* 'prompt) (lambda (num) 
			 (with-let (*repl* 'repl-let)
			   (set! prompt-string "scheme> ") 
			   (set! prompt-length (length prompt-string)))))

red lambda prompt: 
(set! (*repl* 'prompt)
      (lambda (num)
	(with-let (*repl* 'repl-let)
	  (set! prompt-string (bold (red (string #\xce #\xbb #\> #\space))))
	  (set! prompt-length 3)))) ; until we get unicode length calc


to post a help string (kinda tedious, but the helper list is aimed more at posting variable values):
(set! (*repl* 'helpers)
      (list (lambda (c)
	      (let ((sym (with-let (sublet (*repl* 'repl-let))
			   (let ((len (length cur-line)))
			     (let loop ((i (min (- len 1) cursor-pos)))
			       (and (not (negative? i))
				    (if (char=? (cur-line i) #\()
					(let loop1 ((k (+ i 1)))
					  (and (< k len)
					       (if (not (char-alphabetic? (cur-line k)))
						   (and (> k (+ i 1))
							(string->symbol (substring cur-line (+ i 1) k)))
						   (loop1 (+ k 1)))))
					(loop (- i 1)))))))))
		(if sym
		    (let ((str (help sym)))
		      (if str
			  (substring (help sym) 0 (min (length str) 40))
			  ""))
		    "")))))

;; function keys:
(set! ((*repl* 'keymap) (string #\escape #\[))
      (lambda (c)
	(with-let (sublet (*repl* 'repl-let))
	  (let ((next (next-char)))
	    (case next
	      ((#\A) ((keymap-functions C-p) C-p))
	      ((#\B) ((keymap-functions C-n) C-n))
	      ((#\C) ((keymap-functions C-f) C-f))
	      ((#\D) ((keymap-functions C-b) C-b))
	      ((#\1) ; on my system F1 is esc [ 1 1 ~, F2 esc [ 1 2 ~, etc (but they skip F6?)
	       (let ((n (- (char->integer (next-char)) (char->integer #\0))))
		 (next-char) ; throw away the tilde
		 (set! cur-line (string-append cur-line (format #f "--F~D!--" n))))))))))

;; this actually works but doesn't echo the filename correctly and the cursor is off during the filename processing
(set! ((*repl* 'keymap) (integer->char 24)) ; C-x
      (lambda (c)
	(with-let (sublet (*repl* 'repl-let))
	  (let ((next (next-char)))
	    (when (char=? next (integer->char 6)) ; C-f
	      (format *stderr* "~%load: ")
	      ;; now recursive call: prompt="load: "
	      (let ((old-prompt (*repl* 'prompt))
		    (old-cur-line cur-line)
		    (old-cursor-pos cursor-pos)
		    (old-enter-func (keymap-functions 10))
		    (filename #f))
		(set! (*repl 'prompt) (lambda (num) 
					(with-let (*repl* 'repl-let)
					  (set! prompt-string "load: ")
					  (set! prompt-length 6))))
		(set! cur-line "")
		(set! cursor-pos 0)
		(set! (keymap-functions 10) (lambda (c)
					      (set! filename cur-line)
					      (set! (*repl* 'prompt) old-prompt)
					      (set! cur-line old-cur-line)
					      (set! cursor-pos old-cursor-pos)
					      (set! (keymap-functions 10) old-enter-func)))
		(do ((c (next-char) (next-char)))
		    ((string? filename))
		  ((keymap-functions (char->integer c)) c)
		  (display-lines))
		(load filename)))))))

|#

;; unicode someday: I think all we need is unicode_string_length and index into unicode string (set/ref)
;; scroll past top line?

*repl*
