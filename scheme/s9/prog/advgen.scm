#! /usr/local/bin/s9 -f

; advgen -- generate HTML text adventures
; by Nils M Holm, 2010,2012
; Placed in the Public Domain
;
; Usage: advgen [too many options] [file]
;
; Sample adventure:
;        
;        (room
;         ("This is a sample room.")
;         (((not light))
;          ("It's pitch-dark!")
;          "Turn on light"
;          (add light))
;         ((light)
;          ("It's too bright!")
;          "Turn off light"
;          (rem light)))

(load-from-library "string-split.scm")
(load-from-library "string-translate.scm")
(load-from-library "for-all.scm")
(load-from-library "remove.scm")
(load-from-library "basename.scm")
(load-from-library "displaystar.scm")
(load-from-library "append-to-output-file.scm")
(load-from-library "string-digest.scm")
(load-from-library "write-to-string.scm")
(load-from-library "hash-table.scm")
(load-from-library "parse-optionsb.scm")

(define bare-html      (option #\b #f))
(define debug-info     (option #\d #f))
(define dry-run        (option #\n #f))
(define dumpdir        (option #\o 'string))
(define hash-length    (option #\H 'integer))
(define intro-text     (option #\i 'string))
(define path-length    (option #\P 'string))
(define page-epilog    (option #\e 'string))
(define remove-old     (option #\r #f))
(define page-prolog    (option #\p 'string))
(define show-help      (option #\h #f))
(define squeeze-nl     (option #\s #f))
(define state-comments (option #\c #f))
(define style-sheet    (option #\y 'string))
(define title-text     (option #\t 'string))
(define verbose        (option #\v 'counter))
(define options        `(,show-help
                         ,bare-html
                         ,debug-info
                         ,dry-run
                         ,dumpdir
                         ,hash-length
                         ,intro-text
                         ,page-epilog
                         ,path-length
                         ,page-prolog
                         ,remove-old
                         ,squeeze-nl
                         ,state-comments
                         ,style-sheet
                         ,title-text
                         ,verbose))

(define *dumpdir*     "advdump")
(define *dest-node*   #f)
(define *min-depth*   #f)
(define *hash-length* 8)

(define *visited* (make-hash-table))
(define *base*    #t)

(define vertex-id    car)
(define vertex-desc  cadr)
(define vertex-cond* cddr)

(define mod-cond   car)
(define mod-desc   cadr)
(define mod-link   caddr)
(define mod-action cadddr)

(define (valid-action? a)
  (and (list? a)
       (not (null? a))
       (or (and (memq (car a) '(go add rem))
                (<= 2 (length a) 3)
                (symbol? (cadr a))
                (or (null? (cddr a))
                    (string? (caddr a))))
           (and (memq (car a) '(nop))
                (<= 1 (length a)))
           (and (memq (car a) '(add/go rem/go))
                (<= 3 (length a) 4)
                (symbol? (cadr a))
                (symbol? (caddr a))
                (or (null? (cdddr a))
                    (string? (cadddr a))))
           (and (memq (car a) '(go/cut go/sel))
                (>= (length a) 2)
                (for-all symbol? (cdr a))))))

(define (valid-clause? x)
  (or (symbol? x)
      (and (list? x)
           (= 2 (length x))
           (eq? 'not (car x))
           (symbol? (cadr x)))))


(define (valid-description? x)
  (and (list? x)
       (or (for-all string? x)
           (and (= 2 (length x))
                (eq? 'copy-from (car x))
                (symbol? (cadr x))))))

(define (list-of-string? x)
  (and (list? x)
       (for-all string? x)))

(define (valid-link-text? x)
  (or (string? x)
      (and (list-of-string? x)
           (= 3 (length x)))))

(define (adv-error v-id msg . obj)
  (display "advgen: ")
  (if v-id
      (display* "vertex " v-id ": "))
  (display* "error: " msg)
  (if (not (null? obj))
      (display* ": " (car obj)))
  (newline)
  (sys:exit 1))

; An adventure description is a sequence of vertexes
; of the following form:
;
; vertex := (<identifier>
;            ("description"
;             ...)
;            ((<identifier>|(not <identifier>) ...)
;             ("description" ...)
;             "action description"
;             <action>
;            ...)
;
; <action> := (go     <identifier>                 ["message"])
;           | (add    <identifier>                 ["message"])
;           | (add/go <identifier>   <identifier>  ["message"])
;           | (rem    <identifier>                 ["message"])
;           | (rem/go <identifier>   <identifier>  ["message"])
;           | (go/cut <identifier>   ...                      )
;           | (go/sel <identifier>   ...                      )
;           | (nop   [<identifier>] [<identifier>] ["message"])
;
; <identifier> := [a-z0-9:\-]+

(define (read-description)
  (let ((vertexes (make-hash-table))
        (base    #f))
    (let loop ((obj (read)))
      (cond ((eof-object? obj)
              (list base vertexes))
            ((not (list? obj))
              (adv-error #f "expected a vertex, got" obj))
            ((< (length obj) 2)
              (adv-error #f "missing members in vertex" obj))
            ((not (symbol? (vertex-id obj)))
              (adv-error #f "malformed vertex ID" (vertex-id obj)))
            ((not (valid-description? (vertex-desc obj)))
              (adv-error (vertex-id obj)
                         "malformed description in vertex"
                         (vertex-desc obj)))
            ((let cloop ((c* (vertex-cond* obj)))
               (and (not (null? c*))
                    (let ((c (car c*)))
                      (if (not (= 4 (length c)))
                          (adv-error (vertex-id obj)
                                     "missing members in modifier"
                                     c))
                      (if (or (not (list? (mod-cond c)))
                              (not (for-all valid-clause? (mod-cond c))))
                          (adv-error (vertex-id obj)
                                     "malformed condition in modifier"
                                     (mod-cond c)))
                      (if (not (valid-description? (mod-desc c)))
                          (adv-error (vertex-id obj)
                                     "malformed description in modifier"
                                     (mod-desc c)))
                      (if (not (valid-link-text? (mod-link c)))
                          (adv-error (vertex-id obj)
                                     "malformed link text in modifier"
                                     (mod-link c)))
                      (if (not (valid-action? (mod-action c)))
                          (adv-error (vertex-id obj)
                                     "malformed action in modifier"
                                     (mod-action c)))
                      (cloop (cdr c*))))))
            (else
              (if (hash-table-ref vertexes (vertex-id obj))
                  (adv-error #f "duplicate vertex ID" (vertex-id obj)))
              (hash-table-set! vertexes (vertex-id obj) obj)
              (if (not base)
                  (set! base (vertex-id obj)))
              (loop (read)))))))

(define (append-map p . x)
  (apply append (apply map p x)))

(define (make-path id)
  (string-append *dumpdir* "/" id ".html"))

(define *vertex-counter* 0)

(define unique
  (let ((h (make-hash-table)))
    (lambda (seed len)
      (let ((v (string-digest (write-to-string seed) (* 4 len))))
        (set! *vertex-counter* (+ 1 *vertex-counter*))
        (let loop ((v v))
          (if (hash-table-ref h v)
              (loop (+ 1 v))
              (let ((s (number->string v 16)))
                (hash-table-set! h v #t)
                (if (>= (string-length s) len)
                    (substring s 0 len)
                    (string-append (make-string (- len (string-length s))
                                                #\0)
                                   s)))))))))

(define (emit* . args)
  (if (not (opt-val dry-run))
      (if (opt-val squeeze-nl)
          (apply display* (remv #\newline args))
          (apply display* args))))

(define (call-with-dummy-file file thunk)
  (thunk #f))

(define (copy-to out)
  (if (not (opt-val dry-run))
      (let loop ((c (read-char)))
        (if (not (eof-object? c))
            (begin (write-char c out)
                   (loop (read-char)))))))

(define (html-prelude out title)
  (if (not (opt-val bare-html))
      (emit* out
             "<HTML>"   #\newline
             "<HEAD>"   #\newline
             "<TITLE>"
             title
             "</TITLE>" #\newline
             (cond ((opt-val style-sheet)
                     => (lambda (name)
                          (string-append "<LINK rel=stylesheet title=\""
                                         (basename name ".css")
                                         "\" href=\""
                                         name
                                         "\">"
                                         (string #\newline))))
                   (else
                     ""))
             "</HEAD>"  #\newline
             "<BODY>"  #\newline))
  (cond ((opt-val page-prolog)
          => (lambda (path)
               (with-input-from-file
                 path
                 (lambda ()
                   (copy-to out))))))
  (cond ((opt-val title-text)
          => (lambda (text)
               (emit* out
                      "<H1><A href=\"index.html\">"
                      text
                      "</A></H1>"
                      #\newline
                      (if (opt-val page-prolog) "" "<HR>")
                      #\newline
                      )))))

(define (html-postlude out)
  (cond ((opt-val page-epilog)
          => (lambda (path)
               (with-input-from-file
                 path
                 (lambda ()
                   (copy-to out))))))
  (if (not (opt-val bare-html))
      (emit* out
             "</BODY>" #\newline
             "</HTML>" #\newline)))

(define (make-interim-page msg dest)
  (let ((i-vertex (unique msg *hash-length*)))
    (if (and (opt-val verbose) (> (opt-val verbose) 1))
        (display* "state: interim" #\newline))
    (if (not (opt-val dry-run))
        (with-output-to-file
          (make-path i-vertex)
          (lambda ()
            (html-prelude (current-output-port) "Info")
            (emit* "<P>"
                   msg
                   "</P>"
                   #\newline
                   "<P>"
                   "<A href=\""
                   dest
                   ".html\">Continue</A></P>"
                   #\newline)
            (html-postlude (current-output-port)))))
    i-vertex))

(define (make-heading sym)
  (let* ((s (symbol->string sym))
         (s (car (string-split #\: s)))
         (l (string->list s))
         (l (cons (char-upcase (car l)) (cdr l))))
    (string-translate (list->string l) "-" " ")))

(define (last x)
  (car (reverse x)))

(define (all-but-last x)
  (reverse! (cdr (reverse x))))

(define (emit-desc out desc)
  (for-each (lambda (x)
              (emit* out x #\newline))
            desc))

(define (emit-mod-desc* out c*)
  (cond ((not (null? c*))
           (for-each (lambda (x)
                       (if (not (null? (mod-desc x)))
                           (emit-desc out (mod-desc x))))
                     c*))))

(define (cycle prop v-id)
  (adv-error v-id "cycling through property" prop))

(define (cut-props props state)
  (if (null? props)
      state
      (cut-props (cdr props)
                 (remq (car props) state))))

(define (retain-props props state)
  (let ((cut (cut-props props state)))
    (cut-props cut state)))

(define (make-nav-grid out dir*)
  (letrec
    ((find-dir
       (lambda (d d*)
         (cond ((null? d*)
                 #f)
               ((string=? d (mod-link (cadar d*)))
                 (caar d*))
               (else
                 (find-dir d (cdr d*)))))))
    (emit* out "<DIV>&nbsp;</DIV>" #\newline "<TABLE>" #\newline)
    (let y-loop ((grid '(("NW" "N" "NE")
                         ("W"  ""  "E")
                         ("SW" "S" "SE"))))
      (if (not (null? grid))
          (begin (emit* out "<TR>" #\newline)
                 (let x-loop ((row (car grid)))
                   (if (null? row)
                       (begin (emit* out "</TR>" #\newline)
                              (y-loop (cdr grid)))
                       (begin (emit* out "<TD>&nbsp;")
                              (let ((dest (find-dir (car row) dir*)))
                                (if dest
                                    (emit* out
                                           "<A href=\""
                                           (string-append dest ".html")
                                           "\">"
                                           (car row)
                                           "</A>")
                                    (emit* out (car row))))
                              (emit* out "&nbsp;</TD>" #\newline)
                              (x-loop (cdr row))))))))
    (emit* out "</TABLE>" #\newline)))

(define (split-desc s)
  (if (string? s)
      (list "" s "")
      s))

(define (emit-action* out page-id c* vertexes v-id state visited depth)
  (if (not (null? c*))
      (emit* out "<DL>" #\newline))
  (let loop ((c*   c*)
             (dir* '()))
    (if (null? c*)
        (if (not (null? dir*))
            (make-nav-grid out dir*))
        (let ((x (car c*)))
          (let ((a         (mod-action x))
                (new-v-id  v-id)
                (new-state state))
            (case (car a)
              ((go)     (set! new-v-id (cadr a)))
              ((add)    (if (memq (cadr a) state) (cycle (cadr a) v-id))
                        (set! new-state (cons (cadr a) state)))
              ((add/go) (if (memq (cadr a) state) (cycle (cadr a) v-id))
                        (set! new-state (cons (cadr a) state))
                        (set! new-v-id (caddr a)))
              ((rem)    (set! new-state (remq (cadr a) state)))
              ((rem/go) (set! new-state (remq (cadr a) state))
                        (set! new-v-id (caddr a)))
              ((go/cut) (set! new-state (cut-props (cddr a) state))
                        (set! new-v-id (cadr a)))
              ((go/sel) (set! new-state (retain-props (cddr a) state))
                        (set! new-v-id (cadr a)))
              ((nop)    'nop)
              ((nop)    'nop)
              (else     (adv-error v-id "bad action" (car a))))
            (let* ((new-tag       (cons new-v-id new-state))
                   (linked-vertex (hash-table-ref visited new-tag)))
              (if out (close-output-port out))
              (let ((v (if linked-vertex
                           (car linked-vertex)
                           (render-vertex vertexes
                                          new-v-id
                                          new-state
                                          (+ 1 depth)))))
                (if out
                    (set! out (append-to-output-file
                                (make-path page-id))))
                (cond ((and (string? (mod-link x))
                            (string=? "" (mod-link x)))
                        (loop (cdr c*) dir*))
                      ((member (mod-link x)
                               '("N" "E" "S" "W" "NE" "SE" "SW" "NW"))
                        (loop (cdr c*) (cons (list v x)
                                             dir*)))
                      (else
                        (let ((s* (split-desc (mod-link x))))
                          (emit* out
                                 "<DD>"
                                 (car s*)
                                 "<A href=\""
                                 (if (string? (last a))
                                     (make-interim-page (last a) v)
                                     v)
                                 ".html\">"
                                 (cadr s*)
                                 "</A>"
                                 (caddr s*)
                                 "</DD>"
                                 #\newline))
                          (loop (cdr c*) dir*)))))))))
  (if (not (null? c*))
      (emit* out "</DL>" #\newline))
  out)

(define (true-conditions cond* state)
  (append-map (lambda (this-cond)
                (let loop ((c* (mod-cond this-cond)))
                  (cond ((null? c*)
                          (list this-cond))
                        ((or (and (symbol? (car c*))
                                  (not (memq (car c*) state)))
                             (and (pair? (car c*))
                                  (memq (cadar c*) state)))
                          '())
                        (else
                          (loop (cdr c*))))))
              cond*))

(define (find-vertex src vertexes v-id)
  (let ((v (hash-table-ref vertexes v-id)))
    (if v
        (car v)
        (adv-error src "no such vertex" v-id))))

(define (render-vertex vertexes v-id state depth)
  (if (eq? v-id *dest-node*)
      (if (or (not *min-depth*)
              (< depth *min-depth*))
          (set! *min-depth* depth)))
  (let* ((vertex (find-vertex #f vertexes v-id))
         (tag    (cons (vertex-id vertex) state))
         (c*     (true-conditions (vertex-cond* vertex) state)))
    (let ((page-id (unique tag *hash-length*)))
      (if (and (opt-val verbose) (> (opt-val verbose) 1))
          (display* "state: " tag #\newline))
      (if (and (not (opt-val dry-run))
               (not (file-exists? *dumpdir*)))
          (sys:mkdir *dumpdir*))
      ((if (opt-val dry-run)
           call-with-dummy-file
           call-with-output-file)
        (make-path page-id)
        (lambda (out)
          (html-prelude out (make-heading v-id))
          (if (opt-val state-comments)
              (emit* out "<!-- state: " tag " -->" #\newline))
          (if (opt-val debug-info)
              (emit* out "<P>State: " tag "</P>" #\newline))
          (hash-table-set! *visited* tag page-id)
          (emit* out
                 (if (opt-val title-text) "<H2>" "<H1>")
                 (make-heading (vertex-id vertex))
                 (if (opt-val title-text) "</H2>" "</H1>")
                 #\newline)
          (emit* out "<P>" #\newline)
          (let* ((desc (vertex-desc vertex))
                 (desc (if (and (pair? desc)
                                (eq? 'copy-from (car desc)))
                           (vertex-desc (find-vertex v-id
                                                     vertexes
                                                     (cadr desc)))
                           desc)))
            (emit-desc out desc))
          (emit-mod-desc* out c*)
          (emit* out "</P>" #\newline)
          (let ((out (emit-action* out
                                   page-id
                                   c*
                                   vertexes
                                   v-id
                                   state
                                   *visited*
                                   depth)))
            (html-postlude out))))
      page-id)))

(define (dump-state v-id vertexes)
  (render-vertex vertexes v-id '() 0))

(define (make-entry-page base)
  (if (not (opt-val dry-run))
      (with-output-to-file
        (make-path "index")
        (lambda ()
          (html-prelude (current-output-port)
                        (if (opt-val title-text)
                            (opt-val title-text)
                            "The Beginning"))
          (cond ((opt-val intro-text)
                  => (lambda (file)
                       (with-input-from-file
                         file
                         (lambda ()
                           (copy-to (current-output-port)))))))
          (emit* "<H2><A href=\""
                 base
                 ".html\">Let the journey begin!</A></H2>"
                 #\newline)
          (html-postlude (current-output-port))))))

(define (advgen)
  (let* ((base+vertexes (read-description))
         (base          (car  base+vertexes))
         (vertexes      (cadr base+vertexes))
         (n-vertexes    (length (hash-table->alist vertexes))))
    (if base
        (let ((entry-page (dump-state base vertexes)))
          (make-entry-page entry-page)
          (if (opt-val verbose)
              (display* n-vertexes
                        " vertexes read, "
                        *vertex-counter*
                        " states generated."
                        #\newline))
          (if (opt-val path-length)
              (if (not *min-depth*)
                  (display* "there is no path to a vertex named \""
                            *dest-node*
                            "\""
                            #\newline)
                  (display* "shortest path to \""
                            *dest-node*
                            "\": "
                            *min-depth*
                            " nodes"
                            #\newline)))))))

(define (usage)
  (display* "Usage: advgen [-bcdnrsv] [-e file] [-H length] [-i file]"
            "[-o path]"
            #\newline
            "              [-p file] [-P node] [-t text] [-y url] [file]")
  (newline))

(let ((file (parse-options! (sys:command-line) options usage)))
  (if (opt-val show-help)
      (begin (display-usage
               `(""
                 ,usage
                 ""
                 "Generate HTML text adventures"
                 ""
                 "-b       emit bare HTML without headers or BODY tags"
                 "-c       include state comments in source files"
                 "-d       write visible state information to each page"
                 "-e file  copy page epilogue from file"
                 "-H len   hash length (file name length)"
                 "-i file  copy introduction text from file"
                 "-n       dry-run, do not emit any data"
                 "-o path  output path"
                 "-p file  copy page prologue from file"
                 "-P node  print the length of the shortest path to node"
                 "-r       remove left-over files before processing"
                 "         (will remove *all* files from destination!)"
                 "-s       squeeze out newlines to save space"
                 "-t text  title to be displayed on each page"
                 "-y url   link style sheet (not with -b)"
                 "-v       verbose operation"
                 ""))
             (sys:exit)))
  (cond ((opt-val dumpdir)
          => (lambda (path)
               (set! *dumpdir* path))))
  (cond ((opt-val path-length)
          => (lambda (path)
               (set! *dest-node* (string->symbol path)))))
  (cond ((opt-val hash-length)
          => (lambda (len)
               (set! *hash-length* len))))
  (if (and (opt-val remove-old)
           (file-exists? *dumpdir*))
      (for-each (lambda (name)
                  (sys:unlink (string-append *dumpdir* "/" name)))
                (sys:readdir *dumpdir*)))
  (cond ((> (length file) 1)
          (usage)
          (sys:exit))
        ((null? file)
          (advgen))
        (else
          (with-input-from-file
            (car file)
            advgen))))
