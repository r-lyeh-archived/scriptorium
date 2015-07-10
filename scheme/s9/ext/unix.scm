; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009-2012
; Placed in the Public Domain
;
; An interface to some Unix system services.
;
; (sys:change-mode integer string)  ==>  integer
; (sys:chmod string1 string2|integer)  ==>  unspecific
; (sys:chown string1 string2|integer1 string3|integer2)  ==>  unspecific
; (sys:creat string)  ==>  integer
; (sys:exit)  ==>  undefined
; (sys:group-name string) ==> string
; (sys:group-gid string) ==> integer
; (sys:kill integer)  ==>  unspecific
; (sys:lstat-name string)  ==>  string
; (sys:lstat-size string)  ==>  integer
; (sys:lstat-uid string)  ==>  integer
; (sys:lstat-gid string)  ==>  integer
; (sys:lstat-mode string)  ==>  integer
; (sys:lstat-ctime string)  ==>  integer
; (sys:lstat-atime string)  ==>  integer
; (sys:lstat-mtime string)  ==>  integer
; (sys:lstat-dev string)  ==>  integer
; (sys:lstat-ino string)  ==>  integer
; (sys:lstat-nlink string)  ==>  integer
; (sys:mkdir string)  ==>  unspecific
; (sys:notify integer)  ==>  unspecific
; (sys:notify integer1 integer2)  ==>  unspecific
; (sys:stat-name string)  ==>  string
; (sys:stat-size string)  ==>  integer
; (sys:stat-uid string)  ==>  integer
; (sys:stat-gid string)  ==>  integer
; (sys:stat-mode string)  ==>  integer
; (sys:stat-ctime string)  ==>  integer
; (sys:stat-atime string)  ==>  integer
; (sys:stat-mtime string)  ==>  integer
; (sys:stat-dev string)  ==>  integer
; (sys:stat-ino string)  ==>  integer
; (sys:stat-nlink string)  ==>  integer
; (sys:time)  ==>  integer
; (sys:user-name string)  ==>  string
; (sys:user-uid string)  ==>  integer
; (sys:user-gid string)  ==>  integer
; (sys:user-gecos string)  ==>  string
; (sys:user-home string)  ==>  string
; (sys:user-shell string)  ==>  string
;
; (Example):   (sys:getcwd) ==> "/u/home/nmh"

(require-extension sys-unix)

(load-from-library "bitops.scm")
(load-from-library "for-all.scm")
(load-from-library "string-split.scm")
(load-from-library "string-unsplit.scm")

(define sys:access-f-ok (sys:get-magic-value "F_OK"))
(define sys:access-x-ok (sys:get-magic-value "X_OK"))
(define sys:access-w-ok (sys:get-magic-value "W_OK"))
(define sys:access-r-ok (sys:get-magic-value "R_OK"))

(define sys:read-only  (sys:get-magic-value "O_RDONLY"))
(define sys:write-only (sys:get-magic-value "O_WRONLY"))
(define sys:read+write (sys:get-magic-value "O_RDWR"))

(define old:creat sys:creat)

(define sys:creat
  (let ((old:creat old:creat))
    (lambda (file . mode)
      (let ((mode (if (null? mode)
                      #o644
                      (car mode))))
        (old:creat file mode)))))

(define sys:seek-set (sys:get-magic-value "SEEK_SET"))
(define sys:seek-cur (sys:get-magic-value "SEEK_CUR"))
(define sys:seek-end (sys:get-magic-value "SEEK_END"))

(define sys:sighup  (sys:get-magic-value "SIGHUP"))
(define sys:sigint  (sys:get-magic-value "SIGINT"))
(define sys:sigquit (sys:get-magic-value "SIGQUIT"))
(define sys:sigill  (sys:get-magic-value "SIGILL"))
(define sys:sigtrap (sys:get-magic-value "SIGTRAP"))
(define sys:sigabrt (sys:get-magic-value "SIGABRT"))
(define sys:sigemt  (sys:get-magic-value "SIGEMT"))
(define sys:sigfpe  (sys:get-magic-value "SIGFPE"))
(define sys:sigkill (sys:get-magic-value "SIGKILL"))
(define sys:sigbus  (sys:get-magic-value "SIGBUS"))
(define sys:sigsegv (sys:get-magic-value "SIGSEGV"))
(define sys:sigsys  (sys:get-magic-value "SIGSYS"))
(define sys:sigpipe (sys:get-magic-value "SIGPIPE"))
(define sys:sigalrm (sys:get-magic-value "SIGALRM"))
(define sys:sigterm (sys:get-magic-value "SIGTERM"))

(define old:kill sys:kill)

(define sys:kill
  (let ((old:kill old:kill))
    (lambda (pid . signal)
      (let ((signal (if (null? signal)
                        sys:sigterm
                        (car signal))))
        (old:kill pid signal)))))

(define sys:notify sys:kill)

(define old:mkdir sys:mkdir)

(define sys:mkdir
  (let ((old:mkdir old:mkdir))
    (lambda (path . mode)
      (let ((mode (if (null? mode)
                      #o755
                      (car mode))))
        (old:mkdir path mode)))))

(define (stat-accessor stat tag)
  (lambda (file)
    (let ((s (stat file)))
      (if s
          (cdr (assq tag s))
          (error "sys:stat-accessor: cannot stat" file)))))

(define sys:stat-name  (stat-accessor sys:stat 'name))
(define sys:stat-size  (stat-accessor sys:stat 'size))
(define sys:stat-uid   (stat-accessor sys:stat 'uid))
(define sys:stat-gid   (stat-accessor sys:stat 'gid))
(define sys:stat-mode  (stat-accessor sys:stat 'mode))
(define sys:stat-ctime (stat-accessor sys:stat 'ctime))
(define sys:stat-atime (stat-accessor sys:stat 'atime))
(define sys:stat-mtime (stat-accessor sys:stat 'mtime))
(define sys:stat-dev   (stat-accessor sys:stat 'dev))
(define sys:stat-ino   (stat-accessor sys:stat 'ino))
(define sys:stat-nlink (stat-accessor sys:stat 'nlink))

(define sys:lstat-name  (stat-accessor sys:lstat 'name))
(define sys:lstat-size  (stat-accessor sys:lstat 'size))
(define sys:lstat-uid   (stat-accessor sys:lstat 'uid))
(define sys:lstat-gid   (stat-accessor sys:lstat 'gid))
(define sys:lstat-mode  (stat-accessor sys:lstat 'mode))
(define sys:lstat-ctime (stat-accessor sys:lstat 'ctime))
(define sys:lstat-atime (stat-accessor sys:lstat 'atime))
(define sys:lstat-mtime (stat-accessor sys:lstat 'mtime))
(define sys:lstat-dev   (stat-accessor sys:lstat 'dev))
(define sys:lstat-ino   (stat-accessor sys:lstat 'ino))
(define sys:lstat-nlink (stat-accessor sys:lstat 'nlink))

(define sys:s-isuid (sys:get-magic-value "S_ISUID"))
(define sys:s-isgid (sys:get-magic-value "S_ISGID"))
(define sys:s-isvtx (sys:get-magic-value "S_ISVTX"))
(define sys:s-irwxu (sys:get-magic-value "S_IRWXU"))
(define sys:s-irusr (sys:get-magic-value "S_IRUSR"))
(define sys:s-iwusr (sys:get-magic-value "S_IWUSR"))
(define sys:s-ixusr (sys:get-magic-value "S_IXUSR"))
(define sys:s-irwxg (sys:get-magic-value "S_IRWXG"))
(define sys:s-irgrp (sys:get-magic-value "S_IRGRP"))
(define sys:s-iwgrp (sys:get-magic-value "S_IWGRP"))
(define sys:s-ixgrp (sys:get-magic-value "S_IXGRP"))
(define sys:s-irwxo (sys:get-magic-value "S_IRWXO"))
(define sys:s-iroth (sys:get-magic-value "S_IROTH"))
(define sys:s-iwoth (sys:get-magic-value "S_IWOTH"))
(define sys:s-ixoth (sys:get-magic-value "S_IXOTH"))

; Modifier may be
; - an integer representing a decimal mode, e.g. 511
; - a string representing an octal mode, e.g. "0755"
; - a string of the form "[ugoa][+-=][rwxst]", e.g. "a+rwx"
; - a sequence of the above, e.g. "a=rwx,go=rx,+t"

(define (sys:change-mode mode modifier)
  (letrec
    ((suid sys:s-isuid)
     (sgid sys:s-isgid)
     (svtx sys:s-isvtx)
     (rwxu sys:s-irwxu)
     (rwxg sys:s-irwxg)
     (rwxo sys:s-irwxo)
     (str->mode
       (lambda (s)
         (letrec
           ((op #f)
            (make-mode
              (lambda (u g o m)
                (if (char=? #\= op)
                    (let* ((new (if u
                                    (bit+
                                      (bit* rwxu (bitsl m 6))
                                      (bit* m suid)
                                      (bit* mode #o77770077))
                                    mode))
                           (new (if g
                                    (bit+
                                      (bit* rwxg (bitsl m 3))
                                      (if (zero? (bit* m suid))
                                          0
                                          sgid)
                                      (bit* new #o77770707))
                                    new))
                           (new (if o
                                    (bit+
                                      (bit* rwxo m)
                                      (bit* new #o77770770))
                                    new))
                           (new (bit+ new (bit* m svtx))))
                      new)
                    ((if (char=? #\+ op)
                         bit+
                         bit*c)
                     mode
                     (bit+ (if u (bit+
                                   (bit* rwxu (bitsl m 6))
                                   (bit* m suid))
                                 0)
                           (if g (bit+
                                   (bit* rwxg (bitsl m 3))
                                   (if (zero? (bit* m suid))
                                       0
                                       sgid))
                                 0)
                           (if o (bit* rwxo m) 0)
                           (bit* m svtx)))))))
           (let loop ((l (string->list s))
                      (u #f)
                      (g #f)
                      (o #f)
                      (m 0)
                      (b #f))
             (cond
               ((null? l)
                 (if op (make-mode u g o m) #f))
               ((char=? #\, (car l))
                 (let ((new (if op (make-mode u g o m) #f)))
                   (if new
                       (begin (set! mode new)
                              (loop (cdr l) #f #f #f 0 #f))
                       #f)))
               (else
                 (case (car l)
                   ((#\u) (if b #f (loop (cdr l) #t g  o  m #f)))
                   ((#\g) (if b #f (loop (cdr l) u  #t o  m #f)))
                   ((#\o) (if b #f (loop (cdr l) u  g  #t m #f)))
                   ((#\a) (if b #f (loop (cdr l) #t #t #t m #f)))
                   ((#\r) (if b (loop (cdr l) u g o (bit+ m 4) #t) #f))
                   ((#\w) (if b (loop (cdr l) u g o (bit+ m 2) #t) #f))
                   ((#\x) (if b (loop (cdr l) u g o (bit+ m 1) #t) #f))
                   ((#\s) (if b (loop (cdr l) u g o (bit+ m suid) #t) #f))
                   ((#\t) (if b (loop (cdr l) u g o (bit+ m svtx) #t) #f))
                   ((#\+ #\- #\=)
                          (if b #f (begin (set! op (car l))
                                          (loop (cdr l) u g o m #t))))
                   (else  #f)))))))))
    (cond ((integer? modifier)
            modifier)
          ((string? modifier)
            (if (and (> (string-length modifier) 0)
                     (char<=? #\0 (string-ref modifier 0) #\9))
                (string->number modifier 8)
                (let ((m (str->mode modifier)))
                  (if m m (error "sys:change-mode: bad modifier" modifier)))))
          (else
            (error "change-mode: expected string or integer, got"
                   modifier)))))

(define (do-chmod who chmod-proc stat-proc file modifier)
  (let ((old-mode (stat-proc file)))
    (if (not old-mode)
        (error (string-append who ": no such file") file)
        (let ((new-mode (sys:change-mode old-mode modifier)))
          (chmod-proc file new-mode)))))

(define old:chmod sys:chmod)

(define sys:chmod
  (let ((do-chmod  do-chmod)
        (old:chmod old:chmod))
    (lambda (file modifier)
      (do-chmod "sys:chmod" old:chmod sys:stat-mode file modifier))))

(define old:exit sys:exit)

(define sys:exit
  (let ((old:exit old:exit))
    (lambda code
      (cond ((null? code)
              (old:exit 0))
            ((and (null? (cdr code))
                  (integer? (car code))
                  (<= 0 (car code) 127))
              (old:exit (car code)))
            (else
              (error "sys:exit: bad argument(s)" code))))))

(define (sys:time)
  (car (sys:gettimeofday)))

(define (user-accessor who tag)
  (lambda (x)
    (let ((u (if (string? x)
                 (sys:getpwnam x)
                 (sys:getpwuid x))))
      (if u
          (cdr (assq tag u))
          (error (string-append who ": no such user name or UID: " x))))))

(define sys:user-name  (user-accessor "sys:user-name" 'name))
(define sys:user-uid   (user-accessor "sys:user-uid" 'uid))
(define sys:user-gid   (user-accessor "sys:user-gid" 'gid))
(define sys:user-gecos (user-accessor "sys:user-gecos" 'gecos))
(define sys:user-home  (user-accessor "sys:user-home" 'home))
(define sys:user-shell (user-accessor "sys:user-shell" 'shell))

(define (group-accessor who tag)
  (lambda (x)
    (let ((g (if (string? x)
                 (sys:getgrnam x)
                 (sys:getgrgid x))))
      (if g
          (cdr (assq tag g))
          (error (string-append who ": no such group name or GID: " x))))))

(define sys:group-name (group-accessor "sys:group-name" 'name))
(define sys:group-gid  (group-accessor "sys:group-gid" 'gid))

(define (do-chown proc file user group)
  (let* ((s (sys:stat file))
         (u (if s
                (cdr (assq 'uid s))
                (error "chown: cannot stat" file)))
         (g (cdr (assq 'gid s)))
         (new-u (cond ((integer? user)
                        user)
                      ((string? user)
                        (let ((u (sys:user-uid user)))
                          (if u u (error "chown: no such user" user))))
                      ((not user)
                        u)
                      (else
                        (error "chown: bad user name/ID" user))))
         (new-g (cond ((integer? group)
                        group)
                      ((string? group)
                        (let ((g (sys:group-gid group)))
                          (if g g (error "chown: no such group" group))))
                      ((not group)
                        g)
                      (else
                        (error "chown: bad group name/ID" group)))))
    (proc file new-u new-g)))

(define old:chown sys:chown)

(define sys:chown
  (let ((do-chown  do-chown)
        (old:chown old:chown))
    (lambda (file user group)
      (do-chown old:chown file user group))))

(define (char-ready? . port)
  (let ((port (if (null? port)
                  (current-input-port)
                  (car port))))
    (and (sys:select '(0 0) (list (sys:fileno port)) '())
         #t)))

(define (sys-unix:sys-unix) #t)
