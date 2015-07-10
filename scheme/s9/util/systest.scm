; Scheme 9 from Empty Space
; Unix Extension Test Suite
; By Nils M Holm, 2010,2012

; NOTE: SOCKET-TEST will fail if this test is run
; multiple times in quick succession.

(load-from-library "mergesort.scm")
(load-from-library "displaystar.scm")
(load-from-library "read-line.scm")
(load-from-library "bitops.scm")

; ----- Prelude --------------------------------------------------------------

(define *SANDBOX* "systest-sandbox")
(define *Errors*  0)
(define fd        #f)
(define fd0       #f)

(sys:catch-errors #t)

(if (zero? (sys:getuid))
    (error "friends don't let friends test as root!"))

(sys:umask #o22)

(if (not (sys:mkdir *SANDBOX*))
    (error "could not create systest sandbox"
           (sys:strerror (sys:errno))))

(sys:mkdir *SANDBOX*) ; errors caught?

(if (not (sys:chdir *SANDBOX*))
    (error "could not chdir to test sandbox!"
           (sys:strerror (sys:errno))))

(define (test2 form passed)
;  (write form) (display " => ") (write passed) (newline)
  (if (not passed)
      (begin (set! *Errors* (+ 1 *Errors*))
             (display  "FAILED: ")
             (write    form)
             (display* #\newline
                       "REASON: "
                       (sys:strerror (sys:errno))
                       #\newline))))

(define-syntax (test form)
  `(begin (sys:errno)
          (test2 ',form ,form)))

(define-syntax (test/fd form)
  `(begin (sys:errno)
          (test2 ',form
                 (let ((x ,form))
                   (set! fd x)
                   x))))

(define-syntax (test/close form)
  `(begin (sys:errno)
          (test2 ',form
                 (let ((fd ,form))
                   (sys:close fd)
                   fd))))

(define (fail2 form passed)
  (if passed
      (begin (set! *Errors* (+ 1 *Errors*))
             (display  "FAILED: ")
             (write    form)
             (display* #\newline
                       "REASON: succeeded, but should have failed"
                       #\newline))))

(define-syntax (fail form)
  `(begin (sys:errno)
          (fail2 ',form ,form)))

(define (stat-umode file)
  (bit* #o777 (sys:stat-mode file)))

; ----- Directory creation and access ----------------------------------------

(fail (sys:chdir *SANDBOX*))

(test (sys:mkdir "testdir"))
(fail (sys:mkdir "testdir"))

(test (not (sys:stat-block-dev? "testdir")))
(test (not (sys:stat-char-dev?  "testdir")))
(test      (sys:stat-directory? "testdir"))
(test (not (sys:stat-pipe?      "testdir")))
(test (not (sys:stat-regular?   "testdir")))
(test (not (sys:stat-socket?    "testdir")))
(test (not (sys:lstat-symlink?  "testdir")))

(test (sys:chdir "testdir"))
(fail (sys:chdir "testdir"))
(test (sys:chdir ".."))

(test (sys:rmdir "testdir"))
(fail (sys:rmdir "testdir"))

(test (sys:getcwd))
(test (sys:chdir (sys:getcwd)))

; ----- Stat -----------------------------------------------------------------

(test (let ((s (sys:stat ".")))
        (not (memq #f (map (lambda (x)
                             (assq x s))
                           '(name size uid gid mode ctime
                             atime mtime dev ino nlink))))))

(test/close (sys:creat "testfile"))
(test/close (sys:creat "testfile"))

(test (not (sys:stat-block-dev? "testfile")))
(test (not (sys:stat-char-dev?  "testfile")))
(test (not (sys:stat-directory? "testfile")))
(test (not (sys:stat-pipe?      "testfile")))
(test      (sys:stat-regular?   "testfile"))
(test (not (sys:stat-socket?    "testfile")))
(test (not (sys:lstat-symlink?  "testfile")))

(test (sys:getuid))
(test (sys:getgid))

(test (equal? (sys:stat-name  "testfile") "testfile"))
(test (equal? (sys:stat-size  "testfile") 0))
(test (equal? (sys:stat-uid   "testfile") (sys:getuid)))
(test (equal? (sys:stat-gid   "testfile") (sys:getgid)))
(test/close (sys:creat "testfile"))
(test (equal? (sys:stat-ctime "testfile") (sys:stat-mtime "testfile")))
(test (equal? (sys:stat-nlink "testfile") 1))

(test (sys:link "testfile" "testlink"))
(test (equal? (sys:stat-nlink "testfile") 2))

(test (equal? (sys:stat-name  "testlink") "testlink"))
(test (equal? (sys:stat-size  "testlink") 0))
(test (equal? (sys:stat-uid   "testlink") (sys:getuid)))
(test (equal? (sys:stat-gid   "testlink") (sys:getgid)))
(test (equal? (sys:stat-ctime "testlink") (sys:stat-mtime "testlink")))
(test (equal? (sys:stat-nlink "testlink") 2))

(test (sys:unlink "testfile"))
(test (equal? (sys:stat-nlink "testlink") 1))
(test (sys:unlink "testlink"))
(fail (sys:stat "testlink"))

; ----- File creation and access ---------------------------------------------

(fail (sys:open "testfile" sys:read-only))
(test/close (sys:creat "testfile"))
(test/fd (sys:open "testfile" sys:read-only))
(test (sys:close fd))
(fail (sys:close fd))

(test (sys:unlink "testfile"))
(test/close (sys:creat "testfile" #o400))
(fail (sys:open "testfile" sys:write-only))

(test (sys:unlink "testfile"))
(test/close (sys:creat "testfile" #o200))
(fail (sys:open "testfile" sys:read-only))

(test (sys:unlink "testfile"))
(test/close (sys:creat "testfile" #o600))
(test/fd (sys:open "testfile" sys:write-only))
(fail (sys:read fd 1024))
(test (sys:write fd "hello, world!"))
(test (= (sys:stat-size "testfile") 13))
(test (sys:close fd))

(test/fd (sys:open "testfile" sys:read-only))
(fail (sys:write fd "hello, world!"))
(test (equal? "hello, world!" (sys:read fd 1024)))
(test (sys:close fd))

(test/fd (sys:open "testfile" sys:read-only))
(test (equal? "hello" (sys:read fd 5)))

(test/fd (sys:open "testfile" sys:read+write))
(test (sys:write fd "HELLO, world!"))
(test (sys:lseek fd 0 sys:seek-set))
(test (equal? "HELLO, world!" (sys:read fd 1024)))
(test (sys:lseek fd -1 sys:seek-end))
(test (sys:write fd "..."))
(test (sys:lseek fd 0 sys:seek-set))
(test (equal? "HELLO, world..." (sys:read fd 1024)))
(test (sys:lseek fd -15 sys:seek-cur))
(test (sys:write fd "hello"))
(test (sys:lseek fd 0 sys:seek-set))
(test (equal? "hello, world..." (sys:read fd 1024)))

(set! fd0 fd)
(test/fd (sys:dup fd0))
(test (sys:lseek fd 0 sys:seek-set))
(test (sys:write fd "FOO"))
(test (sys:lseek fd0 0 sys:seek-set))
(test (equal? "FOO" (sys:read fd0 3)))
(test (sys:close fd0))

(set! fd0 fd)
(test/fd (sys:dup2 fd0 (sys:open "testfile" sys:read-only)))
(test (sys:lseek fd 0 sys:seek-set))
(test (sys:write fd "BAR"))
(test (sys:lseek fd0 0 sys:seek-set))
(test (equal? "BAR" (sys:read fd0 3)))
(test (sys:close fd))
(test (sys:close fd0))

(test (sys:unlink "testfile"))

; ----- File mode ------------------------------------------------------------

(test/close (sys:creat "modefile"))
(test (= (stat-umode "modefile") #o644))

(test (sys:unlink "modefile"))
(test/fd (sys:creat "modefile" #o600))
(test (= (stat-umode "modefile") #o600))

(test (sys:umask #o000))
(test (sys:unlink "modefile"))
(test (sys:creat "modefile" #o731))
(test (= (stat-umode "modefile") #o731))

(test (sys:umask #o007))
(test (sys:unlink "modefile"))
(test (sys:creat "modefile" #o731))
(test (= (stat-umode "modefile") #o730))

(test (sys:umask #o070))
(test (sys:unlink "modefile"))
(test (sys:creat "modefile" #o731))
(test (= (stat-umode "modefile") #o701))

(test (sys:umask #o700))
(test (sys:unlink "modefile"))
(test (sys:creat "modefile" #o731))
(test (= (stat-umode "modefile") #o031))

(test (sys:umask #o022))

(test (sys:chmod "modefile" #o644))
(test (= (stat-umode "modefile") #o644))
(test (sys:chmod "modefile" "644"))
(test (= (stat-umode "modefile") #o644))
(test (sys:chmod "modefile" "u=rw,g=r,o=r"))
(test (= (stat-umode "modefile") #o644))
(test (sys:chmod "modefile" "a-rwx,u+rw,go+r"))
(test (= (stat-umode "modefile") #o644))
(test (sys:chmod "modefile" "a=rwx,u-x,go-wx"))
(test (= (stat-umode "modefile") #o644))
(test (sys:chmod "modefile" "a-w"))
(test (= (stat-umode "modefile") #o444))
(test (sys:chmod "modefile" "g+x"))
(test (= (stat-umode "modefile") #o454))
(test (sys:chmod "modefile" "u+w"))
(test (= (stat-umode "modefile") #o654))

(test (sys:chmod "modefile" #o000))
(test (sys:access "modefile" sys:access-f-ok))
(fail (sys:access "modefile" sys:access-r-ok))
(fail (sys:access "modefile" sys:access-w-ok))
(fail (sys:access "modefile" sys:access-x-ok))

(test (sys:chmod "modefile" #o100))
(fail (sys:access "modefile" sys:access-r-ok))
(fail (sys:access "modefile" sys:access-w-ok))
(test (sys:access "modefile" sys:access-x-ok))

(test (sys:chmod "modefile" #o200))
(fail (sys:access "modefile" sys:access-r-ok))
(test (sys:access "modefile" sys:access-w-ok))
(fail (sys:access "modefile" sys:access-x-ok))

(test (sys:chmod "modefile" #o400))
(test (sys:access "modefile" sys:access-r-ok))
(fail (sys:access "modefile" sys:access-w-ok))
(fail (sys:access "modefile" sys:access-x-ok))

(test (sys:unlink "modefile"))

; ----- File owner -----------------------------------------------------------

(test/close (sys:creat "userfile"))

(test (sys:chown "userfile" #f #f))
(test (= (sys:stat-uid "userfile") (sys:getuid)))
(test (= (sys:stat-gid "userfile") (sys:getgid)))

(test (sys:chown "userfile" (sys:getuid) #f))
(test (sys:chown "userfile" #f (sys:getgid)))
(test (sys:chown "userfile" (sys:getuid) (sys:getgid)))
(test (= (sys:stat-uid "userfile") (sys:getuid)))
(test (= (sys:stat-gid "userfile") (sys:getgid)))

(fail (sys:chown "userfile" 0 #f))
(fail (sys:chown "userfile" 0 0))

(test (sys:unlink "userfile"))

; ----- Directory Entries ----------------------------------------------------

(test/close (sys:creat "name"))
(test (sys:rename "name" "other-name"))
(fail (sys:rename "name" "other-name"))
(test/close (sys:creat "name"))
(test (sys:rename "other-name" "name"))

(test (= 1 (sys:stat-nlink "name")))
(test (sys:link "name" "alias"))
(test (= 2 (sys:stat-nlink "name")))
(test (= 2 (sys:stat-nlink "alias")))

(test (sys:unlink "alias"))
(test (= 1 (sys:stat-nlink "name")))

(test (sys:link "name" "alias"))
(test (= 2 (sys:stat-nlink "name")))

(test (sys:symlink "name" "reference"))
(test (= 2 (sys:stat-nlink "name")))
(test (sys:lstat-symlink? "reference"))
(test (= 0 (sys:stat-size "reference")))
(test (= 4 (sys:lstat-size "reference")))
(test (= 2 (sys:stat-nlink "reference")))
(test (= 1 (sys:lstat-nlink "reference")))

(test (equal? "name" (sys:readlink "reference")))
(fail (sys:readlink "name"))

(test (sys:utimes "reference"))

(test (sys:unlink "reference"))
(test (sys:unlink "alias"))
(test (sys:unlink "name"))

(test (sys:creat "foo"))
(test (sys:creat "bar"))
(test (sys:creat "baz"))

(test (equal? '("bar" "baz" "foo") (mergesort string<? (sys:readdir "."))))

(test (sys:unlink "foo"))
(test (sys:unlink "bar"))
(test (sys:unlink "baz"))

(test (null? (sys:readdir ".")))

; ----- Processes ------------------------------------------------------------

(test/fd (sys:pipe))

(test (sys:write (cadr fd) "hello, world!"))
(test (equal? "hello, world!" (sys:read (car fd) 13)))
(test (sys:close (car fd)))
(test (sys:close (cadr fd)))

(test (= 42 (car (if (zero? (sys:fork))
                     (sys:exit 42)
                     (sys:wait)))))

(let* ((pipe (sys:pipe))
       (pid  (sys:fork)))
  (if (zero? pid)
      (let loop ()
        (sys:write (cadr pipe) (sys:read (car pipe) 1024))
        (loop))
      (and (test (sys:write (cadr pipe) "echo"))
           (test (equal? (sys:read (car pipe) 4) "echo"))
           (test (sys:notify pid))
           (sys:usleep 100000)
           (test (sys:waitpid pid)))))

(with-output-to-file
  "testprog"
  (lambda ()
    (for-each (lambda (x)
                (display* x #\newline))
              '("#! ./s9 -f"
                "(write (sys:command-line))"
                "(newline)"))))

(test (sys:chmod "testprog" #o700))

(test (let ((pipe (sys:pipe)))
        (cond ((zero? (sys:fork))
                (sys:dup2 (cadr pipe) 1)
                (sys:chdir "..")
                (sys:execv (string-append *SANDBOX* "/testprog")
                           '("foo" "bar" "baz")))
              (else
                (equal? '("foo" "bar" "baz")
                        (read (sys:make-input-port (car pipe))))))))

; XXX should also test built-in ARGV primitive

(test (let ((pipe (sys:pipe)))
        (cond ((zero? (sys:fork))
                (sys:dup2 (cadr pipe) 1)
                (sys:system "echo hello")
                (sys:exit))
              (else
                (equal? (string-append "hello" (string #\newline))
                        (sys:read (car pipe) 1024))))))

(test (let ((pipe (sys:pipe))) ; SYSTEM of core S9, not of SYS:
        (cond ((zero? (sys:fork))
                (sys:dup2 (cadr pipe) 1)
                (system "echo hello")
                (sys:exit))
              (else
                (equal? (string-append "hello" (string #\newline))
                        (sys:read (car pipe) 1024))))))

(test (sys:unlink "testprog"))

; ----- Select ---------------------------------------------------------------

(let* ((pipe (sys:pipe))
       (pid  (sys:fork)))
  (if (zero? pid)
      (let loop ()
        (sys:write (cadr pipe) (sys:read (car pipe) 1024))
        (loop))
      (and (test (sys:select '(1 0) '() (list (cadr pipe))))
           (test (not (sys:select '(0 0) (list (car pipe)) '())))
           (test (sys:write (cadr pipe) "echo"))
           (test (sys:select '(1 0) (list (car pipe)) '()))
           (test (equal? (sys:read (car pipe) 4) "echo"))
           (test (not (sys:select '(0 0) (list (car pipe)) '())))
           (test (sys:notify pid))
           (test (sys:wait)))))

; ----- Sockets --------------------------------------------------------------

(define (socket-test)
  (cond ((zero? (sys:fork))
          (let* ((sok (sys:inet-listen "127.0.0.1" "12345" 5))
                 (con (sys:inet-accept sok))
                 (in  (sys:make-input-port con))
                 (out (sys:make-output-port con)))
            (display (read-line in) out)
            (newline out)
            (sys:flush out)
            (sys:exit)))
        (else
          (sys:sleep 1)
          (let* ((sok (sys:inet-connect "127.0.0.1" "12345"))
                 (in  (sys:make-input-port sok))
                 (out (sys:make-output-port sok)))
            (display "hello, socket!" out)
            (newline out)
            (sys:flush out)
            (equal? "hello, socket!" (read-line in))))))

;(test (socket-test))

; ----- I/O Ports ------------------------------------------------------------

(test/fd (sys:creat "porttest"))

(define outport (sys:make-output-port fd))
(test outport)
(test (= (sys:fileno outport) fd))

(display "foo" outport)
(test (sys:flush outport))
(test (sys:write (sys:fileno outport) "bar"))
(close-output-port outport)

(test/fd (sys:open "porttest" sys:read-only))
(define inport (sys:make-input-port fd))
(test (= (sys:fileno inport) fd))
(test (equal? "foobar" (read-line inport)))

(test (sys:unlink "porttest"))

; ----- Users and groups -----------------------------------------------------

(test (sys:getgrgid 0))
(test (sys:getgrgid (sys:getgid)))
(test (sys:getgrnam (cdr (assq 'name (sys:getgrgid 0)))))

(test (sys:getpwuid 0))
(test (sys:getpwuid (sys:getuid)))
(test (sys:getpwnam (cdr (assq 'name (sys:getpwuid 0)))))

(test (list? (sys:getpwent)))
(test (member "root" (sys:getpwent)))

(test (let ((s (sys:getpwnam "root")))
        (not (memq #f (map (lambda (x)
                             (assq x s))
                           '(name uid gid gecos home shell))))))

(test (let ((s (sys:getgrgid 0)))
        (not (memq #f (map (lambda (x)
                             (assq x s))
                           '(name gid))))))

(test (sys:group-gid 0))
(test (sys:group-name 0))

(test (sys:user-gecos 0))
(test (sys:user-gid 0))
(test (sys:user-home 0))
(test (sys:user-name 0))
(test (sys:user-shell 0))
(test (sys:user-uid 0))

; ----- Time -----------------------------------------------------------------

(let ((t (sys:time)))
  (test (sys:sleep 1))
  (test (sys:usleep 100000))
  (test (not (= t (sys:time)))))

(let ((t (cadr (sys:gettimeofday))))
  (test (sys:usleep 1000))
  (test (not (= t (cadr (sys:gettimeofday))))))

; ----- Miscellanea ----------------------------------------------------------

(test (sys:lock "foo"))
(fail (sys:lock "foo"))
(test (sys:unlock "foo"))
(test (sys:unlock "foo"))

(test (sys:errno))
(test (sys:getcwd))
(test (zero? (sys:errno)))
(fail (sys:unlink "non-existent"))
(let ((e (sys:errno)))
  (test (not (zero? e))))
(fail (sys:unlink "non-existent"))
(test (sys:strerror (sys:errno)))
(test (zero? (sys:errno)))

; We'll just have to trust these...

(test (sys:getenv "HOME"))
(fail (sys:getenv ""))

(test (environ "HOME")) ; these two are in fact not in SYS:
(fail (environ ""))

(test (sys:getpid))

(test (sys:getgid))
(test (sys:getpgid))
(test (sys:getuid))

(test (sys:setgid (sys:getgid)))
(test (sys:setpgid (sys:getpgid)))
(test (sys:setuid (sys:getuid)))

; ----- Magic values ---------------------------------------------------------

(test (sys:get-magic-value "X_OK"))
(test (sys:get-magic-value "O_RDONLY"))
(test (sys:get-magic-value "SEEK_END"))
(test (sys:get-magic-value "SIGSEGV"))
(test (sys:get-magic-value "S_IRWXU"))

; ----- Postlude -------------------------------------------------------------

(test (sys:chdir ".."))
(test (sys:rmdir *SANDBOX*))

(if (zero? *Errors*)
    (display* "Everything fine!" #\newline)
    (display* *Errors* " errors" #\newline))
