; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (r4rs-syntax-objects)         ==>  list
; (s9fes-syntax-objects)        ==>  list
; (r4rs-procedures)             ==>  list
; (s9fes-procedures)            ==>  list
; (s9fes-extension-procedures)  ==>  list
; (s9fes-extension-symbols)     ==>  list
;
; (load-from-library "symbols.scm")
;
; Return lists of symbols bound the corresponding type of object.
; Note: only the R4RS symbols defined in S9fES are included here.
; Caveat Utilitor.
;
; Example:   (s9fes-syntax-objects)  ==>  ()

(define (r4rs-syntax-objects)
  '(=> and begin case cond define define-syntax delay do else if
    lambda let let* letrec quote quasiquote or set!  syntax-rules
    unquote unquote-splicing))

(define (s9fes-syntax-objects)
  '())

(define (r4rs-procedures)
  '(* + - / < <= = > >= abs acos append apply asin assoc assq assv
    atan boolean? caaaar caaadr caaar caadar caaddr caadr caar
    cadaar cadadr cadar caddar cadddr caddr cadr
    call-with-current-continuation call-with-input-file
    call-with-output-file call/cc car cdaaar cdaadr cdaar cdadar
    cdaddr cdadr cdar cddaar cddadr cddar cdddar cddddr cdddr cddr
    cdr ceiling char->integer char-alphabetic? char-ci<=? char-ci<?
    char-ci=? char-ci>=? char-ci>? char-downcase char-lower-case?
    char-numeric? char-upcase char-upper-case? char-whitespace?
    char<=? char<? char=? char>=? char>? char? close-input-port
    close-output-port cons cos current-input-port current-output-port
    display eof-object? eq? equal? eqv? even? exact->inexact exact?
    exp expt floor for-each force gcd inexact->exact inexact?
    input-port? integer->char integer? lcm length list list->string
    list->vector list-ref list-tail list? load log make-string
    make-vector map max member memq memv min modulo negative? newline
    not null? number->string number? odd? open-input-file
    open-output-file output-port? pair? peek-char port? positive?
    procedure? quotient read read-char real? remainder reverse
    round set-car! set-cdr! sin sqrt string string->list string->number
    string->symbol string-append string-ci<=? string-ci<? string-ci=?
    string-ci>=? string-ci>? string-copy string-fill! string-length
    string-ref string-set! string<=? string<? string=? string>=?
    string>? string? substring symbol->string symbol? tan truncate
    unquote unquote-splicing vector vector->list vector-fill!
    vector-length vector-ref vector-set! vector? with-input-from-file
    with-output-to-file write write-char zero?))

(define (s9fes-procedures)
  '(delete-file error file-exists? fold-left fold-right gensym
    load-from-library locate-file macro-expand macro-expand-1 print
    require-extension reverse! set-input-port! set-output-port! stats
    symbols vector-append vector-copy void ** *extensions*
    *library-path* *loading*))

(define (s9fes-extension-procedures)
  '(sys:access sys:catch-errors sys:chdir sys:change-mode sys:chmod
    sys:chown sys:close sys:command-line sys:creat sys:dup sys:dup2
    sys:errno sys:execv sys:exit sys:fileno sys:flush sys:fork
    sys:get-magic-value sys:getcwd sys:getenv sys:getgid sys:getgrgid
    sys:getgrnam sys:getpgid sys:getpid sys:getpwent sys:getpwnam
    sys:getpwuid sys:gettimeofday sys:getuid sys:group-name
    sys:group-gid sys:kill sys:lchmod sys:lchown sys:lutimes sys:link
    sys:lock sys:lseek sys:lstat sys:lstat-atime sys:lstat-ctime
    sys:lstat-dev sys:lstat-gid sys:lstat-ino sys:lstat-mode
    sys:lstat-mtime sys:lstat-name sys:lstat-nlink sys:lstat-size
    sys:lstat-uid sys:make-input-port sys:make-output-port sys:mkdir
    sys:notify sys:open sys:pipe sys:read sys:readdir sys:readlink
    sys:rename sys:rmdir sys:select sys:setgid sys:setpgid sys:setuid
    sys:stat sys:lstat-block-dev? sys:lstat-char-dev? sys:lstat-directory?
    sys:lstat-pipe? sys:lstat-regular? sys:lstat-socket?
    sys:lstat-symlink? sys:stat-atime sys:stat-block-dev?
    sys:stat-char-dev? sys:stat-ctime sys:stat-dev sys:stat-directory?
    sys:stat-gid sys:stat-ino sys:stat-mode sys:stat-mtime sys:stat-name
    sys:stat-nlink sys:stat-pipe? sys:stat-regular? sys:stat-size
    sys:stat-socket? sys:stat-uid sys:strerror sys:symlink sys:system
    sys:time sys:umask sys:unlink sys:unlock sys:user-gecos
    sys:user-gid sys:user-home sys:user-shell sys:user-name
    sys:user-uid sys:utimes sys:wait sys:waitpid sys:write curs:addch
    curs:addstr curs:attroff curs:attron curs:attrset curs:beep
    curs:cbreak curs:clear curs:clearok curs:clrtobot curs:clrtoeol
    curs:cols curs:cursoff curs:curson curs:delch curs:deleteln
    curs:echo curs:endwin curs:flash curs:flushinp curs:getch
    curs:getyx curs:idlok curs:inch curs:insch curs:initscr
    curs:insertln curs:keypad curs:get-magic-value curs:lines
    curs:move curs:mvaddch curs:mvaddstr curs:mvcur curs:mvdelch
    curs:mvgetch curs:mvinch curs:mvinsch curs:nl curs:nocbreak
    curs:nodelay curs:noecho curs:nonl curs:noraw curs:raw curs:refresh
    curs:resetty curs:savetty curs:scroll curs:scrollok curs:standend
    curs:standout curs:unctrl curs:ungetch))

(define (s9fes-extension-symbols)
  '(sys:access-f-ok sys:access-r-ok sys:access-w-ok sys:access-x-ok
    sys:read+write sys:read-only sys:s-irgrp sys:s-iroth sys:s-irusr
    sys:s-irwxg sys:s-irwxo sys:s-irwxu sys:s-isgid sys:s-isuid
    sys:s-isvtx sys:s-iwgrp sys:s-iwoth sys:s-iwusr sys:s-ixgrp
    sys:s-ixoth sys:s-ixusr sys:seek-cur sys:seek-end sys:seek-set
    sys:sigabrt sys:sigalrm sys:sigbus sys:sigemt sys:sigfpe
    sys:sighup sys:sigill sys:sigint sys:sigkill sys:sigpipe
    sys:sigquit sys:sigsegv sys:sigsys sys:sigterm sys:sigtrap
    sys:write-only sys:inet-accept sys:inet-connect sys:inet-getpeername
    sys:inet-listen curs:attr-normal curs:attr-standout curs:attr-underline
    curs:attr-bold curs:key-backspace curs:key-up curs:key-down
    curs:key-left curs:key-right curs:key-home curs:key-eol
    curs:key-ppage curs:key-npage curs:key-dc curs:key-ic curs:key-end
    curs:color-black curs:color-blue curs:color-green curs:color-cyan
    curs:color-red curs:color-magenta curs:color-yellow curs:color-gray))
