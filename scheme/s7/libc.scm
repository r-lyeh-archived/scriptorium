;;; libc.scm
;;;
;;; tie the C library into the *libc* environment

(provide 'libc.scm)

;; if loading from a different directory, pass that info to C
(let ((current-file (port-filename (current-input-port))))
  (let ((directory (and (or (char=? (current-file 0) #\/)
			    (char=? (current-file 0) #\~))
			(substring current-file 0 (- (length current-file) 9)))))
    (when (and directory (not (member directory *load-path*)))
      (set! *load-path* (cons directory *load-path*)))
    (with-let (rootlet)
      (require cload.scm))
    (when (and directory (not (string-position directory *cload-cflags*)))
      (set! *cload-cflags* (string-append "-I" directory " " *cload-cflags*)))))

(if (not (defined? '*libc*))
    (define *libc*
      (with-let (unlet)
	(set! *libraries* (cons (cons "libc.scm" (curlet)) *libraries*))
	
	;; -------- stddef.h --------
	(define NULL (c-pointer 0))
	(define (c-null? p) (equal? p NULL))

	;; -------- stdbool.h --------
	(define false #f)
	(define true #t)

	;; -------- iso646.h --------
	;; spelled-out names for & = bitand et al

	;; -------- stdarg.h --------
	;; the varargs macros

	;; -------- assert.h --------
	;; assert macro
#|
	(define-expansion (assert assertion)
	  (reader-cond ((not (defined? 'NDEBUG))
	                `(if (not ,assertion) 
			    (error 'assert-failure "~S[~D]: ~A failed~%"
				   (port-filename) (port-line-number) ',assertion)))
	               (#t (values))))
        (define (hiho a) (assert (> a 2)) (+ a 1))

	(define-expansion (comment . stuff)
	  (reader-cond (#t (values))))
|#


	;; -------- setjmp.h --------
	;; longjmp etc

	;; -------- dlfn.h --------
	;; see libdl.scm, similarly for pthreads see libpthread.scm

	;; -------- sys/types.h inttypes.h getopt.h--------
	;; C type declarations

	(c-define 
	 '(;; -------- limits.h --------
	   (C-macro (int (SCHAR_MIN SCHAR_MAX UCHAR_MAX CHAR_BIT CHAR_MIN CHAR_MAX __WORDSIZE 
			  SHRT_MIN SHRT_MAX USHRT_MAX INT_MIN INT_MAX UINT_MAX LONG_MIN LONG_MAX ULONG_MAX
			  LLONG_MIN LLONG_MAX ULLONG_MAX
			  _POSIX_AIO_LISTIO_MAX _POSIX_AIO_MAX _POSIX_ARG_MAX _POSIX_CHILD_MAX _POSIX_DELAYTIMER_MAX _POSIX_HOST_NAME_MAX 
			  _POSIX_LINK_MAX _POSIX_LOGIN_NAME_MAX _POSIX_MAX_CANON _POSIX_MAX_INPUT _POSIX_MQ_OPEN_MAX _POSIX_MQ_PRIO_MAX 
			  _POSIX_NAME_MAX _POSIX_NGROUPS_MAX _POSIX_OPEN_MAX _POSIX_FD_SETSIZE _POSIX_PATH_MAX _POSIX_PIPE_BUF _POSIX_RE_DUP_MAX 
			  _POSIX_RTSIG_MAX _POSIX_SEM_NSEMS_MAX _POSIX_SEM_VALUE_MAX _POSIX_SIGQUEUE_MAX _POSIX_SSIZE_MAX _POSIX_STREAM_MAX 
			  _POSIX_SYMLINK_MAX _POSIX_SYMLOOP_MAX _POSIX_TIMER_MAX _POSIX_TTY_NAME_MAX _POSIX_TZNAME_MAX _POSIX_QLIMIT 
			  _POSIX_HIWAT _POSIX_UIO_MAXIOV _POSIX_CLOCKRES_MIN SSIZE_MAX NGROUPS_MAX _POSIX2_BC_BASE_MAX _POSIX2_BC_DIM_MAX 
			  _POSIX2_BC_SCALE_MAX _POSIX2_BC_STRING_MAX _POSIX2_COLL_WEIGHTS_MAX _POSIX2_EXPR_NEST_MAX _POSIX2_LINE_MAX 
			  _POSIX2_RE_DUP_MAX _POSIX2_CHARCLASS_NAME_MAX BC_BASE_MAX BC_DIM_MAX BC_SCALE_MAX BC_STRING_MAX COLL_WEIGHTS_MAX 
			  EXPR_NEST_MAX LINE_MAX CHARCLASS_NAME_MAX RE_DUP_MAX)))
	   

	   ;; -------- float.h --------
	   (C-macro (int (FLT_RADIX FLT_MANT_DIG DBL_MANT_DIG LDBL_MANT_DIG FLT_DIG DBL_DIG LDBL_DIG FLT_MIN_EXP DBL_MIN_EXP
		          LDBL_MIN_EXP FLT_MIN_10_EXP DBL_MIN_10_EXP LDBL_MIN_10_EXP FLT_MAX_EXP DBL_MAX_EXP LDBL_MAX_EXP
			  FLT_MAX_10_EXP DBL_MAX_10_EXP LDBL_MAX_10_EXP FLT_ROUNDS FLT_EVAL_METHOD)))
	   (C-macro (double (FLT_MAX DBL_MAX LDBL_MAX FLT_EPSILON DBL_EPSILON LDBL_EPSILON FLT_MIN DBL_MIN LDBL_MIN)))
	   
	   
	   ;; -------- stdint.h --------
	   (C-macro (int (INT8_MIN INT16_MIN INT32_MIN INT64_MIN INT8_MAX INT16_MAX INT32_MAX INT64_MAX UINT8_MAX UINT16_MAX 
			  UINT32_MAX UINT64_MAX INT_LEAST8_MIN INT_LEAST16_MIN INT_LEAST32_MIN INT_LEAST64_MIN INT_LEAST8_MAX 
			  INT_LEAST16_MAX INT_LEAST32_MAX INT_LEAST64_MAX UINT_LEAST8_MAX UINT_LEAST16_MAX UINT_LEAST32_MAX 
			  UINT_LEAST64_MAX INT_FAST8_MIN INT_FAST16_MIN INT_FAST32_MIN INT_FAST64_MIN INT_FAST8_MAX INT_FAST16_MAX 
			  INT_FAST32_MAX INT_FAST64_MAX UINT_FAST8_MAX UINT_FAST16_MAX UINT_FAST32_MAX UINT_FAST64_MAX INTPTR_MIN 
			  INTPTR_MAX UINTPTR_MAX INTMAX_MIN INTMAX_MAX UINTMAX_MAX PTRDIFF_MIN PTRDIFF_MAX SIG_ATOMIC_MIN SIG_ATOMIC_MAX 
			  SIZE_MAX WCHAR_MIN WCHAR_MAX WINT_MIN WINT_MAX )))
	   
	   (c-pointer (stdin stdout stderr))
	   
	   ;; -------- endian.h --------
	   ;; also has htobe16 etc
	   (C-macro (int (__BYTE_ORDER __BIG_ENDIAN __LITTLE_ENDIAN)))
	   
	   
	   (in-C "static s7_pointer g_c_pointer_to_string(s7_scheme *sc, s7_pointer args) 
                  {return(s7_make_string_with_length(sc, (const char *)s7_c_pointer(s7_car(args)), s7_integer(s7_cadr(args))));}
                  static s7_pointer g_string_to_c_pointer(s7_scheme *sc, s7_pointer args)
                  {
                   if (s7_is_string(s7_car(args)))
                     return(s7_make_c_pointer(sc, (void *)s7_string(s7_car(args))));
                   return(s7_car(args));
                  }")
	   
	   (C-function ("c-pointer->string" g_c_pointer_to_string "" 2))
	   (C-function ("string->c-pointer" g_string_to_c_pointer "" 1))
	   
	   ;; -------- ctype.h --------
	   (int isalnum (int))
	   (int isalpha (int))
	   (int iscntrl (int))
	   (int isdigit (int))
	   (int islower (int))
	   (int isgraph (int))
	   (int isprint (int))
	   (int ispunct (int))
	   (int isspace (int))
	   (int isupper (int))
	   (int isxdigit (int))
	   (int tolower (int))
	   (int toupper (int))

	   ;; -------- fcntl.h --------
	   (C-macro (int (S_IFMT S_IFDIR S_IFCHR S_IFBLK S_IFREG S_IFIFO __S_IFLNK S_IFSOCK S_ISUID S_ISGID S_IRUSR 
			  S_IWUSR S_IXUSR S_IRWXU S_IRGRP S_IWGRP S_IXGRP S_IRWXG S_IROTH S_IWOTH S_IXOTH S_IRWXO R_OK W_OK X_OK 
			  F_OK SEEK_SET SEEK_CUR SEEK_END F_ULOCK F_LOCK F_TLOCK F_TEST O_ACCMODE O_RDONLY O_WRONLY O_RDWR O_CREAT 
			  O_EXCL O_NOCTTY O_TRUNC O_APPEND O_NONBLOCK O_NDELAY O_SYNC O_FSYNC O_ASYNC O_DSYNC O_RSYNC O_LARGEFILE 
			  F_DUPFD F_GETFD F_SETFD F_GETFL F_SETFL F_GETLK F_SETLK F_SETLKW F_GETLK64 F_SETLK64 F_SETLKW64 
			  FD_CLOEXEC F_RDLCK F_WRLCK F_UNLCK POSIX_FADV_NORMAL POSIX_FADV_RANDOM POSIX_FADV_SEQUENTIAL 
			  POSIX_FADV_WILLNEED POSIX_FADV_DONTNEED POSIX_FADV_NOREUSE)))
	   (int fcntl (int int))
	   (in-C "static s7_pointer g_c_open(s7_scheme *sc, s7_pointer args)
                  {
                    s7_pointer arg;
                    char* name;
                    int flags, mode;
                    arg = args;
                    if (s7_is_string(s7_car(arg)))
                       name = (char*)s7_string(s7_car(arg));
                    else return(s7_wrong_type_arg_error(sc, \"open\", 1, s7_car(arg), \"string\"));
                    arg = s7_cdr(arg);
                    if (s7_is_integer(s7_car(arg)))
                       flags = (int)s7_integer(s7_car(arg));
                    else return(s7_wrong_type_arg_error(sc, \"open\", 2, s7_car(arg), \"integer\"));
                    if (s7_is_pair(s7_cdr(arg)))
                      {
                        arg = s7_cdr(arg);
                        if (s7_is_integer(s7_car(arg)))
                          mode = (int)s7_integer(s7_car(arg));
                        else return(s7_wrong_type_arg_error(sc, \"open\", 3, s7_car(arg), \"integer\"));
                        return(s7_make_integer(sc, (s7_int)open(name, flags, mode)));
                       }
                     return(s7_make_integer(sc, (s7_int)open(name, flags)));
                    }")
	   (C-function ("open" g_c_open "" 2 1))
	   (int creat (char* (mode_t int)))
	   (int lockf (int int int))
	   (reader-cond ((provided? 'linux) 
			 (int posix_fadvise (int int int int))
			 (int posix_fallocate (int int int))))
	   
	   
	   ;; -------- fenv.h --------
	   (C-macro (int (FE_INEXACT FE_DIVBYZERO FE_UNDERFLOW FE_OVERFLOW FE_INVALID FE_ALL_EXCEPT
			  FE_TONEAREST FE_UPWARD FE_DOWNWARD FE_TOWARDZERO)))
	   (int feclearexcept (int))
	   (int fegetexceptflag (fexcept_t* int) )
	   (int feraiseexcept (int) )
	   (int fesetexceptflag (fexcept_t* int) )
	   (int fetestexcept (int) )
	   (int fegetround (void) )
	   (int fesetround (int) )
	   (int fegetenv (fenv_t*) )
	   (int feholdexcept (fenv_t*) )
	   (int fesetenv (fenv_t*) )
	   (int feupdateenv (fenv_t*) )
	   
	   
	   ;; -------- fnmatch.h --------
	   (C-macro (int (FNM_PATHNAME FNM_NOESCAPE FNM_PERIOD FNM_FILE_NAME FNM_LEADING_DIR FNM_CASEFOLD FNM_EXTMATCH FNM_NOMATCH)))
	   (int fnmatch (char* char* int))
	   
	   
	   ;; -------- string.h --------
	   (void* memcpy (void* void* size_t))
	   (void* memmove (void* void* size_t))
	   (void* memset (void* int size_t))
	   (int memcmp (void* void* size_t))
	   (void* memchr (void* int size_t))
	   (char* strcpy (char* char*))
	   (char* strncpy (char* char* size_t))
	   (char* strcat (char* char*))
	   (char* strncat (char* char* size_t))
	   (int strcmp (char* char*))
	   (int strncmp (char* char* size_t))
	   (int strcoll (char* char*))
	   (size_t strxfrm (char* char* size_t))
	   (char* strchr (char* int))
	   (char* strrchr (char* int))
	   (size_t strcspn (char* char*))
	   (size_t strspn (char* char*))
	   (char* strpbrk (char* char*))
	   (char* strstr (char* char*))
	   (char* strtok (char* char*))
	   (size_t strlen (char*))
	   (reader-cond ((not (provided? 'osx)) (size_t strnlen (char* size_t))))
	   ;; strnlen is in OSX 10.8, not 10.6
	   (char* strerror (int))
	   (int strcasecmp (char* char*))
	   (int strncasecmp (char* char* size_t))
	   
	   
	   ;; -------- stdio.h --------
	   (C-macro (int (_IOFBF _IOLBF _IONBF BUFSIZ EOF L_tmpnam TMP_MAX FILENAME_MAX L_ctermid L_cuserid FOPEN_MAX IOV_MAX)))
	   (C-macro (char* P_tmpdir))
	   
	   (int remove (char*))
	   (int rename (char* char*))
	   (FILE* tmpfile (void))
	   (reader-cond ((not (provided? 'osx)) 
			 (char* tmpnam (char*))
			 (char* tempnam (char* char*))))
	   (int fclose (FILE*))
	   (int fflush (FILE*))
	   ;;		    (reader-cond ((provided? 'linux) (int fcloseall (void))))
	   (FILE* fopen (char* char*))
	   (FILE* freopen (char*  char* FILE*))
	   (FILE* fdopen (int char*))
	   (void setbuf (FILE* char*))
	   (int setvbuf (FILE* char* int size_t))
	   (void setlinebuf (FILE*))
	   (int fgetc (FILE*))
	   (int getc (FILE*))
	   (int getchar (void))
	   (int fputc (int FILE*))
	   (int putc (int FILE*))
	   (int putchar (int))
	   (char* fgets (char* int FILE*))
	   (int fputs (char* FILE*))
	   (int puts (char*))
	   (int ungetc (int FILE*))
	   (size_t fread (void* size_t size_t FILE*))
	   (size_t fwrite (void* size_t size_t FILE*))
	   (int fseek (FILE* int int))
	   (int ftell (FILE*))
	   (void rewind (FILE*))
	   (int fgetpos (FILE* fpos_t*))
	   (int fsetpos (FILE* fpos_t*))
	   (void clearerr (FILE*))
	   (int feof (FILE*))
	   (int ferror (FILE*))
	   (void perror (char*))
	   (int fileno (FILE*))
	   (FILE* popen (char* char*))
	   (int pclose (FILE*))
	   (char* ctermid (char*))
	   ;;		    (reader-cond ((provided? 'linux) (char* cuserid (char*))))
	   (void flockfile (FILE*))
	   (int ftrylockfile (FILE*))
	   (void funlockfile (FILE*))
	   ;; int fprintf (FILE* char* ...)
	   ;; int printf (char* ...)
	   ;; int sprintf (char* char* ...) 
	   ;; int vfprintf (FILE* char* va_list)
	   ;; int vprintf (char* va_list)
	   ;; int vsprintf (char* char* va_list) 
	   ;; int snprintf (char* size_t char* ...)
	   ;; int vsnprintf (char* size_t char* va_list)
	   ;; int vasprintf (char** char* va_list)
	   ;; int asprintf (char** char* ...)
	   ;; int fscanf (FILE* char* ...)
	   ;; int scanf (char* ...)
	   ;; int sscanf (char* char* ...) 
	   ;; int vfscanf (FILE* char* va_list)
	   ;; int vscanf (char* va_list)
	   ;; int vsscanf (char* char* va_list)
	   
	   
	   ;; -------- stdlib.h --------
	   (C-macro (int (RAND_MAX EXIT_FAILURE EXIT_SUCCESS MB_CUR_MAX)))
	   (double atof (char*))
	   (int atoi (char*))
	   (int atol (char*))
	   (int atoll (char*))
	   (int random (void))
	   (void srandom (int))
	   (char* initstate (int char* size_t))
	   (char* setstate (char*))
	   (int rand (void))
	   (void srand (int))
	   (void* malloc (size_t))
	   (void* calloc (size_t size_t))
	   (void* realloc (void* size_t))
	   (void free (void*))
	   (void abort (void))
	   (void exit (int))
	   (char* getenv (char*))
	   (int putenv (char*))
	   (int setenv (char* char* int))
	   (int unsetenv (char*))
	   (char* mktemp (char*))
	   (int mkstemp (char*))
	   (int system (char*))
	   (char* realpath (char* char*))
	   (int abs (int))
	   (int labs (int))
	   
	   (in-C "static s7_pointer g_llabs(s7_scheme *sc, s7_pointer args) 
                  {
                  #if  ((__GNUC__) && ((__GNUC__ < 4) || ((__GNUC__ == 4) && (__GNUC_MINOR__ < 4))))
                    return(s7_make_integer(sc, labs(s7_integer(s7_car(args)))));
                  #else
                    return(s7_make_integer(sc, llabs(s7_integer(s7_car(args)))));
                  #endif
                  }
                 static s7_pointer g_strtod(s7_scheme *sc, s7_pointer args) 
                 {return(s7_make_real(sc, strtod(s7_string(s7_car(args)), NULL)));}
                 static s7_pointer g_strtof(s7_scheme *sc, s7_pointer args) 
                 {return(s7_make_real(sc, strtof(s7_string(s7_car(args)), NULL)));}
                 static s7_pointer g_strtol(s7_scheme *sc, s7_pointer args) 
                 {return(s7_make_integer(sc, strtol(s7_string(s7_car(args)), NULL, s7_integer(s7_cadr(args)))));}
                 static s7_pointer g_strtoll(s7_scheme *sc, s7_pointer args)
                 {return(s7_make_integer(sc, strtoll(s7_string(s7_car(args)), NULL, s7_integer(s7_cadr(args)))));}
                 static s7_pointer g_div(s7_scheme *sc, s7_pointer args)
                 {
                   div_t d;
                   d = div(s7_integer(s7_car(args)), s7_integer(s7_cadr(args)));
                   return(s7_list(sc, 2, s7_make_integer(sc, d.quot), s7_make_integer(sc, d.rem)));
                 }
                  static s7_pointer g_ldiv(s7_scheme *sc, s7_pointer args)
                 {
                   ldiv_t d;
                   d = ldiv(s7_integer(s7_car(args)), s7_integer(s7_cadr(args)));
                   return(s7_list(sc, 2, s7_make_integer(sc, d.quot), s7_make_integer(sc, d.rem)));
                 }
                  ")
	   (C-function ("llabs" g_llabs "" 1))
	   (C-function ("strtod" g_strtod "" 1))
	   (C-function ("strtof" g_strtof "" 1))
	   (C-function ("strtol" g_strtol "" 2))
	   (C-function ("strtoll" g_strtoll "" 2))
	   (C-function ("div" g_div "" 1))
	   (C-function ("ldiv" g_ldiv "" 1))
	   
	   
	   ;; -------- errno.h --------
	   ;; pws for errno?
	   (C-macro (int (__GLIBC__ __GLIBC_MINOR__ ; features.h from errno.h
			  ECANCELED EOWNERDEAD ENOTRECOVERABLE ERFKILL EILSEQ
			  ;; asm-generic/errno-base.h
			  EPERM ENOENT ESRCH EINTR EIO ENXIO E2BIG ENOEXEC EBADF ECHILD EAGAIN ENOMEM EACCES EFAULT
			  ENOTBLK EBUSY EEXIST EXDEV ENODEV ENOTDIR EISDIR EINVAL ENFILE EMFILE ENOTTY ETXTBSY EFBIG
			  ENOSPC ESPIPE EROFS EMLINK EPIPE EDOM ERANGE
			  )))
	   (in-C "static s7_pointer g_errno(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, errno));}
                           static s7_pointer g_set_errno(s7_scheme *sc, s7_pointer args) {errno = (int)s7_integer(s7_car(args)); return(s7_car(args));}")
	   (C-function ("errno" g_errno "" 0))
	   (C-function ("set_errno" g_set_errno "" 1))
	   
	   
	   ;; -------- locale.h --------
	   (C-macro (int (LC_CTYPE LC_NUMERIC LC_TIME LC_COLLATE LC_MONETARY LC_MESSAGES LC_ALL LC_PAPER LC_NAME 
			  LC_ADDRESS LC_TELEPHONE LC_MEASUREMENT LC_IDENTIFICATION)))
	   (char* setlocale (int char*))
	   (in-C "
             static s7_pointer g_localeconv(s7_scheme *sc, s7_pointer args)
             {
               struct lconv *lc;
               lc = localeconv();
               return(s7_list(sc, 18,
             		     s7_cons(sc, s7_make_symbol(sc, \"decimal_point\"),     s7_make_string(sc, lc->decimal_point)),
             		     s7_cons(sc, s7_make_symbol(sc, \"thousands_sep\"),     s7_make_string(sc, lc->thousands_sep)),
             		     s7_cons(sc, s7_make_symbol(sc, \"grouping\"),          s7_make_string(sc, lc->grouping)),
             		     s7_cons(sc, s7_make_symbol(sc, \"int_curr_symbol\"),   s7_make_string(sc, lc->int_curr_symbol)),
             		     s7_cons(sc, s7_make_symbol(sc, \"currency_symbol\"),   s7_make_string(sc, lc->currency_symbol)),
             		     s7_cons(sc, s7_make_symbol(sc, \"mon_decimal_point\"), s7_make_string(sc, lc->mon_decimal_point)),
             		     s7_cons(sc, s7_make_symbol(sc, \"mon_thousands_sep\"), s7_make_string(sc, lc->mon_thousands_sep)),
             		     s7_cons(sc, s7_make_symbol(sc, \"mon_grouping\"),      s7_make_string(sc, lc->mon_grouping)),
             		     s7_cons(sc, s7_make_symbol(sc, \"positive_sign\"),     s7_make_string(sc, lc->positive_sign)),
             		     s7_cons(sc, s7_make_symbol(sc, \"negative_sign\"),     s7_make_string(sc, lc->negative_sign)),
             		     
             		     s7_cons(sc, s7_make_symbol(sc, \"int_frac_digits\"),   s7_make_integer(sc, lc->int_frac_digits)),
             		     s7_cons(sc, s7_make_symbol(sc, \"frac_digits\"),       s7_make_integer(sc, lc->frac_digits)),
             		     s7_cons(sc, s7_make_symbol(sc, \"p_cs_precedes\"),     s7_make_integer(sc, lc->p_cs_precedes)),
             		     s7_cons(sc, s7_make_symbol(sc, \"p_sep_by_space\"),    s7_make_integer(sc, lc->p_sep_by_space)),
             		     s7_cons(sc, s7_make_symbol(sc, \"n_cs_precedes\"),     s7_make_integer(sc, lc->n_cs_precedes)),
             		     s7_cons(sc, s7_make_symbol(sc, \"n_sep_by_space\"),    s7_make_integer(sc, lc->n_sep_by_space)),
             		     s7_cons(sc, s7_make_symbol(sc, \"p_sign_posn\"),       s7_make_integer(sc, lc->p_sign_posn)),
             		     s7_cons(sc, s7_make_symbol(sc, \"n_sign_posn\"),       s7_make_integer(sc, lc->n_sign_posn))));
              }")
	   (C-function ("localeconv" g_localeconv "" 0))
	   
	   
	   ;; -------- sys/utsname.h --------
	   (in-C "
             static s7_pointer g_uname(s7_scheme *sc, s7_pointer args)
             {
               struct utsname buf;
               uname(&buf);
               return(s7_list(sc, 5, s7_make_string(sc, buf.sysname), 
             		        s7_make_string(sc, buf.machine), 
             		        s7_make_string(sc, buf.nodename), 
             		        s7_make_string(sc, buf.version), 
             		        s7_make_string(sc, buf.release)));
             }")
	   (C-function ("uname" g_uname "" 0))
	   
	   
	   ;; -------- unistd.h --------                  
	   (C-macro (int (_POSIX_VERSION _POSIX2_VERSION _POSIX_JOB_CONTROL _POSIX_SAVED_IDS _POSIX_PRIORITY_SCHEDULING _POSIX_SYNCHRONIZED_IO
			  _POSIX_FSYNC _POSIX_MAPPED_FILES _POSIX_MEMLOCK _POSIX_MEMLOCK_RANGE _POSIX_MEMORY_PROTECTION _POSIX_CHOWN_RESTRICTED
			  _POSIX_VDISABLE _POSIX_NO_TRUNC _POSIX_THREADS _POSIX_REENTRANT_FUNCTIONS _POSIX_THREAD_SAFE_FUNCTIONS
			  _POSIX_THREAD_PRIORITY_SCHEDULING _POSIX_THREAD_ATTR_STACKSIZE _POSIX_THREAD_ATTR_STACKADDR _POSIX_THREAD_PRIO_INHERIT
			  _POSIX_THREAD_PRIO_PROTECT _POSIX_SEMAPHORES _POSIX_REALTIME_SIGNALS _POSIX_ASYNCHRONOUS_IO _POSIX_ASYNC_IO
			  _POSIX_PRIORITIZED_IO _POSIX_SHARED_MEMORY_OBJECTS _POSIX_CPUTIME _POSIX_THREAD_CPUTIME _POSIX_REGEXP
			  _POSIX_READER_WRITER_LOCKS _POSIX_SHELL _POSIX_TIMEOUTS _POSIX_SPIN_LOCKS _POSIX_SPAWN _POSIX_TIMERS 
			  _POSIX_BARRIERS _POSIX_MESSAGE_PASSING _POSIX_THREAD_PROCESS_SHARED _POSIX_MONOTONIC_CLOCK _POSIX_CLOCK_SELECTION
			  _POSIX_ADVISORY_INFO _POSIX_IPV6 _POSIX_RAW_SOCKETS _POSIX2_CHAR_TERM _POSIX_SPORADIC_SERVER _POSIX_THREAD_SPORADIC_SERVER
			  _POSIX_TRACE _POSIX_TRACE_EVENT_FILTER _POSIX_TRACE_INHERIT _POSIX_TRACE_LOG _POSIX_TYPED_MEMORY_OBJECTS 
			  STDIN_FILENO STDOUT_FILENO STDERR_FILENO)))
	   
	   (C-macro 
	    (int (_PC_LINK_MAX _PC_MAX_CANON _PC_MAX_INPUT _PC_NAME_MAX _PC_PATH_MAX _PC_PIPE_BUF _PC_CHOWN_RESTRICTED _PC_NO_TRUNC
		  _PC_VDISABLE _PC_SYNC_IO _PC_ASYNC_IO _PC_PRIO_IO _PC_SOCK_MAXBUF _PC_FILESIZEBITS _PC_REC_INCR_XFER_SIZE _PC_REC_MAX_XFER_SIZE
		  _PC_REC_MIN_XFER_SIZE _PC_REC_XFER_ALIGN _PC_ALLOC_SIZE_MIN _PC_SYMLINK_MAX _PC_2_SYMLINKS _SC_ARG_MAX _SC_CHILD_MAX _SC_CLK_TCK
		  _SC_NGROUPS_MAX _SC_OPEN_MAX _SC_STREAM_MAX _SC_TZNAME_MAX _SC_JOB_CONTROL _SC_SAVED_IDS _SC_REALTIME_SIGNALS _SC_PRIORITY_SCHEDULING
		  _SC_TIMERS _SC_ASYNCHRONOUS_IO _SC_PRIORITIZED_IO _SC_SYNCHRONIZED_IO _SC_FSYNC _SC_MAPPED_FILES _SC_MEMLOCK _SC_MEMLOCK_RANGE
		  _SC_MEMORY_PROTECTION _SC_MESSAGE_PASSING _SC_SEMAPHORES _SC_SHARED_MEMORY_OBJECTS _SC_AIO_LISTIO_MAX _SC_AIO_MAX _SC_AIO_PRIO_DELTA_MAX
		  _SC_DELAYTIMER_MAX _SC_MQ_OPEN_MAX _SC_MQ_PRIO_MAX _SC_VERSION _SC_PAGESIZE _SC_PAGE_SIZE _SC_RTSIG_MAX _SC_SEM_NSEMS_MAX _SC_SEM_VALUE_MAX
		  _SC_SIGQUEUE_MAX _SC_TIMER_MAX _SC_BC_BASE_MAX _SC_BC_DIM_MAX _SC_BC_SCALE_MAX _SC_BC_STRING_MAX _SC_COLL_WEIGHTS_MAX _SC_EQUIV_CLASS_MAX
		  _SC_EXPR_NEST_MAX _SC_LINE_MAX _SC_RE_DUP_MAX _SC_CHARCLASS_NAME_MAX _SC_2_VERSION _SC_2_C_BIND _SC_2_C_DEV _SC_2_FORT_DEV _SC_2_FORT_RUN
		  _SC_2_SW_DEV _SC_2_LOCALEDEF _SC_PII _SC_PII_XTI _SC_PII_SOCKET _SC_PII_INTERNET _SC_PII_OSI _SC_POLL _SC_SELECT _SC_UIO_MAXIOV 
		  _SC_IOV_MAX _SC_PII_INTERNET_STREAM _SC_PII_INTERNET_DGRAM _SC_PII_OSI_COTS _SC_PII_OSI_CLTS _SC_PII_OSI_M _SC_T_IOV_MAX _SC_THREADS
		  _SC_THREAD_SAFE_FUNCTIONS _SC_GETGR_R_SIZE_MAX _SC_GETPW_R_SIZE_MAX _SC_LOGIN_NAME_MAX _SC_TTY_NAME_MAX _SC_THREAD_DESTRUCTOR_ITERATIONS 
		  _SC_THREAD_KEYS_MAX _SC_THREAD_STACK_MIN _SC_THREAD_THREADS_MAX _SC_THREAD_ATTR_STACKADDR _SC_THREAD_ATTR_STACKSIZE 
		  _SC_THREAD_PRIO_INHERIT _SC_THREAD_PRIO_PROTECT _SC_THREAD_PROCESS_SHARED _SC_NPROCESSORS_CONF _SC_NPROCESSORS_ONLN _SC_PHYS_PAGES 
		  _SC_AVPHYS_PAGES _SC_ATEXIT_MAX _SC_PASS_MAX _SC_2_CHAR_TERM _SC_2_C_VERSION _SC_2_UPE _SC_CHAR_BIT _SC_CHAR_MAX _SC_CHAR_MIN _SC_INT_MAX
		  _SC_INT_MIN _SC_LONG_BIT _SC_WORD_BIT _SC_MB_LEN_MAX _SC_NZERO _SC_SSIZE_MAX _SC_SCHAR_MAX _SC_SCHAR_MIN _SC_SHRT_MAX _SC_SHRT_MIN
		  _SC_UCHAR_MAX _SC_UINT_MAX _SC_ULONG_MAX _SC_USHRT_MAX _SC_NL_ARGMAX _SC_NL_LANGMAX _SC_NL_MSGMAX _SC_NL_NMAX _SC_NL_SETMAX
		  _SC_NL_TEXTMAX _SC_ADVISORY_INFO _SC_BARRIERS _SC_BASE _SC_C_LANG_SUPPORT _SC_C_LANG_SUPPORT_R _SC_CLOCK_SELECTION _SC_CPUTIME
		  _SC_THREAD_CPUTIME _SC_DEVICE_IO _SC_DEVICE_SPECIFIC _SC_DEVICE_SPECIFIC_R _SC_FD_MGMT _SC_FIFO _SC_PIPE _SC_FILE_ATTRIBUTES
		  _SC_FILE_LOCKING _SC_FILE_SYSTEM _SC_MONOTONIC_CLOCK _SC_MULTI_PROCESS _SC_SINGLE_PROCESS _SC_NETWORKING _SC_READER_WRITER_LOCKS
		  _SC_SPIN_LOCKS _SC_REGEXP _SC_REGEX_VERSION _SC_SHELL _SC_SIGNALS _SC_SPAWN _SC_SPORADIC_SERVER _SC_THREAD_SPORADIC_SERVER
		  _SC_SYSTEM_DATABASE _SC_SYSTEM_DATABASE_R _SC_TIMEOUTS _SC_TYPED_MEMORY_OBJECTS _SC_USER_GROUPS _SC_USER_GROUPS_R
		  _SC_2_PBS _SC_2_PBS_ACCOUNTING _SC_2_PBS_LOCATE _SC_2_PBS_MESSAGE _SC_2_PBS_TRACK _SC_SYMLOOP_MAX _SC_STREAMS _SC_2_PBS_CHECKPOINT
		  _SC_HOST_NAME_MAX _SC_TRACE _SC_TRACE_EVENT_FILTER _SC_TRACE_INHERIT _SC_TRACE_LOG _SC_LEVEL1_ICACHE_SIZE _SC_LEVEL1_ICACHE_ASSOC
		  _SC_LEVEL1_ICACHE_LINESIZE _SC_LEVEL1_DCACHE_SIZE _SC_LEVEL1_DCACHE_ASSOC _SC_LEVEL1_DCACHE_LINESIZE _SC_LEVEL2_CACHE_SIZE 
		  _SC_LEVEL2_CACHE_LINESIZE _SC_LEVEL3_CACHE_SIZE _SC_LEVEL3_CACHE_ASSOC _SC_LEVEL3_CACHE_LINESIZE _SC_LEVEL4_CACHE_SIZE 
		  _SC_LEVEL4_CACHE_LINESIZE _SC_IPV6 _SC_RAW_SOCKETS _SC_SS_REPL_MAX _SC_TRACE_EVENT_NAME_MAX _SC_TRACE_NAME_MAX _SC_TRACE_SYS_MAX
		  _SC_TRACE_USER_EVENT_MAX _SC_THREAD_ROBUST_PRIO_INHERIT _SC_THREAD_ROBUST_PRIO_PROTECT _CS_PATH _CS_GNU_LIBC_VERSION 
		  _SC_THREAD_PRIORITY_SCHEDULING _SC_LEVEL2_CACHE_ASSOC _SC_LEVEL4_CACHE_ASSOC _CS_GNU_LIBPTHREAD_VERSION)))
	   
	   (int access (char* int))
	   (int lseek (int int int))
	   (int close (int))
	   (ssize_t read (int void* size_t))
	   (ssize_t write (int void* size_t))
	   (ssize_t pread (int void* size_t int))
	   (ssize_t pwrite (int void* size_t int))
	   (int pipe (int*))
	   (int alarm (int))
	   (int sleep (int))
	   (int pause (void))
	   (int chown (char* int int))
	   (int chdir (char*))
	   (char* getcwd (char* size_t))
	   ;; (deprecated) (char* getwd (char*))
	   (int dup (int))
	   (int dup2 (int int))
	   (void _exit (int))
	   (int pathconf (char* int))
	   (int fpathconf (int int))
	   (int sysconf (int))
	   (size_t confstr (int char* size_t))
	   (int getpid (void))
	   (int getppid (void))
	   (int getpgid (int))
	   (int setpgid (int int))
	   (int setsid (void))
	   (int getsid (int))
	   (int getuid (void))
	   (int geteuid (void))
	   (int getgid (void))
	   (int getegid (void))
	   (int setuid (int))
	   (int setgid (int))
	   (int fork (void))
	   (char* ttyname (int))
	   (int isatty (int))
	   (int link (char* char*))
	   (int unlink (char*))
	   (int rmdir (char*))
	   (int tcgetpgrp (int))
	   (int tcsetpgrp (int int))
	   (char* getlogin (void))
	   (int truncate (char* int))
	   (int ftruncate (int int))
	   
	   (in-C "extern char **environ; 
                  static s7_pointer getenvs(s7_scheme *sc, s7_pointer args)
                  {
                    s7_pointer p;
                    int i;
                    p = s7_nil(sc);
                    for (i = 0; environ[i]; i++)
                      {
                       const char *eq;
                       s7_pointer name, value;
                       eq = strchr((const char *)environ[i], (int)'=');
                       name = s7_make_string_with_length(sc, environ[i], eq - environ[i]);
                       value = s7_make_string(sc, (char *)(eq + 1));
                       p = s7_cons(sc, s7_cons(sc, name, value), p);
                      }
                    return(p);
           }
                  static s7_pointer g_getgroups(s7_scheme *sc, s7_pointer args)
                  {
                    gid_t *gds;
                    int i, size, res;
                    s7_pointer lst;
                    size = s7_integer(s7_car(args));
                    if (size == 0)
                      return(s7_make_integer(sc, getgroups(0, NULL)));
                    gds = (gid_t *)calloc(size, sizeof(gid_t));
                    res = getgroups(size, gds);
                    if (res != -1)
                      {
                        lst = s7_nil(sc);
                        for (i = 0; i < size; i++)
                          lst = s7_cons(sc, s7_make_integer(sc, gds[i]), lst);
                      }
                    else lst = s7_make_integer(sc, -1);
                    free(gds);
                    return(lst);
                  }
                  ")
	   (C-function ("getenvs" getenvs "(getenvs) returns all the environment variables in an alist" 0))
	   (C-function ("getgroups" g_getgroups "" 1))
	   
	   ;; perhaps call these as (define* n (path ...) = args? and use execve for all?
	   ;;   but are these useful in this context?  How is fork used here?
	   ;; int execve (char* path  char* argv[]  char* envp[])
	   ;; int execv (char* path  char* argv[])
	   ;; int execle (char* path  char* arg  ...)
	   ;; int execl (char* path  char* arg  ...)
	   ;; int execvp (char* file  char* argv[])
	   ;; int execlp (char* file  char* arg  ...)
	   
	   
	   ;; -------- dirent.h --------
	   (DIR* opendir (char*))
	   (int closedir (DIR*))
	   (void rewinddir (DIR*))
	   (in-C "static char *read_dir(DIR *p)
                           {                            
                             struct dirent *dirp;        
                             dirp = readdir(p);          
                             if (!dirp) return(NULL);    
                             else return(dirp->d_name);  
                           }")
	   (char* read_dir (DIR*))
	   ;; int scandir (char* dirent*** func func)
	   ;; int alphasort (dirent** dirent**)
	   
	   
	   ;; -------- ftw.h --------
	   (C-macro (int (FTW_F FTW_D FTW_DNR FTW_NS)))
	   (in-C "static s7_scheme *internal_ftw_sc = NULL;
                  static s7_pointer internal_ftw_closure = NULL, internal_ftw_arglist = NULL;
                           
                  static int internal_ftw_function(const char *fpath, const struct stat *sb, int typeflag)
                  {
                    s7_list_set(internal_ftw_sc, internal_ftw_arglist, 0, s7_make_string(internal_ftw_sc, fpath));
                    s7_list_set(internal_ftw_sc, internal_ftw_arglist, 1, s7_make_c_pointer(internal_ftw_sc, (void *)sb));
                    s7_list_set(internal_ftw_sc, internal_ftw_arglist, 2, s7_make_integer(internal_ftw_sc, typeflag));
                    return((int)s7_integer(s7_call(internal_ftw_sc, internal_ftw_closure, internal_ftw_arglist)));
                  }
                    
                  static s7_pointer g_ftw(s7_scheme *sc, s7_pointer args)
                  {
                    if (!internal_ftw_sc)
                      {
                        internal_ftw_sc = sc;
                        internal_ftw_arglist = s7_list(sc, 3, s7_nil(sc), s7_nil(sc), s7_nil(sc));
                        s7_gc_protect(sc, internal_ftw_arglist);
                      }
                    internal_ftw_closure = s7_cadr(args);
                    return(s7_make_integer(sc, ftw(s7_string(s7_car(args)), internal_ftw_function, s7_integer(s7_caddr(args)))));
                  }")
	   (C-function ("ftw" g_ftw "" 3))
	   
	   
	   ;; -------- sys/stat.h --------
	   (C-macro (int S_IFLNK))
	   
	   (in-C "static s7_pointer g_stat(s7_scheme *sc, s7_pointer args)
                    {return(s7_make_integer(sc, stat(s7_string(s7_car(args)), (struct stat *)s7_c_pointer(s7_cadr(args)))));}
                  static s7_pointer g_fstat(s7_scheme *sc, s7_pointer args)
                    {return(s7_make_integer(sc, fstat(s7_integer(s7_car(args)), (struct stat *)s7_c_pointer(s7_cadr(args)))));}
                  static s7_pointer g_lstat(s7_scheme *sc, s7_pointer args)
                    {return(s7_make_integer(sc, lstat(s7_string(s7_car(args)), (struct stat *)s7_c_pointer(s7_cadr(args)))));}
                  ")
	   (C-function ("stat" g_stat "" 2))
	   (C-function ("fstat" g_fstat "" 2))
	   (C-function ("lstat" g_lstat "" 2))
	   
	   (int chmod (char* int))
	   (int mkdir (char* int))
	   (int mknod (char* int int))
	   (int mkfifo (char* int))
	   
	   (in-C "static s7_pointer g_isdir(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_boolean(sc, S_ISDIR(s7_integer(s7_car(args)))));}
                  static s7_pointer g_ischr(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_boolean(sc, S_ISCHR(s7_integer(s7_car(args)))));}
                  static s7_pointer g_isblk(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_boolean(sc, S_ISBLK(s7_integer(s7_car(args)))));}
                  static s7_pointer g_isreg(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_boolean(sc, S_ISREG(s7_integer(s7_car(args)))));}
                  static s7_pointer g_isfifo(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_boolean(sc, S_ISFIFO(s7_integer(s7_car(args)))));}
                  static s7_pointer g_islnk(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_boolean(sc, S_ISLNK(s7_integer(s7_car(args)))));}
                  static s7_pointer g_issock(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_boolean(sc, S_ISSOCK(s7_integer(s7_car(args)))));}
                  static s7_pointer g_st_dev(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, ((struct stat *)s7_c_pointer(s7_car(args)))->st_dev));}
                  static s7_pointer g_st_ino(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, ((struct stat *)s7_c_pointer(s7_car(args)))->st_ino));}
                  static s7_pointer g_st_mode(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, ((struct stat *)s7_c_pointer(s7_car(args)))->st_mode));}
                  static s7_pointer g_st_nlink(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, ((struct stat *)s7_c_pointer(s7_car(args)))->st_nlink));}
                  static s7_pointer g_st_uid(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, ((struct stat *)s7_c_pointer(s7_car(args)))->st_uid));}
                  static s7_pointer g_st_gid(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, ((struct stat *)s7_c_pointer(s7_car(args)))->st_gid));}
                  static s7_pointer g_st_rdev(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, ((struct stat *)s7_c_pointer(s7_car(args)))->st_rdev));}
                  static s7_pointer g_st_size(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, ((struct stat *)s7_c_pointer(s7_car(args)))->st_size));}
                  static s7_pointer g_st_blksize(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, ((struct stat *)s7_c_pointer(s7_car(args)))->st_blksize));}
                  static s7_pointer g_st_blocks(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, ((struct stat *)s7_c_pointer(s7_car(args)))->st_blocks));}
                  static s7_pointer g_st_atime(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, ((struct stat *)s7_c_pointer(s7_car(args)))->st_atime));}
                  static s7_pointer g_st_mtime(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, ((struct stat *)s7_c_pointer(s7_car(args)))->st_mtime));}
                  static s7_pointer g_st_ctime(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, ((struct stat *)s7_c_pointer(s7_car(args)))->st_ctime));}
                  static s7_pointer g_stat_make(s7_scheme *sc, s7_pointer args)
                    {return(s7_make_c_pointer(sc, (void *)calloc(1, sizeof(struct stat))));}
                  ")
	   
	   (C-function ("S_ISDIR" g_isdir "" 1))
	   (C-function ("S_ISCHR" g_ischr "" 1))
	   (C-function ("S_ISBLK" g_isblk "" 1))
	   (C-function ("S_ISREG" g_isreg "" 1))
	   (C-function ("S_ISFIFO" g_isfifo "" 1))
	   (C-function ("S_ISLNK" g_islnk "" 1))
	   (C-function ("S_ISSOCK" g_issock "" 1))
	   
	   (C-function ("stat.st_dev" g_st_dev "" 1))
	   (C-function ("stat.st_ino" g_st_ino "" 1))
	   (C-function ("stat.st_mode" g_st_mode "" 1))
	   (C-function ("stat.st_nlink" g_st_nlink "" 1))
	   (C-function ("stat.st_uid" g_st_uid "" 1))
	   (C-function ("stat.st_gid" g_st_gid "" 1))
	   (C-function ("stat.st_rdev" g_st_rdev "" 1))
	   (C-function ("stat.st_size" g_st_size "" 1))
	   (C-function ("stat.st_blksize" g_st_blksize "" 1))
	   (C-function ("stat.st_blocks" g_st_blocks "" 1))
	   (C-function ("stat.st_atime" g_st_atime "" 1))
	   (C-function ("stat.st_mtime" g_st_mtime "" 1))
	   (C-function ("stat.st_ctime" g_st_ctime "" 1))
	   (C-function ("stat.make" g_stat_make "" 0))
	   
	   
	   ;; -------- time.h sys/time.h --------
	   (C-macro (int (CLOCKS_PER_SEC CLOCK_REALTIME CLOCK_MONOTONIC CLOCK_PROCESS_CPUTIME_ID CLOCK_THREAD_CPUTIME_ID 
			  CLOCK_MONOTONIC_RAW CLOCK_REALTIME_COARSE CLOCK_MONOTONIC_COARSE)))
	   (int clock (void))
	   
	   (int time (time_t*))
	   (double difftime ((time_t integer) (time_t integer)))
	   (tm* gmtime (time_t*))
	   (char* ctime (time_t*))
	   (tm* localtime (time_t*))
	   
	   (in-C "static s7_pointer g_mktime(s7_scheme *sc, s7_pointer args) 
                  {
                    return(s7_make_integer(sc, (s7_int)mktime((struct tm *)s7_c_pointer(s7_car(args)))));
                  }
                  static s7_pointer g_time_make(s7_scheme *sc, s7_pointer args) 
                  {
                    time_t *tm;
                    tm = (time_t *)calloc(1, sizeof(time_t));
                    (*tm) = (time_t)s7_integer(s7_car(args));
                    return(s7_make_c_pointer(sc, (void *)tm));
                  }
                  static s7_pointer g_strftime(s7_scheme *sc, s7_pointer args) 
                  {
                    return(s7_make_integer(sc, (s7_int)strftime((char *)s7_string(s7_car(args)), 
                  				             (size_t)s7_integer(s7_cadr(args)), 
                  					     s7_string(s7_caddr(args)), 
                  					     (const struct tm *)s7_c_pointer(s7_cadddr(args)))));
                  }
                  static s7_pointer g_asctime(s7_scheme *sc, s7_pointer args) 
                  {
                    return(s7_make_string(sc, asctime((const struct tm *)s7_c_pointer(s7_car(args)))));
                  }
                  static s7_pointer g_gettimeofday(s7_scheme *sc, s7_pointer args)
                  {
                    struct timeval t0;
                    gettimeofday(&t0, NULL);
                    return(s7_list(sc, 2, s7_make_integer(sc, t0.tv_sec), s7_make_integer(sc, t0.tv_usec)));
                  }
                  static s7_pointer g_nanosleep(s7_scheme *sc, s7_pointer args)
                  {
                    struct timespec t0;
                    t0.tv_sec = (time_t)s7_integer(s7_car(args));
                    t0.tv_nsec = (long)s7_integer(s7_cadr(args));
                    return(s7_make_integer(sc, nanosleep(&t0, NULL)));
                  }
                  static s7_pointer g_clock_getres(s7_scheme *sc, s7_pointer args)
                  {
                    #if (!__APPLE__)
                    struct timespec t0;
                    int res;
                    res = clock_getres(s7_integer(s7_car(args)), &t0);
                    return(s7_list(sc, 3, s7_make_integer(sc, res), s7_make_integer(sc, t0.tv_sec), s7_make_integer(sc, t0.tv_nsec)));
                    #else
                    return(s7_make_integer(sc, -1));
                    #endif
                  }
                  static s7_pointer g_clock_gettime(s7_scheme *sc, s7_pointer args)
                  {
                    #if (!__APPLE__)
                    struct timespec t0;
                    int res;
                    res = clock_gettime(s7_integer(s7_car(args)), &t0);
                    return(s7_list(sc, 3, s7_make_integer(sc, res), s7_make_integer(sc, t0.tv_sec), s7_make_integer(sc, t0.tv_nsec)));
                    #else
                    return(s7_make_integer(sc, -1));
                    #endif
                  }
                  static s7_pointer g_clock_settime(s7_scheme *sc, s7_pointer args)
                  {
                    #if (!__APPLE__)
                    struct timespec t0;
                    t0.tv_sec = (time_t)s7_integer(s7_cadr(args));
                    t0.tv_nsec = (long)s7_integer(s7_caddr(args));
                    return(s7_make_integer(sc, clock_settime(s7_integer(s7_car(args)), &t0)));
                    #else
                    return(s7_make_integer(sc, -1));
                    #endif
                  }
                  static s7_pointer g_clock_getcpuclockid(s7_scheme *sc, s7_pointer args)
                  {
                    #if __linux__
                    clockid_t c = 0;
                    clock_getcpuclockid((pid_t)s7_integer(s7_car(args)), &c);
                    return(s7_make_integer(sc, (s7_int)c));
                    #else
                    return(s7_make_integer(sc, -1));
                    #endif
                  }
                  static s7_pointer g_clock_nanosleep(s7_scheme *sc, s7_pointer args)
                  {
                    #if __linux__
                    struct timespec t0;
                    t0.tv_sec = (time_t)s7_integer(s7_caddr(args));
                    t0.tv_nsec = (long)s7_integer(s7_cadddr(args));
                    return(s7_make_integer(sc, clock_nanosleep((clockid_t)s7_integer(s7_car(args)), (int)s7_integer(s7_cadr(args)), &t0, NULL)));
                    #else
                    return(s7_make_integer(sc, -1));
                    #endif
                  }
                  ")
	   (C-function ("time.make" g_time_make "" 1))
	   (C-function ("mktime" g_mktime "" 1))
	   (C-function ("asctime" g_asctime "" 1))
	   (C-function ("strftime" g_strftime "" 4))
	   (C-function ("gettimeofday" g_gettimeofday "" 0))
	   (C-function ("nanosleep" g_nanosleep "" 2))
	   (C-function ("clock_getres" g_clock_getres "" 1))
	   (C-function ("clock_gettime" g_clock_gettime "" 1)) ; these need -lrt
	   (C-function ("clock_settime" g_clock_settime "" 3))
	   (reader-cond ((not (provided? 'solaris)) (C-function ("clock_getcpuclockid" g_clock_getcpuclockid "" 1))))
	   (C-function ("clock_nanosleep" g_clock_nanosleep "" 4))
	   
	   
	   ;; -------- utime.h --------
	   (in-C "static s7_pointer g_utime(s7_scheme *sc, s7_pointer args)
                  {
                    struct utimbuf tb;
                    tb.actime = (time_t)s7_integer(s7_cadr(args));
                    tb.modtime = (time_t)s7_integer(s7_caddr(args));
                    return(s7_make_integer(sc, utime(s7_string(s7_car(args)), &tb)));
                  }")
	   (C-function ("utime" g_utime "" 3))
	   
	   
	   ;; -------- termios.h --------
	   (C-macro (int (VINTR VQUIT VERASE VKILL VEOF VTIME VMIN VSWTC VSTART VSTOP VSUSP VEOL VREPRINT 
			  VDISCARD VWERASE VLNEXT VEOL2 IGNBRK BRKINT IGNPAR PARMRK INPCK ISTRIP INLCR 
			  IGNCR ICRNL IUCLC IXON IXANY IXOFF IMAXBEL IUTF8 OPOST OLCUC ONLCR OCRNL ONOCR 
			  ONLRET OFILL OFDEL ISIG ICANON ECHO ECHOE ECHOK ECHONL NOFLSH TOSTOP IEXTEN 
			  TCOOFF TCOON TCIOFF TCION TCIFLUSH TCOFLUSH TCIOFLUSH TCSANOW TCSADRAIN TCSAFLUSH)))
	   
	   (int tcsendbreak (int int))
	   (int tcdrain (int))
	   (int tcflush (int int))
	   (int tcflow (int int))
	   
	   (in-C "static s7_pointer g_cfgetospeed(s7_scheme *sc, s7_pointer args)
                  {
                    struct termios *p;
                    p = (struct termios *)s7_c_pointer(s7_car(args));
                    return(s7_make_integer(sc, (s7_int)cfgetospeed(p)));
                  }
                  static s7_pointer g_cfgetispeed(s7_scheme *sc, s7_pointer args)
                  {
                    struct termios *p;
                    p = (struct termios *)s7_c_pointer(s7_car(args));
                    return(s7_make_integer(sc, (s7_int)cfgetispeed(p)));
                  }
                  static s7_pointer g_cfsetospeed(s7_scheme *sc, s7_pointer args)
                  {
                    struct termios *p;
                    p = (struct termios *)s7_c_pointer(s7_car(args));
                    return(s7_make_integer(sc, cfsetospeed(p, (speed_t)s7_integer(s7_cadr(args)))));
                  }
                  static s7_pointer g_cfsetispeed(s7_scheme *sc, s7_pointer args)
                  {
                    struct termios *p;
                    p = (struct termios *)s7_c_pointer(s7_car(args));
                    return(s7_make_integer(sc, cfsetispeed(p, (speed_t)s7_integer(s7_cadr(args)))));
                  }
                  static s7_pointer g_tcgetattr(s7_scheme *sc, s7_pointer args)
                  {
                    struct termios *p;
                    p = (struct termios *)s7_c_pointer(s7_cadr(args));
                    return(s7_make_integer(sc, tcgetattr(s7_integer(s7_car(args)), p)));
                  }
                  static s7_pointer g_tcsetattr(s7_scheme *sc, s7_pointer args)
                  {
                    struct termios *p;
                    p = (struct termios *)s7_c_pointer(s7_caddr(args));
                    return(s7_make_integer(sc, tcsetattr(s7_integer(s7_car(args)), s7_integer(s7_cadr(args)), p)));
                   }
                  static s7_pointer g_termios_make(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer(sc, (void *)calloc(1, sizeof(struct termios))));}

                  static s7_pointer g_termios_c_lflag(s7_scheme *sc, s7_pointer args)
                  {
                    struct termios *p;
                    p = (struct termios *)s7_c_pointer(s7_car(args));
                    return(s7_make_integer(sc, (s7_int)(p->c_lflag)));
                  }
                  static s7_pointer g_termios_set_c_lflag(s7_scheme *sc, s7_pointer args)
                  {
                    struct termios *p;
                    p = (struct termios *)s7_c_pointer(s7_car(args));
                    p->c_lflag = (tcflag_t)s7_integer(s7_cadr(args));
                    return(s7_cadr(args));
                  }
                  static s7_pointer g_termios_set_c_cc(s7_scheme *sc, s7_pointer args)
                  {
                    struct termios *p;
                    p = (struct termios *)s7_c_pointer(s7_car(args));
                    p->c_cc[(int)s7_integer(s7_cadr(args))] = (cc_t)s7_integer(s7_caddr(args));
                    return(s7_caddr(args));
                  }
                  ")
	   ;; tcflag_t c_iflag, c_oflag, c_cflag; cc_t c_line;
	   ;; cc_t c_cc[NCCS];
	   
	   (C-function ("cfgetospeed" g_cfgetospeed "" 1))
	   (C-function ("cfgetispeed" g_cfgetispeed "" 1))
	   (C-function ("cfsetospeed" g_cfsetospeed "" 2))
	   (C-function ("cfsetispeed" g_cfsetispeed "" 2))
	   (C-function ("tcgetattr" g_tcgetattr "" 2))
	   (C-function ("tcsetattr" g_tcsetattr "" 3))
	   (C-function ("termios.make" g_termios_make "" 0))
	   (C-function ("termios.c_lflag" g_termios_c_lflag "" 1))
	   (C-function ("termios.set_c_lflag" g_termios_set_c_lflag "" 2))
	   (C-function ("termios.set_c_cc" g_termios_set_c_cc "" 3))
	   
	   
	   ;; -------- grp.h --------
	   (void* getgrgid (int))
	   (void* getgrnam (char*))
	   (in-C "static s7_pointer g_group_gr_name(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_string(sc, ((struct group *)s7_c_pointer(s7_car(args)))->gr_name));}
                  static s7_pointer g_group_gr_passwd(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_string(sc, ((struct group *)s7_c_pointer(s7_car(args)))->gr_passwd));}
                  static s7_pointer g_group_gr_gid(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, (s7_int)(((struct group *)s7_c_pointer(s7_car(args)))->gr_gid)));}
                  static s7_pointer g_group_gr_mem(s7_scheme *sc, s7_pointer args)
                    {
                      s7_pointer p;
                      int i;
                      struct group *g;
                      g = (struct group *)s7_c_pointer(s7_car(args));
                      p = s7_nil(sc);
                      for (i = 0; g->gr_mem[i]; i++)
                        p = s7_cons(sc, s7_make_string(sc, g->gr_mem[i]), p);
                      return(p);
                      }
                      ")
	   (C-function ("group.gr_name" g_group_gr_name "" 1))
	   (C-function ("group.gr_passwd" g_group_gr_passwd "" 1))
	   (C-function ("group.gr_gid" g_group_gr_gid "" 1))
	   (C-function ("group.gr_mem" g_group_gr_mem "" 1))
	   ;; ((*libc* 'group.gr_name) ((*libc* 'getgrnam) "wheel")) -> "wheel"
	   
	   
	   ;; -------- pwd.h --------
	   (C-macro (int NSS_BUFLEN_PASSWD))
	   (void setpwent (void))
	   (void endpwent (void))
	   (void* getpwent (void))
	   (void* getpwuid (int))
	   (void* getpwnam (char*))
	   (in-C "static s7_pointer g_passwd_pw_name(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_string(sc, ((struct passwd *)s7_c_pointer(s7_car(args)))->pw_name));}
                  static s7_pointer g_passwd_pw_passwd(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_string(sc, ((struct passwd *)s7_c_pointer(s7_car(args)))->pw_passwd));}
                  static s7_pointer g_passwd_pw_uid(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, ((struct passwd *)s7_c_pointer(s7_car(args)))->pw_uid));}
                  static s7_pointer g_passwd_pw_gid(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, ((struct passwd *)s7_c_pointer(s7_car(args)))->pw_gid));}
                  static s7_pointer g_passwd_pw_gecos(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_string(sc, ((struct passwd *)s7_c_pointer(s7_car(args)))->pw_gecos));}
                  static s7_pointer g_passwd_pw_dir(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_string(sc, ((struct passwd *)s7_c_pointer(s7_car(args)))->pw_dir));}
                  static s7_pointer g_passwd_pw_shell(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_string(sc, ((struct passwd *)s7_c_pointer(s7_car(args)))->pw_shell));}
                  ")
	   (C-function ("passwd.pw_name" g_passwd_pw_name "" 1))
	   (C-function ("passwd.pw_passwd" g_passwd_pw_passwd "" 1))
	   (C-function ("passwd.pw_uid" g_passwd_pw_uid "" 1))
	   (C-function ("passwd.pw_gid" g_passwd_pw_gid "" 1))
	   (C-function ("passwd.pw_gecos" g_passwd_pw_gecos "" 1))
	   (C-function ("passwd.pw_dir" g_passwd_pw_dir "" 1))
	   (C-function ("passwd.pw_shell" g_passwd_pw_shell "" 1))
	   ;; ((*libc* 'passwd.pw_name) ((*libc* 'getpwnam) "bil")) -> "bil"
	   
	   
	   ;; -------- wordexp.h --------
	   (reader-cond ((not (provided? 'openbsd))
			 (int (WRDE_DOOFFS WRDE_APPEND WRDE_NOCMD WRDE_REUSE WRDE_SHOWERR WRDE_UNDEF 
					   WRDE_NOSPACE WRDE_BADCHAR WRDE_BADVAL WRDE_CMDSUB WRDE_SYNTAX))
			 (int wordexp (char* wordexp_t* int))
			 (void wordfree (wordexp_t*))
			 (in-C "static s7_pointer g_wordexp_make(s7_scheme *sc, s7_pointer args)
                           {return(s7_make_c_pointer(sc, (void *)calloc(1, sizeof(wordexp_t))));}
                           static s7_pointer g_wordexp_we_wordc(s7_scheme *sc, s7_pointer args)
                           {return(s7_make_integer(sc, ((wordexp_t *)s7_c_pointer(s7_car(args)))->we_wordc));}
                           static s7_pointer g_wordexp_we_wordv(s7_scheme *sc, s7_pointer args)
                           {
                             s7_pointer p;
                             int i;
                             wordexp_t *g;
                             g = (wordexp_t *)s7_c_pointer(s7_car(args));
                             p = s7_nil(sc);
                             for (i = 0; i < g->we_wordc; i++)
                               p = s7_cons(sc, s7_make_string(sc, g->we_wordv[i]), p);
                             return(p);
                           }")
			 (C-function ("wordexp.make" g_wordexp_make "" 0))
			 (C-function ("wordexp.we_wordc" g_wordexp_we_wordc "" 1))
			 (C-function ("wordexp.we_wordv" g_wordexp_we_wordv "" 1))))
	   ;; (with-let (sublet *libc*) (let ((w (wordexp.make))) (wordexp "~/cl/snd-gdraw" w 0) (wordexp.we_wordv w))) -> ("/home/bil/cl/snd-gdraw")
	   
	   
	   ;; -------- glob.h --------
	   ;; does any of this work in openbsd?
	   (C-macro (int (GLOB_ERR GLOB_MARK GLOB_NOSORT GLOB_DOOFFS GLOB_NOCHECK GLOB_APPEND GLOB_NOESCAPE GLOB_PERIOD 
			  GLOB_MAGCHAR GLOB_ALTDIRFUNC GLOB_BRACE GLOB_NOMAGIC GLOB_TILDE GLOB_ONLYDIR GLOB_TILDE_CHECK 
			  GLOB_NOSPACE GLOB_ABORTED GLOB_NOMATCH GLOB_NOSYS)))
	   (void globfree (glob_t*))
	   (in-C "static s7_pointer g_glob_make(s7_scheme *sc, s7_pointer args)
                           {return(s7_make_c_pointer(sc, (void *)calloc(1, sizeof(glob_t))));}
                           static s7_pointer g_glob_gl_pathc(s7_scheme *sc, s7_pointer args)
                           {return(s7_make_integer(sc, ((glob_t *)s7_c_pointer(s7_car(args)))->gl_pathc));}
                           static s7_pointer g_glob(s7_scheme *sc, s7_pointer args)
                           {return(s7_make_integer(sc, glob(s7_string(s7_car(args)), s7_integer(s7_cadr(args)), NULL, (glob_t *)s7_c_pointer(s7_caddr(args)))));}
                           static s7_pointer g_glob_gl_pathv(s7_scheme *sc, s7_pointer args)
                           {
                             s7_pointer p;
                             int i;
                             glob_t *g;
                             g = (glob_t *)s7_c_pointer(s7_car(args));
                             p = s7_nil(sc);
                             for (i = 0; i < g->gl_pathc; i++)
                               p = s7_cons(sc, s7_make_string(sc, g->gl_pathv[i]), p);
                             return(p);
                           }")
	   (C-function ("glob.make" g_glob_make "" 0))
	   (C-function ("glob.gl_pathc" g_glob_gl_pathc "" 1))
	   (C-function ("glob.gl_pathv" g_glob_gl_pathv "" 1))
	   (C-function ("glob" g_glob "" 3))
	   
	   
	   ;; -------- signal.h sys/wait.h --------
	   (C-macro (int (SIGHUP SIGINT SIGQUIT SIGILL SIGTRAP SIGABRT SIGIOT SIGBUS SIGFPE 
			  SIGKILL SIGUSR1 SIGSEGV SIGUSR2 SIGPIPE SIGALRM SIGTERM SIGSTKFLT 
			  SIGCLD SIGCHLD SIGCONT SIGSTOP SIGTSTP SIGTTIN SIGTTOU SIGURG 
			  SIGXCPU SIGXFSZ SIGVTALRM SIGPROF SIGWINCH SIGPOLL SIGIO SIGPWR SIGSYS 
			  (reader-cond ((not (provided? 'osx)) SIGUNUSED))
			  WNOHANG WUNTRACED WSTOPPED WEXITED WCONTINUED WNOWAIT
			  RLIMIT_CPU RLIMIT_FSIZE RLIMIT_DATA RLIMIT_STACK RLIMIT_CORE RLIMIT_RSS 
			  RLIMIT_NOFILE RLIMIT_OFILE RLIMIT_AS RLIMIT_NPROC RLIMIT_MEMLOCK RLIMIT_LOCKS 
			  RLIMIT_SIGPENDING RLIMIT_MSGQUEUE RLIMIT_NICE RLIMIT_RTPRIO RLIMIT_NLIMITS 
			  RLIM_NLIMITS RLIM_INFINITY RLIM_SAVED_MAX RLIM_SAVED_CUR RUSAGE_SELF 
			  RUSAGE_CHILDREN RUSAGE_THREAD RUSAGE_LWP 
			  PRIO_MIN PRIO_MAX PRIO_PROCESS PRIO_PGRP PRIO_USER
			  SA_NOCLDSTOP SA_NOCLDWAIT SA_SIGINFO SA_ONSTACK SA_RESTART SA_NODEFER SA_RESETHAND SA_NOMASK SA_ONESHOT SA_STACK 
			  SIG_BLOCK SIG_UNBLOCK SIG_SETMASK
			  )))
	   
	   ;; (let ((v (rusage.make))) (getrusage (*libc* 'RUSAGE_SELF) v)  (let ((mem (rusage.ru_maxrss v))) (free v) mem))
	   
	   (int kill (int int))
	   (int raise (int))
	   (int sigemptyset (sigset_t*))
	   (int sigfillset (sigset_t*))
	   (int sigaddset (sigset_t* int))
	   (int sigdelset (sigset_t* int))
	   (int sigismember (sigset_t* int))
	   (int sigprocmask (int sigset_t* sigset_t*))
	   (int sigsuspend (sigset_t*))
	   (int sigpending (sigset_t*))
	   (int getpriority (int int))
	   (int setpriority (int int int)) 
	   
	   (in-C "static s7_pointer g_rlimit_make(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer(sc, (void *)calloc(1, sizeof(struct rlimit))));}
                  static s7_pointer g_rlimit_rlim_cur(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct rlimit *)s7_c_pointer(s7_car(args)))->rlim_cur));}
                  static s7_pointer g_rlimit_rlim_max(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct rlimit *)s7_c_pointer(s7_car(args)))->rlim_max));}

                  static s7_pointer g_rusage_make(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer(sc, (void *)calloc(1, sizeof(struct rusage))));}
                  static s7_pointer g_rusage_ru_maxrss(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct rusage *)s7_c_pointer(s7_car(args)))->ru_maxrss));}
                  static s7_pointer g_rusage_ru_minflt(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct rusage *)s7_c_pointer(s7_car(args)))->ru_minflt));}
                  static s7_pointer g_rusage_ru_majflt(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct rusage *)s7_c_pointer(s7_car(args)))->ru_majflt));}
                  static s7_pointer g_rusage_ru_inblock(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct rusage *)s7_c_pointer(s7_car(args)))->ru_inblock));}
                  static s7_pointer g_rusage_ru_oublock(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct rusage *)s7_c_pointer(s7_car(args)))->ru_oublock));}
                  static s7_pointer g_rusage_ru_nvcsw(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct rusage *)s7_c_pointer(s7_car(args)))->ru_nvcsw));}
                  static s7_pointer g_rusage_ru_nivcsw(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct rusage *)s7_c_pointer(s7_car(args)))->ru_nivcsw));}
                  static s7_pointer g_rusage_ru_utime(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer(sc, &(((struct rusage *)s7_c_pointer(s7_car(args)))->ru_utime)));}
                  static s7_pointer g_rusage_ru_stime(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer(sc, &(((struct rusage *)s7_c_pointer(s7_car(args)))->ru_stime)));}

                  static s7_pointer g_sigset_make(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer(sc, (void *)calloc(1, sizeof(sigset_t))));}

                  #if __linux__
                  static s7_pointer g_WEXITSTATUS(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, WEXITSTATUS(s7_integer(s7_car(args)))));}
                  static s7_pointer g_WTERMSIG(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, WTERMSIG(s7_integer(s7_car(args)))));}
                  static s7_pointer g_WSTOPSIG(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, WSTOPSIG(s7_integer(s7_car(args)))));}
                  static s7_pointer g_WIFEXITED(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, WIFEXITED(s7_integer(s7_car(args)))));}
                  static s7_pointer g_WIFSIGNALED(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, WIFSIGNALED(s7_integer(s7_car(args)))));}
                  static s7_pointer g_WIFSTOPPED(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, WIFSTOPPED(s7_integer(s7_car(args)))));}
                  #endif

                  static s7_pointer g_wait(s7_scheme *sc, s7_pointer args)
                  {
                    int status, result;
                    result = wait(&status);
                    return(s7_list(sc, 2, s7_make_integer(sc, result), s7_make_integer(sc, status)));
                  }
                  static s7_pointer g_waitpid(s7_scheme *sc, s7_pointer args)
                  {
                    int status, result;
                    result = waitpid((pid_t)s7_integer(s7_car(args)), &status, s7_integer(s7_cadr(args)));
                    return(s7_list(sc, 2, s7_make_integer(sc, result), s7_make_integer(sc, status)));
                  }
                  static s7_pointer g_sigqueue(s7_scheme *sc, s7_pointer args)
                  {
                    #if (__linux__)
                      union sigval val;
                      if (s7_is_integer(s7_caddr(args)))
                        val.sival_int = (int)s7_integer(s7_caddr(args));
                      else val.sival_ptr = (void *)s7_c_pointer(s7_caddr(args));
                      return(s7_make_integer(sc, sigqueue((pid_t)s7_integer(s7_car(args)), s7_integer(s7_cadr(args)), val)));
                    #else
                      return(s7_f(sc));
                    #endif
                  }
                  static s7_pointer g_sigwait(s7_scheme *sc, s7_pointer args)
                  {
                    #if (!__sun)
                    int status, result;
                    result = sigwait((const sigset_t *)s7_c_pointer(s7_car(args)), &status);
                    return(s7_list(sc, 2, s7_make_integer(sc, result), s7_make_integer(sc, status)));
                    #else
                    return(s7_f(sc));
                    #endif
                  }
                  static s7_pointer g_sigtimedwait(s7_scheme *sc, s7_pointer args)
                  {
                    #if (__linux__)
                     return(s7_make_integer(sc, sigtimedwait((const sigset_t *)s7_c_pointer(s7_car(args)), 
                  					   (siginfo_t *)s7_c_pointer(s7_cadr(args)),
                                                             (const struct timespec *)s7_c_pointer(s7_caddr(args)))));
                    #else
                      return(s7_f(sc));
                    #endif
                  }
                  #if __linux__
                  static s7_pointer g_siginfo_make(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer(sc, (void *)calloc(1, sizeof(siginfo_t))));}
                  static s7_pointer g_siginfo_si_signo(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((siginfo_t *)s7_c_pointer(s7_car(args)))->si_signo));}
                  static s7_pointer g_siginfo_si_errno(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((siginfo_t *)s7_c_pointer(s7_car(args)))->si_errno));}
                  static s7_pointer g_siginfo_si_code(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((siginfo_t *)s7_c_pointer(s7_car(args)))->si_code));}
                  static s7_pointer g_siginfo_si_pid(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((siginfo_t *)s7_c_pointer(s7_car(args)))->si_pid));}
                  static s7_pointer g_siginfo_si_uid(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((siginfo_t *)s7_c_pointer(s7_car(args)))->si_uid));}
                  static s7_pointer g_siginfo_si_status(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((siginfo_t *)s7_c_pointer(s7_car(args)))->si_status));}
                  static s7_pointer g_siginfo_si_utime(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((siginfo_t *)s7_c_pointer(s7_car(args)))->si_utime));}
                  static s7_pointer g_siginfo_si_stime(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((siginfo_t *)s7_c_pointer(s7_car(args)))->si_stime));}
                  static s7_pointer g_siginfo_si_value(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer(sc, &(((siginfo_t *)s7_c_pointer(s7_car(args)))->si_value)));}
                  static s7_pointer g_siginfo_si_int(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((siginfo_t *)s7_c_pointer(s7_car(args)))->si_int));}
                  static s7_pointer g_siginfo_si_overrun(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((siginfo_t *)s7_c_pointer(s7_car(args)))->si_overrun));}
                  static s7_pointer g_siginfo_si_timerid(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((siginfo_t *)s7_c_pointer(s7_car(args)))->si_timerid));}
                  static s7_pointer g_siginfo_si_band(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((siginfo_t *)s7_c_pointer(s7_car(args)))->si_band));}
                  static s7_pointer g_siginfo_si_fd(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((siginfo_t *)s7_c_pointer(s7_car(args)))->si_fd));}
                  static s7_pointer g_siginfo_si_ptr(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer(sc, ((siginfo_t *)s7_c_pointer(s7_car(args)))->si_ptr));}
                  static s7_pointer g_siginfo_si_addr(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer(sc, ((siginfo_t *)s7_c_pointer(s7_car(args)))->si_addr));}
                  #endif

                  static s7_pointer g_timespec_make(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer(sc, (void *)calloc(1, sizeof(struct timespec))));}
                  static s7_pointer g_timespec_tv_sec(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct timespec *)s7_c_pointer(s7_car(args)))->tv_sec));}
                  static s7_pointer g_timespec_tv_nsec(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct timespec *)s7_c_pointer(s7_car(args)))->tv_nsec));}

                  static s7_pointer g_sigaction_make(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer(sc, (void *)calloc(1, sizeof(sigaction))));}
                  static s7_pointer g_sigaction_sa_flags(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct sigaction *)s7_c_pointer(s7_car(args)))->sa_flags));}
                  static s7_pointer g_sigaction_set_sa_flags(s7_scheme *sc, s7_pointer args)
                  {((struct sigaction *)s7_c_pointer(s7_car(args)))->sa_flags = s7_integer(s7_cadr(args)); return(s7_cadr(args));}
                  static s7_pointer g_sigaction_sa_mask(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer(sc, (void *)(&(((struct sigaction *)s7_c_pointer(s7_car(args)))->sa_mask))));}
                  static s7_pointer g_sigaction_sa_handler(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer(sc, (void *)(((struct sigaction *)s7_c_pointer(s7_car(args)))->sa_handler)));}

                  static s7_pointer sighandlers = NULL;
                  static s7_scheme *sighandlers_s7 = NULL;
                  static void s7_signal_handler(int sig)
                  {
                    if (sighandlers)
                      {
                        s7_pointer handler;
                        handler = s7_vector_ref(sighandlers_s7, sighandlers, sig);
                        if (handler != s7_f(sighandlers_s7))
                           s7_call(sighandlers_s7, handler, s7_cons(sighandlers_s7, s7_make_integer(sighandlers_s7, sig), s7_nil(sighandlers_s7)));
                       }
                  }
                  #ifndef SIGUNUSED
                    #define SIGUNUSED 65
                  #endif
                  static s7_pointer g_sigaction_set_sa_handler(s7_scheme *sc, s7_pointer args)
                  {
                    /* (sigaction.set_sa_handler ptr handler) */
                    if (!sighandlers)
                      {
                        sighandlers_s7 = sc;
                        sighandlers = s7_make_and_fill_vector(sc, SIGUNUSED + 1, s7_f(sc));
                        s7_gc_protect(sc, sighandlers);
                      }
                    if (s7_c_pointer(s7_cadr(args)) == (void *)SIG_DFL)
                       ((struct sigaction *)s7_c_pointer(s7_car(args)))->sa_handler = SIG_DFL;
                    else
                      {
                        if (s7_c_pointer(s7_cadr(args)) == (void *)SIG_IGN)
                           ((struct sigaction *)s7_c_pointer(s7_car(args)))->sa_handler = SIG_IGN;
                        else 
                          {
                            ((struct sigaction *)s7_c_pointer(s7_car(args)))->sa_handler = s7_signal_handler;
                            s7_vector_set(sighandlers_s7, sighandlers, SIGUNUSED, 
                              s7_cons(sc, s7_cons(sc, s7_car(args), s7_cadr(args)), s7_vector_ref(sighandlers_s7, sighandlers, SIGUNUSED)));
                          }
                      }
                    return(s7_cadr(args));
                  }
                static s7_pointer g_sigaction(s7_scheme *sc, s7_pointer args)
                {
                  int sig;
                  const struct sigaction *new_act;
                  struct sigaction *old_act;
                  s7_pointer handler;
                  sig = (int)s7_integer(s7_car(args));
                  new_act = (const struct sigaction *)s7_c_pointer(s7_cadr(args));
                  old_act = (struct sigaction *)s7_c_pointer(s7_caddr(args));
                  handler = s7_assq(sc, s7_cadr(args), s7_vector_ref(sighandlers_s7, sighandlers, SIGUNUSED));
                  if (s7_is_pair(handler))
                    s7_vector_set(sighandlers_s7, sighandlers, sig, s7_cdr(handler));
                  return(s7_make_integer(sc, sigaction(sig, new_act, old_act)));
                }
                static s7_pointer g_signal(s7_scheme *sc, s7_pointer args)
                {
                  int sig;
                  if (!sighandlers)
                    {
                      sighandlers_s7 = sc;
                      sighandlers = s7_make_and_fill_vector(sc, SIGUNUSED + 1, s7_f(sc));
                      s7_gc_protect(sc, sighandlers);
                    }
                  sig = s7_integer(s7_car(args));
                  if (s7_is_c_pointer(s7_cadr(args)))
                    {
                      if (s7_c_pointer(s7_cadr(args)) == (void *)SIG_DFL)
                         return(s7_make_c_pointer(sc, signal(sig, SIG_DFL)));
                      if (s7_c_pointer(s7_cadr(args)) == (void *)SIG_IGN)
                         return(s7_make_c_pointer(sc, signal(sig, SIG_IGN)));
                     }
                  s7_vector_set(sc, sighandlers, sig, s7_cadr(args));
                  return(s7_make_c_pointer(sc, signal(sig, s7_signal_handler)));
                }
                  ")
	   
	   (C-function ("rlimit.make" g_rlimit_make "" 0))
	   (C-function ("rlimit.rlim_cur" g_rlimit_rlim_cur "" 1))
	   (C-function ("rlimit.rlim_max" g_rlimit_rlim_max "" 1))
	   
	   (C-function ("rusage.make" g_rusage_make "" 0))
	   (C-function ("rusage.ru_maxrss" g_rusage_ru_maxrss "" 1))
	   (C-function ("rusage.ru_minflt" g_rusage_ru_minflt "" 1))
	   (C-function ("rusage.ru_majflt" g_rusage_ru_majflt "" 1))
	   (C-function ("rusage.ru_inblock" g_rusage_ru_inblock "" 1))
	   (C-function ("rusage.ru_oublock" g_rusage_ru_oublock "" 1))
	   (C-function ("rusage.ru_nvcsw" g_rusage_ru_nvcsw "" 1))
	   (C-function ("rusage.ru_nivcsw" g_rusage_ru_nivcsw "" 1))
	   (C-function ("rusage.ru_utime" g_rusage_ru_utime "" 1))
	   (C-function ("rusage.ru_stime" g_rusage_ru_stime "" 1))
	   
	   (reader-cond ((provided? 'linux) 
			 (C-function ("siginfo.make" g_siginfo_make "" 0))
			 (C-function ("siginfo.si_signo" g_siginfo_si_signo "" 1))
			 (C-function ("siginfo.si_errno" g_siginfo_si_errno "" 1))
			 (C-function ("siginfo.si_code" g_siginfo_si_code "" 1))
			 (C-function ("siginfo.si_pid" g_siginfo_si_pid "" 1))
			 (C-function ("siginfo.si_uid" g_siginfo_si_uid "" 1))
			 (C-function ("siginfo.si_status" g_siginfo_si_status "" 1))
			 (C-function ("siginfo.si_utime" g_siginfo_si_utime "" 1))
			 (C-function ("siginfo.si_stime" g_siginfo_si_stime "" 1))
			 (C-function ("siginfo.si_value" g_siginfo_si_value "" 1))
			 (C-function ("siginfo.si_int" g_siginfo_si_int "" 1))
			 (C-function ("siginfo.si_overrun" g_siginfo_si_overrun "" 1))
			 (C-function ("siginfo.si_timerid" g_siginfo_si_timerid "" 1))
			 (C-function ("siginfo.si_band" g_siginfo_si_band "" 1))
			 (C-function ("siginfo.si_fd" g_siginfo_si_fd "" 1))
			 (C-function ("siginfo.si_ptr" g_siginfo_si_ptr "" 1))
			 (C-function ("siginfo.si_addr" g_siginfo_si_addr "" 1))))
	   
	   (C-function ("timespec.make" g_timespec_make "" 0))
	   (C-function ("timespec.tv_sec" g_timespec_tv_sec "" 1))
	   (C-function ("timespec.tv_nsec" g_timespec_tv_nsec "" 1))
	   
	   (C-function ("sigaction.make" g_sigaction_make "" 0))
	   (C-function ("sigaction.sa_handler" g_sigaction_sa_handler "" 1))
	   (C-function ("sigaction.sa_flags" g_sigaction_sa_flags "" 1))
	   (C-function ("sigaction.sa_mask" g_sigaction_sa_mask "" 1))
	   (C-function ("sigaction.set_sa_handler" g_sigaction_set_sa_handler "" 2))
	   (C-function ("sigaction.set_sa_flags" g_sigaction_set_sa_flags "" 2))
	   
	   ;; (define sa ((*libc* 'sigaction.make)))
	   ;; ((*libc* 'sigemptyset) ((*libc* 'sigaction.sa_mask) sa))
	   ;; ((*libc* 'sigaction.set_sa_flags) sa 0)
	   ;; ((*libc* 'sigaction.set_sa_handler) sa (lambda (i) (format *stderr* "i: ~A~%" i)))
	   ;; ((*libc* 'sigaction) (*libc* 'SIGINT) sa (*libc* 'NULL))
	   ;; now type C-C to snd and it prints "i: 2"!!
	   
	   (reader-cond ((provided? 'linux) 
			 (C-function ("WEXITSTATUS" g_WEXITSTATUS "" 1))
			 (C-function ("WTERMSIG" g_WTERMSIG "" 1))
			 (C-function ("WSTOPSIG" g_WSTOPSIG "" 1))
			 (C-function ("WIFEXITED" g_WIFEXITED "" 1))
			 (C-function ("WIFSIGNALED" g_WIFSIGNALED "" 1))
			 (C-function ("WIFSTOPPED" g_WIFSTOPPED "" 1))))
	   
	   (C-function ("wait" g_wait "" 0))
	   (C-function ("waitpid" g_waitpid "" 2))
	   (C-function ("sigqueue" g_sigqueue "" 3))
	   (reader-cond ((not (provided? 'solaris)) (C-function ("sigwait" g_sigwait "" 1))))
	   (C-function ("sigaction" g_sigaction "" 3))
	   (C-function ("sigtimedwait" g_sigtimedwait "" 3))
	   (C-function ("sigset.make" g_sigset_make "" 0))
	   
	   (C-function ("signal" g_signal "" 2))
	   
	   (int getrlimit (int void*))
	   (int setrlimit (int void*))
	   (int getrusage (int void*))
	   (reader-cond ((provided? 'linux) 
			 (int sigwaitinfo (sigset_t* siginfo_t*))
			 (int waitid (int int siginfo_t* int))))
	   (c-pointer (SIG_ERR SIG_DFL SIG_IGN))
	   
	   
	   ;; -------- netdb.h --------
	   (reader-cond ((provided? 'linux)
			 (int (IPPORT_ECHO IPPORT_DISCARD IPPORT_SYSTAT IPPORT_DAYTIME IPPORT_NETSTAT IPPORT_FTP IPPORT_TELNET IPPORT_SMTP
			       IPPORT_TIMESERVER IPPORT_NAMESERVER IPPORT_WHOIS IPPORT_MTP IPPORT_TFTP IPPORT_RJE IPPORT_FINGER IPPORT_TTYLINK
			       IPPORT_SUPDUP IPPORT_EXECSERVER IPPORT_LOGINSERVER IPPORT_CMDSERVER IPPORT_EFSSERVER IPPORT_BIFFUDP
			       IPPORT_WHOSERVER IPPORT_ROUTESERVER IPPORT_RESERVED IPPORT_USERRESERVED))))
	   
	   (C-macro (int (AI_PASSIVE AI_CANONNAME AI_NUMERICHOST AI_V4MAPPED AI_ALL AI_ADDRCONFIG AI_NUMERICSERV
			  EAI_BADFLAGS EAI_NONAME EAI_AGAIN EAI_FAIL EAI_FAMILY EAI_SOCKTYPE EAI_SERVICE EAI_MEMORY EAI_SYSTEM EAI_OVERFLOW
			  NI_NUMERICHOST NI_NUMERICSERV NI_NOFQDN NI_NAMEREQD NI_DGRAM
			  SOCK_STREAM SOCK_DGRAM SOCK_RAW SOCK_RDM SOCK_SEQPACKET SOCK_DCCP SOCK_PACKET SOCK_CLOEXEC SOCK_NONBLOCK
			  _PATH_HEQUIV_PATH_HOSTS _PATH_NETWORKS _PATH_NSSWITCH_CONF _PATH_PROTOCOLS _PATH_SERVICES
			  PF_UNSPEC PF_LOCAL PF_UNIX PF_FILE PF_INET PF_AX25 PF_IPX PF_APPLETALK PF_NETROM PF_BRIDGE
			  PF_ATMPVC PF_X25 PF_INET6 PF_ROSE PF_DECnet PF_NETBEUI PF_SECURITY PF_KEY PF_NETLINK PF_ROUTE
			  PF_PACKET PF_ASH PF_ECONET PF_ATMSVC PF_RDS PF_SNA PF_IRDA PF_PPPOX PF_WANPIPE PF_LLC PF_CAN
			  PF_TIPC PF_BLUETOOTH PF_IUCV PF_RXRPC PF_ISDN PF_PHONET PF_IEEE802154 PF_MAX
			  AF_UNSPEC AF_LOCAL AF_UNIX AF_FILE AF_INET AF_AX25 AF_IPX AF_APPLETALK AF_NETROM AF_BRIDGE
			  AF_ATMPVC AF_X25 AF_INET6 AF_ROSE AF_DECnet AF_NETBEUI AF_SECURITY AF_KEY AF_NETLINK AF_ROUTE
			  AF_PACKET AF_ASH AF_ECONET AF_ATMSVC AF_RDS AF_SNA AF_IRDA AF_PPPOX AF_WANPIPE AF_LLC
			  AF_CAN AF_TIPC AF_BLUETOOTH AF_IUCV AF_RXRPC AF_ISDN AF_PHONET AF_IEEE802154 AF_MAX
			  MSG_OOB MSG_PEEK MSG_DONTROUTE MSG_CTRUNC MSG_PROXY MSG_TRUNC MSG_DONTWAIT MSG_EOR MSG_WAITFORONE
			  MSG_WAITALL MSG_FIN MSG_SYN MSG_CONFIRM MSG_RST MSG_ERRQUEUE MSG_NOSIGNAL MSG_MORE MSG_CMSG_CLOEXEC
			  IPPROTO_IP IPPROTO_HOPOPTS IPPROTO_ICMP IPPROTO_IGMP IPPROTO_IPIP IPPROTO_TCP IPPROTO_EGP IPPROTO_PUP
			  IPPROTO_UDP IPPROTO_IDP IPPROTO_TP IPPROTO_DCCP IPPROTO_IPV6 IPPROTO_ROUTING IPPROTO_FRAGMENT
			  IPPROTO_RSVP IPPROTO_GRE IPPROTO_ESP IPPROTO_AH IPPROTO_ICMPV6 IPPROTO_NONE IPPROTO_DSTOPTS
			  IPPROTO_MTP IPPROTO_ENCAP IPPROTO_PIM IPPROTO_COMP IPPROTO_SCTP IPPROTO_UDPLITE IPPROTO_RAW
			  SOL_RAW SOL_DECNET SOL_X25 SOL_PACKET SOL_ATM SOL_AAL SOL_IRDA
			  SHUT_RD SHUT_WR SHUT_RDWR)))
	   
	   (void sethostent (int))
	   (void endhostent (void))
	   (void* gethostent (void))
	   
	   (void setservent (int))
	   (void endservent (void))
	   (void* getservent (void))
	   
	   (void setprotoent (int))
	   (void endprotoent (void))
	   (void* getprotoent (void))
	   
	   (void setnetent (int))
	   (void endnetent (void))
	   (void* getnetent (void))
	   
	   (int socket (int int int))
	   (int listen (int int))
	   (int shutdown (int int))
	   
	   (void* gethostbyname (char*))
	   (void* gethostbyaddr (void* int int))
	   (void* getnetbyname (char*))
	   (void* getnetbyaddr (int int))
	   (void* getservbyname (char* char*))
	   (void* getservbyport (int char*))
	   (void* getprotobyname (char*))
	   (void* getprotobynumber (int))
	   
	   (void freeaddrinfo (void*))
	   (char* gai_strerror (int))
	   
	   (int bind (int void* int))
	   (int connect (int void* int))
	   (int send (int void* int int))
	   (int recv (int void* int int))
	   (int sendto (int void* int int void* int))
	   (int sendmsg (int void* int))
	   (int recvmsg (int void* int))
	   
	   (in-C "static s7_pointer g_ntohl(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, ntohl(s7_integer(s7_car(args)))));}
                  static s7_pointer g_ntohs(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, ntohs(s7_integer(s7_car(args)))));}
                  static s7_pointer g_htonl(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, htonl(s7_integer(s7_car(args)))));}
                  static s7_pointer g_htons(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, htons(s7_integer(s7_car(args)))));}

                  static s7_pointer g_addrinfo_make(s7_scheme *sc, s7_pointer args) 
                  {
                    return(s7_make_c_pointer(sc, (void *)calloc(1, sizeof(struct addrinfo))));
                  }

                  static s7_pointer g_addrinfo_ai_flags(s7_scheme *sc, s7_pointer args) 
                  {
                    return(s7_make_integer(sc, ((struct addrinfo *)s7_c_pointer(s7_car(args)))->ai_flags));
                  }
                  static s7_pointer g_addrinfo_set_ai_flags(s7_scheme *sc, s7_pointer args) 
                  {
                    ((struct addrinfo *)s7_c_pointer(s7_car(args)))->ai_flags = (int)s7_integer(s7_cadr(args));
                    return(s7_cadr(args));
                  }

                  static s7_pointer g_addrinfo_ai_family(s7_scheme *sc, s7_pointer args) 
                  {
                    return(s7_make_integer(sc, ((struct addrinfo *)s7_c_pointer(s7_car(args)))->ai_family));
                  }
                  static s7_pointer g_addrinfo_set_ai_family(s7_scheme *sc, s7_pointer args) 
                  {
                    ((struct addrinfo *)s7_c_pointer(s7_car(args)))->ai_family = (int)s7_integer(s7_cadr(args));
                    return(s7_cadr(args));
                  }

                  static s7_pointer g_addrinfo_ai_socktype(s7_scheme *sc, s7_pointer args) 
                  {
                    return(s7_make_integer(sc, ((struct addrinfo *)s7_c_pointer(s7_car(args)))->ai_socktype));
                  }
                  static s7_pointer g_addrinfo_set_ai_socktype(s7_scheme *sc, s7_pointer args) 
                  {
                    ((struct addrinfo *)s7_c_pointer(s7_car(args)))->ai_socktype = (int)s7_integer(s7_cadr(args));
                    return(s7_cadr(args));
                  }

                  static s7_pointer g_addrinfo_ai_protocol(s7_scheme *sc, s7_pointer args) 
                  {
                    return(s7_make_integer(sc, ((struct addrinfo *)s7_c_pointer(s7_car(args)))->ai_protocol));
                  }
                  static s7_pointer g_addrinfo_set_ai_protocol(s7_scheme *sc, s7_pointer args) 
                  {
                    ((struct addrinfo *)s7_c_pointer(s7_car(args)))->ai_protocol = (int)s7_integer(s7_cadr(args));
                    return(s7_cadr(args));
                  }
                  static s7_pointer g_addrinfo_ai_canonname(s7_scheme *sc, s7_pointer args) 
                  {
                    return(s7_make_string(sc, ((struct addrinfo *)s7_c_pointer(s7_car(args)))->ai_canonname));
                  }
                  static s7_pointer g_addrinfo_ai_next(s7_scheme *sc, s7_pointer args) 
                  {
                    return(s7_make_c_pointer(sc, (void *)(((struct addrinfo *)s7_c_pointer(s7_car(args)))->ai_next)));
                  }

                  static s7_pointer g_getaddrinfo(s7_scheme *sc, s7_pointer args) 
                  {
                     struct addrinfo *result;
                     int err;
                     err = getaddrinfo(s7_string(s7_car(args)), 
                  		     s7_string(s7_cadr(args)),
                  		     (const struct addrinfo *)s7_c_pointer(s7_caddr(args)),
                                       &result);
                    return(s7_list(sc, 2, s7_make_integer(sc, err), s7_make_c_pointer(sc, (void *)result)));
                  }

                  static s7_pointer g_getnameinfo(s7_scheme *sc, s7_pointer args) 
                  {
                    #ifndef NI_MAXHOST
                      #define NI_MAXHOST 1025
                    #endif
                    #ifndef NI_MAXSERV
                      #define NI_MAXSERV 32
                    #endif
                    char *host, *service;
                    int err;
                    host = (char *)calloc(NI_MAXHOST, sizeof(char));
                    service = (char *)calloc(NI_MAXSERV, sizeof(char));
                    err = getnameinfo((const struct sockaddr *)s7_c_pointer(s7_car(args)), s7_integer(s7_cadr(args)),
                  		    host, NI_MAXHOST,
                  		    service, NI_MAXSERV,
                  		    s7_integer(s7_caddr(args)));
                    return(s7_list(sc, 3, s7_make_integer(sc, err), s7_make_string(sc, host), s7_make_string(sc, service)));
                  }
                           
                  static s7_pointer g_socketpair(s7_scheme *sc, s7_pointer args) 
                  {
                    int fds[2];
                    int err;
                    err = socketpair(s7_integer(s7_car(args)), s7_integer(s7_cadr(args)), s7_integer(s7_caddr(args)), fds);
                    return(s7_list(sc, 3, s7_make_integer(sc, err), s7_make_integer(sc, fds[0]), s7_make_integer(sc, fds[1])));
                  }
                           
                  static s7_pointer g_getsockname(s7_scheme *sc, s7_pointer args) 
                  {
                    int err;
                    socklen_t res;
                    res = s7_integer(s7_caddr(args));
                    err = getsockname(s7_integer(s7_car(args)), (struct sockaddr *)s7_c_pointer(s7_cadr(args)), &res);
                    return(s7_list(sc, 2, s7_make_integer(sc, err), s7_make_integer(sc, res)));
                  }
                  static s7_pointer g_getpeername(s7_scheme *sc, s7_pointer args) 
                  {
                    int err;
                    socklen_t res;
                    res = s7_integer(s7_caddr(args));
                    err = getpeername(s7_integer(s7_car(args)), (struct sockaddr *)s7_c_pointer(s7_cadr(args)), &res);
                    return(s7_list(sc, 2, s7_make_integer(sc, err), s7_make_integer(sc, res)));
                  }
                  static s7_pointer g_accept(s7_scheme *sc, s7_pointer args) 
                  {
                    int err;
                    socklen_t res;
                    res = s7_integer(s7_caddr(args));
                    err = accept(s7_integer(s7_car(args)), (struct sockaddr *)s7_c_pointer(s7_cadr(args)), &res);
                    return(s7_list(sc, 2, s7_make_integer(sc, err), s7_make_integer(sc, res)));
                  }
                  static s7_pointer g_getsockopt(s7_scheme *sc, s7_pointer args) 
                  {
                    int err;
                    socklen_t res;
                    res = (socklen_t)s7_integer(s7_list_ref(sc, args, 4));
                    err = getsockopt(s7_integer(s7_car(args)), s7_integer(s7_cadr(args)), s7_integer(s7_caddr(args)), s7_c_pointer(s7_cadddr(args)), &res);
                    return(s7_list(sc, 2, s7_make_integer(sc, err), s7_make_integer(sc, (s7_int)res)));
                  }
                  static s7_pointer g_setsockopt(s7_scheme *sc, s7_pointer args) 
                  {
                    socklen_t res;
                    res = (socklen_t)s7_integer(s7_list_ref(sc, args, 4));
                    return(s7_make_integer(sc, setsockopt(s7_integer(s7_car(args)), s7_integer(s7_cadr(args)), 
                           s7_integer(s7_caddr(args)), s7_c_pointer(s7_cadddr(args)), res)));
                  }
                  static s7_pointer g_recvfrom(s7_scheme *sc, s7_pointer args) 
                  {
                    int err;
                    socklen_t res;
                    res = (socklen_t)s7_integer(s7_list_ref(sc, args, 5));
                    err = recvfrom(s7_integer(s7_car(args)), 
                  		 s7_c_pointer(s7_cadr(args)), 
                  		 s7_integer(s7_caddr(args)), 
                  		 s7_integer(s7_cadddr(args)), 
                  		 (struct sockaddr *)s7_c_pointer(s7_list_ref(sc, args, 4)),
                  		 &res);
                    return(s7_list(sc, 2, s7_make_integer(sc, err), s7_make_integer(sc, (s7_int)res)));
                  }

                  static s7_pointer g_hostent_h_name(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_string(sc, ((struct hostent *)s7_c_pointer(s7_car(args)))->h_name));}
                  static s7_pointer g_netent_n_name(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_string(sc, ((struct netent *)s7_c_pointer(s7_car(args)))->n_name));}
                  static s7_pointer g_servent_s_name(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_string(sc, ((struct servent *)s7_c_pointer(s7_car(args)))->s_name));}
                  static s7_pointer g_servent_s_proto(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_string(sc, ((struct servent *)s7_c_pointer(s7_car(args)))->s_proto));}
                  static s7_pointer g_protoent_p_name(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_string(sc, ((struct protoent *)s7_c_pointer(s7_car(args)))->p_name));}

                  static s7_pointer g_hostent_h_addrtype(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct hostent *)s7_c_pointer(s7_car(args)))->h_addrtype));}
                  static s7_pointer g_hostent_h_length(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct hostent *)s7_c_pointer(s7_car(args)))->h_length));}
                  static s7_pointer g_netent_n_addrtype(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct netent *)s7_c_pointer(s7_car(args)))->n_addrtype));}
                  static s7_pointer g_netent_n_net(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct netent *)s7_c_pointer(s7_car(args)))->n_net));}
                  static s7_pointer g_servent_s_port(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct servent *)s7_c_pointer(s7_car(args)))->s_port));}
                  static s7_pointer g_protoent_p_proto(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct protoent *)s7_c_pointer(s7_car(args)))->p_proto));}

                  static s7_pointer g_hostent_h_aliases(s7_scheme *sc, s7_pointer args)
                  {
                    s7_pointer p;
                    char **str;	     
                    struct hostent *h;
                    p = s7_nil(sc);
                    h = (struct hostent *)s7_c_pointer(s7_car(args));
                    for (str = h->h_aliases; (str) && (*str); str++)
                      p = s7_cons(sc, s7_make_string(sc, *str), p);
                    return(p);
                  }
                  static s7_pointer g_servent_s_aliases(s7_scheme *sc, s7_pointer args)
                  {
                    s7_pointer p;
                    char **str;	     
                    struct servent *h;
                    p = s7_nil(sc);
                    h = (struct servent *)s7_c_pointer(s7_car(args));
                    for (str = h->s_aliases; (str) && (*str); str++)
                      p = s7_cons(sc, s7_make_string(sc, *str), p);
                    return(p);
                  }
                  static s7_pointer g_netent_n_aliases(s7_scheme *sc, s7_pointer args)
                  {
                    s7_pointer p;
                    char **str;	     
                    struct netent *h;
                    p = s7_nil(sc);
                    h = (struct netent *)s7_c_pointer(s7_car(args));
                    for (str = h->n_aliases; (str) && (*str); str++)
                      p = s7_cons(sc, s7_make_string(sc, *str), p);
                    return(p);
                  }
                  static s7_pointer g_protoent_p_aliases(s7_scheme *sc, s7_pointer args)
                  {
                    s7_pointer p;
                    char **str;	     
                    struct protoent *h;
                    p = s7_nil(sc);
                    h = (struct protoent *)s7_c_pointer(s7_car(args));
                    for (str = h->p_aliases; (str) && (*str); str++)
                      p = s7_cons(sc, s7_make_string(sc, *str), p);
                    return(p);
                  }
                 ")
	   (C-function ("htonl" g_htonl "" 1))
	   (C-function ("htons" g_htons "" 1))
	   (C-function ("ntohl" g_ntohl "" 1))
	   (C-function ("ntohs" g_ntohs "" 1))
	   
	   (C-function ("getaddrinfo" g_getaddrinfo "" 3))
	   (C-function ("getnameinfo" g_getnameinfo "" 3))
	   (C-function ("addrinfo.make" g_addrinfo_make "" 0))
	   (C-function ("addrinfo.ai_flags" g_addrinfo_ai_flags "" 1))
	   (C-function ("addrinfo.set_ai_flags" g_addrinfo_set_ai_flags "" 2))
	   (C-function ("addrinfo.ai_family" g_addrinfo_ai_family "" 1))
	   (C-function ("addrinfo.set_ai_family" g_addrinfo_set_ai_family "" 2))
	   (C-function ("addrinfo.ai_socktype" g_addrinfo_ai_socktype "" 1))
	   (C-function ("addrinfo.set_ai_socktype" g_addrinfo_set_ai_socktype "" 2))
	   (C-function ("addrinfo.ai_protocol" g_addrinfo_ai_protocol "" 1))
	   (C-function ("addrinfo.set_ai_protocol" g_addrinfo_set_ai_protocol "" 2))
	   (C-function ("addrinfo.ai_canonname" g_addrinfo_ai_canonname "" 1))
	   (C-function ("addrinfo.ai_next" g_addrinfo_ai_next "" 1))
	   
	   (C-function ("hostent.h_name" g_hostent_h_name "" 1))
	   (C-function ("netent.n_name" g_netent_n_name "" 1))
	   (C-function ("servent.s_name" g_servent_s_name "" 1))
	   (C-function ("servent.s_proto" g_servent_s_proto "" 1))
	   (C-function ("protoent.p_name" g_protoent_p_name "" 1))
	   (C-function ("hostent.h_addrtype" g_hostent_h_addrtype "" 1))
	   (C-function ("hostent.h_length" g_hostent_h_length "" 1))
	   (C-function ("netent.n_addrtype" g_netent_n_addrtype "" 1))
	   (C-function ("netent.n_net" g_netent_n_net "" 1))
	   (C-function ("servent.s_port" g_servent_s_port "" 1))
	   (C-function ("protoent.p_proto" g_protoent_p_proto "" 1))
	   
	   (C-function ("hostent.h_aliases" g_hostent_h_aliases "" 1))
	   (C-function ("servent.s_aliases" g_servent_s_aliases "" 1))
	   (C-function ("netent.n_aliases" g_netent_n_aliases "" 1))
	   (C-function ("protoent.p_aliases" g_protoent_p_aliases "" 1))
	   ;; (define h (gethostbyname "fatty4"))
	   ;; ((*libc* 'hostent.h_aliases) h) -> ("localhost" "localhost.localdomain")
	   
	   (C-function ("socketpair" g_socketpair "" 3))
	   (C-function ("getsockname" g_getsockname "" 3))
	   (C-function ("getpeername" g_getpeername "" 3))
	   (C-function ("accept" g_accept "" 3))
	   (C-function ("getsockopt" g_getsockopt "" 5))
	   (C-function ("setsockopt" g_setsockopt "" 5))
	   (C-function ("recvfrom" g_recvfrom "" 6))
	   )
	 
	 "" 
	 (list "limits.h" "ctype.h" "errno.h" "float.h" "stdint.h" "locale.h" "stdlib.h" "string.h" "fcntl.h" 
	       "fenv.h" "stdio.h" "sys/utsname.h" "unistd.h" "dirent.h" "ftw.h" "sys/stat.h" "time.h" "sys/time.h"
	       "utime.h" "termios.h" "grp.h" "pwd.h" "fnmatch.h" "glob.h" "signal.h" "sys/wait.h" "netdb.h" 
	       "sys/resource.h"
	       (reader-cond ((not (provided? 'openbsd)) "wordexp.h"))
	       (reader-cond ((provided? 'freebsd) "sys/socket.h" "netinet/in.h"))
	       )
	 "" 
	 (if (provided? 'linux) "-lrt" 
	     (if (provided? 'openbsd) "-pthread" ""))
	 "libc_s7")
	
	(curlet))))

*libc*
