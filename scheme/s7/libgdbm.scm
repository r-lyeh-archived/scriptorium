;;; libgdbm.scm
;;;
;;; tie libgdbm into s7

(require cload.scm)
(provide 'libgdbm.scm)

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


(if (not (defined? '*libgdbm*))
    (define *libgdbm*
      (with-let (unlet)
	(set! *libraries* (cons (cons "libgdbm.scm" (curlet)) *libraries*))
	
	(c-define '((C-macro (int (GDBM_READER GDBM_WRITER GDBM_WRCREAT GDBM_NEWDB GDBM_FAST GDBM_SYNC GDBM_NOLOCK
				   GDBM_INSERT GDBM_REPLACE GDBM_CACHESIZE GDBM_FASTMODE GDBM_SYNCMODE GDBM_CENTFREE 
				   GDBM_COALESCEBLKS GDBM_OPENMASK GDBM_NOMMAP GDBM_CLOEXEC GDBM_SETCACHESIZE GDBM_SETSYNCMODE 
				   GDBM_SETCENTFREE GDBM_SETCOALESCEBLKS GDBM_SETMAXMAPSIZE GDBM_SETMMAP GDBM_GETFLAGS GDBM_GETMMAP 
				   GDBM_GETCACHESIZE GDBM_GETSYNCMODE GDBM_GETCENTFREE GDBM_GETCOALESCEBLKS GDBM_GETMAXMAPSIZE GDBM_GETDBNAME)))
		    (C-macro (int (GDBM_VERSION_MAJOR GDBM_VERSION_MINOR GDBM_VERSION_PATCH)))
		    (C-macro (int (GDBM_NO_ERROR GDBM_MALLOC_ERROR GDBM_BLOCK_SIZE_ERROR GDBM_FILE_OPEN_ERROR GDBM_FILE_WRITE_ERROR
				   GDBM_FILE_SEEK_ERROR GDBM_FILE_READ_ERROR GDBM_BAD_MAGIC_NUMBER GDBM_EMPTY_DATABASE GDBM_CANT_BE_READER
				   GDBM_CANT_BE_WRITER GDBM_READER_CANT_DELETE GDBM_READER_CANT_STORE GDBM_READER_CANT_REORGANIZE
				   GDBM_UNKNOWN_UPDATE GDBM_ITEM_NOT_FOUND GDBM_REORGANIZE_FAILED GDBM_CANNOT_REPLACE GDBM_ILLEGAL_DATA
				   GDBM_OPT_ALREADY_SET GDBM_OPT_ILLEGAL GDBM_BYTE_SWAPPED GDBM_BAD_FILE_OFFSET GDBM_BAD_OPEN_FLAGS
				   GDBM_FILE_STAT_ERROR GDBM_FILE_EOF)))

		    (in-C "static s7_pointer g_gdbm_version(s7_scheme *sc, s7_pointer args) {return(s7_make_string(sc, gdbm_version));}")
		    (C-function ("gdbm_version" g_gdbm_version "(gdbm_version) returns the current gbdm version" 0))
		    (in-C "static s7_pointer g_gdbm_errno(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, gdbm_errno));}")
		    (C-function ("gdbm_errno" g_gdbm_errno "(gdbm_errno) returns the current gdbm error number" 0))
		    (char* gdbm_strerror (int))

		    (int gdbm_fdesc ((GDBM_FILE c_pointer)))
		    (int gdbm_reorganize ((GDBM_FILE c_pointer)))
		    (void gdbm_close ((GDBM_FILE c_pointer)))
		    (void gdbm_sync ((GDBM_FILE c_pointer)))

		    ;(int gdbm_export ((GDBM_FILE c_pointer) char* int int))
		    ;(int gdbm_import ((GDBM_FILE c_pointer) char* int))

		    (in-C "
static void *make_datum(datum key) {datum *p; p = (datum *)malloc(sizeof(datum)); p->dptr = key.dptr; p->dsize = key.dsize; return((void *)p);}
static s7_pointer g_gdbm_firstkey(s7_scheme *sc, s7_pointer args)
{
  if (s7_is_c_pointer(s7_car(args)))
    {
      datum key;
      key = gdbm_firstkey((GDBM_FILE)s7_c_pointer(s7_car(args)));
      if (key.dptr)
         return(s7_cons(sc, s7_make_string_with_length(sc, key.dptr, key.dsize), s7_make_c_pointer(sc, make_datum(key))));
      return(s7_eof_object(sc));
    }
  return(s7_wrong_type_arg_error(sc, \"gdbm_firstkey\", 0, s7_car(args), \"a gdbm file\"));
}

static s7_pointer g_gdbm_nextkey(s7_scheme *sc, s7_pointer args)
{
  if (s7_is_c_pointer(s7_car(args)))
    {
      if (s7_is_c_pointer(s7_cadr(args)))
        {
	  datum *p;
          datum key, rtn;
          p = (datum *)s7_c_pointer(s7_cadr(args));
	  key.dptr = p->dptr;
	  key.dsize = p->dsize;
          rtn = gdbm_nextkey((GDBM_FILE)s7_c_pointer(s7_car(args)), key);
          free(key.dptr);
	  free(p);
          if (rtn.dptr)
	     return(s7_cons(sc, s7_make_string_with_length(sc, rtn.dptr, rtn.dsize), s7_make_c_pointer(sc, make_datum(rtn))));
          return(s7_eof_object(sc));
	}
      return(s7_wrong_type_arg_error(sc, \"gdbm_nextkey\", 2, s7_cadr(args), \"a string\"));
    }
  return(s7_wrong_type_arg_error(sc, \"gdbm_nextkey\", 1, s7_car(args), \"a gdbm file\"));
}

static s7_pointer g_gdbm_exists(s7_scheme *sc, s7_pointer args)
{
  if (s7_is_c_pointer(s7_car(args)))
    {
      if (s7_is_string(s7_cadr(args)))
        {
          datum key;
          key.dptr = (char *)s7_string(s7_cadr(args));
          key.dsize = (int)s7_string_length(s7_cadr(args));
          return(s7_make_integer(sc, gdbm_exists((GDBM_FILE)s7_c_pointer(s7_car(args)), key)));
	}
      return(s7_wrong_type_arg_error(sc, \"gdbm_exists\", 2, s7_cadr(args), \"a string\"));
    }
  return(s7_wrong_type_arg_error(sc, \"gdbm_exists\", 1, s7_car(args), \"a gdbm file\"));
}

static s7_pointer g_gdbm_delete(s7_scheme *sc, s7_pointer args)
{
  if (s7_is_c_pointer(s7_car(args)))
    {
      if (s7_is_string(s7_cadr(args)))
        {
          datum key;
          key.dptr = (char *)s7_string(s7_cadr(args));
          key.dsize = (int)s7_string_length(s7_cadr(args));
          return(s7_make_integer(sc, gdbm_delete((GDBM_FILE)s7_c_pointer(s7_car(args)), key)));	}                                                                                                               \n\
      return(s7_wrong_type_arg_error(sc, \"gdbm_delete\", 2, s7_cadr(args), \"a string\"));
    }
  return(s7_wrong_type_arg_error(sc, \"gdbm_delete\", 1, s7_car(args), \"a gdbm file\"));
}

static s7_pointer g_gdbm_fetch(s7_scheme *sc, s7_pointer args)
{
  if (s7_is_c_pointer(s7_car(args)))
    {
      if (s7_is_string(s7_cadr(args)))
        {
          datum key, rtn;
          key.dptr = (char *)s7_string(s7_cadr(args));
          key.dsize = (int)s7_string_length(s7_cadr(args));
          rtn = gdbm_fetch((GDBM_FILE)s7_c_pointer(s7_car(args)), key);
          if (rtn.dptr)
            {
  	      s7_pointer result;
              result = s7_make_string_with_length(sc, rtn.dptr, rtn.dsize - 1);
              free(rtn.dptr);
              return(result);
	    }
          else return(s7_make_string_with_length(sc, \"#<undefined>\", 12));
	}
      return(s7_wrong_type_arg_error(sc, \"gdbm_fetch\", 2, s7_cadr(args), \"a string\"));
    }
  return(s7_wrong_type_arg_error(sc, \"gdbm_fetch\", 1, s7_car(args), \"a gdbm file\"));
}

static s7_pointer g_gdbm_store(s7_scheme *sc, s7_pointer args)
{
  if (s7_is_c_pointer(s7_car(args)))
    {
      if (s7_is_string(s7_cadr(args)))
        {
          if (s7_is_string(s7_caddr(args)))
            {
              if (s7_is_integer(s7_cadddr(args)))
                {
                  datum key, val;
                  key.dptr = (char *)s7_string(s7_cadr(args));
                  key.dsize = (int)s7_string_length(s7_cadr(args));
                  val.dptr = (char *)s7_string(s7_caddr(args));
                  val.dsize = (int)s7_string_length(s7_caddr(args)) + 1;
                  return(s7_make_integer(sc, gdbm_store((GDBM_FILE)s7_c_pointer(s7_car(args)), key, val, (int)s7_integer(s7_cadddr(args)))));
                }
              return(s7_wrong_type_arg_error(sc, \"gdbm_fetch\", 4, s7_cadddr(args), \"an integer (flag)\"));
	    }
          return(s7_wrong_type_arg_error(sc, \"gdbm_fetch\", 3, s7_caddr(args), \"a string\"));
	}
      return(s7_wrong_type_arg_error(sc, \"gdbm_fetch\", 2, s7_cadr(args), \"a string\"));
    }
  return(s7_wrong_type_arg_error(sc, \"gdbm_fetch\", 1, s7_car(args), \"a gdbm file\"));
}

static s7_pointer open_error_func = NULL;
static s7_scheme *open_error_s7 = NULL;
static void gdbm_open_error(const char *name)
{
  if (open_error_func)
    s7_apply_function(open_error_s7, open_error_func, s7_list(open_error_s7, 1, s7_make_string(open_error_s7, name)));
}

static s7_pointer g_gdbm_open(s7_scheme *sc, s7_pointer args)
{
  if (s7_is_string(s7_car(args)))
    {
      char *name;
      name = (char *)s7_string(s7_car(args));
      args = s7_cdr(args);
      if (s7_is_integer(s7_car(args)))
        {
	  int block_size;
          block_size = (int)s7_integer(s7_car(args));
          args = s7_cdr(args);
          if (s7_is_integer(s7_car(args)))
            {
	      int flags;
              flags = (int)s7_integer(s7_car(args));
              args = s7_cdr(args);
              if (s7_is_integer(s7_car(args)))
                {
	          int mode;
                  mode = (int)s7_integer(s7_car(args));
		  if (s7_is_procedure(s7_cadr(args)))
		    {
                      open_error_func = s7_cadr(args);
                      open_error_s7 = sc;
                    }
                  else
		    {
                      open_error_func = NULL;
                      open_error_s7 = NULL;
                    }
                  return(s7_make_c_pointer(sc, (void *)gdbm_open(name, block_size, flags, mode, gdbm_open_error)));
                }
              return(s7_wrong_type_arg_error(sc, \"gdbm_open\", 4, s7_car(args), \"an integer (mode)\"));
            }
          return(s7_wrong_type_arg_error(sc, \"gdbm_open\", 3, s7_car(args), \"an integer (flags)\"));
        }
      return(s7_wrong_type_arg_error(sc, \"gdbm_open\", 2, s7_car(args), \"an integer (block_size)\"));
    }
  return(s7_wrong_type_arg_error(sc, \"gdbm_open\", 1, s7_car(args), \"a string (file name)\"));
}
")
                    (C-function ("gdbm_firstkey" g_gdbm_firstkey "(gdbm_firstkey gdbm)" 1))
                    (C-function ("gdbm_exists" g_gdbm_exists "(gdbm_exists gdbm key)" 2))
                    (C-function ("gdbm_delete" g_gdbm_delete "(gdbm_delete gdbm key)" 2))
                    (C-function ("gdbm_nextkey" g_gdbm_nextkey "(gdbm_nextkey gdbm prev)" 2))
                    (C-function ("gdbm_fetch" g_gdbm_fetch "(gdbm_fetch gdbm key)" 2))
                    (C-function ("gdbm_store" g_gdbm_store "(gdbm_store gdbm key context flag)" 4))
                    (C-function ("gdbm_open" g_gdbm_open "(gdbm_open filename size flags mode func) opens a gdbm data base" 5))

		    )
		  "" "gdbm.h" "" "-lgdbm" "libgdbm_s7")
	
	(curlet))))

*libgdbm*



;;; extern int gdbm_setopt (GDBM_FILE, int, void *, int)
;;; this is a huge mess

#|
(define gfile ((*libgdbm* 'gdbm_open) "test.gdbm" 1024 (*libgdbm* 'GDBM_NEWDB) #o664 (lambda (str) (format *stderr* "str: ~S~%" str))))
((*libgdbm* 'gdbm_store) gfile "1" "1234" (*libgdbm* 'GDBM_REPLACE))
((*libgdbm* 'gdbm_fetch) gfile "1")
((*libgdbm* 'gdbm_close) gfile)
|#
