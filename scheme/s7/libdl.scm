;;; libdl.scm
;;;
;;; tie the dynamic loader library into the *libdl* environment

(require cload.scm)
(provide 'libdl.scm)

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


(if (not (defined? '*libdl*))
    (define *libdl*
      (with-let (unlet)
	(set! *libraries* (cons (cons "libdl.scm" (curlet)) *libraries*))
	(c-define '((void* dlopen (char* int))
		    (int dlclose (void*))
		    (void* dlsym (void* char*))
		    (char* dlerror (void))
		    (C-macro (int (RTLD_LAZY RTLD_NOW RTLD_BINDING_MASK RTLD_NOLOAD RTLD_DEEPBIND RTLD_GLOBAL RTLD_LOCAL RTLD_NODELETE))))
		  "" "dlfcn.h" "" "" "libdl_s7")
	(curlet))))

*libdl*
;; the loader will return *libdl*
