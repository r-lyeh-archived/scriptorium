;;; libm.scm
;;;
;;; tie the math library into the *libm* environment

(require cload.scm)
(provide 'libm.scm)

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


(if (not (defined? '*libm*))
    (define *libm*
      (with-let (unlet)
	
	(set! *libraries* (cons (cons "libm.scm" (curlet)) *libraries*))

	(c-define 
	 '((double j0 (double) "Bessel j0") 
	   (double j1 (double)) 
	   (double erf (double)) 
	   (double erfc (double))
	   (double lgamma (double))
	   
	   (double fabs (double))
	   (double ceil (double))
	   (reader-cond ((not (provided? 'netbsd))
			 (double nearbyint (double))
			 (double scalbln (double int))
			 (double fma (double double double))))
	   (double rint (double))
	   (int llrint (double))
	   (int llround (double))
	   (double trunc (double))
	   (double fmod (double double))
	   (double ldexp (double int))
	   (double scalbn (double int))
	   (double exp2 (double))
	   (double expm1 (double))
	   (double log10 (double))
	   (double log1p (double))
	   (double log2 (double))
	   (int ilogb (double))
	   (double cbrt (double))
	   (double hypot (double double))
	   (double pow (double double))
	   (double fdim (double double))
	   (double tgamma (double))
	   (double copysign (double double))
	   (double nan (char*))
	   (double nextafter (double double))
	   (double nexttoward (double double))
	   
	   (reader-cond ((not (provided? 'solaris)) 
			 (int fpclassify (double))
			 (int isfinite (double))
			 (int isinf (double))
			 (int isnan (double))
			 (int isnormal (double))
			 (int signbit (double))))
	   
	   ;; exporting these will overwrite the built-in versions
	   (double floor (double))
	   (double round (double))
	   (double remainder (double double))
	   (double exp (double))
	   (double log (double))
	   (double sqrt (double))
	   (double cos (double))
	   (double sin (double))
	   (double tan (double))
	   (double cosh (double))
	   (double sinh (double))
	   (double tanh (double))
	   (double acos (double))
	   (double asin (double))
	   (double atan (double))
	   (double atan2 (double double))
	   (double acosh (double))
	   (double asinh (double))
	   (double atanh  (double))
	   
	   (int (FP_NAN FP_INFINITE FP_ZERO FP_SUBNORMAL FP_NORMAL))
	   (double (M_E M_LOG2E M_LOG10E M_LN2 M_LN10 M_PI M_PI_2 M_PI_4 M_1_PI M_2_PI M_2_SQRTPI M_SQRT2 M_SQRT1_2))
	   
	   (C-macro (char* __VERSION__))
	   (C-macro (int (__SIZEOF_LONG_LONG__ __SIZEOF_INT__ __SIZEOF_POINTER__ __SIZEOF_LONG__ __SIZEOF_LONG_DOUBLE__ __SIZEOF_SIZE_T__ 
					       __SIZEOF_FLOAT__ __SIZEOF_SHORT__ __SIZEOF_DOUBLE__ __CHAR_BIT__ __DBL_MIN_EXP__ __DBL_MIN_10_EXP__ __LDBL_MAX_EXP__ 
					       __DBL_DIG__ __DECIMAL_DIG__ __BIGGEST_ALIGNMENT__ __DBL_MAX_EXP__ __LONG_LONG_MAX__ __FLT_MIN_EXP__ __FLT_MANT_DIG__ 
					       __FLT_RADIX__ __FLT_MAX_10_EXP__ __LONG_MAX__ __LDBL_MANT_DIG__ __FLT_DIG__ __INT_MAX__ __FLT_MAX_EXP__ 
					       __DBL_MANT_DIG__ __LDBL_MIN_EXP__ __LDBL_MAX_10_EXP__ __INTMAX_MAX__ __FLT_MIN_10_EXP__ __DBL_MAX_10_EXP__ 
					       __LDBL_MIN_10_EXP__ __LDBL_DIG__)))
	   (C-macro (double (__FLT_MIN__ __DBL_DENORM_MIN__ __LDBL_MAX__ __FLT_EPSILON__ __LDBL_MIN__ __DBL_MAX__ __DBL_MIN__ 
					 __LDBL_EPSILON__ __DBL_EPSILON__ __FLT_DENORM_MIN__ __FLT_MAX__ __LDBL_DENORM_MIN__)))
	   
	   ;; these have arg by reference, return list in s7
	   (in-C "
static s7_pointer g_remquo(s7_scheme *sc, s7_pointer args)
{
  if (s7_is_real(s7_car(args)))
    {
      if (s7_is_real(s7_cadr(args)))
        {
          int quo = 0;
          double rem;
          rem = remquo(s7_number_to_real(sc, s7_car(args)), s7_number_to_real(sc, s7_cadr(args)), &quo);
          return(s7_list(sc, 2, s7_make_real(sc, rem), s7_make_integer(sc, quo)));
        }
      return(s7_wrong_type_arg_error(sc, \"remquo\", 2, s7_cadr(args), \"a real\"));
     }
  return(s7_wrong_type_arg_error(sc, \"remquo\", 1, s7_car(args), \"a real\"));
}
static s7_pointer g_frexp(s7_scheme *sc, s7_pointer args)
{
  if (s7_is_real(s7_car(args)))
    {
      int ex = 0;
      double frac;
      frac = frexp(s7_number_to_real(sc, s7_car(args)), &ex);
      return(s7_list(sc, 2, s7_make_real(sc, frac), s7_make_integer(sc, ex)));
     }
  return(s7_wrong_type_arg_error(sc, \"frexp\", 1, s7_car(args), \"a real\"));
}
static s7_pointer g_modf(s7_scheme *sc, s7_pointer args)
{
  if (s7_is_real(s7_car(args)))
    {
      double frac, ip = 0.0;
      frac = modf(s7_number_to_real(sc, s7_car(args)), &ip);
      return(s7_list(sc, 2, s7_make_real(sc, frac), s7_make_real(sc, ip)));
     }
  return(s7_wrong_type_arg_error(sc, \"modf\", 1, s7_car(args), \"a real\"));
}
")
                    (C-function ("remquo" g_remquo "(remquo x y) returns a list: (remainder messed-up-quotient)" 2))
                    (C-function ("frexp" g_frexp "(frexp x) returns a list: (fraction exponent)" 1))
                    (C-function ("modf" g_modf "(modf x) returns a list: (int-part frac-part) -- this is not the same as fmod!" 1))
		    )
		  "" "math.h" "" "" "libm_s7")
	
	(curlet))))

*libm*
;; the loader will return *libm*

