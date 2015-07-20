;;; libgsl.scm
;;;
;;; tie the gsl library into the *libgsl* environment

(require cload.scm)
(provide 'libgsl.scm)

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


(when (provided? 'make-complex)
  (define magnitude abs)
  (define make-rectangular make-complex)
  (define (make-polar mag ang)
    (if (and (real? mag) (real? ang))
	(make-complex (* mag (cos ang)) (* mag (sin ang)))
	(error 'wrong-type-arg "make-polar args should be real"))))

;; since we might be loading this locally, reader-cond (in that case) won't find gsl-version unless...
(if (not (defined? '*libgsl*))
    (with-let (rootlet)
      (define gsl-version 0.0)		; define at top-level no matter where we are now
      (when (and (provided? 'linux)
		 (defined? 'system))
	(let ((version (system "pkg-config gsl --modversion" #t)))
	  (if (positive? (length version))
	      (set! gsl-version (with-input-from-string version read)))))))

(if (not (defined? '*libgsl*))
    (define *libgsl*
      (with-let (unlet)
	(set! *libraries* (cons (cons "libgsl.scm" (curlet)) *libraries*))

	(define GSL_REAL real-part)
	(define GSL_IMAG imag-part)
	(define GSL_COMPLEX_EQ equal?)
	(define GSL_COMPLEX_ONE 1.0)
	(define GSL_COMPLEX_ZERO 0.0)
	(define GSL_COMPLEX_NEGONE -1.0)
	(define gsl_complex_polar make-polar)
	(define gsl_complex_rect make-rectangular)
	(define GSL_IS_ODD odd?)
	(define GSL_IS_EVEN even?)
	(define (GSL_IS_REAL n) (and (number? n) (not (nan? n)) (not (infinite? n))))
	(define (GSL_SIGN x) (if (negative? x) -1 1))
	(define GSL_MAX max)
	(define GSL_MIN min)
	(define GSL_MAX_INT max)
	(define GSL_MIN_INT min)
	(define GSL_MAX_DBL max)
	(define GSL_MIN_DBL min)
	(define gsl_max max)
	(define gsl_min min)

	(c-define 
	 '((C-macro (double (GSL_CONST_CGS_SPEED_OF_LIGHT GSL_CONST_CGS_GRAVITATIONAL_CONSTANT GSL_CONST_CGS_PLANCKS_CONSTANT_H 
			     GSL_CONST_CGS_PLANCKS_CONSTANT_HBAR GSL_CONST_CGS_ASTRONOMICAL_UNIT GSL_CONST_CGS_LIGHT_YEAR 
			     GSL_CONST_CGS_PARSEC GSL_CONST_CGS_GRAV_ACCEL GSL_CONST_CGS_ELECTRON_VOLT GSL_CONST_CGS_MASS_ELECTRON 
			     GSL_CONST_CGS_MASS_MUON GSL_CONST_CGS_MASS_PROTON GSL_CONST_CGS_MASS_NEUTRON GSL_CONST_CGS_RYDBERG 
			     GSL_CONST_CGS_BOLTZMANN GSL_CONST_CGS_MOLAR_GAS GSL_CONST_CGS_STANDARD_GAS_VOLUME GSL_CONST_CGS_MINUTE 
			     GSL_CONST_CGS_HOUR GSL_CONST_CGS_DAY GSL_CONST_CGS_WEEK GSL_CONST_CGS_INCH GSL_CONST_CGS_FOOT 
			     GSL_CONST_CGS_YARD GSL_CONST_CGS_MILE GSL_CONST_CGS_NAUTICAL_MILE GSL_CONST_CGS_FATHOM GSL_CONST_CGS_MIL 
			     GSL_CONST_CGS_POINT GSL_CONST_CGS_TEXPOINT GSL_CONST_CGS_MICRON GSL_CONST_CGS_ANGSTROM GSL_CONST_CGS_HECTARE 
			     GSL_CONST_CGS_ACRE GSL_CONST_CGS_BARN GSL_CONST_CGS_LITER GSL_CONST_CGS_US_GALLON GSL_CONST_CGS_QUART 
			     GSL_CONST_CGS_PINT GSL_CONST_CGS_CUP GSL_CONST_CGS_FLUID_OUNCE GSL_CONST_CGS_TABLESPOON GSL_CONST_CGS_TEASPOON 
			     GSL_CONST_CGS_CANADIAN_GALLON GSL_CONST_CGS_UK_GALLON GSL_CONST_CGS_MILES_PER_HOUR GSL_CONST_CGS_KILOMETERS_PER_HOUR 
			     GSL_CONST_CGS_KNOT GSL_CONST_CGS_POUND_MASS GSL_CONST_CGS_OUNCE_MASS GSL_CONST_CGS_TON GSL_CONST_CGS_METRIC_TON 
			     GSL_CONST_CGS_UK_TON GSL_CONST_CGS_TROY_OUNCE GSL_CONST_CGS_CARAT GSL_CONST_CGS_UNIFIED_ATOMIC_MASS 
			     GSL_CONST_CGS_GRAM_FORCE GSL_CONST_CGS_POUND_FORCE GSL_CONST_CGS_KILOPOUND_FORCE GSL_CONST_CGS_POUNDAL 
			     GSL_CONST_CGS_CALORIE GSL_CONST_CGS_BTU GSL_CONST_CGS_THERM GSL_CONST_CGS_HORSEPOWER GSL_CONST_CGS_BAR 
			     GSL_CONST_CGS_STD_ATMOSPHERE GSL_CONST_CGS_TORR GSL_CONST_CGS_METER_OF_MERCURY GSL_CONST_CGS_INCH_OF_MERCURY 
			     GSL_CONST_CGS_INCH_OF_WATER GSL_CONST_CGS_PSI GSL_CONST_CGS_POISE GSL_CONST_CGS_STOKES GSL_CONST_CGS_STILB 
			     GSL_CONST_CGS_LUMEN GSL_CONST_CGS_LUX GSL_CONST_CGS_PHOT GSL_CONST_CGS_FOOTCANDLE GSL_CONST_CGS_LAMBERT 
			     GSL_CONST_CGS_FOOTLAMBERT GSL_CONST_CGS_CURIE GSL_CONST_CGS_ROENTGEN GSL_CONST_CGS_RAD GSL_CONST_CGS_SOLAR_MASS 
			     GSL_CONST_CGS_BOHR_RADIUS GSL_CONST_CGS_NEWTON GSL_CONST_CGS_DYNE GSL_CONST_CGS_JOULE GSL_CONST_CGS_ERG 
			     GSL_CONST_CGS_STEFAN_BOLTZMANN_CONSTANT GSL_CONST_CGS_THOMSON_CROSS_SECTION GSL_CONST_CGSM_SPEED_OF_LIGHT 
			     GSL_CONST_CGSM_GRAVITATIONAL_CONSTANT GSL_CONST_CGSM_PLANCKS_CONSTANT_H GSL_CONST_CGSM_PLANCKS_CONSTANT_HBAR 
			     GSL_CONST_CGSM_ASTRONOMICAL_UNIT GSL_CONST_CGSM_LIGHT_YEAR GSL_CONST_CGSM_PARSEC GSL_CONST_CGSM_GRAV_ACCEL 
			     GSL_CONST_CGSM_ELECTRON_VOLT GSL_CONST_CGSM_MASS_ELECTRON GSL_CONST_CGSM_MASS_MUON GSL_CONST_CGSM_MASS_PROTON 
			     GSL_CONST_CGSM_MASS_NEUTRON GSL_CONST_CGSM_RYDBERG GSL_CONST_CGSM_BOLTZMANN GSL_CONST_CGSM_MOLAR_GAS 
			     GSL_CONST_CGSM_STANDARD_GAS_VOLUME GSL_CONST_CGSM_MINUTE GSL_CONST_CGSM_HOUR GSL_CONST_CGSM_DAY 
			     GSL_CONST_CGSM_WEEK GSL_CONST_CGSM_INCH GSL_CONST_CGSM_FOOT GSL_CONST_CGSM_YARD GSL_CONST_CGSM_MILE 
			     GSL_CONST_CGSM_NAUTICAL_MILE GSL_CONST_CGSM_FATHOM GSL_CONST_CGSM_MIL GSL_CONST_CGSM_POINT GSL_CONST_CGSM_TEXPOINT 
			     GSL_CONST_CGSM_MICRON GSL_CONST_CGSM_ANGSTROM GSL_CONST_CGSM_HECTARE GSL_CONST_CGSM_ACRE GSL_CONST_CGSM_BARN 
			     GSL_CONST_CGSM_LITER GSL_CONST_CGSM_US_GALLON GSL_CONST_CGSM_QUART GSL_CONST_CGSM_PINT GSL_CONST_CGSM_CUP 
			     GSL_CONST_CGSM_FLUID_OUNCE GSL_CONST_CGSM_TABLESPOON GSL_CONST_CGSM_TEASPOON GSL_CONST_CGSM_CANADIAN_GALLON 
			     GSL_CONST_CGSM_UK_GALLON GSL_CONST_CGSM_MILES_PER_HOUR GSL_CONST_CGSM_KILOMETERS_PER_HOUR GSL_CONST_CGSM_KNOT 
			     GSL_CONST_CGSM_POUND_MASS GSL_CONST_CGSM_OUNCE_MASS GSL_CONST_CGSM_TON GSL_CONST_CGSM_METRIC_TON 
			     GSL_CONST_CGSM_UK_TON GSL_CONST_CGSM_TROY_OUNCE GSL_CONST_CGSM_CARAT GSL_CONST_CGSM_UNIFIED_ATOMIC_MASS 
			     GSL_CONST_CGSM_GRAM_FORCE GSL_CONST_CGSM_POUND_FORCE GSL_CONST_CGSM_KILOPOUND_FORCE GSL_CONST_CGSM_POUNDAL 
			     GSL_CONST_CGSM_CALORIE GSL_CONST_CGSM_BTU GSL_CONST_CGSM_THERM GSL_CONST_CGSM_HORSEPOWER GSL_CONST_CGSM_BAR 
			     GSL_CONST_CGSM_STD_ATMOSPHERE GSL_CONST_CGSM_TORR GSL_CONST_CGSM_METER_OF_MERCURY GSL_CONST_CGSM_INCH_OF_MERCURY 
			     GSL_CONST_CGSM_INCH_OF_WATER GSL_CONST_CGSM_PSI GSL_CONST_CGSM_POISE GSL_CONST_CGSM_STOKES GSL_CONST_CGSM_STILB 
			     GSL_CONST_CGSM_LUMEN GSL_CONST_CGSM_LUX GSL_CONST_CGSM_PHOT GSL_CONST_CGSM_FOOTCANDLE GSL_CONST_CGSM_LAMBERT 
			     GSL_CONST_CGSM_FOOTLAMBERT GSL_CONST_CGSM_CURIE GSL_CONST_CGSM_ROENTGEN GSL_CONST_CGSM_RAD GSL_CONST_CGSM_SOLAR_MASS 
			     GSL_CONST_CGSM_BOHR_RADIUS GSL_CONST_CGSM_NEWTON GSL_CONST_CGSM_DYNE GSL_CONST_CGSM_JOULE GSL_CONST_CGSM_ERG 
			     GSL_CONST_CGSM_STEFAN_BOLTZMANN_CONSTANT GSL_CONST_CGSM_THOMSON_CROSS_SECTION GSL_CONST_CGSM_BOHR_MAGNETON 
			     GSL_CONST_CGSM_NUCLEAR_MAGNETON GSL_CONST_CGSM_ELECTRON_MAGNETIC_MOMENT GSL_CONST_CGSM_PROTON_MAGNETIC_MOMENT 
			     GSL_CONST_CGSM_FARADAY GSL_CONST_CGSM_ELECTRON_CHARGE GSL_CONST_MKS_SPEED_OF_LIGHT GSL_CONST_MKS_GRAVITATIONAL_CONSTANT 
			     GSL_CONST_MKS_PLANCKS_CONSTANT_H GSL_CONST_MKS_PLANCKS_CONSTANT_HBAR GSL_CONST_MKS_ASTRONOMICAL_UNIT 
			     GSL_CONST_MKS_LIGHT_YEAR GSL_CONST_MKS_PARSEC GSL_CONST_MKS_GRAV_ACCEL GSL_CONST_MKS_ELECTRON_VOLT 
			     GSL_CONST_MKS_MASS_ELECTRON GSL_CONST_MKS_MASS_MUON GSL_CONST_MKS_MASS_PROTON GSL_CONST_MKS_MASS_NEUTRON 
			     GSL_CONST_MKS_RYDBERG GSL_CONST_MKS_BOLTZMANN GSL_CONST_MKS_MOLAR_GAS GSL_CONST_MKS_STANDARD_GAS_VOLUME 
			     GSL_CONST_MKS_MINUTE GSL_CONST_MKS_HOUR GSL_CONST_MKS_DAY GSL_CONST_MKS_WEEK GSL_CONST_MKS_INCH GSL_CONST_MKS_FOOT 
			     GSL_CONST_MKS_YARD GSL_CONST_MKS_MILE GSL_CONST_MKS_NAUTICAL_MILE GSL_CONST_MKS_FATHOM GSL_CONST_MKS_MIL 
			     GSL_CONST_MKS_POINT GSL_CONST_MKS_TEXPOINT GSL_CONST_MKS_MICRON GSL_CONST_MKS_ANGSTROM GSL_CONST_MKS_HECTARE 
			     GSL_CONST_MKS_ACRE GSL_CONST_MKS_BARN GSL_CONST_MKS_LITER GSL_CONST_MKS_US_GALLON GSL_CONST_MKS_QUART 
			     GSL_CONST_MKS_PINT GSL_CONST_MKS_CUP GSL_CONST_MKS_FLUID_OUNCE GSL_CONST_MKS_TABLESPOON GSL_CONST_MKS_TEASPOON 
			     GSL_CONST_MKS_CANADIAN_GALLON GSL_CONST_MKS_UK_GALLON GSL_CONST_MKS_MILES_PER_HOUR GSL_CONST_MKS_KILOMETERS_PER_HOUR 
			     GSL_CONST_MKS_KNOT GSL_CONST_MKS_POUND_MASS GSL_CONST_MKS_OUNCE_MASS GSL_CONST_MKS_TON GSL_CONST_MKS_METRIC_TON 
			     GSL_CONST_MKS_UK_TON GSL_CONST_MKS_TROY_OUNCE GSL_CONST_MKS_CARAT GSL_CONST_MKS_UNIFIED_ATOMIC_MASS 
			     GSL_CONST_MKS_GRAM_FORCE GSL_CONST_MKS_POUND_FORCE GSL_CONST_MKS_KILOPOUND_FORCE GSL_CONST_MKS_POUNDAL 
			     GSL_CONST_MKS_CALORIE GSL_CONST_MKS_BTU GSL_CONST_MKS_THERM GSL_CONST_MKS_HORSEPOWER GSL_CONST_MKS_BAR 
			     GSL_CONST_MKS_STD_ATMOSPHERE GSL_CONST_MKS_TORR GSL_CONST_MKS_METER_OF_MERCURY GSL_CONST_MKS_INCH_OF_MERCURY 
			     GSL_CONST_MKS_INCH_OF_WATER GSL_CONST_MKS_PSI GSL_CONST_MKS_POISE GSL_CONST_MKS_STOKES GSL_CONST_MKS_STILB 
			     GSL_CONST_MKS_LUMEN GSL_CONST_MKS_LUX GSL_CONST_MKS_PHOT GSL_CONST_MKS_FOOTCANDLE GSL_CONST_MKS_LAMBERT GSL_CONST_MKS_FOOTLAMBERT 
			     GSL_CONST_MKS_CURIE GSL_CONST_MKS_ROENTGEN GSL_CONST_MKS_RAD GSL_CONST_MKS_SOLAR_MASS GSL_CONST_MKS_BOHR_RADIUS 
			     GSL_CONST_MKS_NEWTON GSL_CONST_MKS_DYNE GSL_CONST_MKS_JOULE GSL_CONST_MKS_ERG GSL_CONST_MKS_STEFAN_BOLTZMANN_CONSTANT 
			     GSL_CONST_MKS_THOMSON_CROSS_SECTION GSL_CONST_MKS_BOHR_MAGNETON GSL_CONST_MKS_NUCLEAR_MAGNETON 
			     GSL_CONST_MKS_ELECTRON_MAGNETIC_MOMENT GSL_CONST_MKS_PROTON_MAGNETIC_MOMENT GSL_CONST_MKS_FARADAY 
			     GSL_CONST_MKS_ELECTRON_CHARGE GSL_CONST_MKS_VACUUM_PERMITTIVITY GSL_CONST_MKS_VACUUM_PERMEABILITY GSL_CONST_MKS_DEBYE 
			     GSL_CONST_MKS_GAUSS GSL_CONST_MKSA_SPEED_OF_LIGHT GSL_CONST_MKSA_GRAVITATIONAL_CONSTANT GSL_CONST_MKSA_PLANCKS_CONSTANT_H 
			     GSL_CONST_MKSA_PLANCKS_CONSTANT_HBAR GSL_CONST_MKSA_ASTRONOMICAL_UNIT GSL_CONST_MKSA_LIGHT_YEAR GSL_CONST_MKSA_PARSEC 
			     GSL_CONST_MKSA_GRAV_ACCEL GSL_CONST_MKSA_ELECTRON_VOLT GSL_CONST_MKSA_MASS_ELECTRON GSL_CONST_MKSA_MASS_MUON 
			     GSL_CONST_MKSA_MASS_PROTON GSL_CONST_MKSA_MASS_NEUTRON GSL_CONST_MKSA_RYDBERG GSL_CONST_MKSA_BOLTZMANN 
			     GSL_CONST_MKSA_MOLAR_GAS GSL_CONST_MKSA_STANDARD_GAS_VOLUME GSL_CONST_MKSA_MINUTE GSL_CONST_MKSA_HOUR 
			     GSL_CONST_MKSA_DAY GSL_CONST_MKSA_WEEK GSL_CONST_MKSA_INCH GSL_CONST_MKSA_FOOT GSL_CONST_MKSA_YARD 
			     GSL_CONST_MKSA_MILE GSL_CONST_MKSA_NAUTICAL_MILE GSL_CONST_MKSA_FATHOM GSL_CONST_MKSA_MIL GSL_CONST_MKSA_POINT 
			     GSL_CONST_MKSA_TEXPOINT GSL_CONST_MKSA_MICRON GSL_CONST_MKSA_ANGSTROM GSL_CONST_MKSA_HECTARE GSL_CONST_MKSA_ACRE 
			     GSL_CONST_MKSA_BARN GSL_CONST_MKSA_LITER GSL_CONST_MKSA_US_GALLON GSL_CONST_MKSA_QUART GSL_CONST_MKSA_PINT 
			     GSL_CONST_MKSA_CUP GSL_CONST_MKSA_FLUID_OUNCE GSL_CONST_MKSA_TABLESPOON GSL_CONST_MKSA_TEASPOON GSL_CONST_MKSA_CANADIAN_GALLON 
			     GSL_CONST_MKSA_UK_GALLON GSL_CONST_MKSA_MILES_PER_HOUR GSL_CONST_MKSA_KILOMETERS_PER_HOUR GSL_CONST_MKSA_KNOT 
			     GSL_CONST_MKSA_POUND_MASS GSL_CONST_MKSA_OUNCE_MASS GSL_CONST_MKSA_TON GSL_CONST_MKSA_METRIC_TON GSL_CONST_MKSA_UK_TON 
			     GSL_CONST_MKSA_TROY_OUNCE GSL_CONST_MKSA_CARAT GSL_CONST_MKSA_UNIFIED_ATOMIC_MASS GSL_CONST_MKSA_GRAM_FORCE 
			     GSL_CONST_MKSA_POUND_FORCE GSL_CONST_MKSA_KILOPOUND_FORCE GSL_CONST_MKSA_POUNDAL GSL_CONST_MKSA_CALORIE GSL_CONST_MKSA_BTU 
			     GSL_CONST_MKSA_THERM GSL_CONST_MKSA_HORSEPOWER GSL_CONST_MKSA_BAR GSL_CONST_MKSA_STD_ATMOSPHERE GSL_CONST_MKSA_TORR 
			     GSL_CONST_MKSA_METER_OF_MERCURY GSL_CONST_MKSA_INCH_OF_MERCURY GSL_CONST_MKSA_INCH_OF_WATER GSL_CONST_MKSA_PSI 
			     GSL_CONST_MKSA_POISE GSL_CONST_MKSA_STOKES GSL_CONST_MKSA_STILB GSL_CONST_MKSA_LUMEN GSL_CONST_MKSA_LUX GSL_CONST_MKSA_PHOT 
			     GSL_CONST_MKSA_FOOTCANDLE GSL_CONST_MKSA_LAMBERT GSL_CONST_MKSA_FOOTLAMBERT GSL_CONST_MKSA_CURIE GSL_CONST_MKSA_ROENTGEN 
			     GSL_CONST_MKSA_RAD GSL_CONST_MKSA_SOLAR_MASS GSL_CONST_MKSA_BOHR_RADIUS GSL_CONST_MKSA_NEWTON GSL_CONST_MKSA_DYNE 
			     GSL_CONST_MKSA_JOULE GSL_CONST_MKSA_ERG GSL_CONST_MKSA_STEFAN_BOLTZMANN_CONSTANT GSL_CONST_MKSA_THOMSON_CROSS_SECTION 
			     GSL_CONST_MKSA_BOHR_MAGNETON GSL_CONST_MKSA_NUCLEAR_MAGNETON GSL_CONST_MKSA_ELECTRON_MAGNETIC_MOMENT 
			     GSL_CONST_MKSA_PROTON_MAGNETIC_MOMENT GSL_CONST_MKSA_FARADAY GSL_CONST_MKSA_ELECTRON_CHARGE GSL_CONST_MKSA_VACUUM_PERMITTIVITY 
			     GSL_CONST_MKSA_VACUUM_PERMEABILITY GSL_CONST_MKSA_DEBYE GSL_CONST_MKSA_GAUSS GSL_CONST_NUM_FINE_STRUCTURE GSL_CONST_NUM_AVOGADRO 
			     GSL_CONST_NUM_YOTTA GSL_CONST_NUM_ZETTA GSL_CONST_NUM_EXA GSL_CONST_NUM_PETA GSL_CONST_NUM_TERA GSL_CONST_NUM_GIGA 
			     GSL_CONST_NUM_MEGA GSL_CONST_NUM_KILO GSL_CONST_NUM_MILLI GSL_CONST_NUM_MICRO GSL_CONST_NUM_NANO GSL_CONST_NUM_PICO 
			     GSL_CONST_NUM_FEMTO GSL_CONST_NUM_ATTO GSL_CONST_NUM_ZEPTO GSL_CONST_NUM_YOCTO
			     GSL_DBL_EPSILON GSL_SQRT_DBL_EPSILON GSL_ROOT3_DBL_EPSILON GSL_ROOT4_DBL_EPSILON GSL_ROOT5_DBL_EPSILON
			     GSL_ROOT6_DBL_EPSILON GSL_LOG_DBL_EPSILON GSL_DBL_MIN GSL_SQRT_DBL_MIN GSL_ROOT3_DBL_MIN GSL_ROOT4_DBL_MIN
			     GSL_ROOT5_DBL_MIN GSL_ROOT6_DBL_MIN GSL_LOG_DBL_MIN GSL_DBL_MAX GSL_SQRT_DBL_MAX GSL_ROOT3_DBL_MAX
			     GSL_ROOT4_DBL_MAX GSL_ROOT5_DBL_MAX GSL_ROOT6_DBL_MAX GSL_LOG_DBL_MAX GSL_FLT_EPSILON GSL_SQRT_FLT_EPSILON
			     GSL_ROOT3_FLT_EPSILON GSL_ROOT4_FLT_EPSILON GSL_ROOT5_FLT_EPSILON GSL_ROOT6_FLT_EPSILON GSL_LOG_FLT_EPSILON
			     GSL_FLT_MIN GSL_SQRT_FLT_MIN GSL_ROOT3_FLT_MIN GSL_ROOT4_FLT_MIN GSL_ROOT5_FLT_MIN GSL_ROOT6_FLT_MIN
			     GSL_LOG_FLT_MIN GSL_FLT_MAX GSL_SQRT_FLT_MAX GSL_ROOT3_FLT_MAX GSL_ROOT4_FLT_MAX GSL_ROOT5_FLT_MAX
			     GSL_ROOT6_FLT_MAX GSL_LOG_FLT_MAX GSL_SFLT_EPSILON GSL_SQRT_SFLT_EPSILON GSL_ROOT3_SFLT_EPSILON GSL_ROOT4_SFLT_EPSILON
			     GSL_ROOT5_SFLT_EPSILON GSL_ROOT6_SFLT_EPSILON GSL_LOG_SFLT_EPSILON GSL_MACH_EPS GSL_SQRT_MACH_EPS GSL_ROOT3_MACH_EPS
			     GSL_ROOT4_MACH_EPS GSL_ROOT5_MACH_EPS GSL_ROOT6_MACH_EPS GSL_LOG_MACH_EPS)))
	   
	   (int (GSL_SUCCESS GSL_FAILURE GSL_CONTINUE GSL_EDOM GSL_ERANGE GSL_EFAULT GSL_EINVAL GSL_EFAILED GSL_EFACTOR GSL_ESANITY
		 GSL_ENOMEM GSL_EBADFUNC GSL_ERUNAWAY GSL_EMAXITER GSL_EZERODIV GSL_EBADTOL GSL_ETOL GSL_EUNDRFLW GSL_EOVRFLW
		 GSL_ELOSS GSL_EROUND GSL_EBADLEN GSL_ENOTSQR GSL_ESING GSL_EDIVERGE GSL_EUNSUP GSL_EUNIMPL GSL_ECACHE GSL_ETABLE
		 GSL_ENOPROG GSL_ENOPROGJ GSL_ETOLF GSL_ETOLX GSL_ETOLG GSL_EOF
		 GSL_IEEE_ROUND_TO_NEAREST GSL_IEEE_ROUND_DOWN GSL_IEEE_ROUND_UP GSL_IEEE_ROUND_TO_ZERO GSL_IEEE_MASK_INVALID
		 GSL_IEEE_MASK_DENORMALIZED GSL_IEEE_MASK_DIVISION_BY_ZERO GSL_IEEE_MASK_OVERFLOW GSL_IEEE_MASK_UNDERFLOW
		 GSL_IEEE_MASK_ALL GSL_IEEE_TRAP_INEXACT
		 GSL_INTEG_GAUSS15 GSL_INTEG_GAUSS21 GSL_INTEG_GAUSS31 GSL_INTEG_GAUSS41 GSL_INTEG_GAUSS51 GSL_INTEG_GAUSS61
		 GSL_EIGEN_SORT_VAL_ASC GSL_EIGEN_SORT_VAL_DESC GSL_EIGEN_SORT_ABS_ASC GSL_EIGEN_SORT_ABS_DESC
		 gsl_fft_forward gsl_fft_backward
		 GSL_IEEE_TYPE_NAN GSL_IEEE_TYPE_INF GSL_IEEE_TYPE_NORMAL GSL_IEEE_TYPE_DENORMAL GSL_IEEE_TYPE_ZERO
		 GSL_IEEE_SINGLE_PRECISION GSL_IEEE_DOUBLE_PRECISION GSL_IEEE_EXTENDED_PRECISION GSL_LINALG_MOD_NONE
		 GSL_LINALG_MOD_TRANSPOSE GSL_LINALG_MOD_CONJUGATE
		 GSL_MESSAGE_MASK_A GSL_MESSAGE_MASK_B GSL_MESSAGE_MASK_C GSL_MESSAGE_MASK_D GSL_MESSAGE_MASK_E
		 GSL_MESSAGE_MASK_F GSL_MESSAGE_MASK_G GSL_MESSAGE_MASK_H
		 gsl_wavelet_forward gsl_wavelet_backward))
	   
	   (C-macro (int (GSL_PREC_DOUBLE GSL_PREC_SINGLE GSL_PREC_APPROX GSL_SF_MATHIEU_COEFF GSL_SF_FACT_NMAX GSL_SF_DOUBLEFACT_NMAX
			  GSL_MAJOR_VERSION GSL_MINOR_VERSION
			  GSL_MODE_DEFAULT 
			  GSL_INTEG_COSINE GSL_INTEG_SINE)))
	   
	   (C-macro (double (GSL_SF_GAMMA_XMAX
			     GSL_POSINF GSL_NEGINF GSL_NAN GSL_POSZERO GSL_NEGZERO)))
	   
	   (C-macro (char* GSL_VERSION))
	   
	   (int (CblasRowMajor CblasColMajor CblasNoTrans CblasTrans CblasConjTrans
	         CblasUpper CblasLower CblasNonUnit CblasUnit CblasLeft CblasRight))
	   
	   
	   ;; redirect GSL errors to s7_error
	   (in-C "static s7_scheme *gsl_error_s7;
                  static void g_gsl_error(const char *reason, const char *file, int line, int gsl_errno)
                  { 
                    s7_error(gsl_error_s7, s7_make_symbol(gsl_error_s7, \"gsl-error\"),
                  	   s7_list(gsl_error_s7, 5, 
                  		   s7_make_string(gsl_error_s7, \"GSL: ~A, ~A in ~A line ~A\"),
                  		   s7_make_string(gsl_error_s7, gsl_strerror(gsl_errno)),
                  		   s7_make_string(gsl_error_s7, reason),
                  		   s7_make_string(gsl_error_s7, file),
                  		   s7_make_integer(gsl_error_s7, line)));
                  }")
	   (C-init "gsl_error_s7 = sc;")
	   (C-init "gsl_set_error_handler(g_gsl_error);")
	   
	   
	   ;; special functions
	   ;; ((*libgsl* 'gsl_sf_bessel_J0) 1.0) -> 0.7651976865579666
	   ;; (let ((sfr ((*libgsl* 'gsl_sf_result.make)))) ((*libgsl* 'gsl_sf_bessel_J0_e) 1.0 sfr) ((*libgsl* 'gsl_sf_result.val) sfr))
	   
	   (int gsl_sf_airy_Ai_e (double int gsl_sf_result*))
	   (double gsl_sf_airy_Ai (double int))
	   (int gsl_sf_airy_Bi_e (double int gsl_sf_result*))
	   (double gsl_sf_airy_Bi (double int))
	   (int gsl_sf_airy_Ai_scaled_e (double int gsl_sf_result*))
	   (double gsl_sf_airy_Ai_scaled (double int))
	   (int gsl_sf_airy_Bi_scaled_e (double int gsl_sf_result*))
	   (double gsl_sf_airy_Bi_scaled (double int))
	   (int gsl_sf_airy_Ai_deriv_e (double int gsl_sf_result*))
	   (double gsl_sf_airy_Ai_deriv (double int))
	   (int gsl_sf_airy_Bi_deriv_e (double int gsl_sf_result*))
	   (double gsl_sf_airy_Bi_deriv (double int))
	   (int gsl_sf_airy_Ai_deriv_scaled_e (double int gsl_sf_result*))
	   (double gsl_sf_airy_Ai_deriv_scaled (double int))
	   (int gsl_sf_airy_Bi_deriv_scaled_e (double int gsl_sf_result*))
	   (double gsl_sf_airy_Bi_deriv_scaled (double int))
	   (int gsl_sf_airy_zero_Ai_e (int gsl_sf_result*))
	   (double gsl_sf_airy_zero_Ai (int))
	   (int gsl_sf_airy_zero_Bi_e (int gsl_sf_result*))
	   (double gsl_sf_airy_zero_Bi (int))
	   (int gsl_sf_airy_zero_Ai_deriv_e (int gsl_sf_result*))
	   (double gsl_sf_airy_zero_Ai_deriv (int))
	   (int gsl_sf_airy_zero_Bi_deriv_e (int gsl_sf_result*))
	   (double gsl_sf_airy_zero_Bi_deriv (int))
	   (int gsl_sf_bessel_J0_e (double gsl_sf_result*))
	   (double gsl_sf_bessel_J0 (double))
	   (int gsl_sf_bessel_J1_e (double gsl_sf_result*))
	   (double gsl_sf_bessel_J1 (double))
	   (int gsl_sf_bessel_Jn_e (int double gsl_sf_result*))
	   (double gsl_sf_bessel_Jn (int double))
	   (int gsl_sf_bessel_Jn_array (int int double double*))
	   (int gsl_sf_bessel_Y0_e (double gsl_sf_result*))
	   (double gsl_sf_bessel_Y0 (double))
	   (int gsl_sf_bessel_Y1_e (double gsl_sf_result*))
	   (double gsl_sf_bessel_Y1 (double))
	   (int gsl_sf_bessel_Yn_e (int double gsl_sf_result*))
	   (double gsl_sf_bessel_Yn (int double))
	   (int gsl_sf_bessel_Yn_array (int int double double*))
	   (int gsl_sf_bessel_I0_e (double gsl_sf_result*))
	   (double gsl_sf_bessel_I0 (double))
	   (int gsl_sf_bessel_I1_e (double gsl_sf_result*))
	   (double gsl_sf_bessel_I1 (double))
	   (int gsl_sf_bessel_In_e (int double gsl_sf_result*))
	   (double gsl_sf_bessel_In (int double))
	   (int gsl_sf_bessel_In_array (int int double double*))
	   (int gsl_sf_bessel_I0_scaled_e (double gsl_sf_result*))
	   (double gsl_sf_bessel_I0_scaled (double))
	   (int gsl_sf_bessel_I1_scaled_e (double gsl_sf_result*))
	   (double gsl_sf_bessel_I1_scaled (double))
	   (int gsl_sf_bessel_In_scaled_e (int double gsl_sf_result*))
	   (double gsl_sf_bessel_In_scaled (int double))
	   (int gsl_sf_bessel_In_scaled_array (int int double double*))
	   (int gsl_sf_bessel_K0_e (double gsl_sf_result*))
	   (double gsl_sf_bessel_K0 (double))
	   (int gsl_sf_bessel_K1_e (double gsl_sf_result*))
	   (double gsl_sf_bessel_K1 (double))
	   (int gsl_sf_bessel_Kn_e (int double gsl_sf_result*))
	   (double gsl_sf_bessel_Kn (int double))
	   (int gsl_sf_bessel_Kn_array (int int double double*))
	   (int gsl_sf_bessel_K0_scaled_e (double gsl_sf_result*))
	   (double gsl_sf_bessel_K0_scaled (double))
	   (int gsl_sf_bessel_K1_scaled_e (double gsl_sf_result*) )
	   (double gsl_sf_bessel_K1_scaled (double))
	   (int gsl_sf_bessel_Kn_scaled_e (int double gsl_sf_result*))
	   (double gsl_sf_bessel_Kn_scaled (int double))
	   (int gsl_sf_bessel_Kn_scaled_array (int int double double*))
	   (int gsl_sf_bessel_j0_e (double gsl_sf_result*))
	   (double gsl_sf_bessel_j0 (double))
	   (int gsl_sf_bessel_j1_e (double gsl_sf_result*))
	   (double gsl_sf_bessel_j1 (double))
	   (int gsl_sf_bessel_j2_e (double gsl_sf_result*))
	   (double gsl_sf_bessel_j2 (double))
	   (int gsl_sf_bessel_jl_e (int double gsl_sf_result*))
	   (double gsl_sf_bessel_jl (int double))
	   (int gsl_sf_bessel_jl_array (int double double*))
	   (int gsl_sf_bessel_jl_steed_array (int double double*))
	   (int gsl_sf_bessel_y0_e (double gsl_sf_result*))
	   (double gsl_sf_bessel_y0 (double))
	   (int gsl_sf_bessel_y1_e (double gsl_sf_result*))
	   (double gsl_sf_bessel_y1 (double))
	   (int gsl_sf_bessel_y2_e (double gsl_sf_result*))
	   (double gsl_sf_bessel_y2 (double))
	   (int gsl_sf_bessel_yl_e (int double gsl_sf_result*))
	   (double gsl_sf_bessel_yl (int double))
	   (int gsl_sf_bessel_yl_array (int double double*))
	   (int gsl_sf_bessel_i0_scaled_e (double gsl_sf_result*))
	   (double gsl_sf_bessel_i0_scaled (double))
	   (int gsl_sf_bessel_i1_scaled_e (double gsl_sf_result*))
	   (double gsl_sf_bessel_i1_scaled (double))
	   (int gsl_sf_bessel_i2_scaled_e (double gsl_sf_result*))
	   (double gsl_sf_bessel_i2_scaled (double))
	   (int gsl_sf_bessel_il_scaled_e (int double gsl_sf_result*))
	   (double gsl_sf_bessel_il_scaled (int double))
	   (int gsl_sf_bessel_il_scaled_array (int double double*))
	   (int gsl_sf_bessel_k0_scaled_e (double gsl_sf_result*))
	   (double gsl_sf_bessel_k0_scaled (double))
	   (int gsl_sf_bessel_k1_scaled_e (double gsl_sf_result*))
	   (double gsl_sf_bessel_k1_scaled (double))
	   (int gsl_sf_bessel_k2_scaled_e (double gsl_sf_result*))
	   (double gsl_sf_bessel_k2_scaled (double))
	   (int gsl_sf_bessel_kl_scaled_e (int double gsl_sf_result*))
	   (double gsl_sf_bessel_kl_scaled (int double))
	   (int gsl_sf_bessel_kl_scaled_array (int double double*))
	   (int gsl_sf_bessel_Jnu_e (double double gsl_sf_result*))
	   (double gsl_sf_bessel_Jnu (double double))
	   (int gsl_sf_bessel_Ynu_e (double double gsl_sf_result*))
	   (double gsl_sf_bessel_Ynu (double double))
	   (int gsl_sf_bessel_sequence_Jnu_e (double int size_t double*))
	   (int gsl_sf_bessel_Inu_scaled_e (double double gsl_sf_result*))
	   (double gsl_sf_bessel_Inu_scaled (double double))
	   (int gsl_sf_bessel_Inu_e (double double gsl_sf_result*))
	   (double gsl_sf_bessel_Inu (double double))
	   (int gsl_sf_bessel_Knu_scaled_e (double double gsl_sf_result*))
	   (double gsl_sf_bessel_Knu_scaled (double double))
	   (reader-cond ((>= gsl-version 1.15) (int gsl_sf_bessel_Knu_scaled_e10_e (double double gsl_sf_result_e10*))))
	   (int gsl_sf_bessel_Knu_e (double double gsl_sf_result*))
	   (double gsl_sf_bessel_Knu (double double))
	   (int gsl_sf_bessel_lnKnu_e (double double gsl_sf_result*))
	   (double gsl_sf_bessel_lnKnu (double double))
	   (int gsl_sf_bessel_zero_J0_e (int gsl_sf_result*))
	   (double gsl_sf_bessel_zero_J0 (int))
	   (int gsl_sf_bessel_zero_J1_e (int gsl_sf_result*))
	   (double gsl_sf_bessel_zero_J1 (int))
	   (int gsl_sf_bessel_zero_Jnu_e (double int gsl_sf_result*))
	   (double gsl_sf_bessel_zero_Jnu (double int))
	   (int gsl_sf_clausen_e (double gsl_sf_result*))
	   (double gsl_sf_clausen (double))
	   (int gsl_sf_hydrogenicR_1_e (double double gsl_sf_result*))
	   (double gsl_sf_hydrogenicR_1 (double double))
	   (int gsl_sf_hydrogenicR_e (int int double double gsl_sf_result*))
	   (double gsl_sf_hydrogenicR (int int double double))
	   (int gsl_sf_coulomb_wave_FG_e (double double double int gsl_sf_result* gsl_sf_result* gsl_sf_result* gsl_sf_result* double* double*))
	   (int gsl_sf_coulomb_wave_F_array (double int double double double* double*))
	   (int gsl_sf_coulomb_wave_FG_array (double int double double double* double* double* double*))
	   (int gsl_sf_coulomb_wave_FGp_array (double int double double double* double* double* double* double* double*))
	   (int gsl_sf_coulomb_wave_sphF_array (double int double double double* double*))
	   (int gsl_sf_coulomb_CL_e (double double gsl_sf_result*))
	   (int gsl_sf_coulomb_CL_array (double int double double*))
	   (int gsl_sf_coupling_3j_e (int int int int int int gsl_sf_result*))
	   (double gsl_sf_coupling_3j (int int int int int int))
	   (int gsl_sf_coupling_6j_e (int int int int int int gsl_sf_result*))
	   (double gsl_sf_coupling_6j (int int int int int int))
	   (int gsl_sf_coupling_RacahW_e (int int int int int int gsl_sf_result*))
	   (double gsl_sf_coupling_RacahW (int int int int int int))
	   (int gsl_sf_coupling_9j_e (int int int int int int int int int gsl_sf_result*))
	   (double gsl_sf_coupling_9j (int int int int int int int int int))
	   (int gsl_sf_dawson_e (double gsl_sf_result*))
	   (double gsl_sf_dawson (double))
	   (int gsl_sf_debye_1_e (double gsl_sf_result*))
	   (double gsl_sf_debye_1 (double))
	   (int gsl_sf_debye_2_e (double gsl_sf_result*))
	   (double gsl_sf_debye_2 (double))
	   (int gsl_sf_debye_3_e (double gsl_sf_result*))
	   (double gsl_sf_debye_3 (double))
	   (int gsl_sf_debye_4_e (double gsl_sf_result*))
	   (double gsl_sf_debye_4 (double))
	   (int gsl_sf_debye_5_e (double gsl_sf_result*))
	   (double gsl_sf_debye_5 (double))
	   (int gsl_sf_debye_6_e (double gsl_sf_result*))
	   (double gsl_sf_debye_6 (double))
	   (int gsl_sf_dilog_e (double gsl_sf_result*))
	   (double gsl_sf_dilog (double))
	   (int gsl_sf_complex_dilog_xy_e (double double gsl_sf_result* gsl_sf_result*))
	   (int gsl_sf_complex_dilog_e (double double gsl_sf_result* gsl_sf_result*))
	   (int gsl_sf_complex_spence_xy_e (double double gsl_sf_result* gsl_sf_result*))
	   (int gsl_sf_multiply_e (double double gsl_sf_result*))
	   (double gsl_sf_multiply (double double))
	   (int gsl_sf_multiply_err_e (double double double double gsl_sf_result*))
	   (int gsl_sf_ellint_Kcomp_e (double int gsl_sf_result*))
	   (double gsl_sf_ellint_Kcomp (double int))
	   (int gsl_sf_ellint_Ecomp_e (double int gsl_sf_result*))
	   (double gsl_sf_ellint_Ecomp (double int))
	   (int gsl_sf_ellint_Pcomp_e (double double int gsl_sf_result*))
	   (double gsl_sf_ellint_Pcomp (double double int))
	   (int gsl_sf_ellint_Dcomp_e (double int gsl_sf_result*))
	   (double gsl_sf_ellint_Dcomp (double int))
	   (int gsl_sf_ellint_F_e (double double int gsl_sf_result*))
	   (double gsl_sf_ellint_F (double double int))
	   (int gsl_sf_ellint_E_e (double double int gsl_sf_result*))
	   (double gsl_sf_ellint_E (double double int))
	   (int gsl_sf_ellint_P_e (double double double int gsl_sf_result*))
	   (double gsl_sf_ellint_P (double double double int))
	   (int gsl_sf_ellint_D_e (double double double int gsl_sf_result*))
	   (double gsl_sf_ellint_D (double double double int))
	   (int gsl_sf_ellint_RC_e (double double int gsl_sf_result*))
	   (double gsl_sf_ellint_RC (double double int))
	   (int gsl_sf_ellint_RD_e (double double double int gsl_sf_result*))
	   (double gsl_sf_ellint_RD (double double double int))
	   (int gsl_sf_ellint_RF_e (double double double int gsl_sf_result*))
	   (double gsl_sf_ellint_RF (double double double int))
	   (int gsl_sf_ellint_RJ_e (double double double double int gsl_sf_result*))
	   (double gsl_sf_ellint_RJ (double double double double int))
	   (int gsl_sf_elljac_e (double double double* double* double*)) ; these are double by reference
	   (int gsl_sf_erfc_e (double gsl_sf_result*))
	   (double gsl_sf_erfc (double))
	   (int gsl_sf_log_erfc_e (double gsl_sf_result*))
	   (double gsl_sf_log_erfc (double))
	   (int gsl_sf_erf_e (double gsl_sf_result*))
	   (double gsl_sf_erf (double))
	   (int gsl_sf_erf_Z_e (double gsl_sf_result*))
	   (int gsl_sf_erf_Q_e (double gsl_sf_result*))
	   (double gsl_sf_erf_Z (double))
	   (double gsl_sf_erf_Q (double))
	   (int gsl_sf_hazard_e (double gsl_sf_result*))
	   (double gsl_sf_hazard (double))
	   (int gsl_sf_exp_e (double gsl_sf_result*))
	   (double gsl_sf_exp (double))
	   (int gsl_sf_exp_e10_e (double gsl_sf_result_e10*))
	   (int gsl_sf_exp_mult_e (double double gsl_sf_result*))
	   (double gsl_sf_exp_mult (double double))
	   (int gsl_sf_exp_mult_e10_e (double double gsl_sf_result_e10*))
	   (int gsl_sf_expm1_e (double gsl_sf_result*))
	   (double gsl_sf_expm1 (double))
	   (int gsl_sf_exprel_e (double gsl_sf_result*))
	   (double gsl_sf_exprel (double))
	   (int gsl_sf_exprel_2_e (double gsl_sf_result*))
	   (double gsl_sf_exprel_2 (double))
	   (int gsl_sf_exprel_n_e (int double gsl_sf_result*))
	   (double gsl_sf_exprel_n (int double))
	   (int gsl_sf_exprel_n_CF_e (double double gsl_sf_result*))
	   (int gsl_sf_exp_err_e (double double gsl_sf_result*))
	   (int gsl_sf_exp_err_e10_e (double double gsl_sf_result_e10*))
	   (int gsl_sf_exp_mult_err_e (double double double double gsl_sf_result*))
	   (int gsl_sf_exp_mult_err_e10_e (double double double double gsl_sf_result_e10*))
	   (int gsl_sf_expint_E1_e (double gsl_sf_result*))
	   (double gsl_sf_expint_E1 (double))
	   (int gsl_sf_expint_E2_e (double gsl_sf_result*))
	   (double gsl_sf_expint_E2 (double))
	   (int gsl_sf_expint_En_e (int double gsl_sf_result*))
	   (double gsl_sf_expint_En (int double))
	   (int gsl_sf_expint_E1_scaled_e (double gsl_sf_result*))
	   (double gsl_sf_expint_E1_scaled (double))
	   (int gsl_sf_expint_E2_scaled_e (double gsl_sf_result*))
	   (double gsl_sf_expint_E2_scaled (double))
	   (int gsl_sf_expint_En_scaled_e (int double gsl_sf_result*))
	   (double gsl_sf_expint_En_scaled (int double))
	   (int gsl_sf_expint_Ei_e (double gsl_sf_result*))
	   (double gsl_sf_expint_Ei (double))
	   (int gsl_sf_expint_Ei_scaled_e (double gsl_sf_result*))
	   (double gsl_sf_expint_Ei_scaled (double))
	   (int gsl_sf_Shi_e (double gsl_sf_result*))
	   (double gsl_sf_Shi (double))
	   (int gsl_sf_Chi_e (double gsl_sf_result*))
	   (double gsl_sf_Chi (double))
	   (int gsl_sf_expint_3_e (double gsl_sf_result*))
	   (double gsl_sf_expint_3 (double))
	   (int gsl_sf_Si_e (double gsl_sf_result*))
	   (double gsl_sf_Si (double))
	   (int gsl_sf_Ci_e (double gsl_sf_result*))
	   (double gsl_sf_Ci (double))
	   (int gsl_sf_atanint_e (double gsl_sf_result*))
	   (double gsl_sf_atanint (double))
	   (int gsl_sf_fermi_dirac_m1_e (double gsl_sf_result*))
	   (double gsl_sf_fermi_dirac_m1 (double))
	   (int gsl_sf_fermi_dirac_0_e (double gsl_sf_result*))
	   (double gsl_sf_fermi_dirac_0 (double))
	   (int gsl_sf_fermi_dirac_1_e (double gsl_sf_result*))
	   (double gsl_sf_fermi_dirac_1 (double))
	   (int gsl_sf_fermi_dirac_2_e (double gsl_sf_result*))
	   (double gsl_sf_fermi_dirac_2 (double))
	   (int gsl_sf_fermi_dirac_int_e (int double gsl_sf_result*))
	   (double gsl_sf_fermi_dirac_int (int double))
	   (int gsl_sf_fermi_dirac_mhalf_e (double gsl_sf_result*))
	   (double gsl_sf_fermi_dirac_mhalf (double))
	   (int gsl_sf_fermi_dirac_half_e (double gsl_sf_result*))
	   (double gsl_sf_fermi_dirac_half (double))
	   (int gsl_sf_fermi_dirac_3half_e (double gsl_sf_result*))
	   (double gsl_sf_fermi_dirac_3half (double))
	   (int gsl_sf_fermi_dirac_inc_0_e (double double gsl_sf_result*))
	   (double gsl_sf_fermi_dirac_inc_0 (double double))
	   (int gsl_sf_lngamma_e (double gsl_sf_result*))
	   (double gsl_sf_lngamma (double))
	   (int gsl_sf_lngamma_sgn_e (double gsl_sf_result* double*))
	   (int gsl_sf_gamma_e (double gsl_sf_result*))
	   (double gsl_sf_gamma (double))
	   (int gsl_sf_gammastar_e (double gsl_sf_result*))
	   (double gsl_sf_gammastar (double))
	   (int gsl_sf_gammainv_e (double gsl_sf_result*))
	   (double gsl_sf_gammainv (double))
	   (int gsl_sf_lngamma_complex_e (double double gsl_sf_result* gsl_sf_result*))
	   (int gsl_sf_taylorcoeff_e (int double gsl_sf_result*))
	   (double gsl_sf_taylorcoeff (int double))
	   (int gsl_sf_fact_e (int gsl_sf_result*))
	   (double gsl_sf_fact (int))
	   (int gsl_sf_doublefact_e (int gsl_sf_result*))
	   (double gsl_sf_doublefact (int))
	   (int gsl_sf_lnfact_e (int gsl_sf_result*))
	   (double gsl_sf_lnfact (int))
	   (int gsl_sf_lndoublefact_e (int gsl_sf_result*))
	   (double gsl_sf_lndoublefact (int))
	   (int gsl_sf_lnchoose_e (int int gsl_sf_result*))
	   (double gsl_sf_lnchoose (int int))
	   (int gsl_sf_choose_e (int int gsl_sf_result*))
	   (double gsl_sf_choose (int int))
	   (int gsl_sf_lnpoch_e (double double gsl_sf_result*))
	   (double gsl_sf_lnpoch (double double))
	   (int gsl_sf_lnpoch_sgn_e (double double gsl_sf_result* double*))
	   (int gsl_sf_poch_e (double double gsl_sf_result*))
	   (double gsl_sf_poch (double double))
	   (int gsl_sf_pochrel_e (double double gsl_sf_result*))
	   (double gsl_sf_pochrel (double double))
	   (int gsl_sf_gamma_inc_Q_e (double double gsl_sf_result*))
	   (double gsl_sf_gamma_inc_Q (double double))
	   (int gsl_sf_gamma_inc_P_e (double double gsl_sf_result*))
	   (double gsl_sf_gamma_inc_P (double double))
	   (int gsl_sf_gamma_inc_e (double double gsl_sf_result*))
	   (double gsl_sf_gamma_inc (double double))
	   (int gsl_sf_lnbeta_e (double double gsl_sf_result*))
	   (double gsl_sf_lnbeta (double double))
	   (int gsl_sf_lnbeta_sgn_e (double double gsl_sf_result* double*))
	   (int gsl_sf_beta_e (double double gsl_sf_result*))
	   (double gsl_sf_beta (double double))
	   (int gsl_sf_beta_inc_e (double double double gsl_sf_result*))
	   (double gsl_sf_beta_inc (double double double))
	   (int gsl_sf_gegenpoly_1_e (double double gsl_sf_result*))
	   (int gsl_sf_gegenpoly_2_e (double double gsl_sf_result*))
	   (int gsl_sf_gegenpoly_3_e (double double gsl_sf_result*))
	   (double gsl_sf_gegenpoly_1 (double double))
	   (double gsl_sf_gegenpoly_2 (double double))
	   (double gsl_sf_gegenpoly_3 (double double))
	   (int gsl_sf_gegenpoly_n_e (int double double gsl_sf_result*))
	   (double gsl_sf_gegenpoly_n (int double double))
	   (int gsl_sf_gegenpoly_array (int double double double*))
	   (int gsl_sf_hyperg_0F1_e (double double gsl_sf_result*))
	   (double gsl_sf_hyperg_0F1 (double double))
	   (int gsl_sf_hyperg_1F1_int_e (int int double gsl_sf_result*))
	   (double gsl_sf_hyperg_1F1_int (int int double))
	   (int gsl_sf_hyperg_1F1_e (double double double gsl_sf_result*))
	   (double gsl_sf_hyperg_1F1 (double double double))
	   (int gsl_sf_hyperg_U_int_e (int int double gsl_sf_result*))
	   (double gsl_sf_hyperg_U_int (int int double))
	   (int gsl_sf_hyperg_U_int_e10_e (int int double gsl_sf_result_e10*))
	   (int gsl_sf_hyperg_U_e (double double double gsl_sf_result*))
	   (double gsl_sf_hyperg_U (double double double))
	   (int gsl_sf_hyperg_U_e10_e (double double double gsl_sf_result_e10*))
	   (int gsl_sf_hyperg_2F1_e (double double double double gsl_sf_result*))
	   (double gsl_sf_hyperg_2F1 (double double double double))
	   (int gsl_sf_hyperg_2F1_conj_e (double double double double gsl_sf_result*))
	   (double gsl_sf_hyperg_2F1_conj (double double double double))
	   (int gsl_sf_hyperg_2F1_renorm_e (double double double double gsl_sf_result*))
	   (double gsl_sf_hyperg_2F1_renorm (double double double double))
	   (int gsl_sf_hyperg_2F1_conj_renorm_e (double double double double gsl_sf_result*))
	   (double gsl_sf_hyperg_2F1_conj_renorm (double double double double))
	   (int gsl_sf_hyperg_2F0_e (double double double gsl_sf_result*))
	   (double gsl_sf_hyperg_2F0 (double double double))
	   (int gsl_sf_laguerre_1_e (double double gsl_sf_result*))
	   (int gsl_sf_laguerre_2_e (double double gsl_sf_result*))
	   (int gsl_sf_laguerre_3_e (double double gsl_sf_result*))
	   (double gsl_sf_laguerre_1 (double double))
	   (double gsl_sf_laguerre_2 (double double))
	   (double gsl_sf_laguerre_3 (double double))
	   (int gsl_sf_laguerre_n_e (int double double gsl_sf_result*))
	   (double gsl_sf_laguerre_n (int double double))
	   (int gsl_sf_lambert_W0_e (double gsl_sf_result*))
	   (double gsl_sf_lambert_W0 (double))
	   (int gsl_sf_lambert_Wm1_e (double gsl_sf_result*))
	   (double gsl_sf_lambert_Wm1 (double))
	   (int gsl_sf_legendre_Pl_e (int double gsl_sf_result*))
	   (double gsl_sf_legendre_Pl (int double))
	   (int gsl_sf_legendre_Pl_array (int double double*))
	   (int gsl_sf_legendre_Pl_deriv_array ( int double double* double*))
	   (int gsl_sf_legendre_P1_e (double gsl_sf_result*))
	   (int gsl_sf_legendre_P2_e (double gsl_sf_result*))
	   (int gsl_sf_legendre_P3_e (double gsl_sf_result*))
	   (double gsl_sf_legendre_P1 (double))
	   (double gsl_sf_legendre_P2 (double))
	   (double gsl_sf_legendre_P3 (double))
	   (int gsl_sf_legendre_Q0_e (double gsl_sf_result*))
	   (double gsl_sf_legendre_Q0 (double))
	   (int gsl_sf_legendre_Q1_e (double gsl_sf_result*))
	   (double gsl_sf_legendre_Q1 (double))
	   (int gsl_sf_legendre_Ql_e (int double gsl_sf_result*))
	   (double gsl_sf_legendre_Ql (int double))
	   (int gsl_sf_legendre_Plm_e (int int double gsl_sf_result*))
	   (double gsl_sf_legendre_Plm (int int double))
	   (int gsl_sf_legendre_Plm_array (int int double double*))
	   (int gsl_sf_legendre_Plm_deriv_array (int int double double* double*))
	   (int gsl_sf_legendre_sphPlm_e (int int double gsl_sf_result*))
	   (double gsl_sf_legendre_sphPlm (int int double))
	   (int gsl_sf_legendre_sphPlm_array (int int double double*))
	   (int gsl_sf_legendre_sphPlm_deriv_array (int int double double* double*))
	   (int gsl_sf_legendre_array_size (int int))
	   (int gsl_sf_conicalP_half_e (double double gsl_sf_result*))
	   (double gsl_sf_conicalP_half (double double))
	   (int gsl_sf_conicalP_mhalf_e (double double gsl_sf_result*))
	   (double gsl_sf_conicalP_mhalf (double double))
	   (int gsl_sf_conicalP_0_e (double double gsl_sf_result*))
	   (double gsl_sf_conicalP_0 (double double))
	   (int gsl_sf_conicalP_1_e (double double gsl_sf_result*))
	   (double gsl_sf_conicalP_1 (double double))
	   (int gsl_sf_conicalP_sph_reg_e (int double double gsl_sf_result*))
	   (double gsl_sf_conicalP_sph_reg (int double double))
	   (int gsl_sf_conicalP_cyl_reg_e (int double double gsl_sf_result*))
	   (double gsl_sf_conicalP_cyl_reg (int double double))
	   (int gsl_sf_legendre_H3d_0_e (double double gsl_sf_result*))
	   (double gsl_sf_legendre_H3d_0 (double double))
	   (int gsl_sf_legendre_H3d_1_e (double double gsl_sf_result*))
	   (double gsl_sf_legendre_H3d_1 (double double))
	   (int gsl_sf_legendre_H3d_e (int double double gsl_sf_result*))
	   (double gsl_sf_legendre_H3d (int double double))
	   (int gsl_sf_legendre_H3d_array (int double double double*))
	   (int gsl_sf_log_e (double gsl_sf_result*))
	   (double gsl_sf_log (double))
	   (int gsl_sf_log_abs_e (double gsl_sf_result*))
	   (double gsl_sf_log_abs (double))
	   (int gsl_sf_complex_log_e (double double gsl_sf_result* gsl_sf_result*))
	   (int gsl_sf_log_1plusx_e (double gsl_sf_result*))
	   (double gsl_sf_log_1plusx (double))
	   (int gsl_sf_log_1plusx_mx_e (double gsl_sf_result*))
	   (double gsl_sf_log_1plusx_mx (double))
	   (int gsl_sf_mathieu_a_array (int int double gsl_sf_mathieu_workspace* double*))
	   (int gsl_sf_mathieu_b_array (int int double gsl_sf_mathieu_workspace* double*))
	   (int gsl_sf_mathieu_a (int double gsl_sf_result*))
	   (int gsl_sf_mathieu_b (int double gsl_sf_result*))
	   (int gsl_sf_mathieu_a_coeff (int double double double*))
	   (int gsl_sf_mathieu_b_coeff (int double double double*))
	   (gsl_sf_mathieu_workspace* gsl_sf_mathieu_alloc (size_t double))
	   (void gsl_sf_mathieu_free (gsl_sf_mathieu_workspace*))
	   (int gsl_sf_mathieu_ce (int double double gsl_sf_result*))
	   (int gsl_sf_mathieu_se (int double double gsl_sf_result*))
	   (int gsl_sf_mathieu_ce_array (int int double double gsl_sf_mathieu_workspace* double*))
	   (int gsl_sf_mathieu_se_array (int int double double gsl_sf_mathieu_workspace* double*))
	   (int gsl_sf_mathieu_Mc (int int double double gsl_sf_result*))
	   (int gsl_sf_mathieu_Ms (int int double double gsl_sf_result*))
	   (int gsl_sf_mathieu_Mc_array (int int int double double gsl_sf_mathieu_workspace* double*))
	   (int gsl_sf_mathieu_Ms_array (int int int double double gsl_sf_mathieu_workspace* double*))
	   (int gsl_sf_pow_int_e (double int gsl_sf_result*))
	   (double gsl_sf_pow_int (double int))
	   (int gsl_sf_psi_int_e (int gsl_sf_result*))
	   (double gsl_sf_psi_int (int))
	   (int gsl_sf_psi_e (double gsl_sf_result*))
	   (double gsl_sf_psi (double))
	   (int gsl_sf_psi_1piy_e (double gsl_sf_result*))
	   (double gsl_sf_psi_1piy (double))
	   (int gsl_sf_complex_psi_e (double double gsl_sf_result* gsl_sf_result*))
	   (int gsl_sf_psi_1_int_e (int gsl_sf_result*))
	   (double gsl_sf_psi_1_int (int))
	   (int gsl_sf_psi_1_e (double gsl_sf_result*))
	   (double gsl_sf_psi_1 (double))
	   (int gsl_sf_psi_n_e (int double gsl_sf_result*))
	   (double gsl_sf_psi_n (int double))
	   (int gsl_sf_result_smash_e (gsl_sf_result_e10* gsl_sf_result*))
	   (int gsl_sf_synchrotron_1_e (double gsl_sf_result*))
	   (double gsl_sf_synchrotron_1 (double))
	   (int gsl_sf_synchrotron_2_e (double gsl_sf_result*))
	   (double gsl_sf_synchrotron_2 (double))
	   (int gsl_sf_transport_2_e (double gsl_sf_result*))
	   (double gsl_sf_transport_2 (double))
	   (int gsl_sf_transport_3_e (double gsl_sf_result*))
	   (double gsl_sf_transport_3 (double))
	   (int gsl_sf_transport_4_e (double gsl_sf_result*))
	   (double gsl_sf_transport_4 (double))
	   (int gsl_sf_transport_5_e (double gsl_sf_result*))
	   (double gsl_sf_transport_5 (double))
	   (int gsl_sf_sin_e (double gsl_sf_result*))
	   (double gsl_sf_sin (double))
	   (int gsl_sf_cos_e (double gsl_sf_result*))
	   (double gsl_sf_cos (double))
	   (int gsl_sf_hypot_e (double double gsl_sf_result*))
	   (double gsl_sf_hypot (double double))
	   (int gsl_sf_complex_sin_e (double double gsl_sf_result* gsl_sf_result*))
	   (int gsl_sf_complex_cos_e (double double gsl_sf_result* gsl_sf_result*))
	   (int gsl_sf_complex_logsin_e (double double gsl_sf_result* gsl_sf_result*))
	   (int gsl_sf_sinc_e (double gsl_sf_result*))
	   (double gsl_sf_sinc (double))
	   (int gsl_sf_lnsinh_e (double gsl_sf_result*))
	   (double gsl_sf_lnsinh (double))
	   (int gsl_sf_lncosh_e (double gsl_sf_result*))
	   (double gsl_sf_lncosh (double))
	   (int gsl_sf_polar_to_rect (double double gsl_sf_result* gsl_sf_result*))
	   (int gsl_sf_rect_to_polar (double double gsl_sf_result* gsl_sf_result*))
	   (int gsl_sf_sin_err_e (double double gsl_sf_result*))
	   (int gsl_sf_cos_err_e (double double gsl_sf_result*))
	   (int gsl_sf_angle_restrict_symm_e (double*))
	   (double gsl_sf_angle_restrict_symm (double))
	   (int gsl_sf_angle_restrict_pos_e (double*))
	   (double gsl_sf_angle_restrict_pos (double))
	   (int gsl_sf_angle_restrict_symm_err_e (double gsl_sf_result*))
	   (int gsl_sf_angle_restrict_pos_err_e (double gsl_sf_result*))
	   (int gsl_sf_zeta_int_e (int gsl_sf_result*))
	   (double gsl_sf_zeta_int (int))
	   (int gsl_sf_zeta_e (double gsl_sf_result*))
	   (double gsl_sf_zeta (double))
	   (int gsl_sf_zetam1_e (double gsl_sf_result*))
	   (double gsl_sf_zetam1 (double))
	   (int gsl_sf_zetam1_int_e (int gsl_sf_result*))
	   (double gsl_sf_zetam1_int (int))
	   (int gsl_sf_hzeta_e (double double gsl_sf_result*))
	   (double gsl_sf_hzeta (double double))
	   (int gsl_sf_eta_int_e (int gsl_sf_result*))
	   (double gsl_sf_eta_int (int))
	   (int gsl_sf_eta_e (double gsl_sf_result*))
	   (double gsl_sf_eta (double))
	   
	   (in-C "static s7_pointer g_gsl_sf_result_make(s7_scheme *sc, s7_pointer args)
                  {
                    return(s7_make_c_pointer(sc, (void *)calloc(1, sizeof(gsl_sf_result))));
                  }
                  static s7_pointer g_gsl_sf_result_val(s7_scheme *sc, s7_pointer args)
                  {
                    return(s7_make_real(sc, ((gsl_sf_result *)s7_c_pointer(s7_car(args)))->val));
                  }
                  static s7_pointer g_gsl_sf_result_err(s7_scheme *sc, s7_pointer args)
                  {
                    return(s7_make_real(sc, ((gsl_sf_result *)s7_c_pointer(s7_car(args)))->err));
                  }
                  static s7_pointer g_gsl_sf_result_e10_make(s7_scheme *sc, s7_pointer args)
                  {
                    return(s7_make_c_pointer(sc, (void *)calloc(1, sizeof(gsl_sf_result_e10))));
                  }
                  static s7_pointer g_to_doubles(s7_scheme *sc, s7_pointer args)
                  {
                    if (s7_is_vector(s7_car(args)))
                      return(s7_make_c_pointer(sc, (void *)s7_float_vector_elements(s7_car(args))));
                    return(s7_car(args));
                  }
                  ")
	   
	   (C-function ("gsl_sf_result.make" g_gsl_sf_result_make "" 0))
	   (C-function ("gsl_sf_result_e10.make" g_gsl_sf_result_e10_make "" 0))
	   (C-function ("gsl_sf_result.val" g_gsl_sf_result_val "" 1))
	   (C-function ("gsl_sf_result.err" g_gsl_sf_result_err "" 1))
	   (C-function ("double*" g_to_doubles "" 1))
	   
	   (double gsl_log1p (double))
	   (double gsl_expm1 (double))
	   (double gsl_hypot (double double))
	   (double gsl_hypot3 (double double double))
	   (double gsl_acosh (double))
	   (double gsl_asinh (double))
	   (double gsl_atanh (double))
	   (int gsl_isnan (double))
	   (int gsl_isinf (double))
	   (int gsl_finite (double))
	   (double gsl_nan (void))
	   (double gsl_posinf (void))
	   (double gsl_neginf (void))
	   (double gsl_fdiv (double double))
	   (double gsl_coerce_double (double))
	   (double gsl_ldexp (double int))
	   
	   (in-C "static s7_pointer g_gsl_frexp(s7_scheme *sc, s7_pointer args)
                  {
                    int e = 0;
                    double res;
                    res = gsl_frexp(s7_real(s7_car(args)), &e);
                    return(s7_list(sc, 2, s7_make_real(sc, res), s7_make_integer(sc, e)));
                  }
                  ")
	   (C-function ("gsl_frexp" g_gsl_frexp "" 1))
	   
	   (int gsl_fcmp (double double double))
	   (double gsl_pow_2 (double))
	   (double gsl_pow_3 (double))
	   (double gsl_pow_4 (double))
	   (double gsl_pow_5 (double))
	   (double gsl_pow_6 (double))
	   (double gsl_pow_7 (double))
	   (double gsl_pow_8 (double))
	   (double gsl_pow_9 (double))
	   (double gsl_pow_int (double int))
	   
	   ;; gsl_cdf
	   (double gsl_cdf_ugaussian_P (double))
	   (double gsl_cdf_ugaussian_Q (double))
	   (double gsl_cdf_ugaussian_Pinv (double))
	   (double gsl_cdf_ugaussian_Qinv (double))
	   (double gsl_cdf_gaussian_P (double double))
	   (double gsl_cdf_gaussian_Q (double double))
	   (double gsl_cdf_gaussian_Pinv (double double))
	   (double gsl_cdf_gaussian_Qinv (double double))
	   (double gsl_cdf_gamma_P (double double double))
	   (double gsl_cdf_gamma_Q (double double double))
	   (double gsl_cdf_gamma_Pinv (double double double))
	   (double gsl_cdf_gamma_Qinv (double double double))
	   (double gsl_cdf_cauchy_P (double double))
	   (double gsl_cdf_cauchy_Q (double double))
	   (double gsl_cdf_cauchy_Pinv (double double))
	   (double gsl_cdf_cauchy_Qinv (double double))
	   (double gsl_cdf_laplace_P (double double))
	   (double gsl_cdf_laplace_Q (double double))
	   (double gsl_cdf_laplace_Pinv (double double))
	   (double gsl_cdf_laplace_Qinv (double double))
	   (double gsl_cdf_rayleigh_P (double double))
	   (double gsl_cdf_rayleigh_Q (double double))
	   (double gsl_cdf_rayleigh_Pinv (double double))
	   (double gsl_cdf_rayleigh_Qinv (double double))
	   (double gsl_cdf_chisq_P (double double))
	   (double gsl_cdf_chisq_Q (double double))
	   (double gsl_cdf_chisq_Pinv (double double))
	   (double gsl_cdf_chisq_Qinv (double double))
	   (double gsl_cdf_exponential_P (double double))
	   (double gsl_cdf_exponential_Q (double double))
	   (double gsl_cdf_exponential_Pinv (double double))
	   (double gsl_cdf_exponential_Qinv (double double))
	   (double gsl_cdf_exppow_P (double double double))
	   (double gsl_cdf_exppow_Q (double double double))
	   (double gsl_cdf_tdist_P (double double))
	   (double gsl_cdf_tdist_Q (double double))
	   (double gsl_cdf_tdist_Pinv (double double))
	   (double gsl_cdf_tdist_Qinv (double double))
	   (double gsl_cdf_fdist_P (double double double))
	   (double gsl_cdf_fdist_Q (double double double))
	   (double gsl_cdf_fdist_Pinv (double double double))
	   (double gsl_cdf_fdist_Qinv (double double double))
	   (double gsl_cdf_beta_P (double double double))
	   (double gsl_cdf_beta_Q (double double double))
	   (double gsl_cdf_beta_Pinv (double double double))
	   (double gsl_cdf_beta_Qinv (double double double))
	   (double gsl_cdf_flat_P (double double double))
	   (double gsl_cdf_flat_Q (double double double))
	   (double gsl_cdf_flat_Pinv (double double double))
	   (double gsl_cdf_flat_Qinv (double double double))
	   (double gsl_cdf_lognormal_P (double double double))
	   (double gsl_cdf_lognormal_Q (double double double))
	   (double gsl_cdf_lognormal_Pinv (double double double))
	   (double gsl_cdf_lognormal_Qinv (double double double))
	   (double gsl_cdf_gumbel1_P (double double double))
	   (double gsl_cdf_gumbel1_Q (double double double))
	   (double gsl_cdf_gumbel1_Pinv (double double double))
	   (double gsl_cdf_gumbel1_Qinv (double double double))
	   (double gsl_cdf_gumbel2_P (double double double))
	   (double gsl_cdf_gumbel2_Q (double double double))
	   (double gsl_cdf_gumbel2_Pinv (double double double))
	   (double gsl_cdf_gumbel2_Qinv (double double double))
	   (double gsl_cdf_weibull_P (double double double))
	   (double gsl_cdf_weibull_Q (double double double))
	   (double gsl_cdf_weibull_Pinv (double double double))
	   (double gsl_cdf_weibull_Qinv (double double double))
	   (double gsl_cdf_pareto_P (double double double))
	   (double gsl_cdf_pareto_Q (double double double))
	   (double gsl_cdf_pareto_Pinv (double double double))
	   (double gsl_cdf_pareto_Qinv (double double double))
	   (double gsl_cdf_logistic_P (double double))
	   (double gsl_cdf_logistic_Q (double double))
	   (double gsl_cdf_logistic_Pinv (double double))
	   (double gsl_cdf_logistic_Qinv (double double))
	   (double gsl_cdf_binomial_P (int double int))
	   (double gsl_cdf_binomial_Q (int double int))
	   (double gsl_cdf_poisson_P (int double))
	   (double gsl_cdf_poisson_Q (int double))
	   (double gsl_cdf_geometric_P (int double))
	   (double gsl_cdf_geometric_Q (int double))
	   (double gsl_cdf_negative_binomial_P (int double double))
	   (double gsl_cdf_negative_binomial_Q (int double double))
	   (double gsl_cdf_pascal_P (int double int))
	   (double gsl_cdf_pascal_Q (int double int))
	   (double gsl_cdf_hypergeometric_P (int int int int))
	   (double gsl_cdf_hypergeometric_Q (int int int int))
	   
	   
	   ;; gsl_dht
	   (gsl_dht* gsl_dht_alloc (size_t))
	   (gsl_dht* gsl_dht_new (size_t double double))
	   (int gsl_dht_init (gsl_dht* double double))
	   (double gsl_dht_x_sample (gsl_dht* int))
	   (double gsl_dht_k_sample (gsl_dht* int))
	   (void gsl_dht_free (gsl_dht*))
	   (int gsl_dht_apply (gsl_dht* double* double*))
	   
	   
	   ;; gsl_statistics
	   (double gsl_stats_mean (double* size_t size_t))
	   (double gsl_stats_variance (double* size_t size_t))
	   (double gsl_stats_sd (double* size_t size_t))
	   (double gsl_stats_variance_with_fixed_mean (double* size_t size_t double))
	   (double gsl_stats_sd_with_fixed_mean (double* size_t size_t double))
	   (double gsl_stats_tss (double* size_t size_t))
	   (double gsl_stats_tss_m (double* size_t size_t double))
	   (double gsl_stats_absdev (double* size_t size_t))
	   (double gsl_stats_skew (double* size_t size_t))
	   (double gsl_stats_kurtosis (double* size_t size_t))
	   (double gsl_stats_lag1_autocorrelation (double* size_t size_t))
	   (double gsl_stats_covariance (double* size_t double* size_t size_t))
	   (double gsl_stats_correlation (double* size_t double* size_t size_t))
	   (reader-cond ((>= gsl-version 1.16) (double gsl_stats_spearman (double* size_t double* size_t size_t double*))))
	   (double gsl_stats_variance_m (double* size_t size_t double))
	   (double gsl_stats_sd_m (double* size_t size_t double))
	   (double gsl_stats_absdev_m (double* size_t size_t double))
	   (double gsl_stats_skew_m_sd (double* size_t size_t double double))
	   (double gsl_stats_kurtosis_m_sd (double* size_t size_t double double))
	   (double gsl_stats_lag1_autocorrelation_m (double* size_t size_t double))
	   (double gsl_stats_covariance_m (double* size_t double* size_t size_t double double))
	   (double gsl_stats_wmean (double* size_t double* size_t size_t))
	   (double gsl_stats_wvariance (double* size_t double* size_t size_t))
	   (double gsl_stats_wsd (double* size_t double* size_t size_t))
	   (double gsl_stats_wvariance_with_fixed_mean (double* size_t double* size_t size_t double))
	   (double gsl_stats_wsd_with_fixed_mean (double* size_t double* size_t size_t double))
	   (double gsl_stats_wtss (double* size_t double* size_t size_t))
	   (double gsl_stats_wtss_m (double* size_t double* size_t size_t double))
	   (double gsl_stats_wabsdev (double* size_t double* size_t size_t))
	   (double gsl_stats_wskew (double* size_t double* size_t size_t))
	   (double gsl_stats_wkurtosis (double* size_t double* size_t size_t))
	   (double gsl_stats_wvariance_m (double* size_t double* size_t size_t double))
	   (double gsl_stats_wsd_m (double* size_t double* size_t size_t double))
	   (double gsl_stats_wabsdev_m (double* size_t double* size_t size_t double))
	   (double gsl_stats_wskew_m_sd (double* size_t double* size_t size_t double double))
	   (double gsl_stats_wkurtosis_m_sd (double* size_t double* size_t size_t double double))
	   (double gsl_stats_pvariance (double* size_t size_t double* size_t size_t))
	   (double gsl_stats_ttest (double* size_t size_t double* size_t size_t))
	   (double gsl_stats_max (double* size_t size_t))
	   (double gsl_stats_min (double* size_t size_t))
	   (void gsl_stats_minmax (double* double* double* size_t size_t))
	   (size_t gsl_stats_max_index (double* size_t size_t))
	   (size_t gsl_stats_min_index (double* size_t size_t))
	   (void gsl_stats_minmax_index (size_t* size_t* double* size_t size_t))
	   (double gsl_stats_median_from_sorted_data (double* size_t size_t))
	   (double gsl_stats_quantile_from_sorted_data (double* size_t size_t double))
	   
	   (c-pointer (gsl_interp_linear gsl_interp_polynomial gsl_interp_cspline gsl_interp_cspline_periodic gsl_interp_akima
		       gsl_interp_akima_periodic gsl_min_fminimizer_goldensection gsl_min_fminimizer_brent gsl_min_fminimizer_quad_golden
		       gsl_multimin_fminimizer_nmsimplex gsl_multimin_fminimizer_nmsimplex2 gsl_multimin_fminimizer_nmsimplex2rand
		       gsl_multiroot_fsolver_dnewton gsl_multiroot_fsolver_broyden gsl_multiroot_fsolver_hybrid gsl_multiroot_fsolver_hybrids
		       gsl_prec_eps gsl_prec_sqrt_eps
		       gsl_prec_root3_eps gsl_prec_root4_eps gsl_prec_root5_eps gsl_prec_root6_eps gsl_root_fsolver_bisection gsl_root_fsolver_brent
		       gsl_root_fsolver_falsepos gsl_version
		       gsl_wavelet_daubechies gsl_wavelet_daubechies_centered gsl_wavelet_haar gsl_wavelet_haar_centered gsl_wavelet_bspline
		       gsl_wavelet_bspline_centered))
	   
	   (reader-cond ((>= gsl-version 1.16)
			 (c-pointer (gsl_multifit_robust_default gsl_multifit_robust_bisquare gsl_multifit_robust_cauchy gsl_multifit_robust_fair
				     gsl_multifit_robust_huber gsl_multifit_robust_ols gsl_multifit_robust_welsch))))
	   
	   (int (gsl_message_mask gsl_check_range))
	   
	   ;; randist, rng
	   (c-pointer (gsl_qrng_niederreiter_2 gsl_qrng_sobol gsl_qrng_halton gsl_qrng_reversehalton
		       gsl_rng_borosh13 gsl_rng_coveyou gsl_rng_cmrg gsl_rng_fishman18 gsl_rng_fishman20 gsl_rng_fishman2x gsl_rng_gfsr4 
		       gsl_rng_knuthran gsl_rng_knuthran2 gsl_rng_knuthran2002 gsl_rng_lecuyer21 gsl_rng_minstd gsl_rng_mrg gsl_rng_mt19937 
		       gsl_rng_mt19937_1999 gsl_rng_mt19937_1998 gsl_rng_r250 gsl_rng_ran0 gsl_rng_ran1 gsl_rng_ran2 gsl_rng_ran3 gsl_rng_rand 
		       gsl_rng_rand48 gsl_rng_random128_bsd gsl_rng_random128_glibc2 gsl_rng_random128_libc5 gsl_rng_random256_bsd 
		       gsl_rng_random256_glibc2 gsl_rng_random256_libc5 gsl_rng_random32_bsd gsl_rng_random32_glibc2 gsl_rng_random32_libc5 
		       gsl_rng_random64_bsd gsl_rng_random64_glibc2 gsl_rng_random64_libc5 gsl_rng_random8_bsd gsl_rng_random8_glibc2 
		       gsl_rng_random8_libc5 gsl_rng_random_bsd gsl_rng_random_glibc2 gsl_rng_random_libc5 gsl_rng_randu 
		       gsl_rng_ranf gsl_rng_ranlux gsl_rng_ranlux389 gsl_rng_ranlxd1 gsl_rng_ranlxd2 gsl_rng_ranlxs0 gsl_rng_ranlxs1 
		       gsl_rng_ranlxs2 gsl_rng_ranmar gsl_rng_slatec gsl_rng_taus gsl_rng_taus2 gsl_rng_taus113 gsl_rng_transputer 
		       gsl_rng_tt800 gsl_rng_uni gsl_rng_uni32 gsl_rng_vax gsl_rng_waterman14 gsl_rng_zuf gsl_rng_default gsl_rng_default_seed))
	   
	   (gsl_qrng* gsl_qrng_alloc (gsl_qrng_type* int))
	   (int gsl_qrng_memcpy (gsl_qrng* gsl_qrng*))
	   (gsl_qrng* gsl_qrng_clone (gsl_qrng*))
	   (void gsl_qrng_free (gsl_qrng*))
	   (void gsl_qrng_init (gsl_qrng*))
	   (char* gsl_qrng_name (gsl_qrng*))
	   (size_t gsl_qrng_size (gsl_qrng*))
	   (void* gsl_qrng_state (gsl_qrng*))
	   (int gsl_qrng_get (gsl_qrng* double*))
	   (int gsl_ran_bernoulli (gsl_rng* double))
	   (double gsl_ran_bernoulli_pdf (int double))
	   (double gsl_ran_beta (gsl_rng* double double))
	   (double gsl_ran_beta_pdf (double double double))
	   (int gsl_ran_binomial (gsl_rng* double int))
	   (int gsl_ran_binomial_knuth (gsl_rng* double int))
	   (int gsl_ran_binomial_tpe (gsl_rng* double int))
	   (double gsl_ran_binomial_pdf (int double int))
	   (double gsl_ran_exponential (gsl_rng* double))
	   (double gsl_ran_exponential_pdf (double double))
	   (double gsl_ran_exppow (gsl_rng* double double))
	   (double gsl_ran_exppow_pdf (double double double))
	   (double gsl_ran_cauchy (gsl_rng* double))
	   (double gsl_ran_cauchy_pdf (double double))
	   (double gsl_ran_chisq (gsl_rng* double))
	   (double gsl_ran_chisq_pdf (double double))
	   (void gsl_ran_dirichlet (gsl_rng* size_t double* double*))
	   (double gsl_ran_dirichlet_pdf (size_t double* double*))
	   (double gsl_ran_dirichlet_lnpdf (size_t double* double*))
	   (double gsl_ran_erlang (gsl_rng* double double))
	   (double gsl_ran_erlang_pdf (double double double))
	   (double gsl_ran_fdist (gsl_rng* double double))
	   (double gsl_ran_fdist_pdf (double double double))
	   (double gsl_ran_flat (gsl_rng* double double))
	   (double gsl_ran_flat_pdf (double double double))
	   (double gsl_ran_gamma (gsl_rng* double double))
	   (double gsl_ran_gamma_int (gsl_rng* int))
	   (double gsl_ran_gamma_pdf (double double double))
	   (double gsl_ran_gamma_mt (gsl_rng* double double))
	   (double gsl_ran_gamma_knuth (gsl_rng* double double))
	   (double gsl_ran_gaussian (gsl_rng* double))
	   (double gsl_ran_gaussian_ratio_method (gsl_rng* double))
	   (double gsl_ran_gaussian_ziggurat (gsl_rng* double))
	   (double gsl_ran_gaussian_pdf (double double))
	   (double gsl_ran_ugaussian (gsl_rng*))
	   (double gsl_ran_ugaussian_ratio_method (gsl_rng*))
	   (double gsl_ran_ugaussian_pdf (double))
	   (double gsl_ran_gaussian_tail (gsl_rng* double double))
	   (double gsl_ran_gaussian_tail_pdf (double double double))
	   (double gsl_ran_ugaussian_tail (gsl_rng* double))
	   (double gsl_ran_ugaussian_tail_pdf (double double))
	   (void gsl_ran_bivariate_gaussian (gsl_rng* double double double double* double*))
	   (double gsl_ran_bivariate_gaussian_pdf (double double double double double))
	   (double gsl_ran_landau (gsl_rng*))
	   (double gsl_ran_landau_pdf (double))
	   (int gsl_ran_geometric (gsl_rng* double))
	   (double gsl_ran_geometric_pdf (int double))
	   (int gsl_ran_hypergeometric (gsl_rng* int int int))
	   (double gsl_ran_hypergeometric_pdf (int int int int))
	   (double gsl_ran_gumbel1 (gsl_rng* double double))
	   (double gsl_ran_gumbel1_pdf (double double double))
	   (double gsl_ran_gumbel2 (gsl_rng* double double))
	   (double gsl_ran_gumbel2_pdf (double double double))
	   (double gsl_ran_logistic (gsl_rng* double))
	   (double gsl_ran_logistic_pdf (double double))
	   (double gsl_ran_lognormal (gsl_rng* double double))
	   (double gsl_ran_lognormal_pdf (double double double))
	   (int gsl_ran_logarithmic (gsl_rng* double))
	   (double gsl_ran_logarithmic_pdf (int double))
	   ;; int*		    (void gsl_ran_multinomial (gsl_rng* size_t int double* int*)) ; unsigned int* 
	   ;; int*		    (double gsl_ran_multinomial_pdf (size_t double* int*))        ; unsigned int*
	   ;; int*		    (double gsl_ran_multinomial_lnpdf (size_t double* int*))      ; unsigned int*
	   (int gsl_ran_negative_binomial (gsl_rng* double double))
	   (double gsl_ran_negative_binomial_pdf (int double double))
	   (int gsl_ran_pascal (gsl_rng* double int))
	   (double gsl_ran_pascal_pdf (int double int))
	   (double gsl_ran_pareto (gsl_rng* double double))
	   (double gsl_ran_pareto_pdf (double double double))
	   (int gsl_ran_poisson (gsl_rng* double))
	   ;; int*		    (void gsl_ran_poisson_array (gsl_rng* size_t int* double))   ; unsigned int*
	   (double gsl_ran_poisson_pdf (int double))
	   (double gsl_ran_rayleigh (gsl_rng* double))
	   (double gsl_ran_rayleigh_pdf (double double))
	   (double gsl_ran_rayleigh_tail (gsl_rng* double double))
	   (double gsl_ran_rayleigh_tail_pdf (double double double))
	   (double gsl_ran_tdist (gsl_rng* double))
	   (double gsl_ran_tdist_pdf (double double))
	   (double gsl_ran_laplace (gsl_rng* double))
	   (double gsl_ran_laplace_pdf (double double))
	   (double gsl_ran_levy (gsl_rng* double double))
	   (double gsl_ran_levy_skew (gsl_rng* double double double))
	   (double gsl_ran_weibull (gsl_rng* double double))
	   (double gsl_ran_weibull_pdf (double double double))
	   (void gsl_ran_dir_2d (gsl_rng* double* double*))
	   (void gsl_ran_dir_2d_trig_method (gsl_rng* double* double*))
	   (void gsl_ran_dir_3d (gsl_rng* double* double* double*))
	   (void gsl_ran_dir_nd (gsl_rng* size_t double*))
	   (void gsl_ran_shuffle (gsl_rng* void* size_t size_t))
	   (int gsl_ran_choose (gsl_rng* void* size_t void* size_t size_t))
	   (void gsl_ran_sample (gsl_rng* void* size_t void* size_t size_t))
	   (gsl_ran_discrete_t* gsl_ran_discrete_preproc (size_t double*))
	   (void gsl_ran_discrete_free (gsl_ran_discrete_t*))
	   (size_t gsl_ran_discrete (gsl_rng* gsl_ran_discrete_t*))
	   (double gsl_ran_discrete_pdf (size_t gsl_ran_discrete_t*))
	   (gsl_rng_type** gsl_rng_types_setup (void))
	   (gsl_rng* gsl_rng_alloc (gsl_rng_type*))
	   (int gsl_rng_memcpy (gsl_rng* gsl_rng*))
	   (gsl_rng* gsl_rng_clone (gsl_rng*))
	   (void gsl_rng_free (gsl_rng*))
	   (void gsl_rng_set (gsl_rng* int))
	   (int gsl_rng_max (gsl_rng*))
	   (int gsl_rng_min (gsl_rng*))
	   (char* gsl_rng_name (gsl_rng*))
	   (int gsl_rng_fread (FILE* gsl_rng*))
	   (int gsl_rng_fwrite (FILE* gsl_rng*))
	   (size_t gsl_rng_size (gsl_rng*))
	   (void* gsl_rng_state (gsl_rng*))
	   (void gsl_rng_print_state (gsl_rng*))
	   (gsl_rng_type* gsl_rng_env_setup (void))
	   (int gsl_rng_get (gsl_rng*))
	   (double gsl_rng_uniform (gsl_rng*))
	   (double gsl_rng_uniform_pos (gsl_rng*))
	   (int gsl_rng_uniform_int (gsl_rng* int))
	   
	   ;; gsl_complex
	   (in-C "#define S7_TO_GSL_COMPLEX(sg, g) GSL_SET_COMPLEX(&g, s7_real_part(sg), s7_imag_part(sg))
                  #define GSL_TO_S7_COMPLEX(sc, g) s7_make_complex(sc, GSL_REAL(g), GSL_IMAG(g))
                           
                  static s7_pointer s7_gsl_c_c(s7_scheme *sc, s7_pointer arg1, gsl_complex (*callee)(gsl_complex a))
                  {
                    gsl_complex g, g1;
                    S7_TO_GSL_COMPLEX(arg1, g1);
                    g = callee(g1);
                    return(GSL_TO_S7_COMPLEX(sc, g));
                  }
                  static s7_pointer s7_gsl_r_c(s7_scheme *sc, s7_pointer arg1, gsl_complex (*callee)(double a))
                  {
                    gsl_complex g;
                    g = callee(s7_number_to_real(sc, arg1));
                    return(GSL_TO_S7_COMPLEX(sc, g));
                  }
                  static s7_pointer s7_gsl_c_r(s7_scheme *sc, s7_pointer arg1, double (*callee)(gsl_complex a))
                  {
                    gsl_complex g1;
                    S7_TO_GSL_COMPLEX(arg1, g1);
                    return(s7_make_real(sc, callee(g1)));
                  }
                  static s7_pointer s7_gsl_cc_c(s7_scheme *sc, s7_pointer arg1, s7_pointer arg2, gsl_complex (*callee)(gsl_complex a, gsl_complex b))
                  {
                    gsl_complex g, g1, g2;
                    S7_TO_GSL_COMPLEX(arg1, g1);
                    S7_TO_GSL_COMPLEX(arg2, g2);
                    g = callee(g1, g2);
                    return(GSL_TO_S7_COMPLEX(sc, g));
                  }
                  static s7_pointer s7_gsl_cr_c(s7_scheme *sc, s7_pointer arg1, s7_pointer arg2, gsl_complex (*callee)(gsl_complex a, double b))
                  {
                    gsl_complex g, g1;
                    S7_TO_GSL_COMPLEX(arg1, g1);
                    g = callee(g1, s7_number_to_real(sc, arg2));
                    return(GSL_TO_S7_COMPLEX(sc, g));
                  }
                  static s7_pointer g_gsl_complex_arg(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_r(sc, s7_car(args), gsl_complex_arg));}
                  static s7_pointer g_gsl_complex_abs(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_r(sc, s7_car(args), gsl_complex_abs));}
                  static s7_pointer g_gsl_complex_abs2(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_r(sc, s7_car(args), gsl_complex_abs2));}
                  static s7_pointer g_gsl_complex_logabs(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_r(sc, s7_car(args), gsl_complex_logabs));}
                  static s7_pointer g_gsl_complex_conjugate(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_conjugate));}
                  static s7_pointer g_gsl_complex_inverse(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_inverse));}
                  static s7_pointer g_gsl_complex_negative(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_negative));}
                  static s7_pointer g_gsl_complex_sqrt(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_sqrt));}
                  static s7_pointer g_gsl_complex_exp(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_exp));}
                  static s7_pointer g_gsl_complex_log(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_log));}
                  static s7_pointer g_gsl_complex_log10(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_log10));}
                  static s7_pointer g_gsl_complex_sin(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_sin));}
                  static s7_pointer g_gsl_complex_cos(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_cos));}
                  static s7_pointer g_gsl_complex_sec(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_sec));}
                  static s7_pointer g_gsl_complex_csc(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_csc));}
                  static s7_pointer g_gsl_complex_tan(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_tan));}
                  static s7_pointer g_gsl_complex_cot(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_cot));}
                  static s7_pointer g_gsl_complex_sinh(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_sinh));}
                  static s7_pointer g_gsl_complex_cosh(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_cosh));}
                  static s7_pointer g_gsl_complex_sech(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_sech));}
                  static s7_pointer g_gsl_complex_csch(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_csch));}
                  static s7_pointer g_gsl_complex_tanh(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_tanh));}
                  static s7_pointer g_gsl_complex_coth(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_coth));}
                  static s7_pointer g_gsl_complex_arctan(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_arctan));}
                  static s7_pointer g_gsl_complex_arccot(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_arccot));}
                  static s7_pointer g_gsl_complex_arcsinh(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_arcsinh));}
                  static s7_pointer g_gsl_complex_arcsech(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_arcsech));}
                  static s7_pointer g_gsl_complex_arccsch(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_arccsch));}
                  static s7_pointer g_gsl_complex_arccoth(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_arccoth));}
                  static s7_pointer g_gsl_complex_arcsin(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_arcsin));}
                  static s7_pointer g_gsl_complex_arccos(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_arccos));}
                  static s7_pointer g_gsl_complex_arcsec(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_arcsec));}
                  static s7_pointer g_gsl_complex_arccsc(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_arccsc));}
                  static s7_pointer g_gsl_complex_arccosh(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_arccosh));}
                  static s7_pointer g_gsl_complex_arctanh(s7_scheme *sc, s7_pointer args) {return(s7_gsl_c_c(sc, s7_car(args), gsl_complex_arctanh));}
                  static s7_pointer g_gsl_complex_sqrt_real(s7_scheme *sc, s7_pointer args) {return(s7_gsl_r_c(sc, s7_car(args), gsl_complex_sqrt_real));}
                  static s7_pointer g_gsl_complex_arcsin_real(s7_scheme *sc, s7_pointer args) {return(s7_gsl_r_c(sc, s7_car(args), gsl_complex_arcsin_real));}
                  static s7_pointer g_gsl_complex_arccos_real(s7_scheme *sc, s7_pointer args) {return(s7_gsl_r_c(sc, s7_car(args), gsl_complex_arccos_real));}
                  static s7_pointer g_gsl_complex_arcsec_real(s7_scheme *sc, s7_pointer args) {return(s7_gsl_r_c(sc, s7_car(args), gsl_complex_arcsec_real));}
                  static s7_pointer g_gsl_complex_arccsc_real(s7_scheme *sc, s7_pointer args) {return(s7_gsl_r_c(sc, s7_car(args), gsl_complex_arccsc_real));}
                  static s7_pointer g_gsl_complex_arccosh_real(s7_scheme *sc, s7_pointer args) {return(s7_gsl_r_c(sc, s7_car(args), gsl_complex_arccosh_real));}
                  static s7_pointer g_gsl_complex_arctanh_real(s7_scheme *sc, s7_pointer args) {return(s7_gsl_r_c(sc, s7_car(args), gsl_complex_arctanh_real));}
                  static s7_pointer g_gsl_complex_add(s7_scheme *sc, s7_pointer args) {return(s7_gsl_cc_c(sc, s7_car(args), s7_cadr(args), gsl_complex_add));}
                  static s7_pointer g_gsl_complex_sub(s7_scheme *sc, s7_pointer args) {return(s7_gsl_cc_c(sc, s7_car(args), s7_cadr(args), gsl_complex_sub));}
                  static s7_pointer g_gsl_complex_mul(s7_scheme *sc, s7_pointer args) {return(s7_gsl_cc_c(sc, s7_car(args), s7_cadr(args), gsl_complex_mul));}
                  static s7_pointer g_gsl_complex_div(s7_scheme *sc, s7_pointer args) {return(s7_gsl_cc_c(sc, s7_car(args), s7_cadr(args), gsl_complex_div));}
                  static s7_pointer g_gsl_complex_log_b(s7_scheme *sc, s7_pointer args) {return(s7_gsl_cc_c(sc, s7_car(args), s7_cadr(args), gsl_complex_log_b));}
                  static s7_pointer g_gsl_complex_pow(s7_scheme *sc, s7_pointer args) {return(s7_gsl_cc_c(sc, s7_car(args), s7_cadr(args), gsl_complex_pow));}
                  static s7_pointer g_gsl_complex_add_real(s7_scheme *sc, s7_pointer args) {return(s7_gsl_cr_c(sc, s7_car(args), s7_cadr(args), gsl_complex_add_real));}
                  static s7_pointer g_gsl_complex_sub_real(s7_scheme *sc, s7_pointer args) {return(s7_gsl_cr_c(sc, s7_car(args), s7_cadr(args), gsl_complex_sub_real));}
                  static s7_pointer g_gsl_complex_mul_real(s7_scheme *sc, s7_pointer args) {return(s7_gsl_cr_c(sc, s7_car(args), s7_cadr(args), gsl_complex_mul_real));}
                  static s7_pointer g_gsl_complex_div_real(s7_scheme *sc, s7_pointer args) {return(s7_gsl_cr_c(sc, s7_car(args), s7_cadr(args), gsl_complex_div_real));}
                  static s7_pointer g_gsl_complex_add_imag(s7_scheme *sc, s7_pointer args) {return(s7_gsl_cr_c(sc, s7_car(args), s7_cadr(args), gsl_complex_add_imag));}
                  static s7_pointer g_gsl_complex_sub_imag(s7_scheme *sc, s7_pointer args) {return(s7_gsl_cr_c(sc, s7_car(args), s7_cadr(args), gsl_complex_sub_imag));}
                  static s7_pointer g_gsl_complex_mul_imag(s7_scheme *sc, s7_pointer args) {return(s7_gsl_cr_c(sc, s7_car(args), s7_cadr(args), gsl_complex_mul_imag));}
                  static s7_pointer g_gsl_complex_div_imag(s7_scheme *sc, s7_pointer args) {return(s7_gsl_cr_c(sc, s7_car(args), s7_cadr(args), gsl_complex_div_imag));}
                  static s7_pointer g_gsl_complex_pow_real(s7_scheme *sc, s7_pointer args) {return(s7_gsl_cr_c(sc, s7_car(args), s7_cadr(args), gsl_complex_pow_real));}
                  ")
	   
	   (C-function ("gsl_complex_arg" g_gsl_complex_arg "" 1))
	   (C-function ("gsl_complex_abs" g_gsl_complex_abs "" 1))
	   (C-function ("gsl_complex_abs2" g_gsl_complex_abs2 "" 1))
	   (C-function ("gsl_complex_logabs" g_gsl_complex_logabs "" 1))
	   
	   (C-function ("gsl_complex_conjugate" g_gsl_complex_conjugate "" 1))
	   (C-function ("gsl_complex_inverse" g_gsl_complex_inverse "" 1))
	   (C-function ("gsl_complex_negative" g_gsl_complex_negative "" 1))
	   (C-function ("gsl_complex_sqrt" g_gsl_complex_sqrt "" 1))
	   (C-function ("gsl_complex_exp" g_gsl_complex_exp "" 1))
	   (C-function ("gsl_complex_log" g_gsl_complex_log "" 1))
	   (C-function ("gsl_complex_log10" g_gsl_complex_log10 "" 1))
	   (C-function ("gsl_complex_sin" g_gsl_complex_sin "" 1))
	   (C-function ("gsl_complex_cos" g_gsl_complex_cos "" 1))
	   (C-function ("gsl_complex_sec" g_gsl_complex_sec "" 1))
	   (C-function ("gsl_complex_csc" g_gsl_complex_csc "" 1))
	   (C-function ("gsl_complex_tan" g_gsl_complex_tan "" 1))
	   (C-function ("gsl_complex_cot" g_gsl_complex_cot "" 1))
	   (C-function ("gsl_complex_sinh" g_gsl_complex_sinh "" 1))
	   (C-function ("gsl_complex_cosh" g_gsl_complex_cosh "" 1))
	   (C-function ("gsl_complex_sech" g_gsl_complex_sech "" 1))
	   (C-function ("gsl_complex_csch" g_gsl_complex_csch "" 1))
	   (C-function ("gsl_complex_tanh" g_gsl_complex_tanh "" 1))
	   (C-function ("gsl_complex_coth" g_gsl_complex_coth "" 1))
	   (C-function ("gsl_complex_arctan" g_gsl_complex_arctan "" 1))
	   (C-function ("gsl_complex_arccot" g_gsl_complex_arccot "" 1))
	   (C-function ("gsl_complex_arcsinh" g_gsl_complex_arcsinh "" 1))
	   (C-function ("gsl_complex_arcsech" g_gsl_complex_arcsech "" 1))
	   (C-function ("gsl_complex_arccsch" g_gsl_complex_arccsch "" 1))
	   (C-function ("gsl_complex_arccoth" g_gsl_complex_arccoth "" 1))
	   (C-function ("gsl_complex_arcsin" g_gsl_complex_arcsin "" 1))
	   (C-function ("gsl_complex_arccos" g_gsl_complex_arccos "" 1))
	   (C-function ("gsl_complex_arcsec" g_gsl_complex_arcsec "" 1))
	   (C-function ("gsl_complex_arccsc" g_gsl_complex_arccsc "" 1))
	   (C-function ("gsl_complex_arccosh" g_gsl_complex_arccosh "" 1))
	   (C-function ("gsl_complex_arctanh" g_gsl_complex_arctanh "" 1))
	   
	   (C-function ("gsl_complex_sqrt_real" g_gsl_complex_sqrt_real "" 1))
	   (C-function ("gsl_complex_arcsin_real" g_gsl_complex_arcsin_real "" 1))
	   (C-function ("gsl_complex_arccos_real" g_gsl_complex_arccos_real "" 1))
	   (C-function ("gsl_complex_arcsec_real" g_gsl_complex_arcsec_real "" 1))
	   (C-function ("gsl_complex_arccsc_real" g_gsl_complex_arccsc_real "" 1))
	   (C-function ("gsl_complex_arccosh_real" g_gsl_complex_arccosh_real "" 1))
	   (C-function ("gsl_complex_arctanh_real" g_gsl_complex_arctanh_real "" 1))
	   
	   (C-function ("gsl_complex_add" g_gsl_complex_add "" 2))
	   (C-function ("gsl_complex_sub" g_gsl_complex_sub "" 2))
	   (C-function ("gsl_complex_mul" g_gsl_complex_mul "" 2))
	   (C-function ("gsl_complex_div" g_gsl_complex_div "" 2))
	   (C-function ("gsl_complex_log_b" g_gsl_complex_log_b "" 2))
	   (C-function ("gsl_complex_pow" g_gsl_complex_pow "" 2))
	   
	   (C-function ("gsl_complex_add_real" g_gsl_complex_add_real "" 2))
	   (C-function ("gsl_complex_sub_real" g_gsl_complex_sub_real "" 2))
	   (C-function ("gsl_complex_mul_real" g_gsl_complex_mul_real "" 2))
	   (C-function ("gsl_complex_div_real" g_gsl_complex_div_real "" 2))
	   (C-function ("gsl_complex_add_imag" g_gsl_complex_add_imag "" 2))
	   (C-function ("gsl_complex_sub_imag" g_gsl_complex_sub_imag "" 2))
	   (C-function ("gsl_complex_mul_imag" g_gsl_complex_mul_imag "" 2))
	   (C-function ("gsl_complex_div_imag" g_gsl_complex_div_imag "" 2))
	   (C-function ("gsl_complex_pow_real" g_gsl_complex_pow_real "" 2))
	   
	   
	   ;; cheb
	   (gsl_cheb_series* gsl_cheb_alloc (size_t))
	   (void gsl_cheb_free (gsl_cheb_series*))
	   (size_t gsl_cheb_order (gsl_cheb_series*))
	   (size_t gsl_cheb_size (gsl_cheb_series*))
	   (double* gsl_cheb_coeffs (gsl_cheb_series*))
	   (double gsl_cheb_eval (gsl_cheb_series* double))
	   (int gsl_cheb_eval_err (gsl_cheb_series* double double* double*))
	   (double gsl_cheb_eval_n (gsl_cheb_series* size_t double))
	   (int gsl_cheb_eval_n_err (gsl_cheb_series* size_t double double* double*))
	   (double gsl_cheb_eval_mode (gsl_cheb_series* double int))
	   (int gsl_cheb_eval_mode_e (gsl_cheb_series* double int double* double*))
	   (int gsl_cheb_calc_deriv (gsl_cheb_series* gsl_cheb_series*))
	   (int gsl_cheb_calc_integ (gsl_cheb_series* gsl_cheb_series*))
	   
	   ;; gsl_function is a struct with double function(double void*) and void* params
	   (in-C "static s7_scheme *gsl_f_s7;
                  static gsl_function gsl_f;
                  static double gsl_f_caller(double x, void *p) 
                  {
                    return(s7_real(s7_call(gsl_f_s7, (s7_pointer)p, s7_cons(gsl_f_s7, s7_make_real(gsl_f_s7, x), s7_nil(gsl_f_s7)))));
                  }
                  #define make_gsl_function(Args) do {gsl_f.function = gsl_f_caller; gsl_f.params = (void *)Args; gsl_f_s7 = sc;} while (0)
                  static s7_pointer g_gsl_cheb_init(s7_scheme *sc, s7_pointer args)
                  {
                    make_gsl_function(s7_cadr(args));
                    return(s7_make_integer(sc, gsl_cheb_init((gsl_cheb_series *)s7_c_pointer(s7_car(args)),
                                                             &gsl_f, s7_real(s7_caddr(args)), s7_real(s7_cadddr(args)))));
                  }
                  ")
	   (C-function ("gsl_cheb_init" g_gsl_cheb_init "" 4))
	   
	   ;; interp
	   (gsl_interp_accel* gsl_interp_accel_alloc (void))
	   (int gsl_interp_accel_reset (gsl_interp_accel*))
	   (void gsl_interp_accel_free (gsl_interp_accel*))
	   (gsl_interp* gsl_interp_alloc (gsl_interp_type* size_t))
	   (int gsl_interp_init (gsl_interp* double* double* size_t))
	   (char* gsl_interp_name (gsl_interp*))
	   (int gsl_interp_min_size (gsl_interp*))
	   (reader-cond ((>= gsl-version 1.15) (int gsl_interp_type_min_size (gsl_interp_type*))))
	   (int gsl_interp_eval_e (gsl_interp* double* double* double gsl_interp_accel* double*))
	   (double gsl_interp_eval (gsl_interp* double* double* double gsl_interp_accel*))
	   (int gsl_interp_eval_deriv_e (gsl_interp* double* double* double gsl_interp_accel* double*))
	   (double gsl_interp_eval_deriv (gsl_interp* double* double* double gsl_interp_accel*))
	   (int gsl_interp_eval_deriv2_e (gsl_interp* double* double* double gsl_interp_accel* double*))
	   (double gsl_interp_eval_deriv2 (gsl_interp* double* double* double gsl_interp_accel*))
	   (int gsl_interp_eval_integ_e (gsl_interp* double* double* double double gsl_interp_accel* double*))
	   (double gsl_interp_eval_integ (gsl_interp* double* double* double double gsl_interp_accel*))
	   (void gsl_interp_free (gsl_interp*))
	   (size_t gsl_interp_bsearch (double* double size_t size_t))
	   (size_t gsl_interp_accel_find (gsl_interp_accel* double* size_t double))
	   
	   ;; spline (based on interp above)
	   (gsl_spline* gsl_spline_alloc (gsl_interp_type* size_t))
	   (int gsl_spline_init (gsl_spline* double* double* size_t))
	   (char* gsl_spline_name (gsl_spline*))
	   (int gsl_spline_min_size (gsl_spline*))
	   (int gsl_spline_eval_e (gsl_spline* double gsl_interp_accel* double*))
	   (double gsl_spline_eval (gsl_spline* double gsl_interp_accel*))
	   (int gsl_spline_eval_deriv_e (gsl_spline* double gsl_interp_accel* double*))
	   (double gsl_spline_eval_deriv (gsl_spline* double gsl_interp_accel*))
	   (int gsl_spline_eval_deriv2_e (gsl_spline* double gsl_interp_accel* double*))
	   (double gsl_spline_eval_deriv2 (gsl_spline* double gsl_interp_accel*))
	   (int gsl_spline_eval_integ_e (gsl_spline* double double gsl_interp_accel* double*))
	   (double gsl_spline_eval_integ (gsl_spline* double double gsl_interp_accel*))
	   (void gsl_spline_free (gsl_spline*))
	   
	   ;; bspline
	   (gsl_bspline_workspace* gsl_bspline_alloc (size_t size_t))
	   (void gsl_bspline_free (gsl_bspline_workspace*))
	   (size_t gsl_bspline_ncoeffs (gsl_bspline_workspace*))
	   (size_t gsl_bspline_order (gsl_bspline_workspace*))
	   (size_t gsl_bspline_nbreak (gsl_bspline_workspace*))
	   (double gsl_bspline_breakpoint (size_t gsl_bspline_workspace*))
	   (reader-cond ((>= gsl-version 1.16) 
			 (double gsl_bspline_greville_abscissa (size_t gsl_bspline_workspace*))
			 (int gsl_bspline_knots_greville (gsl_vector* gsl_bspline_workspace* double*))))
	   (int gsl_bspline_knots (gsl_vector* gsl_bspline_workspace*))
	   (int gsl_bspline_knots_uniform (double double gsl_bspline_workspace*))
	   (int gsl_bspline_eval (double gsl_vector* gsl_bspline_workspace*))
	   (int gsl_bspline_eval_nonzero (double gsl_vector* size_t* size_t* gsl_bspline_workspace*))
	   (gsl_bspline_deriv_workspace* gsl_bspline_deriv_alloc (size_t))
	   (void gsl_bspline_deriv_free (gsl_bspline_deriv_workspace*))
	   (int gsl_bspline_deriv_eval (double size_t gsl_matrix* gsl_bspline_workspace* gsl_bspline_deriv_workspace*))
	   (int gsl_bspline_deriv_eval_nonzero (double size_t gsl_matrix* size_t* size_t* gsl_bspline_workspace* gsl_bspline_deriv_workspace*))
	   
	   ;; sort
	   ;; perhaps size_t* -> int vector?
	   
	   (void gsl_sort (double* size_t size_t))
	   (reader-cond ((>= gsl-version 1.16) 
			 (void gsl_sort2 (double* size_t double* size_t size_t))
			 (void gsl_sort_vector2 (gsl_vector* gsl_vector*))
			 (int gsl_poly_dd_hermite_init (double* double* double* double* double* size_t))))
	   (void gsl_sort_index (size_t* double* size_t size_t))
	   (int gsl_sort_smallest (double* size_t double* size_t size_t))
	   (int gsl_sort_smallest_index (size_t* size_t double* size_t size_t))
	   (int gsl_sort_largest (double* size_t double* size_t size_t))
	   (int gsl_sort_largest_index (size_t* size_t double* size_t size_t))
	   (void gsl_sort_vector (gsl_vector*))
	   (int gsl_sort_vector_index (gsl_permutation* gsl_vector*))
	   (int gsl_sort_vector_smallest (double* size_t gsl_vector*))
	   (int gsl_sort_vector_largest (double* size_t gsl_vector*))
	   (int gsl_sort_vector_smallest_index (size_t* size_t gsl_vector*))
	   (int gsl_sort_vector_largest_index (size_t* size_t gsl_vector*))
	   
	   ;; poly
	   (double gsl_poly_eval (double* int double))
	   (int gsl_poly_eval_derivs (double* size_t double double* size_t))
	   (int gsl_poly_dd_init (double* double* double* size_t))
	   (double gsl_poly_dd_eval (double* double* size_t double))
	   (int gsl_poly_dd_taylor (double* double double* double* size_t double*))
	   (void gsl_poly_complex_workspace_free (gsl_poly_complex_workspace*))
	   (gsl_poly_complex_workspace* gsl_poly_complex_workspace_alloc (size_t))
	   
	   (in-C "static s7_pointer g_gsl_poly_complex_eval(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_complex z, rz;
                    S7_TO_GSL_COMPLEX(s7_caddr(args), z);
                    rz = gsl_poly_complex_eval((double *)s7_c_pointer(s7_car(args)), (int)s7_integer(s7_cadr(args)), z);
                    return(GSL_TO_S7_COMPLEX(sc, rz));
                  }
                  static s7_pointer g_gsl_complex_poly_complex_eval(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_complex *z;
                    gsl_complex rz, x;
                    int i, n;
                    s7_pointer v;
                    v = s7_car(args);
                    n = s7_integer(s7_cadr(args));
                    z = (gsl_complex *)calloc(n, sizeof(gsl_complex));
                    for (i = 0; i < n; i++)
                      S7_TO_GSL_COMPLEX(s7_vector_ref(sc, v, i), z[i]);
                    S7_TO_GSL_COMPLEX(s7_caddr(args), x);
                    rz = gsl_complex_poly_complex_eval(z, n, x);
                    free(z);
                    return(GSL_TO_S7_COMPLEX(sc, rz));
                  }
                  static s7_pointer g_gsl_poly_complex_solve_quadratic(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_complex z0, z1;
                    int result;
                    s7_pointer res;
                  
                    res = s7_cadddr(args);
                    result = gsl_poly_complex_solve_quadratic(s7_number_to_real(sc, s7_car(args)), s7_number_to_real(sc, s7_cadr(args)), 
                                                              s7_number_to_real(sc, s7_caddr(args)), &z0, &z1);
                    s7_vector_set(sc, res, 0, GSL_TO_S7_COMPLEX(sc, z0));
                    s7_vector_set(sc, res, 1, GSL_TO_S7_COMPLEX(sc, z1));
                  
                    return(s7_make_integer(sc, result));
                  }
                  static s7_pointer g_gsl_poly_complex_solve_cubic(s7_scheme *sc, s7_pointer args)
                  {
                    /* trailing args are by ref, but I think I'll mimic the real solver above */
                    gsl_complex z0, z1, z2;
                    int result;
                    s7_pointer res;
                  
                    result = gsl_poly_complex_solve_cubic(s7_number_to_real(sc, s7_car(args)), s7_number_to_real(sc, s7_cadr(args)), 
                                                          s7_number_to_real(sc, s7_caddr(args)), &z0, &z1, &z2);
                    res = s7_cadddr(args);
                    s7_vector_set(sc, res, 0, GSL_TO_S7_COMPLEX(sc, z0));
                    s7_vector_set(sc, res, 1, GSL_TO_S7_COMPLEX(sc, z1));
                    s7_vector_set(sc, res, 2, GSL_TO_S7_COMPLEX(sc, z2));
                  
                    return(s7_make_integer(sc, result));
                  }
                           
                  static s7_pointer g_gsl_poly_complex_solve(s7_scheme *sc, s7_pointer args)
                  {
                    /* trailing args are by ref, but I think I'll mimic the real solver above */
                    double *z;
                    gsl_poly_complex_workspace *w;
                    int result, i, size;
                    s7_pointer res;
                           
                    size = s7_integer(s7_cadr(args));
                    res = s7_caddr(args);
                           
                    z = (double *)calloc(size * 2, sizeof(double));
                    w = gsl_poly_complex_workspace_alloc(size);
                    result = gsl_poly_complex_solve((double *)s7_c_pointer(s7_car(args)), size, w, (gsl_complex_packed_ptr)z);
                    gsl_poly_complex_workspace_free(w);
                           
                    for (i = 0; i < size - 1; i++)
                      s7_vector_set(sc, res, i, s7_make_complex(sc, z[2 * i], z[2 * i + 1]));
                    free(z);
                           
                    return(s7_make_integer(sc, result));
                  }
                           
                  static s7_pointer g_gsl_poly_solve_quadratic(s7_scheme *sc, s7_pointer args)
                  {
                    double x0, x1;
                    int result;
                    double *res;
                    res = (double *)s7_c_pointer(s7_cadddr(args));
                    result = gsl_poly_solve_quadratic(s7_number_to_real(sc, s7_car(args)), s7_number_to_real(sc, s7_cadr(args)), 
                                                      s7_number_to_real(sc, s7_caddr(args)), &x0, &x1);
                    res[0] = x0;
                    res[1] = x1;
                    return(s7_make_integer(sc, result));
                  }
                           
                  static s7_pointer g_gsl_poly_solve_cubic(s7_scheme *sc, s7_pointer args)
                  {
                    double x0, x1, x2;
                    int result;
                    double *res;
                    res = (double *)s7_c_pointer(s7_cadddr(args));
                    result = gsl_poly_solve_cubic(s7_number_to_real(sc, s7_car(args)), s7_number_to_real(sc, s7_cadr(args)), 
                                                  s7_number_to_real(sc, s7_caddr(args)), &x0, &x1, &x2);
                    res[0] = x0;
                    res[1] = x1;
                    res[2] = x2;
                    return(s7_make_integer(sc, result));
                  }
                  ")
	   
	   (C-function ("gsl_poly_complex_eval" g_gsl_poly_complex_eval "" 3))
	   (C-function ("gsl_complex_poly_complex_eval" g_gsl_complex_poly_complex_eval "" 3))
	   (C-function ("gsl_poly_complex_solve_quadratic" g_gsl_poly_complex_solve_quadratic "" 4))
	   (C-function ("gsl_poly_complex_solve_cubic" g_gsl_poly_complex_solve_cubic "" 4))
	   (C-function ("gsl_poly_complex_solve" g_gsl_poly_complex_solve "" 3))
	   (C-function ("gsl_poly_solve_quadratic" g_gsl_poly_solve_quadratic "" 4))
	   (C-function ("gsl_poly_solve_cubic" g_gsl_poly_solve_cubic "" 4))
	   
	   ;; vector
	   (in-C "static s7_pointer g_float_vector_to_gsl_vector(s7_scheme *sc, s7_pointer args)
                  {
                     gsl_vector *g;
                     int size;
                     s7_pointer v;
                     v = s7_car(args);
                     size = s7_vector_length(v);
                     g = (gsl_vector *)s7_c_pointer(s7_cadr(args));
                     memcpy((void *)(g->data), (void *)s7_float_vector_elements(v), size * sizeof(double));
                     return(s7_cadr(args));
                  }
                  static s7_pointer g_gsl_vector_to_float_vector(s7_scheme *sc, s7_pointer args)
                  {
                     gsl_vector *g;
                     int size;
                     s7_pointer v;
                     v = s7_cadr(args);
                     size = s7_vector_length(v);
                     g = (gsl_vector *)s7_c_pointer(s7_car(args));
                     memcpy((void *)s7_float_vector_elements(v), (void *)(g->data), size * sizeof(double));
                     return(s7_cadr(args));
                  }
                  ")
	   (C-function ("float-vector->gsl_vector" g_float_vector_to_gsl_vector "" 2))
	   (C-function ("gsl_vector->float-vector" g_gsl_vector_to_float_vector "" 2))
	   
	   (gsl_vector* gsl_vector_alloc (size_t))
	   (gsl_vector* gsl_vector_calloc (size_t))
	   (gsl_vector* gsl_vector_alloc_from_vector (gsl_vector* size_t size_t size_t))
	   (void gsl_vector_free (gsl_vector*))
	   (void gsl_vector_set_zero (gsl_vector*))
	   (void gsl_vector_set_all (gsl_vector* double))
	   (int gsl_vector_set_basis (gsl_vector* size_t))
	   (int gsl_vector_fread (FILE* gsl_vector*))
	   (int gsl_vector_fwrite (FILE* gsl_vector*))
	   (int gsl_vector_fscanf (FILE* gsl_vector*))
	   (int gsl_vector_fprintf (FILE* gsl_vector* char*))
	   (int gsl_vector_memcpy (gsl_vector* gsl_vector*))
	   (int gsl_vector_reverse (gsl_vector*))
	   (int gsl_vector_swap (gsl_vector* gsl_vector*))
	   (int gsl_vector_swap_elements (gsl_vector* size_t size_t))
	   (double gsl_vector_max (gsl_vector*))
	   (double gsl_vector_min (gsl_vector*))
	   (size_t gsl_vector_max_index (gsl_vector*))
	   (size_t gsl_vector_min_index (gsl_vector*))
	   (int gsl_vector_add (gsl_vector* gsl_vector*))
	   (int gsl_vector_sub (gsl_vector* gsl_vector*))
	   (int gsl_vector_mul (gsl_vector* gsl_vector*))
	   (int gsl_vector_div (gsl_vector* gsl_vector*))
	   (int gsl_vector_scale (gsl_vector* double))
	   (int gsl_vector_add_constant (gsl_vector* double))
	   (reader-cond ((>= gsl-version 1.15) (int gsl_vector_equal (gsl_vector* gsl_vector*))))
	   (int gsl_vector_isnull (gsl_vector*))
	   (int gsl_vector_ispos (gsl_vector*))
	   (int gsl_vector_isneg (gsl_vector*))
	   (int gsl_vector_isnonneg (gsl_vector*))
	   (double gsl_vector_get (gsl_vector* size_t))
	   (void gsl_vector_set (gsl_vector* size_t double))
	   (double* gsl_vector_ptr (gsl_vector* size_t))
	   (double* gsl_vector_const_ptr (gsl_vector* size_t))
	   (void gsl_vector_minmax (gsl_vector* double* double*)) ; by ref
	   (void gsl_vector_minmax_index (gsl_vector* size_t* size_t*)) ; by ref 
	   
	   ;; matrix
	   (in-C "static s7_pointer g_float_vector_to_gsl_matrix(s7_scheme *sc, s7_pointer args)
                  {
                     gsl_matrix *g;
                     int size;
                     s7_pointer v;
                     v = s7_car(args);
                     size = s7_vector_length(v);
                     g = (gsl_matrix *)s7_c_pointer(s7_cadr(args));
                     memcpy((void *)(g->data), (void *)s7_float_vector_elements(v), size * sizeof(double));
                     return(s7_cadr(args));
                  }
                  static s7_pointer g_gsl_matrix_to_float_vector(s7_scheme *sc, s7_pointer args)
                  {
                     gsl_matrix *g;
                     int size;
                     s7_pointer v;
                     v = s7_cadr(args);
                     size = s7_vector_length(v);
                     g = (gsl_matrix *)s7_c_pointer(s7_car(args));
                     memcpy((void *)s7_float_vector_elements(v), (void *)(g->data), size * sizeof(double));
                     return(s7_cadr(args));
                  }
                  ")
	   (C-function ("float-vector->gsl_matrix" g_float_vector_to_gsl_matrix "" 2))
	   (C-function ("gsl_matrix->float-vector" g_gsl_matrix_to_float_vector "" 2))
	   
	   (gsl_matrix* gsl_matrix_alloc (size_t size_t))
	   (gsl_matrix* gsl_matrix_calloc (size_t size_t))
	   (gsl_matrix* gsl_matrix_alloc_from_matrix (gsl_matrix* size_t size_t size_t size_t))
	   (gsl_vector* gsl_vector_alloc_row_from_matrix (gsl_matrix* size_t))
	   (gsl_vector* gsl_vector_alloc_col_from_matrix (gsl_matrix* size_t))
	   (void gsl_matrix_free (gsl_matrix*))
	   (void gsl_matrix_set_zero (gsl_matrix*))
	   (void gsl_matrix_set_identity (gsl_matrix*))
	   (void gsl_matrix_set_all (gsl_matrix* double))
	   (int gsl_matrix_fread (FILE* gsl_matrix*) )
	   (int gsl_matrix_fwrite (FILE* gsl_matrix*) )
	   (int gsl_matrix_fscanf (FILE* gsl_matrix*))
	   (int gsl_matrix_fprintf (FILE* gsl_matrix* char*))
	   (int gsl_matrix_memcpy (gsl_matrix* gsl_matrix*))
	   (int gsl_matrix_swap (gsl_matrix* gsl_matrix*))
	   (int gsl_matrix_swap_rows (gsl_matrix* size_t size_t))
	   (int gsl_matrix_swap_columns (gsl_matrix* size_t size_t))
	   (int gsl_matrix_swap_rowcol (gsl_matrix* size_t size_t))
	   (int gsl_matrix_transpose (gsl_matrix*))
	   (int gsl_matrix_transpose_memcpy (gsl_matrix* gsl_matrix*))
	   (double gsl_matrix_max (gsl_matrix*))
	   (double gsl_matrix_min (gsl_matrix*))
	   (void gsl_matrix_minmax (gsl_matrix* double* double*))
	   (void gsl_matrix_max_index (gsl_matrix* size_t* size_t*))
	   (void gsl_matrix_min_index (gsl_matrix* size_t* size_t*))
	   (void gsl_matrix_minmax_index (gsl_matrix* size_t* size_t* size_t* size_t*))
	   (reader-cond ((>= gsl-version 1.15) (int gsl_matrix_equal (gsl_matrix* gsl_matrix*))))
	   (int gsl_matrix_isnull (gsl_matrix*))
	   (int gsl_matrix_ispos (gsl_matrix*))
	   (int gsl_matrix_isneg (gsl_matrix*))
	   (int gsl_matrix_isnonneg (gsl_matrix*))
	   (int gsl_matrix_add (gsl_matrix* gsl_matrix*))
	   (int gsl_matrix_sub (gsl_matrix* gsl_matrix*))
	   (int gsl_matrix_mul_elements (gsl_matrix* gsl_matrix*))
	   (int gsl_matrix_div_elements (gsl_matrix* gsl_matrix*))
	   (int gsl_matrix_scale (gsl_matrix* double))
	   (int gsl_matrix_add_constant (gsl_matrix* double))
	   (int gsl_matrix_add_diagonal (gsl_matrix* double))
	   (int gsl_matrix_get_row (gsl_vector* gsl_matrix* size_t))
	   (int gsl_matrix_get_col (gsl_vector* gsl_matrix* size_t))
	   (int gsl_matrix_set_row (gsl_matrix* size_t gsl_vector*))
	   (int gsl_matrix_set_col (gsl_matrix* size_t gsl_vector*))
	   (double gsl_matrix_get (gsl_matrix* size_t size_t))
	   (void gsl_matrix_set (gsl_matrix* size_t size_t double))
	   (double* gsl_matrix_ptr (gsl_matrix* size_t size_t))
	   (double* gsl_matrix_const_ptr (gsl_matrix* size_t size_t))
	   (in-C "static s7_pointer g_gsl_matrix_size(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_matrix *g; g = (gsl_matrix *)s7_c_pointer(s7_car(args));
                    return(s7_cons(sc, s7_make_integer(sc, (s7_int)(g->size1)), s7_make_integer(sc, (s7_int)(g->size2))));
                  }")
           (C-function ("gsl_matrix_size" g_gsl_matrix_size "" 1))
	   
	   ;; cblas
	   (int gsl_blas_zdotu (gsl_vector_complex* gsl_vector_complex* gsl_complex*))
	   (int gsl_blas_zdotc (gsl_vector_complex* gsl_vector_complex* gsl_complex*))
	   (double gsl_blas_dnrm2 (gsl_vector*))
	   (double gsl_blas_dasum (gsl_vector*))
	   (double gsl_blas_dznrm2 (gsl_vector_complex*))
	   (double gsl_blas_dzasum (gsl_vector_complex*))
	   (size_t_t gsl_blas_idamax (gsl_vector*))
	   (size_t_t gsl_blas_izamax (gsl_vector_complex*))
	   (int gsl_blas_dswap (gsl_vector* gsl_vector*))
	   (int gsl_blas_dcopy (gsl_vector* gsl_vector*))
	   (int gsl_blas_daxpy (double gsl_vector* gsl_vector*))
	   (int gsl_blas_zswap (gsl_vector_complex* gsl_vector_complex*))
	   (int gsl_blas_zcopy (gsl_vector_complex* gsl_vector_complex*))
	   (int gsl_blas_drotg (double* double* double* double*))
	   (int gsl_blas_drotmg (double* double* double* double double*))
	   (int gsl_blas_drot (gsl_vector* gsl_vector* double double))
	   (int gsl_blas_drotm (gsl_vector* gsl_vector* double*))
	   (void gsl_blas_dscal (double gsl_vector*))
	   (void gsl_blas_zdscal (double gsl_vector_complex*))
	   (int gsl_blas_dgemv (int double gsl_matrix* gsl_vector* double gsl_vector*))
	   (int gsl_blas_dtrmv (int int int gsl_matrix* gsl_vector*))
	   (int gsl_blas_dtrsv (int int int gsl_matrix* gsl_vector*))
	   (int gsl_blas_ztrmv (int int int gsl_matrix_complex* gsl_vector_complex*))
	   (int gsl_blas_ztrsv (int int int gsl_matrix_complex* gsl_vector_complex*))
	   (int gsl_blas_dsymv (int double gsl_matrix* gsl_vector* double gsl_vector*))
	   (int gsl_blas_dger (double gsl_vector* gsl_vector* gsl_matrix*))
	   (int gsl_blas_dsyr (int double gsl_vector* gsl_matrix*))
	   (int gsl_blas_dsyr2 (int double gsl_vector* gsl_vector* gsl_matrix*))
	   (int gsl_blas_zher (int double gsl_vector_complex* gsl_matrix_complex*))
	   (int gsl_blas_dgemm (int int double gsl_matrix* gsl_matrix* double gsl_matrix*))
	   (int gsl_blas_dsymm (int int double gsl_matrix* gsl_matrix* double gsl_matrix*))
	   (int gsl_blas_dsyrk (int int double gsl_matrix* double gsl_matrix*))
	   (int gsl_blas_dsyr2k (int int double gsl_matrix* gsl_matrix* double gsl_matrix*))
	   (int gsl_blas_dtrmm (int int int int double gsl_matrix* gsl_matrix*))
	   (int gsl_blas_dtrsm (int int int int double gsl_matrix* gsl_matrix*))
	   (int gsl_blas_zherk (int int double gsl_matrix_complex* double gsl_matrix_complex*))
	   (double cblas_ddot (int double* int double* int))
	   (void cblas_zdotu_sub (int double* int double* int double*))
	   (void cblas_zdotc_sub (int double* int double* int double*))
	   (double cblas_dnrm2 (int double* int))
	   (double cblas_dasum (int double* int))
	   (double cblas_dznrm2 (int void* int))
	   (double cblas_dzasum (int void* int))
	   (size_t cblas_idamax (int double* int))
	   (size_t cblas_icamax (int void* int))
	   (size_t cblas_izamax (int void* int))
	   (void cblas_dswap (int double* int double* int))
	   (void cblas_dcopy (int double* int double* int))
	   (void cblas_daxpy (int double double* int double* int))
	   (void cblas_zswap (int double* int double* int))
	   (void cblas_zcopy (int double* int double* int))
	   (void cblas_zaxpy (int double* double* int double* int))
	   (void cblas_drotg (double* double* double* double*))
	   (void cblas_drotmg (double* double* double* double double*))
	   (void cblas_drot (int double* int double* int double double))
	   (void cblas_drotm (int double* int double* int double*))
	   (void cblas_dscal (int double double* int))
	   (void cblas_zscal (int double* double* int))
	   (void cblas_zdscal (int double double* int))
	   (void cblas_dgemv (int int int int double double* int double* int double double* int))
	   (void cblas_dgbmv (int int int int int int double double* int double* int double double* int))
	   (void cblas_dtrmv (int int int int int double* int double* int))
	   (void cblas_dtbmv (int int int int int int double* int double* int))
	   (void cblas_dtpmv (int int int int int double* double* int))
	   (void cblas_dtrsv (int int int int int double* int double* int))
	   (void cblas_dtbsv (int int int int int int double* int double* int))
	   (void cblas_dtpsv (int int int int int double* double* int))
	   (void cblas_zgemv (int int int int double* double* int double* int double* double* int))
	   (void cblas_zgbmv (int int int int int int double* double* int double* int double* double* int))
	   (void cblas_ztrmv (int int int int int double* int double* int))
	   (void cblas_ztbmv (int int int int int int double* int double* int))
	   (void cblas_ztpmv (int int int int int double* double* int))
	   (void cblas_ztrsv (int int int int int double* int double* int))
	   (void cblas_ztbsv (int int int int int int double* int double* int))
	   (void cblas_ztpsv (int int int int int double* double* int))
	   (void cblas_dsymv (int int int double double* int double* int double double* int))
	   (void cblas_dsbmv (int int int int double double* int double* int double double* int))
	   (void cblas_dspmv (int int int double double* double* int double double* int))
	   (void cblas_dger (int int int double double* int double* int double* int))
	   (void cblas_dsyr (int int int double double* int double* int))
	   (void cblas_dspr (int int int double double* int double*))
	   (void cblas_dsyr2 (int int int double double* int double* int double* int))
	   (void cblas_dspr2 (int int int double double* int double* int double*))
	   (void cblas_zhemv (int int int double* double* int double* int double* double* int))
	   (void cblas_zhbmv (int int int int double* double* int double* int double* double* int))
	   (void cblas_zhpmv (int int int double* double* double* int double* double* int))
	   (void cblas_zgeru (int int int double* double* int double* int double* int))
	   (void cblas_zgerc (int int int double* double* int double* int double* int))
	   (void cblas_zher (int int int double double* int double* int))
	   (void cblas_zhpr (int int int double double* int double*))
	   (void cblas_zher2 (int int int double* double* int double* int double* int))
	   (void cblas_zhpr2 (int int int double* double* int double* int double*))
	   (void cblas_dgemm (int int int int int int double double* int double* int double double* int))
	   (void cblas_dsymm (int int int int int double double* int double* int double double* int))
	   (void cblas_dsyrk (int int int int int double double* int double double* int))
	   (void cblas_dsyr2k (int int int int int double double* int double* int double double* int))
	   (void cblas_dtrmm (int int int int int int int double double* int double* int))
	   (void cblas_dtrsm (int int int int int int int double double* int double* int))
	   (void cblas_zgemm (int int int int int int double* double* int double* int double* double* int))
	   (void cblas_zsymm (int int int int int double* double* int double* int double* double* int))
	   (void cblas_zsyrk (int int int int int double* double* int double* double* int))
	   (void cblas_zsyr2k (int int int int int double* double* int double* int double* double* int))
	   (void cblas_ztrmm (int int int int int int int double* double* int double* int))
	   (void cblas_ztrsm (int int int int int int int double* double* int double* int))
	   (void cblas_zhemm (int int int int int double* double* int double* int double* double* int))
	   (void cblas_zherk (int int int int int double double* int double double* int))
	   (void cblas_zher2k (int int int int int double* double* int double* int double double* int))
	   
	   ;; combination
	   (gsl_combination* gsl_combination_alloc (size_t size_t))
	   (gsl_combination* gsl_combination_calloc (size_t size_t))
	   (void gsl_combination_init_first (gsl_combination*))
	   (void gsl_combination_init_last (gsl_combination*))
	   (void gsl_combination_free (gsl_combination*))
	   (int gsl_combination_memcpy (gsl_combination* gsl_combination*) )
	   (int gsl_combination_fread (FILE* gsl_combination*))
	   (int gsl_combination_fwrite (FILE* gsl_combination*))
	   (int gsl_combination_fscanf (FILE* gsl_combination*))
	   (int gsl_combination_fprintf (FILE* gsl_combination* char*))
	   (size_t gsl_combination_n (gsl_combination*))
	   (size_t gsl_combination_k (gsl_combination*))
					;(size_t* gsl_combination_data (gsl_combination*))
	   (int gsl_combination_valid (gsl_combination*))
	   (int gsl_combination_next (gsl_combination*))
	   (int gsl_combination_prev (gsl_combination*))
	   (size_t gsl_combination_get (gsl_combination* size_t))
	   (in-C "static s7_pointer g_gsl_combination_to_int_vector(s7_scheme *sc, s7_pointer args)
                  {
                     gsl_combination *g;
                     int i, size;
                     s7_int *el;
                     s7_pointer v;
                     v = s7_cadr(args);
                     size = s7_vector_length(v);
                     g = (gsl_combination *)s7_c_pointer(s7_car(args));
                     el = s7_int_vector_elements(v);
                     for (i = 0; i < size; i++) el[i] = (s7_int)(g->data[i]);
                     return(s7_cadr(args));
                  }
                  ")
	   (C-function ("gsl_combination->int-vector" g_gsl_combination_to_int_vector "" 2))
	   
	   ;; rl+im for complex in most of these cases
	   (int gsl_dft_complex_forward (double* size_t size_t double*))
	   (int gsl_dft_complex_backward (double* size_t size_t double*))
	   (int gsl_dft_complex_inverse (double* size_t size_t double*))
	   (int gsl_dft_complex_transform (double* size_t size_t double* int))
	   (int gsl_fft_complex_radix2_forward (double* size_t size_t))
	   (int gsl_fft_complex_radix2_backward (double* size_t size_t))
	   (int gsl_fft_complex_radix2_inverse (double* size_t size_t))
	   (int gsl_fft_complex_radix2_transform (double* size_t size_t int))
	   (int gsl_fft_complex_radix2_dif_forward (double* size_t size_t))
	   (int gsl_fft_complex_radix2_dif_backward (double* size_t size_t))
	   (int gsl_fft_complex_radix2_dif_inverse (double* size_t size_t))
	   (int gsl_fft_complex_radix2_dif_transform (double* size_t size_t int))
	   (gsl_fft_complex_wavetable* gsl_fft_complex_wavetable_alloc (size_t))
	   (void gsl_fft_complex_wavetable_free (gsl_fft_complex_wavetable*))
	   (gsl_fft_complex_workspace* gsl_fft_complex_workspace_alloc (size_t))
	   (void gsl_fft_complex_workspace_free (gsl_fft_complex_workspace*))
	   (int gsl_fft_complex_memcpy (gsl_fft_complex_wavetable* gsl_fft_complex_wavetable*))
	   (int gsl_fft_complex_forward (double* size_t size_t gsl_fft_complex_wavetable* gsl_fft_complex_workspace*))
	   (int gsl_fft_complex_backward (double* size_t size_t gsl_fft_complex_wavetable* gsl_fft_complex_workspace*))
	   (int gsl_fft_complex_inverse (double* size_t size_t gsl_fft_complex_wavetable* gsl_fft_complex_workspace*))
	   (int gsl_fft_complex_transform (double* size_t size_t gsl_fft_complex_wavetable* gsl_fft_complex_workspace* int))
	   (int gsl_fft_real_radix2_transform (double* size_t size_t) )
	   (gsl_fft_real_wavetable* gsl_fft_real_wavetable_alloc (size_t))
	   (void gsl_fft_real_wavetable_free (gsl_fft_real_wavetable*))
	   (gsl_fft_real_workspace* gsl_fft_real_workspace_alloc (size_t))
	   (void gsl_fft_real_workspace_free (gsl_fft_real_workspace*))
	   (int gsl_fft_real_transform (double* size_t size_t gsl_fft_real_wavetable* gsl_fft_real_workspace*))
	   (int gsl_fft_real_unpack (double* double* size_t size_t))
	   
	   
	   (gsl_eigen_symm_workspace* gsl_eigen_symm_alloc (size_t))
	   (void gsl_eigen_symm_free (gsl_eigen_symm_workspace*))
	   (int gsl_eigen_symm (gsl_matrix* gsl_vector* gsl_eigen_symm_workspace*))
	   (gsl_eigen_symmv_workspace* gsl_eigen_symmv_alloc (size_t))
	   (void gsl_eigen_symmv_free (gsl_eigen_symmv_workspace*))
	   (int gsl_eigen_symmv (gsl_matrix* gsl_vector* gsl_matrix* gsl_eigen_symmv_workspace*))
	   (gsl_eigen_herm_workspace* gsl_eigen_herm_alloc (size_t))
	   (void gsl_eigen_herm_free (gsl_eigen_herm_workspace*))
	   (int gsl_eigen_herm (gsl_matrix_complex* gsl_vector* gsl_eigen_herm_workspace*))
	   (gsl_eigen_hermv_workspace* gsl_eigen_hermv_alloc (size_t))
	   (void gsl_eigen_hermv_free (gsl_eigen_hermv_workspace*))
	   (int gsl_eigen_hermv (gsl_matrix_complex* gsl_vector* gsl_matrix_complex* gsl_eigen_hermv_workspace*))
	   (gsl_eigen_francis_workspace* gsl_eigen_francis_alloc (void))
	   (void gsl_eigen_francis_free (gsl_eigen_francis_workspace*))
	   (void gsl_eigen_francis_T (int gsl_eigen_francis_workspace*))
	   (int gsl_eigen_francis (gsl_matrix* gsl_vector_complex* gsl_eigen_francis_workspace*))
	   (int gsl_eigen_francis_Z (gsl_matrix* gsl_vector_complex* gsl_matrix* gsl_eigen_francis_workspace*))
	   (gsl_eigen_nonsymm_workspace* gsl_eigen_nonsymm_alloc (size_t))
	   (void gsl_eigen_nonsymm_free (gsl_eigen_nonsymm_workspace*))
	   (void gsl_eigen_nonsymm_params (int int gsl_eigen_nonsymm_workspace*))
	   (int gsl_eigen_nonsymm (gsl_matrix* gsl_vector_complex* gsl_eigen_nonsymm_workspace*))
	   (int gsl_eigen_nonsymm_Z (gsl_matrix* gsl_vector_complex* gsl_matrix* gsl_eigen_nonsymm_workspace*))
	   (gsl_eigen_nonsymmv_workspace* gsl_eigen_nonsymmv_alloc (size_t))
	   (void gsl_eigen_nonsymmv_free (gsl_eigen_nonsymmv_workspace*))
	   (reader-cond ((>= gsl-version 1.15) (void gsl_eigen_nonsymmv_params (int gsl_eigen_nonsymmv_workspace*))))
	   (int gsl_eigen_nonsymmv (gsl_matrix* gsl_vector_complex* gsl_matrix_complex* gsl_eigen_nonsymmv_workspace*))
	   (int gsl_eigen_nonsymmv_Z (gsl_matrix* gsl_vector_complex* gsl_matrix_complex* gsl_matrix* gsl_eigen_nonsymmv_workspace*))
	   (gsl_eigen_gensymm_workspace* gsl_eigen_gensymm_alloc (size_t))
	   (void gsl_eigen_gensymm_free (gsl_eigen_gensymm_workspace*))
	   (int gsl_eigen_gensymm (gsl_matrix* gsl_matrix* gsl_vector* gsl_eigen_gensymm_workspace*))
	   (int gsl_eigen_gensymm_standardize (gsl_matrix* gsl_matrix*))
	   (gsl_eigen_gensymmv_workspace* gsl_eigen_gensymmv_alloc (size_t))
	   (void gsl_eigen_gensymmv_free (gsl_eigen_gensymmv_workspace*))
	   (int gsl_eigen_gensymmv (gsl_matrix* gsl_matrix* gsl_vector* gsl_matrix* gsl_eigen_gensymmv_workspace*))
	   (gsl_eigen_genherm_workspace* gsl_eigen_genherm_alloc (size_t))
	   (void gsl_eigen_genherm_free (gsl_eigen_genherm_workspace*))
	   (int gsl_eigen_genherm (gsl_matrix_complex* gsl_matrix_complex* gsl_vector* gsl_eigen_genherm_workspace*))
	   (int gsl_eigen_genherm_standardize (gsl_matrix_complex* gsl_matrix_complex*))
	   (gsl_eigen_genhermv_workspace* gsl_eigen_genhermv_alloc (size_t))
	   (void gsl_eigen_genhermv_free (gsl_eigen_genhermv_workspace*))
	   (int gsl_eigen_genhermv (gsl_matrix_complex* gsl_matrix_complex* gsl_vector* gsl_matrix_complex* gsl_eigen_genhermv_workspace*))
	   (gsl_eigen_gen_workspace* gsl_eigen_gen_alloc (size_t))
	   (void gsl_eigen_gen_free (gsl_eigen_gen_workspace*))
	   (void gsl_eigen_gen_params (int int int gsl_eigen_gen_workspace*))
	   (int gsl_eigen_gen (gsl_matrix* gsl_matrix* gsl_vector_complex* gsl_vector* gsl_eigen_gen_workspace*))
	   (int gsl_eigen_gen_QZ (gsl_matrix* gsl_matrix* gsl_vector_complex* gsl_vector* gsl_matrix* gsl_matrix* gsl_eigen_gen_workspace*))
	   (gsl_eigen_genv_workspace* gsl_eigen_genv_alloc (size_t))
	   (void gsl_eigen_genv_free (gsl_eigen_genv_workspace*))
	   (int gsl_eigen_genv (gsl_matrix* gsl_matrix* gsl_vector_complex* gsl_vector* gsl_matrix_complex* gsl_eigen_genv_workspace*))
	   (int gsl_eigen_genv_QZ (gsl_matrix* gsl_matrix* gsl_vector_complex* gsl_vector* gsl_matrix_complex* gsl_matrix* gsl_matrix* gsl_eigen_genv_workspace*))
	   (int gsl_eigen_symmv_sort (gsl_vector* gsl_matrix* int))
	   (int gsl_eigen_hermv_sort (gsl_vector* gsl_matrix_complex* int))
	   (int gsl_eigen_nonsymmv_sort (gsl_vector_complex* gsl_matrix_complex* int))
	   (int gsl_eigen_gensymmv_sort (gsl_vector* gsl_matrix* int))
	   (int gsl_eigen_genhermv_sort (gsl_vector* gsl_matrix_complex* int))
	   (int gsl_eigen_genv_sort (gsl_vector_complex* gsl_vector* gsl_matrix_complex* int))
	   (int gsl_schur_gen_eigvals (gsl_matrix* gsl_matrix* double* double* double* double* double*))
	   (int gsl_schur_solve_equation (double gsl_matrix* double double double gsl_vector* gsl_vector* double* double* double))
	   (int gsl_schur_solve_equation_z (double gsl_matrix* gsl_complex* double double gsl_vector_complex* gsl_vector_complex* double* double* double))
	   (int gsl_eigen_invert_jacobi (gsl_matrix* gsl_matrix* int))
	   (in-C "static s7_pointer g_gsl_eigen_jacobi(s7_scheme *sc, s7_pointer args)
                  {
                    unsigned int ref_arg = 0;
                    return(s7_make_integer(sc, (s7_int)gsl_eigen_jacobi((gsl_matrix*)s7_c_pointer(s7_car(args)), (gsl_vector*)s7_c_pointer(s7_cadr(args)),
                                               (gsl_matrix*)s7_c_pointer(s7_caddr(args)), (int)s7_integer(s7_cadddr(args)), &ref_arg)));
                  }")
	   (C-function ("gsl_eigen_jacobi" g_gsl_eigen_jacobi "" 4))
	   
	   (void gsl_error (char* char* int int))
	   (void gsl_stream_printf (char* char* int char*))
	   (char* gsl_strerror (int))
	   (gsl_error_handler_t* gsl_set_error_handler (gsl_error_handler_t*))
	   (gsl_error_handler_t* gsl_set_error_handler_off (void))
	   (gsl_stream_handler_t* gsl_set_stream_handler (gsl_stream_handler_t*))
	   (FILE* gsl_set_stream (FILE*))
	   
	   (int gsl_fit_linear (double* size_t double* size_t size_t double* double* double* double* double* double*))
	   (int gsl_fit_wlinear (double* size_t double* size_t double* size_t size_t double* double* double* double* double* double*))
	   (int gsl_fit_linear_est (double double double double double double double* double*))
	   (int gsl_fit_mul (double* size_t double* size_t size_t double* double* double*))
	   (int gsl_fit_wmul (double* size_t double* size_t double* size_t size_t double* double* double*))
	   (int gsl_fit_mul_est (double double double double* double*))
	   
	   (gsl_histogram* gsl_histogram_alloc (size_t))
	   (gsl_histogram* gsl_histogram_calloc (size_t))
	   (gsl_histogram* gsl_histogram_calloc_uniform (size_t double double))
	   (void gsl_histogram_free (gsl_histogram*))
	   (int gsl_histogram_increment (gsl_histogram* double))
	   (int gsl_histogram_accumulate (gsl_histogram* double double))
	   (int gsl_histogram_find (gsl_histogram* double size_t*))
	   (double gsl_histogram_get (gsl_histogram* size_t))
	   (int gsl_histogram_get_range (gsl_histogram* size_t double* double*))
	   (double gsl_histogram_max (gsl_histogram*))
	   (double gsl_histogram_min (gsl_histogram*))
	   (size_t gsl_histogram_bins (gsl_histogram*))
	   (void gsl_histogram_reset (gsl_histogram*))
	   (gsl_histogram* gsl_histogram_calloc_range (size_t double*))
	   (int gsl_histogram_set_ranges (gsl_histogram* double* size_t))
	   (int gsl_histogram_set_ranges_uniform (gsl_histogram* double double))
	   (int gsl_histogram_memcpy (gsl_histogram* gsl_histogram*))
	   (gsl_histogram* gsl_histogram_clone (gsl_histogram*))
	   (double gsl_histogram_max_val (gsl_histogram*))
	   (size_t gsl_histogram_max_bin (gsl_histogram*))
	   (double gsl_histogram_min_val (gsl_histogram*))
	   (size_t gsl_histogram_min_bin (gsl_histogram*))
	   (int gsl_histogram_equal_bins_p (gsl_histogram* gsl_histogram*))
	   (int gsl_histogram_add (gsl_histogram* gsl_histogram*))
	   (int gsl_histogram_sub (gsl_histogram* gsl_histogram*))
	   (int gsl_histogram_mul (gsl_histogram* gsl_histogram*))
	   (int gsl_histogram_div (gsl_histogram* gsl_histogram*))
	   (int gsl_histogram_scale (gsl_histogram* double))
	   (int gsl_histogram_shift (gsl_histogram* double))
	   (double gsl_histogram_sigma (gsl_histogram*))
	   (double gsl_histogram_mean (gsl_histogram*))
	   (double gsl_histogram_sum (gsl_histogram*))
	   (int gsl_histogram_fwrite (FILE* gsl_histogram*) )
	   (int gsl_histogram_fread (FILE* gsl_histogram*))
	   (int gsl_histogram_fprintf (FILE* gsl_histogram* char* char*))
	   (int gsl_histogram_fscanf (FILE* gsl_histogram*))
	   (gsl_histogram_pdf* gsl_histogram_pdf_alloc (size_t))
	   (int gsl_histogram_pdf_init (gsl_histogram_pdf* gsl_histogram*))
	   (void gsl_histogram_pdf_free (gsl_histogram_pdf*))
	   (double gsl_histogram_pdf_sample (gsl_histogram_pdf* double))
	   (gsl_histogram2d* gsl_histogram2d_alloc (size_t size_t))
	   (gsl_histogram2d* gsl_histogram2d_calloc (size_t size_t))
	   (gsl_histogram2d* gsl_histogram2d_calloc_uniform (size_t size_t double double double double))
	   (void gsl_histogram2d_free (gsl_histogram2d*))
	   (int gsl_histogram2d_increment (gsl_histogram2d* double double))
	   (int gsl_histogram2d_accumulate (gsl_histogram2d* double double double))
	   (int gsl_histogram2d_find (gsl_histogram2d* double double size_t* size_t*))
	   (double gsl_histogram2d_get (gsl_histogram2d* size_t size_t))
	   (int gsl_histogram2d_get_xrange (gsl_histogram2d* size_t double* double*))
	   (int gsl_histogram2d_get_yrange (gsl_histogram2d* size_t double* double*))
	   (double gsl_histogram2d_xmax (gsl_histogram2d*))
	   (double gsl_histogram2d_xmin (gsl_histogram2d*))
	   (size_t gsl_histogram2d_nx (gsl_histogram2d*))
	   (double gsl_histogram2d_ymax (gsl_histogram2d*))
	   (double gsl_histogram2d_ymin (gsl_histogram2d*))
	   (size_t gsl_histogram2d_ny (gsl_histogram2d*))
	   (void gsl_histogram2d_reset (gsl_histogram2d*))
	   (gsl_histogram2d* gsl_histogram2d_calloc_range (size_t size_t double* double*))
	   (int gsl_histogram2d_set_ranges_uniform (gsl_histogram2d* double double double double))
	   (int gsl_histogram2d_memcpy (gsl_histogram2d* gsl_histogram2d*))
	   (gsl_histogram2d* gsl_histogram2d_clone (gsl_histogram2d*))
	   (double gsl_histogram2d_max_val (gsl_histogram2d*))
	   (void gsl_histogram2d_max_bin (gsl_histogram2d* size_t* size_t*))
	   (double gsl_histogram2d_min_val (gsl_histogram2d*))
	   (void gsl_histogram2d_min_bin (gsl_histogram2d* size_t* size_t*))
	   (double gsl_histogram2d_xmean (gsl_histogram2d*))
	   (double gsl_histogram2d_ymean (gsl_histogram2d*))
	   (double gsl_histogram2d_xsigma (gsl_histogram2d*))
	   (double gsl_histogram2d_ysigma (gsl_histogram2d*))
	   (double gsl_histogram2d_cov (gsl_histogram2d*))
	   (double gsl_histogram2d_sum (gsl_histogram2d*))
	   (int gsl_histogram2d_equal_bins_p (gsl_histogram2d* gsl_histogram2d*) )
	   (int gsl_histogram2d_add (gsl_histogram2d* gsl_histogram2d*))
	   (int gsl_histogram2d_sub (gsl_histogram2d* gsl_histogram2d*))
	   (int gsl_histogram2d_mul (gsl_histogram2d* gsl_histogram2d*))
	   (int gsl_histogram2d_div (gsl_histogram2d* gsl_histogram2d*))
	   (int gsl_histogram2d_scale (gsl_histogram2d* double))
	   (int gsl_histogram2d_shift (gsl_histogram2d* double))
	   (int gsl_histogram2d_fwrite (FILE* gsl_histogram2d*) )
	   (int gsl_histogram2d_fread (FILE* gsl_histogram2d*))
	   (int gsl_histogram2d_fprintf (FILE* gsl_histogram2d* char* char*))
	   (int gsl_histogram2d_fscanf (FILE* gsl_histogram2d*))
	   (gsl_histogram2d_pdf* gsl_histogram2d_pdf_alloc (size_t size_t))
	   (int gsl_histogram2d_pdf_init (gsl_histogram2d_pdf* gsl_histogram2d*))
	   (void gsl_histogram2d_pdf_free (gsl_histogram2d_pdf*))
	   (int gsl_histogram2d_pdf_sample (gsl_histogram2d_pdf* double double double* double*))
	   
					;(void gsl_ieee_printf_double (double*) ) ; these are ridiculous
					;(void gsl_ieee_fprintf_double (FILE* double*) )
					;(void gsl_ieee_double_to_rep (double* gsl_ieee_double_rep*) )
	   (void gsl_ieee_env_setup (void) ) ; looks for GSL_IEEE_MODE home var
					;(int gsl_ieee_read_mode_string (char* int* int* int*) ) ; int by ref
	   (int gsl_ieee_set_mode (int int int) )
	   
	   (in-C "static s7_pointer g_gsl_deriv_central(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_function gsl_f;
                    make_gsl_function(s7_car(args));
                    return(s7_make_integer(sc, gsl_deriv_central(&gsl_f, s7_real(s7_cadr(args)), s7_real(s7_caddr(args)),
                                               (double *)s7_c_pointer(s7_cadddr(args)), (double *)s7_c_pointer(s7_list_ref(sc, args, 4)))));
                  }
                 static s7_pointer g_gsl_deriv_backward(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_function gsl_f;
                    make_gsl_function(s7_car(args));
                    return(s7_make_integer(sc, gsl_deriv_backward(&gsl_f, s7_real(s7_cadr(args)), s7_real(s7_caddr(args)),
                                               (double *)s7_c_pointer(s7_cadddr(args)), (double *)s7_c_pointer(s7_list_ref(sc, args, 4)))));
                  }
                 static s7_pointer g_gsl_deriv_forward(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_function gsl_f;
                    make_gsl_function(s7_car(args));
                    return(s7_make_integer(sc, gsl_deriv_forward(&gsl_f, s7_real(s7_cadr(args)), s7_real(s7_caddr(args)),
                                               (double *)s7_c_pointer(s7_cadddr(args)), (double *)s7_c_pointer(s7_list_ref(sc, args, 4)))));
                  }
                  ")
	   (C-function ("gsl_deriv_central" g_gsl_deriv_central "" 5))
	   (C-function ("gsl_deriv_backward" g_gsl_deriv_backward "" 5))
	   (C-function ("gsl_deriv_forward" g_gsl_deriv_forward "" 5))
	   
	   
	   (gsl_integration_workspace* gsl_integration_workspace_alloc (size_t))
	   (void gsl_integration_workspace_free (gsl_integration_workspace*))
	   (gsl_integration_qaws_table* gsl_integration_qaws_table_alloc (double double int int))
	   (int gsl_integration_qaws_table_set (gsl_integration_qaws_table* double double int int))
	   (void gsl_integration_qaws_table_free (gsl_integration_qaws_table*))
	   (gsl_integration_qawo_table* gsl_integration_qawo_table_alloc (double double int size_t))
	   (int gsl_integration_qawo_table_set (gsl_integration_qawo_table* double double int))
	   (int gsl_integration_qawo_table_set_length (gsl_integration_qawo_table* double))
	   (void gsl_integration_qawo_table_free (gsl_integration_qawo_table*))

	   (in-C "#define Integration(Name) \
                    static s7_pointer g_gsl_integration_ ## Name (s7_scheme *sc, s7_pointer args)   \
                  {                                                                                 \
                    gsl_function gsl_f;                                                             \
                    make_gsl_function(s7_car(args));                                                \
                    gsl_integration_ ## Name (&gsl_f, s7_real(s7_cadr(args)), s7_real(s7_caddr(args)), \
                                         (double *)s7_c_pointer(s7_list_ref(sc, args, 3)), (double *)s7_c_pointer(s7_list_ref(sc, args, 4)), \
                                         (double *)s7_c_pointer(s7_list_ref(sc, args, 5)), (double *)s7_c_pointer(s7_list_ref(sc, args, 6))); \
                    return(s7_car(args));                                                          \
                  }
                 Integration(qk15)
                 Integration(qk21)
                 Integration(qk31)
                 Integration(qk41)
                 Integration(qk51)
                 Integration(qk61)
                 ")
	   (C-function ("gsl_integration_qk15" g_gsl_integration_qk15 "" 7))
	   (C-function ("gsl_integration_qk21" g_gsl_integration_qk21 "" 7))
	   (C-function ("gsl_integration_qk31" g_gsl_integration_qk31 "" 7))
	   (C-function ("gsl_integration_qk41" g_gsl_integration_qk41 "" 7))
	   (C-function ("gsl_integration_qk51" g_gsl_integration_qk51 "" 7))
	   (C-function ("gsl_integration_qk61" g_gsl_integration_qk61 "" 7))

	   (in-C "static s7_pointer g_gsl_integration_qcheb(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_function gsl_f; make_gsl_function(s7_car(args));
                    gsl_integration_qcheb(&gsl_f, s7_real(s7_cadr(args)), s7_real(s7_caddr(args)),
                                               (double *)s7_c_pointer(s7_cadddr(args)), (double *)s7_c_pointer(s7_list_ref(sc, args, 4)));
                    return(s7_car(args));
                  }
                  static s7_pointer g_gsl_integration_qng(s7_scheme *sc, s7_pointer args)
                  {
                    size_t ref;
                    gsl_function gsl_f; make_gsl_function(s7_car(args));
                    return(s7_make_integer(sc, gsl_integration_qng(&gsl_f, 
                                               s7_real(s7_list_ref(sc, args, 1)), s7_real(s7_list_ref(sc, args, 2)),
                                               s7_real(s7_list_ref(sc, args, 3)), s7_real(s7_list_ref(sc, args, 4)),
                                               (double *)s7_c_pointer(s7_list_ref(sc, args, 5)), (double *)s7_c_pointer(s7_list_ref(sc, args, 6)),
                                               &ref)));
                  }
                  static s7_pointer g_gsl_integration_qag(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_function gsl_f; make_gsl_function(s7_car(args));
                    return(s7_make_integer(sc, gsl_integration_qag(&gsl_f, 
                                               s7_real(s7_list_ref(sc, args, 1)), s7_real(s7_list_ref(sc, args, 2)),
                                               s7_real(s7_list_ref(sc, args, 3)), s7_real(s7_list_ref(sc, args, 4)),
                                               (size_t)s7_integer(s7_list_ref(sc, args, 5)), (int)s7_integer(s7_list_ref(sc, args, 6)), 
                                               (gsl_integration_workspace *)s7_c_pointer(s7_list_ref(sc, args, 7)),
                                               (double *)s7_c_pointer(s7_list_ref(sc, args, 8)), (double *)s7_c_pointer(s7_list_ref(sc, args, 9)))));
                  }
                  static s7_pointer g_gsl_integration_qagi(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_function gsl_f; make_gsl_function(s7_car(args));
                    return(s7_make_integer(sc, gsl_integration_qagi(&gsl_f, 
                                               s7_real(s7_list_ref(sc, args, 1)), s7_real(s7_list_ref(sc, args, 2)),
                                               (size_t)s7_integer(s7_list_ref(sc, args, 3)),
                                               (gsl_integration_workspace *)s7_c_pointer(s7_list_ref(sc, args, 4)),
                                               (double *)s7_c_pointer(s7_list_ref(sc, args, 5)), (double *)s7_c_pointer(s7_list_ref(sc, args, 6)))));
                  }
                  static s7_pointer g_gsl_integration_qagiu(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_function gsl_f; make_gsl_function(s7_car(args));
                    return(s7_make_integer(sc, gsl_integration_qagiu(&gsl_f, 
                                               s7_real(s7_list_ref(sc, args, 1)), s7_real(s7_list_ref(sc, args, 2)), s7_real(s7_list_ref(sc, args, 3)),
                                               (size_t)s7_integer(s7_list_ref(sc, args, 4)),
                                               (gsl_integration_workspace *)s7_c_pointer(s7_list_ref(sc, args, 5)),
                                               (double *)s7_c_pointer(s7_list_ref(sc, args, 6)), (double *)s7_c_pointer(s7_list_ref(sc, args, 7)))));
                  }
                  static s7_pointer g_gsl_integration_qagil(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_function gsl_f; make_gsl_function(s7_car(args));
                    return(s7_make_integer(sc, gsl_integration_qagil(&gsl_f, 
                                               s7_real(s7_list_ref(sc, args, 1)), s7_real(s7_list_ref(sc, args, 2)), s7_real(s7_list_ref(sc, args, 3)),
                                               (size_t)s7_integer(s7_list_ref(sc, args, 4)),
                                               (gsl_integration_workspace *)s7_c_pointer(s7_list_ref(sc, args, 5)),
                                               (double *)s7_c_pointer(s7_list_ref(sc, args, 6)), (double *)s7_c_pointer(s7_list_ref(sc, args, 7)))));
                  }
                  static s7_pointer g_gsl_integration_qags(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_function gsl_f; make_gsl_function(s7_car(args));
                    return(s7_make_integer(sc, gsl_integration_qags(&gsl_f, 
                                               s7_real(s7_list_ref(sc, args, 1)), s7_real(s7_list_ref(sc, args, 2)), 
                                               s7_real(s7_list_ref(sc, args, 3)), s7_real(s7_list_ref(sc, args, 4)),
                                               (size_t)s7_integer(s7_list_ref(sc, args, 5)),
                                               (gsl_integration_workspace *)s7_c_pointer(s7_list_ref(sc, args, 6)),
                                               (double *)s7_c_pointer(s7_list_ref(sc, args, 7)), (double *)s7_c_pointer(s7_list_ref(sc, args, 8)))));
                  }
                  static s7_pointer g_gsl_integration_qagp(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_function gsl_f; make_gsl_function(s7_car(args));
                    return(s7_make_integer(sc, gsl_integration_qagp(&gsl_f, (double *)s7_c_pointer(s7_list_ref(sc, args, 1)),
                                               (size_t)s7_integer(s7_list_ref(sc, args, 2)),
                                               s7_real(s7_list_ref(sc, args, 3)), s7_real(s7_list_ref(sc, args, 4)), 
                                               (size_t)s7_integer(s7_list_ref(sc, args, 5)),
                                               (gsl_integration_workspace *)s7_c_pointer(s7_list_ref(sc, args, 6)),
                                               (double *)s7_c_pointer(s7_list_ref(sc, args, 7)), (double *)s7_c_pointer(s7_list_ref(sc, args, 8)))));
                  }
                  static s7_pointer g_gsl_integration_qawc(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_function gsl_f; make_gsl_function(s7_car(args));
                    return(s7_make_integer(sc, gsl_integration_qawc(&gsl_f, 
                                               s7_real(s7_list_ref(sc, args, 1)), s7_real(s7_list_ref(sc, args, 2)), 
                                               s7_real(s7_list_ref(sc, args, 3)), s7_real(s7_list_ref(sc, args, 4)), 
                                               s7_real(s7_list_ref(sc, args, 5)), 
                                               (size_t)s7_integer(s7_list_ref(sc, args, 6)),
                                               (gsl_integration_workspace *)s7_c_pointer(s7_list_ref(sc, args, 7)),
                                               (double *)s7_c_pointer(s7_list_ref(sc, args, 8)), (double *)s7_c_pointer(s7_list_ref(sc, args, 9)))));
                  }
                  static s7_pointer g_gsl_integration_qaws(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_function gsl_f; make_gsl_function(s7_car(args));
                    return(s7_make_integer(sc, gsl_integration_qaws(&gsl_f, 
                                               s7_real(s7_list_ref(sc, args, 1)), s7_real(s7_list_ref(sc, args, 2)), 
                                               (gsl_integration_qaws_table *)s7_c_pointer(s7_list_ref(sc, args, 3)),
                                               s7_real(s7_list_ref(sc, args, 4)), s7_real(s7_list_ref(sc, args, 5)), 
                                               (size_t)s7_integer(s7_list_ref(sc, args, 6)),
                                               (gsl_integration_workspace *)s7_c_pointer(s7_list_ref(sc, args, 7)),
                                               (double *)s7_c_pointer(s7_list_ref(sc, args, 8)), (double *)s7_c_pointer(s7_list_ref(sc, args, 9)))));
                  }
                  static s7_pointer g_gsl_integration_qawo(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_function gsl_f; make_gsl_function(s7_car(args));
                    return(s7_make_integer(sc, gsl_integration_qawo(&gsl_f, 
                                               s7_real(s7_list_ref(sc, args, 1)), s7_real(s7_list_ref(sc, args, 2)), 
                                               s7_real(s7_list_ref(sc, args, 3)), 
                                               (size_t)s7_integer(s7_list_ref(sc, args, 4)),
                                               (gsl_integration_workspace *)s7_c_pointer(s7_list_ref(sc, args, 5)),
                                               (gsl_integration_qawo_table *)s7_c_pointer(s7_list_ref(sc, args, 6)),
                                               (double *)s7_c_pointer(s7_list_ref(sc, args, 7)), (double *)s7_c_pointer(s7_list_ref(sc, args, 8)))));
                  }
                  static s7_pointer g_gsl_integration_qawf(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_function gsl_f; make_gsl_function(s7_car(args));
                    return(s7_make_integer(sc, gsl_integration_qawf(&gsl_f, 
                                               s7_real(s7_list_ref(sc, args, 1)), s7_real(s7_list_ref(sc, args, 2)), 
                                               (size_t)s7_integer(s7_list_ref(sc, args, 3)),
                                               (gsl_integration_workspace *)s7_c_pointer(s7_list_ref(sc, args, 4)),
                                               (gsl_integration_workspace *)s7_c_pointer(s7_list_ref(sc, args, 5)),
                                               (gsl_integration_qawo_table *)s7_c_pointer(s7_list_ref(sc, args, 6)),
                                               (double *)s7_c_pointer(s7_list_ref(sc, args, 7)), (double *)s7_c_pointer(s7_list_ref(sc, args, 8)))));
                  }
                  static s7_pointer g_gsl_integration_glfixed(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_function gsl_f; make_gsl_function(s7_car(args));
                    return(s7_make_real(sc, gsl_integration_glfixed(&gsl_f, 
                                               s7_real(s7_list_ref(sc, args, 1)), s7_real(s7_list_ref(sc, args, 2)), 
                                               (const gsl_integration_glfixed_table *)s7_c_pointer(s7_list_ref(sc, args, 3)))));
                  }
                  static s7_pointer g_gsl_integration_qk(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_function gsl_f; make_gsl_function(s7_list_ref(sc, args, 6));
                    gsl_integration_qk((int)s7_integer(s7_car(args)),
                                               (double *)s7_c_pointer(s7_list_ref(sc, args, 1)), (double *)s7_c_pointer(s7_list_ref(sc, args, 2)),
                                               (double *)s7_c_pointer(s7_list_ref(sc, args, 3)), (double *)s7_c_pointer(s7_list_ref(sc, args, 4)),
                                               (double *)s7_c_pointer(s7_list_ref(sc, args, 5)),
                                               &gsl_f, 
                                               s7_real(s7_list_ref(sc, args, 7)), s7_real(s7_list_ref(sc, args, 8)), 
                                               (double *)s7_c_pointer(s7_list_ref(sc, args, 9)), (double *)s7_c_pointer(s7_list_ref(sc, args, 10)),
                                               (double *)s7_c_pointer(s7_list_ref(sc, args, 11)), (double *)s7_c_pointer(s7_list_ref(sc, args, 12)));
                    return(s7_car(args));
                  }")
	   (C-function ("gsl_integration_qcheb" g_gsl_integration_qcheb "" 5))
	   (C-function ("gsl_integration_qng" g_gsl_integration_qng "" 7))
	   (C-function ("gsl_integration_qag" g_gsl_integration_qag "" 10))
	   (C-function ("gsl_integration_qagi" g_gsl_integration_qagi "" 7))
	   (C-function ("gsl_integration_qagiu" g_gsl_integration_qagiu "" 8))
	   (C-function ("gsl_integration_qagil" g_gsl_integration_qagil "" 8))
	   (C-function ("gsl_integration_qags" g_gsl_integration_qags "" 9))
	   (C-function ("gsl_integration_qagp" g_gsl_integration_qagp "" 9))
	   (C-function ("gsl_integration_qawc" g_gsl_integration_qawc "" 10))
	   (C-function ("gsl_integration_qaws" g_gsl_integration_qaws "" 10))
	   (C-function ("gsl_integration_qawo" g_gsl_integration_qawo "" 9))
	   (C-function ("gsl_integration_qawf" g_gsl_integration_qawf "" 9))
	   (C-function ("gsl_integration_qk" g_gsl_integration_qk "" 13))
	   (C-function ("gsl_integration_glfixed" g_gsl_integration_glfixed "" 4))

	   (gsl_integration_glfixed_table* gsl_integration_glfixed_table_alloc (size_t))
	   (void gsl_integration_glfixed_table_free (gsl_integration_glfixed_table*))
	   (reader-cond ((>= gsl-version 1.15) 
			 (int gsl_integration_glfixed_point (double double size_t double* double* gsl_integration_glfixed_table*))
			 (gsl_integration_cquad_workspace* gsl_integration_cquad_workspace_alloc (size_t))
			 (void gsl_integration_cquad_workspace_free (gsl_integration_cquad_workspace*))))
	   
	   (int gsl_linalg_matmult (gsl_matrix* gsl_matrix* gsl_matrix*))
	   (int gsl_linalg_matmult_mod (gsl_matrix* int gsl_matrix* int gsl_matrix*))
	   (int gsl_linalg_exponential_ss (gsl_matrix* gsl_matrix* int))
	   (double gsl_linalg_householder_transform (gsl_vector*))
	   (int gsl_linalg_householder_hm (double gsl_vector* gsl_matrix*))
	   (int gsl_linalg_householder_mh (double gsl_vector* gsl_matrix*))
	   (int gsl_linalg_householder_hv (double gsl_vector* gsl_vector*))
	   (int gsl_linalg_householder_hm1 (double gsl_matrix*))
	   
	   (in-C "static s7_pointer g_gsl_linalg_complex_householder_transform(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_complex g;
                    g = gsl_linalg_complex_householder_transform((gsl_vector_complex *)s7_c_pointer(s7_car(args)));
                    return(GSL_TO_S7_COMPLEX(sc, g));
                  }
                  static s7_pointer g_gsl_linalg_complex_householder_hm(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_complex g;
                    S7_TO_GSL_COMPLEX(s7_car(args), g);
                    return(s7_make_integer(sc, gsl_linalg_complex_householder_hm(g, (gsl_vector_complex *)s7_c_pointer(s7_cadr(args)),
                                                                                    (gsl_matrix_complex *)s7_c_pointer(s7_caddr(args)))));
                  }
                  static s7_pointer g_gsl_linalg_complex_householder_mh(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_complex g;
                    S7_TO_GSL_COMPLEX(s7_car(args), g);
                    return(s7_make_integer(sc, gsl_linalg_complex_householder_mh(g, (gsl_vector_complex *)s7_c_pointer(s7_cadr(args)),
                                                                                    (gsl_matrix_complex *)s7_c_pointer(s7_caddr(args)))));
                  }
                  static s7_pointer g_gsl_linalg_complex_householder_hv(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_complex g;
                    S7_TO_GSL_COMPLEX(s7_car(args), g);
                    return(s7_make_integer(sc, gsl_linalg_complex_householder_hv(g, (gsl_vector_complex *)s7_c_pointer(s7_cadr(args)),
                                                                                    (gsl_vector_complex *)s7_c_pointer(s7_caddr(args)))));
                  }
                  static s7_pointer g_gsl_linalg_complex_LU_det(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_complex g;
                    g = gsl_linalg_complex_LU_det((gsl_matrix_complex *)s7_c_pointer(s7_car(args)), (int)s7_integer(s7_cadr(args)));
                    return(GSL_TO_S7_COMPLEX(sc, g));
                  }
                  static s7_pointer g_gsl_linalg_complex_LU_sgndet(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_complex g;
                    g = gsl_linalg_complex_LU_sgndet((gsl_matrix_complex *)s7_c_pointer(s7_car(args)), (int)s7_integer(s7_cadr(args)));
                    return(GSL_TO_S7_COMPLEX(sc, g));
                  }
                  ")
	   (C-function ("gsl_linalg_complex_householder_transform" g_gsl_linalg_complex_householder_transform "" 1))
	   (C-function ("gsl_linalg_complex_householder_hm" g_gsl_linalg_complex_householder_hm "" 3))
	   (C-function ("gsl_linalg_complex_householder_mh" g_gsl_linalg_complex_householder_mh "" 3))
	   (C-function ("gsl_linalg_complex_householder_hv" g_gsl_linalg_complex_householder_hv "" 3))
	   (C-function ("gsl_linalg_complex_LU_det" g_gsl_linalg_complex_LU_det "" 2))
	   (C-function ("gsl_linalg_complex_LU_sgndet" g_gsl_linalg_complex_LU_sgndet "" 2))
	   
	   (int gsl_linalg_hessenberg_decomp (gsl_matrix* gsl_vector*))
	   (int gsl_linalg_hessenberg_unpack (gsl_matrix* gsl_vector* gsl_matrix*))
	   (int gsl_linalg_hessenberg_unpack_accum (gsl_matrix* gsl_vector* gsl_matrix*))
	   (int gsl_linalg_hessenberg_set_zero (gsl_matrix*))
	   (int gsl_linalg_hessenberg_submatrix (gsl_matrix* gsl_matrix* size_t gsl_vector*))
	   (int gsl_linalg_hessenberg (gsl_matrix* gsl_vector*))
	   (int gsl_linalg_hesstri_decomp (gsl_matrix* gsl_matrix* gsl_matrix* gsl_matrix* gsl_vector*))
	   (int gsl_linalg_SV_decomp (gsl_matrix* gsl_matrix* gsl_vector* gsl_vector*))
	   (int gsl_linalg_SV_decomp_mod (gsl_matrix* gsl_matrix* gsl_matrix* gsl_vector* gsl_vector*))
	   (int gsl_linalg_SV_decomp_jacobi (gsl_matrix* gsl_matrix* gsl_vector*))
	   (int gsl_linalg_SV_solve (gsl_matrix* gsl_matrix* gsl_vector* gsl_vector* gsl_vector*))
	   (reader-cond ((>= gsl-version 1.16) (int gsl_linalg_SV_leverage (gsl_matrix* gsl_vector*))))
	   (in-C "static s7_pointer g_gsl_linalg_LU_decomp(s7_scheme *sc, s7_pointer args)
                  {
                    int s = 0;
                    return(s7_make_integer(sc, (s7_int)gsl_linalg_LU_decomp((gsl_matrix *)s7_c_pointer(s7_car(args)),
                                               (gsl_permutation *)s7_c_pointer(s7_cadr(args)), &s)));
                  }")
	   (C-function ("gsl_linalg_LU_decomp" g_gsl_linalg_LU_decomp "" 2))
	   (int gsl_linalg_LU_solve (gsl_matrix* gsl_permutation* gsl_vector* gsl_vector*))
	   (int gsl_linalg_LU_svx (gsl_matrix* gsl_permutation* gsl_vector*))
	   (int gsl_linalg_LU_refine (gsl_matrix* gsl_matrix* gsl_permutation* gsl_vector* gsl_vector* gsl_vector*))
	   (int gsl_linalg_LU_invert (gsl_matrix* gsl_permutation* gsl_matrix*))
	   (double gsl_linalg_LU_det (gsl_matrix* int))
	   (double gsl_linalg_LU_lndet (gsl_matrix*))
	   (int gsl_linalg_LU_sgndet (gsl_matrix* int))
	   (in-C "static s7_pointer g_gsl_linalg_complex_LU_decomp(s7_scheme *sc, s7_pointer args)
                  {
                    int s = 0;
                    return(s7_make_integer(sc, (s7_int)gsl_linalg_complex_LU_decomp((gsl_matrix_complex *)s7_c_pointer(s7_car(args)),
                                               (gsl_permutation *)s7_c_pointer(s7_cadr(args)), &s)));
                  }")
	   (C-function ("gsl_linalg_complex_LU_decomp" g_gsl_linalg_complex_LU_decomp "" 2))
	   (int gsl_linalg_complex_LU_solve (gsl_matrix_complex* gsl_permutation* gsl_vector_complex* gsl_vector_complex*))
	   (int gsl_linalg_complex_LU_svx (gsl_matrix_complex* gsl_permutation* gsl_vector_complex*))
	   (int gsl_linalg_complex_LU_refine (gsl_matrix_complex* gsl_matrix_complex* gsl_permutation* gsl_vector_complex* gsl_vector_complex* gsl_vector_complex*))
	   (int gsl_linalg_complex_LU_invert (gsl_matrix_complex* gsl_permutation* gsl_matrix_complex*))
	   (double gsl_linalg_complex_LU_lndet (gsl_matrix_complex*))
	   (int gsl_linalg_QR_decomp (gsl_matrix* gsl_vector*))
	   (int gsl_linalg_QR_solve (gsl_matrix* gsl_vector* gsl_vector* gsl_vector*))
	   (int gsl_linalg_QR_svx (gsl_matrix* gsl_vector* gsl_vector*))
	   (int gsl_linalg_QR_lssolve (gsl_matrix* gsl_vector* gsl_vector* gsl_vector* gsl_vector*))
	   (int gsl_linalg_QR_QRsolve (gsl_matrix* gsl_matrix* gsl_vector* gsl_vector*))
	   (int gsl_linalg_QR_Rsolve (gsl_matrix* gsl_vector* gsl_vector*))
	   (int gsl_linalg_QR_Rsvx (gsl_matrix* gsl_vector*))
	   (int gsl_linalg_QR_update (gsl_matrix* gsl_matrix* gsl_vector* gsl_vector*))
	   (int gsl_linalg_QR_QTvec (gsl_matrix* gsl_vector* gsl_vector*))
	   (int gsl_linalg_QR_Qvec (gsl_matrix* gsl_vector* gsl_vector*))
	   (int gsl_linalg_QR_QTmat (gsl_matrix* gsl_vector* gsl_matrix*))
	   (int gsl_linalg_QR_unpack (gsl_matrix* gsl_vector* gsl_matrix* gsl_matrix*))
	   (int gsl_linalg_R_solve (gsl_matrix* gsl_vector* gsl_vector*))
	   (int gsl_linalg_R_svx (gsl_matrix* gsl_vector*))
	   (in-C "static s7_pointer g_gsl_linalg_QRPT_decomp(s7_scheme *sc, s7_pointer args)
                  {
                    int s = 0;
                    return(s7_make_integer(sc, (s7_int)gsl_linalg_QRPT_decomp((gsl_matrix *)s7_c_pointer(s7_car(args)),
                                               (gsl_vector *)s7_c_pointer(s7_cadr(args)), (gsl_permutation *)s7_c_pointer(s7_caddr(args)), &s,
                                               (gsl_vector *)s7_c_pointer(s7_cadddr(args)))));
                  }")
	   (C-function ("gsl_linalg_QRPT_decomp" g_gsl_linalg_QRPT_decomp "" 4))
	   (in-C "static s7_pointer g_gsl_linalg_QRPT_decomp2(s7_scheme *sc, s7_pointer args)
                  {
                    int s = 0;
                    return(s7_make_integer(sc, (s7_int)gsl_linalg_QRPT_decomp2((gsl_matrix *)s7_c_pointer(s7_car(args)), (gsl_matrix *)s7_c_pointer(s7_cadr(args)),
                                               (gsl_matrix *)s7_c_pointer(s7_caddr(args)),
                                               (gsl_vector *)s7_c_pointer(s7_cadddr(args)), (gsl_permutation *)s7_c_pointer(s7_list_ref(sc, args, 4)), &s,
                                               (gsl_vector *)s7_c_pointer(s7_list_ref(sc, args, 5)))));
                  }")
	   (C-function ("gsl_linalg_QRPT_decomp2" g_gsl_linalg_QRPT_decomp2 "" 6))
	   (int gsl_linalg_QRPT_solve (gsl_matrix* gsl_vector* gsl_permutation* gsl_vector* gsl_vector*))
	   (int gsl_linalg_QRPT_svx (gsl_matrix* gsl_vector* gsl_permutation* gsl_vector*))
	   (int gsl_linalg_QRPT_QRsolve (gsl_matrix* gsl_matrix* gsl_permutation* gsl_vector* gsl_vector*))
	   (int gsl_linalg_QRPT_Rsolve (gsl_matrix* gsl_permutation* gsl_vector* gsl_vector*))
	   (int gsl_linalg_QRPT_Rsvx (gsl_matrix* gsl_permutation* gsl_vector*))
	   (int gsl_linalg_QRPT_update (gsl_matrix* gsl_matrix* gsl_permutation* gsl_vector* gsl_vector*))
	   (int gsl_linalg_LQ_decomp (gsl_matrix* gsl_vector*))
	   (int gsl_linalg_LQ_solve_T (gsl_matrix* gsl_vector* gsl_vector* gsl_vector*))
	   (int gsl_linalg_LQ_svx_T (gsl_matrix* gsl_vector* gsl_vector*))
	   (int gsl_linalg_LQ_lssolve_T (gsl_matrix* gsl_vector* gsl_vector* gsl_vector* gsl_vector*))
	   (int gsl_linalg_LQ_Lsolve_T (gsl_matrix* gsl_vector* gsl_vector*))
	   (int gsl_linalg_LQ_Lsvx_T (gsl_matrix* gsl_vector*))
	   (int gsl_linalg_L_solve_T (gsl_matrix* gsl_vector* gsl_vector*))
	   (int gsl_linalg_LQ_vecQ (gsl_matrix* gsl_vector* gsl_vector*))
	   (int gsl_linalg_LQ_vecQT (gsl_matrix* gsl_vector* gsl_vector*))
	   (int gsl_linalg_LQ_unpack (gsl_matrix* gsl_vector* gsl_matrix* gsl_matrix*))
	   (int gsl_linalg_LQ_update (gsl_matrix* gsl_matrix* gsl_vector* gsl_vector*))
	   (int gsl_linalg_LQ_LQsolve (gsl_matrix* gsl_matrix* gsl_vector* gsl_vector*))
	   (in-C "static s7_pointer g_gsl_linalg_PTLQ_decomp(s7_scheme *sc, s7_pointer args)
                  {
                    int s = 0;
                    return(s7_make_integer(sc, (s7_int)gsl_linalg_PTLQ_decomp((gsl_matrix *)s7_c_pointer(s7_car(args)),
                                               (gsl_vector *)s7_c_pointer(s7_cadr(args)), (gsl_permutation *)s7_c_pointer(s7_caddr(args)), &s,
                                               (gsl_vector *)s7_c_pointer(s7_cadddr(args)))));
                  }")
	   (C-function ("gsl_linalg_PTLQ_decomp" g_gsl_linalg_PTLQ_decomp "" 4))
	   (in-C "static s7_pointer g_gsl_linalg_PTLQ_decomp2(s7_scheme *sc, s7_pointer args)
                  {
                    int s = 0;
                    return(s7_make_integer(sc, (s7_int)gsl_linalg_PTLQ_decomp2((gsl_matrix *)s7_c_pointer(s7_car(args)), (gsl_matrix *)s7_c_pointer(s7_cadr(args)),
                                               (gsl_matrix *)s7_c_pointer(s7_caddr(args)),
                                               (gsl_vector *)s7_c_pointer(s7_cadddr(args)), (gsl_permutation *)s7_c_pointer(s7_list_ref(sc, args, 4)), &s,
                                               (gsl_vector *)s7_c_pointer(s7_list_ref(sc, args, 5)))));
                  }")
	   (C-function ("gsl_linalg_PTLQ_decomp2" g_gsl_linalg_PTLQ_decomp2 "" 6))
	   (int gsl_linalg_PTLQ_solve_T (gsl_matrix* gsl_vector* gsl_permutation* gsl_vector* gsl_vector*))
	   (int gsl_linalg_PTLQ_svx_T (gsl_matrix* gsl_vector* gsl_permutation* gsl_vector*))
	   (int gsl_linalg_PTLQ_LQsolve_T (gsl_matrix* gsl_matrix* gsl_permutation* gsl_vector* gsl_vector*))
	   (int gsl_linalg_PTLQ_Lsolve_T (gsl_matrix* gsl_permutation* gsl_vector* gsl_vector*))
	   (int gsl_linalg_PTLQ_Lsvx_T (gsl_matrix* gsl_permutation* gsl_vector*))
	   (int gsl_linalg_PTLQ_update (gsl_matrix* gsl_matrix* gsl_permutation* gsl_vector* gsl_vector*))
	   (int gsl_linalg_cholesky_decomp (gsl_matrix*))
	   (int gsl_linalg_cholesky_solve (gsl_matrix* gsl_vector* gsl_vector*))
	   (int gsl_linalg_cholesky_svx (gsl_matrix* gsl_vector*))
	   (int gsl_linalg_cholesky_invert (gsl_matrix*))
	   (int gsl_linalg_cholesky_decomp_unit (gsl_matrix* gsl_vector*))
	   (int gsl_linalg_complex_cholesky_decomp (gsl_matrix_complex*))
	   (int gsl_linalg_complex_cholesky_solve (gsl_matrix_complex* gsl_vector_complex* gsl_vector_complex*))
	   (int gsl_linalg_complex_cholesky_svx (gsl_matrix_complex* gsl_vector_complex*))
	   (reader-cond ((>= gsl-version 1.15) (int gsl_linalg_complex_cholesky_invert (gsl_matrix_complex*))))
	   (int gsl_linalg_symmtd_decomp (gsl_matrix* gsl_vector*))
	   (int gsl_linalg_symmtd_unpack (gsl_matrix* gsl_vector* gsl_matrix* gsl_vector* gsl_vector*))
	   (int gsl_linalg_symmtd_unpack_T (gsl_matrix* gsl_vector* gsl_vector*))
	   (int gsl_linalg_hermtd_decomp (gsl_matrix_complex* gsl_vector_complex*))
	   (int gsl_linalg_hermtd_unpack (gsl_matrix_complex* gsl_vector_complex* gsl_matrix_complex* gsl_vector* gsl_vector*))
	   (int gsl_linalg_hermtd_unpack_T (gsl_matrix_complex* gsl_vector* gsl_vector*))
	   (int gsl_linalg_HH_solve (gsl_matrix* gsl_vector* gsl_vector*))
	   (int gsl_linalg_HH_svx (gsl_matrix* gsl_vector*))
	   (int gsl_linalg_solve_symm_tridiag (gsl_vector* gsl_vector* gsl_vector* gsl_vector*))
	   (int gsl_linalg_solve_tridiag (gsl_vector* gsl_vector* gsl_vector* gsl_vector* gsl_vector*))
	   (int gsl_linalg_solve_symm_cyc_tridiag (gsl_vector* gsl_vector* gsl_vector* gsl_vector*))
	   (int gsl_linalg_solve_cyc_tridiag (gsl_vector* gsl_vector* gsl_vector* gsl_vector* gsl_vector*))
	   (int gsl_linalg_bidiag_decomp (gsl_matrix* gsl_vector* gsl_vector*))
	   (int gsl_linalg_bidiag_unpack (gsl_matrix* gsl_vector* gsl_matrix* gsl_vector* gsl_matrix* gsl_vector* gsl_vector*))
	   (int gsl_linalg_bidiag_unpack2 (gsl_matrix* gsl_vector* gsl_vector* gsl_matrix*))
	   (int gsl_linalg_bidiag_unpack_B (gsl_matrix* gsl_vector* gsl_vector*))
	   (int gsl_linalg_balance_matrix (gsl_matrix* gsl_vector*))
	   (int gsl_linalg_balance_accum (gsl_matrix* gsl_vector*))
	   (int gsl_linalg_balance_columns (gsl_matrix* gsl_vector*))
	   
	   (gsl_matrix_complex* gsl_matrix_complex_alloc (size_t size_t))
	   (gsl_matrix_complex* gsl_matrix_complex_calloc (size_t size_t))
	   (gsl_matrix_complex* gsl_matrix_complex_alloc_from_matrix (gsl_matrix_complex* size_t size_t size_t size_t))
	   (gsl_vector_complex* gsl_vector_complex_alloc_row_from_matrix (gsl_matrix_complex* size_t))
	   (gsl_vector_complex* gsl_vector_complex_alloc_col_from_matrix (gsl_matrix_complex* size_t))
	   (gsl_vector_complex* gsl_vector_complex_alloc (size_t))
	   (void gsl_vector_complex_free (gsl_vector_complex*))
	   (void gsl_matrix_complex_free (gsl_matrix_complex*))
	   (void gsl_matrix_complex_set_zero (gsl_matrix_complex*))
	   (void gsl_matrix_complex_set_identity (gsl_matrix_complex*))
	   (int gsl_matrix_complex_fread (FILE* gsl_matrix_complex*) )
	   (int gsl_matrix_complex_fwrite (FILE* gsl_matrix_complex*) )
	   (int gsl_matrix_complex_fscanf (FILE* gsl_matrix_complex*))
	   (int gsl_matrix_complex_fprintf (FILE* gsl_matrix_complex* char*))
	   (int gsl_matrix_complex_memcpy (gsl_matrix_complex* gsl_matrix_complex*))
	   (int gsl_matrix_complex_swap (gsl_matrix_complex* gsl_matrix_complex*))
	   (int gsl_matrix_complex_swap_rows (gsl_matrix_complex* size_t size_t))
	   (int gsl_matrix_complex_swap_columns (gsl_matrix_complex* size_t size_t))
	   (int gsl_matrix_complex_swap_rowcol (gsl_matrix_complex* size_t size_t))
	   (int gsl_matrix_complex_transpose (gsl_matrix_complex*))
	   (int gsl_matrix_complex_transpose_memcpy (gsl_matrix_complex* gsl_matrix_complex*))
	   (reader-cond ((>= gsl-version 1.15) (int gsl_matrix_complex_equal (gsl_matrix_complex* gsl_matrix_complex*))))
	   (int gsl_matrix_complex_isnull (gsl_matrix_complex*))
	   (int gsl_matrix_complex_ispos (gsl_matrix_complex*))
	   (int gsl_matrix_complex_isneg (gsl_matrix_complex*))
	   (int gsl_matrix_complex_isnonneg (gsl_matrix_complex*))
	   (int gsl_matrix_complex_add (gsl_matrix_complex* gsl_matrix_complex*))
	   (int gsl_matrix_complex_sub (gsl_matrix_complex* gsl_matrix_complex*))
	   (int gsl_matrix_complex_mul_elements (gsl_matrix_complex* gsl_matrix_complex*))
	   (int gsl_matrix_complex_div_elements (gsl_matrix_complex* gsl_matrix_complex*))
	   
	   (in-C "static s7_pointer g_gsl_matrix_complex_set_all(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_complex g;
                    S7_TO_GSL_COMPLEX(s7_cadr(args), g);
                    gsl_matrix_complex_set_all((gsl_matrix_complex *)s7_c_pointer(s7_car(args)), g);
                    return(s7_cadr(args));
                  }
                  static s7_pointer g_gsl_matrix_complex_set(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_complex g;
                    s7_pointer cg;
                    cg = s7_cadddr(args);	      
                    S7_TO_GSL_COMPLEX(cg, g);
                    gsl_matrix_complex_set((gsl_matrix_complex *)s7_c_pointer(s7_car(args)), s7_integer(s7_cadr(args)), s7_integer(s7_caddr(args)), g);
                    return(cg);
                  }
                  static s7_pointer g_gsl_matrix_complex_get(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_complex g;
                    g = gsl_matrix_complex_get((gsl_matrix_complex *)s7_c_pointer(s7_car(args)), s7_integer(s7_cadr(args)), s7_integer(s7_caddr(args)));
                    return(GSL_TO_S7_COMPLEX(sc, g));
                  }
                  static s7_pointer g_gsl_matrix_complex_scale(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_complex g;
                    S7_TO_GSL_COMPLEX(s7_cadr(args), g);
                    return(s7_make_integer(sc, gsl_matrix_complex_scale((gsl_matrix_complex *)s7_c_pointer(s7_car(args)), g)));
                  }
                  static s7_pointer g_gsl_matrix_complex_add_constant(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_complex g;
                    S7_TO_GSL_COMPLEX(s7_cadr(args), g);
                    return(s7_make_integer(sc, gsl_matrix_complex_add_constant((gsl_matrix_complex *)s7_c_pointer(s7_car(args)), g)));
                  }
                  static s7_pointer g_gsl_matrix_complex_add_diagonal(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_complex g;
                    S7_TO_GSL_COMPLEX(s7_cadr(args), g);
                    return(s7_make_integer(sc, gsl_matrix_complex_add_diagonal((gsl_matrix_complex *)s7_c_pointer(s7_car(args)), g)));
                  }
                  static s7_pointer g_gsl_vector_complex_get(s7_scheme *sc, s7_pointer args)
                  {
                    gsl_complex g;
                    g = gsl_vector_complex_get((gsl_vector_complex *)s7_c_pointer(s7_car(args)), s7_integer(s7_cadr(args)));
                    return(GSL_TO_S7_COMPLEX(sc, g));
                  }
                  ")
	   (C-function ("gsl_matrix_complex_set_all" g_gsl_matrix_complex_set_all "" 2))
	   (C-function ("gsl_matrix_complex_set" g_gsl_matrix_complex_set "" 4))
	   (C-function ("gsl_matrix_complex_get" g_gsl_matrix_complex_get "" 3))
	   (C-function ("gsl_vector_complex_get" g_gsl_vector_complex_get "" 2))
	   (C-function ("gsl_matrix_complex_scale" g_gsl_matrix_complex_scale "" 2))
	   (C-function ("gsl_matrix_complex_add_constant" g_gsl_matrix_complex_add_constant "" 2))
	   (C-function ("gsl_matrix_complex_add_diagonal" g_gsl_matrix_complex_add_diagonal "" 2))
	   
	   (int gsl_matrix_complex_get_row (gsl_vector_complex* gsl_matrix_complex* size_t))
	   (int gsl_matrix_complex_get_col (gsl_vector_complex* gsl_matrix_complex* size_t))
	   (int gsl_matrix_complex_set_row (gsl_matrix_complex* size_t gsl_vector_complex*))
	   (int gsl_matrix_complex_set_col (gsl_matrix_complex* size_t gsl_vector_complex*))
	   (gsl_complex* gsl_matrix_complex_ptr (gsl_matrix_complex* size_t size_t))
	   (gsl_complex* gsl_matrix_complex_const_ptr (gsl_matrix_complex* size_t size_t))
	   
	   (void gsl_message (char* char* int int))
	   
	   (gsl_min_fminimizer* gsl_min_fminimizer_alloc (gsl_min_fminimizer_type*) )
	   (void gsl_min_fminimizer_free (gsl_min_fminimizer*))
	   (int gsl_min_fminimizer_set (gsl_min_fminimizer* gsl_function* double double double))
	   (int gsl_min_fminimizer_set_with_values (gsl_min_fminimizer* gsl_function* double double double double double double))
	   (int gsl_min_fminimizer_iterate (gsl_min_fminimizer*))
	   (char* gsl_min_fminimizer_name (gsl_min_fminimizer*))
	   (double gsl_min_fminimizer_x_minimum (gsl_min_fminimizer*))
	   (double gsl_min_fminimizer_x_lower (gsl_min_fminimizer*))
	   (double gsl_min_fminimizer_x_upper (gsl_min_fminimizer*))
	   (double gsl_min_fminimizer_f_minimum (gsl_min_fminimizer*))
	   (double gsl_min_fminimizer_f_lower (gsl_min_fminimizer*))
	   (double gsl_min_fminimizer_f_upper (gsl_min_fminimizer*))
	   (double gsl_min_fminimizer_minimum (gsl_min_fminimizer*))
	   (int gsl_min_test_interval (double double double double))
	   (int gsl_min_find_bracket (gsl_function* double* double* double* double* double* double* size_t))
	   
           ;; gsl_monte* is not doable -- they chose to pass a bare double* array to the gsl_monte_function,
	   ;;   and there's nothing I can do with that.  To wrap and unwrap it on every call would make it unusable.
	   ;;   I could keep wrappers around of all so-far-used sizes, but not until someone actually needs them.
	   ;; the fdf cases are similar, I think, and the ode functions.  GSL also assumes direct access to their
	   ;; structs (as in matrix size1/2) -- not very nice for our style of use.
	   
	   (gsl_multifit_linear_workspace* gsl_multifit_linear_alloc (size_t size_t))
	   (void gsl_multifit_linear_free (gsl_multifit_linear_workspace*))
	   (int gsl_multifit_linear (gsl_matrix* gsl_vector* gsl_vector* gsl_matrix* double* gsl_multifit_linear_workspace*))
	   (int gsl_multifit_linear_svd (gsl_matrix* gsl_vector* double size_t* gsl_vector* gsl_matrix* double* gsl_multifit_linear_workspace*))
	   (int gsl_multifit_linear_usvd (gsl_matrix* gsl_vector* double size_t* gsl_vector* gsl_matrix* double* gsl_multifit_linear_workspace*))
	   (int gsl_multifit_wlinear (gsl_matrix* gsl_vector* gsl_vector* gsl_vector* gsl_matrix* double* gsl_multifit_linear_workspace*))
	   (int gsl_multifit_wlinear_svd (gsl_matrix* gsl_vector* gsl_vector* double size_t* gsl_vector* gsl_matrix* double* gsl_multifit_linear_workspace*))
	   (int gsl_multifit_wlinear_usvd (gsl_matrix* gsl_vector* gsl_vector* double size_t* gsl_vector* gsl_matrix* double* gsl_multifit_linear_workspace*))
	   (int gsl_multifit_linear_est (gsl_vector* gsl_vector* gsl_matrix* double* double*))
	   (int gsl_multifit_linear_residuals (gsl_matrix* gsl_vector* gsl_vector* gsl_vector*))
	   (reader-cond ((>= gsl-version 1.16) 
			 (gsl_multifit_robust_workspace* gsl_multifit_robust_alloc (gsl_multifit_robust_type* size_t size_t))
			 (void gsl_multifit_robust_free (gsl_multifit_robust_workspace*))
			 (int gsl_multifit_robust_tune (double gsl_multifit_robust_workspace*))
			 (char* gsl_multifit_robust_name (gsl_multifit_robust_workspace*))
			 (gsl_multifit_robust_stats gsl_multifit_robust_statistics (gsl_multifit_robust_workspace*))
			 (int gsl_multifit_robust (gsl_matrix* gsl_vector* gsl_vector* gsl_matrix* gsl_multifit_robust_workspace*))
			 (int gsl_multifit_robust_est (gsl_vector* gsl_vector* gsl_matrix* double* double*))
			 (int gsl_multifit_fsolver_driver (gsl_multifit_fsolver* size_t double double))))
	   (int gsl_multifit_gradient (gsl_matrix* gsl_vector* gsl_vector*))
	   (int gsl_multifit_covar (gsl_matrix* double gsl_matrix*))
	   (gsl_multifit_fsolver* gsl_multifit_fsolver_alloc (gsl_multifit_fsolver_type* size_t size_t))
	   (void gsl_multifit_fsolver_free (gsl_multifit_fsolver*))
	   (int gsl_multifit_fsolver_set (gsl_multifit_fsolver* gsl_multifit_function* gsl_vector*))
	   (int gsl_multifit_fsolver_iterate (gsl_multifit_fsolver*))
	   (char* gsl_multifit_fsolver_name (gsl_multifit_fsolver*))
	   (gsl_vector* gsl_multifit_fsolver_position (gsl_multifit_fsolver*))
	   (int gsl_multifit_test_delta (gsl_vector* gsl_vector* double double))
	   (int gsl_multifit_test_gradient (gsl_vector* double))
	   
	   (gsl_multimin_fminimizer* gsl_multimin_fminimizer_alloc (gsl_multimin_fminimizer_type* size_t))
	   (void gsl_multimin_fminimizer_free (gsl_multimin_fminimizer*))
	   (char* gsl_multimin_fminimizer_name (gsl_multimin_fminimizer*))
	   (int gsl_multimin_fminimizer_iterate (gsl_multimin_fminimizer*))
	   (gsl_vector* gsl_multimin_fminimizer_x (gsl_multimin_fminimizer*))
	   (double gsl_multimin_fminimizer_minimum (gsl_multimin_fminimizer*))
	   (double gsl_multimin_fminimizer_size (gsl_multimin_fminimizer*))
	   (int gsl_multimin_test_gradient (gsl_vector* double))
	   (int gsl_multimin_test_size (double double))

	   ;; multimin_function is double f(gsl_vector* void*) -- so we can handle it (but not the fdf brand)
	   (in-C "static s7_scheme *gsl_mmf_s7;
                  static gsl_multimin_function gsl_mmf;
                  static double gsl_mmf_caller(const gsl_vector *x, void *p) 
                  {
                    return(s7_real(s7_call(gsl_mmf_s7, (s7_pointer)p, s7_cons(gsl_mmf_s7, s7_make_c_pointer(gsl_mmf_s7, (void *)x), s7_nil(gsl_mmf_s7)))));
                  }
                  #define make_gsl_mm_function(Args, Size) do {gsl_mmf.f = gsl_mmf_caller; gsl_mmf.n = Size; gsl_mmf.params = (void *)Args; gsl_mmf_s7 = sc;} while (0)
                  static s7_pointer g_gsl_multimin_fminimizer_set(s7_scheme *sc, s7_pointer args)
                  {
                    make_gsl_mm_function(s7_cadr(args), ((gsl_vector *)s7_c_pointer(s7_caddr(args)))->size);
                    return(s7_make_integer(sc, gsl_multimin_fminimizer_set((gsl_multimin_fminimizer *)s7_c_pointer(s7_car(args)),
                                                  &gsl_mmf, (gsl_vector *)s7_c_pointer(s7_caddr(args)), (gsl_vector *)s7_c_pointer(s7_cadddr(args)))));
                  }
                  static s7_pointer g_gsl_multimin_diff(s7_scheme *sc, s7_pointer args)
                  {
                    make_gsl_mm_function(s7_caddr(args), ((gsl_vector *)s7_c_pointer(s7_car(args)))->size);
                    return(s7_make_integer(sc, gsl_multimin_diff(&gsl_mmf, (gsl_vector *)s7_c_pointer(s7_cadr(args)), (gsl_vector *)s7_c_pointer(s7_caddr(args)))));
                  }
	          static s7_pointer g_gsl_multimin_fminimizer_fval(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_real(sc, ((gsl_multimin_fminimizer *)s7_c_pointer(s7_car(args)))->fval));}
                 ")
	   (C-function ("gsl_multimin_fminimizer_set" g_gsl_multimin_fminimizer_set "" 4))
	   (C-function ("gsl_multimin_diff" g_gsl_multimin_diff "" 3))
	   (C-function ("gsl_multimin_fminimizer_fval" g_gsl_multimin_fminimizer_fval "" 1))
	   

	   ;; int f(const gsl_vector* void* gsl_vector*) so the function is doable
	   (in-C "static s7_scheme *gsl_rf_s7;
                  static gsl_multiroot_function gsl_rf;
                  static int gsl_rf_caller(const gsl_vector *x, void *p, gsl_vector *y) 
                  {
                    return(s7_integer(s7_call(gsl_rf_s7, (s7_pointer)p, 
                            s7_cons(gsl_rf_s7, s7_make_c_pointer(gsl_rf_s7, (void *)x), 
                               s7_cons(gsl_rf_s7, s7_make_c_pointer(gsl_rf_s7, (void *)y), s7_nil(gsl_rf_s7))))));
                  }
                  #define make_gsl_rf_function(Args, Size) do {gsl_rf.f = gsl_rf_caller; gsl_rf.n = Size; gsl_rf.params = (void *)Args; gsl_rf_s7 = sc;} while (0)
                  static s7_pointer g_gsl_multiroot_fsolver_set(s7_scheme *sc, s7_pointer args)
                  {
                    make_gsl_rf_function(s7_cadr(args), ((gsl_vector *)s7_c_pointer(s7_caddr(args)))->size);
                    return(s7_make_integer(sc, gsl_multiroot_fsolver_set((gsl_multiroot_fsolver *)s7_c_pointer(s7_car(args)),
                                                  &gsl_rf, (const gsl_vector *)s7_c_pointer(s7_caddr(args)))));
                  }
                  static s7_pointer g_gsl_multiroot_fdjacobian(s7_scheme *sc, s7_pointer args)
                  {
                    make_gsl_rf_function(s7_car(args), ((gsl_vector *)s7_c_pointer(s7_cadr(args)))->size);
                    return(s7_make_integer(sc, gsl_multiroot_fdjacobian(&gsl_rf, (gsl_vector *)s7_c_pointer(s7_cadr(args)), 
                               (gsl_vector *)s7_c_pointer(s7_caddr(args)), s7_real(s7_cadddr(args)), (gsl_matrix *)s7_list_ref(sc, args, 4))));
                  }")
	   (C-function ("gsl_multiroot_fsolver_set" g_gsl_multiroot_fsolver_set "" 3))
	   (C-function ("gsl_multiroot_fdjacobian" g_gsl_multiroot_fdjacobian "" 5))
	   (gsl_multiroot_fsolver* gsl_multiroot_fsolver_alloc (gsl_multiroot_fsolver_type* size_t) )
	   (void gsl_multiroot_fsolver_free (gsl_multiroot_fsolver*))
	   (int gsl_multiroot_fsolver_iterate (gsl_multiroot_fsolver*))
	   (char* gsl_multiroot_fsolver_name (gsl_multiroot_fsolver*))
	   (gsl_vector* gsl_multiroot_fsolver_root (gsl_multiroot_fsolver*))
	   (gsl_vector* gsl_multiroot_fsolver_dx (gsl_multiroot_fsolver*))
	   (gsl_vector* gsl_multiroot_fsolver_f (gsl_multiroot_fsolver*))
	   (int gsl_multiroot_test_delta (gsl_vector* gsl_vector* double double))
	   (int gsl_multiroot_test_residual (gsl_vector* double))
	   
	   (gsl_multiset* gsl_multiset_alloc (size_t size_t))
	   (gsl_multiset* gsl_multiset_calloc (size_t size_t))
	   (void gsl_multiset_init_first (gsl_multiset*))
	   (void gsl_multiset_init_last (gsl_multiset*))
	   (void gsl_multiset_free (gsl_multiset*))
	   (int gsl_multiset_memcpy (gsl_multiset* gsl_multiset*))
	   (int gsl_multiset_fread (FILE* gsl_multiset*))
	   (int gsl_multiset_fwrite (FILE* gsl_multiset*))
	   (int gsl_multiset_fscanf (FILE* gsl_multiset*))
	   (int gsl_multiset_fprintf (FILE* gsl_multiset* char*))
	   (size_t gsl_multiset_n (gsl_multiset*))
	   (size_t gsl_multiset_k (gsl_multiset*))
	   (size_t* gsl_multiset_data (gsl_multiset*))
	   (int gsl_multiset_valid (gsl_multiset*))
	   (int gsl_multiset_next (gsl_multiset*))
	   (int gsl_multiset_prev (gsl_multiset*))
	   (size_t gsl_multiset_get (gsl_multiset* size_t))
	   
	   ;; the ode functions all pass bare double* arrays to the called function.
	   (gsl_permutation* gsl_permutation_alloc (size_t))
	   (gsl_permutation* gsl_permutation_calloc (size_t))
	   (void gsl_permutation_init (gsl_permutation*))
	   (void gsl_permutation_free (gsl_permutation*))
	   (int gsl_permutation_memcpy (gsl_permutation* gsl_permutation*))
	   (int gsl_permutation_fread (FILE* gsl_permutation*))
	   (int gsl_permutation_fwrite (FILE* gsl_permutation*))
	   (int gsl_permutation_fscanf (FILE* gsl_permutation*))
	   (int gsl_permutation_fprintf (FILE* gsl_permutation* char*))
	   (size_t gsl_permutation_size (gsl_permutation*))
	   (size_t* gsl_permutation_data (gsl_permutation*))
	   (int gsl_permutation_swap (gsl_permutation* size_t size_t))
	   (int gsl_permutation_valid (gsl_permutation*))
	   (void gsl_permutation_reverse (gsl_permutation*))
	   (int gsl_permutation_inverse (gsl_permutation* gsl_permutation*))
	   (int gsl_permutation_next (gsl_permutation*))
	   (int gsl_permutation_prev (gsl_permutation*))
	   (int gsl_permutation_mul (gsl_permutation* gsl_permutation* gsl_permutation*))
	   (int gsl_permutation_linear_to_canonical (gsl_permutation* gsl_permutation*))
	   (int gsl_permutation_canonical_to_linear (gsl_permutation* gsl_permutation*))
	   (size_t gsl_permutation_inversions (gsl_permutation*))
	   (size_t gsl_permutation_linear_cycles (gsl_permutation*))
	   (size_t gsl_permutation_canonical_cycles (gsl_permutation*))
	   (size_t gsl_permutation_get (gsl_permutation* size_t))
	   (int gsl_permute_complex (size_t* double* size_t size_t))
	   (int gsl_permute_complex_inverse (size_t* double* size_t size_t))
	   (int gsl_permute (size_t* double* size_t size_t))
	   (int gsl_permute_inverse (size_t* double* size_t size_t))
	   (int gsl_permute_vector_complex (gsl_permutation* gsl_vector_complex*))
	   (int gsl_permute_vector_complex_inverse (gsl_permutation* gsl_vector_complex*))
	   (int gsl_permute_vector (gsl_permutation* gsl_vector*))
	   (int gsl_permute_vector_inverse (gsl_permutation* gsl_vector*))
	   
	   (gsl_root_fsolver* gsl_root_fsolver_alloc (gsl_root_fsolver_type*))
	   (void gsl_root_fsolver_free (gsl_root_fsolver*))
	   (int gsl_root_fsolver_set (gsl_root_fsolver* gsl_function* double double))
	   (int gsl_root_fsolver_iterate (gsl_root_fsolver*))
	   (char* gsl_root_fsolver_name (gsl_root_fsolver*))
	   (double gsl_root_fsolver_root (gsl_root_fsolver*))
	   (double gsl_root_fsolver_x_lower (gsl_root_fsolver*))
	   (double gsl_root_fsolver_x_upper (gsl_root_fsolver*))
	   (int gsl_root_test_interval (double double double double))
	   (int gsl_root_test_residual (double double))
	   (int gsl_root_test_delta (double double double double))
	   
	   (gsl_sum_levin_u_workspace* gsl_sum_levin_u_alloc (size_t))
	   (void gsl_sum_levin_u_free (gsl_sum_levin_u_workspace*))
	   (int gsl_sum_levin_u_accel (double* size_t gsl_sum_levin_u_workspace* double* double*))
	   (int gsl_sum_levin_u_minmax (double* size_t size_t size_t gsl_sum_levin_u_workspace* double* double*))
	   (int gsl_sum_levin_u_step (double size_t size_t gsl_sum_levin_u_workspace* double*))
	   (gsl_sum_levin_utrunc_workspace* gsl_sum_levin_utrunc_alloc (size_t))
	   (void gsl_sum_levin_utrunc_free (gsl_sum_levin_utrunc_workspace*))
	   (int gsl_sum_levin_utrunc_accel (double* size_t gsl_sum_levin_utrunc_workspace* double* double*))
	   (int gsl_sum_levin_utrunc_minmax (double* size_t size_t size_t gsl_sum_levin_utrunc_workspace* double* double*))
	   (int gsl_sum_levin_utrunc_step (double size_t gsl_sum_levin_utrunc_workspace* double*))
	   
	   (gsl_wavelet* gsl_wavelet_alloc (gsl_wavelet_type* size_t))
	   (void gsl_wavelet_free (gsl_wavelet*))
	   (char* gsl_wavelet_name (gsl_wavelet*))
	   (gsl_wavelet_workspace* gsl_wavelet_workspace_alloc (size_t))
	   (void gsl_wavelet_workspace_free (gsl_wavelet_workspace*))
	   (int gsl_wavelet_transform (gsl_wavelet* double* size_t size_t int gsl_wavelet_workspace*))
	   (int gsl_wavelet_transform_forward (gsl_wavelet* double* size_t size_t gsl_wavelet_workspace*))
	   (int gsl_wavelet_transform_inverse (gsl_wavelet* double* size_t size_t gsl_wavelet_workspace*))
	   (int gsl_wavelet2d_transform (gsl_wavelet* double* size_t size_t size_t int gsl_wavelet_workspace*))
	   (int gsl_wavelet2d_transform_forward (gsl_wavelet* double* size_t size_t size_t gsl_wavelet_workspace*))
	   (int gsl_wavelet2d_transform_inverse (gsl_wavelet* double* size_t size_t size_t gsl_wavelet_workspace*))
	   (int gsl_wavelet2d_nstransform (gsl_wavelet* double* size_t size_t size_t int gsl_wavelet_workspace*))
	   (int gsl_wavelet2d_nstransform_forward (gsl_wavelet* double* size_t size_t size_t gsl_wavelet_workspace*))
	   (int gsl_wavelet2d_nstransform_inverse (gsl_wavelet* double* size_t size_t size_t gsl_wavelet_workspace*))
	   (int gsl_wavelet2d_transform_matrix (gsl_wavelet* gsl_matrix* int gsl_wavelet_workspace*))
	   (int gsl_wavelet2d_transform_matrix_forward (gsl_wavelet* gsl_matrix* gsl_wavelet_workspace*))
	   (int gsl_wavelet2d_transform_matrix_inverse (gsl_wavelet* gsl_matrix* gsl_wavelet_workspace*))
	   (int gsl_wavelet2d_nstransform_matrix (gsl_wavelet* gsl_matrix* int gsl_wavelet_workspace*))
	   (int gsl_wavelet2d_nstransform_matrix_forward (gsl_wavelet* gsl_matrix* gsl_wavelet_workspace*))
	   (int gsl_wavelet2d_nstransform_matrix_inverse (gsl_wavelet* gsl_matrix* gsl_wavelet_workspace*))
	   
	   )
	 "" (list "gsl/gsl_blas.h"
		  "gsl/gsl_blas_types.h"
		  "gsl/gsl_block.h"
		  "gsl/gsl_block_complex_double.h"
		  "gsl/gsl_block_double.h"
		  "gsl/gsl_bspline.h"
		  "gsl/gsl_cblas.h"
		  "gsl/gsl_cdf.h"
		  "gsl/gsl_chebyshev.h"
		  "gsl/gsl_check_range.h"
		  "gsl/gsl_combination.h"
		  "gsl/gsl_complex.h"
		  "gsl/gsl_complex_math.h"
		  "gsl/gsl_const.h"
		  "gsl/gsl_const_cgs.h"
		  "gsl/gsl_const_cgsm.h"
		  "gsl/gsl_const_mks.h"
		  "gsl/gsl_const_mksa.h"
		  "gsl/gsl_const_num.h"
		  "gsl/gsl_deriv.h"
		  "gsl/gsl_dft_complex.h"
		  "gsl/gsl_dht.h"
		  "gsl/gsl_diff.h"
		  "gsl/gsl_eigen.h"
		  "gsl/gsl_errno.h"
		  "gsl/gsl_fft.h"
		  "gsl/gsl_fft_complex.h"
		  "gsl/gsl_fft_real.h"
		  "gsl/gsl_fit.h"
		  "gsl/gsl_heapsort.h"
		  "gsl/gsl_histogram.h"
		  "gsl/gsl_histogram2d.h"
		  "gsl/gsl_ieee_utils.h"
		  "gsl/gsl_inline.h"
		  "gsl/gsl_integration.h"
		  "gsl/gsl_interp.h"
		  "gsl/gsl_linalg.h"
		  "gsl/gsl_machine.h"
		  "gsl/gsl_math.h"
		  "gsl/gsl_matrix.h"
		  "gsl/gsl_matrix_complex_double.h"
		  "gsl/gsl_matrix_double.h"
		  "gsl/gsl_message.h"
		  "gsl/gsl_min.h"
		  "gsl/gsl_minmax.h"
		  "gsl/gsl_mode.h"
		  "gsl/gsl_multifit.h"
		  "gsl/gsl_multifit_nlin.h"
		  "gsl/gsl_multimin.h"
		  "gsl/gsl_multiroots.h"
		  "gsl/gsl_multiset.h"
		  "gsl/gsl_nan.h"
		  "gsl/gsl_permutation.h"
		  "gsl/gsl_permute.h"
		  "gsl/gsl_permute_complex_double.h"
		  "gsl/gsl_permute_double.h"
		  "gsl/gsl_permute_vector.h"
		  "gsl/gsl_permute_vector_complex_double.h"
		  "gsl/gsl_permute_vector_double.h"
		  "gsl/gsl_poly.h"
		  "gsl/gsl_pow_int.h"
		  "gsl/gsl_precision.h"
		  "gsl/gsl_qrng.h"
		  "gsl/gsl_randist.h"
		  "gsl/gsl_rng.h"
		  "gsl/gsl_roots.h"
		  "gsl/gsl_sf.h"
		  "gsl/gsl_sf_airy.h"
		  "gsl/gsl_sf_bessel.h"
		  "gsl/gsl_sf_clausen.h"
		  "gsl/gsl_sf_coulomb.h"
		  "gsl/gsl_sf_coupling.h"
		  "gsl/gsl_sf_dawson.h"
		  "gsl/gsl_sf_debye.h"
		  "gsl/gsl_sf_dilog.h"
		  "gsl/gsl_sf_elementary.h"
		  "gsl/gsl_sf_ellint.h"
		  "gsl/gsl_sf_elljac.h"
		  "gsl/gsl_sf_erf.h"
		  "gsl/gsl_sf_exp.h"
		  "gsl/gsl_sf_expint.h"
		  "gsl/gsl_sf_fermi_dirac.h"
		  "gsl/gsl_sf_gamma.h"
		  "gsl/gsl_sf_gegenbauer.h"
		  "gsl/gsl_sf_hyperg.h"
		  "gsl/gsl_sf_laguerre.h"
		  "gsl/gsl_sf_lambert.h"
		  "gsl/gsl_sf_legendre.h"
		  "gsl/gsl_sf_log.h"
		  "gsl/gsl_sf_mathieu.h"
		  "gsl/gsl_sf_pow_int.h"
		  "gsl/gsl_sf_psi.h"
		  "gsl/gsl_sf_result.h"
		  "gsl/gsl_sf_synchrotron.h"
		  "gsl/gsl_sf_transport.h"
		  "gsl/gsl_sf_trig.h"
		  "gsl/gsl_sf_zeta.h"
		  "gsl/gsl_siman.h"
		  "gsl/gsl_sort.h"
		  "gsl/gsl_sort_double.h"
		  "gsl/gsl_sort_vector.h"
		  "gsl/gsl_sort_vector_double.h"
		  "gsl/gsl_specfunc.h"
		  "gsl/gsl_spline.h"
		  "gsl/gsl_statistics.h"
		  "gsl/gsl_statistics_double.h"
		  "gsl/gsl_sum.h"
		  "gsl/gsl_sys.h"
		  "gsl/gsl_vector.h"
		  "gsl/gsl_vector_complex.h"
		  "gsl/gsl_vector_complex_double.h"
		  "gsl/gsl_vector_double.h"
		  "gsl/gsl_version.h"
		  "gsl/gsl_wavelet.h"
		  "gsl/gsl_wavelet2d.h"
		  )
	 
	 "-I/usr/local/include -g3 -DGSL_DISABLE_DEPRECATED" "-lgsl -lgslcblas" "libgsl_s7")
					; GSL_DISABLE_DEPRECATED is needed to avoid a name collision (dating from version 1.7!!)
	(curlet))))

*libgsl*

