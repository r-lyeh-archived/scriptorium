#define TR_VERSION "0.0"

#include <limits.h>

/* Non portable optimizations */
#ifndef TR_COMPAT_MODE

/* Direct threaded code is used to dispatch instructions.
   It's very fast, but a GCC 3.x or greater extension. */
#if __GNUC__ > 3
#define TR_THREADED_DISPATCH 1
#endif

/* Force the interpreter to store the stack and instruction
   pointer in machine registers. Works only on x86 machines. */
#if __GNUC__
#define TR_USE_MACHINE_REGS 1
#endif

#endif

/* Various limits */
#define TR_MAX_FRAMES 255
#define MAX_INT       (INT_MAX-2)  /* maximum value of an int (-2 for safety) */

/* TR_BITSINT defines the number of bits in an int. (Blindly stolen from Lua) */
#if INT_MAX-20 < 32760
#define TR_BITSINT  16
#elif INT_MAX > 2147483640L
/* int has at least 32 bits */
#define TR_BITSINT  32
#else
#error "you must define TR_BITSINT with number of bits in an integer"
#endif

#ifdef _WIN32
#include <malloc.h>
#define alloca _alloca
#else
#include <alloca.h>
#endif
