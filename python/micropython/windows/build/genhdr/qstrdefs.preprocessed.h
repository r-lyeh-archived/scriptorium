# 1 "<stdin>"
# 1 "<interno>"
# 1 "<línea-de-orden>"
# 1 "<stdin>"
# 27 "<stdin>"
# 1 "../py/mpconfig.h" 1
# 45 "../py/mpconfig.h"
# 1 "./mpconfigport.h" 1
# 109 "./mpconfigport.h"
typedef int mp_int_t;
typedef unsigned int mp_uint_t;
# 123 "./mpconfigport.h"
typedef long mp_off_t;


typedef void *machine_ptr_t;
typedef const void *machine_const_ptr_t;



extern const struct _mp_obj_fun_builtin_t mp_builtin_input_obj;
extern const struct _mp_obj_fun_builtin_t mp_builtin_open_obj;




extern const struct _mp_obj_module_t mp_module_os;
extern const struct _mp_obj_module_t mp_module_time;
# 153 "./mpconfigport.h"
# 1 "/usr/include/malloc.h" 1 3 4





# 1 "/usr/include/_ansi.h" 1 3 4
# 15 "/usr/include/_ansi.h" 3 4
# 1 "/usr/include/newlib.h" 1 3 4
# 16 "/usr/include/_ansi.h" 2 3 4
# 1 "/usr/include/sys/config.h" 1 3 4



# 1 "/usr/include/machine/ieeefp.h" 1 3 4
# 5 "/usr/include/sys/config.h" 2 3 4
# 1 "/usr/include/sys/features.h" 1 3 4
# 6 "/usr/include/sys/config.h" 2 3 4
# 234 "/usr/include/sys/config.h" 3 4
# 1 "/usr/include/cygwin/config.h" 1 3 4
# 235 "/usr/include/sys/config.h" 2 3 4
# 17 "/usr/include/_ansi.h" 2 3 4
# 7 "/usr/include/malloc.h" 2 3 4
# 1 "/usr/include/sys/reent.h" 1 3 4
# 14 "/usr/include/sys/reent.h" 3 4
# 1 "/usr/lib/gcc/x86_64-pc-msys/4.9.2/include/stddef.h" 1 3 4
# 147 "/usr/lib/gcc/x86_64-pc-msys/4.9.2/include/stddef.h" 3 4
typedef long int ptrdiff_t;
# 212 "/usr/lib/gcc/x86_64-pc-msys/4.9.2/include/stddef.h" 3 4
typedef long unsigned int size_t;
# 324 "/usr/lib/gcc/x86_64-pc-msys/4.9.2/include/stddef.h" 3 4
typedef short unsigned int wchar_t;
# 15 "/usr/include/sys/reent.h" 2 3 4
# 1 "/usr/include/sys/_types.h" 1 3 4
# 12 "/usr/include/sys/_types.h" 3 4
# 1 "/usr/include/machine/_types.h" 1 3 4






# 1 "/usr/include/machine/_default_types.h" 1 3 4
# 27 "/usr/include/machine/_default_types.h" 3 4
typedef signed char __int8_t;

typedef unsigned char __uint8_t;
# 41 "/usr/include/machine/_default_types.h" 3 4
typedef short int __int16_t;

typedef short unsigned int __uint16_t;
# 63 "/usr/include/machine/_default_types.h" 3 4
typedef int __int32_t;

typedef unsigned int __uint32_t;
# 89 "/usr/include/machine/_default_types.h" 3 4
typedef long int __int64_t;

typedef long unsigned int __uint64_t;
# 120 "/usr/include/machine/_default_types.h" 3 4
typedef signed char __int_least8_t;

typedef unsigned char __uint_least8_t;
# 146 "/usr/include/machine/_default_types.h" 3 4
typedef short int __int_least16_t;

typedef short unsigned int __uint_least16_t;
# 168 "/usr/include/machine/_default_types.h" 3 4
typedef int __int_least32_t;

typedef unsigned int __uint_least32_t;
# 186 "/usr/include/machine/_default_types.h" 3 4
typedef long int __int_least64_t;

typedef long unsigned int __uint_least64_t;
# 200 "/usr/include/machine/_default_types.h" 3 4
typedef long int __intptr_t;

typedef long unsigned int __uintptr_t;
# 8 "/usr/include/machine/_types.h" 2 3 4
# 13 "/usr/include/sys/_types.h" 2 3 4
# 1 "/usr/include/sys/lock.h" 1 3 4
# 14 "/usr/include/sys/lock.h" 3 4
typedef void *_LOCK_T;
# 44 "/usr/include/sys/lock.h" 3 4
void __cygwin_lock_init(_LOCK_T *);
void __cygwin_lock_init_recursive(_LOCK_T *);
void __cygwin_lock_fini(_LOCK_T *);
void __cygwin_lock_lock(_LOCK_T *);
int __cygwin_lock_trylock(_LOCK_T *);
void __cygwin_lock_unlock(_LOCK_T *);
# 14 "/usr/include/sys/_types.h" 2 3 4


typedef long _off_t;



typedef short __dev_t;



typedef unsigned short __uid_t;


typedef unsigned short __gid_t;



__extension__ typedef long long _off64_t;







typedef long _fpos_t;





typedef _off64_t _fpos64_t;
# 55 "/usr/include/sys/_types.h" 3 4
typedef long signed int _ssize_t;
# 67 "/usr/include/sys/_types.h" 3 4
# 1 "/usr/lib/gcc/x86_64-pc-msys/4.9.2/include/stddef.h" 1 3 4
# 353 "/usr/lib/gcc/x86_64-pc-msys/4.9.2/include/stddef.h" 3 4
typedef unsigned int wint_t;
# 68 "/usr/include/sys/_types.h" 2 3 4



typedef struct
{
  int __count;
  union
  {
    wint_t __wch;
    unsigned char __wchb[4];
  } __value;
} _mbstate_t;



typedef _LOCK_T _flock_t;




typedef void *_iconv_t;
# 16 "/usr/include/sys/reent.h" 2 3 4
# 25 "/usr/include/sys/reent.h" 3 4
typedef unsigned int __ULong;
# 38 "/usr/include/sys/reent.h" 3 4
struct _reent;






struct _Bigint
{
  struct _Bigint *_next;
  int _k, _maxwds, _sign, _wds;
  __ULong _x[1];
};


struct __tm
{
  int __tm_sec;
  int __tm_min;
  int __tm_hour;
  int __tm_mday;
  int __tm_mon;
  int __tm_year;
  int __tm_wday;
  int __tm_yday;
  int __tm_isdst;
};







struct _on_exit_args {
 void * _fnargs[32];
 void * _dso_handle[32];

 __ULong _fntypes;


 __ULong _is_cxa;
};
# 91 "/usr/include/sys/reent.h" 3 4
struct _atexit {
 struct _atexit *_next;
 int _ind;

 void (*_fns[32])(void);
        struct _on_exit_args _on_exit_args;
};
# 115 "/usr/include/sys/reent.h" 3 4
struct __sbuf {
 unsigned char *_base;
 int _size;
};
# 179 "/usr/include/sys/reent.h" 3 4
struct __sFILE {
  unsigned char *_p;
  int _r;
  int _w;
  short _flags;
  short _file;
  struct __sbuf _bf;
  int _lbfsize;






  void * _cookie;

  _ssize_t (__attribute__((__cdecl__)) * _read) (struct _reent *, void *, char *, size_t)
                                          ;
  _ssize_t (__attribute__((__cdecl__)) * _write) (struct _reent *, void *, const char *, size_t)

                                   ;
  _fpos_t (__attribute__((__cdecl__)) * _seek) (struct _reent *, void *, _fpos_t, int);
  int (__attribute__((__cdecl__)) * _close) (struct _reent *, void *);


  struct __sbuf _ub;
  unsigned char *_up;
  int _ur;


  unsigned char _ubuf[3];
  unsigned char _nbuf[1];


  struct __sbuf _lb;


  int _blksize;
  _off_t _offset;


  struct _reent *_data;



  _flock_t _lock;

  _mbstate_t _mbstate;
  int _flags2;
};
# 237 "/usr/include/sys/reent.h" 3 4
struct __sFILE64 {
  unsigned char *_p;
  int _r;
  int _w;
  short _flags;
  short _file;
  struct __sbuf _bf;
  int _lbfsize;

  struct _reent *_data;


  void * _cookie;

  _ssize_t (__attribute__((__cdecl__)) * _read) (struct _reent *, void *, char *, size_t)
                                          ;
  _ssize_t (__attribute__((__cdecl__)) * _write) (struct _reent *, void *, const char *, size_t)

                                   ;
  _fpos_t (__attribute__((__cdecl__)) * _seek) (struct _reent *, void *, _fpos_t, int);
  int (__attribute__((__cdecl__)) * _close) (struct _reent *, void *);


  struct __sbuf _ub;
  unsigned char *_up;
  int _ur;


  unsigned char _ubuf[3];
  unsigned char _nbuf[1];


  struct __sbuf _lb;


  int _blksize;
  int _flags2;

  _off64_t _offset;
  _fpos64_t (__attribute__((__cdecl__)) * _seek64) (struct _reent *, void *, _fpos64_t, int);


  _flock_t _lock;

  _mbstate_t _mbstate;
};
typedef struct __sFILE64 __FILE;





struct _glue
{
  struct _glue *_next;
  int _niobs;
  __FILE *_iobs;
};
# 317 "/usr/include/sys/reent.h" 3 4
struct _rand48 {
  unsigned short _seed[3];
  unsigned short _mult[3];
  unsigned short _add;




};
# 569 "/usr/include/sys/reent.h" 3 4
struct _reent
{
  int _errno;




  __FILE *_stdin, *_stdout, *_stderr;

  int _inc;
  char _emergency[25];

  int _current_category;
  const char *_current_locale;

  int __sdidinit;

  void (__attribute__((__cdecl__)) * __cleanup) (struct _reent *);


  struct _Bigint *_result;
  int _result_k;
  struct _Bigint *_p5s;
  struct _Bigint **_freelist;


  int _cvtlen;
  char *_cvtbuf;

  union
    {
      struct
        {
          unsigned int _unused_rand;
          char * _strtok_last;
          char _asctime_buf[26];
          struct __tm _localtime_buf;
          int _gamma_signgam;
          __extension__ unsigned long long _rand_next;
          struct _rand48 _r48;
          _mbstate_t _mblen_state;
          _mbstate_t _mbtowc_state;
          _mbstate_t _wctomb_state;
          char _l64a_buf[8];
          char _signal_buf[24];
          int _getdate_err;
          _mbstate_t _mbrlen_state;
          _mbstate_t _mbrtowc_state;
          _mbstate_t _mbsrtowcs_state;
          _mbstate_t _wcrtomb_state;
          _mbstate_t _wcsrtombs_state;
   int _h_errno;
        } _reent;



      struct
        {

          unsigned char * _nextf[30];
          unsigned int _nmalloc[30];
        } _unused;
    } _new;



  struct _atexit *_atexit;
  struct _atexit _atexit0;



  void (**(_sig_func))(int);




  struct _glue __sglue;
  __FILE __sf[3];
};
# 762 "/usr/include/sys/reent.h" 3 4
extern struct _reent *_impure_ptr ;
extern struct _reent *const _global_impure_ptr ;

void _reclaim_reent (struct _reent *);





  struct _reent * __attribute__((__cdecl__)) __getreent (void);
# 8 "/usr/include/malloc.h" 2 3 4


# 1 "/usr/lib/gcc/x86_64-pc-msys/4.9.2/include/stddef.h" 1 3 4
# 11 "/usr/include/malloc.h" 2 3 4


# 1 "/usr/include/machine/malloc.h" 1 3 4
# 14 "/usr/include/malloc.h" 2 3 4
# 22 "/usr/include/malloc.h" 3 4
struct mallinfo {
  size_t arena;
  size_t ordblks;
  size_t smblks;
  size_t hblks;
  size_t hblkhd;
  size_t usmblks;
  size_t fsmblks;
  size_t uordblks;
  size_t fordblks;
  size_t keepcost;
};



extern void * malloc (size_t);







extern void free (void *);







extern void * realloc (void *, size_t);







extern void * calloc (size_t, size_t);







extern void * memalign (size_t, size_t);







extern struct mallinfo mallinfo (void);







extern void malloc_stats (void);







extern int mallopt (int, int);







extern size_t malloc_usable_size (void *);
# 112 "/usr/include/malloc.h" 3 4
extern void * valloc (size_t);







extern void * pvalloc (size_t);







extern int malloc_trim (size_t);
# 138 "/usr/include/malloc.h" 3 4
extern void mstats (char *);
# 154 "./mpconfigport.h" 2

# 1 "./realpath.h" 1
# 27 "./realpath.h"
extern char *realpath(const char *path, char *resolved_path);
# 156 "./mpconfigport.h" 2
# 1 "./init.h" 1
# 27 "./init.h"
void init(void);
void deinit(void);
# 157 "./mpconfigport.h" 2


void msec_sleep(double msec);
# 46 "../py/mpconfig.h" 2
# 359 "../py/mpconfig.h"
typedef double mp_float_t;
# 28 "<stdin>" 2





QCFG(BYTES_IN_LEN, (1))

Q()
Q(*)
Q(__build_class__)
Q(__class__)
Q(__doc__)
Q(__import__)
Q(__init__)
Q(__new__)
Q(__locals__)
Q(__main__)
Q(__module__)
Q(__name__)
Q(__hash__)
Q(__next__)
Q(__qualname__)
Q(__path__)
Q(__repl_print__)

Q(__file__)


Q(__bool__)
Q(__contains__)
Q(__enter__)
Q(__exit__)
Q(__len__)
Q(__iter__)
Q(__getitem__)
Q(__setitem__)
Q(__delitem__)
Q(__add__)
Q(__sub__)
Q(__repr__)
Q(__str__)

Q(__get__)
Q(__set__)
Q(__delete__)

Q(__getattr__)
Q(__del__)
Q(__call__)
Q(__lt__)
Q(__gt__)
Q(__eq__)
Q(__le__)
Q(__ge__)
Q(__reversed__)

Q(__mul__)
Q(__truediv__)
Q(__floordiv__)
Q(__iadd__)
Q(__isub__)
Q(__invert__)
Q(__neg__)
Q(__pos__)


Q(micropython)
Q(bytecode)
Q(const)
# 114 "<stdin>"
Q(builtins)

Q(Ellipsis)
Q(StopIteration)

Q(NotImplemented)


Q(BaseException)
Q(ArithmeticError)
Q(AssertionError)
Q(AttributeError)
Q(BufferError)
Q(EOFError)
Q(Exception)
Q(FileExistsError)
Q(FileNotFoundError)
Q(FloatingPointError)
Q(GeneratorExit)
Q(ImportError)
Q(IndentationError)
Q(IndexError)
Q(KeyboardInterrupt)
Q(KeyError)
Q(LookupError)
Q(MemoryError)
Q(NameError)
Q(NotImplementedError)
Q(OSError)



Q(OverflowError)
Q(RuntimeError)
Q(SyntaxError)
Q(SystemExit)
Q(TypeError)
Q(UnboundLocalError)
Q(ValueError)



Q(ZeroDivisionError)

Q(UnicodeError)


Q(None)
Q(False)
Q(True)
Q(object)

Q(NoneType)


Q(OrderedDict)


Q(abs)
Q(all)
Q(any)
Q(args)

Q(array)

Q(bin)
Q({:#b})
Q(bool)

Q(bytearray)


Q(memoryview)

Q(bytes)
Q(callable)
Q(chr)
Q(classmethod)
Q(_collections)

Q(complex)
Q(real)
Q(imag)

Q(dict)
Q(dir)
Q(divmod)
Q(enumerate)
Q(eval)
Q(exec)



Q(filter)

Q(float)

Q(from_bytes)
Q(getattr)
Q(setattr)
Q(globals)
Q(hasattr)
Q(hash)
Q(hex)
Q(%#x)
Q(id)
Q(int)
Q(isinstance)
Q(issubclass)
Q(iter)
Q(len)
Q(list)
Q(locals)
Q(map)
Q(max)
Q(min)
Q(namedtuple)
Q(next)
Q(oct)
Q(%#o)
Q(open)
Q(ord)
Q(path)
Q(pow)
Q(print)
Q(range)
Q(read)
Q(repr)
Q(reversed)
Q(round)
Q(sorted)
Q(staticmethod)
Q(sum)
Q(super)
Q(str)
Q(sys)
Q(to_bytes)
Q(tuple)
Q(type)
Q(value)
Q(write)
Q(zip)


Q(compile)
Q(code)
Q(single)


Q(sep)
Q(end)


Q(step)
Q(stop)


Q(clear)
Q(copy)
Q(fromkeys)
Q(get)
Q(items)
Q(keys)
Q(pop)
Q(popitem)
Q(setdefault)
Q(update)
Q(values)
Q(append)
Q(close)
Q(send)
Q(throw)
Q(count)
Q(extend)
Q(index)
Q(remove)
Q(insert)
Q(pop)
Q(sort)
Q(join)
Q(strip)
Q(lstrip)
Q(rstrip)
Q(format)
Q(key)
Q(reverse)
Q(add)
Q(clear)
Q(copy)
Q(pop)
Q(remove)
Q(find)
Q(rfind)
Q(rindex)
Q(split)

Q(splitlines)
Q(keepends)
Q(\n)

Q(rsplit)
Q(startswith)
Q(endswith)
Q(replace)
Q(partition)
Q(rpartition)
Q(lower)
Q(upper)
Q(isspace)
Q(isalpha)
Q(isdigit)
Q(isupper)
Q(islower)
Q(iterable)
Q(start)

Q(bound_method)
Q(closure)
Q(dict_view)
Q(function)
Q(generator)
Q(iterator)
Q(module)
Q(slice)


Q(discard)
Q(difference)
Q(difference_update)
Q(intersection)
Q(intersection_update)
Q(isdisjoint)
Q(issubset)
Q(issuperset)
Q(set)
Q(symmetric_difference)
Q(symmetric_difference_update)
Q(union)
Q(update)



Q(frozenset)



Q(math)
Q(e)
Q(pi)
Q(sqrt)
Q(pow)
Q(exp)
Q(expm1)
Q(log)
Q(log2)
Q(log10)
Q(cosh)
Q(sinh)
Q(tanh)
Q(acosh)
Q(asinh)
Q(atanh)
Q(cos)
Q(sin)
Q(tan)
Q(acos)
Q(asin)
Q(atan)
Q(atan2)
Q(ceil)
Q(copysign)
Q(fabs)
Q(fmod)
Q(floor)
Q(isfinite)
Q(isinf)
Q(isnan)
Q(trunc)
Q(modf)
Q(frexp)
Q(ldexp)
Q(degrees)
Q(radians)

Q(erf)
Q(erfc)
Q(gamma)
Q(lgamma)




Q(cmath)
Q(phase)
Q(polar)
Q(rect)




Q(mem_total)
Q(mem_current)
Q(mem_peak)

Q(mem_info)
Q(qstr_info)





Q(maximum recursion depth exceeded)

Q(<module>)
Q(<lambda>)
Q(<listcomp>)
Q(<dictcomp>)
Q(<setcomp>)
Q(<genexpr>)
Q(<string>)
Q(<stdin>)


Q(encode)
Q(decode)
Q(utf-8)



Q(argv)
Q(byteorder)
Q(big)
Q(exit)
Q(little)

Q(platform)

Q(stdin)
Q(stdout)
Q(stderr)



Q(version)
Q(version_info)

Q(name)

Q(implementation)

Q(maxsize)


Q(exc_info)

Q(print_exception)



Q(ustruct)
Q(pack)
Q(unpack)
Q(calcsize)



Q(uctypes)
Q(struct)
Q(sizeof)
Q(addressof)
Q(bytes_at)
Q(bytearray_at)

Q(NATIVE)
Q(LITTLE_ENDIAN)
Q(BIG_ENDIAN)

Q(VOID)

Q(UINT8)
Q(INT8)
Q(UINT16)
Q(INT16)
Q(UINT32)
Q(INT32)
Q(UINT64)
Q(INT64)

Q(BFUINT8)
Q(BFINT8)
Q(BFUINT16)
Q(BFINT16)
Q(BFUINT32)
Q(BFINT32)

Q(FLOAT32)
Q(FLOAT64)

Q(ARRAY)
Q(PTR)


Q(BF_POS)
Q(BF_LEN)



Q(_io)
Q(readall)
Q(readinto)
Q(readline)
Q(readlines)
Q(seek)
Q(FileIO)
Q(TextIOWrapper)
Q(StringIO)
Q(BytesIO)
Q(getvalue)
Q(file)
Q(mode)
Q(r)
Q(encoding)



Q(gc)
Q(collect)
Q(disable)
Q(enable)
Q(isenabled)
Q(mem_free)
Q(mem_alloc)



Q(property)
Q(getter)
Q(setter)
Q(deleter)



Q(uzlib)
Q(decompress)



Q(ujson)
Q(dumps)
Q(loads)



Q(ure)
Q(compile)
Q(match)
Q(search)
Q(group)
Q(DEBUG)



Q(uheapq)
Q(heappush)
Q(heappop)
Q(heapify)



Q(uhashlib)
Q(update)
Q(digest)
Q(sha256)



Q(ubinascii)
Q(hexlify)
Q(unhexlify)
Q(a2b_base64)
Q(b2a_base64)



Q(machine)
Q(mem)
Q(mem8)
Q(mem16)
Q(mem32)
# 632 "<stdin>"
Q(Test)

Q(fileno)
Q(makefile)

Q(FileIO)
Q(flush)

Q(_os)
Q(stat)
Q(system)
Q(unlink)

Q(ffi)
Q(ffimod)
Q(ffifunc)
Q(fficallback)
Q(ffivar)
Q(as_bytearray)
Q(callback)
Q(addr)
Q(func)
Q(var)
Q(get)
Q(set)

Q(input)
Q(utime)
Q(time)
Q(clock)
Q(sleep)

Q(socket)
Q(sockaddr_in)
Q(htons)
Q(inet_aton)
Q(gethostbyname)
Q(getaddrinfo)
Q(usocket)
Q(connect)
Q(bind)
Q(listen)
Q(accept)
Q(recv)
Q(setsockopt)
Q(setblocking)

Q(AF_UNIX)
Q(AF_INET)
Q(AF_INET6)
Q(SOCK_STREAM)
Q(SOCK_DGRAM)
Q(SOCK_RAW)

Q(MSG_DONTROUTE)
Q(MSG_DONTWAIT)

Q(SOL_SOCKET)
Q(SO_BROADCAST)
Q(SO_ERROR)
Q(SO_KEEPALIVE)
Q(SO_LINGER)
Q(SO_REUSEADDR)
