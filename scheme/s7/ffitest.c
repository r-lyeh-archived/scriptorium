/* s7 ffi tester
 *
 * gcc -o ffitest ffitest.c -g3 -Wall s7.o -lm -I. -ldl
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include "s7.h"

#define TO_STR(x) s7_object_to_c_string(sc, x)
#define TO_S7_INT(x) s7_make_integer(sc, x)

static s7_pointer a_function(s7_scheme *sc, s7_pointer args)
{
  return(s7_car(args));
}

static s7_pointer test_hook_function(s7_scheme *sc, s7_pointer args)
{
  s7_pointer val;
  val = s7_symbol_local_value(sc, s7_make_symbol(sc, "a"), s7_car(args));
  if ((!s7_is_integer(val)) ||
      (s7_integer(val) != 1))
    {
      char *s1;
      s1 = TO_STR(val);
      fprintf(stderr, "%d: (hook 'a) is %s\n", __LINE__, s1);
      free(s1);
    }
  return(val);
}

static char last_c;
static void my_print(s7_scheme *sc, unsigned char c, s7_pointer port)
{
  last_c = c;
}

static s7_pointer my_read(s7_scheme *sc, s7_read_t peek, s7_pointer port)
{
  return(s7_make_character(sc, '0'));
}


static bool tested_begin_hook = false;
static void test_begin_hook(s7_scheme *sc, bool *val)
{
  tested_begin_hook = true;
}

static s7_pointer test_error_handler(s7_scheme *sc, s7_pointer args)
{
  s7_display(sc, s7_make_symbol(sc, "error!"), s7_current_error_port(sc));
  return(s7_f(sc));
}

static s7_pointer set_sym, set_val;
static s7_pointer scheme_set_notification(s7_scheme *sc, s7_pointer args)
{
  set_sym = s7_car(args);
  set_val = s7_cadr(args);
  return(set_val);
}


typedef struct {
  s7_double x;
  s7_pointer data;
} dax;

static char *print_dax(s7_scheme *sc, void *val)
{
  char *data_str, *str;
  int data_str_len;
  dax *o = (dax *)val;
  data_str = s7_object_to_c_string(sc, o->data);
  data_str_len = strlen(data_str);
  str = (char *)calloc(data_str_len + 32, sizeof(char));
  snprintf(str, data_str_len + 32, "#<dax %.3f %s>", o->x, data_str);
  free(data_str);
  return(str);
}

static void free_dax(void *val)
{
  if (val) free(val);
}

static bool equal_dax(void *val1, void *val2)
{
  return(val1 == val2);
}

static void mark_dax(void *val)
{
  dax *o = (dax *)val;
  if (o) s7_mark_object(o->data);
}

static int dax_type_tag = 0;

static s7_pointer make_dax(s7_scheme *sc, s7_pointer args)
{
  dax *o;
  o = (dax *)malloc(sizeof(dax));
  o->x = s7_real(s7_car(args));
  if (s7_cdr(args) != s7_nil(sc))
    o->data = s7_car(s7_cdr(args));
  else o->data = s7_nil(sc);
  return(s7_make_object(sc, dax_type_tag, (void *)o));
}

static s7_pointer is_dax(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_boolean(sc, 
			 s7_is_object(s7_car(args)) &&
			 s7_object_type(s7_car(args)) == dax_type_tag));
}

static s7_pointer dax_x(s7_scheme *sc, s7_pointer args)
{
  dax *o;
  o = (dax *)s7_object_value(s7_car(args));
  return(s7_make_real(sc, o->x));
}

static s7_pointer set_dax_x(s7_scheme *sc, s7_pointer args)
{
  dax *o;
  o = (dax *)s7_object_value(s7_car(args));
  o->x = s7_real(s7_car(s7_cdr(args)));
  return(s7_car(s7_cdr(args)));
}

static s7_pointer dax_data(s7_scheme *sc, s7_pointer args)
{
  dax *o;
  o = (dax *)s7_object_value(s7_car(args));
  return(o->data);
}

static s7_pointer set_dax_data(s7_scheme *sc, s7_pointer args)
{
  dax *o;
  o = (dax *)s7_object_value(s7_car(args));
  o->data = s7_car(s7_cdr(args));
  return(o->data);
}

static s7_pointer plus(s7_scheme *sc, s7_pointer args)
{
  /* (define* (plus (red 32) blue) (+ (* 2 red) blue)) */
  return(TO_S7_INT(2 * s7_integer(s7_car(args)) + s7_integer(s7_car(s7_cdr(args)))));
}

static s7_pointer mac_plus(s7_scheme *sc, s7_pointer args)
{
  /* (define-macro (plus a b) `(+ ,a ,b)) */
  s7_pointer a, b;
  a = s7_car(args);
  b = s7_cadr(args);
  return(s7_list(sc, 3, s7_make_symbol(sc, "+"),  a, b));
}

static s7_pointer mac_plus_mv(s7_scheme *sc, s7_pointer args)
{
  /* (define-macro (plus-mv a b) (values `(define a ,a) `(define b ,b))) */
  return(s7_values(sc, args));
}

static s7_pointer open_plus(s7_scheme *sc, s7_pointer args)
{
  #define plus_help "(plus obj ...) applies obj's plus method to obj and any trailing arguments."
  s7_pointer obj, method;
  obj = s7_car(args);
  if (s7_is_openlet(obj))
    {
      method = s7_method(sc, obj, s7_make_symbol(sc, "plus"));
      if (s7_is_procedure(method))
	return(s7_apply_function(sc, method, s7_cdr(args)));
    }
  return(s7_f(sc));
}

static s7_pointer multivector_ref(s7_scheme *sc, s7_pointer vector, int indices, ...)
{
  /* multivector_ref returns an element of a multidimensional vector */
  int ndims;
  ndims = s7_vector_rank(vector);

  if (ndims == indices)
    {
      va_list ap;
      s7_int index = 0;
      va_start(ap, indices);

      if (ndims == 1)
	{
	  index = va_arg(ap, s7_int);
	  va_end(ap);
	  return(s7_vector_ref(sc, vector, index));
	}
      else
	{
	  int i;
	  s7_pointer *elements;
	  s7_int *offsets, *dimensions;

	  elements = s7_vector_elements(vector);
	  dimensions = s7_vector_dimensions(vector);
	  offsets = s7_vector_offsets(vector);

	  for (i = 0; i < indices; i++)
	    {
	      int ind;
	      ind = va_arg(ap, int);
	      if ((ind < 0) ||
		  (ind >= dimensions[i]))
		{
		  va_end(ap);
		  return(s7_out_of_range_error(sc, 
                                               "multivector_ref", i, 
                                               s7_make_integer(sc, ind), 
                                               "index should be between 0 and the dimension size"));
		}
	      index += (ind * offsets[i]);
	    }
	  va_end(ap);
	  return(elements[index]);
	}
    }
  return(s7_wrong_number_of_args_error(sc, 
                                       "multivector_ref: wrong number of indices: ~A", 
                                       s7_make_integer(sc, indices)));
}


typedef struct {
  size_t size;
  double *data;
} g_block;    

static int g_block_type = 0;
static s7_pointer g_block_methods;

static s7_pointer g_make_block(s7_scheme *sc, s7_pointer args)
{
  #define g_make_block_help "(make-block size) returns a new block of the given size"
  g_block *g;
  s7_pointer new_g;
  g = (g_block *)calloc(1, sizeof(g_block));
  g->size = (size_t)s7_integer(s7_car(args));
  g->data = (double *)calloc(g->size, sizeof(double));
  new_g = s7_make_object(sc, g_block_type, (void *)g);
  s7_object_set_let(new_g, g_block_methods);
  s7_openlet(new_g);
  return(new_g);
}

static s7_pointer g_to_block(s7_scheme *sc, s7_pointer args)
{
  #define g_block_help "(block ...) returns a block object with the arguments as its contents."
  s7_pointer p, b;
  size_t i, len;
  g_block *gb;
  len = s7_list_length(sc, args);
  b = g_make_block(sc, s7_cons(sc, s7_make_integer(sc, len), s7_nil(sc)));
  gb = (g_block *)s7_object_value(b);
  for (i = 0, p = args; i < len; i++, p = s7_cdr(p))
    gb->data[i] = s7_number_to_real(sc, s7_car(p));
  return(b);
}

static char *g_block_display(s7_scheme *sc, void *value)
{
  return(strdup("#<block>"));
}

static void g_block_free(void *value)
{
  g_block *g = (g_block *)value;
  free(g->data);
  free(g);
}

static bool g_block_is_equal(void *val1, void *val2)
{
  return(val1 == val2);
}

static void g_block_mark(void *val)
{
  /* nothing to mark */
}

static s7_pointer g_block_ref(s7_scheme *sc, s7_pointer obj, s7_pointer args)
{
  g_block *g = (g_block *)s7_object_value(obj);
  size_t index;
  index = (size_t)s7_integer(s7_car(args));
  if (index < g->size)
    return(s7_make_real(sc, g->data[index]));
  return(s7_out_of_range_error(sc, "block-ref", 2, s7_car(args), "should be less than block length"));
}

static s7_pointer g_block_set(s7_scheme *sc, s7_pointer obj, s7_pointer args)
{
  g_block *g = (g_block *)s7_object_value(obj);
  s7_int index;
  index = s7_integer(s7_car(args));
  if ((index >= 0) && (index < g->size))
    {
      g->data[index] = s7_number_to_real(sc, s7_cadr(args));
      return(s7_cadr(args));
    }
  return(s7_out_of_range_error(sc, "block-set", 2, s7_car(args), "should be less than block length"));
}

static s7_pointer g_block_length(s7_scheme *sc, s7_pointer obj)
{
  g_block *g = (g_block *)s7_object_value(obj);
  return(s7_make_integer(sc, g->size));
}

static s7_pointer g_block_copy(s7_scheme *sc, s7_pointer args)
{
  s7_pointer obj, new_g;
  g_block *g, *g1;
  obj = s7_car(args);
  g = (g_block *)s7_object_value(obj);
  new_g = g_make_block(sc, s7_cons(sc, s7_make_integer(sc, g->size), s7_nil(sc)));
  g1 = (g_block *)s7_object_value(new_g);
  memcpy((void *)(g1->data), (void *)(g->data), g->size * sizeof(double));
  return(new_g);
}

static s7_pointer g_block_reverse(s7_scheme *sc, s7_pointer obj)
{
  size_t i, j;
  g_block *g = (g_block *)s7_object_value(obj);
  g_block *g1;
  s7_pointer new_g;
  new_g = g_make_block(sc, s7_cons(sc, s7_make_integer(sc, g->size), s7_nil(sc)));
  g1 = (g_block *)s7_object_value(new_g);
  for (i = 0, j = g->size - 1; i < g->size; i++, j--)
    g1->data[i] = g->data[j];
  return(new_g);
}

static s7_pointer g_block_fill(s7_scheme *sc, s7_pointer args)
{
  s7_pointer obj;
  size_t i;
  double fill_val;
  g_block *g;
  obj = s7_car(args);
  g = (g_block *)s7_object_value(obj);
  fill_val = s7_number_to_real(sc, s7_cadr(args));
  for (i = 0; i < g->size; i++)
    g->data[i] = fill_val;
  return(obj);
}


int main(int argc, char **argv)
{
  s7_scheme *sc;
  s7_pointer p, p1;
  int i, gc_loc;
  char *s1, *s2;
  
  sc = s7_init(); 
  
  /* try each straight (no errors) case */

  if (!s7_is_null(sc, s7_nil(sc))) 
    {fprintf(stderr, "%d: %s is not null?\n", __LINE__, s1 = TO_STR(s7_nil(sc))); free(s1);}

  if (s7_is_pair(s7_nil(sc))) 
    {fprintf(stderr, "%d: %s is a pair?\n", __LINE__, s1 = TO_STR(s7_nil(sc))); free(s1);}

  if (!s7_is_boolean(s7_t(sc))) 
    {fprintf(stderr, "%d: %s is not boolean?\n", __LINE__, s1 = TO_STR(s7_t(sc))); free(s1);}

  if (!s7_is_boolean(s7_f(sc))) 
    {fprintf(stderr, "%d: %s is not boolean?\n", __LINE__, s1 = TO_STR(s7_f(sc))); free(s1);}

  if (s7_boolean(sc, s7_f(sc)))
    {fprintf(stderr, "%d: %s is #t?\n", __LINE__, s1 = TO_STR(s7_f(sc))); free(s1);}

  if (!s7_boolean(sc, s7_t(sc)))
    {fprintf(stderr, "%d: %s is #f?\n", __LINE__, s1 = TO_STR(s7_t(sc))); free(s1);}

  p = s7_make_boolean(sc, true);
  if (p != s7_t(sc))
    {fprintf(stderr, "%d: %s is not #t?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  p = s7_make_boolean(sc, false);
  if (p != s7_f(sc))
    {fprintf(stderr, "%d: %s is not #f?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_eq(s7_f(sc), s7_f(sc))) 
    {fprintf(stderr, "%d: (eq? %s %s) -> #f?\n", __LINE__, s1 = TO_STR(s7_f(sc)), s2 = TO_STR(s7_f(sc))); free(s1); free(s2);}

  if (!s7_is_eqv(s7_f(sc), s7_f(sc))) 
    {fprintf(stderr, "%d: (eqv? %s %s) -> #f?\n", __LINE__, s1 = TO_STR(s7_f(sc)), s2 = TO_STR(s7_f(sc))); free(s1); free(s2);}

  if (!s7_is_equal(sc, s7_f(sc), s7_f(sc))) 
    {fprintf(stderr, "%d: (equal? %s %s) -> #f?\n", __LINE__, s1 = TO_STR(s7_f(sc)), s2 = TO_STR(s7_f(sc))); free(s1); free(s2);}

  if (!s7_is_unspecified(sc, s7_unspecified(sc))) 
    {fprintf(stderr, "%d: %s is not #<unspecified>?\n", __LINE__, s1 = TO_STR(s7_unspecified(sc))); free(s1);}

  if (s7_is_eq(s7_eof_object(sc), s7_undefined(sc)))
    {fprintf(stderr, "%d: (eq? %s %s) -> #t?\n", __LINE__, s1 = TO_STR(s7_eof_object(sc)), s2 = TO_STR(s7_undefined(sc))); free(s1); free(s2);}

  if (s7_is_eqv(s7_eof_object(sc), s7_undefined(sc)))
    {fprintf(stderr, "%d: (eqv? %s %s) -> #t?\n", __LINE__, s1 = TO_STR(s7_eof_object(sc)), s2 = TO_STR(s7_undefined(sc))); free(s1); free(s2);}

  if (s7_is_equal(sc, s7_eof_object(sc), s7_undefined(sc)))
    {fprintf(stderr, "%d: (equal? %s %s) -> #t?\n", __LINE__, s1 = TO_STR(s7_eof_object(sc)), s2 = TO_STR(s7_undefined(sc))); free(s1); free(s2);}

  if (!s7_is_valid(sc, s7_t(sc)))
    {fprintf(stderr, "%d: %s is not valid?\n", __LINE__, s1 = TO_STR(s7_t(sc))); free(s1);}

  if (s7_is_c_pointer(s7_t(sc)))
    {fprintf(stderr, "%d: %s is a raw c pointer?\n", __LINE__, s1 = TO_STR(s7_t(sc))); free(s1);}

  i = 32;
  p = s7_make_c_pointer(sc, (void *)(&i));
  if (!s7_is_c_pointer(p))
    {fprintf(stderr, "%d: %s is not a raw c pointer?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  i = (*((int *)s7_c_pointer(p)));
  if (i != 32)
    fprintf(stderr, "%d: 32 -> %d via raw c pointer?\n", __LINE__, i);

  s7_provide(sc, "ffitest");
  if (!s7_is_provided(sc, "ffitest"))
    {fprintf(stderr, "%d: *features* %s doesn't provide 'ffitest?\n", __LINE__, s1 = TO_STR(s7_name_to_value(sc, "*features*"))); free(s1);}

  p = s7_cons(sc, s7_f(sc), s7_t(sc));
  gc_loc = s7_gc_protect(sc, p);
  if (p != s7_gc_protected_at(sc, gc_loc))
    {fprintf(stderr, "%d: %s is not gc protected at %d: %s?\n", __LINE__, s1 = TO_STR(p), gc_loc, s2 = TO_STR(s7_gc_protected_at(sc, gc_loc))); free(s1); free(s2);}
  
  if (s7_car(p) != s7_f(sc))
    {fprintf(stderr, "%d: (car %s) is not #f?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_cdr(p) != s7_t(sc))
    {fprintf(stderr, "%d: (cdr %s) is not #t?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  if (!s7_is_pair(p))
    {fprintf(stderr, "%d: %s is not a pair?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_set_car(p, s7_eof_object(sc));
  if (s7_car(p) != s7_eof_object(sc))
    {fprintf(stderr, "%d: (car %s) is not #<eof>?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_set_cdr(p, s7_unspecified(sc));
  if (s7_cdr(p) != s7_unspecified(sc))
    {fprintf(stderr, "%d: (cdr %s) is not #<unspecified>?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_gc_unprotect_at(sc, gc_loc);


  
  p = TO_S7_INT(123);
  gc_loc = s7_gc_protect(sc, p);

  if (!s7_is_integer(p))
    {fprintf(stderr, "%d: %s is not integral?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_rational(p))
    {fprintf(stderr, "%d: %s is not rational?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_is_ratio(p))
    {fprintf(stderr, "%d: %s is a ratio?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_real(p))
    {fprintf(stderr, "%d: %s is not real?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_complex(p))
    {fprintf(stderr, "%d: %s is not complex?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_number(p))
    {fprintf(stderr, "%d: %s is not complex?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_integer(p) != 123)
    {fprintf(stderr, "%d: %s is not 123?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s2 = s7_number_to_string(sc, p, 10);
  if (strcmp(s2, "123") != 0)
    {fprintf(stderr, "%d: (number->string %s) is not \"123\"?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  free(s2);

  if (s7_number_to_integer(sc, p) != 123)
    {fprintf(stderr, "%d: s7_number_to_integer %s is not 123?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_gc_unprotect_at(sc, gc_loc);
  

  p = s7_make_ratio(sc, 123, 5);
  gc_loc = s7_gc_protect(sc, p);

  if (s7_is_integer(p))
    {fprintf(stderr, "%d: %s is integral?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_rational(p))
    {fprintf(stderr, "%d: %s is not rational?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_ratio(p))
    {fprintf(stderr, "%d: %s is not a ratio?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_real(p))
    {fprintf(stderr, "%d: %s is not real?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_complex(p))
    {fprintf(stderr, "%d: %s is not complex?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_number(p))
    {fprintf(stderr, "%d: %s is not complex?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_numerator(p) != 123)
    {fprintf(stderr, "%d: (numerator %s) is not 123?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_denominator(p) != 5)
    {fprintf(stderr, "%d: (denominator %s) is not 5?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s2 = s7_number_to_string(sc, p, 10);
  if (strcmp(s2, "123/5") != 0)
    {fprintf(stderr, "%d: (number->string %s) is not \"123/5\"?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  free(s2);

  s7_gc_unprotect_at(sc, gc_loc);


  p = s7_make_real(sc, 1.5);
  gc_loc = s7_gc_protect(sc, p);

  if (s7_is_integer(p))
    {fprintf(stderr, "%d: %s is integral?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_is_rational(p))
    {fprintf(stderr, "%d: %s is rational?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_is_ratio(p))
    {fprintf(stderr, "%d: %s is a ratio?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_real(p))
    {fprintf(stderr, "%d: %s is not real?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_complex(p))
    {fprintf(stderr, "%d: %s is not complex?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_number(p))
    {fprintf(stderr, "%d: %s is not complex?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_real(p) != 1.5)
    {fprintf(stderr, "%d: %s is not 1.5?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s2 = s7_number_to_string(sc, p, 10);
  if (strcmp(s2, "1.5") != 0)
    {fprintf(stderr, "%d: (number->string %s) is not \"1.5\"?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  free(s2);

  if (s7_number_to_real(sc, p) != 1.5)
    {fprintf(stderr, "%d: s7_number_to_real %s is not 1.5?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_is_mutable(p))
    {fprintf(stderr, "%d: %s is mutable?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_gc_unprotect_at(sc, gc_loc);
  

  p = s7_make_complex(sc, 1.0, 1.0);
  gc_loc = s7_gc_protect(sc, p);

  if (s7_is_integer(p))
    {fprintf(stderr, "%d: %s is integral?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_is_rational(p))
    {fprintf(stderr, "%d: %s is rational?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_is_ratio(p))
    {fprintf(stderr, "%d: %s is a ratio?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_is_real(p))
    {fprintf(stderr, "%d: %s is real?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_complex(p))
    {fprintf(stderr, "%d: %s is not complex?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_number(p))
    {fprintf(stderr, "%d: %s is not complex?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_real_part(p) != 1.0)
    {fprintf(stderr, "%d: (real-part %s) is not 1.0?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_imag_part(p) != 1.0)
    {fprintf(stderr, "%d: (imag-part %s) is not 1.0?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s2 = s7_number_to_string(sc, p, 10);
  if (strcmp(s2, "1+1i") != 0)
    {fprintf(stderr, "%d: (number->string %s) is not \"1+1i\"?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  free(s2);

  s7_gc_unprotect_at(sc, gc_loc);


  p = s7_rationalize(sc, 1.5, 1e-12);
  gc_loc = s7_gc_protect(sc, p);
  s1 = TO_STR(p);
  if (strcmp(s1, "3/2") != 0)
    fprintf(stderr, "%d: ratio is %s?\n", __LINE__, s1);
  free(s1);
  s7_gc_unprotect_at(sc, gc_loc);
  

  p = s7_make_vector(sc, 12);
  gc_loc = s7_gc_protect(sc, p);

  if (!s7_is_vector(p))
    {fprintf(stderr, "%d: %s is not a vector?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_vector_rank(p) != 1)
    {fprintf(stderr, "%d: (dimensions %s) is not 1?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_vector_set(sc, p, 1, s7_t(sc));
  if (s7_vector_ref(sc, p, 1) != s7_t(sc))
    {fprintf(stderr, "%d: (%s 1) is not #t?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_vector_fill(sc, p, TO_S7_INT(123));
  if (s7_integer(s7_vector_ref(sc, p, 1)) != 123)
    {fprintf(stderr, "%d: (%s 1) is not 123?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_gc_unprotect_at(sc, gc_loc);

  p = s7_make_and_fill_vector(sc, 3, TO_S7_INT(3));
  gc_loc = s7_gc_protect(sc, p);

  if (s7_integer(s7_vector_ref(sc, p, 1)) != 3)
    {fprintf(stderr, "%d: (%s 1) is not 3?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  p1 = s7_vector_copy(sc, p);
  if ((p == p1) ||
      (!s7_is_vector(p1)))
    {fprintf(stderr, "%d: copied vector: %s\n", __LINE__, s1 = TO_STR(p1)); free(s1);}
  s7_gc_unprotect_at(sc, gc_loc);


  p = s7_make_string(sc, "1234");
  gc_loc = s7_gc_protect(sc, p);

  if (!s7_is_string(p))
    {fprintf(stderr, "%d: %s is not a string?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_string_length(p) != 4)
    {fprintf(stderr, "%d: (length %s) is 4?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (strcmp(s7_string(p), "1234") != 0)
    {fprintf(stderr, "%d: %s is \"1234\"?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_gc_unprotect_at(sc, gc_loc);

  
  p = s7_make_character(sc, 65);
  if (!s7_is_character(p))
    {fprintf(stderr, "%d: %s is not a character?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_character(p) != 'A')
    {fprintf(stderr, "%d: %s is not #\\A?\n", __LINE__, s1 = TO_STR(p)); free(s1);}


  p = s7_list(sc, 3, TO_S7_INT(1), TO_S7_INT(2), TO_S7_INT(3));
  gc_loc = s7_gc_protect(sc, p);

  if (!s7_is_list(sc, p))
    {fprintf(stderr, "%d: %s is not a list?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  if (s7_list_length(sc, p) != 3)
    {fprintf(stderr, "%d: (length %s) is not 3?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  if (s7_integer(s7_list_ref(sc, p, 1)) != 2)
    {fprintf(stderr, "%d: (%s 1) is not 2?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  if (s7_integer(s7_car(p)) != 1)
    {fprintf(stderr, "%d: (car %s) is not 1?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  if (s7_integer(s7_cadr(p)) != 2)
    {fprintf(stderr, "%d: (cadr %s) is not 2?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  if (s7_integer(s7_caddr(p)) != 3)
    {fprintf(stderr, "%d: (caddr %s) is not 2?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  if (s7_integer(s7_car(s7_cddr(p))) != 3)
    {fprintf(stderr, "%d: (car (cddr %s)) is not 2?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  s7_list_set(sc, p, 1, s7_f(sc));
  if (s7_list_ref(sc, p, 1) != s7_f(sc))
    {fprintf(stderr, "%d: (%s 1) is not #f?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  s7_gc_unprotect_at(sc, gc_loc);


  {
    s7_pointer c1, c2, c3, c12, c23, c123, c1234, c1d2, c2d3, c3d4, c12d3, c23d4, c123d4, c1234d5;
    s7_gc_on(sc, false);
    c1 = s7_list(sc, 1, TO_S7_INT(1));                                              /* (1) */
    c2 = s7_list(sc, 1, TO_S7_INT(2));                                              /* (2) */
    c3 = s7_list(sc, 1, TO_S7_INT(3));                                              /* (3) */
    c12 = s7_list(sc, 2, TO_S7_INT(1), TO_S7_INT(2));                               /* (1 2) */
    c23 = s7_list(sc, 2, TO_S7_INT(2), TO_S7_INT(3));                               /* (2 3) */
    c123 = s7_list(sc, 3, TO_S7_INT(1), TO_S7_INT(2), TO_S7_INT(3));                /* (1 2 3) */
    c1234 = s7_list(sc, 4, TO_S7_INT(1), TO_S7_INT(2), TO_S7_INT(3), TO_S7_INT(4)); /* (1 2 3 4) */
    c1d2 = s7_cons(sc, TO_S7_INT(1), TO_S7_INT(2));                                 /* (1 . 2) */
    c2d3 = s7_cons(sc, TO_S7_INT(2), TO_S7_INT(3));                                 /* (2 . 3) */
    c3d4 = s7_cons(sc, TO_S7_INT(3), TO_S7_INT(4));                                 /* (3 . 4) */
    c12d3 = s7_cons(sc, TO_S7_INT(1), s7_cons(sc, TO_S7_INT(2), TO_S7_INT(3)));     /* (1 2 . 3) */
    c23d4 = s7_cons(sc, TO_S7_INT(2), s7_cons(sc, TO_S7_INT(3), TO_S7_INT(4)));     /* (2 3 . 4) */
    c123d4 = s7_cons(sc, TO_S7_INT(1), s7_cons(sc, TO_S7_INT(2), s7_cons(sc, TO_S7_INT(3), TO_S7_INT(4))));                             /* (1 2 3 . 4) */
    c1234d5 = s7_cons(sc, TO_S7_INT(1), s7_cons(sc, TO_S7_INT(2), s7_cons(sc, TO_S7_INT(3), s7_cons(sc, TO_S7_INT(4), TO_S7_INT(5))))); /* (1 2 3 4 . 5) */
    
    if (s7_integer(p = s7_caar(s7_list(sc, 1, c1))) != 1)
      {fprintf(stderr, "%d: caar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cadr(c12)) != 2)
      {fprintf(stderr, "%d: cadr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cdar(s7_list(sc, 1, c1d2))) != 2)
      {fprintf(stderr, "%d: cdar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cddr(c12d3)) != 3)
      {fprintf(stderr, "%d: cddr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_caaar(s7_list(sc, 1, s7_list(sc, 1, c1)))) != 1)
      {fprintf(stderr, "%d: caaar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_caadr(s7_list(sc, 2, TO_S7_INT(1), c2))) != 2)
      {fprintf(stderr, "%d: caadr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cadar(s7_list(sc, 1, c12))) != 2)
      {fprintf(stderr, "%d: cadar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cdaar(s7_list(sc, 1, s7_list(sc, 1, c1d2)))) != 2)
      {fprintf(stderr, "%d: cdaar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_caddr(c123)) != 3)
      {fprintf(stderr, "%d: caddr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cdddr(c123d4)) != 4)
      {fprintf(stderr, "%d: cdddr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cdadr(s7_list(sc, 2, TO_S7_INT(1), c2d3))) != 3)
      {fprintf(stderr, "%d: cdadr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cddar(s7_list(sc, 1, c12d3))) != 3)
      {fprintf(stderr, "%d: cddar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_caaaar(s7_list(sc, 1, s7_list(sc, 1, s7_list(sc, 1, c1))))) != 1)
      {fprintf(stderr, "%d: caaaar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_caaadr(s7_list(sc, 2, TO_S7_INT(1), s7_list(sc, 1, c2)))) != 2)
      {fprintf(stderr, "%d: caaadr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_caadar(s7_list(sc, 1, s7_list(sc, 2, TO_S7_INT(1), c2)))) != 2)
      {fprintf(stderr, "%d: caadar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cadaar(s7_list(sc, 1, s7_list(sc, 1, c12)))) != 2)
      {fprintf(stderr, "%d: cadaar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_caaddr(s7_list(sc, 3, TO_S7_INT(1), TO_S7_INT(2), c3))) != 3)
      {fprintf(stderr, "%d: caaddr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cadddr(c1234)) != 4)
      {fprintf(stderr, "%d: cadddr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cadadr(s7_list(sc, 2, TO_S7_INT(1), c23))) != 3)
      {fprintf(stderr, "%d: cadadr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_caddar(s7_list(sc, 1, c123))) != 3)
      {fprintf(stderr, "%d: caddar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cdaaar(s7_list(sc, 1, s7_list(sc, 1, s7_list(sc, 1, c1d2))))) != 2)
      {fprintf(stderr, "%d: cdaaar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cdaadr(s7_list(sc, 2, TO_S7_INT(1), s7_list(sc, 1, c2d3)))) != 3)
      {fprintf(stderr, "%d: cdaadr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cdadar(s7_list(sc, 1, s7_list(sc, 2, TO_S7_INT(1), c2d3)))) != 3)
      {fprintf(stderr, "%d: cdadar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cddaar(s7_list(sc, 1, s7_list(sc, 1, c12d3)))) != 3)
      {fprintf(stderr, "%d: cddaar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cdaddr(s7_list(sc, 3, TO_S7_INT(1), TO_S7_INT(2), c3d4))) != 4)
      {fprintf(stderr, "%d: cdaddr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cddddr(c1234d5)) != 5)
      {fprintf(stderr, "%d: cdddd is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cddadr(s7_list(sc, 2, TO_S7_INT(1), c23d4))) != 4)
      {fprintf(stderr, "%d: cddadr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cdddar(s7_list(sc, 1, c123d4))) != 4)
      {fprintf(stderr, "%d: cdddar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    p = s7_reverse(sc, c123);
    s1 = TO_STR(p);
    if (strcmp(s1, "(3 2 1)") != 0)
      {fprintf(stderr, "%d: (reverse '(1 2 3)) is %s?\n", __LINE__, s1);}
    free(s1);

    p = s7_append(sc, c1, c2);
    s1 = TO_STR(p);
    if (strcmp(s1, "(1 2)") != 0)
      {fprintf(stderr, "%d: (append '(1) '(2)) is %s?\n", __LINE__, s1);}
    free(s1);

    p = s7_list(sc, 2, s7_cons(sc, s7_make_symbol(sc, "a"), TO_S7_INT(32)), s7_cons(sc, s7_make_symbol(sc, "b"), TO_S7_INT(1)));
    p1 = s7_assq(sc, s7_make_symbol(sc, "a"), p);
    s1 = TO_STR(p1);
    if (strcmp(s1, "(a . 32)") != 0)
      {fprintf(stderr, "%d: (assq 'a '((a . 32) (b . 1)))) is %s?\n", __LINE__, s1);}
    free(s1);
    
    p1 = s7_assoc(sc, s7_make_symbol(sc, "b"), p);
    s1 = TO_STR(p1);
    if (strcmp(s1, "(b . 1)") != 0)
      {fprintf(stderr, "%d: (assoc 'b '((a . 32) (b . 1))) is %s?\n", __LINE__, s1);}
    free(s1);

    p = s7_member(sc, TO_S7_INT(2), c1234);
    s1 = TO_STR(p);
    if (strcmp(s1, "(2 3 4)") != 0)
      {fprintf(stderr, "%d: (member 2 '(1 2 3 4)) is %s?\n", __LINE__, s1);}
    free(s1);

    p = s7_list(sc, 2, s7_make_symbol(sc, "a"), s7_make_symbol(sc, "b"));
    p1 = s7_memq(sc, s7_make_symbol(sc, "b"), p);
    s1 = TO_STR(p1);
    if (strcmp(s1, "(b)") != 0)
      {fprintf(stderr, "%d: (memq 'b '(a b)) is %s?\n", __LINE__, s1);}
    free(s1);    

    s7_set_car(c1234, s7_make_symbol(sc, "+")); 
    p = s7_eval(sc, s7_list(sc, 2, s7_make_symbol(sc, "quote"), c1234), s7_sublet(sc, s7_rootlet(sc), s7_nil(sc)));
    if (s7_integer(p) != 9)
      {fprintf(stderr, "%d: (eval '(+ 2 3 4)) is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    p = s7_eval_form(sc, c1234, s7_sublet(sc, s7_rootlet(sc), s7_nil(sc)));
    if (s7_integer(p) != 9)
      {fprintf(stderr, "%d: (eval(form) '(+ 2 3 4)) is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    s7_gc_on(sc, true);
  }


  p = s7_make_ulong(sc, 123);
  gc_loc = s7_gc_protect(sc, p);

  if (!s7_is_ulong(p))
    {fprintf(stderr, "%d: %s is not a ulong?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  if (s7_ulong(p) != (unsigned long)123)
    {fprintf(stderr, "%d: %s is not 123?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  s7_gc_unprotect_at(sc, gc_loc);


  p = s7_make_ulong_long(sc, 123);
  gc_loc = s7_gc_protect(sc, p);

  if (!s7_is_ulong_long(p))
    {fprintf(stderr, "%d: %s is not a ulong_long?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  if (s7_ulong_long(p) != (unsigned long long)123)
    {fprintf(stderr, "%d: %s is not 123?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  s7_gc_unprotect_at(sc, gc_loc);


  p = s7_make_hash_table(sc, 255);
  gc_loc = s7_gc_protect(sc, p);

  if (!s7_is_hash_table(p))
    {fprintf(stderr, "%d: %s is not a hash-table?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  if (s7_hash_table_ref(sc, p, s7_eof_object(sc)) != s7_f(sc))
    {fprintf(stderr, "%d: (hash-table-ref %s #<eof>) is not #f?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_hash_table_set(sc, p, s7_eof_object(sc), s7_unspecified(sc));
  if (s7_hash_table_ref(sc, p, s7_eof_object(sc)) != s7_unspecified(sc))
    {fprintf(stderr, "%d: (hash-table-ref %s #<eof>) is not #<unspecified>?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  s7_gc_unprotect_at(sc, gc_loc);

  p = s7_current_input_port(sc);
  if (!s7_is_input_port(sc, p))
    {fprintf(stderr, "%d: %s is not an input port?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  p = s7_current_output_port(sc);
  if (!s7_is_output_port(sc, p))
    {fprintf(stderr, "%d: %s is not an output port?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  p = s7_name_to_value(sc, "abs");
  if (!s7_is_procedure(p))
    {fprintf(stderr, "%d: %s is not a procedure?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  p = s7_make_symbol(sc, "abs");
  if (!s7_is_symbol(p))
    {fprintf(stderr, "%d: %s is not a symbol?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  p = s7_gensym(sc, "abs");
  if (!s7_is_symbol(p))
    {fprintf(stderr, "%d: %s is not a symbol?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  p = s7_make_keyword(sc, "key");
  if (!s7_is_keyword(p))
    {fprintf(stderr, "%d: %s is not a keyword?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_eq(p, p))
    {fprintf(stderr, "%d: %s is not a self-eq??\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  p = s7_rootlet(sc);
  if (!s7_is_let(p))
    {fprintf(stderr, "%d: %s is not an environment?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  p = s7_curlet(sc);
  if ((!s7_is_null(sc, p)) && (!s7_is_let(p)))
    {fprintf(stderr, "%d: %s is not an environment?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  s7_define_constant(sc, "a_constant", s7_t(sc));
  if (!s7_is_constant(s7_name_to_value(sc, "a_constant")))
    {fprintf(stderr, "%d: a_constant is not a constant?\n", __LINE__);}
  if (!s7_is_defined(sc, "a_constant"))
    {fprintf(stderr, "%d: a_constant is not defined?\n", __LINE__);}
  
  s7_define_function(sc, "a_function", a_function, 1, 0, false, "a function");
  if (!s7_is_defined(sc, "a_function"))
    {fprintf(stderr, "%d: a_function is not defined?\n", __LINE__);}
  if (!s7_is_function(s7_name_to_value(sc, "a_function")))
    {fprintf(stderr, "%d: a_function is not a function?\n", __LINE__);}

  p = s7_apply_function(sc, s7_name_to_value(sc, "a_function"), s7_cons(sc, TO_S7_INT(32), s7_nil(sc)));
  if (!s7_is_integer(p))
    {fprintf(stderr, "%d: %s is not an integer?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  if (s7_integer(p) != 32)
    {fprintf(stderr, "%d: %s is not 32?\n", __LINE__, s1 = TO_STR(p)); free(s1);}


  dax_type_tag = s7_new_type("dax", print_dax, free_dax, equal_dax, mark_dax, NULL, NULL);
  s7_define_function(sc, "make-dax", make_dax, 2, 0, false, "(make-dax x data) makes a new dax");
  s7_define_function(sc, "dax?", is_dax, 1, 0, false, "(dax? anything) returns #t if its argument is a dax object");

  s7_define_variable(sc, "dax-x", 
                     s7_dilambda(sc, "dax-x", dax_x, 1, 0, set_dax_x, 2, 0, "dax x field (a real)"));

  s7_define_variable(sc, "dax-data", 
                     s7_dilambda(sc, "dax-data", dax_data, 1, 0, set_dax_data, 2, 0, "dax data field"));

  if (!s7_is_procedure_with_setter(s7_name_to_value(sc, "dax-x")))
    {fprintf(stderr, "%d: dax-x is not a pws?\n", __LINE__);}

  p = make_dax(sc, s7_cons(sc, s7_make_real(sc, 1.0), s7_cons(sc, TO_S7_INT(2), s7_nil(sc))));
  gc_loc = s7_gc_protect(sc, p);

  if (!s7_is_object(p))
    {fprintf(stderr, "%d: %s is not an object?\n", __LINE__, s1 = TO_STR(p)); free(s1);}    

  p1 = s7_apply_function(sc, s7_name_to_value(sc, "dax?"), s7_cons(sc, p, s7_nil(sc)));
  if (p1 != s7_t(sc))
    {fprintf(stderr, "%d: %s is not a dax object?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s1 = TO_STR(p);
  if (strcmp(s1, "#<dax 1.000 2>") != 0)
    {fprintf(stderr, "%d: dax prints as %s?\n", __LINE__, s2 = TO_STR(p)); free(s2);}    
  free(s1);

  p1 = s7_apply_function(sc, s7_name_to_value(sc, "dax-data"), s7_cons(sc, p, s7_nil(sc)));
  if (!s7_is_integer(p1))
    {fprintf(stderr, "%d: %s is not an integer?\n", __LINE__, s1 = TO_STR(p1)); free(s1);}
  if (s7_integer(p1) != 2)
    {fprintf(stderr, "%d: %s is not 2?\n", __LINE__, s1 = TO_STR(p1)); free(s1);}

  s7_apply_function(sc, s7_procedure_setter(sc, s7_name_to_value(sc, "dax-data")), s7_cons(sc, p, s7_cons(sc, TO_S7_INT(32), s7_nil(sc))));
  p1 = s7_apply_function(sc, s7_name_to_value(sc, "dax-data"), s7_cons(sc, p, s7_nil(sc)));
  if (!s7_is_integer(p1))
    {fprintf(stderr, "%d: %s is not an integer?\n", __LINE__, s1 = TO_STR(p1)); free(s1);}
  if (s7_integer(p1) != 32)
    {fprintf(stderr, "%d: %s is not 32?\n", __LINE__, s1 = TO_STR(p1)); free(s1);}

  s7_gc_unprotect_at(sc, gc_loc);


  s7_define_function_star(sc, "plus", plus, "(red 32) blue", "an example of define* from C");
  if (!s7_is_procedure(s7_name_to_value(sc, "plus")))
    {fprintf(stderr, "%d: plus is not a function?\n", __LINE__);}

  p = s7_apply_function(sc, s7_name_to_value(sc, "plus"), s7_cons(sc, TO_S7_INT(1), s7_cons(sc, TO_S7_INT(2), s7_nil(sc))));
  if (!s7_is_integer(p))
    {fprintf(stderr, "%d: %s is not an integer?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  if (s7_integer(p) != 4)
    {fprintf(stderr, "%d: %s is not 4?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  p = s7_apply_function(sc, s7_name_to_value(sc, "plus"), s7_cons(sc, s7_make_keyword(sc, "blue"), s7_cons(sc, TO_S7_INT(2), s7_nil(sc))));
  if (!s7_is_integer(p))
    {fprintf(stderr, "%d: %s is not an integer?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  if (s7_integer(p) != 66)
    {fprintf(stderr, "%d: %s is not 66?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  s7_define_variable(sc, "my-1", s7_make_integer(sc, 1));
  p = s7_name_to_value(sc, "my-1");
  if (!s7_is_integer(p))
    {fprintf(stderr, "%d: %s is not an integer?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_integer(p) != 1)
    {fprintf(stderr, "%d: %s is not 1?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_symbol_set_value(sc, s7_make_symbol(sc, "my-1"), s7_make_integer(sc, 32));
  p = s7_name_to_value(sc, "my-1");
  if (s7_integer(p) != 32)
    {fprintf(stderr, "%d: %s is not 32?\n", __LINE__, s1 = TO_STR(p)); free(s1);}


  s7_define_macro(sc, "mac-plus", mac_plus, 2, 0, false, "plus adds its two arguments");
  p = s7_eval_c_string(sc, "(mac-plus 2 3)");
  if (s7_integer(p) != 5)
    {fprintf(stderr, "%d: %s is not 5?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  p1 = s7_apply_function(sc, 
	s7_name_to_value(sc, "mac-plus"),
	s7_list(sc, 2, s7_make_integer(sc, 3), s7_make_integer(sc, 4)));
  p = s7_eval_form(sc, p1, s7_rootlet(sc));
  if ((!s7_is_integer(p)) ||
      (s7_integer(p) != 7))
    {char *s2; fprintf(stderr, "%d: %s -> %s is not 7?\n", __LINE__, s1 = TO_STR(p1), s2 = TO_STR(p)); free(s1); free(s2);}

  s7_define_macro(sc, "mac-plus-mv", mac_plus_mv, 2, 0, false, "macro values test");
  p = s7_eval_c_string(sc, "(let () (+ (mac-plus-mv 2 3)))");
  if (s7_integer(p) != 5)
    {fprintf(stderr, "%d: %s is not 5?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  

  s7_define_function(sc, "open-plus", open_plus, 1, 0, true, plus_help);
  p = s7_sublet(sc, s7_nil(sc), s7_cons(sc, s7_cons(sc, s7_make_symbol(sc, "plus"), s7_name_to_value(sc, "plus")), s7_nil(sc)));
  s7_openlet(p);
  p1 = s7_apply_function(sc, s7_name_to_value(sc, "open-plus"), s7_list(sc, 3, p, s7_make_integer(sc, 2), s7_make_integer(sc, 3)));
  if ((!s7_is_integer(p1)) ||
      (s7_integer(p1) != 7))
    {fprintf(stderr, "%d: %s is not 7?\n", __LINE__, s1 = TO_STR(p1)); free(s1);}


  s7_eval_c_string(sc,  "(define my-vect (make-vector '(2 3 4) 0))");
  s7_eval_c_string(sc,  "(set! (my-vect 1 1 1) 32)");
  p1 = s7_name_to_value(sc, "my-vect");

  p = multivector_ref(sc,  p1, 3, 0, 0, 0);
  if (s7_integer(p) != 0)
    {fprintf(stderr, "%d: %s is not 0?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  p = s7_vector_ref_n(sc,  p1, 3, 0, 0, 0);
  if (s7_integer(p) != 0)
    {fprintf(stderr, "%d: %s is not 0?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  p = multivector_ref(sc,  p1, 3, 1, 1, 1);
  if (s7_integer(p) != 32)
    {fprintf(stderr, "%d: %s is not 32?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  p = s7_vector_ref_n(sc,  p1, 3, 1, 1, 1);
  if (s7_integer(p) != 32)
    {fprintf(stderr, "%d: %s is not 32?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_vector_set_n(sc,  p1, TO_S7_INT(12), 3, 1, 1, 2);
  p = s7_vector_ref_n(sc,  p1, 3, 1, 1, 2);
  if (s7_integer(p) != 12)
    {fprintf(stderr, "%d: %s is not 12?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_vector_length(p1) != 24)
    {fprintf(stderr, "%d: (length %s) is not 24?\n", __LINE__, s1 = TO_STR(p1)); free(s1);}
  if (s7_vector_rank(p1) != 3)
    {fprintf(stderr, "%d: (vector-dimensions %s) is not 3?\n", __LINE__, s1 = TO_STR(p1)); free(s1);}

  {
    s7_int *dims, *offs;
    s7_pointer *els;
    dims = s7_vector_dimensions(p1);
    offs = s7_vector_offsets(p1);
    els = s7_vector_elements(p1);
    if (dims[0] != 2) fprintf(stderr, "%d: dims[0]: %lld?\n", __LINE__, dims[0]);
    if (dims[1] != 3) fprintf(stderr, "%d: dims[1]: %lld?\n", __LINE__, dims[1]);
    if (dims[2] != 4) fprintf(stderr, "%d: dims[2]: %lld?\n", __LINE__, dims[2]);
    if (offs[0] != 12) fprintf(stderr, "%d: offs[0]: %lld?\n", __LINE__, offs[0]);
    if (offs[1] != 4) fprintf(stderr, "%d: offs[1]: %lld?\n", __LINE__, offs[1]);
    if (s7_integer(p = els[12 + 4 + 1]) != 32)
      {fprintf(stderr, "%d: %s is not 32?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  }

  s7_vector_fill(sc, p1, s7_t(sc));
  p = s7_vector_ref_n(sc,  p1, 3, 1, 1, 1);
  if (p != s7_t(sc))
    {fprintf(stderr, "%d: %s is not #t?\n", __LINE__, s1 = TO_STR(p)); free(s1);}


  {
    s7_pointer new_env, old_env;
    new_env = s7_sublet(sc, old_env = s7_curlet(sc), s7_nil(sc));
    gc_loc = s7_gc_protect(sc, new_env);

    s7_define(sc, new_env, s7_make_symbol(sc, "var1"), s7_make_integer(sc, 32));

    if (new_env == s7_curlet(sc))
      {fprintf(stderr, "%d: %s is the current env?\n", __LINE__, s1 = TO_STR(new_env)); free(s1);}
    
    s1 = TO_STR(s7_let_to_list(sc, new_env));
    if (strcmp(s1, "((var1 . 32))") != 0)
      {fprintf(stderr, "%d: new-env is %s?\n", __LINE__, s1);}
    free(s1);
    
    p = s7_let_ref(sc, new_env, s7_make_symbol(sc, "var1"));
    if (s7_integer(p) != 32)
      {fprintf(stderr, "%d: %s is not 32?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    s7_let_set(sc, new_env, s7_make_symbol(sc, "var1"), TO_S7_INT(3));
    p = s7_let_ref(sc, new_env, s7_make_symbol(sc, "var1"));
    if (s7_integer(p) != 3)
      {fprintf(stderr, "%d: %s is not 3?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    s7_set_curlet(sc, new_env);
    p = s7_slot(sc, s7_make_symbol(sc, "var1"));
    if (s7_integer(s7_slot_value(p)) != 3)
      {fprintf(stderr, "%d: slot-value %s is not 3?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    s7_slot_set_value(sc, p, s7_f(sc));
    p = s7_let_ref(sc, new_env, s7_make_symbol(sc, "var1"));
    if (p != s7_f(sc))
      {fprintf(stderr, "%d: set slot-value %s is not #f?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    if (s7_outlet(new_env) != old_env)
      {fprintf(stderr, "%d: outer-env %s?\n", __LINE__, s1 = TO_STR(old_env)); free(s1);}

    s7_make_slot(sc, new_env, s7_make_symbol(sc, "var2"), TO_S7_INT(-1));
    p = s7_let_ref(sc, new_env, s7_make_symbol(sc, "var2"));
    if (s7_integer(p) != -1)
      {fprintf(stderr, "%d: make_slot %s is not -1?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

    s7_symbol_set_value(sc, s7_make_symbol(sc, "var2"), s7_t(sc));
    p = s7_symbol_local_value(sc, s7_make_symbol(sc, "var2"), new_env);
    if (p != s7_t(sc))
      {fprintf(stderr, "%d: set symbol-value %s is not #t?\n", __LINE__, s1 = TO_STR(p)); free(s1);}    
    
    p = s7_let_to_list(sc, new_env);
    {
      int gloc;
      gloc = s7_gc_protect(sc, p);
      s1 = TO_STR(p);
      if (strcmp(s1, "((var1 . #f) (var2 . #t))") != 0)
	{fprintf(stderr, "%d: env->list: %s\n", __LINE__, s1);}
      free(s1);
      s7_gc_unprotect_at(sc, gloc);
    }
    s7_set_curlet(sc, old_env);
    s7_gc_unprotect_at(sc, gc_loc);
  }

  if (!s7_is_list(sc, p = s7_load_path(sc)))
    {fprintf(stderr, "%d: %s is not a list?\n", __LINE__, s1 = TO_STR(p)); free(s1);}


  {
    s7_pointer port;
    port = s7_open_output_file(sc, "ffitest.scm", "w");

    if (!s7_is_output_port(sc, port))
      {fprintf(stderr, "%d: %s is not an output port?\n", __LINE__, s1 = TO_STR(port)); free(s1);}
    else
      {
	/* (define loaded_var 321) hopefully */
	gc_loc = s7_gc_protect(sc, port);
	s7_write_char(sc, (int)'(', port);
	s7_write(sc, s7_make_symbol(sc, "define"), port);
	s7_write_char(sc, (int)' ', port);
	s7_display(sc, s7_make_symbol(sc, "loaded_var"), port);
	s7_write_char(sc, (int)' ', port);
	s7_format(sc, s7_list(sc, 3, port, s7_make_string(sc, "~A)"), TO_S7_INT(321)));
	s7_newline(sc, port);
	s7_flush_output_port(sc, port);
	s7_close_output_port(sc, port);
	s7_gc_unprotect_at(sc, gc_loc);

	s7_load(sc, "ffitest.scm");
	if (!s7_is_defined(sc, "loaded_var"))
	  {fprintf(stderr, "%d: load unhappy?\n", __LINE__);}
	else
	  {
	    int c;
	    if (s7_integer(p = s7_name_to_value(sc, "loaded_var")) != 321)
	      {fprintf(stderr, "%d: %s is not 321?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

	    port = s7_open_input_file(sc, "ffitest.scm", "r");
	    if (!s7_is_input_port(sc, port))
	      {fprintf(stderr, "%d: %s is not an input port?\n", __LINE__, s1 = TO_STR(port)); free(s1);}
	    else
	      {
		gc_loc = s7_gc_protect(sc, port);
		c = s7_peek_char(sc, port);
		if (c != (int)'(')
		  {fprintf(stderr, "%d: peek-char sees %c?\n", __LINE__, (unsigned char)c);}
		
		c = s7_read_char(sc, port);
		if (c != (int)'(')
		  {fprintf(stderr, "%d: read-char sees %c?\n", __LINE__, (unsigned char)c);}
		
		s7_close_input_port(sc, port);
		s7_gc_unprotect_at(sc, gc_loc);

		port = s7_open_input_file(sc, "ffitest.scm", "r");
		gc_loc = s7_gc_protect(sc, port);

		p = s7_read(sc, port);
		s1 = TO_STR(p);
		if (strcmp(s1, "(define loaded_var 321)") != 0)
		  {fprintf(stderr, "%d: read file sees %s?\n", __LINE__, s1);}
		free(s1);

		s7_close_input_port(sc, port);
		s7_gc_unprotect_at(sc, gc_loc);
	      }
	  }
      }

    port = s7_open_input_string(sc, "(+ 1 2)");
    if (!s7_is_input_port(sc, port))
      {fprintf(stderr, "%d: %s is not an input port?\n", __LINE__, s1 = TO_STR(port)); free(s1);}
    gc_loc = s7_gc_protect(sc, port);
    p = s7_read(sc, port);
    s1 = TO_STR(p);
    if (strcmp(s1, "(+ 1 2)") != 0)
      {fprintf(stderr, "%d: read string sees %s?\n", __LINE__, s1);}
    free(s1);
    s7_close_input_port(sc, port);
    s7_gc_unprotect_at(sc, gc_loc);

    port = s7_open_output_string(sc);
    if (!s7_is_output_port(sc, port))
      {fprintf(stderr, "%d: %s is not an output port?\n", __LINE__, s1 = TO_STR(port)); free(s1);}
    gc_loc = s7_gc_protect(sc, port);
    s7_display(sc, s7_make_string(sc, "(+ 2 3)"), port);
    {
      const char *s2;
      s2 = s7_get_output_string(sc, port);
      if (strcmp(s2, "(+ 2 3)") != 0)
	{fprintf(stderr, "%d: read output string sees %s?\n", __LINE__, s2);}
    }
    s7_close_output_port(sc, port);
    s7_gc_unprotect_at(sc, gc_loc);

    p = s7_set_current_output_port(sc, s7_open_output_function(sc, my_print));
    p1 = s7_open_input_function(sc, my_read);
    gc_loc = s7_gc_protect(sc, p1);

    s7_display(sc, s7_make_character(sc, '3'), s7_current_output_port(sc));
    if (last_c != '3')
      {fprintf(stderr, "%d: last_c: %c, c: %c\n", __LINE__, last_c, '3');}
    last_c = s7_read_char(sc, p1);
    if (last_c != '0')
      {fprintf(stderr, "%d: last_c: %c\n", __LINE__, last_c);}
    s7_set_current_output_port(sc, p);
    s7_gc_unprotect_at(sc, gc_loc);
  }

  {
    s7_pointer port, val;
    s7_autoload(sc, s7_make_symbol(sc, "auto_var"), s7_make_string(sc, "ffitest.scm"));
    port = s7_open_output_file(sc, "ffitest.scm", "w");
    gc_loc = s7_gc_protect(sc, port);      
    s7_display(sc, s7_make_string(sc, "(define auto_var 123)"), port);
    s7_newline(sc, port);
    s7_close_output_port(sc, port);
    s7_gc_unprotect_at(sc, gc_loc);
    val = s7_eval_c_string(sc, "(+ auto_var 1)");
    if ((!s7_is_integer(val)) ||
	(s7_integer(val) != 124))
      {fprintf(stderr, "%d: auto_var+1 = %s?\n", __LINE__, s1 = TO_STR(val)); free(s1);}
  }
    
  {
    s7_pointer test_hook;
    test_hook = s7_eval_c_string(sc, "(make-hook 'a 'b)");
    s7_define_constant(sc, "test-hook", test_hook); 
    s7_hook_set_functions(sc, test_hook, 
			  s7_cons(sc, s7_make_function(sc, "test-hook-function", test_hook_function, 1, 0, false, "a test-hook function"), 
				  s7_hook_functions(sc, test_hook)));
    s7_call(sc, test_hook, s7_list(sc, 2, TO_S7_INT(1), TO_S7_INT(2)));
    s7_call_with_location(sc, test_hook, s7_list(sc, 2, TO_S7_INT(1), TO_S7_INT(2)), "ffitest", "ffitest.c", __LINE__);
  }

  {
    s7_pointer x, y, funcs;
    funcs = s7_eval_c_string(sc, "(let ((x 0)) (list (lambda () (set! x 1)) (lambda () (set! x (+ x 1))) (lambda () (set! x (+ x 1))) (lambda () x)))");
    gc_loc = s7_gc_protect(sc, funcs);
    y = s7_dynamic_wind(sc, s7_car(funcs), s7_cadr(funcs), s7_caddr(funcs));
    x = s7_call(sc, s7_cadddr(funcs), s7_nil(sc));
    if ((!s7_is_integer(x)) ||
	(!s7_is_integer(y)) ||
	(s7_integer(x) != 3) ||
	(s7_integer(y) != 2))
      fprintf(stderr, "s7_dynamic_wind: x: %s, y: %s\n", s7_object_to_c_string(sc, x), s7_object_to_c_string(sc, y));
    y = s7_dynamic_wind(sc, s7_f(sc), s7_car(funcs), s7_cadr(funcs));
    x = s7_call(sc, s7_cadddr(funcs), s7_nil(sc));
    if ((!s7_is_integer(x)) ||
	(!s7_is_integer(y)) ||
	(s7_integer(x) != 2) ||
	(s7_integer(y) != 1))
      fprintf(stderr, "s7_dynamic_wind (init #f): x: %s, y: %s\n", s7_object_to_c_string(sc, x), s7_object_to_c_string(sc, y));
    y = s7_dynamic_wind(sc, s7_f(sc), s7_cadr(funcs), s7_f(sc));
    x = s7_call(sc, s7_cadddr(funcs), s7_nil(sc));
    if ((!s7_is_integer(x)) ||
	(!s7_is_integer(y)) ||
	(s7_integer(x) != 3) ||
	(s7_integer(y) != 3))
      fprintf(stderr, "s7_dynamic_wind (init #f, finish #f): x: %s, y: %s\n", s7_object_to_c_string(sc, x), s7_object_to_c_string(sc, y));
    y = s7_dynamic_wind(sc, s7_cadr(funcs), s7_cadr(funcs), s7_f(sc));
    x = s7_call(sc, s7_cadddr(funcs), s7_nil(sc));
    if ((!s7_is_integer(x)) ||
	(!s7_is_integer(y)) ||
	(s7_integer(x) != 5) ||
	(s7_integer(y) != 5))
      fprintf(stderr, "s7_dynamic_wind (finish #f): x: %s, y: %s\n", s7_object_to_c_string(sc, x), s7_object_to_c_string(sc, y));
    s7_gc_unprotect_at(sc, gc_loc);
  }

  if (s7_begin_hook(sc) != NULL)
    {fprintf(stderr, "%d: begin_hook is not null?\n", __LINE__);}
  tested_begin_hook = false;
  s7_set_begin_hook(sc, test_begin_hook);
  s7_eval_c_string(sc, "(begin (+ 1 2))");
  if (!tested_begin_hook)
    {fprintf(stderr, "%d: begin_hook not called?\n", __LINE__);}
  if (s7_begin_hook(sc) != test_begin_hook)
    {fprintf(stderr, "%d: begin_hook was not set?\n", __LINE__);}
  s7_set_begin_hook(sc, NULL);

  
  p1 = s7_name_to_value(sc, "abs");
  if (!s7_is_procedure(p1))
    {fprintf(stderr, "%d: (procedure? abs) = #f?\n", __LINE__);}
  if (s7_is_macro(sc, p1))
    {fprintf(stderr, "%d: (macro? abs) = #t?\n", __LINE__);}
  
  if (!s7_is_aritable(sc, p1, 1))
    {fprintf(stderr, "%d: (aritable? abs 1) = #f?\n", __LINE__);}
  if (s7_is_aritable(sc, p1, 2))
    {fprintf(stderr, "%d: (aritable? abs 2) = #t?\n", __LINE__);}

  p = s7_funclet(sc, p1);
  if (p != s7_rootlet(sc))
    {fprintf(stderr, "%d: (funclet abs) = %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  {
    const char *s3;
    s3 = s7_procedure_documentation(sc, p1);
    if (strcmp(s3, "(abs x) returns the absolute value of the real number x") != 0)
      {fprintf(stderr, "%d: (procedure-documentation abs) = %s?\n", __LINE__, s3);}

    s3 = s7_help(sc, p1);
    if (strcmp(s3, "(abs x) returns the absolute value of the real number x") != 0)
      {fprintf(stderr, "%d: (help abs) = %s?\n", __LINE__, s3);}
  }

  p = s7_eval_c_string(sc, "(lambda (a b . c) (+ a b (apply * c)))");
  gc_loc = s7_gc_protect(sc, p);
  
  if (!s7_is_procedure(p))
    {fprintf(stderr, "%d: %s is not a procedure?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s1 = TO_STR(s7_closure_body(sc, p));
  if (strcmp(s1, "((+ a b (apply * c)))") != 0)
    {fprintf(stderr, "%d: s7_closure_body is %s?\n", __LINE__, s1);}
  free(s1);
  
  s1 = TO_STR(s7_closure_args(sc, p));
  if (strcmp(s1, "(a b . c)") != 0)
    {fprintf(stderr, "%d: s7_closure_args is %s?\n", __LINE__, s1);}
  free(s1);
  
  s1 = TO_STR(s7_closure_let(sc, p));
  if (strcmp(s1, "()") != 0)
    {fprintf(stderr, "%d: s7_closure_let is %s?\n", __LINE__, s1);}
  free(s1);
  
  if (!s7_is_aritable(sc, p, 2))
    {fprintf(stderr, "%d: aritable? lambda 2 = #f?\n", __LINE__);}
  if (s7_is_aritable(sc, p, 1))
    {fprintf(stderr, "%d: aritable? lambda 1 = #t?\n", __LINE__);}

  s7_gc_unprotect_at(sc, gc_loc);

  {
    /* iterators */
    s7_pointer iter, x;
    iter = s7_make_iterator(sc, s7_list(sc, 3, TO_S7_INT(1), TO_S7_INT(2), TO_S7_INT(3)));
    if (!s7_is_iterator(iter))
      fprintf(stderr, "%d: %s is not an interator\n", __LINE__, TO_STR(iter));
    if (s7_iterator_is_at_end(iter))
      fprintf(stderr, "%d: %s is prematurely done\n", __LINE__, TO_STR(iter));
    x = s7_iterate(sc, iter);
    if ((!s7_is_integer(x)) || (s7_integer(x) != 1))
      fprintf(stderr, "%d: %s should be 1\n", __LINE__, TO_STR(x));
    x = s7_iterate(sc, iter);
    if ((!s7_is_integer(x)) || (s7_integer(x) != 2))
      fprintf(stderr, "%d: %s should be 2\n", __LINE__, TO_STR(x));
    x = s7_iterate(sc, iter);
    if ((!s7_is_integer(x)) || (s7_integer(x) != 3))
      fprintf(stderr, "%d: %s should be 3\n", __LINE__, TO_STR(x));
    x = s7_iterate(sc, iter);
    if ((x != s7_eof_object(sc)) || (!s7_iterator_is_at_end(iter)))
      fprintf(stderr, "%d: %s should be #<eof> and iter should be done\n", __LINE__, TO_STR(x));
  }

  g_block_type = s7_new_type_x(sc, "#<block>", 
			       g_block_display, g_block_free, 
			       g_block_is_equal, g_block_mark,
			       g_block_ref, g_block_set, g_block_length, 
			       g_block_copy, g_block_reverse, g_block_fill);

  s7_define_function(sc, "make-block", g_make_block, 1, 0, false, g_make_block_help);
  s7_define_function(sc, "block", g_to_block, 0, 0, true, g_block_help);

  g_block_methods = s7_eval_c_string(sc, "(inlet (cons 'vector? (lambda (p) #t)))");
  s7_gc_protect(sc, g_block_methods);

  {
    g_block *g;
    s7_pointer gp;

    gp = g_make_block(sc, s7_list(sc, 1, TO_S7_INT(32)));
    gc_loc = s7_gc_protect(sc, gp);
    if (!s7_is_object(gp))
      {fprintf(stderr, "%d: g_block %s is not an object?\n", __LINE__, s1 = TO_STR(gp)); free(s1);}
    g = (g_block *)s7_object_value(gp);
    if (s7_object_type(gp) != g_block_type)
      {fprintf(stderr, "%d: g_block types: %d %d\n", __LINE__, g_block_type, s7_object_type(gp));}
    if (s7_object_value_checked(gp, g_block_type) != g)
      {fprintf(stderr, "%d: checked g_block types: %d %d\n", __LINE__, g_block_type, s7_object_type(gp));}

    s7_gc_unprotect_at(sc, gc_loc);
  }
  
  {                            
    s7_pointer old_port;
    const char *errmsg = NULL;

    old_port = s7_set_current_error_port(sc, s7_open_output_string(sc));
    gc_loc = s7_gc_protect(sc, old_port);

    s7_eval_c_string(sc, "(+ 1 #\\c)");
    errmsg = s7_get_output_string(sc, s7_current_error_port(sc));
    if (!errmsg)
      fprintf(stderr, "%d: no error!\n", __LINE__);

    s7_close_output_port(sc, s7_current_error_port(sc));
    s7_set_current_error_port(sc, old_port);
    s7_gc_unprotect_at(sc, gc_loc);
  }

  {                            
    s7_pointer old_port, result;
    const char *errmsg = NULL;

    s7_define_function(sc, "error-handler", test_error_handler, 1, 0, false, "our error handler");

    s7_eval_c_string(sc, "(set! (hook-functions *error-hook*)                                 \n\
                            (list (lambda (hook)                                              \n\
                                    (error-handler                                            \n\
				     (string-append \"hook: \" (apply format #f (hook 'data)))) \n\
                                    (set! (hook 'result) 'our-error))))");

    old_port = s7_set_current_error_port(sc, s7_open_output_string(sc));
    gc_loc = s7_gc_protect(sc, old_port);

    result = s7_eval_c_string(sc, "(+ 1 #\\c)");
    if (result != s7_make_symbol(sc, "our-error"))
      {fprintf(stderr, "%d: error hook result: %s\n", __LINE__, s1 = TO_STR(result)); free(s1);}
    errmsg = s7_get_output_string(sc, s7_current_error_port(sc));
    if ((errmsg) && (*errmsg))
      {
	if (strcmp(errmsg, "error!") != 0)
	  fprintf(stderr, "%d: error: %s\n", __LINE__, errmsg);
      }
    else fprintf(stderr, "%d: no error!\n", __LINE__);

    s7_close_output_port(sc, s7_current_error_port(sc));
    s7_set_current_error_port(sc, old_port);
    s7_gc_unprotect_at(sc, gc_loc);

    s7_eval_c_string(sc, "(set! (hook-functions *error-hook*) ())");
  }

  s7_define_function(sc, "notify-C", scheme_set_notification, 2, 0, false, "called if notified-var is set!");
  s7_define_variable(sc, "notified-var", s7_make_integer(sc, 0));
  s7_symbol_set_access(sc, s7_make_symbol(sc, "notified-var"), s7_name_to_value(sc, "notify-C"));
  s7_eval_c_string(sc, "(set! notified-var 32)");
  p = s7_name_to_value(sc, "notified-var");
  if (s7_integer(p) != 32)
    {fprintf(stderr, "%d: sym set: %s\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  if (s7_integer(set_val) != 32)
    {fprintf(stderr, "%d: sym val: %s\n", __LINE__, s1 = TO_STR(set_val)); free(s1);}
  if (set_sym != s7_make_symbol(sc, "notified-var"))
    {fprintf(stderr, "%d: sym: %s\n", __LINE__, s1 = TO_STR(set_sym)); free(s1);}
    
  return(0);
}

