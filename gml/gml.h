#ifndef GML_HDR
#define GML_HDR
#include "list.h"
#include <stdint.h>
#include <stddef.h>

#define GML_VER_MAJOR 0
#define GML_VER_MINOR 5
#define GML_VER_PATCH 0

typedef struct {
    const char *filename;
    size_t      line;
    size_t      column;
} gml_position_t;

typedef enum {
    GML_TYPE_NUMBER,
    GML_TYPE_STRING,
    GML_TYPE_ATOM,
    GML_TYPE_ARRAY,
    GML_TYPE_TABLE,
    GML_TYPE_NATIVE,
    GML_TYPE_FUNCTION
} gml_type_t;

/*
 * In GML a rune is a character of a string. Every character is thus 32bits.
 * This allows for a variety of character sets. How the implementation
 * choses to utilize the 32 bits is undefined. In this paticular case the
 * implementation is using UTF-8.
 */
typedef uint32_t gml_string_rune_t;

/*
 * An anonoums structure that represents a closure. Rather the enviroment
 * for closures to function.
 */
typedef struct gml_env_s   gml_env_t;

/*
 * An anonoums structure that represents the GML runtime state. This is
 * passed around as context.
 */
typedef struct gml_state_s gml_state_t;

/*
 * Values in GML are boxed and assumes that the machine's floating-point
 * representation is IEEE 754 64-bit binary. Additionally, GML requires
 * that the highest bit of the significand is used to indicate a
 * singling NaN when set.
 *
 * Numbers encode to themselves.
 *
 * Pointers to the heap are encoded in the unused 51-bits of the significand,
 * with the exponent set to all ones and the topmost bit of the significand
 * set to one such that regular NaNs are not mistaken as boxed pointers.
 */
typedef double gml_value_t;
typedef struct gml_header_s gml_header_t;

struct gml_header_s {
    gml_type_t    type;
    void        (*destroy)(gml_state_t *gml, gml_value_t value);
};

typedef gml_value_t (*gml_native_func_t)(gml_state_t *state, gml_value_t *value_array, size_t value_length);

gml_value_t gml_nil_create(gml_state_t *gml);
gml_value_t gml_true_create(gml_state_t *gml);
gml_value_t gml_false_create(gml_state_t *gml);
gml_value_t gml_number_create(gml_state_t *gml, double value);
double gml_number_value(gml_state_t *gml, gml_value_t number);
gml_value_t gml_native_create(gml_state_t *gml, gml_native_func_t func, int min, int max);
int gml_isfalse(gml_state_t *gml, gml_value_t value);
int gml_istrue(gml_state_t *gml, gml_value_t value);
int gml_equal(gml_state_t *gml, gml_value_t v1, gml_value_t v2);
int gml_same(gml_state_t *gml, gml_value_t v1, gml_value_t v2);
const char *gml_typename(gml_state_t *gml, gml_type_t type);
gml_type_t gml_value_typeof(gml_state_t *gml, gml_value_t value);
gml_state_t *gml_state_create(void);
void gml_state_destroy(gml_state_t *state);
size_t gml_dump(gml_state_t *gml, gml_value_t value, char *buffer, size_t length);
gml_value_t gml_run_string(gml_state_t *gml, const char *source);
gml_value_t gml_run_file(gml_state_t *gml, const char *filename);
void gml_env_bind(gml_env_t *env, const char *name, gml_value_t value);
int gml_env_lookup(gml_env_t *env, const char *name, gml_value_t **out);
gml_value_t gml_value_box(gml_state_t *gml, gml_header_t *value);
gml_header_t *gml_value_unbox(gml_state_t *gml, gml_value_t value);
void gml_set_global(gml_state_t *gml, const char *name, gml_value_t value);
void gml_set_native(gml_state_t *gml, const char *name, gml_native_func_t func, int min, int max);
gml_value_t gml_string_create(gml_state_t *gml, const char *string);
size_t gml_string_length(gml_state_t *gml, gml_value_t string);
gml_value_t gml_array_create(gml_state_t *gml, gml_value_t *elements, size_t length);
gml_type_t gml_arg_contract(char c);
void gml_arg_check(gml_state_t *gml, gml_value_t *args, size_t nargs, const char *name, const char *contract);
void gml_builtins_install(gml_state_t *gml);
char *gml_string_utf8data(gml_state_t *gml, gml_value_t string);
size_t gml_string_utf8length(gml_state_t *gml, gml_value_t string);
void gml_throw(int internal, const char *format, ...);
gml_value_t gml_string_substring(gml_state_t *gml, gml_value_t string, size_t start, size_t length);
void gml_table_put(gml_state_t *gml, gml_value_t dict, gml_value_t key, gml_value_t value);
gml_value_t gml_table_get(gml_state_t *gml, gml_value_t dict, gml_value_t key);
int gml_table_empty(gml_state_t *gml, gml_value_t dict);
list_t *gml_table_keys(gml_state_t *gml, gml_value_t dict);
gml_value_t gml_table_create(gml_state_t *gml);
gml_value_t gml_function_run(gml_state_t *gml, gml_value_t function, gml_value_t *args, size_t nargs);
size_t gml_array_length(gml_state_t *gml, gml_value_t array);
gml_value_t gml_array_get(gml_state_t *gml, gml_value_t array, size_t index);
gml_value_t gml_none_create(gml_state_t *gml);
void gml_state_user_set(gml_state_t *gml, void *user);
void *gml_state_user_get(gml_state_t *gml);

#endif
