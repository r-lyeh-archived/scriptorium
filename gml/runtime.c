#include "gml.h"
#include "parse.h"

#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdio.h>

/* Core runtime */
void gml_error(gml_position_t *position, const char *format, ...) {
    va_list args;
    va_start(args, format);
    fprintf(stderr, "%s:%zu:%zu: ", position->filename, position->line, position->column);
    vfprintf(stderr, format, args);
    fprintf(stderr, "\n");
    va_end(args);
}

void gml_throw(int internal, const char *format, ...) {
    va_list args;
    va_start(args, format);
    fprintf(stderr, (internal) ? "internal error: " : "error: ");
    vfprintf(stderr, format, args);
    fprintf(stderr, "\n");
    va_end(args);
}

const char *gml_typename(gml_state_t *gml, gml_type_t type) {
    (void)gml;
    switch (type) {
        case GML_TYPE_NUMBER:   return "number";
        case GML_TYPE_STRING:   return "string";
        case GML_TYPE_ATOM:     return "atom";
        case GML_TYPE_ARRAY:    return "array";
        case GML_TYPE_TABLE:    return "table";
        case GML_TYPE_NATIVE:   return "native";
        case GML_TYPE_FUNCTION: return "function";
    }
    return NULL;
}

/* Enviroment */
#define ENV_BUCKETS 7

typedef struct gml_env_binding_s gml_env_binding_t;

struct gml_env_binding_s {
    char              *name;
    gml_env_binding_t *next;
    gml_value_t        value;
};

struct gml_env_s {
    gml_env_binding_t *buckets[ENV_BUCKETS];
    gml_env_t         *outer;
    gml_env_t         *inner;
};

static gml_env_t *gml_env_push(gml_env_t *out) {
    gml_env_t *env = malloc(sizeof(*env));
    if (!env)
        return NULL;
    if (out)
        out->inner = env;
    memset(env->buckets, 0, sizeof(env->buckets));
    env->outer = out;
    env->inner = NULL;
    return env;
}

static gml_env_t *gml_env_create(void) {
    return gml_env_push(NULL);
}

static void gml_env_destroy(gml_env_t *env) {
    while (env->inner)
        env = env->inner;
    while (env) {
        for (size_t i = 0; i < ENV_BUCKETS; i++) {
            for (gml_env_binding_t *bind = env->buckets[i]; bind;) {
                gml_env_binding_t *next = bind->next;
                free(bind->name);
                free(bind);
                bind = next;
            }
        }
        if (env->outer) {
            gml_env_t *out = env->outer;
            free(env);
            env = out;
        } else {
            free(env);
            break;
        }
    }
}

static uint32_t gml_env_hash(const char *string) {
    const size_t length = strlen(string);
    uint32_t     hash   = 5381;
    for (size_t i = 0; i < length; i++)
        hash = hash * 33 ^ string[i];
    return hash;
}

static gml_env_binding_t **gml_env_bucket(gml_env_t *env, const char *name) {
    return &env->buckets[gml_env_hash(name) % ENV_BUCKETS];
}

void gml_env_bind(gml_env_t *env, const char *name, gml_value_t value) {
    gml_env_binding_t **bucket  = gml_env_bucket(env, name);
    gml_env_binding_t  *binding = malloc(sizeof(*binding));
    if (!binding)
        return;

    binding->name  = strdup(name);
    binding->value = value;
    binding->next  = *bucket;
    *bucket        = binding;
}

int gml_env_lookup(gml_env_t *env, const char *name, gml_value_t **out) {
    while (env) {
        gml_env_binding_t *binding = *gml_env_bucket(env, name);
        for (; binding; binding = binding->next) {
            if (!strcmp(binding->name, name)) {
                *out = &binding->value;
                return 1;
            }
        }
        env = env->outer;
    }
    return 0;
}

/* Builtins */
typedef struct {
    size_t   size;
    list_t **table;
} gml_ht_t;

typedef struct {
    char *key;
    void *value;
} gml_ht_entry_t;

static inline gml_ht_entry_t *gml_ht_entry_create(const void *key, void *value) {
    gml_ht_entry_t *entry = malloc(sizeof(*entry));
    if (!entry)
        return NULL;

    entry->key   = strdup(key);
    entry->value = value;
    return entry;
}

static inline bool gml_ht_entry_compare(const void *a, const void *b) {
    return !strcmp(((gml_ht_entry_t*)a)->key, (const char *)b);
}

static inline gml_ht_entry_t *gml_ht_entry_find(gml_ht_t *hashtable, const char *key, size_t *index) {
    *index = gml_env_hash(key) & (hashtable->size - 1);
    return list_search(hashtable->table[*index], &gml_ht_entry_compare, key);
}

static gml_ht_t *gml_ht_create(size_t size) {
    gml_ht_t *hashtable = malloc(sizeof(*hashtable));
    if (!hashtable)
        return NULL;

    hashtable->size  = size;
    if (!(hashtable->table = malloc(sizeof(list_t*) * size))) {
        free(hashtable);
        return NULL;
    }
    for (size_t i = 0; i < hashtable->size; i++)
        hashtable->table[i] = list_create();
    return hashtable;
}

static void gml_ht_destroy(gml_ht_t *hashtable) {
    for (size_t i = 0; i < hashtable->size; i++) {
        gml_ht_entry_t *entry;
        while ((entry = list_pop(hashtable->table[i]))) {
            free(entry->key);
            free(entry);
        }
        list_destroy(hashtable->table[i]);
    }
    free(hashtable->table);
    free(hashtable);
}

static void gml_ht_insert(gml_ht_t *hashtable, const char *key, void *value) {
    size_t hash = gml_env_hash(key) & (hashtable->size - 1);
    list_push(hashtable->table[hash], gml_ht_entry_create(key, value));
}

static void *gml_ht_find(gml_ht_t *hashtable, const char *key) {
    gml_ht_entry_t *find = gml_ht_entry_find(hashtable, key, &(size_t){0});
    return find ? find->value : NULL;
}

struct gml_state_s {
    void      *user;
    gml_env_t *global;
    gml_ht_t  *atoms;
    parse_t   *parse;
    list_t    *asts;
    list_t    *objects;
    list_t    *classes;
    jmp_buf    escape;
    size_t     lambdaindex;
};

static void gml_abort(gml_state_t *gml) {
    longjmp(gml->escape, 1);
}

gml_type_t gml_arg_contract(char c) {
    switch (c) {
        case 'n': return GML_TYPE_NUMBER;
        case 's': return GML_TYPE_STRING;
        case 'a': return GML_TYPE_ARRAY;
        case 't': return GML_TYPE_TABLE;
        case 'f': return GML_TYPE_FUNCTION;
        case ':': return GML_TYPE_ATOM;
        default:
            return (gml_type_t)-1;
    }
}

void gml_arg_check(gml_state_t *gml, gml_value_t *args, size_t nargs, const char *name, const char *contract) {
    size_t count = strlen(contract);
    if (nargs != count) {
        if (count == 1) {
            if (nargs == 0) {
                gml_throw(false, "function `%s' expects an argument, got none", name);
            } else {
                gml_throw(false, "function `%s' expects an argument, got %zu", name, nargs);
            }
        } else if (count == 0) {
            gml_throw(false, "function `%s' expects no arguments, got %zu", name, nargs);
        } else {
            gml_throw(false, "function `%s' expects %zu arguments, got %zu", name, count, nargs);
        }
        gml_abort(gml);
    }
    for (size_t i = 0; i < nargs; i++) {
        gml_type_t type = gml_arg_contract(contract[i]);
        if (gml_value_typeof(gml, args[i]) != type) {
            gml_throw(false, "incompatible type `%s' in passing argument `%zu' of `%s', expected type `%s'",
                gml_typename(gml, gml_value_typeof(gml, args[i])),
                i + 1,
                name,
                gml_typename(gml, type)
            );
            gml_abort(gml);
        }
    }
}

/* State runtime */
gml_state_t *gml_state_create(void) {
    gml_state_t *state = malloc(sizeof(*state));
    if (!state)
        return NULL;

    state->global      = gml_env_create();
    state->atoms       = gml_ht_create(32);
    state->parse       = NULL;
    state->asts        = list_create();
    state->objects     = list_create();
    state->classes     = list_create();
    state->lambdaindex = 0;
    return state;
}

void gml_state_destroy(gml_state_t *state) {
    /* Destroy anything not already handled by the GC */
    list_iterator_t *it = list_iterator_create(state->objects);
    while (!list_iterator_end(it)) {
        gml_header_t *head = list_iterator_next(it);
        head->destroy(state, gml_value_box(state, head));
    }
    list_iterator_destroy(it);
    gml_env_destroy(state->global);
    gml_ht_destroy(state->atoms);
    list_destroy(state->objects);
    it = list_iterator_create(state->asts);
    while (!list_iterator_end(it))
        ast_destroy(list_iterator_next(it));
    list_iterator_destroy(it);
    list_destroy(state->asts);
    list_destroy(state->classes);
    if (state->parse)
        parse_destroy(state->parse);
    free(state);
}

void gml_state_user_set(gml_state_t *gml, void *user) {
    gml->user = user;
}

void *gml_state_user_get(gml_state_t *gml) {
    return gml->user;
}

/* NaN boxed value representation of types */
#define GML_VALUE_BOX_TAG  0x7FF8000000000000U
#define GML_VALUE_BOX_MASK 0xFFFF000000000000U

typedef union {
    uint64_t      u64;
    gml_value_t   val;
    gml_header_t *ptr;
} gml_value_box_t;

gml_value_t gml_value_box(gml_state_t *gml, gml_header_t *value) {
    (void)gml;
    gml_value_box_t box = { .ptr = value };
    box.u64 |= GML_VALUE_BOX_TAG;
    return box.val;
}

gml_header_t *gml_value_unbox(gml_state_t *gml, gml_value_t value) {
    (void)gml;
    gml_value_box_t box = { .val = value };
    box.u64 &= ~GML_VALUE_BOX_MASK;
    return box.ptr;
}

gml_type_t gml_value_typeof(gml_state_t *gml, gml_value_t value) {
    (void)gml;
    gml_value_box_t box = { .val = value };
    if ((box.u64 & GML_VALUE_BOX_MASK) == GML_VALUE_BOX_TAG)
        return gml_value_unbox(gml, value)->type;
    return GML_TYPE_NUMBER;
}

/* Registration of globals and native functions */
void gml_set_global(gml_state_t *gml, const char *name, gml_value_t value) {
    gml_value_t *oldp;
    if (gml_env_lookup(gml->global, name, &oldp)) {
        if (gml_value_typeof(gml, *oldp) != GML_TYPE_NUMBER) {
            gml_header_t *head = gml_value_unbox(gml, *oldp);
            head->destroy(gml, *oldp);
            list_erase(gml->objects, head);
        }
        *oldp = value;
    } else {
        gml_env_bind(gml->global, name, value);
    }
}

void gml_set_native(gml_state_t *gml, const char *name, gml_native_func_t func, int min, int max) {
    gml_value_t value = gml_native_create(gml, func, min, max);
    gml_set_global(gml, name, value);
}

/* Runtime atom */
typedef struct {
    gml_header_t header;
    size_t       length;
    char        *key;
} gml_atom_t;

void gml_atom_destroy(gml_state_t *gml, gml_value_t value) {
    gml_atom_t *atom = (gml_atom_t*)gml_value_unbox(gml, value);
    free(atom->key);
    free(atom);
}

gml_value_t gml_atom_create(gml_state_t *gml, const char *key) {
    gml_atom_t *atom = gml_ht_find(gml->atoms, key);
    if (atom)
        return gml_value_box(gml, (gml_header_t*)atom);

    /* Create a new atom and insert it into our hashtable */
    const size_t length = strlen(key);
    if (!(atom = malloc(sizeof(*atom))))
        return gml_nil_create(gml);
    atom->header.type    = GML_TYPE_ATOM;
    atom->header.destroy = &gml_atom_destroy;
    atom->length         = length;
    atom->key            = strdup(key);
    gml_ht_insert(gml->atoms, key, atom);
    list_push(gml->objects, atom);
    return gml_value_box(gml, (gml_header_t*)atom);
}

size_t gml_atom_length(gml_state_t *gml, gml_value_t value) {
    return ((gml_atom_t*)gml_value_unbox(gml, value))->length;
}

const char *gml_atom_key(gml_state_t *gml, gml_value_t value) {
    return ((gml_atom_t*)gml_value_unbox(gml, value))->key;
}

/* Runtime array */
typedef struct {
    gml_header_t header;
    gml_value_t *elements;
    size_t       length;
    size_t       capacity;
} gml_array_t;

void gml_array_destroy(gml_state_t *gml, gml_value_t value) {
    gml_array_t *array = (gml_array_t*)gml_value_unbox(gml, value);
    free(array->elements);
    free(array);
}

gml_value_t gml_array_create(gml_state_t *gml, gml_value_t *elements, size_t length) {
    gml_array_t *array = malloc(sizeof(*array));
    if (!array)
        return gml_nil_create(gml);
    if (!(array->elements = malloc(sizeof(gml_value_t) * length))) {
        free(array);
        return gml_nil_create(gml);
    }
    array->header.type    = GML_TYPE_ARRAY;
    array->header.destroy = &gml_array_destroy;
    memcpy(array->elements, elements, sizeof(gml_value_t) * length);
    array->length   = length;
    array->capacity = length;
    list_push(gml->objects, array);
    return gml_value_box(gml, (gml_header_t*)array);
}

gml_value_t gml_array_create_cat(gml_state_t *gml, gml_value_t a1, gml_value_t a2) {
    gml_array_t *array1 = (gml_array_t*)gml_value_unbox(gml, a1);
    gml_array_t *array2 = (gml_array_t*)gml_value_unbox(gml, a2);
    gml_array_t *array  = malloc(sizeof(*array));
    if (!array)
        return gml_nil_create(gml);
    if (!(array->elements = malloc(sizeof(gml_value_t) * (array1->length + array2->length)))) {
        free(array);
        return gml_nil_create(gml);
    }
    array->header.type    = GML_TYPE_ARRAY;
    array->header.destroy = &gml_array_destroy;
    memcpy(array->elements, array1->elements, sizeof(gml_value_t) * array1->length);
    memcpy(&array->elements[array1->length], array2->elements, sizeof(gml_value_t) * array2->length);
    array->length   = array1->length + array2->length;
    array->capacity = array->length;
    list_push(gml->objects, array);
    return gml_value_box(gml, (gml_header_t*)array);
}

size_t gml_array_length(gml_state_t *gml, gml_value_t array) {
    return ((gml_array_t*)gml_value_unbox(gml, array))->length;
}

gml_value_t gml_array_get(gml_state_t *gml, gml_value_t array, size_t index) {
    return ((gml_array_t*)gml_value_unbox(gml, array))->elements[index];
}

void gml_array_set(gml_state_t *gml, gml_value_t array, size_t index, gml_value_t value) {
    ((gml_array_t*)gml_value_unbox(gml, array))->elements[index] = value;
}

/* Runtime function */
typedef struct {
    gml_header_t header;
    char        *name;
    list_t      *formals;
    list_t      *body;
    void        *env;
} gml_function_t;

void gml_function_destroy(gml_state_t *gml, gml_value_t value) {
    gml_function_t *function = (gml_function_t*)gml_value_unbox(gml, value);
    free(function->name);
    free(function);
}

gml_value_t gml_function_create(gml_state_t *gml, const char *name, list_t *formals, list_t *body, gml_env_t *env) {
    gml_function_t *fun = malloc(sizeof(*fun));
    if (!fun)
        return gml_nil_create(gml);

    fun->header.type    = GML_TYPE_FUNCTION;
    fun->header.destroy = &gml_function_destroy;
    fun->name           = strdup(name);
    fun->formals        = formals;
    fun->body           = body;
    fun->env            = env;

    list_push(gml->objects, fun);
    return gml_value_box(gml, (gml_header_t*)fun);
}

const char *gml_function_name(gml_state_t *gml, gml_value_t fun) {
    return ((gml_function_t*)gml_value_unbox(gml, fun))->name;
}

list_t *gml_function_formals(gml_state_t *gml, gml_value_t fun) {
    return ((gml_function_t*)gml_value_unbox(gml, fun))->formals;
}

list_t *gml_function_body(gml_state_t *gml, gml_value_t fun) {
    return ((gml_function_t*)gml_value_unbox(gml, fun))->body;
}

static gml_env_t *gml_function_env(gml_state_t *gml, gml_value_t fun) {
    return ((gml_function_t*)gml_value_unbox(gml, fun))->env;
}

/* Native FFI runtime */
typedef struct {
    gml_header_t      header;
    gml_native_func_t func;
    int               min;
    int               max;
} gml_native_t;

void gml_native_destroy(gml_state_t *gml, gml_value_t value) {
    free(gml_value_unbox(gml, value));
}

gml_value_t gml_native_create(gml_state_t *gml, gml_native_func_t func, int min, int max) {
    gml_native_t *native = malloc(sizeof(*native));
    if (!native)
        return gml_nil_create(gml);

    native->header.type    = GML_TYPE_NATIVE;
    native->header.destroy = &gml_native_destroy;
    native->func           = func;
    native->min            = min;
    native->max            = max;

    list_push(gml->objects, native);
    return gml_value_box(gml, (gml_header_t*)native);
}

gml_native_func_t gml_native_func(gml_state_t *gml, gml_value_t func) {
    return ((gml_native_t*)gml_value_unbox(gml, func))->func;
}

/* Runtime string */
static size_t gml_string_rune_size(gml_string_rune_t rune) {
    if      (rune >  0x10FFFF) return 0;
    else if (rune >= 0x10000)  return 4;
    else if (rune >= 0x800)    return 3;
    else if (rune >= 0x80)     return 2;
    return 1;
}

static uint8_t *gml_string_encode(gml_string_rune_t *runes, size_t nrunes, uint8_t *u) {
    for (size_t i = 0; i < nrunes; i++) {
        gml_string_rune_t rune = runes[i];
        unsigned n = gml_string_rune_size(rune);
        if (n == 0) {
            return NULL;
        } else if (n == 1) {
            *u++ = (uint8_t)rune;
            continue;
        }
        for (unsigned k = n; k > 1; k--) {
            *(u + k - 1) = 0x80 | (rune & 0x3F);
            rune >>= 6;
        }
        *u = ~(0xFF >> n) | ((uint8_t)rune & (0x7F >> n));
        u += n;
    }
    *u = 0;
    return u;
}

static size_t gml_string_encoded_length(gml_string_rune_t *runes, size_t nrunes) {
    size_t n = 0;
    for (; nrunes; nrunes--)
        n += gml_string_rune_size(*runes++);
    return n;
}

static gml_string_rune_t *gml_string_decode(const uint8_t *u, gml_string_rune_t *runes, size_t *nrunes) {
    gml_string_rune_t rune = 0;
    *nrunes = 0;
    while (*u) {
        if (*u >= 0xC0) {
            unsigned n = 0;
            while (*u & (0x80 >> n))
                n++;
            rune = *u++ & (0xFF >> n);
            for (; n > 1; n--) {
                if ((*u & 0xC0) != 0x80)
                    return NULL;
                rune = (rune << 6) | (*u++ & 0x3F);
            }
        } else {
            rune = (gml_string_rune_t)*u++;
        }
        runes[(*nrunes)++] = rune;
    }
    return runes;
}

typedef struct {
    gml_header_t       header;
    size_t             length;
    gml_string_rune_t *runes;
} gml_string_t;

void gml_string_destroy(gml_state_t *gml, gml_value_t value) {
    gml_string_t *string = (gml_string_t*)gml_value_unbox(gml, value);
    free(string->runes);
    free(string);
}

static gml_value_t gml_string_from_runes(gml_state_t *gml, gml_string_rune_t *runes, size_t nrunes) {
    gml_string_t *string = malloc(sizeof(*string));
    if (!string)
        return gml_nil_create(gml);

    string->header.type    = GML_TYPE_STRING;
    string->header.destroy = &gml_string_destroy;
    string->length         = nrunes;
    string->runes          = runes;

    list_push(gml->objects, string);
    return gml_value_box(gml, (gml_header_t*)string);
}

gml_value_t gml_string_create(gml_state_t *gml, const char *string) {
    size_t             length = strlen(string);
    gml_string_rune_t *runes  = malloc(sizeof(gml_string_rune_t) * length);
    size_t             nrunes = 0;
    if (!runes)
        return gml_nil_create(gml);
    if (!gml_string_decode((uint8_t*)string, runes, &nrunes)) {
        free(runes);
        return gml_nil_create(gml);
    }
    /* Allow empty strings */
    if (nrunes != 0) {
        if (!(runes = realloc(runes, nrunes * sizeof(gml_string_rune_t)))) {
            free(runes);
            return gml_nil_create(gml);
        }
    }
    return gml_string_from_runes(gml, runes, nrunes);
}

gml_value_t gml_string_create_cat(gml_state_t *gml, const char *str1, const char *str2) {
    size_t             length  = strlen(str1) + strlen(str2);
    gml_string_rune_t *runes   = malloc(sizeof(gml_string_rune_t) * length);
    size_t             nrunesa = 0;
    size_t             nrunesb = 0;
    if (!runes)
        return gml_nil_create(gml);
    if (!gml_string_decode((uint8_t*)str1, runes, &nrunesa)) {
        free(runes);
        return gml_nil_create(gml);
    }
    if (!gml_string_decode((uint8_t*)str2, &runes[nrunesa], &nrunesb)) {
        free(runes);
        return gml_nil_create(gml);
    }
    /* Allow empty string concatenation */
    if (nrunesa + nrunesb != 0) {
        if (!(runes = realloc(runes, (nrunesa + nrunesb) * sizeof(gml_string_rune_t)))) {
            free(runes);
            return gml_nil_create(gml);
        }
    }
    return gml_string_from_runes(gml, runes, (nrunesa + nrunesb));
}

gml_value_t gml_string_substring(gml_state_t *gml, gml_value_t string, size_t start, size_t length) {
    gml_string_t *source = (gml_string_t*)gml_value_unbox(gml, string);
    return gml_string_from_runes(gml, source->runes + start, length);
}

const gml_string_rune_t *gml_string_runes(gml_state_t *gml, gml_value_t string) {
    return ((gml_string_t*)gml_value_unbox(gml, string))->runes;
}

size_t gml_string_length(gml_state_t *gml, gml_value_t string) {
    return ((gml_string_t*)gml_value_unbox(gml, string))->length;
}

char *gml_string_utf8data(gml_state_t *gml, gml_value_t string) {
    gml_string_t *source = (gml_string_t*)gml_value_unbox(gml, string);
    size_t        length = gml_string_encoded_length(source->runes, source->length);
    char         *utf8   = malloc(length + 1);
    if (!utf8)
        return NULL;
    gml_string_encode(source->runes, source->length, (uint8_t*)utf8);
    return utf8;
}
size_t gml_string_utf8length(gml_state_t *gml, gml_value_t string) {
    gml_string_t *source = (gml_string_t*)gml_value_unbox(gml, string);
    size_t length = gml_string_encoded_length(source->runes, source->length);
    return length;
}

/* Runtime table */
typedef struct {
    gml_value_t key;
    gml_value_t value;
} gml_table_bucket_t;

typedef struct {
    gml_header_t        header;
    gml_table_bucket_t *buckets;
    size_t              size;
} gml_table_t;

static void gml_table_clear(gml_state_t *gml, gml_table_t *table) {
    gml_value_t nil = gml_nil_create(gml);
    for (size_t i = 0; i < table->size; i++) {
        table->buckets[i].key   = nil;
        table->buckets[i].value = nil;
    }
}

static size_t gml_table_probe(gml_table_t *table, uint32_t hash, size_t index) {
    return (hash + index) % table->size;
}

static uint32_t gml_table_hash(gml_state_t *gml, gml_value_t value) {
    const char *start;
    size_t      length;
    switch (gml_value_typeof(gml, value)) {
        case GML_TYPE_NUMBER:
            start  = (char *)&value;
            length = sizeof(value);
            break;
        case GML_TYPE_STRING:
            start  = (char *)gml_string_runes(gml, value);
            length = gml_string_length(gml, value) * sizeof(gml_string_rune_t);
            break;
        case GML_TYPE_ATOM:
            start  = gml_atom_key(gml, value);
            length = gml_atom_length(gml, value);
            break;
        default:
            gml_throw(true, "Tried to hash a non hashable value.");
            gml_abort(gml);
            break;
    }
    uint32_t hash = 7;
    for (size_t i = 0; i < length; i++)
        hash = (hash * 31) ^ *start++;
    return hash;
}

static void gml_table_rehash(gml_state_t *gml, gml_table_t *table);

void gml_table_put(gml_state_t *gml, gml_value_t dict, gml_value_t key, gml_value_t value) {
    gml_table_t *table = (gml_table_t*)gml_value_unbox(gml, dict);
    uint32_t     hash  = gml_table_hash(gml, key);
    gml_value_t  nil   = gml_nil_create(gml);

    for (size_t i = 0; i < table->size; i++) {
        size_t slot = gml_table_probe(table, hash, i);
        if (gml_equal(gml, table->buckets[slot].key, nil)
        ||  gml_equal(gml, table->buckets[slot].key, key)) {
            table->buckets[slot].key   = key;
            table->buckets[slot].value = value;
            return;
        }
    }
    gml_table_rehash(gml, table);
    gml_table_put(gml, dict, key, value);
}

gml_value_t gml_table_get(gml_state_t *gml, gml_value_t dict, gml_value_t key) {
    gml_table_t *table   = (gml_table_t*)gml_value_unbox(gml, dict);
    uint32_t     hash    = gml_table_hash(gml, key);
    gml_value_t  nil     = gml_nil_create(gml);
    gml_value_t  deleted = gml_atom_create(gml, "deleted");
    for (size_t i = 0; i < table->size; i++) {
        size_t slot = gml_table_probe(table, hash, i);
        if (gml_equal(gml, table->buckets[slot].key, nil)
        && !gml_equal(gml, table->buckets[slot].value, deleted))
            break;
        else if (gml_equal(gml, table->buckets[slot].key, key))
            return table->buckets[slot].value;
    }
    return nil;
}

int gml_table_empty(gml_state_t *gml, gml_value_t dict) {
    gml_table_t *table = (gml_table_t*)gml_value_unbox(gml, dict);
    gml_value_t  nil   = gml_nil_create(gml);
    for (size_t i = 0; i < table->size; i++)
        if (!gml_equal(gml, table->buckets[i].key, nil))
            return 0;
    return 1;
}

list_t *gml_table_keys(gml_state_t *gml, gml_value_t dict) {
    gml_table_t *table = (gml_table_t*)gml_value_unbox(gml, dict);
    list_t      *keys  = list_create();
    gml_value_t  nil   = gml_nil_create(gml);
    for (size_t i = 0; i < table->size; i++)
        if(!gml_equal(gml, table->buckets[i].key, nil))
            list_push(keys, &table->buckets[i].key);
    return keys;
}

static void gml_table_rehash(gml_state_t *gml, gml_table_t *table) {
    size_t              osize    = table->size;
    gml_table_bucket_t *obuckets = table->buckets;
    gml_value_t         nil      = gml_nil_create(gml);

    table->size = osize * 2;
    if (!(table->buckets = malloc(sizeof(gml_table_bucket_t) * table->size)))
        return;

    gml_table_clear(gml, table);
    for (size_t i = 0; i < osize; i++) {
        if (obuckets[i].key == nil)
            continue;
        gml_table_put(
            gml,
            gml_value_box(gml, (gml_header_t*)table),
            obuckets[i].key,
            obuckets[i].value
        );
    }
    free(obuckets);
}

void gml_table_destroy(gml_state_t *gml, gml_value_t value) {
    gml_table_t *table = (gml_table_t*)gml_value_unbox(gml, value);
    free(table->buckets);
    free(table);
}

gml_value_t gml_table_create(gml_state_t *gml) {
    gml_table_t *table = malloc(sizeof(*table));
    if (!table)
        gml_nil_create(gml);

    table->header.type    = GML_TYPE_TABLE;
    table->header.destroy = &gml_table_destroy;
    table->size           = 11;
    if (!(table->buckets = malloc(sizeof(gml_table_bucket_t) * table->size))) {
        free(table);
        return gml_nil_create(gml);
    }

    gml_table_clear(gml, table);
    list_push(gml->objects, table);
    return gml_value_box(gml, (gml_header_t*)table);
}

/* Runtime number (for consistency) */
gml_value_t gml_number_create(gml_state_t *gml, double value) {
    (void)gml;
    return value;
}

double gml_number_value(gml_state_t *gml, gml_value_t number) {
    (void)gml;
    return number;
}

/* Special atoms */
gml_value_t gml_nil_create(gml_state_t *gml) {
    return gml_atom_create(gml, "nil");
}

gml_value_t gml_none_create(gml_state_t *gml) {
    return gml_atom_create(gml, "none");
}

gml_value_t gml_true_create(gml_state_t *gml) {
    return gml_atom_create(gml, "true");
}

gml_value_t gml_false_create(gml_state_t *gml) {
    return gml_atom_create(gml, "false");
}

/* Comparision runtime */
int gml_isfalse(gml_state_t *gml, gml_value_t value) {
    gml_type_t type = gml_value_typeof(gml, value);
    switch (type) {
        case GML_TYPE_NUMBER:
            return gml_number_value(gml, value) == 0.0;
        case GML_TYPE_STRING:
            return gml_string_length(gml, value) == 0;
        case GML_TYPE_ATOM:
            return (!strcmp(gml_atom_key(gml, value), "false") ||
                    !strcmp(gml_atom_key(gml, value), "nil")   ||
                    !strcmp(gml_atom_key(gml, value), "none"));
        case GML_TYPE_ARRAY:
            return gml_array_length(gml, value) == 0;
        case GML_TYPE_TABLE:
            return gml_table_empty(gml, value);
        case GML_TYPE_NATIVE:
        case GML_TYPE_FUNCTION:
            return 0;
        default:
            gml_throw(true, "Invalid type `%s' for boolean comparision.", gml_typename(gml, type));
            gml_abort(gml);
            break;
    }
    return 0;
}

int gml_istrue(gml_state_t *gml, gml_value_t value) {
    return !gml_isfalse(gml, value);
}

int gml_istable(gml_state_t *gml, gml_value_t value) {
    gml_type_t type = gml_value_typeof(gml, value);
    switch (type) {
        case GML_TYPE_NUMBER:
        case GML_TYPE_STRING:
        case GML_TYPE_ATOM:
            return 1;
        default:
            gml_throw(true, "Invalid type `%s' for table key.", gml_typename(gml, type));
            gml_abort(gml);
            break;
    }
    return 0;
}

int gml_equal(gml_state_t *gml, gml_value_t v1, gml_value_t v2) {
    size_t length;
    if (gml_value_typeof(gml, v1) != gml_value_typeof(gml, v2))
        return 0;
    switch (gml_value_typeof(gml, v1)) {
        case GML_TYPE_NUMBER:
            return gml_number_value(gml, v1) == gml_number_value(gml, v2);
        case GML_TYPE_ATOM: /* interned compare */
            return gml_value_unbox(gml, v1) == gml_value_unbox(gml, v2);
        case GML_TYPE_STRING:
            length = gml_string_length(gml, v1);
            if (length != gml_string_length(gml, v2))
                return 0;
            return !memcmp(gml_string_runes(gml, v1),
                           gml_string_runes(gml, v2),
                           length * sizeof(gml_string_rune_t));
        case GML_TYPE_ARRAY:
            if (gml_array_length(gml, v1) != gml_array_length(gml, v2))
                return 0;
            length = gml_array_length(gml, v1);
            for (size_t i = 0; i < length; i++) {
                if (!gml_equal(gml, gml_array_get(gml, v1, i),
                                    gml_array_get(gml, v2, i))) {
                    return 0;
                }
            }
            return 1;
        default:
            return 0;
    }
}

int gml_same(gml_state_t *gml, gml_value_t v1, gml_value_t v2) {
    if (gml_value_typeof(gml, v1) != gml_value_typeof(gml, v2))
        return 0;
    if (gml_value_typeof(gml, v1) == GML_TYPE_NUMBER)
        return gml_number_value(gml, v1) == gml_number_value(gml, v2);
    return gml_value_unbox(gml, v1) == gml_value_unbox(gml, v2);
}

/*
 * The evaluator. We should consider compiling the AST to a bytecode
 * instead of walking the AST to evaluate expressions.
 */
static gml_value_t gml_eval(gml_state_t *gml, ast_t *expr, gml_env_t *env);
static gml_value_t gml_eval_block(gml_state_t *gml, list_t *block, gml_env_t *env) {
    list_iterator_t *it = list_iterator_create(block);
    gml_value_t value = gml_nil_create(gml);
    while (!list_iterator_end(it)) {
        ast_t *next = list_iterator_next(it);
        value = gml_eval(gml, next, env);
    }
    list_iterator_destroy(it);
    return value;
}

static gml_value_t gml_eval_ident(gml_state_t *gml, ast_t *expr, gml_env_t *env) {
    gml_value_t *ptr = NULL;
    if (!gml_env_lookup(env, expr->ident, &ptr)) {
        gml_error(&expr->position, "`%s' is unbound.", expr->ident);
        gml_abort(gml);
    }
    return *ptr;
}

static gml_value_t gml_eval_call(gml_state_t *gml, ast_t *expr, gml_env_t *env) {
    gml_value_t callee   = gml_eval(gml, expr->call.callee, env);
    size_t      nargs    = list_length(expr->call.args);
    gml_type_t  calltype = gml_value_typeof(gml, callee);
    list_t     *formals;
    gml_value_t result;
    gml_value_t *lookup = NULL;
    int          method = 0;

    switch (calltype) {
        gml_env_t       *callenv;
        gml_value_t     *actuals;
        case GML_TYPE_FUNCTION:
            callenv = gml_env_push((gml_env_t*)gml_function_env(gml, callee));
            formals = gml_function_formals(gml, callee);
            /*
             * If the function contains a binding of "self" already it means the
             * function was promoted to a method for a table which was promoted to
             * a class.
             *
             * We need to start from formals[1] instead.
             */
            if (gml_env_lookup(callenv, "self", &lookup))
                method = 1;
            for (size_t i = 0; i < nargs; i++) {
                gml_value_t value = gml_eval(gml, list_at(expr->call.args, i), env);
                gml_env_bind(callenv, list_at(formals, i + method), value);
            }
            return gml_eval_block(gml, gml_function_body(gml, callee), callenv);

        case GML_TYPE_NATIVE:
            if (!(actuals = malloc(nargs * sizeof(gml_value_t))))
                return gml_nil_create(gml);
            for (size_t i = 0; i < nargs; i++)
                actuals[i] = gml_eval(gml, list_at(expr->call.args, i), env);
            result = gml_native_func(gml, callee)(gml, actuals, nargs);
            free(actuals);
            return result;

        default:
            gml_error(
                &expr->position,
                "Type `%s' is not a callable type.", gml_typename(gml, calltype)
            );
            gml_abort(gml);
            break;
    }
    return gml_nil_create(gml);
}

static gml_value_t gml_eval_array(gml_state_t *gml, ast_t *expr, gml_env_t *env) {
    size_t       length   = list_length(expr->array);
    gml_value_t *elements = malloc(sizeof(gml_value_t) * length);
    if (!elements)
        return gml_nil_create(gml);
    for (size_t i = 0; i < length; i++)
        elements[i] = gml_eval(gml, list_at(expr->array, i), env);
    gml_value_t value = gml_array_create(gml, elements, length);
    free(elements);
    return value;
}

static gml_value_t gml_eval_table(gml_state_t *gml, ast_t *expr, gml_env_t *env) {
    size_t      length  = list_length(expr->table);
    gml_value_t table   = gml_table_create(gml);
    int         isclass = 0;
    for (size_t i = 0; i < length; i++) {
        ast_t      *entry = list_at(expr->table, i);
        gml_value_t key   = gml_eval(gml, entry->dictentry.key,  env);
        gml_value_t value = gml_eval(gml, entry->dictentry.expr, env);

        /*
         * If there is a function which contains a `self' as the first
         * formal, then we mark the table as being a class one.
         */
        if (isclass == 0 && gml_value_typeof(gml, value) == GML_TYPE_FUNCTION) {
            isclass = 1;
            list_push(gml->classes, gml_value_unbox(gml, table));
        }

        gml_table_put(gml, table, key, value);
    }
    return table;
}

static gml_value_t gml_eval_assign_variable(gml_state_t *gml, ast_t *expr, gml_env_t *env) {
    gml_value_t value = gml_eval(gml, expr->binary.right, env);
    gml_value_t *old;
    if (gml_env_lookup(env, expr->binary.left->ident, &old)) {
        if (gml_value_typeof(gml, *old) != GML_TYPE_NUMBER) {
            gml_header_t *head = gml_value_unbox(gml, *old);
            head->destroy(gml, *old);
            list_erase(gml->objects, head);
        }
        *old = value;
    } else {
        gml_env_bind(env, expr->binary.left->ident, value);
    }
    return value;
}

static gml_value_t gml_eval_assign_array(gml_state_t *gml, gml_value_t array, gml_value_t index, ast_t *expr, gml_env_t *env) {
    gml_value_t value = gml_eval(gml, expr, env);
    gml_type_t  type  = gml_value_typeof(gml, index);
    if (type != GML_TYPE_NUMBER) {
        gml_error(
            &expr->position,
            "invalid array subscript: Expected type `number', got type `%s'.",
            gml_typename(gml, type)
        );
        gml_abort(gml);
    }
    gml_array_set(gml, array, (size_t)gml_number_value(gml, index), value);
    return value;
}

static gml_value_t gml_eval_assign_table(gml_state_t *gml, gml_value_t table, gml_value_t key, ast_t *expr, gml_env_t *env) {
    gml_value_t value = gml_eval(gml, expr, env);
    if (!gml_istable(gml, key)) {
        gml_throw(true, "Table is not a hashtable.");
        gml_abort(gml);
    }
    /*
     * Deal with binding the `self' formal for the function if assigning
     * a method function to the table.
     */
    if (gml_value_typeof(gml, value) == GML_TYPE_FUNCTION) {
        gml_function_t *fun = (gml_function_t*)gml_value_unbox(gml, value);
        if (!strcmp(list_at(fun->formals, 0), "self")) {
            /*
             * If the table hasn't already been promoted to a class we'll
             * promote it now.
             */
            gml_table_t *unbox = (gml_table_t*)gml_value_unbox(gml, table);
            if (!list_find(gml->classes, unbox))
                list_push(gml->classes, unbox);

            gml_env_bind(fun->env, "self", table);
        }
    }
    gml_table_put(gml, table, key, value);
    return value;
}

static gml_value_t gml_eval_assign(gml_state_t *gml, ast_t *expr, gml_env_t *env) {
    if (expr->binary.left->class == AST_IDENT)
        return gml_eval_assign_variable(gml, expr, env);
    if (expr->binary.left->class != AST_SUBSCRIPT) {
        goto expect_lvalue;
    }
    gml_value_t target = gml_eval(gml, expr->binary.left->subscript.expr, env);
    gml_value_t key    = gml_eval(gml, expr->binary.left->subscript.key,  env);

    switch (gml_value_typeof(gml, target)) {
        case GML_TYPE_ARRAY: return gml_eval_assign_array(gml, target, key, expr->binary.right, env);
        case GML_TYPE_TABLE: return gml_eval_assign_table(gml, target, key, expr->binary.right, env);
        default:
        expect_lvalue:
            gml_error(
                &expr->position,
                "Assignment target is not a valid lvalue."
            );
            gml_abort(gml);
            break;
    }
    return gml_nil_create(gml);
}

static void gml_typecheck(gml_state_t *gml, gml_position_t position, gml_value_t value, gml_type_t type) {
    gml_type_t actual = gml_value_typeof(gml, value);
    if (actual != type) {
        gml_error(&position, "Expected type `%s' but expression has type `%s'.",
            gml_typename(gml, type),
            gml_typename(gml, actual));
        gml_abort(gml);
    }
}

static gml_value_t gml_eval_binary(gml_state_t *gml, ast_t *expr, gml_env_t *env) {
    if (expr->binary.op == LEX_TOKEN_ASSIGN)
        return gml_eval_assign(gml, expr, env);

    gml_value_t vleft  = gml_eval(gml, expr->binary.left,  env);
    gml_value_t vright = gml_eval(gml, expr->binary.right, env);
    gml_value_t vtrue  = gml_true_create(gml);
    gml_value_t vfalse = gml_false_create(gml);
    double      nleft;
    double      nright;

    switch (expr->binary.op) {
        case LEX_TOKEN_PLUS:
        case LEX_TOKEN_MINUS:
        case LEX_TOKEN_MUL:
        case LEX_TOKEN_DIV:
        case LEX_TOKEN_MOD:
        case LEX_TOKEN_LESS:
        case LEX_TOKEN_GREATER:
        case LEX_TOKEN_LEQUAL:
        case LEX_TOKEN_GEQUAL:
        case LEX_TOKEN_BITAND:
        case LEX_TOKEN_BITOR:
        case LEX_TOKEN_BITLSHIFT:
        case LEX_TOKEN_BITRSHIFT:
        case LEX_TOKEN_BITXOR:
            gml_typecheck(gml, expr->position, vleft, gml_value_typeof(gml, vright));

            /* String concatenation */
            if (gml_value_typeof(gml, vright) == GML_TYPE_STRING && expr->binary.op == LEX_TOKEN_PLUS) {
                const char *lhs = gml_string_utf8data(gml, vleft);
                const char *rhs = gml_string_utf8data(gml, vright);
                return gml_string_create_cat(gml, lhs, rhs);
            }

            /* Array concatenation */
            if (gml_value_typeof(gml, vright) == GML_TYPE_ARRAY && expr->binary.op == LEX_TOKEN_PLUS)
                return gml_array_create_cat(gml, vleft, vright);

            nleft  = gml_number_create(gml, vleft);
            nright = gml_number_create(gml, vright);

            switch (expr->binary.op) {
                case LEX_TOKEN_PLUS:      return gml_number_create(gml, nleft + nright);
                case LEX_TOKEN_MINUS:     return gml_number_create(gml, nleft - nright);
                case LEX_TOKEN_MUL:       return gml_number_create(gml, nleft * nright);
                case LEX_TOKEN_DIV:       return gml_number_create(gml, nleft / nright);
                case LEX_TOKEN_MOD:       return gml_number_create(gml, (uint32_t)nleft %  (uint32_t)nright);
                case LEX_TOKEN_BITAND:    return gml_number_create(gml, (uint32_t)nleft &  (uint32_t)nright);
                case LEX_TOKEN_BITOR:     return gml_number_create(gml, (uint32_t)nleft |  (uint32_t)nright);
                case LEX_TOKEN_BITXOR:    return gml_number_create(gml, (uint32_t)nleft ^  (uint32_t)nright);
                case LEX_TOKEN_BITLSHIFT: return gml_number_create(gml, (uint32_t)nleft << (uint32_t)nright);
                case LEX_TOKEN_BITRSHIFT: return gml_number_create(gml, (uint32_t)nleft >> (uint32_t)nright);
                case LEX_TOKEN_LESS:      return nleft <  nright ? vtrue : vfalse;
                case LEX_TOKEN_GREATER:   return nleft >  nright ? vtrue : vfalse;
                case LEX_TOKEN_LEQUAL:    return nleft <= nright ? vtrue : vfalse;
                case LEX_TOKEN_GEQUAL:    return nleft >= nright ? vtrue : vfalse;
                default:
                    gml_throw(true, "operation %s is not a binary operation",
                        lex_token_classname(expr->binary.op));
                    break;
            }
            break;

        case LEX_TOKEN_IS:     return gml_same(gml, vleft, vright) ? vtrue : vfalse;
        case LEX_TOKEN_EQUAL:  return gml_equal(gml, vleft, vright) ? vtrue : vfalse;
        case LEX_TOKEN_NEQUAL: return gml_equal(gml, vleft, vright) ? vfalse : vtrue;
        case LEX_TOKEN_AND:    return gml_istrue(gml, vleft) ? vright : vleft;
        case LEX_TOKEN_OR:     return gml_isfalse(gml, vleft) ? vright : vleft;

        default:
            gml_throw(true, "operation %s is not a binary operation",
                lex_token_classname(expr->binary.op));
            break;
    }
    return gml_nil_create(gml);
}

static gml_value_t gml_eval_unary(gml_state_t *gml, ast_t *expr, gml_env_t *env) {
    gml_value_t value;
    gml_type_t  type;

    switch (expr->unary.op) {
        case LEX_TOKEN_NOT:
            if (gml_istrue(gml, gml_eval(gml, expr->unary.expr, env)))
                return gml_false_create(gml);
            return gml_true_create(gml);
        case LEX_TOKEN_PLUS:
            return gml_eval(gml, expr->unary.expr, env);
        case LEX_TOKEN_MINUS:
            value = gml_eval(gml, expr->unary.expr, env);
            type  = gml_value_typeof(gml, value);
            if (type != GML_TYPE_NUMBER) {
                gml_error(&expr->position, "invalid type `%s' in unary expression `%s'",
                    gml_typename(gml, type),
                    lex_token_classname(expr->unary.op));
                gml_abort(gml);
            }
            value = -value;
            return value;
        case LEX_TOKEN_BITNOT:
            value = gml_eval(gml, expr->unary.expr, env);
            type  = gml_value_typeof(gml, value);
            if (type != GML_TYPE_NUMBER) {
                gml_error(&expr->position, "invalid type `%s' in unary expression `%s'",
                    gml_typename(gml, type),
                    lex_token_classname(expr->unary.op));
                gml_abort(gml);
            }
            value = ~(uint32_t)value;
            return value;
        default:
            gml_throw(true, "operation %s is not a unary operation",
                lex_token_classname(expr->binary.op));
            break;
    }
    return gml_nil_create(gml);
}

static void gml_eval_subscript_check(gml_state_t *gml, gml_position_t *position, gml_value_t key, gml_value_t value) {
    gml_type_t keytype  = gml_value_typeof(gml, key);
    gml_type_t exprtype = gml_value_typeof(gml, value);
    if (keytype != GML_TYPE_NUMBER) {
        gml_error(
            position,
            "invalid type in subscript, expected type `number', got type `%s'.",
            gml_typename(gml, keytype)
        );
        gml_abort(gml);
    }

    int index  = (int)gml_number_value(gml, key);
    int length = (exprtype == GML_TYPE_STRING)
                    ? (int)gml_string_length(gml, value)
                    : (int)gml_array_length(gml, value);

    if (index < 0 || index >= length) {
        gml_error(
            position,
            "subscripting index out of bounds (index=%d, length=%d).",
            (int)index,
            (int)length
        );
        gml_abort(gml);
    }
}

static gml_value_t gml_eval_subscript(gml_state_t *gml, ast_t *subexpr, gml_env_t *env) {
    gml_value_t expr     = gml_eval(gml, subexpr->subscript.expr, env);
    gml_value_t key      = gml_eval(gml, subexpr->subscript.key,  env);
    gml_type_t  exprtype = gml_value_typeof(gml, expr);
    gml_value_t value;
    switch (exprtype) {
        case GML_TYPE_ARRAY:
            gml_eval_subscript_check(gml, &subexpr->position, key, expr);
            return gml_array_get(gml, expr, (size_t)gml_number_value(gml, key));
        case GML_TYPE_TABLE:
            value = gml_table_get(gml, expr, key);
            if (list_find(gml->classes, (const void *)gml_value_unbox(gml, expr))) {
                /*
                 * If the table is a class, we need to do a quick check to
                 * evaluate that the subscript yeilds a method that passes
                 * self.
                 */
                if (gml_value_typeof(gml, value) == GML_TYPE_FUNCTION) {
                    /*
                     * If the first formal of the function is `self' then
                     * this is a method and we need to bind the table to
                     * the functions environment as `self'.
                     */
                    gml_function_t *fun = (gml_function_t*)gml_value_unbox(gml, value);
                    if (!strcmp(list_at(fun->formals, 0), "self"))
                        gml_env_bind(fun->env, "self", expr);
                }
            }
            return value;
        case GML_TYPE_STRING:
            gml_eval_subscript_check(gml, &subexpr->position, key, expr);
            return gml_string_substring(gml, expr, (size_t)gml_number_value(gml, key), 1);
        default:
            gml_error(
                &subexpr->position,
                "Subscripting on unsupported type `%s' `%s'.",
                gml_typename(gml, exprtype),
                gml_typename(gml, gml_value_typeof(gml, key))
            );
            gml_abort(gml);
            break;
    }
    return gml_nil_create(gml);
}

static gml_value_t gml_eval_lambda(gml_state_t *gml, ast_t *expr, gml_env_t *env) {
    char name[1024];
    snprintf(name, sizeof(name), "#lambda(%zu)", gml->lambdaindex++);
    return gml_function_create(gml,
                               name,
                               expr->lambda.formals,
                               expr->lambda.body,
                               env);
}

static gml_value_t gml_eval_declfun(gml_state_t *gml, ast_t *expr, gml_env_t *env) {
    gml_value_t value = gml_function_create(gml,
                            expr->fundecl.name,
                            expr->fundecl.impl.formals,
                            expr->fundecl.impl.body,
                            env);
    gml_env_bind(env, expr->fundecl.name, value);
    return value;
}

static gml_value_t gml_eval_declvar(gml_state_t *gml, ast_t *expr, gml_env_t *env) {
    const char *name = expr->vardecl.name;
    gml_value_t value = (expr->vardecl.initializer)
                            ? gml_eval(gml, expr->vardecl.initializer, env)
                            : gml_nil_create(gml);
    gml_env_bind(env, name, value);
    return value;
}

static gml_value_t gml_eval_if(gml_state_t *gml, ast_t *expr, gml_env_t *env) {
    size_t nclauses = list_length(expr->ifstmt);
    for (size_t i = 0; i < nclauses; i++) {
        ast_t *clause = list_at(expr->ifstmt, i);
        if (!clause->ifclause.condition || gml_istrue(gml, gml_eval(gml, clause->ifclause.condition, env)))
            return gml_eval_block(gml, clause->ifclause.body, env);
    }
    return gml_nil_create(gml);
}

static gml_value_t gml_eval_while(gml_state_t *gml, ast_t *expr, gml_env_t *env) {
    gml_value_t result = gml_nil_create(gml);
    while (gml_istrue(gml, gml_eval(gml, expr->whilestmt.condition, env)))
        result = gml_eval_block(gml, expr->whilestmt.body, env);
    return result;
}

static gml_value_t gml_eval_for_type(gml_state_t *gml, list_t *formals, list_t *body, gml_value_t value, gml_env_t *env) {
    size_t      i;
    size_t      nformals  = list_length(formals);
    size_t      nelements = 0;
    gml_value_t result;
    list_t     *keys;

    switch (gml_value_typeof(gml, value)) {
        case GML_TYPE_ARRAY:
            nelements = gml_array_length(gml, value);
            for (i = 0; i < nelements; i += nformals) {
                for (size_t j = 0; j < nformals && i + j < nelements; j++) {
                    const char *name = list_at(formals, j);
                    gml_env_bind(env, name, gml_array_get(gml, value, i + j));
                }
                result = gml_eval_block(gml, body, env);
            }
            return result;

        case GML_TYPE_STRING:
            nelements = gml_string_length(gml, value);
            for (i = 0; i < nelements; i += nformals) {
                for (size_t j = 0; j < nformals && i + j < nelements; j++) {
                    const char *name = list_at(formals, j);
                    gml_env_bind(env, name, gml_string_substring(gml, value, i + j, 1));
                }
                result = gml_eval_block(gml, body, env);
            }
            return result;

        case GML_TYPE_TABLE:
            keys      = gml_table_keys(gml, value);
            nelements = list_length(keys);
            for (i = 0; i < nelements; i += nformals) {
                for (size_t j = 0; j < nformals && i + j < nelements; j++) {
                    const char  *name = list_at(formals, j);
                    gml_value_t  key  = *(gml_value_t*)list_at(keys, i + j);
                    gml_value_t  val  = gml_table_get(gml, value, key);
                    gml_value_t  pair = gml_array_create(gml, (gml_value_t[]) { key, val }, 2);
                    gml_env_bind(env, name, pair);
                }
                result = gml_eval_block(gml, body, env);
            }
            return result;

        case GML_TYPE_FUNCTION:
            break;

        default:
            break;
    }

    return gml_nil_create(gml);
}

static gml_value_t gml_eval_for(gml_state_t *gml, ast_t *ast, gml_env_t *env) {
    gml_value_t value   = gml_nil_create(gml);
    list_t     *formals = ast->forstmt.impl.formals;
    list_t     *body    = ast->forstmt.impl.body;
    ast_t      *expr    = ast->forstmt.expr;

    switch (expr->class) {
        case AST_ARRAY:
            value = gml_eval_array(gml, expr, env);
            return gml_eval_for_type(gml, formals, body, value, env);
        case AST_STRING:
            value = gml_string_create(gml, expr->string);
            return gml_eval_for_type(gml, formals, body, value, env);
        case AST_TABLE:
            value = gml_eval_table(gml, expr, env);
            return gml_eval_for_type(gml, formals, body, value, env);
        case AST_CALL:
            value = gml_eval_call(gml, expr, env);
            return gml_eval_for_type(gml, formals, body, value, env);
        default:
            break;
    }
    return gml_nil_create(gml);
}

static gml_value_t gml_eval_statement(gml_state_t *gml, ast_t *expr, gml_env_t *env) {
    switch (expr->class) {
        case AST_DECLFUN: return gml_eval_declfun(gml, expr, env);
        case AST_DECLVAR: return gml_eval_declvar(gml, expr, env);
        case AST_IF:      return gml_eval_if(gml, expr, env);
        case AST_WHILE:   return gml_eval_while(gml, expr, env);
        case AST_FOR:     return gml_eval_for(gml, expr, env);
        default:
            return gml_nil_create(gml);
    }
}

static gml_value_t gml_eval(gml_state_t *gml, ast_t *expr, gml_env_t *env) {
    switch (expr->class) {
        case AST_TOPLEVEL:  return gml_eval_block(gml, expr->toplevel, env);
        case AST_IDENT:     return gml_eval_ident(gml, expr, env);
        case AST_CALL:      return gml_eval_call(gml, expr, env);
        case AST_ATOM:      return gml_atom_create(gml, expr->atom);
        case AST_NUMBER:    return gml_number_create(gml, expr->number);
        case AST_STRING:    return gml_string_create(gml, expr->string);
        case AST_ARRAY:     return gml_eval_array(gml, expr, env);
        case AST_TABLE:     return gml_eval_table(gml, expr, env);
        case AST_BINARY:    return gml_eval_binary(gml, expr, env);
        case AST_UNARY:     return gml_eval_unary(gml, expr, env);
        case AST_SUBSCRIPT: return gml_eval_subscript(gml, expr, env);
        case AST_LAMBDA:    return gml_eval_lambda(gml, expr, env);
        case AST_DECLFUN:   return gml_eval_statement(gml, expr, env);
        case AST_DECLVAR:   return gml_eval_statement(gml, expr, env);
        case AST_IF:        return gml_eval_statement(gml, expr, env);
        case AST_WHILE:     return gml_eval_statement(gml, expr, env);
        case AST_FOR:       return gml_eval_statement(gml, expr, env);
        default:
            return gml_nil_create(gml);
    }
}

size_t gml_dump(gml_state_t *gml, gml_value_t value, char *buffer, size_t length) {
#   define space      ((length - offset) > 0 ? (length - offset) : 0)
#   define append(...) offset += snprintf(buffer + offset, space, __VA_ARGS__);

    size_t      offset = 0;
    size_t      nelems;
    size_t      nkeys;
    list_t     *keys;
    const char *atom;
    switch (gml_value_typeof(gml, value)) {
        case GML_TYPE_NUMBER:
            return snprintf(buffer, length, "%g", gml_number_value(gml, value));
        case GML_TYPE_STRING:
            return snprintf(buffer, length, "\"%s\"", gml_string_utf8data(gml, value));
        case GML_TYPE_ATOM:
            /* The none atom is nothingness. It's used to specify nothingness. */
            atom = gml_atom_key(gml, value);
            if (strcmp(atom, "none"))
                return snprintf(buffer, length, ":%s", gml_atom_key(gml, value));
            else
                return 0;
        case GML_TYPE_ARRAY:
            nelems = gml_array_length(gml, value);
            append("[");
            for (size_t i = 0; i < nelems; i++) {
                gml_value_t element = gml_array_get(gml, value, i);
                offset += gml_dump(gml, element, buffer + offset, space);
                if (i < nelems - 1)
                    append(", ");
            }
            append("]");
            return offset;
        case GML_TYPE_TABLE:
            keys  = gml_table_keys(gml, value);
            nkeys = list_length(keys);
            append("{");
            for (size_t i = 0; i < nkeys; i++) {
                gml_value_t key = *(gml_value_t*)list_at(keys, i);
                gml_value_t val = gml_table_get(gml, value, key);
                offset += gml_dump(gml, key, buffer + offset, space);
                append(" = ");
                offset += gml_dump(gml, val, buffer + offset, space);
                if (i < nkeys - 1)
                    append(", ");
            }
            list_destroy(keys);
            append("}");
            return offset;
        case GML_TYPE_NATIVE:
            return snprintf(buffer, length, "<native:%p>", gml_native_func(gml, value));
        case GML_TYPE_FUNCTION:
            return snprintf(buffer, length, "<function:%s>", gml_function_name(gml, value));
    }
    return offset;
}

static gml_value_t gml_runbuffer(gml_state_t *gml, const char *filename, const char *source) {
    if (gml->parse)
        parse_destroy(gml->parse);

    ast_t *ast;
    gml->parse = parse_create(filename, source);
    if (setjmp(gml->escape) == 0) {
        ast = parse_run(gml->parse);
        if (ast) {
            list_push(gml->asts, ast);
            return gml_eval(gml, ast, gml->global);
        }
    }

    return gml_nil_create(gml);
}

gml_value_t gml_run_string(gml_state_t *gml, const char *source) {
    return gml_runbuffer(gml, "<string>", source);
}

gml_value_t gml_run_file(gml_state_t *gml, const char *filename) {
    FILE *fp = fopen(filename, "rb");
    if (!fp)
        return gml_nil_create(gml);
    fseek(fp, 0, SEEK_END);
    size_t length = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    char *source = malloc(length + 1);
    if (!source) {
        fclose(fp);
        return gml_nil_create(gml);
    }
    source[length] = '\0';
    if (fread(source, 1, length, fp) != length) {
        fclose(fp);
        free(source);
        return gml_nil_create(gml);
    }
    gml_value_t value = gml_runbuffer(gml, filename, source);
    free(source);
    fclose(fp);
    return value;
}

gml_value_t gml_function_run(gml_state_t *gml, gml_value_t function, gml_value_t *args, size_t nargs) {
    gml_env_t *callenv = gml_env_push((gml_env_t*)gml_function_env(gml, function));
    for (size_t i = 0; i < nargs; i++)
        gml_env_bind(callenv, list_at(gml_function_formals(gml, function), i), args[i]);
    return gml_eval_block(gml, gml_function_body(gml, function), callenv);
}
