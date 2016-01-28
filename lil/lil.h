/*
 * LIL - Little Interpreted Language
 * Copyright (C) 2010-2013 Kostas Michalopoulos
 *
 * This software is provided 'as-is', without any express or implied
 * warranty.  In no event will the authors be held liable for any damages
 * arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not
 *    claim that you wrote the original software. If you use this software
 *    in a product, an acknowledgment in the product documentation would be
 *    appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 *
 * Kostas Michalopoulos <badsector@runtimelegend.com>
 */

#ifndef __LIL_H_INCLUDED__
#define __LIL_H_INCLUDED__

#define LIL_VERSION_STRING "0.1"

#define LIL_SETVAR_GLOBAL 0
#define LIL_SETVAR_LOCAL 1
#define LIL_SETVAR_LOCAL_NEW 2
#define LIL_SETVAR_LOCAL_ONLY 3

#define LIL_CALLBACK_EXIT 0
#define LIL_CALLBACK_WRITE 1
#define LIL_CALLBACK_READ 2
#define LIL_CALLBACK_STORE 3
#define LIL_CALLBACK_SOURCE 4
#define LIL_CALLBACK_ERROR 5
#define LIL_CALLBACK_SETVAR 6
#define LIL_CALLBACK_GETVAR 7

#define LIL_EMBED_NOFLAGS 0x0000

#if defined(LILDLL) && (defined(WIN32) || defined(_WIN32))
#ifdef __LIL_C_FILE__
#define LILAPI __declspec(dllexport __stdcall)
#else
#define LILAPI __declspec(dllimport __stdcall)
#endif
#define LILCALLBACK __declspec(__stdcall)
#else
#define LILAPI
#define LILCALLBACK
#endif

#ifdef LILINT_LONGLONG
typedef long long int lilint_t;
#define LILINT_PRINTF "%lli"
#else
#ifdef LILINT_INT64
typedef __int64 lilint_t;
#define LILINT_PRINTF "%I64i"
#else
#ifndef LILINT_CUSTOM
#include <stdint.h>
#include <inttypes.h>
typedef int64_t lilint_t;
#define LILINT_PRINTF "%"PRIi64
#endif
#endif
#endif

typedef struct _lil_value_t* lil_value_t;
typedef struct _lil_func_t* lil_func_t;
typedef struct _lil_var_t* lil_var_t;
typedef struct _lil_env_t* lil_env_t;
typedef struct _lil_list_t* lil_list_t;
typedef struct _lil_t* lil_t;
typedef LILCALLBACK lil_value_t (*lil_func_proc_t)(lil_t lil, size_t argc, lil_value_t* argv);
typedef LILCALLBACK void (*lil_exit_callback_proc_t)(lil_t lil, lil_value_t arg);
typedef LILCALLBACK void (*lil_write_callback_proc_t)(lil_t lil, const char* msg);
typedef LILCALLBACK char* (*lil_read_callback_proc_t)(lil_t lil, const char* name);
typedef LILCALLBACK char* (*lil_source_callback_proc_t)(lil_t lil, const char* name);
typedef LILCALLBACK void (*lil_store_callback_proc_t)(lil_t lil, const char* name, const char* data);
typedef LILCALLBACK void (*lil_error_callback_proc_t)(lil_t lil, size_t pos, const char* msg);
typedef LILCALLBACK int (*lil_setvar_callback_proc_t)(lil_t lil, const char* name, lil_value_t* value);
typedef LILCALLBACK int (*lil_getvar_callback_proc_t)(lil_t lil, const char* name, lil_value_t* value);
typedef LILCALLBACK void (*lil_callback_proc_t)(void);

LILAPI lil_t lil_new(void);
LILAPI void lil_free(lil_t lil);

LILAPI int lil_register(lil_t lil, const char* name, lil_func_proc_t proc);

LILAPI lil_value_t lil_parse(lil_t lil, const char* code, size_t codelen, int funclevel);
LILAPI lil_value_t lil_parse_value(lil_t lil, lil_value_t val, int funclevel);
LILAPI lil_value_t lil_call(lil_t lil, const char* funcname, size_t argc, lil_value_t* argv);

LILAPI void lil_callback(lil_t lil, int cb, lil_callback_proc_t proc);

LILAPI void lil_set_error(lil_t lil, const char* msg);
LILAPI void lil_set_error_at(lil_t lil, size_t pos, const char* msg);
LILAPI int lil_error(lil_t lil, const char** msg, size_t* pos);

LILAPI const char* lil_to_string(lil_value_t val);
LILAPI double lil_to_double(lil_value_t val);
LILAPI lilint_t lil_to_integer(lil_value_t val);
LILAPI int lil_to_boolean(lil_value_t val);

LILAPI lil_value_t lil_alloc_string(const char* str);
LILAPI lil_value_t lil_alloc_double(double num);
LILAPI lil_value_t lil_alloc_integer(lilint_t num);
LILAPI void lil_free_value(lil_value_t val);

LILAPI lil_value_t lil_clone_value(lil_value_t src);
LILAPI int lil_append_char(lil_value_t val, char ch);
LILAPI int lil_append_string(lil_value_t val, const char* s);
LILAPI int lil_append_val(lil_value_t val, lil_value_t v);

LILAPI lil_list_t lil_alloc_list(void);
LILAPI void lil_free_list(lil_list_t list);
LILAPI void lil_list_append(lil_list_t list, lil_value_t val);
LILAPI size_t lil_list_size(lil_list_t list);
LILAPI lil_value_t lil_list_get(lil_list_t list, size_t index);
LILAPI lil_value_t lil_list_to_value(lil_list_t list, int do_escape);

LILAPI lil_list_t lil_subst_to_list(lil_t lil, lil_value_t code);
LILAPI lil_value_t lil_subst_to_value(lil_t lil, lil_value_t code);

LILAPI lil_env_t lil_alloc_env(lil_env_t parent);
LILAPI void lil_free_env(lil_env_t env);
LILAPI lil_env_t lil_push_env(lil_t lil);
LILAPI void lil_pop_env(lil_t lil);

LILAPI lil_var_t lil_set_var(lil_t lil, const char* name, lil_value_t val, int local);
LILAPI lil_value_t lil_get_var(lil_t lil, const char* name);
LILAPI lil_value_t lil_get_var_or(lil_t lil, const char* name, lil_value_t defvalue);

LILAPI lil_value_t lil_eval_expr(lil_t lil, lil_value_t code);
LILAPI lil_value_t lil_unused_name(lil_t lil, const char* part);

LILAPI lil_value_t lil_arg(lil_value_t* argv, size_t index);

LILAPI void lil_set_data(lil_t lil, void* data);
LILAPI void* lil_get_data(lil_t lil);

LILAPI char* lil_embedded(lil_t lil, const char* code, unsigned int flags);
LILAPI void lil_freemem(void* ptr);

#endif
