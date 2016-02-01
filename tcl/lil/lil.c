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

#define __LIL_C_FILE__
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "lil.h"

/* Enable pools for reusing values, lists and environments. This will use more memory and
 * will rely on the runtime/OS to free the pools once the program ends, but will cause
 * considerably less memory fragmentation and improve the script execution performance. */
/*#define LIL_ENABLE_POOLS*/

/* Visual C++ does not have atoll */
#ifdef _MSC_VER
#define atoll _atoi64
/* disable warning about unsafe standard C calls */
#pragma warning(disable:4996)
/* disable float/int conversion warnings */
#pragma warning(disable:4244)
#endif

#define ERROR_NOERROR 0
#define ERROR_DEFAULT 1
#define ERROR_FIXHEAD 2

#define CALLBACKS 8
#define MAX_CATCHER_DEPTH 16384
#define HASHMAP_CELLS 256
#define HASHMAP_CELLMASK 0xFF

/* note: static lil_xxx functions might become public later */

struct hashentry_t
{
    char* k;
    void* v;
};

struct hashcell_t
{
    struct hashentry_t* e;
    size_t c;
};

typedef struct _hashmap_t
{
    struct hashcell_t cell[HASHMAP_CELLS];
} hashmap_t;

struct _lil_value_t
{
    size_t l;
#ifdef LIL_ENABLE_POOLS
    size_t c;
#endif
    char* d;
};

struct _lil_var_t
{
    char* n;
    struct _lil_env_t* env;
    lil_value_t v;
};

struct _lil_env_t
{
    struct _lil_env_t* parent;
    lil_func_t func;
    lil_value_t catcher_for;
    lil_var_t* var;
    size_t vars;
    hashmap_t varmap;
    lil_value_t retval;
    int retval_set;
    int breakrun;
};

struct _lil_list_t
{
    lil_value_t* v;
    size_t c;
    size_t cap;
};

struct _lil_func_t
{
    char* name;
    lil_value_t code;
    lil_list_t argnames;
    lil_func_proc_t proc;
};

struct _lil_t
{
    const char* code; /* need save on parse */
    const char* rootcode;
    size_t clen; /* need save on parse */
    size_t head; /* need save on parse */
    int ignoreeol;
    lil_func_t* cmd;
    size_t cmds;
    size_t syscmds;
    hashmap_t cmdmap;
    char* catcher;
    int in_catcher;
    char* dollarprefix;
    lil_env_t env;
    lil_env_t rootenv;
    lil_env_t downenv;
    lil_value_t empty;
    int error;
    size_t err_head;
    char* err_msg;
    lil_callback_proc_t callback[CALLBACKS];
    size_t parse_depth;
    void* data;
    char* embed;
    size_t embedlen;
};

typedef struct _expreval_t
{
    const char* code;
    size_t len, head;
    lilint_t ival;
    double dval;
    int type;
    int error;
} expreval_t;

static lil_value_t next_word(lil_t lil);
static void register_stdcmds(lil_t lil);

#ifdef LIL_ENABLE_POOLS
static lil_value_t* pool;
static int poolsize, poolcap;
static lil_list_t* listpool;
static size_t listpoolsize, listpoolcap;
static lil_env_t* envpool;
static size_t envpoolsize, envpoolcap;
#endif

static char* strclone(const char* s)
{
    size_t len = strlen(s) + 1;
    char* ns = malloc(len);
    if (!ns) return NULL;
    memcpy(ns, s, len);
    return ns;
}

static unsigned long hm_hash(const char* key)
{
    unsigned long hash = 5381;
    int c;
    while ((c = *key++)) hash = ((hash << 5) + hash) + c;
    return hash;
}

static void hm_init(hashmap_t* hm)
{
    memset(hm, 0, sizeof(hashmap_t));
}

static void hm_destroy(hashmap_t* hm)
{
    size_t i, j;
    for (i=0; i<HASHMAP_CELLS; i++) {
        for (j=0; j<hm->cell[i].c; j++)
            free(hm->cell[i].e[j].k);
        free(hm->cell[i].e);
    }
}

static void hm_put(hashmap_t* hm, const char* key, void* value)
{
    struct hashcell_t* cell = hm->cell + (hm_hash(key) & HASHMAP_CELLMASK);
    size_t i;
    for (i=0; i<cell->c; i++)
        if (!strcmp(key, cell->e[i].k)) {
            cell->e[i].v = value;
            return;
        }
    cell->e = realloc(cell->e, sizeof(struct hashentry_t)*(cell->c + 1));
    cell->e[cell->c].k = strclone(key);
    cell->e[cell->c].v = value;
    cell->c++;
}

static void* hm_get(hashmap_t* hm, const char* key)
{
    struct hashcell_t* cell = hm->cell + (hm_hash(key) & HASHMAP_CELLMASK);
    size_t i;
    for (i = 0; i<cell->c; i++)
        if (!strcmp(key, cell->e[i].k))
            return cell->e[i].v;
    return NULL;
}

static int hm_has(hashmap_t* hm, const char* key)
{
    struct hashcell_t* cell = hm->cell + (hm_hash(key) & HASHMAP_CELLMASK);
    size_t i;
    for (i = 0; i < cell->c; i++)
        if (!strcmp(key, cell->e[i].k))
            return 1;
    return 0;
}

#ifdef LIL_ENABLE_POOLS
static lil_value_t alloc_from_pool(void)
{
    if (poolsize > 0) {
        poolsize--;
        return pool[poolsize];
    } else {
        lil_value_t val = calloc(1, sizeof(struct _lil_value_t));
        return val;
    }
}

static void release_to_pool(lil_value_t val)
{
    if (poolsize == poolcap) {
        poolcap = poolcap ? (poolcap + poolcap / 2) : 64;
        pool = realloc(pool, sizeof(lil_value_t)*poolcap);
    }
    pool[poolsize++] = val;
}

static void ensure_capacity(lil_value_t val, size_t cap)
{
    if (val->c < cap) {
        val->c = cap + 128;
        val->d = realloc(val->d, val->c);
    }
}
#endif

static lil_value_t alloc_value_len(const char* str, size_t len)
{
#ifdef LIL_ENABLE_POOLS
    lil_value_t val = alloc_from_pool();
#else
    lil_value_t val = calloc(1, sizeof(struct _lil_value_t));
#endif
    if (!val) return NULL;
    if (str) {
        val->l = len;
#ifdef LIL_ENABLE_POOLS
        ensure_capacity(val, len + 1);
#else
        val->d = malloc(len + 1);
        if (!val->d) {
            free(val);
            return NULL;
        }
#endif
        memcpy(val->d, str, len);
        val->d[len] = 0;
    } else {
        val->l = 0;
#ifdef LIL_ENABLE_POOLS
        ensure_capacity(val, 1);
        val->d[0] = '\0';
#else
        val->d = NULL;
#endif
    }
    return val;
}

static lil_value_t alloc_value(const char* str)
{
    return alloc_value_len(str, str ? strlen(str) : 0);
}

lil_value_t lil_clone_value(lil_value_t src)
{
    lil_value_t val;
    if (!src) return NULL;
#ifdef LIL_ENABLE_POOLS
    val = alloc_from_pool();
#else
    val = calloc(1, sizeof(struct _lil_value_t));
#endif
    if (!val) return NULL;
    val->l = src->l;
    if (src->l) {
#ifdef LIL_ENABLE_POOLS
        ensure_capacity(val, val->l + 1);
#else
        val->d = malloc(val->l + 1);
        if (!val->d) {
            free(val);
            return NULL;
        }
#endif
        memcpy(val->d, src->d, val->l + 1);
    } else {
#ifdef LIL_ENABLE_POOLS
        ensure_capacity(val, 1);
        val->d[0] = '\0';
#else
        val->d = NULL;
#endif
    }
    return val;
}

int lil_append_char(lil_value_t val, char ch)
{
#ifdef LIL_ENABLE_POOLS
    ensure_capacity(val, val->l + 2);
    val->d[val->l++] = ch;
    val->d[val->l] = '\0';
#else
    char* new = realloc(val->d, val->l + 2);
    if (!new) return 0;
    new[val->l++] = ch;
    new[val->l] = 0;
    val->d = new;
#endif
    return 1;
}

int lil_append_string_len(lil_value_t val, const char* s, size_t len)
{
#ifndef LIL_ENABLE_POOLS
    char* new;
#endif
    if (!s || !s[0]) return 1;
#ifdef LIL_ENABLE_POOLS
    ensure_capacity(val, val->l + len + 1);
    memcpy(val->d + val->l, s, len + 1);
#else
    new = realloc(val->d, val->l + len + 1);
    if (!new) return 0;
    memcpy(new + val->l, s, len + 1);
    val->d = new;
#endif
    val->l += len;
    return 1;
}

int lil_append_string(lil_value_t val, const char* s)
{
    return lil_append_string_len(val, s, strlen(s));
}

int lil_append_val(lil_value_t val, lil_value_t v)
{
#ifndef LIL_ENABLE_POOLS
    char* new;
#endif
    if (!v || !v->l) return 1;
#ifdef LIL_ENABLE_POOLS
    ensure_capacity(val, val->l + v->l + 1);
    memcpy(val->d + val->l, v->d, v->l + 1);
#else
    new = realloc(val->d, val->l + v->l + 1);
    if (!new) return 0;
    memcpy(new + val->l, v->d, v->l + 1);
    val->d = new;
#endif
    val->l += v->l;
    return 1;
}

void lil_free_value(lil_value_t val)
{
    if (!val) return;
#ifdef LIL_ENABLE_POOLS
    release_to_pool(val);
#else
    free(val->d);
    free(val);
#endif
}

lil_list_t lil_alloc_list(void)
{
    lil_list_t list;
#ifdef LIL_ENABLE_POOLS
    if (listpoolsize > 0)
        return listpool[--listpoolsize];
#endif
    list = calloc(1, sizeof(struct _lil_list_t));
    list->v = NULL;
    return list;
}

void lil_free_list(lil_list_t list)
{
    size_t i;
    if (!list) return;
    for (i = 0; i<list->c; i++) lil_free_value(list->v[i]);
#ifdef LIL_ENABLE_POOLS
    list->c = 0;
    if (listpoolsize == listpoolcap) {
        listpoolcap = listpoolcap ? (listpoolcap + listpoolcap / 2) : 32;
        listpool = realloc(listpool, sizeof(lil_list_t)*listpoolcap);
    }
    listpool[listpoolsize++] = list;
#else
    free(list->v);
    free(list);
#endif
}

void lil_list_append(lil_list_t list, lil_value_t val)
{
    if (list->c == list->cap) {
        size_t cap = list->cap ? (list->cap + list->cap / 2) : 32;
        lil_value_t* nv = realloc(list->v, sizeof(lil_value_t)*cap);
        if (!nv) return;
        list->cap = cap;
        list->v = nv;
    }
    list->v[list->c++] = val;
}

size_t lil_list_size(lil_list_t list)
{
    return list->c;
}

lil_value_t lil_list_get(lil_list_t list, size_t index)
{
    return index >= list->c ? NULL : list->v[index];
}

static int needs_escape(const char* str)
{
    size_t i;
    if (!str || !str[0]) return 1;
    for (i=0; str[i]; i++)
        if (ispunct(str[i]) || isspace(str[i])) return 1;
    return 0;
}

lil_value_t lil_list_to_value(lil_list_t list, int do_escape)
{
    lil_value_t val = alloc_value(NULL);
    size_t i;
    for (i=0; i<list->c; i++) {
        int escape = do_escape ? needs_escape(lil_to_string(list->v[i])) : 0;
        if (i) lil_append_char(val, ' ');
        if (escape) lil_append_char(val, '{');
        lil_append_val(val, list->v[i]);
        if (escape) lil_append_char(val, '}');
    }
    return val;
}

lil_env_t lil_alloc_env(lil_env_t parent)
{
    lil_env_t env;
#ifdef LIL_ENABLE_POOLS
    if (envpoolsize > 0) {
        size_t i, j;
        env = envpool[--envpoolsize];
        env->parent = parent;
        env->func = NULL;
        env->catcher_for = NULL;
        env->var = NULL;
        env->vars = 0;
        env->retval = NULL;
        env->retval_set = 0;
        env->breakrun = 0;
        for (i = 0; i < HASHMAP_CELLS; i++) {
            for (j = 0; j < env->varmap.cell[i].c; j++)
                free(env->varmap.cell[i].e[j].k);
            env->varmap.cell[i].c = 0;
        }
        return env;
    }
#endif
    env = calloc(1, sizeof(struct _lil_env_t));
    env->parent = parent;
    return env;
}

void lil_free_env(lil_env_t env)
{
    size_t i;
    if (!env) return;
    lil_free_value(env->retval);
#ifdef LIL_ENABLE_POOLS
    for (i = 0; i<env->vars; i++) {
        free(env->var[i]->n);
        lil_free_value(env->var[i]->v);
        free(env->var[i]);
    }
    free(env->var);
    if (envpoolsize == envpoolcap) {
        envpoolcap = envpoolcap ? (envpoolcap + envpoolcap / 2) : 64;
        envpool = realloc(envpool, sizeof(lil_env_t)*envpoolcap);
    }
    envpool[envpoolsize++] = env;
#else
    hm_destroy(&env->varmap);
    for (i=0; i<env->vars; i++) {
        free(env->var[i]->n);
        lil_free_value(env->var[i]->v);
        free(env->var[i]);
    }
    free(env->var);
    free(env);
#endif
}

static lil_var_t lil_find_local_var(lil_t lil, lil_env_t env, const char* name)
{
    #if 0
    if (env->vars > 0) {
        size_t i = env->vars - 1;
        while (1) {
            if (!strcmp(env->var[i]->n, name)) return env->var[i];
            if (!i) break;
            i--;
        }
    }
    return NULL;
    #else
    return hm_get(&env->varmap, name);
    #endif
}

static lil_var_t lil_find_var(lil_t lil, lil_env_t env, const char* name)
{
    lil_var_t r = lil_find_local_var(lil, env, name);
    return r ? r : (env == lil->rootenv ? NULL : lil_find_var(lil, lil->rootenv, name));
}

static lil_func_t find_cmd(lil_t lil, const char* name)
{
    #if 0
    if (lil->cmds > 0) {
        size_t i = lil->cmds - 1;
        while (1) {
            if (!strcmp(lil->cmd[i]->name, name)) return lil->cmd[i];
            if (!i) break;
            i--;
        }
    }
    return NULL;
    #else
    return hm_get(&lil->cmdmap, name);
    #endif
}

static lil_func_t add_func(lil_t lil, const char* name)
{
    lil_func_t cmd;
    lil_func_t* ncmd;
    cmd = find_cmd(lil, name);
    if (cmd) return cmd;
    cmd = calloc(1, sizeof(struct _lil_func_t));
    cmd->name = strclone(name);
    ncmd = realloc(lil->cmd, sizeof(lil_func_t)*(lil->cmds + 1));
    if (!ncmd) {
        free(cmd);
        return NULL;
    }
    lil->cmd = ncmd;
    ncmd[lil->cmds++] = cmd;
    hm_put(&lil->cmdmap, name, cmd);
    return cmd;
}

int lil_register(lil_t lil, const char* name, lil_func_proc_t proc)
{
    lil_func_t cmd = add_func(lil, name);
    if (!cmd) return 0;
    cmd->proc = proc;
    return 1;
}

lil_var_t lil_set_var(lil_t lil, const char* name, lil_value_t val, int local)
{
    lil_var_t* nvar;
    lil_env_t env = local == LIL_SETVAR_GLOBAL ? lil->rootenv : lil->env;
    int freeval = 0;
    if (!name[0]) return NULL;
    if (local != LIL_SETVAR_LOCAL_NEW) {
        lil_var_t var = lil_find_var(lil, env, name);
        if (local == LIL_SETVAR_LOCAL_ONLY && var && var->env == lil->rootenv && var->env != env)
            var = NULL;
        if (((!var && env == lil->rootenv) || (var && var->env == lil->rootenv)) && lil->callback[LIL_CALLBACK_SETVAR]) {
            lil_setvar_callback_proc_t proc = (lil_setvar_callback_proc_t)lil->callback[LIL_CALLBACK_SETVAR];
            lil_value_t newval = val;
            int r = proc(lil, name, &newval);
            if (r < 0) return NULL;
            if (r) {
                val = newval;
                freeval = 1;
            }
        }
        if (var) {
            lil_free_value(var->v);
            var->v = freeval ? val : lil_clone_value(val);
            return var;
        }
    }

    nvar = realloc(env->var, sizeof(lil_var_t)*(env->vars + 1));
    if (!nvar) {
        /* TODO: report memory error */
        return NULL;
    }
    env->var = nvar;
    nvar[env->vars] = calloc(1, sizeof(struct _lil_var_t));
    nvar[env->vars]->n = strclone(name);
    nvar[env->vars]->env = env;
    nvar[env->vars]->v = freeval ? val : lil_clone_value(val);
    hm_put(&env->varmap, name, nvar[env->vars]);
    return nvar[env->vars++];
}

lil_value_t lil_get_var(lil_t lil, const char* name)
{
    return lil_get_var_or(lil, name, lil->empty);
}

lil_value_t lil_get_var_or(lil_t lil, const char* name, lil_value_t defvalue)
{
    lil_var_t var = lil_find_var(lil, lil->env, name);
    lil_value_t retval = var ? var->v : defvalue;
    if (lil->callback[LIL_CALLBACK_GETVAR] && (!var || var->env == lil->rootenv)) {
        lil_getvar_callback_proc_t proc = (lil_getvar_callback_proc_t)lil->callback[LIL_CALLBACK_GETVAR];
        lil_value_t newretval = retval;
        if (proc(lil, name, &newretval))
            retval = newretval;
    }
    return retval;
}

lil_env_t lil_push_env(lil_t lil)
{
    lil_env_t env = lil_alloc_env(lil->env);
    lil->env = env;
    return env;
}

void lil_pop_env(lil_t lil)
{
    if (lil->env->parent) {
        lil_env_t next = lil->env->parent;
        lil_free_env(lil->env);
        lil->env = next;
    }
}

lil_t lil_new(void)
{
    lil_t lil = calloc(1, sizeof(struct _lil_t));
    lil->rootenv = lil->env = lil_alloc_env(NULL);
    lil->empty = alloc_value(NULL);
    lil->dollarprefix = strclone("set ");
    hm_init(&lil->cmdmap);
    register_stdcmds(lil);
    return lil;
}

static int islilspecial(char ch)
{
    return ch == '$' || ch == '{' || ch == '}' || ch == '[' || ch == ']' || ch == '"' || ch == '\'' || ch == ';';
}

static int ateol(lil_t lil)
{
    return !(lil->ignoreeol) && (lil->code[lil->head] == '\n' || lil->code[lil->head] == '\r' || lil->code[lil->head] == ';');
}

static void skip_spaces(lil_t lil)
{
    while (lil->head < lil->clen) {
        if (lil->code[lil->head] == '#') {
            if (lil->code[lil->head + 1] == '#' && lil->code[lil->head + 2] != '#') {
                lil->head += 2;
                while (lil->head < lil->clen) {
                    if ((lil->code[lil->head] == '#') && (lil->code[lil->head + 1] == '#') && (lil->code[lil->head + 2] != '#')) {
                        lil->head += 2;
                        break;
                    }
                    lil->head++;
                }
            } else {
                while (lil->head < lil->clen && !ateol(lil)) lil->head++;
            }
        } else if (lil->code[lil->head] == '\\' && (lil->code[lil->head + 1] == '\r' || lil->code[lil->head + 1] == '\n')) {
            lil->head++;
            while (lil->head < lil->clen && ateol(lil)) lil->head++;
        } else if (lil->code[lil->head] == '\r' || lil->code[lil->head] == '\n') {
            if (lil->ignoreeol) lil->head++;
            else break;
        } else if (isspace(lil->code[lil->head]))
            lil->head++;
        else break;
    }
}

static lil_value_t get_bracketpart(lil_t lil)
{
    size_t cnt = 1;
    lil_value_t val, cmd = alloc_value(NULL);
    lil->head++;
    while (lil->head < lil->clen) {
        if (lil->code[lil->head] == '[') {
            lil->head++;
            cnt++;
            lil_append_char(cmd, '[');
        } else if (lil->code[lil->head] == ']') {
            lil->head++;
            if (--cnt == 0) break;
            else lil_append_char(cmd, ']');
        } else {
            lil_append_char(cmd, lil->code[lil->head++]);
        }
    }
    val = lil_parse_value(lil, cmd, 0);
    lil_free_value(cmd);
    return val;
}

static lil_value_t get_dollarpart(lil_t lil)
{
    lil_value_t val, name, tmp;
    lil->head++;
    name = next_word(lil);
    tmp = alloc_value(lil->dollarprefix);
    lil_append_val(tmp, name);
    lil_free_value(name);
    val = lil_parse_value(lil, tmp, 0);
    lil_free_value(tmp);
    return val;
}

static lil_value_t next_word(lil_t lil)
{
    lil_value_t val;
    size_t start;
    skip_spaces(lil);
    if (lil->code[lil->head] == '$') {
        val = get_dollarpart(lil);
    } else if (lil->code[lil->head] == '{') {
        size_t cnt = 1;
        lil->head++;
        val = alloc_value(NULL);
        while (lil->head < lil->clen) {
            if (lil->code[lil->head] == '{') {
                lil->head++;
                cnt++;
                lil_append_char(val, '{');
            } else if (lil->code[lil->head] == '}') {
                lil->head++;
                if (--cnt == 0) break;
                else lil_append_char(val, '}');
            } else {
                lil_append_char(val, lil->code[lil->head++]);
            }
        }
    } else if (lil->code[lil->head] == '[') {
        val = get_bracketpart(lil);
    } else if (lil->code[lil->head] == '"' || lil->code[lil->head] == '\'') {
        char sc = lil->code[lil->head++];
        val = alloc_value(NULL);
        while (lil->head < lil->clen) {
            if (lil->code[lil->head] == '[' || lil->code[lil->head] == '$') {
                lil_value_t tmp = lil->code[lil->head] == '$' ? get_dollarpart(lil) : get_bracketpart(lil);
                lil_append_val(val, tmp);
                lil_free_value(tmp);
                lil->head--; /* avoid skipping the char below */
            } else if (lil->code[lil->head] == '\\') {
                lil->head++;
                switch (lil->code[lil->head]) {
                    case 'b': lil_append_char(val, '\b'); break;
                    case 't': lil_append_char(val, '\t'); break;
                    case 'n': lil_append_char(val, '\n'); break;
                    case 'v': lil_append_char(val, '\v'); break;
                    case 'f': lil_append_char(val, '\f'); break;
                    case 'r': lil_append_char(val, '\r'); break;
                    case '0': lil_append_char(val, 0); break;
                    case 'a': lil_append_char(val, '\a'); break;
                    case 'c': lil_append_char(val, '}'); break;
                    case 'o': lil_append_char(val, '{'); break;
                    default: lil_append_char(val, lil->code[lil->head]); break;
                }
            } else if (lil->code[lil->head] == sc) {
                lil->head++;
                break;
            } else {
                lil_append_char(val, lil->code[lil->head]);
            }
            lil->head++;
        }
    } else {
        start = lil->head;
        while (lil->head < lil->clen && !isspace(lil->code[lil->head]) && !islilspecial(lil->code[lil->head])) {
            lil->head++;
        }
        val = alloc_value_len(lil->code + start, lil->head - start);
    }
    return val ? val : alloc_value(NULL);
}

static lil_list_t substitute(lil_t lil)
{
    lil_list_t words = lil_alloc_list();

    skip_spaces(lil);
    while (lil->head < lil->clen && !ateol(lil) && !lil->error) {
        lil_value_t w = alloc_value(NULL);
        do {
            size_t head = lil->head;
            lil_value_t wp = next_word(lil);
            if (head == lil->head) { /* something wrong, the parser can't proceed */
                lil_free_value(w);
                lil_free_value(wp);
                lil_free_list(words);
                return NULL;
            }
            lil_append_val(w, wp);
            lil_free_value(wp);
        } while (lil->head < lil->clen && !ateol(lil) && !isspace(lil->code[lil->head]) && !lil->error);
        skip_spaces(lil);

        lil_list_append(words, w);
    }

    return words;
}

lil_list_t lil_subst_to_list(lil_t lil, lil_value_t code)
{
    const char* save_code = lil->code;
    size_t save_clen = lil->clen;
    size_t save_head = lil->head;
    int save_igeol = lil->ignoreeol;
    lil_list_t words;
    lil->code = lil_to_string(code);
    lil->clen = code->l;
    lil->head = 0;
    lil->ignoreeol = 1;
    words = substitute(lil);
    lil->code = save_code;
    lil->clen = save_clen;
    lil->head = save_head;
    lil->ignoreeol = save_igeol;
    return words;
}

lil_value_t lil_subst_to_value(lil_t lil, lil_value_t code)
{
    lil_list_t words = lil_subst_to_list(lil, code);
    lil_value_t val;
    if (!words) return lil_clone_value(code);
    val = lil_list_to_value(words, 0);
    lil_free_list(words);
    return val;
}

lil_value_t lil_parse(lil_t lil, const char* code, size_t codelen, int funclevel)
{
    const char* save_code = lil->code;
    size_t save_clen = lil->clen;
    size_t save_head = lil->head;
    lil_value_t val = NULL;
    lil_list_t words = NULL;
    if (!save_code) lil->rootcode = code;
    lil->code = code;
    lil->clen = codelen ? codelen : strlen(code);
    lil->head = 0;
    skip_spaces(lil);
    lil->parse_depth++;
    if (lil->parse_depth == 1) lil->error = 0;
    if (funclevel) lil->env->breakrun = 0;
    while (lil->head < lil->clen && !lil->error) {
        if (words) lil_free_list(words);
        if (val) lil_free_value(val);
        val = NULL;

        words = substitute(lil);
        if (!words || lil->error) goto cleanup;

        if (words->c) {
            lil_func_t cmd = find_cmd(lil, lil_to_string(words->v[0]));
            if (!cmd) {
                if (words->v[0]->l) {
                    if (lil->catcher) {
                        if (lil->in_catcher < MAX_CATCHER_DEPTH) {
                            lil_value_t args;
                            lil->in_catcher++;
                            lil_push_env(lil);
                            lil->env->catcher_for = words->v[0];
                            args = lil_list_to_value(words, 1);
                            lil_set_var(lil, "args", args, LIL_SETVAR_LOCAL_NEW);
                            lil_free_value(args);
                            val = lil_parse(lil, lil->catcher, 0, 1);
                            lil_pop_env(lil);
                            lil->in_catcher--;
                        } else {
                            char* msg = malloc(words->v[0]->l + 64);
                            sprintf(msg, "catcher limit reached while trying to call unknown function %s", words->v[0]->d);
                            lil_set_error_at(lil, lil->head, msg);
                            free(msg);
                            goto cleanup;
                        }
                    } else {
                        char* msg = malloc(words->v[0]->l + 32);
                        sprintf(msg, "unknown function %s", words->v[0]->d);
                        lil_set_error_at(lil, lil->head, msg);
                        free(msg);
                        goto cleanup;
                    }
                }
            }
            if (cmd) {
                if (cmd->proc) {
                    size_t shead = lil->head;
                    val = cmd->proc(lil, words->c - 1, words->v + 1);
                    if (lil->error == ERROR_FIXHEAD) {
                        lil->error = ERROR_DEFAULT;
                        lil->err_head = shead;
                    }
                } else {
                    lil_push_env(lil);
                    lil->env->func = cmd;
                    if (cmd->argnames->c == 1 && !strcmp(lil_to_string(cmd->argnames->v[0]), "args")) {
                        lil_value_t args = lil_list_to_value(words, 1);
                        lil_set_var(lil, "args", args, LIL_SETVAR_LOCAL_NEW);
                        lil_free_value(args);
                    } else {
                        size_t i;
                        for (i=0; i<cmd->argnames->c; i++) {
                            lil_set_var(lil, lil_to_string(cmd->argnames->v[i]), i < words->c - 1 ? words->v[i + 1] : lil->empty, LIL_SETVAR_LOCAL_NEW);
                        }
                    }
                    val = lil_parse_value(lil, cmd->code, 1);
                    lil_pop_env(lil);
                }
            }
        }

        if (lil->env->breakrun) goto cleanup;

        skip_spaces(lil);
        while (ateol(lil)) lil->head++;
        skip_spaces(lil);
    }
cleanup:
    if (lil->error && lil->callback[LIL_CALLBACK_ERROR] && lil->parse_depth == 1) {
        lil_error_callback_proc_t proc = (lil_error_callback_proc_t)lil->callback[LIL_CALLBACK_ERROR];
        proc(lil, lil->err_head, lil->err_msg);
    }
    if (words) lil_free_list(words);
    lil->code = save_code;
    lil->clen = save_clen;
    lil->head = save_head;
    if (funclevel && lil->env->retval_set) {
        if (val) lil_free_value(val);
        val = lil->env->retval;
        lil->env->retval = NULL;
        lil->env->retval_set = 0;
        lil->env->breakrun = 0;
    }
    lil->parse_depth--;
    return val ? val : alloc_value(NULL);
}

lil_value_t lil_parse_value(lil_t lil, lil_value_t val, int funclevel)
{
    if (!val || !val->d || !val->l) return alloc_value(NULL);
    return lil_parse(lil, val->d, val->l, funclevel);
}

LILAPI lil_value_t lil_call(lil_t lil, const char* funcname, size_t argc, lil_value_t* argv)
{
    lil_func_t cmd = find_cmd(lil, funcname);
    lil_value_t r = NULL;
    if (cmd) {
        if (cmd->proc)
            r = cmd->proc(lil, argc, argv);
        else {
            size_t i;
            lil_push_env(lil);
            lil->env->func = cmd;
            if (cmd->argnames->c == 1 && !strcmp(lil_to_string(cmd->argnames->v[0]), "args")) {
                lil_list_t args = lil_alloc_list();
                lil_value_t argsval;
                for (i=0; i<argc; i++)
                    lil_list_append(args, lil_clone_value(argv[i]));
                argsval = lil_list_to_value(args, 0);
                lil_set_var(lil, "args", argsval, LIL_SETVAR_LOCAL_NEW);
                lil_free_value(argsval);
                lil_free_list(args);
            } else {
                for (i=0; i<cmd->argnames->c; i++)
                    lil_set_var(lil, lil_to_string(cmd->argnames->v[i]), i < argc ? argv[i] : NULL, LIL_SETVAR_LOCAL_NEW);
            }
            r = lil_parse_value(lil, cmd->code, 1);
            lil_pop_env(lil);
        }
    }
    return r;
}

void lil_callback(lil_t lil, int cb, lil_callback_proc_t proc)
{
    if (cb < 0 || cb > CALLBACKS) return;
    lil->callback[cb] = proc;
}

void lil_set_error(lil_t lil, const char* msg)
{
    if (lil->error) return;
    free(lil->err_msg);
    lil->error = ERROR_FIXHEAD;
    lil->err_head = 0;
    lil->err_msg = strclone(msg ? msg : "");
}

void lil_set_error_at(lil_t lil, size_t pos, const char* msg)
{
    if (lil->error) return;
    free(lil->err_msg);
    lil->error = ERROR_DEFAULT;
    lil->err_head = pos;
    lil->err_msg = strclone(msg ? msg : "");
}

int lil_error(lil_t lil, const char** msg, size_t* pos)
{
    if (!lil->error) return 0;
    *msg = lil->err_msg;
    *pos = lil->err_head;
    lil->error = ERROR_NOERROR;
    return 1;
}

#define EE_INT 0
#define EE_FLOAT 1
#define EERR_NO_ERROR 0
#define EERR_SYNTAX_ERROR 1
#define EERR_INVALID_TYPE 2
#define EERR_DIVISION_BY_ZERO 3
#define EERR_INVALID_EXPRESSION 4

static void ee_expr(expreval_t* ee);

static int ee_invalidpunct(int ch)
{
    return ispunct(ch) && ch != '!' && ch != '~' && ch != '(' && ch != ')' && ch != '-' && ch != '+';
}

static void ee_skip_spaces(expreval_t* ee)
{
    while (ee->head < ee->len && isspace(ee->code[ee->head])) ee->head++;
}

static void ee_numeric_element(expreval_t* ee)
{
    lilint_t fpart = 0, fpartlen = 1;
    ee->type = EE_INT;
    ee_skip_spaces(ee);
    ee->ival = 0;
    ee->dval = 0;
    while (ee->head < ee->len) {
        if (ee->code[ee->head] == '.') {
            if (ee->type == EE_FLOAT) break;
            ee->type = EE_FLOAT;
            ee->head++;
        } else if (!isdigit(ee->code[ee->head])) break;
        if (ee->type == EE_INT)
            ee->ival = ee->ival*10 + (ee->code[ee->head] - '0');
        else {
            fpart = fpart*10 + (ee->code[ee->head] - '0');
            fpartlen *= 10;
        }
        ee->head++;
    }
    if (ee->type == EE_FLOAT)
        ee->dval = ee->ival + (double)fpart/(double)fpartlen;
}

static void ee_element(expreval_t* ee)
{
    if (isdigit(ee->code[ee->head])) {
        ee_numeric_element(ee);
        return;
    }

    /* for anything else that might creep in (usually from strings), we set the
     * value to 1 so that strings evaluate as "true" when used in conditional
     * expressions */
    ee->type = EE_INT;
    ee->ival = 1;
    ee->error = EERR_INVALID_EXPRESSION; /* special flag, will be cleared */
}

static void ee_paren(expreval_t* ee)
{
    ee_skip_spaces(ee);
    if (ee->code[ee->head] == '(') {
        ee->head++;
        ee_expr(ee);
        ee_skip_spaces(ee);
        if (ee->code[ee->head] == ')') ee->head++;
        else ee->error = EERR_SYNTAX_ERROR;
    } else ee_element(ee);
}

static void ee_unary(expreval_t* ee)
{
    ee_skip_spaces(ee);
    if (ee->head < ee->len && !ee->error &&
        (ee->code[ee->head] == '-' ||
         ee->code[ee->head] == '+' ||
         ee->code[ee->head] == '~' ||
         ee->code[ee->head] == '!')) {
        char op = ee->code[ee->head++];
        ee_unary(ee);
        if (ee->error) return;
        switch (op) {
        case '-':
            switch (ee->type) {
            case EE_FLOAT:
                ee->dval = -ee->dval;
                break;
            case EE_INT:
                ee->ival = -ee->ival;
                break;
            default:
                ee->error = EERR_INVALID_TYPE;
                break;
            }
            break;
        case '+':
            /* ignore it, doesn't change a thing */
            break;
        case '~':
            switch (ee->type) {
            case EE_FLOAT:
                ee->ival = ~((lilint_t)ee->dval);
                ee->type = EE_INT;
                break;
            case EE_INT:
                ee->ival = ~ee->ival;
                break;
            default:
                ee->error = EERR_INVALID_TYPE;
                break;
            }
            break;
        case '!':
            switch (ee->type) {
            case EE_FLOAT:
                ee->dval = !ee->dval;
                break;
            case EE_INT:
                ee->ival = !ee->ival;
                break;
            default:
                ee->error = EERR_INVALID_TYPE;
                break;
            }
            break;
        }
    } else {
        ee_paren(ee);
    }
}

static void ee_muldiv(expreval_t* ee)
{
    ee_unary(ee);
    if (ee->error) return;
    ee_skip_spaces(ee);
    while (ee->head < ee->len && !ee->error && !ee_invalidpunct(ee->code[ee->head + 1]) &&
        (ee->code[ee->head] == '*' ||
         ee->code[ee->head] == '/' ||
         ee->code[ee->head] == '\\' ||
         ee->code[ee->head] == '%')) {
        double odval = ee->dval;
        lilint_t oival = ee->ival;

        switch (ee->code[ee->head]) {
        case '*':
            switch (ee->type) {
            case EE_FLOAT:
                ee->head++;
                ee_unary(ee);
                if (ee->error) return;
                switch (ee->type) {
                case EE_FLOAT:
                    ee->dval = ee->dval*odval;
                    break;
                case EE_INT:
                    ee->dval = ee->ival*odval;
                    ee->type = EE_FLOAT;
                    break;
                default:
                    ee->error = EERR_INVALID_TYPE;
                    break;
                }
                break;
            case EE_INT:
                ee->head++;
                ee_unary(ee);
                if (ee->error) return;
                switch (ee->type) {
                case EE_FLOAT:
                    ee->dval = ee->dval*oival;
                    ee->type = EE_FLOAT;
                    break;
                case EE_INT:
                    ee->ival = ee->ival*oival;
                    break;
                default:
                    ee->error = EERR_INVALID_TYPE;
                    break;
                }
                break;
            default:
                ee->error = EERR_INVALID_TYPE;
                break;
            }
            break;
        case '%':
            switch (ee->type) {
            case EE_FLOAT:
                ee->head++;
                ee_unary(ee);
                if (ee->error) return;
                switch (ee->type) {
                case EE_FLOAT:
                    if (ee->dval == 0.0) {
                        ee->error = EERR_DIVISION_BY_ZERO;
                    } else {
                        ee->dval = fmod(odval, ee->dval);
                    }
                    break;
                case EE_INT:
                    if (ee->ival == 0) {
                        ee->error = EERR_DIVISION_BY_ZERO;
                    } else {
                        ee->dval = fmod(odval, ee->ival);
                    }
                    ee->type = EE_FLOAT;
                    break;
                default:
                    ee->error = EERR_INVALID_TYPE;
                    break;
                }
                break;
            case EE_INT:
                ee->head++;
                ee_unary(ee);
                if (ee->error) return;
                switch (ee->type) {
                case EE_FLOAT:
                    if (ee->dval == 0.0) {
                        ee->error = EERR_DIVISION_BY_ZERO;
                    } else {
                        ee->dval = fmod(oival, ee->dval);
                    }
                    break;
                case EE_INT:
                    if (ee->ival == 0) {
                        ee->error = EERR_DIVISION_BY_ZERO;
                    } else {
                        ee->ival = oival%ee->ival;
                    }
                    break;
                default:
                    ee->error = EERR_INVALID_TYPE;
                    break;
                }
                break;
            }
            break;
        case '/':
            switch (ee->type) {
            case EE_FLOAT:
                ee->head++;
                ee_unary(ee);
                if (ee->error) return;
                switch (ee->type) {
                case EE_FLOAT:
                    if (ee->dval == 0.0) {
                        ee->error = EERR_DIVISION_BY_ZERO;
                    } else {
                        ee->dval = odval/ee->dval;
                    }
                    break;
                case EE_INT:
                    if (ee->ival == 0) {
                        ee->error = EERR_DIVISION_BY_ZERO;
                    } else {
                        ee->dval = odval/(double)ee->ival;
                    }
                    ee->type = EE_FLOAT;
                    break;
                default:
                    ee->error = EERR_INVALID_TYPE;
                    break;
                }
                break;
            case EE_INT:
                ee->head++;
                ee_unary(ee);
                if (ee->error) return;
                switch (ee->type) {
                case EE_FLOAT:
                    if (ee->dval == 0.0) {
                        ee->error = EERR_DIVISION_BY_ZERO;
                    } else {
                        ee->dval = (double)oival/ee->dval;
                    }
                    break;
                case EE_INT:
                    if (ee->ival == 0) {
                        ee->error = EERR_DIVISION_BY_ZERO;
                    } else {
                        ee->dval = (double)oival/(double)ee->ival;
                    }
                    ee->type = EE_FLOAT;
                    break;
                default:
                    ee->error = EERR_INVALID_TYPE;
                    break;
                }
                break;
            }
            break;
        case '\\':
            switch (ee->type) {
            case EE_FLOAT:
                ee->head++;
                ee_unary(ee);
                if (ee->error) return;
                switch (ee->type) {
                case EE_FLOAT:
                    if (ee->dval == 0.0) {
                        ee->error = EERR_DIVISION_BY_ZERO;
                    } else {
                        ee->ival = (lilint_t)(odval/ee->dval);
                    }
                    ee->type = EE_INT;
                    break;
                case EE_INT:
                    if (ee->ival == 0) {
                        ee->error = EERR_DIVISION_BY_ZERO;
                    } else {
                        ee->ival = (lilint_t)(odval/(double)ee->ival);
                    }
                    break;
                default:
                    ee->error = EERR_INVALID_TYPE;
                    break;
                }
                break;
            case EE_INT:
                ee->head++;
                ee_unary(ee);
                if (ee->error) return;
                switch (ee->type) {
                case EE_FLOAT:
                    if (ee->dval == 0.0) {
                        ee->error = EERR_DIVISION_BY_ZERO;
                    } else {
                        ee->ival = (lilint_t)((double)oival/ee->dval);
                    }
                    ee->type = EE_INT;
                    break;
                case EE_INT:
                    if (ee->ival == 0) {
                        ee->error = EERR_DIVISION_BY_ZERO;
                    } else {
                        ee->ival = oival/ee->ival;
                    }
                    break;
                default:
                    ee->error = EERR_INVALID_TYPE;
                    break;
                }
                break;
            default:
                ee->error = EERR_INVALID_TYPE;
                break;
            }
            break;
        }

        ee_skip_spaces(ee);
    }
}

static void ee_addsub(expreval_t* ee)
{
    ee_muldiv(ee);
    ee_skip_spaces(ee);
    while (ee->head < ee->len && !ee->error && !ee_invalidpunct(ee->code[ee->head + 1]) &&
        (ee->code[ee->head] == '+' ||
         ee->code[ee->head] == '-')) {
        double odval = ee->dval;
        lilint_t oival = ee->ival;

        switch (ee->code[ee->head]) {
        case '+':
            switch (ee->type) {
            case EE_FLOAT:
                ee->head++;
                ee_muldiv(ee);
                if (ee->error) return;
                switch (ee->type) {
                case EE_FLOAT:
                    ee->dval = ee->dval+odval;
                    break;
                case EE_INT:
                    ee->dval = ee->ival+odval;
                    ee->type = EE_FLOAT;
                    break;
                default:
                    ee->error = EERR_INVALID_TYPE;
                    break;
                }
                break;
            case EE_INT:
                ee->head++;
                ee_muldiv(ee);
                if (ee->error) return;
                switch (ee->type) {
                case EE_FLOAT:
                    ee->dval = ee->dval+oival;
                    ee->type = EE_FLOAT;
                    break;
                case EE_INT:
                    ee->ival = ee->ival+oival;
                    break;
                default:
                    ee->error = EERR_INVALID_TYPE;
                    break;
                }
                break;
            default:
                ee->error = EERR_INVALID_TYPE;
                break;
            }
            break;
        case '-':
            switch (ee->type) {
            case EE_FLOAT:
                ee->head++;
                ee_muldiv(ee);
                if (ee->error) return;
                switch (ee->type) {
                case EE_FLOAT:
                    ee->dval = odval-ee->dval;
                    break;
                case EE_INT:
                    ee->dval = odval-ee->ival;
                    ee->type = EE_FLOAT;
                    break;
                default:
                    ee->error = EERR_INVALID_TYPE;
                    break;
                }
                break;
            case EE_INT:
                ee->head++;
                ee_muldiv(ee);
                if (ee->error) return;
                switch (ee->type) {
                case EE_FLOAT:
                    ee->dval = (double)oival-ee->dval;
                    ee->type = EE_FLOAT;
                    break;
                case EE_INT:
                    ee->ival = oival-ee->ival;
                    break;
                default:
                    ee->error = EERR_INVALID_TYPE;
                    break;
                }
                break;
            default:
                ee->error = EERR_INVALID_TYPE;
                break;
            }
            break;
        }

        ee_skip_spaces(ee);
    }
}

static void ee_shift(expreval_t* ee)
{
    ee_addsub(ee);
    ee_skip_spaces(ee);
    while (ee->head < ee->len && !ee->error &&
        ((ee->code[ee->head] == '<' && ee->code[ee->head + 1] == '<') ||
         (ee->code[ee->head] == '>' && ee->code[ee->head + 1] == '>'))) {
        double odval = ee->dval;
        lilint_t oival = ee->ival;
        ee->head++;

        switch (ee->code[ee->head]) {
        case '<':
            switch (ee->type) {
            case EE_FLOAT:
                ee->head++;
                ee_addsub(ee);
                if (ee->error) return;
                switch (ee->type) {
                case EE_FLOAT:
                    ee->ival = (lilint_t)odval << (lilint_t)ee->dval;
                    ee->type = EE_INT;
                    break;
                case EE_INT:
                    ee->ival = (lilint_t)odval << ee->ival;
                    break;
                default:
                    ee->error = EERR_INVALID_TYPE;
                    break;
                }
                break;
            case EE_INT:
                ee->head++;
                ee_addsub(ee);
                if (ee->error) return;
                switch (ee->type) {
                case EE_FLOAT:
                    ee->ival = oival << (lilint_t)ee->dval;
                    ee->type = EE_INT;
                    break;
                case EE_INT:
                    ee->ival = oival << ee->ival;
                    break;
                default:
                    ee->error = EERR_INVALID_TYPE;
                    break;
                }
                break;
            default:
                ee->error = EERR_INVALID_TYPE;
                break;
            }
            break;
        case '>':
            switch (ee->type) {
            case EE_FLOAT:
                ee->head++;
                ee_addsub(ee);
                if (ee->error) return;
                switch (ee->type) {
                case EE_FLOAT:
                    ee->ival = (lilint_t)odval >> (lilint_t)ee->dval;
                    ee->type = EE_INT;
                    break;
                case EE_INT:
                    ee->ival = (lilint_t)odval >> ee->ival;
                    break;
                default:
                    ee->error = EERR_INVALID_TYPE;
                    break;
                }
                break;
            case EE_INT:
                ee->head++;
                ee_addsub(ee);
                if (ee->error) return;
                switch (ee->type) {
                case EE_FLOAT:
                    ee->ival = oival >> (lilint_t)ee->dval;
                    ee->type = EE_INT;
                    break;
                case EE_INT:
                    ee->ival = oival >> ee->ival;
                    break;
                default:
                    ee->error = EERR_INVALID_TYPE;
                    break;
                }
                break;
            default:
                ee->error = EERR_INVALID_TYPE;
                break;
            }
            break;
        }

        ee_skip_spaces(ee);
    }
}

static void ee_compare(expreval_t* ee)
{
    ee_shift(ee);
    ee_skip_spaces(ee);
    while (ee->head < ee->len && !ee->error &&
        ((ee->code[ee->head] == '<' && !ee_invalidpunct(ee->code[ee->head + 1])) ||
         (ee->code[ee->head] == '>' && !ee_invalidpunct(ee->code[ee->head + 1])) ||
         (ee->code[ee->head] == '<' && ee->code[ee->head + 1] == '=') ||
         (ee->code[ee->head] == '>' && ee->code[ee->head + 1] == '='))) {
        double odval = ee->dval;
        lilint_t oival = ee->ival;
        int op = 4;
        if (ee->code[ee->head] == '<' && !ee_invalidpunct(ee->code[ee->head + 1])) op = 1;
        else if (ee->code[ee->head] == '>' && !ee_invalidpunct(ee->code[ee->head + 1])) op = 2;
        else if (ee->code[ee->head] == '<' && ee->code[ee->head + 1] == '=') op = 3;

        ee->head += op > 2 ? 2 : 1;

        switch (op) {
        case 1:
            switch (ee->type) {
            case EE_FLOAT:
                ee_shift(ee);
                if (ee->error) return;
                switch (ee->type) {
                case EE_FLOAT:
                    ee->ival = (odval < ee->dval)?1:0;
                    ee->type = EE_INT;
                    break;
                case EE_INT:
                    ee->ival = (odval < ee->ival)?1:0;
                    break;
                default:
                    ee->error = EERR_INVALID_TYPE;
                    break;
                }
                break;
            case EE_INT:
                ee_shift(ee);
                if (ee->error) return;
                switch (ee->type) {
                case EE_FLOAT:
                    ee->ival = (oival < ee->dval)?1:0;
                    ee->type = EE_INT;
                    break;
                case EE_INT:
                    ee->ival = (oival < ee->ival)?1:0;
                    break;
                default:
                    ee->error = EERR_INVALID_TYPE;
                    break;
                }
                break;
            default:
                ee->error = EERR_INVALID_TYPE;
                break;
            }
            break;
        case 2:
            switch (ee->type) {
            case EE_FLOAT:
                ee_shift(ee);
                if (ee->error) return;
                switch (ee->type) {
                case EE_FLOAT:
                    ee->ival = (odval > ee->dval)?1:0;
                    ee->type = EE_INT;
                    break;
                case EE_INT:
                    ee->ival = (odval > ee->ival)?1:0;
                    break;
                default:
                    ee->error = EERR_INVALID_TYPE;
                    break;
                }
                break;
            case EE_INT:
                ee_shift(ee);
                if (ee->error) return;
                switch (ee->type) {
                case EE_FLOAT:
                    ee->ival = (oival > ee->dval)?1:0;
                    ee->type = EE_INT;
                    break;
                case EE_INT:
                    ee->ival = (oival > ee->ival)?1:0;
                    break;
                default:
                    ee->error = EERR_INVALID_TYPE;
                    break;
                }
                break;
            default:
                ee->error = EERR_INVALID_TYPE;
                break;
            }
            break;
        case 3:
            switch (ee->type) {
            case EE_FLOAT:
                ee_shift(ee);
                if (ee->error) return;
                switch (ee->type) {
                case EE_FLOAT:
                    ee->ival = (odval <= ee->dval)?1:0;
                    ee->type = EE_INT;
                    break;
                case EE_INT:
                    ee->ival = (odval <= ee->ival)?1:0;
                    break;
                default:
                    ee->error = EERR_INVALID_TYPE;
                    break;
                }
                break;
            case EE_INT:
                ee_shift(ee);
                if (ee->error) return;
                switch (ee->type) {
                case EE_FLOAT:
                    ee->ival = (oival <= ee->dval)?1:0;
                    ee->type = EE_INT;
                    break;
                case EE_INT:
                    ee->ival = (oival <= ee->ival)?1:0;
                    break;
                default:
                    ee->error = EERR_INVALID_TYPE;
                    break;
                }
                break;
            default:
                ee->error = EERR_INVALID_TYPE;
                break;
            }
            break;
        case 4:
            switch (ee->type) {
            case EE_FLOAT:
                ee_shift(ee);
                if (ee->error) return;
                switch (ee->type) {
                case EE_FLOAT:
                    ee->ival = (odval >= ee->dval)?1:0;
                    ee->type = EE_INT;
                    break;
                case EE_INT:
                    ee->ival = (odval >= ee->ival)?1:0;
                    break;
                default:
                    ee->error = EERR_INVALID_TYPE;
                    break;
                }
                break;
            case EE_INT:
                ee_shift(ee);
                if (ee->error) return;
                switch (ee->type) {
                case EE_FLOAT:
                    ee->ival = (oival >= ee->dval)?1:0;
                    ee->type = EE_INT;
                    break;
                case EE_INT:
                    ee->ival = (oival >= ee->ival)?1:0;
                    break;
                default:
                    ee->error = EERR_INVALID_TYPE;
                    break;
                }
                break;
            default:
                ee->error = EERR_INVALID_TYPE;
                break;
            }
            break;
        }

        ee_skip_spaces(ee);
    }
}

static void ee_equals(expreval_t* ee)
{
    ee_compare(ee);
    ee_skip_spaces(ee);
    while (ee->head < ee->len && !ee->error &&
        ((ee->code[ee->head] == '=' && ee->code[ee->head + 1] == '=') ||
         (ee->code[ee->head] == '!' && ee->code[ee->head + 1] == '='))) {
        double odval = ee->dval;
        lilint_t oival = ee->ival;
        int op = ee->code[ee->head] == '=' ? 1 : 2;
        ee->head += 2;

        switch (op) {
        case 1:
            switch (ee->type) {
            case EE_FLOAT:
                ee_compare(ee);
                if (ee->error) return;
                switch (ee->type) {
                case EE_FLOAT:
                    ee->ival = (odval == ee->dval)?1:0;
                    ee->type = EE_INT;
                    break;
                case EE_INT:
                    ee->ival = (odval == ee->ival)?1:0;
                    break;
                default:
                    ee->error = EERR_INVALID_TYPE;
                    break;
                }
                break;
            case EE_INT:
                ee_compare(ee);
                if (ee->error) return;
                switch (ee->type) {
                case EE_FLOAT:
                    ee->ival = (oival == ee->dval)?1:0;
                    ee->type = EE_INT;
                    break;
                case EE_INT:
                    ee->ival = (oival == ee->ival)?1:0;
                    break;
                default:
                    ee->error = EERR_INVALID_TYPE;
                    break;
                }
                break;
            default:
                ee->error = EERR_INVALID_TYPE;
                break;
            }
            break;
        case 2:
            switch (ee->type) {
            case EE_FLOAT:
                ee_compare(ee);
                if (ee->error) return;
                switch (ee->type) {
                case EE_FLOAT:
                    ee->ival = (odval != ee->dval)?1:0;
                    ee->type = EE_INT;
                    break;
                case EE_INT:
                    ee->ival = (odval != ee->ival)?1:0;
                    break;
                default:
                    ee->error = EERR_INVALID_TYPE;
                    break;
                }
                break;
            case EE_INT:
                ee_compare(ee);
                if (ee->error) return;
                switch (ee->type) {
                case EE_FLOAT:
                    ee->ival = (oival != ee->dval)?1:0;
                    ee->type = EE_INT;
                    break;
                case EE_INT:
                    ee->ival = (oival != ee->ival)?1:0;
                    break;
                default:
                    ee->error = EERR_INVALID_TYPE;
                    break;
                }
                break;
            default:
                ee->error = EERR_INVALID_TYPE;
                break;
            }
            break;
        }

        ee_skip_spaces(ee);
    }
}

static void ee_bitand(expreval_t* ee)
{
    ee_equals(ee);
    ee_skip_spaces(ee);
    while (ee->head < ee->len && !ee->error &&
        (ee->code[ee->head] == '&' && !ee_invalidpunct(ee->code[ee->head + 1]))) {
        double odval = ee->dval;
        lilint_t oival = ee->ival;
        ee->head++;

        switch (ee->type) {
        case EE_FLOAT:
            ee_equals(ee);
            if (ee->error) return;
            switch (ee->type) {
            case EE_FLOAT:
                ee->ival = (lilint_t)odval & (lilint_t)ee->dval;
                ee->type = EE_INT;
                break;
            case EE_INT:
                ee->ival = (lilint_t)odval & ee->ival;
                break;
            default:
                ee->error = EERR_INVALID_TYPE;
                break;
            }
            break;
        case EE_INT:
            ee_equals(ee);
            if (ee->error) return;
            switch (ee->type) {
            case EE_FLOAT:
                ee->ival = oival & (lilint_t)ee->dval;
                ee->type = EE_INT;
                break;
            case EE_INT:
                ee->ival = oival & ee->ival;
                break;
            default:
                ee->error = EERR_INVALID_TYPE;
                break;
            }
            break;
        default:
            ee->error = EERR_INVALID_TYPE;
            break;
        }

        ee_skip_spaces(ee);
    }
}

static void ee_bitor(expreval_t* ee)
{
    ee_bitand(ee);
    ee_skip_spaces(ee);
    while (ee->head < ee->len && !ee->error &&
        (ee->code[ee->head] == '|' && !ee_invalidpunct(ee->code[ee->head + 1]))) {
        double odval = ee->dval;
        lilint_t oival = ee->ival;
        ee->head++;

        switch (ee->type) {
        case EE_FLOAT:
            ee_bitand(ee);
            if (ee->error) return;
            switch (ee->type) {
            case EE_FLOAT:
                ee->ival = (lilint_t)odval | (lilint_t)ee->dval;
                ee->type = EE_INT;
                break;
            case EE_INT:
                ee->ival = (lilint_t)odval | ee->ival;
                break;
            default:
                ee->error = EERR_INVALID_TYPE;
                break;
            }
            break;
        case EE_INT:
            ee_bitand(ee);
            if (ee->error) return;
            switch (ee->type) {
            case EE_FLOAT:
                ee->ival = oival | (lilint_t)ee->dval;
                ee->type = EE_INT;
                break;
            case EE_INT:
                ee->ival = oival | ee->ival;
                break;
            default:
                ee->error = EERR_INVALID_TYPE;
                break;
            }
            break;
        default:
            ee->error = EERR_INVALID_TYPE;
            break;
        }

        ee_skip_spaces(ee);
    }
}

static void ee_logand(expreval_t* ee)
{
    ee_bitor(ee);
    ee_skip_spaces(ee);
    while (ee->head < ee->len && !ee->error &&
        (ee->code[ee->head] == '&' && ee->code[ee->head + 1] == '&')) {
        double odval = ee->dval;
        lilint_t oival = ee->ival;
        ee->head += 2;

        switch (ee->type) {
        case EE_FLOAT:
            ee_bitor(ee);
            if (ee->error) return;
            switch (ee->type) {
            case EE_FLOAT:
                ee->ival = (odval && ee->dval)?1:0;
                ee->type = EE_INT;
                break;
            case EE_INT:
                ee->ival = (odval && ee->ival)?1:0;
                break;
            default:
                ee->error = EERR_INVALID_TYPE;
                break;
            }
            break;
        case EE_INT:
            ee_bitor(ee);
            if (ee->error) return;
            switch (ee->type) {
            case EE_FLOAT:
                ee->ival = (oival && ee->dval)?1:0;
                ee->type = EE_INT;
                break;
            case EE_INT:
                ee->ival = (oival && ee->ival)?1:0;
                break;
            default:
                ee->error = EERR_INVALID_TYPE;
                break;
            }
            break;
        default:
            ee->error = EERR_INVALID_TYPE;
            break;
        }

        ee_skip_spaces(ee);
    }
}

static void ee_logor(expreval_t* ee)
{
    ee_logand(ee);
    ee_skip_spaces(ee);
    while (ee->head < ee->len && !ee->error &&
        (ee->code[ee->head] == '|' && ee->code[ee->head + 1] == '|')) {
        double odval = ee->dval;
        lilint_t oival = ee->ival;
        ee->head += 2;

        switch (ee->type) {
        case EE_FLOAT:
            ee_logand(ee);
            if (ee->error) return;
            switch (ee->type) {
            case EE_FLOAT:
                ee->ival = (odval || ee->dval)?1:0;
                ee->type = EE_INT;
                break;
            case EE_INT:
                ee->ival = (odval || ee->ival)?1:0;
                break;
            default:
                ee->error = EERR_INVALID_TYPE;
                break;
            }
            break;
        case EE_INT:
            ee_logand(ee);
            if (ee->error) return;
            switch (ee->type) {
            case EE_FLOAT:
                ee->ival = (oival || ee->dval)?1:0;
                ee->type = EE_INT;
                break;
            case EE_INT:
                ee->ival = (oival || ee->ival)?1:0;
                break;
            default:
                ee->error = EERR_INVALID_TYPE;
                break;
            }
            break;
        default:
            ee->error = EERR_INVALID_TYPE;
            break;
        }

        ee_skip_spaces(ee);
    }
}

static void ee_expr(expreval_t* ee)
{
    ee_logor(ee);
    /* invalid expression doesn't really matter, it is only used to stop
     * the expression parsing. */
    if (ee->error == EERR_INVALID_EXPRESSION) {
        ee->error = EERR_NO_ERROR;
        ee->ival = 1;
    }
}

lil_value_t lil_eval_expr(lil_t lil, lil_value_t code)
{
    expreval_t ee;
    code = lil_subst_to_value(lil, code);
    if (lil->error) return NULL;
    ee.code = lil_to_string(code);
    /* an empty expression equals to 0 so that it can be used as a false value
     * in conditionals */
    if (!ee.code[0]) {
        lil_free_value(code);
        return lil_alloc_integer(0);
    }
    ee.head = 0;
    ee.len = code->l;
    ee.ival = 0;
    ee.dval = 0;
    ee.type = EE_INT;
    ee.error = 0;
    ee_expr(&ee);
    lil_free_value(code);
    if (ee.error) {
        switch (ee.error) {
        case EERR_DIVISION_BY_ZERO:
            lil_set_error(lil, "division by zero in expression");
            break;
        case EERR_INVALID_TYPE:
            lil_set_error(lil, "mixing invalid types in expression");
            break;
        case EERR_SYNTAX_ERROR:
            lil_set_error(lil, "expression syntax error");
            break;
        }
        return NULL;
    }
    if (ee.type == EE_INT)
        return lil_alloc_integer(ee.ival);
    else
        return lil_alloc_double(ee.dval);
}

lil_value_t lil_unused_name(lil_t lil, const char* part)
{
    char* name = malloc(strlen(part) + 64);
    lil_value_t val;
    size_t i;
    for (i=0; i<(size_t)-1; i++) {
        sprintf(name, "!!un!%s!%09u!nu!!", part, (unsigned int)i);
        if (find_cmd(lil, name)) continue;
        if (lil_find_var(lil, lil->env, name)) continue;
        val = lil_alloc_string(name);
        free(name);
        return val;
    }
    return NULL;
}

lil_value_t lil_arg(lil_value_t* argv, size_t index)
{
    return argv ? argv[index] : NULL;
}

const char* lil_to_string(lil_value_t val)
{
    return (val && val->l) ? val->d : "";
}

double lil_to_double(lil_value_t val)
{
    return atof(lil_to_string(val));
}

lilint_t lil_to_integer(lil_value_t val)
{
    return (lilint_t)atoll(lil_to_string(val));
}

int lil_to_boolean(lil_value_t val)
{
    const char* s = lil_to_string(val);
    size_t i, dots = 0;
    if (!s[0]) {return 0;}
    for (i=0; s[i]; i++) {
        if (s[i] != '0' && s[i] != '.') return 1;
        if (s[i] == '.') {
            if (dots) return 1;
            dots = 1;
        }
    }
    return 0;
}

lil_value_t lil_alloc_string(const char* str)
{
    return alloc_value(str);
}

lil_value_t lil_alloc_string_len(const char* str, size_t len)
{
    return alloc_value_len(str, len);
}

lil_value_t lil_alloc_double(double num)
{
    char buff[128];
    sprintf(buff, "%f", num);
    return alloc_value(buff);
}

lil_value_t lil_alloc_integer(lilint_t num)
{
    char buff[128];
    sprintf(buff, LILINT_PRINTF, num);
    return alloc_value(buff);
}

void lil_free(lil_t lil)
{
    size_t i;
    if (!lil) return;
    free(lil->err_msg);
    lil_free_value(lil->empty);
    while (lil->env) {
        lil_env_t next = lil->env->parent;
        lil_free_env(lil->env);
        lil->env = next;
    }
    for (i=0; i<lil->cmds; i++) {
        if (lil->cmd[i]->argnames)
            lil_free_list(lil->cmd[i]->argnames);
        lil_free_value(lil->cmd[i]->code);
        free(lil->cmd[i]->name);
        free(lil->cmd[i]);
    }
    hm_destroy(&lil->cmdmap);
    free(lil->cmd);
    free(lil->dollarprefix);
    free(lil->catcher);
    free(lil);
}

LILAPI void lil_set_data(lil_t lil, void* data)
{
    lil->data = data;
}

LILAPI void* lil_get_data(lil_t lil)
{
    return lil->data;
}

static LILCALLBACK void fnc_embed_write(lil_t lil, const char* msg)
{
    size_t len = strlen(msg) + 1;
    lil->embed = realloc(lil->embed, lil->embedlen + len);
    memcpy(lil->embed + lil->embedlen, msg, len);
    lil->embedlen += len - 1;
}

char* lil_embedded(lil_t lil, const char* code, unsigned int flags)
{
    char* prev_embed = lil->embed;
    size_t prev_embedlen = lil->embedlen;
    lil_callback_proc_t prev_write = lil->callback[LIL_CALLBACK_WRITE];
    char* lilcode = NULL;
    size_t lilcodelen = 0;
    char* cont = NULL;
    size_t contlen = 0;
    size_t head = 0, codelen = strlen(code);
    char* result;

    lil->callback[LIL_CALLBACK_WRITE] = (lil_callback_proc_t)fnc_embed_write;
    lil->embed = NULL;
    lil->embedlen = 0;

    while (head < codelen) {
        if (head < codelen - 4 &&
            code[head] == '<' &&
            code[head + 1] == '?' &&
            code[head + 2] == 'l' &&
            code[head + 3] == 'i' &&
            code[head + 4] == 'l') {
            head += 5;
            if (contlen) {
                lilcode = realloc(lilcode, lilcodelen + contlen + 10);
                memcpy(lilcode + lilcodelen, "\nwrite {", 8);
                memcpy(lilcode + lilcodelen + 8, cont, contlen);
                lilcode[lilcodelen + contlen + 8] = '}';
                lilcode[lilcodelen + contlen + 9] = '\n';
                lilcodelen += contlen + 10;
                free(cont);
                cont = NULL;
                contlen = 0;
            }
            while (head < codelen) {
                if (head < codelen - 1 && code[head] == '?' && code[head + 1] == '>') {
                    head += 2;
                    break;
                }
                lilcode = realloc(lilcode, lilcodelen + 1);
                lilcode[lilcodelen++] = code[head++];
            }
            lilcode = realloc(lilcode, lilcodelen + 1);
            lilcode[lilcodelen++] = '\n';
        } else {
            if (code[head] == '{' || code[head] == '}') {
                cont = realloc(cont, contlen + 6);
                cont[contlen++] = '}';
                cont[contlen++] = '"';
                cont[contlen++] = '\\';
                cont[contlen++] = code[head] == '{' ? 'o' : 'c';
                cont[contlen++] = '"';
                cont[contlen++] = '{';
                head++;
            } else {
                cont = realloc(cont, contlen + 1);
                cont[contlen++] = code[head++];
            }
        }
    }
    if (contlen) {
        lilcode = realloc(lilcode, lilcodelen + contlen + 10);
        memcpy(lilcode + lilcodelen, "\nwrite {", 8);
        memcpy(lilcode + lilcodelen + 8, cont, contlen);
        lilcode[lilcodelen + contlen + 8] = '}';
        lilcode[lilcodelen + contlen + 9] = '\n';
        lilcodelen += contlen + 10;
        free(cont);
        cont = NULL;
    }

    lilcode = realloc(lilcode, lilcodelen + 1);
    lilcode[lilcodelen] = 0;
    lil_free_value(lil_parse(lil, lilcode, 0, 1));
    free(lilcode);
    result = lil->embed ? lil->embed : strclone("");

    lil->embed = prev_embed;
    lil->embedlen = prev_embedlen;
    lil->callback[LIL_CALLBACK_WRITE] = prev_write;

    return result;
}

void lil_freemem(void* ptr)
{
    free(ptr);
}

static LILCALLBACK lil_value_t fnc_reflect(lil_t lil, size_t argc, lil_value_t* argv)
{
    lil_func_t func;
    const char* type;
    size_t i;
    lil_value_t r;
    if (!argc) return NULL;
    type = lil_to_string(argv[0]);
    if (!strcmp(type, "version")) {
        return lil_alloc_string(LIL_VERSION_STRING);
    }
    if (!strcmp(type, "args")) {
        if (argc < 2) return NULL;
        func = find_cmd(lil, lil_to_string(argv[1]));
        if (!func || !func->argnames) return NULL;
        return lil_list_to_value(func->argnames, 1);
    }
    if (!strcmp(type, "body")) {
        if (argc < 2) return NULL;
        func = find_cmd(lil, lil_to_string(argv[1]));
        if (!func || func->proc) return NULL;
        return lil_clone_value(func->code);
    }
    if (!strcmp(type, "func-count")) {
        return lil_alloc_integer(lil->cmds);
    }
    if (!strcmp(type, "funcs")) {
        lil_list_t funcs = lil_alloc_list();
        for (i=0; i<lil->cmds; i++)
            lil_list_append(funcs, lil_alloc_string(lil->cmd[i]->name));
        r = lil_list_to_value(funcs, 1);
        lil_free_list(funcs);
        return r;
    }
    if (!strcmp(type, "vars")) {
        lil_list_t vars = lil_alloc_list();
        lil_env_t env = lil->env;
        while (env) {
            for (i=0; i<env->vars; i++)
                lil_list_append(vars, lil_alloc_string(env->var[i]->n));
            env = env->parent;
        }
        r = lil_list_to_value(vars, 1);
        lil_free_list(vars);
        return r;
    }
    if (!strcmp(type, "globals")) {
        lil_list_t vars = lil_alloc_list();
        for (i=0; i<lil->rootenv->vars; i++)
            lil_list_append(vars, lil_alloc_string(lil->rootenv->var[i]->n));
        r = lil_list_to_value(vars, 1);
        lil_free_list(vars);
        return r;
    }
    if (!strcmp(type, "has-func")) {
        const char* target;
        if (argc == 1) return NULL;
        target = lil_to_string(argv[1]);
        return hm_has(&lil->cmdmap, target) ? lil_alloc_string("1") : NULL;
    }
    if (!strcmp(type, "has-var")) {
        const char* target;
        lil_env_t env = lil->env;
        if (argc == 1) return NULL;
        target = lil_to_string(argv[1]);
        while (env) {
            if (hm_has(&env->varmap, target)) return lil_alloc_string("1");
            env = env->parent;
        }
        return NULL;
    }
    if (!strcmp(type, "has-global")) {
        const char* target;
        if (argc == 1) return NULL;
        target = lil_to_string(argv[1]);
        for (i=0; i<lil->rootenv->vars; i++)
            if (!strcmp(target, lil->rootenv->var[i]->n)) return lil_alloc_string("1");
        return NULL;
    }
    if (!strcmp(type, "error")) {
        return lil->err_msg ? lil_alloc_string(lil->err_msg) : NULL;
    }
    if (!strcmp(type, "dollar-prefix")) {
        lil_value_t r;
        if (argc == 1) return lil_alloc_string(lil->dollarprefix);
        r = lil_alloc_string(lil->dollarprefix);
        free(lil->dollarprefix);
        lil->dollarprefix = strclone(lil_to_string(argv[1]));
        return r;
    }
    if (!strcmp(type, "this")) {
        lil_env_t env = lil->env;
        while (env != lil->rootenv && !env->catcher_for && !env->func) env = env->parent;
        if (env->catcher_for) return lil_alloc_string(lil->catcher);
        if (env == lil->rootenv) return lil_alloc_string(lil->rootcode);
        return env->func ? env->func->code : NULL;
    }
    if (!strcmp(type, "name")) {
        lil_env_t env = lil->env;
        while (env != lil->rootenv && !env->catcher_for && !env->func) env = env->parent;
        if (env->catcher_for) return env->catcher_for;
        if (env == lil->rootenv) return NULL;
        return env->func ? lil_alloc_string(env->func->name) : NULL;
    }
    return NULL;
}

static LILCALLBACK lil_value_t fnc_func(lil_t lil, size_t argc, lil_value_t* argv)
{
    lil_value_t name;
    lil_func_t cmd;
    if (argc < 1) return NULL;
    if (argc == 3) {
        name = lil_clone_value(argv[0]);
        cmd = add_func(lil, lil_to_string(argv[0]));
        cmd->argnames = lil_subst_to_list(lil, argv[1]);
        cmd->code = lil_clone_value(argv[2]);
    } else {
        name = lil_unused_name(lil, "anonymous-function");
        cmd = add_func(lil, lil_to_string(name));
        if (argc < 2) {
            lil_value_t tmp = lil_alloc_string("args");
            cmd->argnames = lil_subst_to_list(lil, tmp);
            lil_free_value(tmp);
            cmd->code = lil_clone_value(argv[0]);
        } else {
            cmd->argnames = lil_subst_to_list(lil, argv[0]);
            cmd->code = lil_clone_value(argv[1]);
        }
    }
    return name;
}

static LILCALLBACK lil_value_t fnc_rename(lil_t lil, size_t argc, lil_value_t* argv)
{
    lil_value_t r;
    lil_func_t func;
    const char* oldname;
    const char* newname;
    if (argc < 2) return NULL;
    oldname = lil_to_string(argv[0]);
    newname = lil_to_string(argv[1]);
    func = find_cmd(lil, oldname);
    if (!func) {
        char* msg = malloc(24 + strlen(oldname));
        sprintf(msg, "unknown function '%s'", oldname);
        lil_set_error_at(lil, lil->head, msg);
        free(msg);
        return NULL;
    }
    r = lil_alloc_string(func->name);
    free(func->name);
    func->name = strclone(newname);
    return r;
}

static LILCALLBACK lil_value_t fnc_unusedname(lil_t lil, size_t argc, lil_value_t* argv)
{
    return lil_unused_name(lil, argc > 0 ? lil_to_string(argv[0]) : "unusedname");
}

static LILCALLBACK lil_value_t fnc_quote(lil_t lil, size_t argc, lil_value_t* argv)
{
    lil_value_t r;
    size_t i;
    if (argc < 1) return NULL;
    r = alloc_value(NULL);
    for (i=0; i<argc; i++) {
        if (i) lil_append_char(r, ' ');
        lil_append_val(r, argv[i]);
    }
    return r;
}

static LILCALLBACK lil_value_t fnc_set(lil_t lil, size_t argc, lil_value_t* argv)
{
    size_t i = 0;
    lil_var_t var = NULL;
    int access = LIL_SETVAR_LOCAL;
    if (!argc) return NULL;
    if (!strcmp(lil_to_string(argv[0]), "global")) {
        i = 1;
        access = LIL_SETVAR_GLOBAL;
    }
    while (i < argc) {
        if (argc == i + 1) return lil_clone_value(lil_get_var(lil, lil_to_string(argv[i])));
        var = lil_set_var(lil, lil_to_string(argv[i]), argv[i + 1], access);
        i += 2;
    }
    return var ? lil_clone_value(var->v) : NULL;
}

static LILCALLBACK lil_value_t fnc_local(lil_t lil, size_t argc, lil_value_t* argv)
{
    size_t i;
    for (i=0; i<argc; i++) {
        const char* varname = lil_to_string(argv[i]);
        if (!lil_find_local_var(lil, lil->env, varname))
            lil_set_var(lil, varname, lil->empty, LIL_SETVAR_LOCAL_NEW);
    }
    return NULL;
}

static LILCALLBACK lil_value_t fnc_write(lil_t lil, size_t argc, lil_value_t* argv)
{
    size_t i;
    lil_value_t msg = lil_alloc_string(NULL);
    for (i=0; i<argc; i++) {
        if (i) lil_append_char(msg, ' ');
        lil_append_val(msg, argv[i]);
    }
    if (lil->callback[LIL_CALLBACK_WRITE]) {
        lil_write_callback_proc_t proc = (lil_write_callback_proc_t)lil->callback[LIL_CALLBACK_WRITE];
        proc(lil, lil_to_string(msg));
    } else printf("%s", lil_to_string(msg));
    lil_free_value(msg);
    return NULL;
}

static LILCALLBACK lil_value_t fnc_print(lil_t lil, size_t argc, lil_value_t* argv)
{
    fnc_write(lil, argc, argv);
    if (lil->callback[LIL_CALLBACK_WRITE]) {
        lil_write_callback_proc_t proc = (lil_write_callback_proc_t)lil->callback[LIL_CALLBACK_WRITE];
        proc(lil, "\n");
    } else printf("\n");
    return NULL;
}

static LILCALLBACK lil_value_t fnc_eval(lil_t lil, size_t argc, lil_value_t* argv)
{
    if (argc == 1) return lil_parse_value(lil, argv[0], 0);
    if (argc > 1) {
        lil_value_t val = alloc_value(NULL), r;
        size_t i;
        for (i=0; i<argc; i++) {
            if (i) lil_append_char(val, ' ');
            lil_append_val(val, argv[i]);
        }
        r = lil_parse_value(lil, val, 0);
        lil_free_value(val);
        return r;
    }
    return NULL;
}

static LILCALLBACK lil_value_t fnc_topeval(lil_t lil, size_t argc, lil_value_t* argv)
{
    lil_env_t thisenv = lil->env;
    lil_env_t thisdownenv = lil->downenv;
    lil_value_t r;
    lil->env = lil->rootenv;
    lil->downenv = thisenv;
    r = fnc_eval(lil, argc, argv);
    lil->downenv = thisdownenv;
    lil->env = thisenv;
    return r;
}

static LILCALLBACK lil_value_t fnc_upeval(lil_t lil, size_t argc, lil_value_t* argv)
{
    lil_env_t thisenv = lil->env;
    lil_env_t thisdownenv = lil->downenv;
    lil_value_t r;
    if (lil->rootenv == thisenv) return fnc_eval(lil, argc, argv);
    lil->env = thisenv->parent;
    lil->downenv = thisenv;
    r = fnc_eval(lil, argc, argv);
    lil->env = thisenv;
    lil->downenv = thisdownenv;
    return r;
}

static LILCALLBACK lil_value_t fnc_downeval(lil_t lil, size_t argc, lil_value_t* argv)
{
    lil_value_t r;
    lil_env_t upenv = lil->env;
    lil_env_t downenv = lil->downenv;
    if (!downenv) return fnc_eval(lil, argc, argv);
    lil->downenv = NULL;
    lil->env = downenv;
    r = fnc_eval(lil, argc, argv);
    lil->downenv = downenv;
    lil->env = upenv;
    return r;
}

static LILCALLBACK lil_value_t fnc_enveval(lil_t lil, size_t argc, lil_value_t* argv)
{
    lil_value_t r;
    lil_list_t invars = NULL;
    lil_list_t outvars = NULL;
    lil_value_t* varvalues = NULL;
    int codeindex;
    size_t i;
    if (argc < 1) return NULL;
    if (argc == 1) codeindex = 0;
    else if (argc >= 2) {
        invars = lil_subst_to_list(lil, argv[0]);
        varvalues = malloc(sizeof(lil_value_t)*lil_list_size(invars));
        for (i=0; i<lil_list_size(invars); i++)
            varvalues[i] = lil_clone_value(lil_get_var(lil, lil_to_string(lil_list_get(invars, i))));
        if (argc > 2) {
            codeindex = 2;
            outvars = lil_subst_to_list(lil, argv[1]);
        } else {
            codeindex = 1;
        }
    }
    lil_push_env(lil);
    if (invars) {
        for (i=0; i<lil_list_size(invars); i++) {
            lil_set_var(lil, lil_to_string(lil_list_get(invars, i)), varvalues[i], LIL_SETVAR_LOCAL_NEW);
            lil_free_value(varvalues[i]);
        }
    }
    r = lil_parse_value(lil, argv[codeindex], 0);
    if (invars || outvars) {
        if (outvars) {
            varvalues = realloc(varvalues, sizeof(lil_value_t)*lil_list_size(outvars));
            for (i=0; i<lil_list_size(outvars); i++)
                varvalues[i] = lil_clone_value(lil_get_var(lil, lil_to_string(lil_list_get(outvars, i))));
        } else {
            for (i=0; i<lil_list_size(invars); i++)
                varvalues[i] = lil_clone_value(lil_get_var(lil, lil_to_string(lil_list_get(invars, i))));
        }
    }
    lil_pop_env(lil);
    if (invars) {
        if (outvars) {
            for (i=0; i<lil_list_size(outvars); i++) {
                lil_set_var(lil, lil_to_string(lil_list_get(outvars, i)), varvalues[i], LIL_SETVAR_LOCAL);
                lil_free_value(varvalues[i]);
            }
        } else {
            for (i=0; i<lil_list_size(invars); i++) {
                lil_set_var(lil, lil_to_string(lil_list_get(invars, i)), varvalues[i], LIL_SETVAR_LOCAL);
                lil_free_value(varvalues[i]);
            }
        }
        lil_free_list(invars);
        if (outvars) lil_free_list(outvars);
        free(varvalues);
    }
    return r;
}

static LILCALLBACK lil_value_t fnc_jaileval(lil_t lil, size_t argc, lil_value_t* argv)
{
    size_t i;
    lil_t sublil;
    lil_value_t r;
    size_t base = 0;
    if (!argc) return NULL;
    if (!strcmp(lil_to_string(argv[0]), "clean")) {
        base = 1;
        if (argc == 1) return NULL;
    }
    sublil = lil_new();
    if (base != 1) {
        for (i=lil->syscmds; i<lil->cmds; i++) {
            lil_func_t fnc = lil->cmd[i];
            if (!fnc->proc) continue;
            lil_register(sublil, fnc->name, fnc->proc);
        }
    }
    r = lil_parse_value(sublil, argv[base], 1);
    lil_free(sublil);
    return r;
}

static LILCALLBACK lil_value_t fnc_count(lil_t lil, size_t argc, lil_value_t* argv)
{
    lil_list_t list;
    char buff[64];
    if (!argc) return alloc_value("0");
    list = lil_subst_to_list(lil, argv[0]);
    sprintf(buff, "%u", (unsigned int)list->c);
    lil_free_list(list);
    return alloc_value(buff);
}

static LILCALLBACK lil_value_t fnc_index(lil_t lil, size_t argc, lil_value_t* argv)
{
    lil_list_t list;
    size_t index;
    lil_value_t r;
    if (argc < 2) return NULL;
    list = lil_subst_to_list(lil, argv[0]);
    index = (size_t)lil_to_integer(argv[1]);
    if (index >= list->c)
        r = NULL;
    else
        r = lil_clone_value(list->v[index]);
    lil_free_list(list);
    return r;
}

static LILCALLBACK lil_value_t fnc_indexof(lil_t lil, size_t argc, lil_value_t* argv)
{
    lil_list_t list;
    size_t index;
    lil_value_t r = NULL;
    if (argc < 2) return NULL;
    list = lil_subst_to_list(lil, argv[0]);
    for (index = 0; index < list->c; index++)
        if (!strcmp(lil_to_string(list->v[index]), lil_to_string(argv[1]))) {
            r = lil_alloc_integer(index);
            break;
        }
    lil_free_list(list);
    return r;
}

static LILCALLBACK lil_value_t fnc_append(lil_t lil, size_t argc, lil_value_t* argv)
{
    lil_list_t list;
    lil_value_t r;
    size_t i, base = 1;
    int access = LIL_SETVAR_LOCAL;
    const char* varname;
    if (argc < 2) return NULL;
    varname = lil_to_string(argv[0]);
    if (!strcmp(varname, "global")) {
        if (argc < 3) return NULL;
        varname = lil_to_string(argv[1]);
        base = 2;
        access = LIL_SETVAR_GLOBAL;
    }
    list = lil_subst_to_list(lil, lil_get_var(lil, varname));
    for (i=base; i<argc; i++)
        lil_list_append(list, lil_clone_value(argv[i]));
    r = lil_list_to_value(list, 1);
    lil_free_list(list);
    lil_set_var(lil, varname, r, access);
    return r;
}

static LILCALLBACK lil_value_t fnc_slice(lil_t lil, size_t argc, lil_value_t* argv)
{
    lil_list_t list, slice;
    size_t i;
    lilint_t from, to;
    lil_value_t r;
    if (argc < 1) return NULL;
    if (argc < 2) return lil_clone_value(argv[0]);
    from = lil_to_integer(argv[1]);
    if (from < 0) from = 0;
    list = lil_subst_to_list(lil, argv[0]);
    to = argc > 2 ? lil_to_integer(argv[2]) : (lilint_t)list->c;
    if (to > (lilint_t)list->c) to = list->c;
    if (to < from) to = from;
    slice = lil_alloc_list();
    for (i=(size_t)from; i<(size_t)to; i++)
        lil_list_append(slice, lil_clone_value(list->v[i]));
    lil_free_list(list);
    r = lil_list_to_value(slice, 1);
    lil_free_list(slice);
    return r;
}

static LILCALLBACK lil_value_t fnc_filter(lil_t lil, size_t argc, lil_value_t* argv)
{
    lil_list_t list, filtered;
    size_t i;
    lil_value_t r;
    const char* varname = "x";
    int base = 0;
    if (argc < 1) return NULL;
    if (argc < 2) return lil_clone_value(argv[0]);
    if (argc > 2) {
        base = 1;
        varname = lil_to_string(argv[0]);
    }
    list = lil_subst_to_list(lil, argv[base]);
    filtered = lil_alloc_list();
    for (i=0; i<list->c && !lil->env->breakrun; i++) {
        lil_set_var(lil, varname, list->v[i], LIL_SETVAR_LOCAL_ONLY);
        r = lil_eval_expr(lil, argv[base + 1]);
        if (lil_to_boolean(r))
            lil_list_append(filtered, lil_clone_value(list->v[i]));
        lil_free_value(r);
    }
    lil_free_list(list);
    r = lil_list_to_value(filtered, 1);
    lil_free_list(filtered);
    return r;
}

static LILCALLBACK lil_value_t fnc_list(lil_t lil, size_t argc, lil_value_t* argv)
{
    lil_list_t list = lil_alloc_list();
    lil_value_t r;
    size_t i;
    for (i=0; i<argc; i++)
        lil_list_append(list, lil_clone_value(argv[i]));
    r = lil_list_to_value(list, 1);
    lil_free_list(list);
    return r;
}

static LILCALLBACK lil_value_t fnc_subst(lil_t lil, size_t argc, lil_value_t* argv)
{
    if (argc < 1) return NULL;
    return lil_subst_to_value(lil, argv[0]);
}

static LILCALLBACK lil_value_t fnc_concat(lil_t lil, size_t argc, lil_value_t* argv)
{
    lil_list_t list;
    lil_value_t r, tmp;
    size_t i;
    if (argc < 1) return NULL;
    r = lil_alloc_string("");
    for (i=0; i<argc; i++) {
        list = lil_subst_to_list(lil, argv[i]);
        tmp = lil_list_to_value(list, 1);
        lil_free_list(list);
        lil_append_val(r, tmp);
        lil_free_value(tmp);
    }
    return r;
}

static LILCALLBACK lil_value_t fnc_foreach(lil_t lil, size_t argc, lil_value_t* argv)
{
    lil_list_t list, rlist;
    lil_value_t r;
    size_t i, listidx = 0, codeidx = 1;
    const char* varname = "i";
    if (argc < 2) return NULL;
    if (argc >= 3) {
        varname = lil_to_string(argv[0]);
        listidx = 1;
        codeidx = 2;
    }
    rlist = lil_alloc_list();
    list = lil_subst_to_list(lil, argv[listidx]);
    for (i=0; i<list->c; i++) {
        lil_value_t rv;
        lil_set_var(lil, varname, list->v[i], LIL_SETVAR_LOCAL_ONLY);
        rv = lil_parse_value(lil, argv[codeidx], 0);
        if (rv->l) lil_list_append(rlist, rv);
        else lil_free_value(rv);
        if (lil->env->breakrun || lil->error) break;
    }
    r = lil_list_to_value(rlist, 1);
    lil_free_list(list);
    lil_free_list(rlist);
    return r;
}

static LILCALLBACK lil_value_t fnc_return(lil_t lil, size_t argc, lil_value_t* argv)
{
    lil->env->breakrun = 1;
    lil_free_value(lil->env->retval);
    lil->env->retval = argc < 1 ? NULL : lil_clone_value(argv[0]);
    lil->env->retval_set = 1;
    return argc < 1 ? NULL : lil_clone_value(argv[0]);
}

static LILCALLBACK lil_value_t fnc_result(lil_t lil, size_t argc, lil_value_t* argv)
{
    if (argc > 0) {
        lil_free_value(lil->env->retval);
        lil->env->retval = lil_clone_value(argv[0]);
        lil->env->retval_set = 1;
    }
    return lil->env->retval_set ? lil_clone_value(lil->env->retval) : NULL;
}

static LILCALLBACK lil_value_t fnc_expr(lil_t lil, size_t argc, lil_value_t* argv)
{
    if (argc == 1) return lil_eval_expr(lil, argv[0]);
    if (argc > 1) {
        lil_value_t val = alloc_value(NULL), r;
        size_t i;
        for (i=0; i<argc; i++) {
            if (i) lil_append_char(val, ' ');
            lil_append_val(val, argv[i]);
        }
        r = lil_eval_expr(lil, val);
        lil_free_value(val);
        return r;
    }
    return NULL;
}

static lil_value_t real_inc(lil_t lil, const char* varname, float v)
{
    lil_value_t pv = lil_get_var(lil, varname);
    double dv = lil_to_double(pv) + v;
    if (fmod(dv, 1))
        pv = lil_alloc_double(dv);
    else
        pv = lil_alloc_integer(lil_to_integer(pv) + v);
    lil_set_var(lil, varname, pv, LIL_SETVAR_LOCAL);
    return pv;
}

static LILCALLBACK lil_value_t fnc_inc(lil_t lil, size_t argc, lil_value_t* argv)
{
    if (argc < 1) return NULL;
    return real_inc(lil, lil_to_string(argv[0]), argc > 1 ? lil_to_double(argv[1]) : 1);
}

static LILCALLBACK lil_value_t fnc_dec(lil_t lil, size_t argc, lil_value_t* argv)
{
    if (argc < 1) return NULL;
    return real_inc(lil, lil_to_string(argv[0]), -(argc > 1 ? lil_to_double(argv[1]) : 1));
}

static LILCALLBACK lil_value_t fnc_read(lil_t lil, size_t argc, lil_value_t* argv)
{
    FILE* f;
    size_t size;
    char* buffer;
    lil_value_t r;
    if (argc < 1) return NULL;
    if (lil->callback[LIL_CALLBACK_READ]) {
        lil_read_callback_proc_t proc = (lil_read_callback_proc_t) lil->callback[LIL_CALLBACK_READ];
        buffer = proc(lil, lil_to_string(argv[0]));
    } else {
        f = fopen(lil_to_string(argv[0]), "rb");
        if (!f) return NULL;
        fseek(f, 0, SEEK_END);
        size = ftell(f);
        fseek(f, 0, SEEK_SET);
        buffer = malloc(size + 1);
        fread(buffer, 1, size, f);
        buffer[size] = 0;
        fclose(f);
    }
    r = lil_alloc_string(buffer);
    free(buffer);
    return r;
}

static LILCALLBACK lil_value_t fnc_store(lil_t lil, size_t argc, lil_value_t* argv)
{
    FILE* f;
    const char* buffer;
    if (argc < 2) return NULL;
    if (lil->callback[LIL_CALLBACK_STORE]) {
        lil_store_callback_proc_t proc = (lil_store_callback_proc_t)lil->callback[LIL_CALLBACK_STORE];
        proc(lil, lil_to_string(argv[0]), lil_to_string(argv[1]));
    } else {
        f = fopen(lil_to_string(argv[0]), "wb");
        if (!f) return NULL;
        buffer = lil_to_string(argv[1]);
        fwrite(buffer, 1, strlen(buffer), f);
        fclose(f);
    }
    return lil_clone_value(argv[1]);
}

static LILCALLBACK lil_value_t fnc_if(lil_t lil, size_t argc, lil_value_t* argv)
{
    lil_value_t val, r = NULL;
    int base = 0, not = 0, v;
    if (argc < 1) return NULL;
    if (!strcmp(lil_to_string(argv[0]), "not")) base = not = 1;
    if (argc < (size_t)base + 2) return NULL;
    val = lil_eval_expr(lil, argv[base]);
    if (!val || lil->error) return NULL;
    v = lil_to_boolean(val);
    if (not) v = !v;
    if (v) {
        r = lil_parse_value(lil, argv[base + 1], 0);
    } else if (argc > (size_t)base + 2) {
        r = lil_parse_value(lil, argv[base + 2], 0);
    }
    lil_free_value(val);
    return r;
}

static LILCALLBACK lil_value_t fnc_while(lil_t lil, size_t argc, lil_value_t* argv)
{
    lil_value_t val, r = NULL;
    int base = 0, not = 0, v;
    if (argc < 1) return NULL;
    if (!strcmp(lil_to_string(argv[0]), "not")) base = not = 1;
    if (argc < (size_t)base + 2) return NULL;
    while (!lil->error && !lil->env->breakrun) {
        val = lil_eval_expr(lil, argv[base]);
        if (!val || lil->error) return NULL;
        v = lil_to_boolean(val);
        if (not) v = !v;
        if (!v) {
            lil_free_value(val);
            break;
        }
        if (r) lil_free_value(r);
        r = lil_parse_value(lil, argv[base + 1], 0);
        lil_free_value(val);
    }
    return r;
}

static LILCALLBACK lil_value_t fnc_for(lil_t lil, size_t argc, lil_value_t* argv)
{
    lil_value_t val, r = NULL;
    if (argc < 4) return NULL;
    lil_free_value(lil_parse_value(lil, argv[0], 0));
    while (!lil->error && !lil->env->breakrun) {
        val = lil_eval_expr(lil, argv[1]);
        if (!val || lil->error) return NULL;
        if (!lil_to_boolean(val)) {
            lil_free_value(val);
            break;
        }
        if (r) lil_free_value(r);
        r = lil_parse_value(lil, argv[3], 0);
        lil_free_value(val);
        lil_free_value(lil_parse_value(lil, argv[2], 0));
    }
    return r;
}


static LILCALLBACK lil_value_t fnc_char(lil_t lil, size_t argc, lil_value_t* argv)
{
    char s[2];
    if (!argc) return NULL;
    s[0] = (char)lil_to_integer(argv[0]);
    s[1] = 0;
    return lil_alloc_string(s);
}

static LILCALLBACK lil_value_t fnc_charat(lil_t lil, size_t argc, lil_value_t* argv)
{
    size_t index;
    char chstr[2];
    const char* str;
    if (argc < 2) return NULL;
    str = lil_to_string(argv[0]);
    index = (size_t)lil_to_integer(argv[1]);
    if (index >= strlen(str)) return NULL;
    chstr[0] = str[index];
    chstr[1] = 0;
    return lil_alloc_string(chstr);
}

static LILCALLBACK lil_value_t fnc_codeat(lil_t lil, size_t argc, lil_value_t* argv)
{
    size_t index;
    const char* str;
    if (argc < 2) return NULL;
    str = lil_to_string(argv[0]);
    index = (size_t)lil_to_integer(argv[1]);
    if (index >= strlen(str)) return NULL;
    return lil_alloc_integer(str[index]);
}

static LILCALLBACK lil_value_t fnc_substr(lil_t lil, size_t argc, lil_value_t* argv)
{
    const char* str;
    lil_value_t r;
    size_t start, end, i, slen;
    if (argc < 2) return NULL;
    str = lil_to_string(argv[0]);
    if (!str[0]) return NULL;
    slen = strlen(str);
    start = (size_t)atoll(lil_to_string(argv[1]));
    end = argc > 2 ? (size_t)atoll(lil_to_string(argv[2])) : slen;
    if (end > slen) end = slen;
    if (start >= end) return NULL;
    r = lil_alloc_string("");
    for (i=start; i<end; i++)
        lil_append_char(r, str[i]);
    return r;
}

static LILCALLBACK lil_value_t fnc_strpos(lil_t lil, size_t argc, lil_value_t* argv)
{
    const char* hay;
    const char* str;
    size_t min = 0;
    if (argc < 2) return lil_alloc_integer(-1);
    hay = lil_to_string(argv[0]);
    if (argc > 2) {
        min = (size_t)atoll(lil_to_string(argv[2]));
        if (min >= strlen(hay)) return lil_alloc_integer(-1);
    }
    str = strstr(hay + min, lil_to_string(argv[1]));
    if (!str) return lil_alloc_integer(-1);
    return lil_alloc_integer(str - hay);
}

static LILCALLBACK lil_value_t fnc_length(lil_t lil, size_t argc, lil_value_t* argv)
{
    size_t i, total = 0;
    for (i=0; i<argc; i++) {
        if (i) total++;
        total += strlen(lil_to_string(argv[i]));
    }
    return lil_alloc_integer((lilint_t)total);
}

static lil_value_t real_trim(const char* str, const char* chars, int left, int right)
{
    int base = 0;
    lil_value_t r = NULL;
    if (left) {
        while (str[base] && strchr(chars, str[base])) base++;
        if (!right) r = lil_alloc_string(str[base] ? str + base : NULL);
    }
    if (right) {
        size_t len;
        char* s;
        s = strclone(str + base);
        len = strlen(s);
        while (len && strchr(chars, s[len - 1])) len--;
        s[len] = 0;
        r = lil_alloc_string(s);
        free(s);
    }
    return r;
}

static LILCALLBACK lil_value_t fnc_trim(lil_t lil, size_t argc, lil_value_t* argv)
{
    if (!argc) return NULL;
    return real_trim(lil_to_string(argv[0]), argc < 2 ? " \f\n\r\t\v" : lil_to_string(argv[1]), 1, 1);
}

static LILCALLBACK lil_value_t fnc_ltrim(lil_t lil, size_t argc, lil_value_t* argv)
{
    if (!argc) return NULL;
    return real_trim(lil_to_string(argv[0]), argc < 2 ? " \f\n\r\t\v" : lil_to_string(argv[1]), 1, 0);
}

static LILCALLBACK lil_value_t fnc_rtrim(lil_t lil, size_t argc, lil_value_t* argv)
{
    if (!argc) return NULL;
    return real_trim(lil_to_string(argv[0]), argc < 2 ? " \f\n\r\t\v" : lil_to_string(argv[1]), 0, 1);
}

static LILCALLBACK lil_value_t fnc_strcmp(lil_t lil, size_t argc, lil_value_t* argv)
{
    if (argc < 2) return NULL;
    return lil_alloc_integer(strcmp(lil_to_string(argv[0]), lil_to_string(argv[1])));
}

static LILCALLBACK lil_value_t fnc_streq(lil_t lil, size_t argc, lil_value_t* argv)
{
    if (argc < 2) return NULL;
    return lil_alloc_integer(strcmp(lil_to_string(argv[0]), lil_to_string(argv[1]))?0:1);
}

static LILCALLBACK lil_value_t fnc_repstr(lil_t lil, size_t argc, lil_value_t* argv)
{
    const char* from;
    const char* to;
    char* src;
    const char* sub;
    size_t idx;
    size_t fromlen;
    size_t tolen;
    size_t srclen;
    lil_value_t r;
    if (argc < 1) return NULL;
    if (argc < 3) return lil_clone_value(argv[0]);
    from = lil_to_string(argv[1]);
    to = lil_to_string(argv[2]);
    if (!from[0]) return NULL;
    src = strclone(lil_to_string(argv[0]));
    srclen = strlen(src);
    fromlen = strlen(from);
    tolen = strlen(to);
    while ((sub = strstr(src, from))) {
        char* newsrc = malloc(srclen - fromlen + tolen + 1);
        idx = sub - src;
        if (idx) memcpy(newsrc, src, idx);
        memcpy(newsrc + idx, to, tolen);
        memcpy(newsrc + idx + tolen, src + idx + fromlen, srclen - idx - fromlen);
        srclen = srclen - fromlen + tolen;
        free(src);
        src = newsrc;
        src[srclen] = 0;
    }
    r = lil_alloc_string(src);
    free(src);
    return r;
}

static LILCALLBACK lil_value_t fnc_split(lil_t lil, size_t argc, lil_value_t* argv)
{
    lil_list_t list;
    const char* sep = " ";
    size_t i;
    lil_value_t val;
    const char* str;
    if (argc == 0) return NULL;
    if (argc > 1) {
        sep = lil_to_string(argv[1]);
        if (!sep || !sep[0]) return lil_clone_value(argv[0]);
    }
    val = lil_alloc_string("");
    str = lil_to_string(argv[0]);
    list = lil_alloc_list();
    for (i=0; str[i]; i++) {
        if (strchr(sep, str[i])) {
            lil_list_append(list, val);
            val = lil_alloc_string("");
        } else {
            lil_append_char(val, str[i]);
        }
    }
    lil_list_append(list, val);
    val = lil_list_to_value(list, 1);
    lil_free_list(list);
    return val;
}

static LILCALLBACK lil_value_t fnc_try(lil_t lil, size_t argc, lil_value_t* argv)
{
    lil_value_t r;
    if (argc < 1) return NULL;
    if (lil->error) return NULL;
    r = lil_parse_value(lil, argv[0], 0);
    if (lil->error) {
        lil->error = ERROR_NOERROR;
        lil_free_value(r);
        if (argc > 1) r = lil_parse_value(lil, argv[1], 0);
        else r = 0;
    }
    return r;
}

static LILCALLBACK lil_value_t fnc_error(lil_t lil, size_t argc, lil_value_t* argv)
{
    lil_set_error(lil, argc > 0 ? lil_to_string(argv[0]) : NULL);
    return NULL;
}

static LILCALLBACK lil_value_t fnc_exit(lil_t lil, size_t argc, lil_value_t* argv)
{
    if (lil->callback[LIL_CALLBACK_EXIT]) {
        lil_exit_callback_proc_t proc = (lil_exit_callback_proc_t)lil->callback[LIL_CALLBACK_EXIT];
        proc(lil, argc > 0 ? argv[0] : NULL);
    }
    return NULL;
}

static LILCALLBACK lil_value_t fnc_source(lil_t lil, size_t argc, lil_value_t* argv)
{
    FILE* f;
    size_t size;
    char* buffer;
    lil_value_t r;
    if (argc < 1) return NULL;
    if (lil->callback[LIL_CALLBACK_SOURCE]) {
        lil_source_callback_proc_t proc = (lil_source_callback_proc_t)lil->callback[LIL_CALLBACK_SOURCE];
        buffer = proc(lil, lil_to_string(argv[0]));
    } else if (lil->callback[LIL_CALLBACK_READ]) {
        lil_read_callback_proc_t proc = (lil_read_callback_proc_t)lil->callback[LIL_CALLBACK_READ];
        buffer = proc(lil, lil_to_string(argv[0]));
    } else {
        f = fopen(lil_to_string(argv[0]), "rb");
        if (!f) return NULL;
        fseek(f, 0, SEEK_END);
        size = ftell(f);
        fseek(f, 0, SEEK_SET);
        buffer = malloc(size + 1);
        fread(buffer, 1, size, f);
        buffer[size] = 0;
        fclose(f);
    }
    r = lil_parse(lil, buffer, 0, 0);
    free(buffer);
    return r;
}

static LILCALLBACK lil_value_t fnc_lmap(lil_t lil, size_t argc, lil_value_t* argv)
{
    lil_list_t list;
    size_t i;
    if (argc < 2) return NULL;
    list = lil_subst_to_list(lil, argv[0]);
    for (i=1; i<argc; i++)
        lil_set_var(lil, lil_to_string(argv[i]), lil_list_get(list, i - 1), LIL_SETVAR_LOCAL);
    lil_free_list(list);
    return NULL;
}

static LILCALLBACK lil_value_t fnc_rand(lil_t lil, size_t argc, lil_value_t* argv)
{
    return lil_alloc_double(rand()/(double)RAND_MAX);
}

static LILCALLBACK lil_value_t fnc_catcher(lil_t lil, size_t argc, lil_value_t* argv)
{
    if (argc == 0) {
        return lil_alloc_string(lil->catcher);
    } else {
        const char* catcher = lil_to_string(argv[0]);
        free(lil->catcher);
        lil->catcher = catcher[0] ? strclone(catcher) : NULL;
    }
    return NULL;
}

static void register_stdcmds(lil_t lil)
{
    lil_register(lil, "reflect", fnc_reflect);
    lil_register(lil, "func", fnc_func);
    lil_register(lil, "rename", fnc_rename);
    lil_register(lil, "unusedname", fnc_unusedname);
    lil_register(lil, "quote", fnc_quote);
    lil_register(lil, "set", fnc_set);
    lil_register(lil, "local", fnc_local);
    lil_register(lil, "write", fnc_write);
    lil_register(lil, "print", fnc_print);
    lil_register(lil, "eval", fnc_eval);
    lil_register(lil, "topeval", fnc_topeval);
    lil_register(lil, "upeval", fnc_upeval);
    lil_register(lil, "downeval", fnc_downeval);
    lil_register(lil, "enveval", fnc_enveval);
    lil_register(lil, "jaileval", fnc_jaileval);
    lil_register(lil, "count", fnc_count);
    lil_register(lil, "index", fnc_index);
    lil_register(lil, "indexof", fnc_indexof);
    lil_register(lil, "filter", fnc_filter);
    lil_register(lil, "list", fnc_list);
    lil_register(lil, "append", fnc_append);
    lil_register(lil, "slice", fnc_slice);
    lil_register(lil, "subst", fnc_subst);
    lil_register(lil, "concat", fnc_concat);
    lil_register(lil, "foreach", fnc_foreach);
    lil_register(lil, "return", fnc_return);
    lil_register(lil, "result", fnc_result);
    lil_register(lil, "expr", fnc_expr);
    lil_register(lil, "inc", fnc_inc);
    lil_register(lil, "dec", fnc_dec);
    lil_register(lil, "read", fnc_read);
    lil_register(lil, "store", fnc_store);
    lil_register(lil, "if", fnc_if);
    lil_register(lil, "while", fnc_while);
    lil_register(lil, "for", fnc_for);
    lil_register(lil, "char", fnc_char);
    lil_register(lil, "charat", fnc_charat);
    lil_register(lil, "codeat", fnc_codeat);
    lil_register(lil, "substr", fnc_substr);
    lil_register(lil, "strpos", fnc_strpos);
    lil_register(lil, "length", fnc_length);
    lil_register(lil, "trim", fnc_trim);
    lil_register(lil, "ltrim", fnc_ltrim);
    lil_register(lil, "rtrim", fnc_rtrim);
    lil_register(lil, "strcmp", fnc_strcmp);
    lil_register(lil, "streq", fnc_streq);
    lil_register(lil, "repstr", fnc_repstr);
    lil_register(lil, "split", fnc_split);
    lil_register(lil, "try", fnc_try);
    lil_register(lil, "error", fnc_error);
    lil_register(lil, "exit", fnc_exit);
    lil_register(lil, "source", fnc_source);
    lil_register(lil, "lmap", fnc_lmap);
    lil_register(lil, "rand", fnc_rand);
    lil_register(lil, "catcher", fnc_catcher);
    lil->syscmds = lil->cmds;
}
