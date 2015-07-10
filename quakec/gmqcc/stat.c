/*
 * Copyright (C) 2012, 2013, 2014, 2015
 *     Dale Weiler
 *     Wolfgang Bumiller
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include <string.h>
#include <stdlib.h>

#include "gmqcc.h"

typedef struct stat_mem_block_s stat_mem_block_t;

#define IDENT_SIZE 4
#define IDENT_VEC  "vec"
#define IDENT_MEM  "mem"
#define IDENT_VEC_TOP (sizeof(vector_t) + IDENT_SIZE)
#define IDENT_MEM_TOP (sizeof(stat_mem_block_t) + IDENT_SIZE)

/*
 * For the valgrind integration of our allocator. This allows us to have
 * more `accurate` valgrind output for our allocator, and also secures the
 * possible underflows (where one could obtain access to the redzone that
 * represents info about that allocation).
 */
#ifndef NVALGRIND
#   include <valgrind/valgrind.h>
#   include <valgrind/memcheck.h>
#else
#   define VALGRIND_MALLOCLIKE_BLOCK(PTR, ALLOC_SIZE, REDZONE_SIZE, ZEROED)
#   define VALGRIND_FREELIKE_BLOCK(PTR, REDZONE_SIZE)
#   define VALGRIND_MAKE_MEM_DEFINED(PTR, REDZONE_SIZE)
#   define VALGRIND_MAKE_MEM_NOACCESS(PTR, REDZONE_SIZE)
#endif

/*
 * GMQCC performs tons of allocations, constructions, and crazyness
 * all around. When trying to optimizes systems, or just get fancy
 * statistics out of the compiler, it's often printf mess. This file
 * implements the statistics system of the compiler. I.E the allocator
 * we use to track allocations, and other systems of interest.
 */
#define ST_SIZE 1024

struct stat_mem_block_s {
    const char              *file;
    size_t                   line;
    size_t                   size;
    const char              *expr;
    struct stat_mem_block_s *next;
    struct stat_mem_block_s *prev;
};

typedef struct {
    size_t key;
    size_t value;
} stat_size_entry_t, **stat_size_table_t;

static uint64_t          stat_mem_allocated         = 0;
static uint64_t          stat_mem_deallocated       = 0;
static uint64_t          stat_mem_allocated_total   = 0;
static uint64_t          stat_mem_deallocated_total = 0;
static uint64_t          stat_mem_high              = 0;
static uint64_t          stat_mem_peak              = 0;
static uint64_t          stat_mem_strdups           = 0;
static uint64_t          stat_used_strdups          = 0;
static uint64_t          stat_used_vectors          = 0;
static uint64_t          stat_used_hashtables       = 0;
static uint64_t          stat_type_vectors          = 0;
static uint64_t          stat_type_hashtables       = 0;
static stat_size_table_t stat_size_vectors          = NULL;
static stat_size_table_t stat_size_hashtables       = NULL;
static stat_mem_block_t *stat_mem_block_root        = NULL;

/*
 * A tiny size_t key-value hashtbale for tracking vector and hashtable
 * sizes. We can use it for other things too, if we need to. This is
 * very TIGHT, and efficent in terms of space though.
 */
static stat_size_table_t stat_size_new(void) {
    return (stat_size_table_t)memset(
        mem_a(sizeof(stat_size_entry_t*) * ST_SIZE),
        0, ST_SIZE * sizeof(stat_size_entry_t*)
    );
}

static void stat_size_del(stat_size_table_t table) {
    size_t i = 0;
    for (; i < ST_SIZE; i++) if(table[i]) mem_d(table[i]);
    mem_d(table);
}

static stat_size_entry_t *stat_size_get(stat_size_table_t table, size_t key) {
    size_t hash = (key % ST_SIZE);
    while (table[hash] && table[hash]->key != key)
        hash = (hash + 1) % ST_SIZE;
    return table[hash];
}
static void stat_size_put(stat_size_table_t table, size_t key, size_t value) {
    size_t hash = (key % ST_SIZE);
    while (table[hash] && table[hash]->key != key)
        hash = (hash + 1) % ST_SIZE;
    table[hash]        = (stat_size_entry_t*)mem_a(sizeof(stat_size_entry_t));
    table[hash]->key   = key;
    table[hash]->value = value;
}

/*
 * A basic header of information wrapper allocator. Simply stores
 * information as a header, returns the memory + 1 past it, can be
 * retrieved again with - 1. Where type is stat_mem_block_t*.
 */
void *stat_mem_allocate(size_t size, size_t line, const char *file, const char *expr) {
    stat_mem_block_t *info = (stat_mem_block_t*)malloc(size + IDENT_MEM_TOP);
    void             *data = (void *)((char*)info + IDENT_MEM_TOP);

    if(GMQCC_UNLIKELY(!info))
        return NULL;

    info->line = line;
    info->size = size;
    info->file = file;
    info->expr = expr;
    info->prev = NULL;
    info->next = stat_mem_block_root;

    /* Write identifier */
    memcpy(info + 1, IDENT_MEM, IDENT_SIZE);

    /* likely since it only happens once */
    if (GMQCC_LIKELY(stat_mem_block_root != NULL)) {
        VALGRIND_MAKE_MEM_DEFINED(stat_mem_block_root, IDENT_MEM_TOP);
        stat_mem_block_root->prev = info;
        VALGRIND_MAKE_MEM_NOACCESS(stat_mem_block_root, IDENT_MEM_TOP);
    }

    stat_mem_block_root       = info;
    stat_mem_allocated       += size;
    stat_mem_high            += size;
    stat_mem_allocated_total ++;

    if (stat_mem_high > stat_mem_peak)
        stat_mem_peak = stat_mem_high;

    VALGRIND_MALLOCLIKE_BLOCK(data, size, IDENT_MEM_TOP, 0);
    return data;
}

void stat_mem_deallocate(void *ptr, size_t line, const char *file) {
    stat_mem_block_t *info  = NULL;
    char             *ident = (char *)ptr - IDENT_SIZE;

    if (GMQCC_UNLIKELY(!ptr))
        return;

    /* Validate usage */
    VALGRIND_MAKE_MEM_DEFINED(ident, IDENT_SIZE);
    if (!strcmp(ident, IDENT_VEC)) {
        vector_t         *vec   = (vector_t*)((char *)ptr - IDENT_VEC_TOP);
        stat_mem_block_t *block = (stat_mem_block_t*)((char *)vec - IDENT_MEM_TOP);

        VALGRIND_MAKE_MEM_DEFINED(block, sizeof(stat_mem_block_t));
        con_err("internal warning: invalid use of mem_d:\n");
        con_err("internal warning:    vector (used elements: %u, allocated elements: %u)\n",
            (unsigned)vec->used,
            (unsigned)vec->allocated
        );
        con_err("internal warning:    vector was last (re)allocated with (size: %u (bytes), at location: %s:%u)\n",
            (unsigned)block->size,
            block->file,
            (unsigned)block->line
        );
        con_err("internal warning:    released with wrong routine at %s:%u\n", file, (unsigned)line);
        con_err("internal warning:    forwarding to vec_free, please fix it\n");
        VALGRIND_MAKE_MEM_NOACCESS(block, sizeof(stat_mem_block_t));
        VALGRIND_MAKE_MEM_NOACCESS(ident, IDENT_SIZE);
        vec_free(ptr);
        return;
    }
    VALGRIND_MAKE_MEM_NOACCESS(ident, IDENT_SIZE);
    info = (stat_mem_block_t*)((char *)ptr - IDENT_MEM_TOP);

    /*
     * we need access to the redzone that represents the info block
     * so lets do that.
     */
    VALGRIND_MAKE_MEM_DEFINED(info, IDENT_MEM_TOP);

    stat_mem_deallocated       += info->size;
    stat_mem_high              -= info->size;
    stat_mem_deallocated_total ++;

    if (info->prev) {
        /* just need access for a short period */
        VALGRIND_MAKE_MEM_DEFINED(info->prev, IDENT_MEM_TOP);
        info->prev->next = info->next;
        /* don't need access anymore */
        VALGRIND_MAKE_MEM_NOACCESS(info->prev, IDENT_MEM_TOP);
    }
    if (info->next) {
        /* just need access for a short period */
        VALGRIND_MAKE_MEM_DEFINED(info->next, IDENT_MEM_TOP);
        info->next->prev = info->prev;
        /* don't need access anymore */
        VALGRIND_MAKE_MEM_NOACCESS(info->next, IDENT_MEM_TOP);
    }

    /* move ahead */
    if (info == stat_mem_block_root)
        stat_mem_block_root = info->next;

    free(info);
    VALGRIND_MAKE_MEM_NOACCESS(info, IDENT_MEM_TOP);
    VALGRIND_FREELIKE_BLOCK(ptr, IDENT_MEM_TOP);
}

void *stat_mem_reallocate(void *ptr, size_t size, size_t line, const char *file, const char *expr) {
    stat_mem_block_t *oldinfo = NULL;
    stat_mem_block_t *newinfo;

    if (GMQCC_UNLIKELY(!ptr))
        return stat_mem_allocate(size, line, file, expr);

    /* stay consistent with glibc */
    if (GMQCC_UNLIKELY(!size)) {
        stat_mem_deallocate(ptr, line, file);
        return NULL;
    }

    oldinfo = (stat_mem_block_t*)((char *)ptr - IDENT_MEM_TOP);
    newinfo = (stat_mem_block_t*)malloc(size + IDENT_MEM_TOP);

    if (GMQCC_UNLIKELY(!newinfo)) {
        stat_mem_deallocate(ptr, line, file);
        return NULL;
    }

    VALGRIND_MALLOCLIKE_BLOCK((char *)newinfo + IDENT_MEM_TOP, size, IDENT_MEM_TOP, 0);

    /* we need access to the old info redzone */
    VALGRIND_MAKE_MEM_DEFINED(oldinfo, IDENT_MEM_TOP);

    /* We need access to the new info redzone */
    VALGRIND_MAKE_MEM_DEFINED(newinfo, IDENT_MEM_TOP);
    memcpy((char *)(newinfo + 1), IDENT_MEM, IDENT_SIZE);
    memcpy((char *)newinfo + IDENT_MEM_TOP, (char *)oldinfo + IDENT_MEM_TOP, oldinfo->size);
    VALGRIND_MAKE_MEM_NOACCESS(newinfo, IDENT_MEM_TOP);

    if (oldinfo->prev) {
        /* just need access for a short period */
        VALGRIND_MAKE_MEM_DEFINED(oldinfo->prev, IDENT_MEM_TOP);
        oldinfo->prev->next = oldinfo->next;
        /* don't need access anymore */
        VALGRIND_MAKE_MEM_NOACCESS(oldinfo->prev, IDENT_MEM_TOP);
    }

    if (oldinfo->next) {
        /* just need access for a short period */
        VALGRIND_MAKE_MEM_DEFINED(oldinfo->next, IDENT_MEM_TOP);
        oldinfo->next->prev = oldinfo->prev;
        /* don't need access anymore */
        VALGRIND_MAKE_MEM_NOACCESS(oldinfo->next, IDENT_MEM_TOP);
    }

    /* move ahead */
    if (oldinfo == stat_mem_block_root)
        stat_mem_block_root = oldinfo->next;

    /* we need access to the redzone for the newinfo block */
    VALGRIND_MAKE_MEM_DEFINED(newinfo, IDENT_MEM_TOP);

    newinfo->line = line;
    newinfo->size = size;
    newinfo->file = file;
    newinfo->expr = expr;
    newinfo->prev = NULL;
    newinfo->next = stat_mem_block_root;

    /*
     * likely since the only time there is no root is when it's
     * being initialized first.
     */
    if (GMQCC_LIKELY(stat_mem_block_root != NULL)) {
        /* we need access to the root */
        VALGRIND_MAKE_MEM_DEFINED(stat_mem_block_root, IDENT_MEM_TOP);
        stat_mem_block_root->prev = newinfo;
        /* kill access */
        VALGRIND_MAKE_MEM_NOACCESS(stat_mem_block_root, IDENT_MEM_TOP);
    }

    stat_mem_block_root = newinfo;
    stat_mem_allocated -= oldinfo->size;
    stat_mem_high      -= oldinfo->size;
    stat_mem_allocated += newinfo->size;
    stat_mem_high      += newinfo->size;

    /*
     * we're finished with the redzones, lets kill the access
     * to them.
     */
    VALGRIND_MAKE_MEM_NOACCESS(newinfo, IDENT_MEM_TOP);
    VALGRIND_MAKE_MEM_NOACCESS(oldinfo, IDENT_MEM_TOP);

    if (stat_mem_high > stat_mem_peak)
        stat_mem_peak = stat_mem_high;

    free(oldinfo);
    VALGRIND_FREELIKE_BLOCK(ptr, IDENT_MEM_TOP);
    return (char *)newinfo + IDENT_MEM_TOP;
}

/*
 * strdup does it's own malloc, we need to track malloc. We don't want
 * to overwrite malloc though, infact, we can't really hook it at all
 * without library specific assumptions. So we re implement strdup.
 */
char *stat_mem_strdup(const char *src, size_t line, const char *file, bool empty) {
    size_t len = 0;
    char  *ptr = NULL;

    if (!src)
        return NULL;

    len = strlen(src);
    if (((!empty) ? len : true) && (ptr = (char*)stat_mem_allocate(len + 1, line, file, "strdup"))) {
        memcpy(ptr, src, len);
        ptr[len] = '\0';
    }

    stat_used_strdups ++;
    stat_mem_strdups  += len;
    return ptr;
}

/*
 * The reallocate function for resizing vectors.
 */
void _util_vec_grow(void **a, size_t i, size_t s) {
    vector_t          *d = (vector_t*)((char *)*a - IDENT_VEC_TOP);
    size_t             m = 0;
    stat_size_entry_t *e = NULL;
    void              *p = NULL;

    if (*a) {
        m = 2 * d->allocated + i;
        p = mem_r(d, s * m + IDENT_VEC_TOP);
    } else {
        m = i + 1;
        p = mem_a(s * m + IDENT_VEC_TOP);
        ((vector_t*)p)->used = 0;
        stat_used_vectors++;
    }

    if (!stat_size_vectors)
        stat_size_vectors = stat_size_new();

    if ((e = stat_size_get(stat_size_vectors, s))) {
        e->value ++;
    } else {
        stat_size_put(stat_size_vectors, s, 1); /* start off with 1 */
        stat_type_vectors++;
    }

    d = (vector_t*)p;
    d->allocated = m;
    memcpy(d + 1, IDENT_VEC, IDENT_SIZE);
    *a = (void *)((char *)d + IDENT_VEC_TOP);
}

void _util_vec_delete(void *data, size_t line, const char *file) {
    char *ident = (char *)data - IDENT_SIZE;
    if (!strcmp(ident, IDENT_MEM)) {
        stat_mem_block_t *block = (stat_mem_block_t*)((char *)data - IDENT_MEM_TOP);
        VALGRIND_MAKE_MEM_DEFINED(block, sizeof(stat_mem_block_t));
        con_err("internal warning: invalid use of vec_free:\n");
        con_err("internal warning:    memory block last allocated (size: %u (bytes), at %s:%u)\n",
            (unsigned)block->size,
            block->file,
            (unsigned)block->line);
        con_err("internal warning:    released with with wrong routine at %s:%u\n", file, (unsigned)line);
        con_err("internal warning:    forwarding to mem_d, please fix it\n");
        VALGRIND_MAKE_MEM_NOACCESS(block, sizeof(stat_mem_block_t));
        mem_d(data);
        return;
    }
    /* forward */
    stat_mem_deallocate((void*)(ident - sizeof(vector_t)), line, file);
}

/*
 * Hash table for generic data, based on dynamic memory allocations
 * all around.  This is the internal interface, please look for
 * EXPOSED INTERFACE comment below
 */
typedef struct hash_node_t {
    char               *key;   /* the key for this node in table */
    void               *value; /* pointer to the data as void*   */
    struct hash_node_t *next;  /* next node (linked list)        */
} hash_node_t;


size_t hash(const char *key);
size_t util_hthash(hash_table_t *ht, const char *key) {
    return hash(key) % ht->size;
}

static hash_node_t *_util_htnewpair(const char *key, void *value) {
    hash_node_t *node;
    if (!(node = (hash_node_t*)mem_a(sizeof(hash_node_t))))
        return NULL;

    if (!(node->key = util_strdupe(key))) {
        mem_d(node);
        return NULL;
    }

    node->value = value;
    node->next  = NULL;

    return node;
}

/*
 * EXPOSED INTERFACE for the hashtable implementation
 * util_htnew(size)                             -- to make a new hashtable
 * util_htset(table, key, value, sizeof(value)) -- to set something in the table
 * util_htget(table, key)                       -- to get something from the table
 * util_htdel(table)                            -- to delete the table
 */
hash_table_t *util_htnew(size_t size) {
    hash_table_t      *hashtable = NULL;
    stat_size_entry_t *find      = NULL;

    if (size < 1)
        return NULL;

    if (!stat_size_hashtables)
        stat_size_hashtables = stat_size_new();

    if (!(hashtable = (hash_table_t*)mem_a(sizeof(hash_table_t))))
        return NULL;

    if (!(hashtable->table = (hash_node_t**)mem_a(sizeof(hash_node_t*) * size))) {
        mem_d(hashtable);
        return NULL;
    }

    if ((find = stat_size_get(stat_size_hashtables, size)))
        find->value++;
    else {
        stat_type_hashtables++;
        stat_size_put(stat_size_hashtables, size, 1);
    }

    hashtable->size = size;
    memset(hashtable->table, 0, sizeof(hash_node_t*) * size);

    stat_used_hashtables++;
    return hashtable;
}

void util_htseth(hash_table_t *ht, const char *key, size_t bin, void *value) {
    hash_node_t *newnode = NULL;
    hash_node_t *next    = NULL;
    hash_node_t *last    = NULL;

    next = ht->table[bin];

    while (next && next->key && strcmp(key, next->key) > 0)
        last = next, next = next->next;

    /* already in table, do a replace */
    if (next && next->key && strcmp(key, next->key) == 0) {
        next->value = value;
    } else {
        /* not found, grow a pair man :P */
        newnode = _util_htnewpair(key, value);
        if (next == ht->table[bin]) {
            newnode->next  = next;
            ht->table[bin] = newnode;
        } else if (!next) {
            last->next = newnode;
        } else {
            newnode->next = next;
            last->next = newnode;
        }
    }
}

void util_htset(hash_table_t *ht, const char *key, void *value) {
    util_htseth(ht, key, util_hthash(ht, key), value);
}

void *util_htgeth(hash_table_t *ht, const char *key, size_t bin) {
    hash_node_t *pair = ht->table[bin];

    while (pair && pair->key && strcmp(key, pair->key) > 0)
        pair = pair->next;

    if (!pair || !pair->key || strcmp(key, pair->key) != 0)
        return NULL;

    return pair->value;
}

void *util_htget(hash_table_t *ht, const char *key) {
    return util_htgeth(ht, key, util_hthash(ht, key));
}

void *code_util_str_htgeth(hash_table_t *ht, const char *key, size_t bin);
void *code_util_str_htgeth(hash_table_t *ht, const char *key, size_t bin) {
    hash_node_t *pair;
    size_t len, keylen;
    int cmp;

    keylen = strlen(key);

    pair = ht->table[bin];
    while (pair && pair->key) {
        len = strlen(pair->key);
        if (len < keylen) {
            pair = pair->next;
            continue;
        }
        if (keylen == len) {
            cmp = strcmp(key, pair->key);
            if (cmp == 0)
                return pair->value;
            if (cmp < 0)
                return NULL;
            pair = pair->next;
            continue;
        }
        cmp = strcmp(key, pair->key + len - keylen);
        if (cmp == 0) {
            uintptr_t up = (uintptr_t)pair->value;
            up += len - keylen;
            return (void*)up;
        }
        pair = pair->next;
    }
    return NULL;
}

/*
 * Free all allocated data in a hashtable, this is quite the amount
 * of work.
 */
void util_htrem(hash_table_t *ht, void (*callback)(void *data)) {
    size_t i = 0;

    for (; i < ht->size; ++i) {
        hash_node_t *n = ht->table[i];
        hash_node_t *p;

        /* free in list */
        while (n) {
            if (n->key)
                mem_d(n->key);
            if (callback)
                callback(n->value);
            p = n;
            n = p->next;
            mem_d(p);
        }

    }
    /* free table */
    mem_d(ht->table);
    mem_d(ht);
}

void util_htrmh(hash_table_t *ht, const char *key, size_t bin, void (*cb)(void*)) {
    hash_node_t **pair = &ht->table[bin];
    hash_node_t *tmp;

    while (*pair && (*pair)->key && strcmp(key, (*pair)->key) > 0)
        pair = &(*pair)->next;

    tmp = *pair;
    if (!tmp || !tmp->key || strcmp(key, tmp->key) != 0)
        return;

    if (cb)
        (*cb)(tmp->value);

    *pair = tmp->next;
    mem_d(tmp->key);
    mem_d(tmp);
}

void util_htrm(hash_table_t *ht, const char *key, void (*cb)(void*)) {
    util_htrmh(ht, key, util_hthash(ht, key), cb);
}

void util_htdel(hash_table_t *ht) {
    util_htrem(ht, NULL);
}

/*
 * The following functions below implement printing / dumping of statistical
 * information.
 */
static void stat_dump_mem_contents(stat_mem_block_t *block, uint16_t cols) {
    unsigned char *buffer = (unsigned char *)mem_a(cols);
    unsigned char *memory = (unsigned char *)(block + 1);
    size_t         i;

    for (i = 0; i < block->size; i++) {
        if (!(i % 16)) {
            if (i != 0)
                con_out(" %s\n", buffer);
            con_out(" 0x%08X: ", i);
        }

        con_out(" %02X", memory[i]);

        buffer[i % cols] = ((memory[i] < 0x20) || (memory[i] > 0x7E))
                            ? '.'
                            : memory[i];

        buffer[(i % cols) + 1] = '\0';
    }

    while ((i % cols) != 0) {
        con_out("   ");
        i++;
    }

    con_out(" %s\n", buffer);
    mem_d(buffer);
}

static void stat_dump_mem_leaks(void) {
    stat_mem_block_t *info;
    /* we need access to the root for this */
    VALGRIND_MAKE_MEM_DEFINED(stat_mem_block_root, sizeof(stat_mem_block_t));
    for (info = stat_mem_block_root; info; info = info->next) {
        /* we need access to the block */
        VALGRIND_MAKE_MEM_DEFINED(info, sizeof(stat_mem_block_t));
        con_out("lost: %u (bytes) at %s:%u from expression `%s`\n",
            info->size,
            info->file,
            info->line,
            info->expr
        );

        stat_dump_mem_contents(info, OPTS_OPTION_U16(OPTION_MEMDUMPCOLS));

        /*
         * we're finished with the access, the redzone should be marked
         * inaccesible so that invalid read/writes that could 'step-into'
         * those redzones will show up as invalid read/writes in valgrind.
         */
        VALGRIND_MAKE_MEM_NOACCESS(info, sizeof(stat_mem_block_t));
    }
    VALGRIND_MAKE_MEM_NOACCESS(stat_mem_block_root, sizeof(stat_mem_block_t));
}

static void stat_dump_mem_info(void) {
    con_out("Memory Information:\n\
    Total allocations:   %llu\n\
    Total deallocations: %llu\n\
    Total allocated:     %f (MB)\n\
    Total deallocated:   %f (MB)\n\
    Total peak memory:   %f (MB)\n\
    Total leaked memory: %f (MB) in %llu allocations\n",
        stat_mem_allocated_total,
        stat_mem_deallocated_total,
        (float)(stat_mem_allocated)                        / 1048576.0f,
        (float)(stat_mem_deallocated)                      / 1048576.0f,
        (float)(stat_mem_peak)                             / 1048576.0f,
        (float)(stat_mem_allocated - stat_mem_deallocated) / 1048576.0f,
        stat_mem_allocated_total - stat_mem_deallocated_total
    );
}

static void stat_dump_stats_table(stat_size_table_t table, const char *string, uint64_t *size) {
    size_t i,j;

    if (!table)
        return;

    for (i = 0, j = 1; i < ST_SIZE; i++) {
        stat_size_entry_t *entry;

        if (!(entry = table[i]))
            continue;

        con_out(string, (unsigned)j, (unsigned)entry->key, (unsigned)entry->value);
        j++;

        if (size)
            *size += entry->key * entry->value;
    }
}

void stat_info() {
    if (OPTS_OPTION_BOOL(OPTION_MEMCHK) ||
        OPTS_OPTION_BOOL(OPTION_STATISTICS)) {
        uint64_t mem = 0;

        con_out("Memory Statistics:\n\
    Total vectors allocated:       %llu\n\
    Total string duplicates:       %llu\n\
    Total string duplicate memory: %f (MB)\n\
    Total hashtables allocated:    %llu\n\
    Total unique vector sizes:     %llu\n",
            stat_used_vectors,
            stat_used_strdups,
            (float)(stat_mem_strdups) / 1048576.0f,
            stat_used_hashtables,
            stat_type_vectors
        );

        stat_dump_stats_table (
            stat_size_vectors,
            "        %2u| # of %5u byte vectors: %u\n",
            &mem
        );

        con_out (
            "    Total unique hashtable sizes: %llu\n",
            stat_type_hashtables
        );

        stat_dump_stats_table (
            stat_size_hashtables,
            "        %2u| # of %5u element hashtables: %u\n",
            NULL
        );

        con_out (
            "    Total vector memory:          %f (MB)\n\n",
            (float)(mem) / 1048576.0f
        );
    }

    if (stat_size_vectors)
        stat_size_del(stat_size_vectors);
    if (stat_size_hashtables)
        stat_size_del(stat_size_hashtables);

    if (OPTS_OPTION_BOOL(OPTION_DEBUG) ||
        OPTS_OPTION_BOOL(OPTION_MEMCHK))
        stat_dump_mem_info();

    if (OPTS_OPTION_BOOL(OPTION_DEBUG))
        stat_dump_mem_leaks();
}
#undef ST_SIZE
