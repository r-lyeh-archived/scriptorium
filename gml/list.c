#include "list.h"

#include <stdlib.h>
#include <string.h>

typedef struct list_node_s list_node_t;

struct list_node_s {
    void        *element;
    list_node_t *next;
    list_node_t *prev;
};

typedef struct {
    list_node_t **data;
    size_t        size;
    size_t        headdirt;
    size_t        taildirt;
} list_atcache_t;

struct list_s {
    size_t         length;
    list_node_t   *head;
    list_node_t   *tail;
    list_atcache_t atcache;
};

struct list_iterator_s {
    list_t      *list;
    list_node_t *pointer;
};

/* List iterator */
list_iterator_t *list_iterator_create(list_t *list) {
    if (!list) return NULL;

    list_iterator_t *it = malloc(sizeof(*it));
    if (!it)
        return NULL;

    it->pointer = list->head;
    it->list    = list;

    return it;
}

void list_iterator_destroy(list_iterator_t *it) {
    free(it);
}

void *list_iterator_next(list_iterator_t *it) {
    if (!it->pointer) return NULL;
    void *ptr = it->pointer->element;
    it->pointer = it->pointer->next;
    return ptr;
}

void *list_iterator_prev(list_iterator_t *it) {
    if (!it->pointer) return NULL;
    void *ptr = it->pointer->element;
    it->pointer = it->pointer->prev;
    return ptr;
}

void list_iterator_reset(list_iterator_t *it) {
    it->pointer = it->list->head;
}

bool list_iterator_end(list_iterator_t *it) {
    return !it->pointer;
}

/* List node */
static list_node_t *list_node_create(void *element) {
    list_node_t *node = malloc(sizeof(*node));
    if (!node)
        return NULL;

    node->element = element;
    node->next    = NULL;
    node->prev    = NULL;

    return node;
}

static void list_node_destroy(list_node_t *node) {
    free(node);
}

static void list_node_scrub(list_node_t **node) {
    list_node_destroy(*node);
    *node = NULL;
}

static void list_atcache_create(list_t *list) {
    list->atcache.data     = calloc(16, sizeof(list_node_t));
    list->atcache.size     = 16;
    list->atcache.taildirt = 0;
    list->atcache.headdirt = 0;
}

static void list_atcache_destroy(list_t *list) {
    free(list->atcache.data);
}

static void list_atcache_resize(list_t *list) {
    size_t last = list->atcache.size;

    list->atcache.size *= 2;
    list->atcache.data  = realloc(list->atcache.data, sizeof(list_node_t*) * list->atcache.size);

    memset(&list->atcache.data[last], 0, sizeof(list_node_t*) * (list->atcache.size - last));
}

static void list_atcache_thrash(list_t *list) {
    memset(list->atcache.data, 0, sizeof(list_node_t*) * list->atcache.size);
    list->atcache.headdirt = 0;
    list->atcache.taildirt = 0;
}

static void list_atcache_check(list_t *list) {
    if (list->atcache.taildirt > 0 && list->atcache.headdirt == 0) {
        size_t index = list->length - (list->atcache.taildirt + 1);
        if (index >= list->atcache.size) {
            list_atcache_thrash(list);
            return;
        }

        for (size_t i = index; i < list->atcache.taildirt; i++)
            list->atcache.data[index] = NULL;
        return;
    }

    /* thrash it all */
    if (list->atcache.headdirt > 0)
        list_atcache_thrash(list);
}

static void list_atcache_cache_index(list_t *list, list_node_t *node, size_t index) {
    list_atcache_check(list);

    if (list->length >= list->atcache.size)
        list_atcache_resize(list);

    list->atcache.data[index] = node;
}

static void list_atcache_cache(list_t *list, list_node_t *node) {
    list_atcache_cache_index(list, node, list->length);
}

/* List */
list_t *list_create(void) {
    list_t *list    = malloc(sizeof(*list));
    if (!list)
        return NULL;

    list->length    = 0;
    list->head      = NULL;
    list->tail      = NULL;

    list_atcache_create(list);
    return list;
}

void list_destroy(list_t *list) {
    list_node_t *temp;
    for (list_node_t *node = list->head; node; ) {
        temp = node->next;
        list_node_destroy(node);
        node = temp;
    }
    list_atcache_destroy(list);
    free(list);
}

void list_push(list_t *list, void *element) {
    list_node_t *node = list_node_create(element);
    if (!list->head)
        list->head = node;
    else {
        list->tail->next = node;
        node->prev       = list->tail;
    }

    list->tail = node;
    list_atcache_cache(list, node);
    list->length++;
}

void list_prepend(list_t *list, void *element) {
    list_node_t *node = list_node_create(element);
    node->next = list->head;
    node->prev = list->tail;
    list->head = node;
    list->atcache.headdirt++;
    list->length++;
}

void *list_pop(list_t *list) {
    if (!list->head)
        return NULL;

    void *element = list->tail->element;
    list->tail = list->tail->prev;
    list_node_scrub((list->tail) ? &list->tail->next : &list->head);
    list->length--;
    list->atcache.taildirt++;
    return element;
}

void *list_shift(list_t *list) {
    if (!list->head)
        return NULL;

    void *element = list->head->element;
    list->head = list->head->next;
    list_node_scrub((list->head) ? &list->head->prev : &list->tail);
    list->length--;
    list->atcache.headdirt++;
    return element;
}

void *list_at(list_t *list, size_t index) {
    list_atcache_check(list);

    if (list->atcache.data[index])
        return list->atcache.data[index]->element;
    if (index > list->length)
        return NULL;

    list_node_t *node = NULL;
    if (index > list->length / 2) {
        node = list->tail;
        for (size_t i = list->length / 2; i < index; i++)
            node = node->prev;
    } else {
        node = list->head;
        for (size_t i = 0; i < index; i++)
            node = node->next;
    }

    if (node) {
        list_atcache_cache_index(list, node, index);
        return node->element;
    }

    return NULL;
}

list_t *list_copy(list_t *list) {
    list_t *copy = list_create();
    for (list_node_t *curr = list->head; curr; curr = curr->next)
        list_push(copy, curr->element);
    return copy;
}

bool list_erase(list_t *list, void *element) {
    for (list_node_t *curr = list->head; curr; curr = curr->next) {
        if (curr->element != element)
            continue;

        if (curr == list->head)
            list->head = list->head->next;
        if (curr == list->tail)
            list->tail = list->tail->prev;
        if (curr->next)
            curr->next->prev = curr->prev;
        if (curr->prev)
            curr->prev->next = curr->next;

        list_node_destroy(curr);
        list->length--;
        list_atcache_thrash(list);
        return true;
    }
    return false;
}

bool list_find(list_t *list, const void *element) {
    list_node_t *node = list->head;

    list_atcache_thrash(list);
    for(size_t index = 0; node && node->element != element; node = node->next, index++)
        list_atcache_cache_index(list, node, index);
    return !!node;
}

void *list_search(list_t *list, bool (*predicate)(const void *, const void *), const void *pass) {
    list_node_t *node = list->head;

    list_atcache_thrash(list);
    for(size_t index = 0; node && !predicate(node->element, pass); node = node->next, index++)
        list_atcache_cache_index(list, node, index);

    return (node) ? node->element : NULL;
}

size_t list_length(list_t *list) {
    return (list) ? list->length : 0;
}

void list_foreach_impl(list_t *list, void *pass, void (*callback)(void *, void *)) {
    for (list_node_t *curr = list->head; curr; curr = curr->next)
        callback(curr->element, pass);
}

static list_node_t *list_sort_split(list_node_t *node) {
    if (!node || !node->next) return NULL;
    list_node_t *split = node->next;
    node->next  = split->next;
    split->next = list_sort_split(split->next);
    return split;
}

static list_node_t *list_sort_merge(list_node_t *a, list_node_t *b, bool (*predicate)(const void *, const void *)) {
    if (!a) return b;
    if (!b) return a;
    if (predicate(a->element, b->element)) {
        b->next       = list_sort_merge(a, b->next, predicate);
        b->next->prev = b;
        return b;
    }
    a->next       = list_sort_merge(a->next, b, predicate);
    a->next->prev = a;
    return a;
}

static list_node_t *list_sort_impl(list_node_t *begin, bool (*predicate)(const void *, const void *)) {
    if (!begin)       return NULL;
    if (!begin->next) return begin;

    list_node_t *split = list_sort_split(begin);
    return list_sort_merge(list_sort_impl(begin, predicate), list_sort_impl(split, predicate), predicate);
}

void list_sort(list_t *list, bool (*predicate)(const void *, const void *)) {
    list->head = list_sort_impl(list->head, predicate);
    list_atcache_thrash(list);
}
