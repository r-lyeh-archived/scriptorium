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
#include "gmqcc.h"

/*
 * This is a very clever method for correcting mistakes in QuakeC code
 * most notably when invalid identifiers are used or inproper assignments;
 * we can proprly lookup in multiple dictonaries (depening on the rules
 * of what the task is trying to acomplish) to find the best possible
 * match.
 *
 *
 * A little about how it works, and probability theory:
 *
 *  When given an identifier (which we will denote I), we're essentially
 *  just trying to choose the most likely correction for that identifier.
 *  (the actual "correction" can very well be the identifier itself).
 *  There is actually no way to know for sure that certian identifers
 *  such as "lates", need to be corrected to "late" or "latest" or any
 *  other permutations that look lexically the same.  This is why we
 *  must advocate the usage of probabilities.  This means that instead of
 *  just guessing, instead we're trying to find the correction for C,
 *  out of all possible corrections that maximizes the probability of C
 *  for the original identifer I.
 *
 *  Thankfully there exists some theroies for probalistic interpretations
 *  of data.  Since we're operating on two distictive intepretations, the
 *  transposition from I to C. We need something that can express how much
 *  degree of I should rationally change to become C.  this is called the
 *  Bayesian interpretation. You can read more about it from here:
 *  http://www.celiagreen.com/charlesmccreery/statistics/bayestutorial.pdf
 *  (which is probably the only good online documentation for bayes theroy
 *  no lie.  Everything else just sucks ..)
 *
 *  Bayes' Thereom suggests something like the following:
 *      AC P(I|C) P(C) / P(I)
 *
 *  However since P(I) is the same for every possibility of I, we can
 *  completley ignore it giving just:
 *      AC P(I|C) P(C)
 *
 *  This greatly helps visualize how the parts of the expression are performed
 *  there is essentially three, from right to left we perform the following:
 *
 *  1: P(C), the probability that a proposed correction C will stand on its
 *     own.  This is called the language model.
 *
 *  2: P(I|C), the probability that I would be used, when the programmer
 *     really meant C.  This is the error model.
 *
 *  3: AC, the control mechanisim, an enumerator if you will, one that
 *     enumerates all feasible values of C, to determine the one that
 *     gives the greatest probability score.
 *
 *  In reality the requirement for a more complex expression involving
 *  two seperate models is considerably a waste.  But one must recognize
 *  that P(C|I) is already conflating two factors.  It's just much simpler
 *  to seperate the two models and deal with them explicitaly.  To properly
 *  estimate P(C|I) you have to consider both the probability of C and
 *  probability of the transposition from C to I.  It's simply much more
 *  cleaner, and direct to seperate the two factors.
 *
 *  Research tells us that 80% to 95% of all spelling errors have an edit
 *  distance no greater than one.  Knowing this we can optimize for most
 *  cases of mistakes without taking a performance hit.  Which is what we
 *  base longer edit distances off of.  Opposed to the original method of
 *  I had concieved of checking everything.
 *
 * A little information on additional algorithms used:
 *
 *   Initially when I implemented this corrector, it was very slow.
 *   Need I remind you this is essentially a brute force attack on strings,
 *   and since every transformation requires dynamic memory allocations,
 *   you can easily imagine where most of the runtime conflated.  Yes
 *   It went right to malloc.  More than THREE MILLION malloc calls are
 *   performed for an identifier about 16 bytes long.  This was such a
 *   shock to me.  A forward allocator (or as some call it a bump-point
 *   allocator, or just a memory pool) was implemented. To combat this.
 *
 *   But of course even other factors were making it slow.  Initially
 *   this used a hashtable.  And hashtables have a good constant lookup
 *   time complexity.  But the problem wasn't in the hashtable, it was
 *   in the hashing (despite having one of the fastest hash functions
 *   known).  Remember those 3 million mallocs? Well for every malloc
 *   there is also a hash.  After 3 million hashes .. you start to get
 *   very slow.  To combat this I had suggested burst tries to Blub.
 *   The next day he had implemented them. Sure enough this brought
 *   down the runtime by a factor > 100%
 *
 *   The trie initially was designed to work on all strings, but later it
 *   became aparent that not only was this not a requirement. It was also
 *   slowing down get/sets' for the trie.  To fully understand, only
 *   correct_alpha needs to be understood by the trie system, knowing this
 *   We can combat the slowness using a very clever but evil optimization.
 *   By Setting a fixed sized amount of branches for the trie using a
 *   char-to-index map into the branches. We've complelty made the trie
 *   accesses entierly constant in lookup time.  No really, a lookup is
 *   literally trie[str[0]] [str[1]] [2] .... .value.
 *
 *
 * Future Work (If we really need it)
 *
 *   Currently we can only distinguish one source of error in the
 *   language model we use.  This could become an issue for identifiers
 *   that have close colliding rates, e.g colate->coat yields collate.
 *
 *   Currently the error model has been fairly trivial, the smaller the
 *   edit distance the smaller the error.  This usually causes some un-
 *   expected problems. e.g reciet->recite yields recipt.  For QuakeC
 *   this could become a problem when lots of identifiers are involved.
 */


#define CORRECT_POOL_SIZE (128*1024*1024)
/*
 * A forward allcator for the corrector.  This corrector requires a lot
 * of allocations.  This forward allocator combats all those allocations
 * and speeds us up a little.  It also saves us space in a way since each
 * allocation isn't wasting a little header space for when NOTRACK isn't
 * defined.
 */
static unsigned char **correct_pool_data = NULL;
static unsigned char  *correct_pool_this = NULL;
static size_t          correct_pool_addr = 0;

static GMQCC_INLINE void correct_pool_new(void) {
    correct_pool_addr = 0;
    correct_pool_this = (unsigned char *)mem_a(CORRECT_POOL_SIZE);

    vec_push(correct_pool_data, correct_pool_this);
}

static GMQCC_INLINE void *correct_pool_alloc(size_t bytes) {
    void *data;
    if (correct_pool_addr + bytes>= CORRECT_POOL_SIZE)
        correct_pool_new();

    data               = (void*)correct_pool_this;
    correct_pool_this += bytes;
    correct_pool_addr += bytes;
    return data;
}

static GMQCC_INLINE void correct_pool_delete(void) {
    size_t i;
    for (i = 0; i < vec_size(correct_pool_data); ++i)
        mem_d(correct_pool_data[i]);

    correct_pool_data = NULL;
    correct_pool_this = NULL;
    correct_pool_addr = 0;
}


static GMQCC_INLINE char *correct_pool_claim(const char *data) {
    char *claim = util_strdup(data);
    return claim;
}

/*
 * _ is valid in identifiers. I've yet to implement numerics however
 * because they're only valid after the first character is of a _, or
 * alpha character.
 */
static const char correct_alpha[] = "abcdefghijklmnopqrstuvwxyz"
                                    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                    "_"; /* TODO: Numbers ... */

static const size_t correct_alpha_index[0x80] = {
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,  0,  0,  0,  0, 52,
     0, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
    41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51,  0,  0,  0,  0,  0
};

/*
 * A fast space efficent trie for a dictionary of identifiers.  This is
 * faster than a hashtable for one reason.  A hashtable itself may have
 * fast constant lookup time, but the hash itself must be very fast. We
 * have one of the fastest hash functions for strings, but if you do a
 * lost of hashing (which we do, almost 3 million hashes per identifier)
 * a hashtable becomes slow.
 */
correct_trie_t* correct_trie_new() {
    correct_trie_t *t = (correct_trie_t*)mem_a(sizeof(correct_trie_t));
    t->value   = NULL;
    t->entries = NULL;
    return t;
}

static GMQCC_INLINE void correct_trie_del_sub(correct_trie_t *t) {
    size_t i;
    if (!t->entries)
        return;
    for (i = 0; i < sizeof(correct_alpha)-1; ++i) {
        correct_trie_del_sub(&t->entries[i]);
    }
    mem_d(t->entries);
}

static GMQCC_INLINE void correct_trie_del(correct_trie_t *t) {
    size_t i;
    if (t->entries) {
        for (i = 0; i < sizeof(correct_alpha)-1; ++i)
            correct_trie_del_sub(&t->entries[i]);
        mem_d(t->entries);
    }
    mem_d(t);
}

static GMQCC_INLINE void* correct_trie_get(const correct_trie_t *t, const char *key) {
    const unsigned char *data = (const unsigned char*)key;

    while (*data) {
        if (!t->entries)
            return NULL;
        t = t->entries + correct_alpha_index[*data];
        ++data;
    }
    return t->value;
}

static GMQCC_INLINE void correct_trie_set(correct_trie_t *t, const char *key, void * const value) {
    const unsigned char *data = (const unsigned char*)key;
    while (*data) {
        if (!t->entries) {
            t->entries = (correct_trie_t*)mem_a(sizeof(correct_trie_t)*(sizeof(correct_alpha)-1));
            memset(t->entries, 0, sizeof(correct_trie_t)*(sizeof(correct_alpha)-1));
        }
        t = t->entries + correct_alpha_index[*data];
        ++data;
    }
    t->value = value;
}


/*
 * Implementation of the corrector algorithm commences. A very efficent
 * brute-force attack (thanks to tries and mempool :-)).
 */
static GMQCC_INLINE size_t *correct_find(correct_trie_t *table, const char *word) {
    return (size_t*)correct_trie_get(table, word);
}

static GMQCC_INLINE bool correct_update(correct_trie_t* *table, const char *word) {
    size_t *data = correct_find(*table, word);
    if (!data)
        return false;

    (*data)++;
    return true;
}

void correct_add(correct_trie_t* table, size_t ***size, const char *ident) {
    size_t     *data = NULL;
    const char *add  = ident;

    if (!correct_update(&table, add)) {
        data  = (size_t*)mem_a(sizeof(size_t));
        *data = 1;

        vec_push((*size), data);
        correct_trie_set(table, add, data);
    }
}

void correct_del(correct_trie_t* dictonary, size_t **data) {
    size_t       i;
    const size_t vs = vec_size(data);

    for (i = 0; i < vs; i++)
        mem_d(data[i]);

    vec_free(data);
    correct_trie_del(dictonary);
}

/*
 * correcting logic for the following forms of transformations:
 *  1) deletion
 *  2) transposition
 *  3) alteration
 *  4) insertion
 *
 * These functions could take an additional size_t **size paramater
 * and store back the results of their new length in an array that
 * is the same as **array for the memcmp in correct_exists. I'm just
 * not able to figure out how to do that just yet.  As my brain is
 * not in the mood to figure out that logic.  This is a reminder to
 * do it, or for someone else to :-) correct_edit however would also
 * need to take a size_t ** to carry it along (would all the argument
 * overhead be worth it?)
 */
static GMQCC_INLINE size_t correct_deletion(const char *ident, char **array) {
    size_t       itr = 0;
    const size_t len = strlen(ident);

    for (; itr < len; itr++) {
        char *a = (char*)correct_pool_alloc(len+1);
        memcpy(a, ident, itr);
        memcpy(a + itr, ident + itr + 1, len - itr);
        array[itr] = a;
    }

    return itr;
}

static GMQCC_INLINE size_t correct_transposition(const char *ident, char **array) {
    size_t       itr = 0;
    const size_t len = strlen(ident);

    for (; itr < len - 1; itr++) {
        char  tmp;
        char *a = (char*)correct_pool_alloc(len+1);
        memcpy(a, ident, len+1);
        tmp      = a[itr];
        a[itr  ] = a[itr+1];
        a[itr+1] = tmp;
        array[itr] = a;
    }

    return itr;
}

static GMQCC_INLINE size_t correct_alteration(const char *ident, char **array) {
    size_t       itr = 0;
    size_t       jtr = 0;
    size_t       ktr = 0;
    const size_t len = strlen(ident);

    for (; itr < len; itr++) {
        for (jtr = 0; jtr < sizeof(correct_alpha)-1; jtr++, ktr++) {
            char *a = (char*)correct_pool_alloc(len+1);
            memcpy(a, ident, len+1);
            a[itr] = correct_alpha[jtr];
            array[ktr] = a;
        }
    }

    return ktr;
}

static GMQCC_INLINE size_t correct_insertion(const char *ident, char **array) {
    size_t       itr = 0;
    size_t       jtr = 0;
    const size_t len = strlen(ident);

    for (; itr <= len; itr++) {
        for (jtr = 0; jtr < sizeof(correct_alpha)-1; jtr++) {
            char *a = (char*)correct_pool_alloc(len+2);
            memcpy(a, ident, itr);
            memcpy(a + itr + 1, ident + itr, len - itr + 1);
            a[itr] = correct_alpha[jtr];
            array[itr * (sizeof(correct_alpha)-1) + jtr] = a;
        }
    }

    return (len+1)*(sizeof(correct_alpha)-1);
}

static GMQCC_INLINE size_t correct_size(const char *ident) {
    /*
     * deletion      = len
     * transposition = len - 1
     * alteration    = len * sizeof(correct_alpha)
     * insertion     = (len + 1) * sizeof(correct_alpha)
     */

    register size_t len = strlen(ident);
    return (len) + (len - 1) + (len * (sizeof(correct_alpha)-1)) + ((len + 1) * (sizeof(correct_alpha)-1));
}

static GMQCC_INLINE char **correct_edit(const char *ident, size_t **lens) {
    size_t next;
    size_t size = correct_size(ident);
    char **find = (char**)correct_pool_alloc(size * sizeof(char*));

    if (!find || !(*lens = (size_t*)correct_pool_alloc(size * sizeof(size_t))))
        return NULL;

    next  = correct_deletion     (ident, find);
    next += correct_transposition(ident, find+next);
    next += correct_alteration   (ident, find+next);
    /*****/ correct_insertion    (ident, find+next);

    /* precompute lengths */
    for (next = 0; next < size; next++)
        (*lens)[next] = strlen(find[next]);

    return find;
}

static GMQCC_INLINE int correct_exist(char **array, register size_t *sizes, size_t rows, char *ident, register size_t len) {
    size_t itr;
    for (itr = 0; itr < rows; itr++) {
        /*
         * We can save tons of calls to memcmp if we simply ignore comparisions
         * that we know cannot contain the same length.
         */
        if (sizes[itr] == len && !memcmp(array[itr], ident, len))
            return 1;
    }

    return 0;
}

static GMQCC_INLINE char **correct_known_resize(char **res, size_t *allocated, size_t size) {
    size_t oldallocated = *allocated;
    char **out;
    if (size < oldallocated)
        return res;

    out = (char**)correct_pool_alloc(sizeof(*res) * oldallocated + 32);
    memcpy(out, res, sizeof(*res) * oldallocated);

    *allocated += 32;
    return out;
}

static char **correct_known(correction_t *corr, correct_trie_t* table, char **array, size_t rows, size_t *next) {
    size_t itr = 0;
    size_t jtr = 0;
    size_t len = 0;
    size_t row = 0;
    size_t nxt = 8;
    char   **res = (char**)correct_pool_alloc(sizeof(char *) * nxt);
    char   **end = NULL;
    size_t  *bit = NULL;

    for (; itr < rows; itr++) {
        if (!array[itr][0])
            continue;
        if (vec_size(corr->edits) > itr+1) {
            end = corr->edits[itr+1];
            bit = corr->lens [itr+1];
        } else {
            end = correct_edit(array[itr], &bit);
            vec_push(corr->edits, end);
            vec_push(corr->lens,  bit);
        }
        row = correct_size(array[itr]);

        for (jtr = 0; jtr < row; jtr++) {
            if (correct_find(table, end[jtr]) && !correct_exist(res, bit, len, end[jtr], bit[jtr])) {
                res        = correct_known_resize(res, &nxt, len+1);
                res[len++] = end[jtr];
            }
        }
    }

    *next = len;
    return res;
}

static GMQCC_INLINE char *correct_maximum(correct_trie_t* table, char **array, size_t rows) {
    char   *str = NULL;
    size_t *itm = NULL;
    size_t  itr = 0;
    size_t  top = 0;

    for (; itr < rows; itr++) {
        if ((itm = correct_find(table, array[itr])) && (*itm > top)) {
            top = *itm;
            str = array[itr];
        }
    }

    return str;
}

/*
 * This is the exposed interface:
 * takes a table for the dictonary a vector of sizes (used for internal
 * probability calculation), and an identifier to "correct".
 */
void correct_init(correction_t *c)
{
    correct_pool_new();
    c->edits = NULL;
    c->lens  = NULL;
}

void correct_free(correction_t *c)
{
    vec_free(c->edits);
    vec_free(c->lens);
    correct_pool_delete();
}

char *correct_str(correction_t *corr, correct_trie_t* table, const char *ident) {
    char **e1      = NULL;
    char **e2      = NULL;
    char  *e1ident = NULL;
    char  *e2ident = NULL;
    size_t e1rows  = 0;
    size_t e2rows  = 0;
    size_t *bits   = NULL;

    /* needs to be allocated for free later */
    if (correct_find(table, ident))
        return correct_pool_claim(ident);

    if ((e1rows = correct_size(ident))) {
        if (vec_size(corr->edits) > 0)
            e1 = corr->edits[0];
        else {
            e1 = correct_edit(ident, &bits);
            vec_push(corr->edits, e1);
            vec_push(corr->lens,  bits);
        }

        if ((e1ident = correct_maximum(table, e1, e1rows)))
            return correct_pool_claim(e1ident);
    }

    e2 = correct_known(corr, table, e1, e1rows, &e2rows);
    if (e2rows && ((e2ident = correct_maximum(table, e2, e2rows))))
        return correct_pool_claim(e2ident);


    return util_strdup(ident);
}
