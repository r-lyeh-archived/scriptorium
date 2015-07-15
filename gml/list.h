#ifndef REDROID_LIST_HDR
#define REDROID_LIST_HDR
#include <stdbool.h>
#include <stddef.h>

typedef struct list_iterator_s list_iterator_t;
typedef struct list_s          list_t;

/*
 * Function: list_iterator_create
 *  Create an iterator for a list.
 *
 * Parameters:
 *  list    - The list to create an iterator of.
 *
 * Returns:
 *  An iterator for the list.
 */
list_iterator_t *list_iterator_create(list_t *list);

/*
 * Function: list_iterator_destroy
 *  Destroy an iterator
 *
 * Parameters:
 *  iterator    - The iterator to destroy.
 */
void list_iterator_destroy(list_iterator_t *iterator);

/*
 * Function: list_iterator_reset
 *  Reset the iterator to the head of the list.
 *
 * Parameters:
 *  iterator    - The iterator to reset.
 */
void list_iterator_reset(list_iterator_t *iterator);

/*
 * Function: list_iterator_end
 *  Check if iterator is at end of list.
 *
 * Parameters:
 *  iterator    - The iterator to check.
 *
 * Returns:
 *  True if iterator is at the end of the list, false otherwise.
 */
bool list_iterator_end(list_iterator_t *iterator);

/*
 * Function: list_iterator_next
 *  Advance the iterator in a forward direction.
 *
 * Parameters:
 *  The iterator to advance.
 *
 * Returns:
 *  The element at the iterators position on advance.
 */
void *list_iterator_next(list_iterator_t *iterator);

/*
 * Function: list_iterator_prev
 *  Advance the iterator in a reverse direction.
 *
 * Parameters:
 *  The iterator to advance.
 *
 * Returns:
 *  The element at the iterators position on advance.
 */
void *list_iterator_prev(list_iterator_t *iterator);

/*
 * Function: list_create
 *  Create a list
 *
 * Returns:
 *  A list
 */
list_t *list_create(void);

/*
 * Function: list_destroy
 *  Destroy a list
 *
 * Parameters:
 *  list    - The list to destroy
 */
void list_destroy(list_t *list);

/*
 * Function: list_push
 *  Push an element onto the list's tail end.
 *
 * Parameters:
 *  list    - The list to put the element into.
 *  element - The element to put in the list.
 */
void list_push(list_t *list, void *element);

/*
 * Function: list_prepend
 *  Push an element onto the list's head end.
 *
 * Parameters:
 *  list    - The list to put the element into.
 *  element - The element to put in the list.
 */
void list_prepend(list_t *list, void *element);

/*
 * Function: list_pop
 *  Pop an element off the list's tail end.
 *
 * Parameters:
 *  list    - The list to pop the element off of.
 *
 * Returns:
 *  The element.
 */
void *list_pop(list_t *list);

/*
 * Function: list_shift
 *  Pop an element off the list's head end.
 *
 * Parameters:
 *  list    - The list to pop the element off of.
 *
 * Returns:
 *  The element.
 *
 * Remarks:
 *  This thrashes the atcache of the list.
 */
void *list_shift(list_t *list);

/*
 * Function: list_at
 *  Get an element at a list index as if it where an array.
 *
 * Parameters:
 *  list    - The list to get the element of
 *  index   - The index (i.e number of indirections from head).
 *
 * Returns:
 *  The element.
 *
 * Remarks:
 *  If the list is used where only tail operations are performed, i.e
 *  no shifting, no erasing and no sorting then the internal atcache
 *  mechanism of the list will make this O(1) best case. In the event
 *  where that isn't the case there may still be hope. For instance
 *  when making calls to list_find and list_search the atcache is
 *  thrashed and replaced to reflect the current ordering of the list
 *  at least up to the find/search invariant. This means O(1) is still
 *  very possible. In the event that none of this is the case a linear
 *  search takes place from the index and it becomes cached so that
 *  subsequent searches become O(1). The worst case complexity is still
 *  O(n).
 */
void *list_at(list_t *list, size_t index);

/*
 * Function: list_find
 *  Find an element in a list.
 *
 * Parameters:
 *  list    - The list to search in.
 *  element - The element to search for.
 *
 * Returns:
 *  True if the element is found, false otherwise.
 *
 * Remarks:
 *  Will thrash atcache and overwrite contents so that it reflects
 *  all visited nodes in the linear search up to the invariant that
 *  breaks out of the search.
 *
 * Other uses:
 *  Using this function with element=NULL or an element which doesn't
 *  exist is a great way to syncronize the atcache of the list. It
 *  will force all nodes to be visited thus filling the atcache with
 *  all the content it needs to make subsequent calls to list_at constant.
 */
bool list_find(list_t *list, const void *element);

/*
 * Function: list_search
 *  Search the list with a user-defined invariant via predicate.
 *
 * Parameters:
 *  list      - The list to search.
 *  predicate - The predicate used for the invariant in the search.
 *  pass      - The information to pass to the predicate's second argument.
 *
 * Returns:
 *  The element of some node in the linear search which was concluded to
 *  when the predicate itself returned true.
 *
 * Other uses:
 *  This function has the same other uses as list_find. Mainly it thrashes
 *  the atcache and syncronizes it with the list.
 */
void *list_search(list_t *list, bool (*predicate)(const void *, const void *), const void *pass);

/*
 * Function: list_copy
 *  Perform a copy of a list.
 *
 * Parameters:
 *  list    - The list to copy.
 *
 * Returns:
 *  A copied list.
 */
list_t *list_copy(list_t *list);

/*
 * Function: list_length
 *  Get the length of a list (i.e number of elements).
 *
 * Parameters:
 *  list    - The list to get the length of.
 *
 * Returns:
 *  The amount of elements in the list.
 */
size_t list_length(list_t *list);


/*
 * Function: list_foreach
 *  Execute a callback passing in each value in the entire list
 *  as well as passing in an additional pointer.
 *
 * Parameters:
 *  list        - The list to execute the callback over.
 *  pass        - The additional thing to pass in for the callback to
 *                get as its second argument.
 *  callback    - Pointer to function callback.
 */
#define list_foreach(LIST, PASS, CALLBACK) \
    list_foreach_impl((LIST), (PASS), (void (*)(void *, void *))(CALLBACK))

void list_foreach_impl(list_t *list, void *pass, void (*callback)(void *, void *));

/*
 * Function: list_erase
 *  Erase an element in a list.
 *
 * Parameters:
 *  list    - The list to erase the element from.
 *  element - The element to erase.
 *
 * Returns:
 *  True if the element was found and erased, false otherwise.
 *
 * Remarks:
 *  This thrashes the atcache of the list.
 */
bool list_erase(list_t *list, void *element);

/*
 * Function: list_sort
 *  Sort a list.
 *
 * Parameters:
 *  list      - The list to store
 *  predicate - Pointer to function predicate that returns a boolean
 *              transitive relationship for two elements of the list.
 *
 * Remarks:
 *  This thrashes the atcache of the list.
 */
void list_sort(list_t *list, bool (*predicate)(const void *, const void *));

#endif
