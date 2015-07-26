#ifdef _WIN32
#include <io.h>
#include <stdlib.h>
#include <string.h>
#include "mman32.h"
enum {
	RTLD_LAZY = 1,
	RTLD_GLOBAL = 2
};
int dlopen(int x, int y)
{
  return 0;
}
void *dlsym(void *handle, char *name)
{
  if (!strcmp(name, "open"  )) return &open;
  if (!strcmp(name, "read"  )) return &read;
  if (!strcmp(name, "close" )) return &close;
  if (!strcmp(name, "printf")) return &printf;
  if (!strcmp(name, "malloc")) return &malloc;
  if (!strcmp(name, "memset")) return &memset;
  if (!strcmp(name, "memcmp")) return &memcmp;
  if (!strcmp(name, "memcpy")) return &memcpy;
  if (!strcmp(name, "mmap"  )) return &mmap;
  if (!strcmp(name, "dlsym" )) return &dlsym;
  if (!strcmp(name, "qsort" )) return &qsort;
  if (!strcmp(name, "exit"  )) return &exit;
  return 0;
}
#else
#include <dlfcn.h>
#endif
