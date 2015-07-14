#include "tr.h"

#ifdef __unix__
  #include <unistd.h>
#else
  #include "freegetopt/getopt.h"
#endif

static int usage() {
  printf("usage: tinyrb [options] [file]\n"
         "options:\n"
         "  -e   eval code\n"
         "  -d   show debug info (multiple times for more)\n"
         "  -v   print version\n"
         "  -h   print this\n");
  return 1;
}

int main (int argc, char *argv[]) {
  int opt;
  TrVM *vm = TrVM_new();

  while((opt = getopt(argc, argv, "e:vdh")) != -1) {
    switch(opt) {
      case 'e':
        TR_FAILSAFE(TrVM_eval(vm, optarg, "<eval>"));
        return 0;
      case 'v':
        printf("tinyrb %s\n", TR_VERSION);
        return 1;
      case 'd':
        vm->debug++;
        continue;
      case 'h':
      default:
        return usage();
    }
  }

  /* These lines allow us to tread argc and argv as though 
   * any switches were not there */
  argc -= optind;
  argv += optind;
  
  if (argc > 0) {
    TR_FAILSAFE(TrVM_load(vm, argv[argc-1]));
    return 0;
  }
  
  TrVM_destroy(vm);
  
  return usage();
}
