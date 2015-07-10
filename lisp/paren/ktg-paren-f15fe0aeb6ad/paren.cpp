// (C) 2013-2014 Kim, Taegyoon
// The Paren Programming Language

#include <cstdio>
#include <cstring>
#include <string>
#include "libparen.h"
using namespace std;

int main(int argc, char *argv[]) {
	libparen::init();
    if (argc <= 1) {        
        libparen::print_logo();
        libparen::repl();
        puts("");
        return 0;
    }
    else if (argc == 2) {
        char *opt(argv[1]);
        if (strcmp(opt, "-h") == 0) {
            puts("Usage: paren [OPTIONS...] [FILES...]");
            puts("");
            puts("OPTIONS:");
            puts("    -h    print this screen.");
            puts("    -v    print version.");
            return 0;
        } else if (strcmp(opt, "-v") == 0) {
            puts(PAREN_VERSION);
            return 0;
        }
    }

    // execute files	
    for (int i = 1; i < argc; i++) {        
		string code;
		if (libparen::slurp(string(argv[i]), code)) {
			libparen::eval_string(code);
		} else {
            fprintf(stderr, "Cannot open file: %s\n", argv[i]);
        }
    }
}
