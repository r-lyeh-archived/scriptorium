/* test.c */
#include "duktape.h"
#include <stdio.h>

int main(int argc, char *argv[]) {
    duk_context *ctx = duk_create_heap_default();
    if( argc > 1 ) {
        FILE *fp = fopen(argv[1], "rb");
        if( fp ) {
                fseek(fp, 0L, SEEK_END);
                size_t size = ftell(fp);
                fseek(fp, 0L, SEEK_SET);
                char *buffer = (char *)malloc( size + 1 );
                if( buffer ) {
                        fread( buffer, size, 1, fp );
                        buffer[ size ] = '\0';
                        duk_eval_string(ctx, buffer);
                }
                free(buffer);
                fclose(fp);
        }
    }
    else {
        duk_eval_string(ctx, "print('Hello world!');");
    }
    duk_destroy_heap(ctx);
    return 0;
}