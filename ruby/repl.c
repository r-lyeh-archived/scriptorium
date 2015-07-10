#include <stdlib.h>
#include <stdio.h>

/* Include the mruby headers */
#include <mruby.h>
#include <mruby/proc.h>
#include <mruby/data.h>
#include <mruby/compile.h>

int
main(int argc, const char **argv)
{
  mrb_state *mrb = mrb_open();
  if (mrb) { /* handle error */ 
  // mrb_load_nstring() for strings without null terminator or with known length

    if( argc > 1 )
    {
        FILE *fp = fopen(argv[1], "rb");
        if( fp ) {
                fseek(fp, 0L, SEEK_END);
                size_t size = ftell(fp);
                fseek(fp, 0L, SEEK_SET);
                char *buffer = (char *)malloc( size + 1 );
                if( buffer ) {
                    fread( buffer, size, 1, fp );
                    buffer[ size ] = '\0';
                    mrb_load_string(mrb, buffer);
                    mrb_close(mrb);
                }
                free(buffer);
                fclose(fp);
        }
    }
    else {

        const char *script =
        "def fibonacci(n)\n"
        "   n <= 1 ? n :  fibonacci( n - 1 ) + fibonacci( n - 2 ) \n"
        "end\n"
        "puts 'hello world'\n"
        "puts fibonacci( 34 )\n";

        mrb_load_string(mrb, script);
        mrb_close(mrb);
    }
  }
}
