#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <linenoise.h>
#include "gml.h"

/* For the REPL we define a quit function */
typedef struct {
    int quit;
} repl_t;

static gml_value_t repl_builtin_quit(gml_state_t *gml, gml_value_t *args, size_t nargs) {
    gml_arg_check(gml, args, nargs, "quit", "");
    ((repl_t*)gml_state_user_get(gml))->quit = 1;
    return gml_none_create(gml);
}

static const char *repl_completions[] = {
    /* Builtins */
    "print",  "println", "cos",    "sin",    "tan",    "acos",  "asin",
    "atan",   "atan2",   "cosh",   "sinh",   "tanh",   "acosh", "asinh",
    "atanh",  "exp",     "exp2",   "expm1",  "ldexp",  "log",   "log2",
    "log10",  "ilogb",   "log1p",  "logb",   "scalbn", "pow",   "sqrt",
    "cbrt",   "hypot",   "floor",  "ceil",   "map",    "range", "filter",
    "reduce", "length",  "find",

    /* Keywords */
    "if",     "elif",    "else",   "fn",     "var",    "for",   "in",
    "while",  "return"
};

static void repl_completion(const char *complete, linenoiseCompletions *lc) {
    (void)complete;
    for (size_t i = 0; i < sizeof(repl_completions) / sizeof(*repl_completions); i++)
        linenoiseAddCompletion(lc, repl_completions[i]);
}

static char *repl_prompt = ">>> ";

static int repl_dump(gml_state_t *gml, gml_value_t value) {
    size_t allocated = 1024;
    size_t count     = 0;
    char  *buffer    = NULL;

    if (!(buffer = malloc(allocated)))
        return 0;

    /* Keep trying until the buffer is large enough to hold the result */
    while ((count = gml_dump(gml, value, buffer, allocated)) >= allocated) {
        allocated *= 2;
        if (!(buffer = realloc(buffer, allocated))) {
            free(buffer);
            fprintf(stderr, "[out of memory]\n");
            return 0;
        }
    }

    if (count != 0)
        printf("%s\n", buffer);
    free(buffer);
    return 1;
}

/*
 * Since this is a read-eval-print-loop, or; repl for short. We need to
 * keep the state around between inputs. We encapsulate the runtime state
 * within here as a result.
 */
static int repl_read(int history) {
    char        *prompt    = repl_prompt;
    char        *promptmem = NULL;
    gml_state_t *gml       = NULL;

    if (!(gml = gml_state_create()))
        return 0;

    repl_t repl = { .quit = 0 };
    gml_state_user_set(gml, (void *)&repl);

    /* Install the builtins */
    gml_builtins_install(gml);
    gml_set_native(gml, "quit", &repl_builtin_quit, 0, 0);

    char   *linedata  = NULL;
    char   *linetail  = NULL;
    size_t  linesize  = 0;
    list_t *linelist  = list_create();
    size_t  indent    = 0;
    size_t  shorthand = 0;

    while (!repl.quit) {
        if (indent) {
            /* construct a new indent prompt */
            free(promptmem);
            promptmem = calloc(1, 4 * indent + 1);
            for (size_t i = 0; i < indent + 1; i++)
                strcat(promptmem, "... ");
            prompt = promptmem;
        } else {
            prompt = repl_prompt;
            if (list_length(linelist)) {
                list_foreach(linelist, NULL, &free);
                list_destroy(linelist);
                linelist = list_create();
            }
        }

        /* No more lines to process? */
        if (!(linedata = linenoise(prompt)))
            break;

        /* Empty lines are ignored */
        if (!*linedata) {
            free(linedata);
            continue;
        }

        linesize = strlen(linedata);
        linetail = &linedata[linesize - 1];

        /* Handle indentation levels */
        if (strchr("([{", *linetail))
            indent++;

        if (strchr(")]}", *linetail)) {
            /* We do not want to subtract too far */
            if (indent)
                indent--;
        }

        /* If there is a shorthand expression then mark it as such */
        if (linetail[-1] == '=' && linetail[0] == '>')
            shorthand++;

        /* No indentation levels left */
        if (indent == 0) {
            /*
             * No indentation levels means any shorthand expressions are
             * now finished.
             */
            if (shorthand)
                shorthand--;

            /*
             * We've just finished the indentation level on the current
             * line or we're a single shorthand expression
             */
            if (strchr("};", *linetail) || shorthand == 1) {
                /* There is a back buffer that goes with all of this */
                if (list_length(linelist)) {
                    size_t           plsize = 1;
                    char            *pldata = NULL;
                    char            *plitem = NULL;
                    list_iterator_t *it     = list_iterator_create(linelist);

                    /* Calculate playload size */
                    while (!list_iterator_end(it))
                        plsize += strlen(list_iterator_next(it));
                    list_iterator_destroy(it);

                    /* Construct payload */
                    pldata = calloc(1, plsize + linesize);
                    while ((plitem = list_shift(linelist))) {
                        strcat(pldata, plitem);
                        free(plitem);
                    }

                    /* Append the current line to the rest of the payload */
                    strcat(pldata, linedata);

                    /* Swap what is going to be processed */
                    free(linedata);
                    linedata = pldata;
                }

                /* Process the payload */
                if (!repl_dump(gml, gml_run_string(gml, linedata)))
                    goto repl_read_leave;
            }
        }
        /* Append the line to the history */
        if (history)
            linenoiseHistoryAdd(linedata);
        /* Append the line to our back buffer */
        list_push(linelist, linedata);
    }

repl_read_leave:
    /* Any unprocessed data in our back buffer needs to be freed */
    list_foreach(linelist, NULL, &free);
    list_destroy(linelist);

    /* If there was an allocated prompt */
    free(promptmem);

    gml_state_destroy(gml);
    return 1;
}

static int repl_exec(list_t *files) {
    gml_state_t *gml = NULL;
    if (!(gml = gml_state_create()))
        return 0;

    /* Install the builtins */
    gml_builtins_install(gml);

    const char *file;
    while ((file = list_shift(files))) {
        gml_run_file(gml, file);
    }
    gml_state_destroy(gml);

    return 1;
}

typedef enum {
    CMD_VERSION      = 1 << 0,
    CMD_MULTILINE    = 1 << 1,
    CMD_HISTORY      = 1 << 2,
    CMD_AUTOCOMPLETE = 1 << 3
} repl_cmd_t;

static const struct {
    repl_cmd_t  flag;
    const char *name;
} repl_options[] = {
    { CMD_VERSION,      "version"      }, /* Must be the top one */
#ifdef HAVE_MULTILINE
    { CMD_MULTILINE,    "multiline"    },
#endif
    { CMD_HISTORY,      "history"      },
    { CMD_AUTOCOMPLETE, "autocomplete" }
};

static void repl_help(const char *app) {
    printf("Usage: %s [options] file...\n", app);
    printf("Options:\n");
    for (size_t i = 0; i < sizeof(repl_options) / sizeof(*repl_options); i++)
        printf("    --%s\n", repl_options[i].name);
    /* Start from 1 to skip CMD_VERSION */
    for (size_t i = 1; i < sizeof(repl_options) / sizeof(*repl_options); i++)
        printf("    --no-%s\n", repl_options[i].name);
}

int main(int argc, char **argv) {
    argc--;
    argv++;

    const char *app   = argv[-1];
    list_t     *files = list_create();
    repl_cmd_t  flags = CMD_HISTORY | CMD_AUTOCOMPLETE;

    for (int arg = 0; arg < argc; arg++) {
        if (!strncmp(argv[arg], "--", 2)) {
            char *command = &argv[arg][2];
            for (size_t i = 0; i < sizeof(repl_options) / sizeof(*repl_options); i++) {
                if (!strcmp(command, repl_options[i].name))
                    flags |= repl_options[i].flag;
            }
            /* If it contains no- then it's a 'disable' */
            if (!strncmp(command, "no-", 3)) {
                command = &argv[arg][5];
                /* Start from 1 to skip CMD_VERSION */
                for (size_t i = 1; i < sizeof(repl_options) / sizeof(*repl_options); i++) {
                    if (!strcmp(command, repl_options[i].name))
                        flags &= ~repl_options[i].flag;
                }
            }
            if (!strcmp(command, "help")) {
                list_destroy(files);
                repl_help(app);
                return 0;
            }
        } else {
            /* If it contains .gml it's a file */
            char *find = strrchr(argv[arg], '.');
            if (find && !strcmp(find, ".gml"))
                list_push(files, argv[arg]);
        }
        argc--;
    }

    /*
     * If there is a version flag all others are ignored. We just print
     * the version and exit
     */
    if (flags & CMD_VERSION) {
        printf("GML %d.%d.%d (%s, %s, %s)\n",
            GML_VER_MAJOR,
            GML_VER_MINOR,
            GML_VER_PATCH,
            GML_TYPE,
            __DATE__,
            __TIME__
        );

        list_destroy(files);
        return 0;
    }

#ifdef HAVE_MULTILINE
    if (flags & CMD_MULTILINE)
        linenoiseSetMultiLine(1);
#endif

    if (flags & CMD_AUTOCOMPLETE)
        linenoiseSetCompletionCallback(repl_completion);

    int status = (list_length(files) != 0)
                    ? repl_exec(files)
                    : repl_read(flags & CMD_HISTORY);
    list_destroy(files);
    return !status;
}
