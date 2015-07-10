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
#include <stdlib.h>
#include <string.h>

#include "gmqcc.h"
#include "lexer.h"

/* TODO: cleanup this whole file .. it's a fuckign mess */

/* set by the standard */
const oper_info *operators      = NULL;
size_t           operator_count = 0;
static bool      opts_output_wasset = false;

typedef struct { char *filename; int   type;  } argitem;
typedef struct { char *name;     char *value; } ppitem;
static argitem *items = NULL;
static ppitem  *ppems = NULL;

#define TYPE_QC  0
#define TYPE_ASM 1
#define TYPE_SRC 2

static const char *app_name;

static void version(void) {
    con_out("GMQCC %d.%d.%d Built %s %s\n" GMQCC_DEV_VERSION_STRING,
        GMQCC_VERSION_MAJOR,
        GMQCC_VERSION_MINOR,
        GMQCC_VERSION_PATCH,
        __DATE__,
        __TIME__
    );
}

static int usage(void) {
    con_out("usage: %s [options] [files...]", app_name);
    con_out("options:\n"
            "  -h, --help             show this help message\n"
            "  -debug                 turns on compiler debug messages\n"
            "  -memchk                turns on compiler memory leak check\n");
    con_out("  -o, --output=file      output file, defaults to progs.dat\n"
            "  -s filename            add a progs.src file to be used\n");
    con_out("  -E                     stop after preprocessing\n");
    con_out("  -q, --quiet            be less verbose\n");
    con_out("  -config file           use the specified ini file\n");
    con_out("  -std=standard          select one of the following standards\n"
            "       -std=qcc          original QuakeC\n"
            "       -std=fteqcc       fteqcc QuakeC\n"
            "       -std=gmqcc        this compiler (default)\n");
    con_out("  -f<flag>               enable a flag\n"
            "  -fno-<flag>            disable a flag\n"
            "  -fhelp                 list possible flags\n");
    con_out("  -W<warning>            enable a warning\n"
            "  -Wno-<warning>         disable a warning\n"
            "  -Wall                  enable all warnings\n");
    con_out("  -Werror                treat warnings as errors\n"
            "  -Werror-<warning>      treat a warning as error\n"
            "  -Wno-error-<warning>   opposite of the above\n");
    con_out("  -Whelp                 list possible warnings\n");
    con_out("  -O<number>             optimization level\n"
            "  -O<name>               enable specific optimization\n"
            "  -Ono-<name>            disable specific optimization\n"
            "  -Ohelp                 list optimizations\n");
    con_out("  -force-crc=num         force a specific checksum into the header\n");
    con_out("  -state-fps=num         emulate OP_STATE with the specified FPS\n");
    con_out("  -coverage              add coverage support\n");
    return -1;
}

/* command line parsing */
static bool options_witharg(int *argc_, char ***argv_, char **out) {
    int  argc   = *argc_;
    char **argv = *argv_;

    if (argv[0][2]) {
        *out = argv[0]+2;
        return true;
    }
    /* eat up the next */
    if (argc < 2) /* no parameter was provided */
        return false;

    *out = argv[1];
    --*argc_;
    ++*argv_;
    return true;
}

static bool options_long_witharg_all(const char *optname, int *argc_, char ***argv_, char **out, int ds, bool split) {
    int  argc   = *argc_;
    char **argv = *argv_;

    size_t len = strlen(optname);

    if (strncmp(argv[0]+ds, optname, len))
        return false;

    /* it's --optname, check how the parameter is supplied */
    if (argv[0][ds+len] == '=') {
        /* using --opt=param */
        *out = argv[0]+ds+len+1;
        return true;
    }

    if (!split || argc < ds) /* no parameter was provided, or only single-arg form accepted */
        return false;

    /* using --opt param */
    *out = argv[1];
    --*argc_;
    ++*argv_;
    return true;
}
static bool options_long_witharg(const char *optname, int *argc_, char ***argv_, char **out) {
    return options_long_witharg_all(optname, argc_, argv_, out, 2, true);
}
static bool options_long_gcc(const char *optname, int *argc_, char ***argv_, char **out) {
    return options_long_witharg_all(optname, argc_, argv_, out, 1, false);
}

static bool options_parse(int argc, char **argv) {
    bool argend = false;
    size_t itr;
    char  buffer[1024];
    char *redirout    = NULL;
    char *redirerr    = NULL;
    char *config      = NULL;
    char *memdumpcols = NULL;

    while (!argend && argc > 1) {
        char *argarg;
        argitem item;
        ppitem  macro;

        ++argv;
        --argc;

        if (argv[0][0] == '-') {
            /* All gcc-type long options */
            if (options_long_gcc("std", &argc, &argv, &argarg)) {
                if (!strcmp(argarg, "gmqcc") || !strcmp(argarg, "default")) {

                    opts_set(opts.flags, ADJUST_VECTOR_FIELDS,          true);
                    opts_set(opts.flags, CORRECT_LOGIC,                 true);
                    opts_set(opts.flags, SHORT_LOGIC,                   true);
                    opts_set(opts.flags, UNTYPED_NIL,                   true);
                    opts_set(opts.flags, VARIADIC_ARGS,                 true);
                    opts_set(opts.flags, FALSE_EMPTY_STRINGS,           false);
                    opts_set(opts.flags, TRUE_EMPTY_STRINGS,            true);
                    opts_set(opts.flags, LOOP_LABELS,                   true);
                    opts_set(opts.flags, TRANSLATABLE_STRINGS,          true);
                    opts_set(opts.flags, INITIALIZED_NONCONSTANTS,      true);
                    opts_set(opts.werror, WARN_INVALID_PARAMETER_COUNT, true);
                    opts_set(opts.werror, WARN_MISSING_RETURN_VALUES,   true);
                    opts_set(opts.flags,  EXPRESSIONS_FOR_BUILTINS,     true);
                    opts_set(opts.warn,   WARN_BREAKDEF,                true);



                    OPTS_OPTION_U32(OPTION_STANDARD) = COMPILER_GMQCC;

                } else if (!strcmp(argarg, "qcc")) {

                    opts_set(opts.flags, ADJUST_VECTOR_FIELDS,  false);
                    opts_set(opts.flags, ASSIGN_FUNCTION_TYPES, true);

                    OPTS_OPTION_U32(OPTION_STANDARD) = COMPILER_QCC;

                } else if (!strcmp(argarg, "fte") || !strcmp(argarg, "fteqcc")) {

                    opts_set(opts.flags, FTEPP,                    true);
                    opts_set(opts.flags, TRANSLATABLE_STRINGS,     true);
                    opts_set(opts.flags, ADJUST_VECTOR_FIELDS,     false);
                    opts_set(opts.flags, ASSIGN_FUNCTION_TYPES,    true);
                    opts_set(opts.flags, CORRECT_TERNARY,          false);
                    opts_set(opts.warn, WARN_TERNARY_PRECEDENCE,   true);
                    opts_set(opts.warn, WARN_BREAKDEF,             true);

                    OPTS_OPTION_U32(OPTION_STANDARD) = COMPILER_FTEQCC;

                } else if (!strcmp(argarg, "qccx")) {

                    opts_set(opts.flags, ADJUST_VECTOR_FIELDS,  false);
                    OPTS_OPTION_U32(OPTION_STANDARD) = COMPILER_QCCX;

                } else {
                    con_out("Unknown standard: %s\n", argarg);
                    return false;
                }
                continue;
            }
            if (options_long_gcc("force-crc", &argc, &argv, &argarg)) {

                OPTS_OPTION_BOOL(OPTION_FORCECRC)   = true;
                OPTS_OPTION_U16 (OPTION_FORCED_CRC) = strtol(argarg, NULL, 0);
                continue;
            }
            if (options_long_gcc("state-fps", &argc, &argv, &argarg)) {
                OPTS_OPTION_U32(OPTION_STATE_FPS) = strtol(argarg, NULL, 0);
                opts_set(opts.flags, EMULATE_STATE, true);
                continue;
            }
            if (options_long_gcc("redirout", &argc, &argv, &redirout)) {
                con_change(redirout, redirerr);
                continue;
            }
            if (options_long_gcc("redirerr", &argc, &argv, &redirerr)) {
                con_change(redirout, redirerr);
                continue;
            }
            if (options_long_gcc("config", &argc, &argv, &argarg)) {
                config = argarg;
                continue;
            }
            if (options_long_gcc("memdumpcols", &argc, &argv, &memdumpcols)) {
                OPTS_OPTION_U16(OPTION_MEMDUMPCOLS) = (uint16_t)strtol(memdumpcols, NULL, 10);
                continue;
            }
            if (options_long_gcc("progsrc", &argc, &argv, &argarg)) {
                OPTS_OPTION_STR(OPTION_PROGSRC) = argarg;
                continue;
            }

            /* show defaults (like pathscale) */
            if (!strcmp(argv[0]+1, "show-defaults")) {
                for (itr = 0; itr < COUNT_FLAGS; ++itr) {
                    if (!OPTS_FLAG(itr))
                        continue;

                    memset(buffer, 0, sizeof(buffer));
                    util_strtononcmd(opts_flag_list[itr].name, buffer, strlen(opts_flag_list[itr].name) + 1);

                    con_out("-f%s ", buffer);
                }
                for (itr = 0; itr < COUNT_WARNINGS; ++itr) {
                    if (!OPTS_WARN(itr))
                        continue;

                    memset(buffer, 0, sizeof(buffer));
                    util_strtononcmd(opts_warn_list[itr].name, buffer, strlen(opts_warn_list[itr].name) + 1);
                    con_out("-W%s ", buffer);
                }
                con_out("\n");
                exit(0);
            }

            if (!strcmp(argv[0]+1, "debug")) {
                OPTS_OPTION_BOOL(OPTION_DEBUG) = true;
                continue;
            }
            if (!strcmp(argv[0]+1, "dump")) {
                OPTS_OPTION_BOOL(OPTION_DUMP)  = true;
                continue;
            }
            if (!strcmp(argv[0]+1, "dumpfin")) {
                OPTS_OPTION_BOOL(OPTION_DUMPFIN) = true;
                continue;
            }
            if (!strcmp(argv[0]+1, "memchk")) {
                OPTS_OPTION_BOOL(OPTION_MEMCHK) = true;
                continue;
            }
            if (!strcmp(argv[0]+1, "nocolor")) {
                con_color(0);
                continue;
            }
            if (!strcmp(argv[0]+1, "coverage")) {
                OPTS_OPTION_BOOL(OPTION_COVERAGE) = true;
                continue;
            }

            switch (argv[0][1]) {
                /* -h, show usage but exit with 0 */
                case 'h':
                    usage();
                    exit(0);
                    /* break; never reached because of exit(0) */

                case 'v':
                    version();
                    exit(0);

                case 'E':
                    OPTS_OPTION_BOOL(OPTION_PP_ONLY) = true;
                    opts_set(opts.flags, FTEPP_PREDEFS, true); /* predefs on for -E */
                    break;

                /* debug turns on -flno */
                case 'g':
                    opts_setflag("LNO", true);
                    OPTS_OPTION_BOOL(OPTION_G) = true;
                    break;

                case 'q':
                    OPTS_OPTION_BOOL(OPTION_QUIET) = true;
                    break;

                case 'D':
                    if (!strlen(argv[0]+2)) {
                        con_err("expected name after -D\n");
                        exit(0);
                    }

                    if (!(argarg = strchr(argv[0] + 2, '='))) {
                        macro.name  = util_strdup(argv[0]+2);
                        macro.value = NULL;
                    } else {
                        *argarg='\0'; /* terminate for name */
                        macro.name  = util_strdup(argv[0]+2);
                        macro.value = util_strdup(argarg+1);
                    }
                    vec_push(ppems, macro);
                    break;

                /* handle all -fflags */
                case 'f':
                    util_strtocmd(argv[0]+2, argv[0]+2, strlen(argv[0]+2)+1);
                    if (!strcmp(argv[0]+2, "HELP") || *(argv[0]+2) == '?') {
                        con_out("Possible flags:\n\n");
                        for (itr = 0; itr < COUNT_FLAGS; ++itr) {
                            util_strtononcmd(opts_flag_list[itr].name, buffer, sizeof(buffer));
                            con_out(" -f%s\n", buffer);
                        }
                        exit(0);
                    }
                    else if (!strncmp(argv[0]+2, "NO_", 3)) {
                        if (!opts_setflag(argv[0]+5, false)) {
                            con_out("unknown flag: %s\n", argv[0]+2);
                            return false;
                        }
                    }
                    else if (!opts_setflag(argv[0]+2, true)) {
                        con_out("unknown flag: %s\n", argv[0]+2);
                        return false;
                    }
                    break;
                case 'W':
                    util_strtocmd(argv[0]+2, argv[0]+2, strlen(argv[0]+2)+1);
                    if (!strcmp(argv[0]+2, "HELP") || *(argv[0]+2) == '?') {
                        con_out("Possible warnings:\n");
                        for (itr = 0; itr < COUNT_WARNINGS; ++itr) {
                            util_strtononcmd(opts_warn_list[itr].name, buffer, sizeof(buffer));
                            con_out(" -W%s\n", buffer);
                            if (itr == WARN_DEBUG)
                                con_out("   Warnings included by -Wall:\n");
                        }
                        exit(0);
                    }
                    else if (!strcmp(argv[0]+2, "NO_ERROR") ||
                             !strcmp(argv[0]+2, "NO_ERROR_ALL"))
                    {
                        for (itr = 0; itr < GMQCC_ARRAY_COUNT(opts.werror); ++itr)
                            opts.werror[itr] = 0;
                        break;
                    }
                    else if (!strcmp(argv[0]+2, "ERROR") ||
                             !strcmp(argv[0]+2, "ERROR_ALL"))
                    {
                        opts_backup_non_Werror_all();
                        for (itr = 0; itr < GMQCC_ARRAY_COUNT(opts.werror); ++itr)
                            opts.werror[itr] = 0xFFFFFFFFL;
                        opts_restore_non_Werror_all();
                        break;
                    }
                    else if (!strcmp(argv[0]+2, "NONE")) {
                        for (itr = 0; itr < GMQCC_ARRAY_COUNT(opts.warn); ++itr)
                            opts.warn[itr] = 0;
                        break;
                    }
                    else if (!strcmp(argv[0]+2, "ALL")) {
                        opts_backup_non_Wall();
                        for (itr = 0; itr < GMQCC_ARRAY_COUNT(opts.warn); ++itr)
                            opts.warn[itr] = 0xFFFFFFFFL;
                        opts_restore_non_Wall();
                        break;
                    }
                    else if (!strncmp(argv[0]+2, "ERROR_", 6)) {
                        if (!opts_setwerror(argv[0]+8, true)) {
                            con_out("unknown warning: %s\n", argv[0]+2);
                            return false;
                        }
                    }
                    else if (!strncmp(argv[0]+2, "NO_ERROR_", 9)) {
                        if (!opts_setwerror(argv[0]+11, false)) {
                            con_out("unknown warning: %s\n", argv[0]+2);
                            return false;
                        }
                    }
                    else if (!strncmp(argv[0]+2, "NO_", 3)) {
                        if (!opts_setwarn(argv[0]+5, false)) {
                            con_out("unknown warning: %s\n", argv[0]+2);
                            return false;
                        }
                    }
                    else if (!opts_setwarn(argv[0]+2, true)) {
                        con_out("unknown warning: %s\n", argv[0]+2);
                        return false;
                    }
                    break;

                case 'O':
                    if (!options_witharg(&argc, &argv, &argarg)) {
                        con_out("option -O requires a numerical argument, or optimization name with an optional 'no-' prefix\n");
                        return false;
                    }
                    if (util_isdigit(argarg[0])) {
                        uint32_t val = (uint32_t)strtol(argarg, NULL, 10);
                        OPTS_OPTION_U32(OPTION_O) = val;
                        opts_setoptimlevel(val);
                    } else {
                        util_strtocmd(argarg, argarg, strlen(argarg)+1);
                        if (!strcmp(argarg, "HELP")) {
                            con_out("Possible optimizations:\n");
                            for (itr = 0; itr < COUNT_OPTIMIZATIONS; ++itr) {
                                util_strtononcmd(opts_opt_list[itr].name, buffer, sizeof(buffer));
                                con_out(" -O%-20s (-O%u)\n", buffer, opts_opt_oflag[itr]);
                            }
                            exit(0);
                        }
                        else if (!strcmp(argarg, "ALL"))
                            opts_setoptimlevel(OPTS_OPTION_U32(OPTION_O) = 9999);
                        else if (!strncmp(argarg, "NO_", 3)) {
                            /* constant folding cannot be turned off for obvious reasons */
                            if (!strcmp(argarg, "NO_CONST_FOLD") || !opts_setoptim(argarg+3, false)) {
                                con_out("unknown optimization: %s\n", argarg+3);
                                return false;
                            }
                        }
                        else {
                            if (!opts_setoptim(argarg, true)) {
                                con_out("unknown optimization: %s\n", argarg);
                                return false;
                            }
                        }
                    }
                    break;

                case 'o':
                    if (!options_witharg(&argc, &argv, &argarg)) {
                        con_out("option -o requires an argument: the output file name\n");
                        return false;
                    }
                    OPTS_OPTION_STR(OPTION_OUTPUT) = argarg;
                    opts_output_wasset = true;
                    break;

                case 'a':
                case 's':
                    item.type = argv[0][1] == 'a' ? TYPE_ASM : TYPE_SRC;
                    if (!options_witharg(&argc, &argv, &argarg)) {
                        con_out("option -a requires a filename %s\n",
                                (argv[0][1] == 'a' ? "containing QC-asm" : "containing a progs.src formatted list"));
                        return false;
                    }
                    item.filename = argarg;
                    vec_push(items, item);
                    break;

                case '-':
                    if (!argv[0][2]) {
                        /* anything following -- is considered a non-option argument */
                        argend = true;
                        break;
                    }
            /* All long options without arguments */
                    else if (!strcmp(argv[0]+2, "help")) {
                        usage();
                        exit(0);
                    }
                    else if (!strcmp(argv[0]+2, "version")) {
                        version();
                        exit(0);
                    }
                    else if (!strcmp(argv[0]+2, "quiet")) {
                        OPTS_OPTION_BOOL(OPTION_QUIET) = true;
                        break;
                    }
                    else if (!strcmp(argv[0]+2, "correct")) {
                        OPTS_OPTION_BOOL(OPTION_CORRECTION) = true;
                        break;
                    }
                    else if (!strcmp(argv[0]+2, "no-correct")) {
                        OPTS_OPTION_BOOL(OPTION_CORRECTION) = false;
                        break;
                    }
                    else if (!strcmp(argv[0]+2, "add-info")) {
                        OPTS_OPTION_BOOL(OPTION_ADD_INFO) = true;
                        break;
                    }
                    else {
            /* All long options with arguments */
                        if (options_long_witharg("output", &argc, &argv, &argarg)) {
                            OPTS_OPTION_STR(OPTION_OUTPUT) = argarg;
                            opts_output_wasset = true;
                        } else {
                            con_out("Unknown parameter: %s\n", argv[0]);
                            return false;
                        }
                    }
                    break;

                default:
                    con_out("Unknown parameter: %s\n", argv[0]);
                    return false;
            }
        }
        else
        {
            /* it's a QC filename */
            item.filename = argv[0];
            item.type     = TYPE_QC;
            vec_push(items, item);
        }
    }
    opts_ini_init(config);
    return true;
}

/* returns the line number, or -1 on error */
static bool progs_nextline(char **out, size_t *alen, fs_file_t *src) {
    int    len;
    char  *line;
    char  *start;
    char  *end;

    line = *out;
    len  = fs_file_getline(&line, alen, src);
    if (len == -1)
        return false;

    /* start at first non-blank */
    for (start = line; util_isspace(*start); ++start) {}
    /* end at the first non-blank */
    for (end = start; *end && !util_isspace(*end);  ++end)   {}

    *out = line;
    /* move the actual filename to the beginning */
    while (start != end) {
        *line++ = *start++;
    }
    *line = 0;
    return true;
}

int main(int argc, char **argv) {
    size_t          itr;
    int             retval           = 0;
    bool            operators_free   = false;
    bool            progs_src        = false;
    fs_file_t       *outfile         = NULL;
    struct parser_s *parser          = NULL;
    struct ftepp_s  *ftepp           = NULL;

    app_name = argv[0];
    con_init ();
    opts_init("progs.dat", COMPILER_QCC, (1024 << 3));

    util_seed(time(0));

    if (!options_parse(argc, argv)) {
        return usage();
    }

    if (OPTS_FLAG(TRUE_EMPTY_STRINGS) && OPTS_FLAG(FALSE_EMPTY_STRINGS)) {
        con_err("-ftrue-empty-strings and -ffalse-empty-strings are mutually exclusive");
        exit(EXIT_FAILURE);
    }

    /* the standard decides which set of operators to use */
    if (OPTS_OPTION_U32(OPTION_STANDARD) == COMPILER_GMQCC) {
        operators      = c_operators;
        operator_count = GMQCC_ARRAY_COUNT(c_operators);
    } else if (OPTS_OPTION_U32(OPTION_STANDARD) == COMPILER_FTEQCC) {
        operators      = fte_operators;
        operator_count = GMQCC_ARRAY_COUNT(fte_operators);
    } else {
        operators      = qcc_operators;
        operator_count = GMQCC_ARRAY_COUNT(qcc_operators);
    }

    if (operators == fte_operators) {
        /* fix ternary? */
        if (OPTS_FLAG(CORRECT_TERNARY)) {
            oper_info *newops;
            if (operators[operator_count-2].id != opid1(',') ||
                operators[operator_count-1].id != opid2(':','?'))
            {
                con_err("internal error: operator precedence table wasn't updated correctly!\n");
                exit(EXIT_FAILURE);
            }
            operators_free = true;
            newops = (oper_info*)mem_a(sizeof(operators[0]) * operator_count);
            memcpy(newops, operators, sizeof(operators[0]) * operator_count);
            memcpy(&newops[operator_count-2], &operators[operator_count-1], sizeof(newops[0]));
            memcpy(&newops[operator_count-1], &operators[operator_count-2], sizeof(newops[0]));
            newops[operator_count-2].prec = newops[operator_count-1].prec+1;
            operators = newops;
        }
    }

    if (OPTS_OPTION_BOOL(OPTION_DUMP)) {
        for (itr = 0; itr < COUNT_FLAGS; ++itr)
            con_out("Flag %s = %i\n",    opts_flag_list[itr].name, OPTS_FLAG(itr));
        for (itr = 0; itr < COUNT_WARNINGS; ++itr)
            con_out("Warning %s = %i\n", opts_warn_list[itr].name, OPTS_WARN(itr));

        con_out("output             = %s\n", OPTS_OPTION_STR(OPTION_OUTPUT));
        con_out("optimization level = %u\n", OPTS_OPTION_U32(OPTION_O));
        con_out("standard           = %u\n", OPTS_OPTION_U32(OPTION_STANDARD));
    }

    if (OPTS_OPTION_BOOL(OPTION_PP_ONLY)) {
        if (opts_output_wasset) {
            outfile = fs_file_open(OPTS_OPTION_STR(OPTION_OUTPUT), "wb");
            if (!outfile) {
                con_err("failed to open `%s` for writing\n", OPTS_OPTION_STR(OPTION_OUTPUT));
                retval = 1;
                goto cleanup;
            }
        }
        else {
            outfile = con_default_out();
        }
    }

    if (!OPTS_OPTION_BOOL(OPTION_PP_ONLY)) {
        if (!(parser = parser_create())) {
            con_err("failed to initialize parser\n");
            retval = 1;
            goto cleanup;
        }
    }

    if (OPTS_OPTION_BOOL(OPTION_PP_ONLY) || OPTS_FLAG(FTEPP)) {
        if (!(ftepp = ftepp_create())) {
            con_err("failed to initialize parser\n");
            retval = 1;
            goto cleanup;
        }
    }

    /* add macros */
    if (OPTS_OPTION_BOOL(OPTION_PP_ONLY) || OPTS_FLAG(FTEPP)) {
        for (itr = 0; itr < vec_size(ppems); itr++) {
            ftepp_add_macro(ftepp, ppems[itr].name, ppems[itr].value);
            mem_d(ppems[itr].name);

            /* can be null */
            if (ppems[itr].value)
                mem_d(ppems[itr].value);
        }
    }

    if (!vec_size(items)) {
        fs_file_t *src;
        char      *line    = NULL;
        size_t     linelen = 0;
        bool       hasline = false;

        progs_src = true;

        src = fs_file_open(OPTS_OPTION_STR(OPTION_PROGSRC), "rb");
        if (!src) {
            con_err("failed to open `%s` for reading\n", OPTS_OPTION_STR(OPTION_PROGSRC));
            retval = 1;
            goto cleanup;
        }

        while (progs_nextline(&line, &linelen, src)) {
            argitem item;

            if (!line[0] || (line[0] == '/' && line[1] == '/'))
                continue;

            if (hasline) {
                item.filename = util_strdup(line);
                item.type     = TYPE_QC;
                vec_push(items, item);
            } else if (!opts_output_wasset) {
                OPTS_OPTION_DUP(OPTION_OUTPUT) = util_strdup(line);
                hasline                        = true;
            }
        }

        fs_file_close(src);
        mem_d(line);
    }

    if (vec_size(items)) {
        if (!OPTS_OPTION_BOOL(OPTION_QUIET) &&
            !OPTS_OPTION_BOOL(OPTION_PP_ONLY))
        {
            con_out("Mode: %s\n", (progs_src ? "progs.src" : "manual"));
            con_out("There are %lu items to compile:\n", (unsigned long)vec_size(items));
        }

        for (itr = 0; itr < vec_size(items); ++itr) {
            if (!OPTS_OPTION_BOOL(OPTION_QUIET) &&
                !OPTS_OPTION_BOOL(OPTION_PP_ONLY))
            {
                con_out("  item: %s (%s)\n",
                       items[itr].filename,
                       ( (items[itr].type == TYPE_QC ? "qc" :
                         (items[itr].type == TYPE_ASM ? "asm" :
                         (items[itr].type == TYPE_SRC ? "progs.src" :
                         ("unknown"))))));
            }

            if (OPTS_OPTION_BOOL(OPTION_PP_ONLY)) {
                const char *out;
                if (!ftepp_preprocess_file(ftepp, items[itr].filename)) {
                    retval = 1;
                    goto cleanup;
                }
                out = ftepp_get(ftepp);
                if (out)
                    fs_file_printf(outfile, "%s", out);
                ftepp_flush(ftepp);
            }
            else {
                if (OPTS_FLAG(FTEPP)) {
                    const char *data;
                    if (!ftepp_preprocess_file(ftepp, items[itr].filename)) {
                        retval = 1;
                        goto cleanup;
                    }
                    data = ftepp_get(ftepp);
                    if (vec_size(data)) {
                        if (!parser_compile_string(parser, items[itr].filename, data, vec_size(data))) {
                            retval = 1;
                            goto cleanup;
                        }
                    }
                    ftepp_flush(ftepp);
                }
                else {
                    if (!parser_compile_file(parser, items[itr].filename)) {
                        retval = 1;
                        goto cleanup;
                    }
                }
            }

            if (progs_src) {
                mem_d(items[itr].filename);
                items[itr].filename = NULL;
            }
        }

        ftepp_finish(ftepp);
        ftepp = NULL;
        if (!OPTS_OPTION_BOOL(OPTION_PP_ONLY)) {
            if (!parser_finish(parser, OPTS_OPTION_STR(OPTION_OUTPUT))) {
                retval = 1;
                goto cleanup;
            }
        }
    }

cleanup:
    if (ftepp)
        ftepp_finish(ftepp);
    con_close();
    vec_free(items);
    vec_free(ppems);

    if (!OPTS_OPTION_BOOL(OPTION_PP_ONLY))
        if(parser) parser_cleanup(parser);

    /* free allocated option strings */
    for (itr = 0; itr < OPTION_COUNT; itr++)
        if (OPTS_OPTION_DUPED(itr))
            mem_d(OPTS_OPTION_STR(itr));

    if (operators_free)
        mem_d((void*)operators);

    lex_cleanup();
    stat_info();

    if (!retval && compile_errors)
        retval = 1;
    return retval;
}
