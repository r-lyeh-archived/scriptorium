/*
 * Copyright (C) 2012, 2013, 2014, 2015
 *     Dale Weiler
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
#define GMQCC_PLATFORM_HEADER /* TODO: eliminate! */
#include <stdlib.h>
#include <string.h>

#include "gmqcc.h"
#include "platform.h"

static const char *task_bins[] = {
    "./gmqcc",
    "./qcvm"
};

/*
 * TODO: Windows version
 * this implements a unique bi-directional popen-like function that
 * allows reading data from both stdout and stderr. And writing to
 * stdin :)
 *
 * Example of use:
 * FILE *handles[3] = task_popen("ls", "-l", "r");
 * if (!handles) { perror("failed to open stdin/stdout/stderr to ls");
 * // handles[0] = stdin
 * // handles[1] = stdout
 * // handles[2] = stderr
 *
 * task_pclose(handles); // to close
 */
#ifndef _WIN32
#include <sys/types.h>
#include <sys/wait.h>
#include <dirent.h>
#include <unistd.h>
typedef struct {
    fs_file_t *handles[3];
    int        pipes  [3];

    int stderr_fd;
    int stdout_fd;
    int pid;
} popen_t;

static fs_file_t **task_popen(const char *command, const char *mode) {
    int     inhandle  [2];
    int     outhandle [2];
    int     errhandle [2];
    int     trypipe;

    popen_t *data = (popen_t*)mem_a(sizeof(popen_t));

    /*
     * Parse the command now into a list for execv, this is a pain
     * in the ass.
     */
    char  *line = (char*)command;
    char **argv = NULL;
    {

        while (*line != '\0') {
            while (*line == ' ' || *line == '\t' || *line == '\n')
                *line++ = '\0';
            vec_push(argv, line);

            while (*line != '\0' && *line != ' ' &&
                   *line != '\t' && *line != '\n') line++;
        }
        vec_push(argv, (char *)0);
    }


    if ((trypipe = pipe(inhandle))  < 0) goto task_popen_error_0;
    if ((trypipe = pipe(outhandle)) < 0) goto task_popen_error_1;
    if ((trypipe = pipe(errhandle)) < 0) goto task_popen_error_2;

    if ((data->pid = fork()) > 0) {
        /* parent */
        close(inhandle  [0]);
        close(outhandle [1]);
        close(errhandle [1]);

        data->pipes  [0] = inhandle [1];
        data->pipes  [1] = outhandle[0];
        data->pipes  [2] = errhandle[0];

        data->handles[0] = (fs_file_t*)fdopen(inhandle [1], "w");
        data->handles[1] = (fs_file_t*)fdopen(outhandle[0], mode);
        data->handles[2] = (fs_file_t*)fdopen(errhandle[0], mode);

        /* sigh */
        vec_free(argv);
        return data->handles;
    } else if (data->pid == 0) {
        /* child */
        close(inhandle [1]);
        close(outhandle[0]);
        close(errhandle[0]);

        /* see piping documentation for this sillyness :P */
        dup2(inhandle [0], 0);
        dup2(outhandle[1], 1);
        dup2(errhandle[1], 2);

        execvp(*argv, argv);
        exit(EXIT_FAILURE);
    } else {
        /* fork failed */
        goto task_popen_error_3;
    }

task_popen_error_3: close(errhandle[0]), close(errhandle[1]);
task_popen_error_2: close(outhandle[0]), close(outhandle[1]);
task_popen_error_1: close(inhandle [0]), close(inhandle [1]);
task_popen_error_0:

    vec_free(argv);
    return NULL;
}

static int task_pclose(fs_file_t **handles) {
    popen_t *data   = (popen_t*)handles;
    int      status = 0;

    close(data->pipes[0]); /* stdin  */
    close(data->pipes[1]); /* stdout */
    close(data->pipes[2]); /* stderr */

    waitpid(data->pid, &status, 0);

    mem_d(data);

    return status;
}
#else
    #include <sys/stat.h>
    typedef struct {
        fs_file_t *handles[3];
        char       name_err[L_tmpnam];
        char       name_out[L_tmpnam];
    } popen_t;

    static fs_file_t **task_popen(const char *command, const char *mode) {
        char    *cmd  = NULL;
        popen_t *open = (popen_t*)mem_a(sizeof(popen_t));

        tmpnam(open->name_err);
        tmpnam(open->name_out);

        (void)mode; /* excluded */

        util_asprintf(&cmd, "%s -redirout=%s -redirerr=%s", command, open->name_out, open->name_err);

        system(cmd); /* HACK */
        open->handles[0] = NULL;
        open->handles[1] = fs_file_open(open->name_out, "r");
        open->handles[2] = fs_file_open(open->name_err, "r");

        mem_d(cmd);

        return open->handles;
    }

    static int task_pclose(fs_file_t **files) {
        popen_t *open = ((popen_t*)files);

        fs_file_close(files[1]);
        fs_file_close(files[2]);

        remove(open->name_err);
        remove(open->name_out);

        mem_d(open);

        return EXIT_SUCCESS;
    }
#   define popen _popen
#   define pclose _pclose
#endif /*! _WIN32 */

#define TASK_COMPILE    0
#define TASK_EXECUTE    1
/*
 * Task template system:
 *  templates are rules for a specific test, used to create a "task" that
 *  is executed with those set of rules (arguments, and what not). Tests
 *  that don't have a template with them cannot become tasks, since without
 *  the information for that test there is no way to properly "test" them.
 *  Rules for these templates are described in a template file, using a
 *  task template language.
 *
 *  The language is a basic finite statemachine, top-down single-line
 *  description language.
 *
 *  The languge is composed entierly of "tags" which describe a string of
 *  text for a task.  Think of it much like a configuration file.  Except
 *  it's been designed to allow flexibility and future support for prodecual
 *  semantics.
 *
 *  The following "tags" are suported by the language
 *
 *      D:
 *          Used to set a description of the current test, this must be
 *          provided, this tag is NOT optional.
 *
 *      T:
 *          Used to set the procedure for the given task, there are two
 *          options for this:
 *              -compile
 *                  This simply performs compilation only
 *              -execute
 *                  This will perform compilation and execution
 *              -fail
 *                  This will perform compilation, but requires
 *                  the compilation to fail in order to succeed.
 *
 *          This must be provided, this tag is NOT optional.
 *
 *      C:
 *          Used to set the compilation flags for the given task, this
 *          must be provided, this tag is NOT optional.
 *
 *      F:  Used to set some test suite flags, currently the only option
 *          is -no-defs (to including of defs.qh)
 *
 *      E:
 *          Used to set the execution flags for the given task. This tag
 *          must be provided if T == -execute, otherwise it's erroneous
 *          as compilation only takes place.
 *
 *      M:
 *          Used to describe a string of text that should be matched from
 *          the output of executing the task.  If this doesn't match the
 *          task fails.  This tag must be provided if T == -execute, otherwise
 *          it's erroneous as compilation only takes place.
 *
 *      I:
 *          Used to specify the INPUT source file to operate on, this must be
 *          provided, this tag is NOT optional
 *
 *
 *  Notes:
 *      These tags have one-time use, using them more than once will result
 *      in template compilation errors.
 *
 *      Lines beginning with # or // in the template file are comments and
 *      are ignored by the template parser.
 *
 *      Whitespace is optional, with exception to the colon ':' between the
 *      tag and it's assignment value/
 *
 *      The template compiler will detect erronrous tags (optional tags
 *      that need not be set), as well as missing tags, and error accordingly
 *      this will result in the task failing.
 */
typedef struct {
    char  *description;
    char  *compileflags;
    char  *executeflags;
    char  *proceduretype;
    char  *sourcefile;
    char  *tempfilename;
    char **comparematch;
    char  *rulesfile;
    char  *testflags;
} task_template_t;

/*
 * This is very much like a compiler code generator :-).  This generates
 * a value from some data observed from the compiler.
 */
static bool task_template_generate(task_template_t *tmpl, char tag, const char *file, size_t line, char *value, size_t *pad) {
    size_t desclen = 0;
    size_t filelen = 0;
    char **destval = NULL;

    if (!tmpl)
        return false;

    switch(tag) {
        case 'D': destval = &tmpl->description;    break;
        case 'T': destval = &tmpl->proceduretype;  break;
        case 'C': destval = &tmpl->compileflags;   break;
        case 'E': destval = &tmpl->executeflags;   break;
        case 'I': destval = &tmpl->sourcefile;     break;
        case 'F': destval = &tmpl->testflags;      break;
        default:
            con_printmsg(LVL_ERROR, __FILE__, __LINE__, 0, "internal error",
                "invalid tag `%c:` during code generation\n",
                tag
            );
            return false;
    }

    /*
     * Ensure if for the given tag, there already exists a
     * assigned value.
     */
    if (*destval) {
        con_printmsg(LVL_ERROR, file, line, 0, /*TODO: column for match*/ "compile error",
            "tag `%c:` already assigned value: %s\n",
            tag, *destval
        );
        return false;
    }

    /*
     * Strip any whitespace that might exist in the value for assignments
     * like "D:      foo"
     */
    if (value && *value && (*value == ' ' || *value == '\t'))
        value++;
    else if (!value)
        exit(EXIT_FAILURE);

    /*
     * Value will contain a newline character at the end, we need to strip
     * this otherwise kaboom, seriously, kaboom :P
     */
    if (strchr(value, '\n'))
        *strrchr(value, '\n')='\0';

    /*
     * Now allocate and set the actual value for the specific tag. Which
     * was properly selected and can be accessed with *destval.
     */
    *destval = util_strdup(value);


    if (*destval == tmpl->description) {
        /*
         * Create some padding for the description to align the
         * printing of the rules file.
         */
        if ((desclen = strlen(tmpl->description)) > pad[0])
            pad[0] = desclen;
    }

    if ((filelen = strlen(file)) > pad[2])
        pad[2] = filelen;

    return true;
}

static bool task_template_parse(const char *file, task_template_t *tmpl, fs_file_t *fp, size_t *pad) {
    char  *data = NULL;
    char  *back = NULL;
    size_t size = 0;
    size_t line = 1;

    if (!tmpl)
        return false;

    /* top down parsing */
    while (fs_file_getline(&back, &size, fp) != FS_FILE_EOF) {
        /* skip whitespace */
        data = back;
        if (*data && (*data == ' ' || *data == '\t'))
            data++;

        switch (*data) {
            /*
             * Handle comments inside task tmpl files.  We're strict
             * about the language for fun :-)
             */
            case '/':
                if (data[1] != '/') {
                    con_printmsg(LVL_ERROR, file, line, 0, /*TODO: column for match*/ "tmpl parse error",
                        "invalid character `/`, perhaps you meant `//` ?");

                    mem_d(back);
                    return false;
                }
            case '#':
                break;

            /*
             * Empty newlines are acceptable as well, so we handle that here
             * despite being just odd since there should't be that many
             * empty lines to begin with.
             */
            case '\r':
            case '\n':
                break;


            /*
             * Now begin the actual "tag" stuff.  This works as you expect
             * it to.
             */
            case 'D':
            case 'T':
            case 'C':
            case 'E':
            case 'I':
            case 'F':
                if (data[1] != ':') {
                    con_printmsg(LVL_ERROR, file, line, 0, /*TODO: column for match*/ "tmpl parse error",
                        "expected `:` after `%c`",
                        *data
                    );
                    goto failure;
                }
                if (!task_template_generate(tmpl, *data, file, line, &data[3], pad)) {
                    con_printmsg(LVL_ERROR, file, line, 0, /*TODO: column for match*/ "tmpl compile error",
                        "failed to generate for given task\n"
                    );
                    goto failure;
                }
                break;

            /*
             * Match requires it's own system since we allow multiple M's
             * for multi-line matching.
             */
            case 'M':
            {
                char *value = &data[3];
                if (data[1] != ':') {
                    con_printmsg(LVL_ERROR, file, line, 0, /*TODO: column for match*/ "tmpl parse error",
                        "expected `:` after `%c`",
                        *data
                    );
                    goto failure;
                }

                /*
                 * Value will contain a newline character at the end, we need to strip
                 * this otherwise kaboom, seriously, kaboom :P
                 */
                if (strrchr(value, '\n'))
                    *strrchr(value, '\n')='\0';
                else /* cppcheck: possible null pointer dereference */
                    exit(EXIT_FAILURE);

                vec_push(tmpl->comparematch, util_strdup(value));

                break;
            }

            default:
                con_printmsg(LVL_ERROR, file, line, 0, /*TODO: column for match*/ "tmpl parse error",
                    "invalid tag `%c`", *data
                );
                goto failure;
            /* no break required */
        }

        /* update line and free old sata */
        line++;
        mem_d(back);
        back = NULL;
    }
    if (back)
        mem_d(back);
    return true;

failure:
    mem_d (back);
    return false;
}

/*
 * Nullifies the template data: used during initialization of a new
 * template and free.
 */
static void task_template_nullify(task_template_t *tmpl) {
    if (!tmpl)
        return;

    tmpl->description    = NULL;
    tmpl->proceduretype  = NULL;
    tmpl->compileflags   = NULL;
    tmpl->executeflags   = NULL;
    tmpl->comparematch   = NULL;
    tmpl->sourcefile     = NULL;
    tmpl->tempfilename   = NULL;
    tmpl->rulesfile      = NULL;
    tmpl->testflags      = NULL;
}

static task_template_t *task_template_compile(const char *file, const char *dir, size_t *pad) {
    /* a page should be enough */
    char             fullfile[4096];
    size_t           filepadd = 0;
    fs_file_t       *tempfile = NULL;
    task_template_t *tmpl     = NULL;

    util_snprintf(fullfile,    sizeof(fullfile), "%s/%s", dir, file);

    tempfile = fs_file_open(fullfile, "r");
    tmpl     = (task_template_t*)mem_a(sizeof(task_template_t));
    task_template_nullify(tmpl);

    /*
     * Create some padding for the printing to align the
     * printing of the rules file to the console.
     */
    if ((filepadd = strlen(fullfile)) > pad[1])
        pad[1] = filepadd;

    tmpl->rulesfile = util_strdup(fullfile);

    /*
     * Esnure the file even exists for the task, this is pretty useless
     * to even do.
     */
    if (!tempfile) {
        con_err("template file: %s does not exist or invalid permissions\n",
            file
        );
        goto failure;
    }

    if (!task_template_parse(file, tmpl, tempfile, pad)) {
        con_err("template parse error: error during parsing\n");
        goto failure;
    }

    /*
     * Regardless procedure type, the following tags must exist:
     *  D
     *  T
     *  C
     *  I
     */
    if (!tmpl->description) {
        con_err("template compile error: %s missing `D:` tag\n", file);
        goto failure;
    }
    if (!tmpl->proceduretype) {
        con_err("template compile error: %s missing `T:` tag\n", file);
        goto failure;
    }
    if (!tmpl->compileflags) {
        con_err("template compile error: %s missing `C:` tag\n", file);
        goto failure;
    }
    if (!tmpl->sourcefile) {
        con_err("template compile error: %s missing `I:` tag\n", file);
        goto failure;
    }

    /*
     * Now lets compile the template, compilation is really just
     * the process of validating the input.
     */
    if (!strcmp(tmpl->proceduretype, "-compile")) {
        if (tmpl->executeflags)
            con_err("template compile warning: %s erroneous tag `E:` when only compiling\n", file);
        if (tmpl->comparematch)
            con_err("template compile warning: %s erroneous tag `M:` when only compiling\n", file);
        goto success;
    } else if (!strcmp(tmpl->proceduretype, "-execute")) {
        if (!tmpl->executeflags) {
            /* default to $null */
            tmpl->executeflags = util_strdup("$null");
        }
        if (!tmpl->comparematch) {
            con_err("template compile error: %s missing `M:` tag (use `$null` for exclude)\n", file);
            goto failure;
        }
    } else if (!strcmp(tmpl->proceduretype, "-fail")) {
        if (tmpl->executeflags)
            con_err("template compile warning: %s erroneous tag `E:` when only failing\n", file);
        if (tmpl->comparematch)
            con_err("template compile warning: %s erroneous tag `M:` when only failing\n", file);
    } else if (!strcmp(tmpl->proceduretype, "-diagnostic")) {
        if (tmpl->executeflags)
            con_err("template compile warning: %s erroneous tag `E:` when only diagnostic\n", file);
        if (!tmpl->comparematch) {
            con_err("template compile error: %s missing `M:` tag (use `$null` for exclude)\n", file);
            goto failure;
        }
    } else if (!strcmp(tmpl->proceduretype, "-pp")) {
        if (tmpl->executeflags)
            con_err("template compile warning: %s erroneous tag `E:` when only preprocessing\n", file);
        if (!tmpl->comparematch) {
            con_err("template compile error: %s missing `M:` tag (use `$null` for exclude)\n", file);
            goto failure;
        }
    } else {
        con_err("template compile error: %s invalid procedure type: %s\n", file, tmpl->proceduretype);
        goto failure;
    }

success:
    fs_file_close(tempfile);
    return tmpl;

failure:
    /*
     * The file might not exist and we jump here when that doesn't happen
     * so the check to see if it's not null here is required.
     */
    if (tempfile)
        fs_file_close(tempfile);
    mem_d (tmpl);

    return NULL;
}

static void task_template_destroy(task_template_t *tmpl) {
    if (!tmpl)
        return;

    if (tmpl->description)    mem_d(tmpl->description);
    if (tmpl->proceduretype)  mem_d(tmpl->proceduretype);
    if (tmpl->compileflags)   mem_d(tmpl->compileflags);
    if (tmpl->executeflags)   mem_d(tmpl->executeflags);
    if (tmpl->sourcefile)     mem_d(tmpl->sourcefile);
    if (tmpl->rulesfile)      mem_d(tmpl->rulesfile);
    if (tmpl->testflags)      mem_d(tmpl->testflags);

    /*
     * Delete all allocated string for task tmpl then destroy the
     * main vector.
     */
    {
        size_t i = 0;
        for (; i < vec_size(tmpl->comparematch); i++)
            mem_d(tmpl->comparematch[i]);

        vec_free(tmpl->comparematch);
    }

    /*
     * Nullify all the template members otherwise NULL comparision
     * checks will fail if tmpl pointer is reused.
     */
    mem_d(tmpl->tempfilename);
    mem_d(tmpl);
}

/*
 * Now comes the task manager, this system allows adding tasks in and out
 * of a task list.  This is the executor of the tasks essentially as well.
 */
typedef struct {
    task_template_t *tmpl;
    fs_file_t       **runhandles;
    fs_file_t       *stderrlog;
    fs_file_t       *stdoutlog;
    char            *stdoutlogfile;
    char            *stderrlogfile;
    bool             compiled;
} task_t;

static task_t *task_tasks = NULL;

/*
 * Read a directory and searches for all template files in it
 * which is later used to run all tests.
 */
static bool task_propagate(const char *curdir, size_t *pad, const char *defs) {
    bool             success = true;
    fs_dir_t        *dir;
    fs_dirent_t     *files;
    struct stat      directory;
    char             buffer[4096];
    size_t           found = 0;
    char           **directories = NULL;
    char            *claim = util_strdup(curdir);
    size_t           i;

    vec_push(directories, claim);
    dir = fs_dir_open(claim);

    /*
     * Generate a list of subdirectories since we'll be checking them too
     * for tmpl files.
     */
    while ((files = fs_dir_read(dir))) {
        util_asprintf(&claim, "%s/%s", curdir, files->d_name);
        if (stat(claim, &directory) == -1) {
            fs_dir_close(dir);
            mem_d(claim);
            return false;
        }

        if (S_ISDIR(directory.st_mode) && files->d_name[0] != '.') {
            vec_push(directories, claim);
        } else {
            mem_d(claim);
            claim = NULL;
        }
    }
    fs_dir_close(dir);

    /*
     * Now do all the work, by touching all the directories inside
     * test as well and compile the task templates into data we can
     * use to run the tests.
     */
    for (i = 0; i < vec_size(directories); i++) {
        dir = fs_dir_open(directories[i]);

        while ((files = fs_dir_read(dir))) {
            util_snprintf(buffer, sizeof(buffer), "%s/%s", directories[i], files->d_name);
            if (stat(buffer, &directory) == -1) {
                con_err("internal error: stat failed, aborting\n");
                abort();
            }

            if (S_ISDIR(directory.st_mode))
                continue;

            /*
             * We made it here, which concludes the file/directory is not
             * actually a directory, so it must be a file :)
             */
            if (strcmp(files->d_name + strlen(files->d_name) - 5, ".tmpl") == 0) {
                task_template_t *tmpl = task_template_compile(files->d_name, directories[i], pad);
                char             buf[4096]; /* one page should be enough */
                const char      *qcflags = NULL;
                task_t           task;

                memset(&task, 0, sizeof(task));

                found ++;
                if (!tmpl) {
                    con_err("error compiling task template: %s\n", files->d_name);
                    success = false;
                    continue;
                }
                /*
                 * Generate a temportary file name for the output binary
                 * so we don't trample over an existing one.
                 */
                tmpl->tempfilename = NULL;
                util_asprintf(&tmpl->tempfilename, "%s/TMPDAT.%s.dat", directories[i], files->d_name);

                /*
                 * Additional QCFLAGS enviroment variable may be used
                 * to test compile flags for all tests.  This needs to be
                 * BEFORE other flags (so that the .tmpl can override them)
                 */
                qcflags = platform_getenv("QCFLAGS");

                /*
                 * Generate the command required to open a pipe to a process
                 * which will be refered to with a handle in the task for
                 * reading the data from the pipe.
                 */
                if (strcmp(tmpl->proceduretype, "-pp")) {
                    if (qcflags) {
                        if (tmpl->testflags && !strcmp(tmpl->testflags, "-no-defs")) {
                            util_snprintf(buf, sizeof(buf), "%s %s/%s %s %s -o %s",
                                task_bins[TASK_COMPILE],
                                directories[i],
                                tmpl->sourcefile,
                                qcflags,
                                tmpl->compileflags,
                                tmpl->tempfilename
                            );
                        } else {
                            util_snprintf(buf, sizeof(buf), "%s %s/%s %s/%s %s %s -o %s",
                                task_bins[TASK_COMPILE],
                                curdir,
                                defs,
                                directories[i],
                                tmpl->sourcefile,
                                qcflags,
                                tmpl->compileflags,
                                tmpl->tempfilename
                            );
                        }
                    } else {
                        if (tmpl->testflags && !strcmp(tmpl->testflags, "-no-defs")) {
                            util_snprintf(buf, sizeof(buf), "%s %s/%s %s -o %s",
                                task_bins[TASK_COMPILE],
                                directories[i],
                                tmpl->sourcefile,
                                tmpl->compileflags,
                                tmpl->tempfilename
                            );
                        } else {
                            util_snprintf(buf, sizeof(buf), "%s %s/%s %s/%s %s -o %s",
                                task_bins[TASK_COMPILE],
                                curdir,
                                defs,
                                directories[i],
                                tmpl->sourcefile,
                                tmpl->compileflags,
                                tmpl->tempfilename
                            );
                        }
                    }
                } else {
                    /* Preprocessing (qcflags mean shit all here we don't allow them) */
                    if (tmpl->testflags && !strcmp(tmpl->testflags, "-no-defs")) {
                        util_snprintf(buf, sizeof(buf), "%s -E %s/%s %s -o %s",
                            task_bins[TASK_COMPILE],
                            directories[i],
                            tmpl->sourcefile,
                            tmpl->compileflags,
                            tmpl->tempfilename
                        );
                    } else {
                        util_snprintf(buf, sizeof(buf), "%s -E %s/%s %s/%s %s -o %s",
                            task_bins[TASK_COMPILE],
                            curdir,
                            defs,
                            directories[i],
                            tmpl->sourcefile,
                            tmpl->compileflags,
                            tmpl->tempfilename
                        );
                    }
                }

                /*
                 * The task template was compiled, now lets create a task from
                 * the template data which has now been propagated.
                 */
                task.tmpl = tmpl;
                if (!(task.runhandles = task_popen(buf, "r"))) {
                    con_err("error opening pipe to process for test: %s\n", tmpl->description);
                    success = false;
                    continue;
                }

                /*
                 * Open up some file desciptors for logging the stdout/stderr
                 * to our own.
                 */
                util_snprintf(buf,  sizeof(buf), "%s.stdout", tmpl->tempfilename);
                task.stdoutlogfile = util_strdup(buf);
                if (!(task.stdoutlog     = fs_file_open(buf, "w"))) {
                    con_err("error opening %s for stdout\n", buf);
                    continue;
                }

                util_snprintf(buf,  sizeof(buf), "%s.stderr", tmpl->tempfilename);
                task.stderrlogfile = util_strdup(buf);
                if (!(task.stderrlog = fs_file_open(buf, "w"))) {
                    con_err("error opening %s for stderr\n", buf);
                    continue;
                }

                vec_push(task_tasks, task);
            }
        }

        fs_dir_close(dir);
        mem_d(directories[i]); /* free claimed memory */
    }
    vec_free(directories);

    return success;
}

/*
 * Task precleanup removes any existing temporary files or log files
 * left behind from a previous invoke of the test-suite.
 */
static void task_precleanup(const char *curdir) {
    fs_dir_t     *dir;
    fs_dirent_t  *files;
    char          buffer[4096];

    dir = fs_dir_open(curdir);

    while ((files = fs_dir_read(dir))) {
        if (strstr(files->d_name, "TMP")     ||
            strstr(files->d_name, ".stdout") ||
            strstr(files->d_name, ".stderr") ||
            strstr(files->d_name, ".dat"))
        {
            util_snprintf(buffer, sizeof(buffer), "%s/%s", curdir, files->d_name);
            if (remove(buffer))
                con_err("error removing temporary file: %s\n", buffer);
        }
    }

    fs_dir_close(dir);
}

static void task_destroy(void) {
    /*
     * Free all the data in the task list and finally the list itself
     * then proceed to cleanup anything else outside the program like
     * temporary files.
     */
    size_t i;
    for (i = 0; i < vec_size(task_tasks); i++) {
        /*
         * Close any open handles to files or processes here.  It's mighty
         * annoying to have to do all this cleanup work.
         */
        if (task_tasks[i].stdoutlog)  fs_file_close (task_tasks[i].stdoutlog);
        if (task_tasks[i].stderrlog)  fs_file_close (task_tasks[i].stderrlog);

        /*
         * Only remove the log files if the test actually compiled otherwise
         * forget about it (or if it didn't compile, and the procedure type
         * was set to -fail (meaning it shouldn't compile) .. stil remove)
         */
        if (task_tasks[i].compiled || !strcmp(task_tasks[i].tmpl->proceduretype, "-fail")) {
            if (remove(task_tasks[i].stdoutlogfile))
                con_err("error removing stdout log file: %s\n", task_tasks[i].stdoutlogfile);
            if (remove(task_tasks[i].stderrlogfile))
                con_err("error removing stderr log file: %s\n", task_tasks[i].stderrlogfile);

            (void)!remove(task_tasks[i].tmpl->tempfilename);
        }

        /* free util_strdup data for log files */
        mem_d(task_tasks[i].stdoutlogfile);
        mem_d(task_tasks[i].stderrlogfile);

        task_template_destroy(task_tasks[i].tmpl);
    }
    vec_free(task_tasks);
}

/*
 * This executes the QCVM task for a specificly compiled progs.dat
 * using the template passed into it for call-flags and user defined
 * messages IF the procedure type is -execute, otherwise it matches
 * the preprocessor output.
 */
static bool task_trymatch(size_t i, char ***line) {
    bool             success = true;
    bool             process = true;
    int              retval  = EXIT_SUCCESS;
    fs_file_t       *execute;
    char             buffer[4096];
    task_template_t *tmpl = task_tasks[i].tmpl;

    memset  (buffer,0,sizeof(buffer));

    if (!strcmp(tmpl->proceduretype, "-execute")) {
        /*
         * Drop the execution flags for the QCVM if none where
         * actually specified.
         */
        if (!strcmp(tmpl->executeflags, "$null")) {
            util_snprintf(buffer,  sizeof(buffer), "%s %s",
                task_bins[TASK_EXECUTE],
                tmpl->tempfilename
            );
        } else {
            util_snprintf(buffer,  sizeof(buffer), "%s %s %s",
                task_bins[TASK_EXECUTE],
                tmpl->executeflags,
                tmpl->tempfilename
            );
        }

        execute = (fs_file_t*)popen(buffer, "r");
        if (!execute)
            return false;
    } else if (!strcmp(tmpl->proceduretype, "-pp")) {
        /*
         * we're preprocessing, which means we need to read int
         * the produced file and do some really weird shit.
         */
        if (!(execute = fs_file_open(tmpl->tempfilename, "r")))
            return false;

        process = false;
    } else {
        /*
         * we're testing diagnostic output, which means it will be
         * in runhandles[2] (stderr) since that is where the compiler
         * puts it's errors.
         */
        if (!(execute = fs_file_open(task_tasks[i].stderrlogfile, "r")))
            return false;

        process = false;
    }

    /*
     * Now lets read the lines and compare them to the matches we expect
     * and handle accordingly.
     */
    {
        char  *data    = NULL;
        size_t size    = 0;
        size_t compare = 0;

        while (fs_file_getline(&data, &size, execute) != FS_FILE_EOF) {
            if (!strcmp(data, "No main function found\n")) {
                con_err("test failure: `%s` (No main function found) [%s]\n",
                    tmpl->description,
                    tmpl->rulesfile
                );
                if (!process)
                    fs_file_close(execute);
                else
                    pclose((FILE*)execute);
                return false;
            }

            /*
             * Trim newlines from data since they will just break our
             * ability to properly validate matches.
             */
            if  (strrchr(data, '\n'))
                *strrchr(data, '\n') = '\0';

            /*
             * We remove the file/directory and stuff from the error
             * match messages when testing diagnostics.
             */
            if(!strcmp(tmpl->proceduretype, "-diagnostic")) {
                if (strstr(data, "there have been errors, bailing out"))
                    continue; /* ignore it */
                if (strstr(data, ": error: ")) {
                    char *claim = util_strdup(data + (strstr(data, ": error: ") - data) + 9);
                    mem_d(data);
                    data = claim;
                }
            }

            /*
             * We need to ignore null lines for when -pp is used (preprocessor), since
             * the preprocessor is likely to create empty newlines in certain macro
             * instantations, otherwise it's in the wrong nature to ignore empty newlines.
             */
            if (!strcmp(tmpl->proceduretype, "-pp") && !*data)
                continue;

            if (vec_size(tmpl->comparematch) > compare) {
                if (strcmp(data, tmpl->comparematch[compare++])) {
                    success = false;
                }
            } else {
                success = false;
            }

            /*
             * Copy to output vector for diagnostics if execution match
             * fails.
             */
            vec_push(*line, data);

            /* reset */
            data = NULL;
            size = 0;
        }

        if (compare != vec_size(tmpl->comparematch))
            success = false;

        mem_d(data);
        data = NULL;
    }

    if (process)
        retval = pclose((FILE*)execute);
    else
        fs_file_close(execute);

    return success && retval == EXIT_SUCCESS;
}

static const char *task_type(task_template_t *tmpl) {
    if (!strcmp(tmpl->proceduretype, "-pp"))
        return "type: preprocessor";
    if (!strcmp(tmpl->proceduretype, "-execute"))
        return "type: execution";
    if (!strcmp(tmpl->proceduretype, "-compile"))
        return "type: compile";
    if (!strcmp(tmpl->proceduretype, "-diagnostic"))
        return "type: diagnostic";
    return "type: fail";
}

/*
 * This schedualizes all tasks and actually runs them individually
 * this is generally easy for just -compile variants.  For compile and
 * execution this takes more work since a task needs to be generated
 * from thin air and executed INLINE.
 */
#include <math.h>
static size_t task_schedualize(size_t *pad) {
    char   space[2][64];
    bool   execute  = false;
    char  *data     = NULL;
    char **match    = NULL;
    size_t size     = 0;
    size_t i        = 0;
    size_t j        = 0;
    size_t failed   = 0;
    int    status   = 0;

    util_snprintf(space[0], sizeof(space[0]), "%d", (int)vec_size(task_tasks));

    for (; i < vec_size(task_tasks); i++) {
        memset(space[1], 0, sizeof(space[1]));
        util_snprintf(space[1], sizeof(space[1]), "%d", (int)(i + 1));

        con_out("test #%u %*s", i + 1, strlen(space[0]) - strlen(space[1]), "");

        /*
         * Generate a task from thin air if it requires execution in
         * the QCVM.
         */

        /* diagnostic is not executed, but compare tested instead, like preproessor */
        execute = !! (!strcmp(task_tasks[i].tmpl->proceduretype, "-execute")) ||
                     (!strcmp(task_tasks[i].tmpl->proceduretype, "-pp"))      ||
                     (!strcmp(task_tasks[i].tmpl->proceduretype, "-diagnostic"));

        /*
         * We assume it compiled before we actually compiled :).  On error
         * we change the value
         */
        task_tasks[i].compiled = true;

        /*
         * Read data from stdout first and pipe that stuff into a log file
         * then we do the same for stderr.
         */
        while (fs_file_getline(&data, &size, task_tasks[i].runhandles[1]) != FS_FILE_EOF) {
            fs_file_puts(task_tasks[i].stdoutlog, data);

            if (strstr(data, "failed to open file")) {
                task_tasks[i].compiled = false;
                execute                = false;
            }
        }
        while (fs_file_getline(&data, &size, task_tasks[i].runhandles[2]) != FS_FILE_EOF) {
            /*
             * If a string contains an error we just dissalow execution
             * of it in the vm.
             *
             * TODO: make this more percise, e.g if we print a warning
             * that refers to a variable named error, or something like
             * that .. then this will blowup :P
             */
            if (strstr(data, "error") && strcmp(task_tasks[i].tmpl->proceduretype, "-diagnostic")) {
                execute                = false;
                task_tasks[i].compiled = false;
            }

            fs_file_puts (task_tasks[i].stderrlog, data);
            fs_file_flush(task_tasks[i].stderrlog); /* fast flush for read */
        }

        if (!task_tasks[i].compiled && strcmp(task_tasks[i].tmpl->proceduretype, "-fail")) {
            con_out("failure:   `%s` %*s %*s\n",
                task_tasks[i].tmpl->description,
                (pad[0] + pad[1] - strlen(task_tasks[i].tmpl->description)) + (strlen(task_tasks[i].tmpl->rulesfile) - pad[1]),
                task_tasks[i].tmpl->rulesfile,
                (pad[1] + pad[2] - strlen(task_tasks[i].tmpl->rulesfile)) + (strlen("(failed to compile)") - pad[2]),
                "(failed to compile)"
            );
            failed++;
            continue;
        }

        status = task_pclose(task_tasks[i].runhandles);
        if ((!strcmp(task_tasks[i].tmpl->proceduretype, "-fail") && status == EXIT_SUCCESS)
        ||  ( strcmp(task_tasks[i].tmpl->proceduretype, "-fail") && status == EXIT_FAILURE)) {
            con_out("failure:   `%s` %*s %*s\n",
                task_tasks[i].tmpl->description,
                (pad[0] + pad[1] - strlen(task_tasks[i].tmpl->description)) + (strlen(task_tasks[i].tmpl->rulesfile) - pad[1]),
                task_tasks[i].tmpl->rulesfile,
                (pad[1] + pad[2] - strlen(task_tasks[i].tmpl->rulesfile)) + (strlen("(compiler didn't return exit success)") - pad[2]),
                "(compiler didn't return exit success)"
            );
            failed++;
            continue;
        }

        if (!execute) {
            con_out("succeeded: `%s` %*s %*s\n",
                task_tasks[i].tmpl->description,
                (pad[0] + pad[1] - strlen(task_tasks[i].tmpl->description)) + (strlen(task_tasks[i].tmpl->rulesfile) - pad[1]),
                task_tasks[i].tmpl->rulesfile,
                (pad[1] + pad[2] - strlen(task_tasks[i].tmpl->rulesfile)) + (strlen(task_type(task_tasks[i].tmpl)) - pad[2]),
                task_type(task_tasks[i].tmpl)

            );
            continue;
        }

        /*
         * If we made it here that concludes the task is to be executed
         * in the virtual machine (or the preprocessor output needs to
         * be matched).
         */
        if (!task_trymatch(i, &match)) {
            size_t d = 0;

            con_out("failure:   `%s` %*s %*s\n",
                task_tasks[i].tmpl->description,
                (pad[0] + pad[1] - strlen(task_tasks[i].tmpl->description)) + (strlen(task_tasks[i].tmpl->rulesfile) - pad[1]),
                task_tasks[i].tmpl->rulesfile,
                (pad[1] + pad[2] - strlen(task_tasks[i].tmpl->rulesfile)) + (strlen(
                    (strcmp(task_tasks[i].tmpl->proceduretype, "-pp"))
                        ? "(invalid results from execution)"
                        : (strcmp(task_tasks[i].tmpl->proceduretype, "-diagnostic"))
                            ? "(invalid results from preprocessing)"
                            : "(invalid results from compiler diagnsotics)"
                ) - pad[2]),
                (strcmp(task_tasks[i].tmpl->proceduretype, "-pp"))
                    ? "(invalid results from execution)"
                    : (strcmp(task_tasks[i].tmpl->proceduretype, "-diagnostic"))
                            ? "(invalid results from preprocessing)"
                            : "(invalid results from compiler diagnsotics)"
            );

            /*
             * Print nicely formatted expected match lists to console error
             * handler for the all the given matches in the template file and
             * what was actually returned from executing.
             */
            con_out("    Expected From %u Matches: (got %u Matches)\n",
                vec_size(task_tasks[i].tmpl->comparematch),
                vec_size(match)
            );
            for (; d < vec_size(task_tasks[i].tmpl->comparematch); d++) {
                char  *select = task_tasks[i].tmpl->comparematch[d];
                size_t length = 60 - strlen(select);

                con_out("        Expected: \"%s\"", select);
                while (length --)
                    con_out(" ");
                con_out("| Got: \"%s\"\n", (d >= vec_size(match)) ? "<<nothing else to compare>>" : match[d]);
            }

            /*
             * Print the non-expected out (since we are simply not expecting it)
             * This will help track down bugs in template files that fail to match
             * something.
             */
            if (vec_size(match) > vec_size(task_tasks[i].tmpl->comparematch)) {
                for (d = 0; d < vec_size(match) - vec_size(task_tasks[i].tmpl->comparematch); d++) {
                    con_out("        Expected: Nothing                                                       | Got: \"%s\"\n",
                        match[d + vec_size(task_tasks[i].tmpl->comparematch)]
                    );
                }
            }


            for (j = 0; j < vec_size(match); j++)
                mem_d(match[j]);
            vec_free(match);
            failed++;
            continue;
        }

        for (j = 0; j < vec_size(match); j++)
            mem_d(match[j]);
        vec_free(match);

        con_out("succeeded: `%s` %*s %*s\n",
            task_tasks[i].tmpl->description,
            (pad[0] + pad[1] - strlen(task_tasks[i].tmpl->description)) + (strlen(task_tasks[i].tmpl->rulesfile) - pad[1]),
            task_tasks[i].tmpl->rulesfile,
            (pad[1] + pad[2] - strlen(task_tasks[i].tmpl->rulesfile)) + (strlen(task_type(task_tasks[i].tmpl))- pad[2]),
            task_type(task_tasks[i].tmpl)

        );
    }
    mem_d(data);
    return failed;
}

/*
 * This is the heart of the whole test-suite process.  This cleans up
 * any existing temporary files left behind as well as log files left
 * behind.  Then it propagates a list of tests from `curdir` by scaning
 * it for template files and compiling them into tasks, in which it
 * schedualizes them (executes them) and actually reports errors and
 * what not.  It then proceeds to destroy the tasks and return memory
 * it's the engine :)
 *
 * It returns true of tests could be propagated, otherwise it returns
 * false.
 *
 * It expects con_init() was called before hand.
 */
static GMQCC_WARN bool test_perform(const char *curdir, const char *defs) {
    size_t             failed       = false;
    static const char *default_defs = "defs.qh";

    size_t pad[] = {
        /* test ### [succeed/fail]: `description`      [tests/template.tmpl]     [type] */
                    0,                                 0,                        0
    };

    /*
     * If the default definition file isn't set to anything.  We will
     * use the default_defs here, which is "defs.qc"
     */
    if (!defs) {
        defs = default_defs;
    }


    task_precleanup(curdir);
    if (!task_propagate(curdir, pad, defs)) {
        con_err("error: failed to propagate tasks\n");
        task_destroy();
        return false;
    }
    /*
     * If we made it here all tasks where propagated from their resultant
     * template file.  So we can start the FILO scheduler, this has been
     * designed in the most thread-safe way possible for future threading
     * it's designed to prevent lock contention, and possible syncronization
     * issues.
     */
    failed = task_schedualize(pad);
    if (failed)
        con_out("%u out of %u tests failed\n", failed, vec_size(task_tasks));
    task_destroy();

    return (failed) ? false : true;
}

/*
 * Fancy GCC-like LONG parsing allows things like --opt=param with
 * assignment operator.  This is used for redirecting stdout/stderr
 * console to specific files of your choice.
 */
static bool parsecmd(const char *optname, int *argc_, char ***argv_, char **out, int ds, bool split) {
    int  argc   = *argc_;
    char **argv = *argv_;

    size_t len = strlen(optname);

    if (strncmp(argv[0]+ds, optname, len))
        return false;

    /* it's --optname, check how the parameter is supplied */
    if (argv[0][ds+len] == '=') {
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

int main(int argc, char **argv) {
    bool          succeed  = false;
    char         *redirout = (char*)stdout;
    char         *redirerr = (char*)stderr;
    char         *defs     = NULL;

    con_init();
    OPTS_OPTION_U16(OPTION_MEMDUMPCOLS) = 16;

    /*
     * Command line option parsing commences now We only need to support
     * a few things in the test suite.
     */
    while (argc > 1) {
        ++argv;
        --argc;

        if (argv[0][0] == '-') {
            if (parsecmd("redirout", &argc, &argv, &redirout, 1, false))
                continue;
            if (parsecmd("redirerr", &argc, &argv, &redirerr, 1, false))
                continue;
            if (parsecmd("defs",     &argc, &argv, &defs,     1, false))
                continue;

            con_change(redirout, redirerr);

            if (!strcmp(argv[0]+1, "debug")) {
                OPTS_OPTION_BOOL(OPTION_DEBUG) = true;
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

            con_err("invalid argument %s\n", argv[0]+1);
            return -1;
        }
    }
    con_change(redirout, redirerr);
    succeed = test_perform("tests", defs);
    stat_info();

    return (succeed) ? EXIT_SUCCESS : EXIT_FAILURE;
}
