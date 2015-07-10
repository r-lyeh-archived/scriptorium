/*
 * Copyright (C) 2012, 2013, 2014, 2015
 *     Wolfgang Bumiller
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
#ifndef QCVM_LOOP
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "gmqcc.h"

static void loaderror(const char *fmt, ...)
{
    int     err = errno;
    va_list ap;
    va_start(ap, fmt);
    vprintf(fmt, ap);
    va_end(ap);
    printf(": %s\n", util_strerror(err));
}

static void qcvmerror(qc_program_t *prog, const char *fmt, ...)
{
    va_list ap;

    prog->vmerror++;

    va_start(ap, fmt);
    vprintf(fmt, ap);
    va_end(ap);
    putchar('\n');
}

qc_program_t* prog_load(const char *filename, bool skipversion)
{
    prog_header_t   header;
    qc_program_t   *prog;
    size_t          i;
    fs_file_t      *file  = fs_file_open(filename, "rb");

    /* we need all those in order to support INSTR_STATE: */
    bool            has_self      = false,
                    has_time      = false,
                    has_think     = false,
                    has_nextthink = false,
                    has_frame     = false;

    if (!file)
        return NULL;

    if (fs_file_read(&header, sizeof(header), 1, file) != 1) {
        loaderror("failed to read header from '%s'", filename);
        fs_file_close(file);
        return NULL;
    }

    util_swap_header(&header);

    if (!skipversion && header.version != 6) {
        loaderror("header says this is a version %i progs, we need version 6\n", header.version);
        fs_file_close(file);
        return NULL;
    }

    prog = (qc_program_t*)mem_a(sizeof(qc_program_t));
    if (!prog) {
        fs_file_close(file);
        fprintf(stderr, "failed to allocate program data\n");
        return NULL;
    }
    memset(prog, 0, sizeof(*prog));

    prog->entityfields = header.entfield;
    prog->crc16 = header.crc16;

    prog->filename = util_strdup(filename);
    if (!prog->filename) {
        loaderror("failed to store program name");
        goto error;
    }

#define read_data(hdrvar, progvar, reserved)                           \
    if (fs_file_seek(file, header.hdrvar.offset, SEEK_SET) != 0) {     \
        loaderror("seek failed");                                      \
        goto error;                                                    \
    }                                                                  \
    if (fs_file_read (                                                 \
            vec_add(prog->progvar, header.hdrvar.length + reserved),   \
            sizeof(*prog->progvar),                                    \
            header.hdrvar.length,                                      \
            file                                                       \
        )!= header.hdrvar.length                                       \
    ) {                                                                \
        loaderror("read failed");                                      \
        goto error;                                                    \
    }
#define read_data1(x)    read_data(x, x, 0)
#define read_data2(x, y) read_data(x, x, y)

    read_data (statements, code, 0);
    read_data1(defs);
    read_data1(fields);
    read_data1(functions);
    read_data1(strings);
    read_data2(globals, 2); /* reserve more in case a RETURN using with the global at "the end" exists */

    util_swap_statements (prog->code);
    util_swap_defs_fields(prog->defs);
    util_swap_defs_fields(prog->fields);
    util_swap_functions  (prog->functions);
    util_swap_globals    (prog->globals);

    fs_file_close(file);

    /* profile counters */
    memset(vec_add(prog->profile, vec_size(prog->code)), 0, sizeof(prog->profile[0]) * vec_size(prog->code));

    /* Add tempstring area */
    prog->tempstring_start = vec_size(prog->strings);
    prog->tempstring_at    = vec_size(prog->strings);
    memset(vec_add(prog->strings, 16*1024), 0, 16*1024);

    /* spawn the world entity */
    vec_push(prog->entitypool, true);
    memset(vec_add(prog->entitydata, prog->entityfields), 0, prog->entityfields * sizeof(prog->entitydata[0]));
    prog->entities = 1;

    /* cache some globals and fields from names */
    for (i = 0; i < vec_size(prog->defs); ++i) {
        const char *name = prog_getstring(prog, prog->defs[i].name);
        if      (!strcmp(name, "self")) {
            prog->cached_globals.self = prog->defs[i].offset;
            has_self = true;
        }
        else if (!strcmp(name, "time")) {
            prog->cached_globals.time = prog->defs[i].offset;
            has_time = true;
        }
    }
    for (i = 0; i < vec_size(prog->fields); ++i) {
        const char *name = prog_getstring(prog, prog->fields[i].name);
        if      (!strcmp(name, "think")) {
            prog->cached_fields.think     = prog->fields[i].offset;
            has_think = true;
        }
        else if (!strcmp(name, "nextthink")) {
            prog->cached_fields.nextthink = prog->fields[i].offset;
            has_nextthink = true;
        }
        else if (!strcmp(name, "frame")) {
            prog->cached_fields.frame     = prog->fields[i].offset;
            has_frame = true;
        }
    }
    if (has_self && has_time && has_think && has_nextthink && has_frame)
        prog->supports_state = true;

    return prog;

error:
    if (prog->filename)
        mem_d(prog->filename);
    vec_free(prog->code);
    vec_free(prog->defs);
    vec_free(prog->fields);
    vec_free(prog->functions);
    vec_free(prog->strings);
    vec_free(prog->globals);
    vec_free(prog->entitydata);
    vec_free(prog->entitypool);
    mem_d(prog);

    fs_file_close(file);
    return NULL;
}

void prog_delete(qc_program_t *prog)
{
    if (prog->filename) mem_d(prog->filename);
    vec_free(prog->code);
    vec_free(prog->defs);
    vec_free(prog->fields);
    vec_free(prog->functions);
    vec_free(prog->strings);
    vec_free(prog->globals);
    vec_free(prog->entitydata);
    vec_free(prog->entitypool);
    vec_free(prog->localstack);
    vec_free(prog->stack);
    vec_free(prog->profile);
    mem_d(prog);
}

/***********************************************************************
 * VM code
 */

const char* prog_getstring(qc_program_t *prog, qcint_t str) {
    /* cast for return required for C++ */
    if (str < 0 || str >= (qcint_t)vec_size(prog->strings))
        return  "<<<invalid string>>>";

    return prog->strings + str;
}

prog_section_def_t* prog_entfield(qc_program_t *prog, qcint_t off) {
    size_t i;
    for (i = 0; i < vec_size(prog->fields); ++i) {
        if (prog->fields[i].offset == off)
            return (prog->fields + i);
    }
    return NULL;
}

prog_section_def_t* prog_getdef(qc_program_t *prog, qcint_t off)
{
    size_t i;
    for (i = 0; i < vec_size(prog->defs); ++i) {
        if (prog->defs[i].offset == off)
            return (prog->defs + i);
    }
    return NULL;
}

qcany_t* prog_getedict(qc_program_t *prog, qcint_t e) {
    if (e >= (qcint_t)vec_size(prog->entitypool)) {
        prog->vmerror++;
        fprintf(stderr, "Accessing out of bounds edict %i\n", (int)e);
        e = 0;
    }
    return (qcany_t*)(prog->entitydata + (prog->entityfields * e));
}

static qcint_t prog_spawn_entity(qc_program_t *prog) {
    char  *data;
    qcint_t  e;
    for (e = 0; e < (qcint_t)vec_size(prog->entitypool); ++e) {
        if (!prog->entitypool[e]) {
            data = (char*)(prog->entitydata + (prog->entityfields * e));
            memset(data, 0, prog->entityfields * sizeof(qcint_t));
            return e;
        }
    }
    vec_push(prog->entitypool, true);
    prog->entities++;
    data = (char*)vec_add(prog->entitydata, prog->entityfields);
    memset(data, 0, prog->entityfields * sizeof(qcint_t));
    return e;
}

static void prog_free_entity(qc_program_t *prog, qcint_t e) {
    if (!e) {
        prog->vmerror++;
        fprintf(stderr, "Trying to free world entity\n");
        return;
    }
    if (e >= (qcint_t)vec_size(prog->entitypool)) {
        prog->vmerror++;
        fprintf(stderr, "Trying to free out of bounds entity\n");
        return;
    }
    if (!prog->entitypool[e]) {
        prog->vmerror++;
        fprintf(stderr, "Double free on entity\n");
        return;
    }
    prog->entitypool[e] = false;
}

qcint_t prog_tempstring(qc_program_t *prog, const char *str) {
    size_t len = strlen(str);
    size_t at = prog->tempstring_at;

    /* when we reach the end we start over */
    if (at + len >= vec_size(prog->strings))
        at = prog->tempstring_start;

    /* when it doesn't fit, reallocate */
    if (at + len >= vec_size(prog->strings))
    {
        (void)vec_add(prog->strings, len+1);
        memcpy(prog->strings + at, str, len+1);
        return at;
    }

    /* when it fits, just copy */
    memcpy(prog->strings + at, str, len+1);
    prog->tempstring_at += len+1;
    return at;
}

static size_t print_escaped_string(const char *str, size_t maxlen) {
    size_t len = 2;
    putchar('"');
    --maxlen; /* because we're lazy and have escape sequences */
    while (*str) {
        if (len >= maxlen) {
            putchar('.');
            putchar('.');
            putchar('.');
            len += 3;
            break;
        }
        switch (*str) {
            case '\a': len += 2; putchar('\\'); putchar('a'); break;
            case '\b': len += 2; putchar('\\'); putchar('b'); break;
            case '\r': len += 2; putchar('\\'); putchar('r'); break;
            case '\n': len += 2; putchar('\\'); putchar('n'); break;
            case '\t': len += 2; putchar('\\'); putchar('t'); break;
            case '\f': len += 2; putchar('\\'); putchar('f'); break;
            case '\v': len += 2; putchar('\\'); putchar('v'); break;
            case '\\': len += 2; putchar('\\'); putchar('\\'); break;
            case '"':  len += 2; putchar('\\'); putchar('"'); break;
            default:
                ++len;
                putchar(*str);
                break;
        }
        ++str;
    }
    putchar('"');
    return len;
}

static void trace_print_global(qc_program_t *prog, unsigned int glob, int vtype) {
    static char spaces[28+1] = "                            ";
    prog_section_def_t *def;
    qcany_t    *value;
    int       len;

    if (!glob) {
        if ((len = printf("<null>,")) == -1)
            len = 0;

        goto done;
    }

    def = prog_getdef(prog, glob);
    value = (qcany_t*)(&prog->globals[glob]);

    len = printf("[@%u] ", glob);
    if (def) {
        const char *name = prog_getstring(prog, def->name);
        if (name[0] == '#')
            len += printf("$");
        else
            len += printf("%s ", name);
        vtype = def->type & DEF_TYPEMASK;
    }

    switch (vtype) {
        case TYPE_VOID:
        case TYPE_ENTITY:
        case TYPE_FIELD:
        case TYPE_FUNCTION:
        case TYPE_POINTER:
            len += printf("(%i),", value->_int);
            break;
        case TYPE_VECTOR:
            len += printf("'%g %g %g',", value->vector[0],
                                         value->vector[1],
                                         value->vector[2]);
            break;
        case TYPE_STRING:
            if (value->string)
                len += print_escaped_string(prog_getstring(prog, value->string), sizeof(spaces)-len-5);
            else
                len += printf("(null)");
            len += printf(",");
            /* len += printf("\"%s\",", prog_getstring(prog, value->string)); */
            break;
        case TYPE_FLOAT:
        default:
            len += printf("%g,", value->_float);
            break;
    }
done:
    if (len < (int)sizeof(spaces)-1) {
        spaces[sizeof(spaces)-1-len] = 0;
        fs_file_puts((fs_file_t*)stdout, spaces);
        spaces[sizeof(spaces)-1-len] = ' ';
    }
}

static void prog_print_statement(qc_program_t *prog, prog_section_statement_t *st) {
    if (st->opcode >= VINSTR_END) {
        printf("<illegal instruction %d>\n", st->opcode);
        return;
    }
    if ((prog->xflags & VMXF_TRACE) && vec_size(prog->function_stack)) {
        size_t i;
        for (i = 0; i < vec_size(prog->function_stack); ++i)
            printf("->");
        printf("%s:", vec_last(prog->function_stack));
    }
    printf(" <> %-12s", util_instr_str[st->opcode]);
    if (st->opcode >= INSTR_IF &&
        st->opcode <= INSTR_IFNOT)
    {
        trace_print_global(prog, st->o1.u1, TYPE_FLOAT);
        printf("%d\n", st->o2.s1);
    }
    else if (st->opcode >= INSTR_CALL0 &&
             st->opcode <= INSTR_CALL8)
    {
        trace_print_global(prog, st->o1.u1, TYPE_FUNCTION);
        printf("\n");
    }
    else if (st->opcode == INSTR_GOTO)
    {
        printf("%i\n", st->o1.s1);
    }
    else
    {
        int t[3] = { TYPE_FLOAT, TYPE_FLOAT, TYPE_FLOAT };
        switch (st->opcode)
        {
            case INSTR_MUL_FV:
                t[1] = t[2] = TYPE_VECTOR;
                break;
            case INSTR_MUL_VF:
                t[0] = t[2] = TYPE_VECTOR;
                break;
            case INSTR_MUL_V:
                t[0] = t[1] = TYPE_VECTOR;
                break;
            case INSTR_ADD_V:
            case INSTR_SUB_V:
            case INSTR_EQ_V:
            case INSTR_NE_V:
                t[0] = t[1] = t[2] = TYPE_VECTOR;
                break;
            case INSTR_EQ_S:
            case INSTR_NE_S:
                t[0] = t[1] = TYPE_STRING;
                break;
            case INSTR_STORE_F:
            case INSTR_STOREP_F:
                t[2] = -1;
                break;
            case INSTR_STORE_V:
                t[0] = t[1] = TYPE_VECTOR; t[2] = -1;
                break;
            case INSTR_STORE_S:
                t[0] = t[1] = TYPE_STRING; t[2] = -1;
                break;
            case INSTR_STORE_ENT:
                t[0] = t[1] = TYPE_ENTITY; t[2] = -1;
                break;
            case INSTR_STORE_FLD:
                t[0] = t[1] = TYPE_FIELD; t[2] = -1;
                break;
            case INSTR_STORE_FNC:
                t[0] = t[1] = TYPE_FUNCTION; t[2] = -1;
                break;
            case INSTR_STOREP_V:
                t[0] = TYPE_VECTOR; t[1] = TYPE_ENTITY; t[2] = -1;
                break;
            case INSTR_STOREP_S:
                t[0] = TYPE_STRING; t[1] = TYPE_ENTITY; t[2] = -1;
                break;
            case INSTR_STOREP_ENT:
                t[0] = TYPE_ENTITY; t[1] = TYPE_ENTITY; t[2] = -1;
                break;
            case INSTR_STOREP_FLD:
                t[0] = TYPE_FIELD; t[1] = TYPE_ENTITY; t[2] = -1;
                break;
            case INSTR_STOREP_FNC:
                t[0] = TYPE_FUNCTION; t[1] = TYPE_ENTITY; t[2] = -1;
                break;
        }
        if (t[0] >= 0) trace_print_global(prog, st->o1.u1, t[0]);
        else           printf("(none),          ");
        if (t[1] >= 0) trace_print_global(prog, st->o2.u1, t[1]);
        else           printf("(none),          ");
        if (t[2] >= 0) trace_print_global(prog, st->o3.u1, t[2]);
        else           printf("(none)");
        printf("\n");
    }
}

static qcint_t prog_enterfunction(qc_program_t *prog, prog_section_function_t *func) {
    qc_exec_stack_t st;
    size_t  parampos;
    int32_t p;

    /* back up locals */
    st.localsp  = vec_size(prog->localstack);
    st.stmt     = prog->statement;
    st.function = func;

    if (prog->xflags & VMXF_TRACE) {
        const char *str = prog_getstring(prog, func->name);
        vec_push(prog->function_stack, str);
    }

#ifdef QCVM_BACKUP_STRATEGY_CALLER_VARS
    if (vec_size(prog->stack))
    {
        prog_section_function_t *cur;
        cur = prog->stack[vec_size(prog->stack)-1].function;
        if (cur)
        {
            qcint_t *globals = prog->globals + cur->firstlocal;
            vec_append(prog->localstack, cur->locals, globals);
        }
    }
#else
    {
        qcint_t *globals = prog->globals + func->firstlocal;
        vec_append(prog->localstack, func->locals, globals);
    }
#endif

    /* copy parameters */
    parampos = func->firstlocal;
    for (p = 0; p < func->nargs; ++p)
    {
        size_t s;
        for (s = 0; s < func->argsize[p]; ++s) {
            prog->globals[parampos] = prog->globals[OFS_PARM0 + 3*p + s];
            ++parampos;
        }
    }

    vec_push(prog->stack, st);

    return func->entry;
}

static qcint_t prog_leavefunction(qc_program_t *prog) {
    prog_section_function_t *prev = NULL;
    size_t oldsp;

    qc_exec_stack_t st = vec_last(prog->stack);

    if (prog->xflags & VMXF_TRACE) {
        if (vec_size(prog->function_stack))
            vec_pop(prog->function_stack);
    }

#ifdef QCVM_BACKUP_STRATEGY_CALLER_VARS
    if (vec_size(prog->stack) > 1) {
        prev  = prog->stack[vec_size(prog->stack)-2].function;
        oldsp = prog->stack[vec_size(prog->stack)-2].localsp;
    }
#else
    prev  = prog->stack[vec_size(prog->stack)-1].function;
    oldsp = prog->stack[vec_size(prog->stack)-1].localsp;
#endif
    if (prev) {
        qcint_t *globals = prog->globals + prev->firstlocal;
        memcpy(globals, prog->localstack + oldsp, prev->locals * sizeof(prog->localstack[0]));
        /* vec_remove(prog->localstack, oldsp, vec_size(prog->localstack)-oldsp); */
        vec_shrinkto(prog->localstack, oldsp);
    }

    vec_pop(prog->stack);

    return st.stmt - 1; /* offset the ++st */
}

bool prog_exec(qc_program_t *prog, prog_section_function_t *func, size_t flags, long maxjumps) {
    long jumpcount = 0;
    size_t oldxflags = prog->xflags;
    prog_section_statement_t *st;

    prog->vmerror = 0;
    prog->xflags = flags;

    st = prog->code + prog_enterfunction(prog, func);
    --st;
    switch (flags)
    {
        default:
        case 0:
        {
#define QCVM_LOOP    1
#define QCVM_PROFILE 0
#define QCVM_TRACE   0
#           include __FILE__
        }
        case (VMXF_TRACE):
        {
#define QCVM_PROFILE 0
#define QCVM_TRACE   1
#           include __FILE__
        }
        case (VMXF_PROFILE):
        {
#define QCVM_PROFILE 1
#define QCVM_TRACE   0
#           include __FILE__
        }
        case (VMXF_TRACE|VMXF_PROFILE):
        {
#define QCVM_PROFILE 1
#define QCVM_TRACE   1
#           include __FILE__
        }
    };

cleanup:
    prog->xflags = oldxflags;
    vec_free(prog->localstack);
    vec_free(prog->stack);
    if (prog->vmerror)
        return false;
    return true;
}

/***********************************************************************
 * main for when building the standalone executor
 */

#include <math.h>

const char *type_name[TYPE_COUNT] = {
    "void",
    "string",
    "float",
    "vector",
    "entity",
    "field",
    "function",
    "pointer",
    "integer",

    "variant",

    "struct",
    "union",
    "array",

    "nil",
    "noexpr"
};

typedef struct {
    int         vtype;
    const char *value;
} qcvm_parameter;

static qcvm_parameter *main_params = NULL;

#define CheckArgs(num) do {                                                    \
    if (prog->argc != (num)) {                                                 \
        prog->vmerror++;                                                       \
        fprintf(stderr, "ERROR: invalid number of arguments for %s: %i, expected %i\n", \
        __FUNCTION__, prog->argc, (num));                                      \
        return -1;                                                             \
    }                                                                          \
} while (0)

#define GetGlobal(idx) ((qcany_t*)(prog->globals + (idx)))
#define GetArg(num) GetGlobal(OFS_PARM0 + 3*(num))
#define Return(any) *(GetGlobal(OFS_RETURN)) = (any)

static int qc_print(qc_program_t *prog) {
    size_t i;
    const char *laststr = NULL;
    for (i = 0; i < (size_t)prog->argc; ++i) {
        qcany_t *str = (qcany_t*)(prog->globals + OFS_PARM0 + 3*i);
        laststr = prog_getstring(prog, str->string);
        printf("%s", laststr);
    }
    if (laststr && (prog->xflags & VMXF_TRACE)) {
        size_t len = strlen(laststr);
        if (!len || laststr[len-1] != '\n')
            printf("\n");
    }
    return 0;
}

static int qc_error(qc_program_t *prog) {
    fprintf(stderr, "*** VM raised an error:\n");
    qc_print(prog);
    prog->vmerror++;
    return -1;
}

static int qc_ftos(qc_program_t *prog) {
    char buffer[512];
    qcany_t *num;
    qcany_t str;
    CheckArgs(1);
    num = GetArg(0);
    util_snprintf(buffer, sizeof(buffer), "%g", num->_float);
    str.string = prog_tempstring(prog, buffer);
    Return(str);
    return 0;
}

static int qc_stof(qc_program_t *prog) {
    qcany_t *str;
    qcany_t num;
    CheckArgs(1);
    str = GetArg(0);
    num._float = (float)strtod(prog_getstring(prog, str->string), NULL);
    Return(num);
    return 0;
}

static int qc_vtos(qc_program_t *prog) {
    char buffer[512];
    qcany_t *num;
    qcany_t str;
    CheckArgs(1);
    num = GetArg(0);
    util_snprintf(buffer, sizeof(buffer), "'%g %g %g'", num->vector[0], num->vector[1], num->vector[2]);
    str.string = prog_tempstring(prog, buffer);
    Return(str);
    return 0;
}

static int qc_etos(qc_program_t *prog) {
    char buffer[512];
    qcany_t *num;
    qcany_t str;
    CheckArgs(1);
    num = GetArg(0);
    util_snprintf(buffer, sizeof(buffer), "%i", num->_int);
    str.string = prog_tempstring(prog, buffer);
    Return(str);
    return 0;
}

static int qc_spawn(qc_program_t *prog) {
    qcany_t ent;
    CheckArgs(0);
    ent.edict = prog_spawn_entity(prog);
    Return(ent);
    return (ent.edict ? 0 : -1);
}

static int qc_kill(qc_program_t *prog) {
    qcany_t *ent;
    CheckArgs(1);
    ent = GetArg(0);
    prog_free_entity(prog, ent->edict);
    return 0;
}

static int qc_sqrt(qc_program_t *prog) {
    qcany_t *num, out;
    CheckArgs(1);
    num = GetArg(0);
    out._float = sqrt(num->_float);
    Return(out);
    return 0;
}

static int qc_vlen(qc_program_t *prog) {
    qcany_t *vec, len;
    CheckArgs(1);
    vec = GetArg(0);
    len._float = sqrt(vec->vector[0] * vec->vector[0] +
                      vec->vector[1] * vec->vector[1] +
                      vec->vector[2] * vec->vector[2]);
    Return(len);
    return 0;
}

static int qc_normalize(qc_program_t *prog) {
    double len;
    qcany_t *vec;
    qcany_t out;
    CheckArgs(1);
    vec = GetArg(0);
    len = sqrt(vec->vector[0] * vec->vector[0] +
               vec->vector[1] * vec->vector[1] +
               vec->vector[2] * vec->vector[2]);
    if (len)
        len = 1.0 / len;
    else
        len = 0;
    out.vector[0] = len * vec->vector[0];
    out.vector[1] = len * vec->vector[1];
    out.vector[2] = len * vec->vector[2];
    Return(out);
    return 0;
}

static int qc_strcat(qc_program_t *prog) {
    char  *buffer;
    size_t len1,   len2;
    qcany_t *str1,  *str2;
    qcany_t  out;

    const char *cstr1;
    const char *cstr2;

    CheckArgs(2);
    str1 = GetArg(0);
    str2 = GetArg(1);
    cstr1 = prog_getstring(prog, str1->string);
    cstr2 = prog_getstring(prog, str2->string);
    len1 = strlen(cstr1);
    len2 = strlen(cstr2);
    buffer = (char*)mem_a(len1 + len2 + 1);
    memcpy(buffer, cstr1, len1);
    memcpy(buffer+len1, cstr2, len2+1);
    out.string = prog_tempstring(prog, buffer);
    mem_d(buffer);
    Return(out);
    return 0;
}

static int qc_strcmp(qc_program_t *prog) {
    qcany_t *str1,  *str2;
    qcany_t out;

    const char *cstr1;
    const char *cstr2;

    if (prog->argc != 2 && prog->argc != 3) {
        fprintf(stderr, "ERROR: invalid number of arguments for strcmp/strncmp: %i, expected 2 or 3\n",
               prog->argc);
        return -1;
    }

    str1 = GetArg(0);
    str2 = GetArg(1);
    cstr1 = prog_getstring(prog, str1->string);
    cstr2 = prog_getstring(prog, str2->string);
    if (prog->argc == 3)
        out._float = strncmp(cstr1, cstr2, GetArg(2)->_float);
    else
        out._float = strcmp(cstr1, cstr2);
    Return(out);
    return 0;
}

static int qc_floor(qc_program_t *prog) {
    qcany_t *num, out;
    CheckArgs(1);
    num = GetArg(0);
    out._float = floor(num->_float);
    Return(out);
    return 0;
}

static int qc_pow(qc_program_t *prog) {
    qcany_t *base, *exp, out;
    CheckArgs(2);
    base = GetArg(0);
    exp = GetArg(1);
    out._float = powf(base->_float, exp->_float);
    Return(out);
    return 0;
}

static prog_builtin_t qc_builtins[] = {
    NULL,
    &qc_print,       /*   1   */
    &qc_ftos,        /*   2   */
    &qc_spawn,       /*   3   */
    &qc_kill,        /*   4   */
    &qc_vtos,        /*   5   */
    &qc_error,       /*   6   */
    &qc_vlen,        /*   7   */
    &qc_etos,        /*   8   */
    &qc_stof,        /*   9   */
    &qc_strcat,      /*   10  */
    &qc_strcmp,      /*   11  */
    &qc_normalize,   /*   12  */
    &qc_sqrt,        /*   13  */
    &qc_floor,       /*   14  */
    &qc_pow          /*   15  */
};

static const char *arg0 = NULL;

static void version(void) {
    printf("GMQCC-QCVM %d.%d.%d Built %s %s\n",
           GMQCC_VERSION_MAJOR,
           GMQCC_VERSION_MINOR,
           GMQCC_VERSION_PATCH,
           __DATE__,
           __TIME__
    );
}

static void usage(void) {
    printf("usage: %s [options] [parameters] file\n", arg0);
    printf("options:\n");
    printf("  -h, --help         print this message\n"
           "  -trace             trace the execution\n"
           "  -profile           perform profiling during execution\n"
           "  -info              print information from the prog's header\n"
           "  -disasm            disassemble and exit\n"
           "  -disasm-func func  disassemble and exit\n"
           "  -printdefs         list the defs section\n"
           "  -printfields       list the field section\n"
           "  -printfuns         list functions information\n"
           "  -v                 be verbose\n"
           "  -vv                be even more verbose\n");
    printf("parameters:\n");
    printf("  -vector <V>   pass a vector parameter to main()\n"
           "  -float  <f>   pass a float parameter to main()\n"
           "  -string <s>   pass a string parameter to main() \n");
}

static void prog_main_setparams(qc_program_t *prog) {
    size_t i;
    qcany_t *arg;

    for (i = 0; i < vec_size(main_params); ++i) {
        arg = GetGlobal(OFS_PARM0 + 3*i);
        arg->vector[0] = 0;
        arg->vector[1] = 0;
        arg->vector[2] = 0;
        switch (main_params[i].vtype) {
            case TYPE_VECTOR:
                (void)util_sscanf(main_params[i].value, " %f %f %f ",
                                       &arg->vector[0],
                                       &arg->vector[1],
                                       &arg->vector[2]);
                break;
            case TYPE_FLOAT:
                arg->_float = atof(main_params[i].value);
                break;
            case TYPE_STRING:
                arg->string = prog_tempstring(prog, main_params[i].value);
                break;
            default:
                fprintf(stderr, "error: unhandled parameter type: %i\n", main_params[i].vtype);
                break;
        }
    }
}

static void prog_disasm_function(qc_program_t *prog, size_t id);

int main(int argc, char **argv) {
    size_t      i;
    qcint_t       fnmain = -1;
    qc_program_t *prog;
    size_t      xflags = VMXF_DEFAULT;
    bool        opts_printfields = false;
    bool        opts_printdefs   = false;
    bool        opts_printfuns   = false;
    bool        opts_disasm      = false;
    bool        opts_info        = false;
    bool        noexec           = false;
    const char *progsfile        = NULL;
    const char **dis_list        = NULL;
    int         opts_v           = 0;

    arg0 = argv[0];

    if (argc < 2) {
        usage();
        exit(EXIT_FAILURE);
    }

    while (argc > 1) {
        if (!strcmp(argv[1], "-h") ||
            !strcmp(argv[1], "-help") ||
            !strcmp(argv[1], "--help"))
        {
            usage();
            exit(EXIT_SUCCESS);
        }
        else if (!strcmp(argv[1], "-v")) {
            ++opts_v;
            --argc;
            ++argv;
        }
        else if (!strncmp(argv[1], "-vv", 3)) {
            const char *av = argv[1]+1;
            for (; *av; ++av) {
                if (*av == 'v')
                    ++opts_v;
                else {
                    usage();
                    exit(EXIT_FAILURE);
                }
            }
            --argc;
            ++argv;
        }
        else if (!strcmp(argv[1], "-version") ||
                 !strcmp(argv[1], "--version"))
        {
            version();
            exit(EXIT_SUCCESS);
        }
        else if (!strcmp(argv[1], "-trace")) {
            --argc;
            ++argv;
            xflags |= VMXF_TRACE;
        }
        else if (!strcmp(argv[1], "-profile")) {
            --argc;
            ++argv;
            xflags |= VMXF_PROFILE;
        }
        else if (!strcmp(argv[1], "-info")) {
            --argc;
            ++argv;
            opts_info = true;
            noexec = true;
        }
        else if (!strcmp(argv[1], "-disasm")) {
            --argc;
            ++argv;
            opts_disasm = true;
            noexec = true;
        }
        else if (!strcmp(argv[1], "-disasm-func")) {
            --argc;
            ++argv;
            if (argc <= 1) {
                usage();
                exit(EXIT_FAILURE);
            }
            vec_push(dis_list, argv[1]);
            --argc;
            ++argv;
            noexec = true;
        }
        else if (!strcmp(argv[1], "-printdefs")) {
            --argc;
            ++argv;
            opts_printdefs = true;
            noexec = true;
        }
        else if (!strcmp(argv[1], "-printfuns")) {
            --argc;
            ++argv;
            opts_printfuns = true;
            noexec = true;
        }
        else if (!strcmp(argv[1], "-printfields")) {
            --argc;
            ++argv;
            opts_printfields = true;
            noexec = true;
        }
        else if (!strcmp(argv[1], "-vector") ||
                 !strcmp(argv[1], "-string") ||
                 !strcmp(argv[1], "-float") )
        {
            qcvm_parameter p;
            if (argv[1][1] == 'f')
                p.vtype = TYPE_FLOAT;
            else if (argv[1][1] == 's')
                p.vtype = TYPE_STRING;
            else if (argv[1][1] == 'v')
                p.vtype = TYPE_VECTOR;
            else
                p.vtype = TYPE_VOID;

            --argc;
            ++argv;
            if (argc < 2) {
                usage();
                exit(EXIT_FAILURE);
            }
            p.value = argv[1];

            vec_push(main_params, p);
            --argc;
            ++argv;
        }
        else if (!strcmp(argv[1], "--")) {
            --argc;
            ++argv;
            break;
        }
        else if (argv[1][0] != '-') {
            if (progsfile) {
                fprintf(stderr, "only 1 program file may be specified\n");
                usage();
                exit(EXIT_FAILURE);
            }
            progsfile = argv[1];
            --argc;
            ++argv;
        }
        else
        {
            fprintf(stderr, "unknown parameter: %s\n", argv[1]);
            usage();
            exit(EXIT_FAILURE);
        }
    }

    if (argc == 2 && !progsfile) {
        progsfile = argv[1];
        --argc;
        ++argv;
    }

    if (!progsfile) {
        fprintf(stderr, "must specify a program to execute\n");
        usage();
        exit(EXIT_FAILURE);
    }

    prog = prog_load(progsfile, noexec);
    if (!prog) {
        fprintf(stderr, "failed to load program '%s'\n", progsfile);
        exit(EXIT_FAILURE);
    }

    prog->builtins       = qc_builtins;
    prog->builtins_count = GMQCC_ARRAY_COUNT(qc_builtins);

    if (opts_info) {
        printf("Program's system-checksum = 0x%04x\n", (unsigned int)prog->crc16);
        printf("Entity field space: %u\n", (unsigned int)prog->entityfields);
        printf("Globals: %u\n", (unsigned int)vec_size(prog->globals));
        printf("Counts:\n"
               "      code: %lu\n"
               "      defs: %lu\n"
               "    fields: %lu\n"
               " functions: %lu\n"
               "   strings: %lu\n",
               (unsigned long)vec_size(prog->code),
               (unsigned long)vec_size(prog->defs),
               (unsigned long)vec_size(prog->fields),
               (unsigned long)vec_size(prog->functions),
               (unsigned long)vec_size(prog->strings));
    }

    if (opts_info) {
        prog_delete(prog);
        return 0;
    }
    for (i = 0; i < vec_size(dis_list); ++i) {
        size_t k;
        printf("Looking for `%s`\n", dis_list[i]);
        for (k = 1; k < vec_size(prog->functions); ++k) {
            const char *name = prog_getstring(prog, prog->functions[k].name);
            if (!strcmp(name, dis_list[i])) {
                prog_disasm_function(prog, k);
                break;
            }
        }
    }
    if (opts_disasm) {
        for (i = 1; i < vec_size(prog->functions); ++i)
            prog_disasm_function(prog, i);
        return 0;
    }
    if (opts_printdefs) {
        const char *getstring = NULL;
        for (i = 0; i < vec_size(prog->defs); ++i) {
            printf("Global: %8s %-16s at %u%s",
                   type_name[prog->defs[i].type & DEF_TYPEMASK],
                   prog_getstring(prog, prog->defs[i].name),
                   (unsigned int)prog->defs[i].offset,
                   ((prog->defs[i].type & DEF_SAVEGLOBAL) ? " [SAVE]" : ""));
            if (opts_v) {
                switch (prog->defs[i].type & DEF_TYPEMASK) {
                    case TYPE_FLOAT:
                        printf(" [init: %g]", ((qcany_t*)(prog->globals + prog->defs[i].offset))->_float);
                        break;
                    case TYPE_INTEGER:
                        printf(" [init: %i]", (int)( ((qcany_t*)(prog->globals + prog->defs[i].offset))->_int ));
                        break;
                    case TYPE_ENTITY:
                    case TYPE_FUNCTION:
                    case TYPE_FIELD:
                    case TYPE_POINTER:
                        printf(" [init: %u]", (unsigned)( ((qcany_t*)(prog->globals + prog->defs[i].offset))->_int ));
                        break;
                    case TYPE_STRING:
                        getstring = prog_getstring(prog, ((qcany_t*)(prog->globals + prog->defs[i].offset))->string);
                        printf(" [init: `");
                        print_escaped_string(getstring, strlen(getstring));
                        printf("`]\n");
                        break;
                    default:
                        break;
                }
            }
            printf("\n");
        }
    }
    if (opts_printfields) {
        for (i = 0; i < vec_size(prog->fields); ++i) {
            printf("Field: %8s %-16s at %u%s\n",
                   type_name[prog->fields[i].type],
                   prog_getstring(prog, prog->fields[i].name),
                   (unsigned int)prog->fields[i].offset,
                   ((prog->fields[i].type & DEF_SAVEGLOBAL) ? " [SAVE]" : ""));
        }
    }
    if (opts_printfuns) {
        for (i = 0; i < vec_size(prog->functions); ++i) {
            int32_t a;
            printf("Function: %-16s taking %u parameters:(",
                   prog_getstring(prog, prog->functions[i].name),
                   (unsigned int)prog->functions[i].nargs);
            for (a = 0; a < prog->functions[i].nargs; ++a) {
                printf(" %i", prog->functions[i].argsize[a]);
            }
            if (opts_v > 1) {
                int32_t start = prog->functions[i].entry;
                if (start < 0)
                    printf(") builtin %i\n", (int)-start);
                else {
                    size_t funsize = 0;
                    prog_section_statement_t *st = prog->code + start;
                    for (;st->opcode != INSTR_DONE; ++st)
                        ++funsize;
                    printf(") - %lu instructions", (unsigned long)funsize);
                    if (opts_v > 2) {
                        printf(" - locals: %i + %i\n",
                               prog->functions[i].firstlocal,
                               prog->functions[i].locals);
                    }
                    else
                        printf("\n");
                }
            }
            else if (opts_v) {
                printf(") locals: %i + %i\n",
                       prog->functions[i].firstlocal,
                       prog->functions[i].locals);
            }
            else
                printf(")\n");
        }
    }
    if (!noexec) {
        for (i = 1; i < vec_size(prog->functions); ++i) {
            const char *name = prog_getstring(prog, prog->functions[i].name);
            if (!strcmp(name, "main"))
                fnmain = (qcint_t)i;
        }
        if (fnmain > 0)
        {
            prog_main_setparams(prog);
            prog_exec(prog, &prog->functions[fnmain], xflags, VM_JUMPS_DEFAULT);
        }
        else
            fprintf(stderr, "No main function found\n");
    }

    prog_delete(prog);
    return 0;
}

static void prog_disasm_function(qc_program_t *prog, size_t id) {
    prog_section_function_t *fdef = prog->functions + id;
    prog_section_statement_t *st;

    if (fdef->entry < 0) {
        printf("FUNCTION \"%s\" = builtin #%i\n", prog_getstring(prog, fdef->name), (int)-fdef->entry);
        return;
    }
    else
        printf("FUNCTION \"%s\"\n", prog_getstring(prog, fdef->name));

    st = prog->code + fdef->entry;
    while (st->opcode != INSTR_DONE) {
        prog_print_statement(prog, st);
        ++st;
    }
}
#else /* !QCVM_LOOP */
/*
 * Everything from here on is not including into the compilation of the
 * executor.  This is simply code that is #included via #include __FILE__
 * see when QCVM_LOOP is defined, the rest of the code above do not get
 * re-included.  So this really just acts like one large macro, but it
 * sort of isn't, which makes it nicer looking.
 */

#define OPA ( (qcany_t*) (prog->globals + st->o1.u1) )
#define OPB ( (qcany_t*) (prog->globals + st->o2.u1) )
#define OPC ( (qcany_t*) (prog->globals + st->o3.u1) )

#define GLOBAL(x) ( (qcany_t*) (prog->globals + (x)) )

/* to be consistent with current darkplaces behaviour */
#if !defined(FLOAT_IS_TRUE_FOR_INT)
#   define FLOAT_IS_TRUE_FOR_INT(x) ( (x) & 0x7FFFFFFF )
#endif

while (prog->vmerror == 0) {
    prog_section_function_t  *newf;
    qcany_t          *ed;
    qcany_t          *ptr;

    ++st;

#if QCVM_PROFILE
    prog->profile[st - prog->code]++;
#endif

#if QCVM_TRACE
    prog_print_statement(prog, st);
#endif

    switch (st->opcode)
    {
        default:
            qcvmerror(prog, "Illegal instruction in %s\n", prog->filename);
            goto cleanup;

        case INSTR_DONE:
        case INSTR_RETURN:
            /* TODO: add instruction count to function profile count */
            GLOBAL(OFS_RETURN)->ivector[0] = OPA->ivector[0];
            GLOBAL(OFS_RETURN)->ivector[1] = OPA->ivector[1];
            GLOBAL(OFS_RETURN)->ivector[2] = OPA->ivector[2];

            st = prog->code + prog_leavefunction(prog);
            if (!vec_size(prog->stack))
                goto cleanup;

            break;

        case INSTR_MUL_F:
            OPC->_float = OPA->_float * OPB->_float;
            break;
        case INSTR_MUL_V:
            OPC->_float = OPA->vector[0]*OPB->vector[0] +
                          OPA->vector[1]*OPB->vector[1] +
                          OPA->vector[2]*OPB->vector[2];
            break;
        case INSTR_MUL_FV:
        {
            qcfloat_t f = OPA->_float;
            OPC->vector[0] = f * OPB->vector[0];
            OPC->vector[1] = f * OPB->vector[1];
            OPC->vector[2] = f * OPB->vector[2];
            break;
        }
        case INSTR_MUL_VF:
        {
            qcfloat_t f = OPB->_float;
            OPC->vector[0] = f * OPA->vector[0];
            OPC->vector[1] = f * OPA->vector[1];
            OPC->vector[2] = f * OPA->vector[2];
            break;
        }
        case INSTR_DIV_F:
            if (OPB->_float != 0.0f)
                OPC->_float = OPA->_float / OPB->_float;
            else
                OPC->_float = 0;
            break;

        case INSTR_ADD_F:
            OPC->_float = OPA->_float + OPB->_float;
            break;
        case INSTR_ADD_V:
            OPC->vector[0] = OPA->vector[0] + OPB->vector[0];
            OPC->vector[1] = OPA->vector[1] + OPB->vector[1];
            OPC->vector[2] = OPA->vector[2] + OPB->vector[2];
            break;
        case INSTR_SUB_F:
            OPC->_float = OPA->_float - OPB->_float;
            break;
        case INSTR_SUB_V:
            OPC->vector[0] = OPA->vector[0] - OPB->vector[0];
            OPC->vector[1] = OPA->vector[1] - OPB->vector[1];
            OPC->vector[2] = OPA->vector[2] - OPB->vector[2];
            break;

        case INSTR_EQ_F:
            OPC->_float = (OPA->_float == OPB->_float);
            break;
        case INSTR_EQ_V:
            OPC->_float = ((OPA->vector[0] == OPB->vector[0]) &&
                           (OPA->vector[1] == OPB->vector[1]) &&
                           (OPA->vector[2] == OPB->vector[2]) );
            break;
        case INSTR_EQ_S:
            OPC->_float = !strcmp(prog_getstring(prog, OPA->string),
                                  prog_getstring(prog, OPB->string));
            break;
        case INSTR_EQ_E:
            OPC->_float = (OPA->_int == OPB->_int);
            break;
        case INSTR_EQ_FNC:
            OPC->_float = (OPA->function == OPB->function);
            break;
        case INSTR_NE_F:
            OPC->_float = (OPA->_float != OPB->_float);
            break;
        case INSTR_NE_V:
            OPC->_float = ((OPA->vector[0] != OPB->vector[0]) ||
                           (OPA->vector[1] != OPB->vector[1]) ||
                           (OPA->vector[2] != OPB->vector[2]) );
            break;
        case INSTR_NE_S:
            OPC->_float = !!strcmp(prog_getstring(prog, OPA->string),
                                   prog_getstring(prog, OPB->string));
            break;
        case INSTR_NE_E:
            OPC->_float = (OPA->_int != OPB->_int);
            break;
        case INSTR_NE_FNC:
            OPC->_float = (OPA->function != OPB->function);
            break;

        case INSTR_LE:
            OPC->_float = (OPA->_float <= OPB->_float);
            break;
        case INSTR_GE:
            OPC->_float = (OPA->_float >= OPB->_float);
            break;
        case INSTR_LT:
            OPC->_float = (OPA->_float < OPB->_float);
            break;
        case INSTR_GT:
            OPC->_float = (OPA->_float > OPB->_float);
            break;

        case INSTR_LOAD_F:
        case INSTR_LOAD_S:
        case INSTR_LOAD_FLD:
        case INSTR_LOAD_ENT:
        case INSTR_LOAD_FNC:
            if (OPA->edict < 0 || OPA->edict >= prog->entities) {
                qcvmerror(prog, "progs `%s` attempted to read an out of bounds entity", prog->filename);
                goto cleanup;
            }
            if ((unsigned int)(OPB->_int) >= (unsigned int)(prog->entityfields)) {
                qcvmerror(prog, "prog `%s` attempted to read an invalid field from entity (%i)",
                          prog->filename,
                          OPB->_int);
                goto cleanup;
            }
            ed = prog_getedict(prog, OPA->edict);
            OPC->_int = ((qcany_t*)( ((qcint_t*)ed) + OPB->_int ))->_int;
            break;
        case INSTR_LOAD_V:
            if (OPA->edict < 0 || OPA->edict >= prog->entities) {
                qcvmerror(prog, "progs `%s` attempted to read an out of bounds entity", prog->filename);
                goto cleanup;
            }
            if (OPB->_int < 0 || OPB->_int + 3 > (qcint_t)prog->entityfields)
            {
                qcvmerror(prog, "prog `%s` attempted to read an invalid field from entity (%i)",
                          prog->filename,
                          OPB->_int + 2);
                goto cleanup;
            }
            ed = prog_getedict(prog, OPA->edict);
            ptr = (qcany_t*)( ((qcint_t*)ed) + OPB->_int );
            OPC->ivector[0] = ptr->ivector[0];
            OPC->ivector[1] = ptr->ivector[1];
            OPC->ivector[2] = ptr->ivector[2];
            break;

        case INSTR_ADDRESS:
            if (OPA->edict < 0 || OPA->edict >= prog->entities) {
                qcvmerror(prog, "prog `%s` attempted to address an out of bounds entity %i", prog->filename, OPA->edict);
                goto cleanup;
            }
            if ((unsigned int)(OPB->_int) >= (unsigned int)(prog->entityfields))
            {
                qcvmerror(prog, "prog `%s` attempted to read an invalid field from entity (%i)",
                          prog->filename,
                          OPB->_int);
                goto cleanup;
            }

            ed = prog_getedict(prog, OPA->edict);
            OPC->_int = ((qcint_t*)ed) - prog->entitydata + OPB->_int;
            break;

        case INSTR_STORE_F:
        case INSTR_STORE_S:
        case INSTR_STORE_ENT:
        case INSTR_STORE_FLD:
        case INSTR_STORE_FNC:
            OPB->_int = OPA->_int;
            break;
        case INSTR_STORE_V:
            OPB->ivector[0] = OPA->ivector[0];
            OPB->ivector[1] = OPA->ivector[1];
            OPB->ivector[2] = OPA->ivector[2];
            break;

        case INSTR_STOREP_F:
        case INSTR_STOREP_S:
        case INSTR_STOREP_ENT:
        case INSTR_STOREP_FLD:
        case INSTR_STOREP_FNC:
            if (OPB->_int < 0 || OPB->_int >= (qcint_t)vec_size(prog->entitydata)) {
                qcvmerror(prog, "`%s` attempted to write to an out of bounds edict (%i)", prog->filename, OPB->_int);
                goto cleanup;
            }
            if (OPB->_int < (qcint_t)prog->entityfields && !prog->allowworldwrites)
                qcvmerror(prog, "`%s` tried to assign to world.%s (field %i)\n",
                          prog->filename,
                          prog_getstring(prog, prog_entfield(prog, OPB->_int)->name),
                          OPB->_int);
            ptr = (qcany_t*)(prog->entitydata + OPB->_int);
            ptr->_int = OPA->_int;
            break;
        case INSTR_STOREP_V:
            if (OPB->_int < 0 || OPB->_int + 2 >= (qcint_t)vec_size(prog->entitydata)) {
                qcvmerror(prog, "`%s` attempted to write to an out of bounds edict (%i)", prog->filename, OPB->_int);
                goto cleanup;
            }
            if (OPB->_int < (qcint_t)prog->entityfields && !prog->allowworldwrites)
                qcvmerror(prog, "`%s` tried to assign to world.%s (field %i)\n",
                          prog->filename,
                          prog_getstring(prog, prog_entfield(prog, OPB->_int)->name),
                          OPB->_int);
            ptr = (qcany_t*)(prog->entitydata + OPB->_int);
            ptr->ivector[0] = OPA->ivector[0];
            ptr->ivector[1] = OPA->ivector[1];
            ptr->ivector[2] = OPA->ivector[2];
            break;

        case INSTR_NOT_F:
            OPC->_float = !FLOAT_IS_TRUE_FOR_INT(OPA->_int);
            break;
        case INSTR_NOT_V:
            OPC->_float = !OPA->vector[0] &&
                          !OPA->vector[1] &&
                          !OPA->vector[2];
            break;
        case INSTR_NOT_S:
            OPC->_float = !OPA->string ||
                          !*prog_getstring(prog, OPA->string);
            break;
        case INSTR_NOT_ENT:
            OPC->_float = (OPA->edict == 0);
            break;
        case INSTR_NOT_FNC:
            OPC->_float = !OPA->function;
            break;

        case INSTR_IF:
            /* this is consistent with darkplaces' behaviour */
            if(FLOAT_IS_TRUE_FOR_INT(OPA->_int))
            {
                st += st->o2.s1 - 1;    /* offset the s++ */
                if (++jumpcount >= maxjumps)
                    qcvmerror(prog, "`%s` hit the runaway loop counter limit of %li jumps", prog->filename, jumpcount);
            }
            break;
        case INSTR_IFNOT:
            if(!FLOAT_IS_TRUE_FOR_INT(OPA->_int))
            {
                st += st->o2.s1 - 1;    /* offset the s++ */
                if (++jumpcount >= maxjumps)
                    qcvmerror(prog, "`%s` hit the runaway loop counter limit of %li jumps", prog->filename, jumpcount);
            }
            break;

        case INSTR_CALL0:
        case INSTR_CALL1:
        case INSTR_CALL2:
        case INSTR_CALL3:
        case INSTR_CALL4:
        case INSTR_CALL5:
        case INSTR_CALL6:
        case INSTR_CALL7:
        case INSTR_CALL8:
            prog->argc = st->opcode - INSTR_CALL0;
            if (!OPA->function)
                qcvmerror(prog, "NULL function in `%s`", prog->filename);

            if(!OPA->function || OPA->function >= (qcint_t)vec_size(prog->functions))
            {
                qcvmerror(prog, "CALL outside the program in `%s`", prog->filename);
                goto cleanup;
            }

            newf = &prog->functions[OPA->function];
            newf->profile++;

            prog->statement = (st - prog->code) + 1;

            if (newf->entry < 0)
            {
                /* negative statements are built in functions */
                qcint_t builtinnumber = -newf->entry;
                if (builtinnumber < (qcint_t)prog->builtins_count && prog->builtins[builtinnumber])
                    prog->builtins[builtinnumber](prog);
                else
                    qcvmerror(prog, "No such builtin #%i in %s! Try updating your gmqcc sources",
                              builtinnumber, prog->filename);
            }
            else
                st = prog->code + prog_enterfunction(prog, newf) - 1; /* offset st++ */
            if (prog->vmerror)
                goto cleanup;
            break;

        case INSTR_STATE:
        {
            qcfloat_t *nextthink;
            qcfloat_t *time;
            qcfloat_t *frame;
            if (!prog->supports_state) {
                qcvmerror(prog, "`%s` tried to execute a STATE operation but misses its defs!", prog->filename);
                goto cleanup;
            }
            ed = prog_getedict(prog, prog->globals[prog->cached_globals.self]);
            ((qcint_t*)ed)[prog->cached_fields.think] = OPB->function;

            frame     = (qcfloat_t*)&((qcint_t*)ed)[prog->cached_fields.frame];
            *frame    = OPA->_float;
            nextthink = (qcfloat_t*)&((qcint_t*)ed)[prog->cached_fields.nextthink];
            time      = (qcfloat_t*)(prog->globals + prog->cached_globals.time);
            *nextthink = *time + 0.1;
            break;
        }

        case INSTR_GOTO:
            st += st->o1.s1 - 1;    /* offset the s++ */
            if (++jumpcount > maxjumps)
                qcvmerror(prog, "`%s` hit the runaway loop counter limit of %li jumps", prog->filename, jumpcount);
            break;

        case INSTR_AND:
            OPC->_float = FLOAT_IS_TRUE_FOR_INT(OPA->_int) &&
                          FLOAT_IS_TRUE_FOR_INT(OPB->_int);
            break;
        case INSTR_OR:
            OPC->_float = FLOAT_IS_TRUE_FOR_INT(OPA->_int) ||
                          FLOAT_IS_TRUE_FOR_INT(OPB->_int);
            break;

        case INSTR_BITAND:
            OPC->_float = ((int)OPA->_float) & ((int)OPB->_float);
            break;
        case INSTR_BITOR:
            OPC->_float = ((int)OPA->_float) | ((int)OPB->_float);
            break;
    }
}

#undef QCVM_PROFILE
#undef QCVM_TRACE
#endif /* !QCVM_LOOP */
