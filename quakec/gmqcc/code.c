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
#include <string.h>
#include "gmqcc.h"

/*
 * We could use the old method of casting to uintptr_t then to void*
 * or qcint_t; however, it's incredibly unsafe for two reasons.
 * 1) The compilers aliasing optimization can legally make it unstable
 *    (it's undefined behaviour).
 *
 * 2) The cast itself depends on fresh storage (newly allocated in which
 *    ever function is using the cast macros), the contents of which are
 *    transferred in a way that the obligation to release storage is not
 *    propagated.
 */
typedef union {
    void   *enter;
    qcint_t leave;
} code_hash_entry_t;

/* Some sanity macros */
#define CODE_HASH_ENTER(ENTRY) ((ENTRY).enter)
#define CODE_HASH_LEAVE(ENTRY) ((ENTRY).leave)

void code_push_statement(code_t *code, prog_section_statement_t *stmt_in, lex_ctx_t ctx)
{
    prog_section_statement_t stmt = *stmt_in;

    if (OPTS_FLAG(TYPELESS_STORES)) {
        switch (stmt.opcode) {
            case INSTR_LOAD_S:
            case INSTR_LOAD_ENT:
            case INSTR_LOAD_FLD:
            case INSTR_LOAD_FNC:
                stmt.opcode = INSTR_LOAD_F;
                break;
            case INSTR_STORE_S:
            case INSTR_STORE_ENT:
            case INSTR_STORE_FLD:
            case INSTR_STORE_FNC:
                stmt.opcode = INSTR_STORE_F;
                break;
            case INSTR_STOREP_S:
            case INSTR_STOREP_ENT:
            case INSTR_STOREP_FLD:
            case INSTR_STOREP_FNC:
                stmt.opcode = INSTR_STOREP_F;
                break;
        }
    }


    if (OPTS_FLAG(SORT_OPERANDS)) {
        uint16_t pair;

        switch (stmt.opcode) {
            case INSTR_MUL_F:
            case INSTR_MUL_V:
            case INSTR_ADD_F:
            case INSTR_EQ_F:
            case INSTR_EQ_S:
            case INSTR_EQ_E:
            case INSTR_EQ_FNC:
            case INSTR_NE_F:
            case INSTR_NE_V:
            case INSTR_NE_S:
            case INSTR_NE_E:
            case INSTR_NE_FNC:
            case INSTR_AND:
            case INSTR_OR:
            case INSTR_BITAND:
            case INSTR_BITOR:
                if (stmt.o1.u1 < stmt.o2.u1) {
                    uint16_t a = stmt.o2.u1;
                    stmt.o1.u1 = stmt.o2.u1;
                    stmt.o2.u1 = a;
                }
                break;

            case INSTR_MUL_VF: pair = INSTR_MUL_FV; goto case_pair_gen;
            case INSTR_MUL_FV: pair = INSTR_MUL_VF; goto case_pair_gen;
            case INSTR_LT:     pair = INSTR_GT;     goto case_pair_gen;
            case INSTR_GT:     pair = INSTR_LT;     goto case_pair_gen;
            case INSTR_LE:     pair = INSTR_GT;     goto case_pair_gen;
            case INSTR_GE:     pair = INSTR_LE;

            case_pair_gen:
                if (stmt.o1.u1 < stmt.o2.u1) {
                    uint16_t x  = stmt.o1.u1;
                    stmt.o1.u1  = stmt.o2.u1;
                    stmt.o2.u1  = x;
                    stmt.opcode = pair;
                }
                break;
        }
    }

    vec_push(code->statements, stmt);
    vec_push(code->linenums,   (int)ctx.line);
    vec_push(code->columnnums, (int)ctx.column);
}

void code_pop_statement(code_t *code)
{
    vec_pop(code->statements);
    vec_pop(code->linenums);
    vec_pop(code->columnnums);
}

code_t *code_init() {
    static lex_ctx_t                empty_ctx       = {0, 0, 0};
    static prog_section_function_t  empty_function  = {0,0,0,0,0,0,0,{0,0,0,0,0,0,0,0}};
    static prog_section_statement_t empty_statement = {0,{0},{0},{0}};
    static prog_section_def_t       empty_def       = {0, 0, 0};

    code_t *code       = (code_t*)mem_a(sizeof(code_t));
    int     i          = 0;

    memset(code, 0, sizeof(code_t));
    code->entfields    = 0;
    code->string_cache = util_htnew(OPTS_OPTIMIZATION(OPTIM_OVERLAP_STRINGS) ? 0x100 : 1024);

    /*
     * The way progs.dat is suppose to work is odd, there needs to be
     * some null (empty) statements, functions, and 28 globals
     */
    for(; i < 28; i++)
        vec_push(code->globals, 0);

    vec_push(code->chars, '\0');
    vec_push(code->functions,  empty_function);

    code_push_statement(code, &empty_statement, empty_ctx);

    vec_push(code->defs,    empty_def);
    vec_push(code->fields,  empty_def);

    return code;
}

void *code_util_str_htgeth(hash_table_t *ht, const char *key, size_t bin);

uint32_t code_genstring(code_t *code, const char *str) {
    size_t            hash;
    code_hash_entry_t existing;

    if (!str)
        return 0;

    if (!*str) {
        if (!code->string_cached_empty) {
            code->string_cached_empty = vec_size(code->chars);
            vec_push(code->chars, 0);
        }
        return code->string_cached_empty;
    }

    if (OPTS_OPTIMIZATION(OPTIM_OVERLAP_STRINGS)) {
        hash                      = ((unsigned char*)str)[strlen(str)-1];
        CODE_HASH_ENTER(existing) = code_util_str_htgeth(code->string_cache, str, hash);
    } else {
        hash                      = util_hthash(code->string_cache, str);
        CODE_HASH_ENTER(existing) = util_htgeth(code->string_cache, str, hash);
    }

    if (CODE_HASH_ENTER(existing))
        return CODE_HASH_LEAVE(existing);

    CODE_HASH_LEAVE(existing) = vec_size(code->chars);
    vec_append(code->chars, strlen(str)+1, str);

    util_htseth(code->string_cache, str, hash, CODE_HASH_ENTER(existing));
    return CODE_HASH_LEAVE(existing);
}

qcint_t code_alloc_field (code_t *code, size_t qcsize)
{
    qcint_t pos = (qcint_t)code->entfields;
    code->entfields += qcsize;
    return pos;
}

static size_t code_size_generic(code_t *code, prog_header_t *code_header, bool lno) {
    size_t size = 0;
    if (lno) {
        size += 4;  /* LNOF */
        size += sizeof(uint32_t); /* version */
        size += sizeof(code_header->defs.length);
        size += sizeof(code_header->globals.length);
        size += sizeof(code_header->fields.length);
        size += sizeof(code_header->statements.length);
        size += sizeof(code->linenums[0])   * vec_size(code->linenums);
        size += sizeof(code->columnnums[0]) * vec_size(code->columnnums);
    } else {
        size += sizeof(prog_header_t);
        size += sizeof(prog_section_statement_t) * vec_size(code->statements);
        size += sizeof(prog_section_def_t)       * vec_size(code->defs);
        size += sizeof(prog_section_field_t)     * vec_size(code->fields);
        size += sizeof(prog_section_function_t)  * vec_size(code->functions);
        size += sizeof(int32_t)                  * vec_size(code->globals);
        size += 1                                * vec_size(code->chars);
    }
    return size;
}

#define code_size_binary(C, H) code_size_generic((C), (H), false)
#define code_size_debug(C, H)  code_size_generic((C), (H), true)

static void code_create_header(code_t *code, prog_header_t *code_header, const char *filename, const char *lnofile) {
    size_t i;

    code_header->statements.offset = sizeof(prog_header_t);
    code_header->statements.length = vec_size(code->statements);
    code_header->defs.offset       = code_header->statements.offset + (sizeof(prog_section_statement_t) * vec_size(code->statements));
    code_header->defs.length       = vec_size(code->defs);
    code_header->fields.offset     = code_header->defs.offset       + (sizeof(prog_section_def_t)       * vec_size(code->defs));
    code_header->fields.length     = vec_size(code->fields);
    code_header->functions.offset  = code_header->fields.offset     + (sizeof(prog_section_field_t)     * vec_size(code->fields));
    code_header->functions.length  = vec_size(code->functions);
    code_header->globals.offset    = code_header->functions.offset  + (sizeof(prog_section_function_t)  * vec_size(code->functions));
    code_header->globals.length    = vec_size(code->globals);
    code_header->strings.offset    = code_header->globals.offset    + (sizeof(int32_t)                  * vec_size(code->globals));
    code_header->strings.length    = vec_size(code->chars);
    code_header->version           = 6;
    code_header->skip              = 0;

    if (OPTS_OPTION_BOOL(OPTION_FORCECRC))
        code_header->crc16         = OPTS_OPTION_U16(OPTION_FORCED_CRC);
    else
        code_header->crc16         = code->crc;
    code_header->entfield          = code->entfields;

    if (OPTS_FLAG(DARKPLACES_STRING_TABLE_BUG)) {
        /* >= + P */
        vec_push(code->chars, '\0'); /* > */
        vec_push(code->chars, '\0'); /* = */
        vec_push(code->chars, '\0'); /* P */
    }

    /* ensure all data is in LE format */
    util_swap_header(code_header);

    /*
     * These are not part of the header but we ensure LE format here to save on duplicated
     * code.
     */

    util_swap_statements (code->statements);
    util_swap_defs_fields(code->defs);
    util_swap_defs_fields(code->fields);
    util_swap_functions  (code->functions);
    util_swap_globals    (code->globals);

    if (!OPTS_OPTION_BOOL(OPTION_QUIET)) {
        if (lnofile)
            con_out("writing '%s' and '%s'...\n", filename, lnofile);
        else
            con_out("writing '%s'\n", filename);
    }

    if (!OPTS_OPTION_BOOL(OPTION_QUIET) &&
        !OPTS_OPTION_BOOL(OPTION_PP_ONLY))
    {
        char buffer[1024];
        con_out("\nOptimizations:\n");
        for (i = 0; i < COUNT_OPTIMIZATIONS; ++i) {
            if (opts_optimizationcount[i]) {
                util_optimizationtostr(opts_opt_list[i].name, buffer, sizeof(buffer));
                con_out(
                    "    %s: %u\n",
                    buffer,
                    (unsigned int)opts_optimizationcount[i]
                );
            }
        }
    }
}

static void code_stats(const char *filename, const char *lnofile, code_t *code, prog_header_t *code_header) {
    if (OPTS_OPTION_BOOL(OPTION_QUIET) ||
        OPTS_OPTION_BOOL(OPTION_PP_ONLY))
            return;

    con_out("\nFile statistics:\n");
    con_out("    dat:\n");
    con_out("        name: %s\n",         filename);
    con_out("        size: %u (bytes)\n", code_size_binary(code, code_header));
    con_out("        crc:  0x%04X\n",     code->crc);

    if (lnofile) {
        con_out("    lno:\n");
        con_out("        name: %s\n",  lnofile);
        con_out("        size: %u (bytes)\n",  code_size_debug(code, code_header));
    }

    con_out("\n");
}

/*
 * Same principle except this one allocates memory and writes the lno(optional) and the dat file
 * directly out to allocated memory. Which is actually very useful for the future library support
 * we're going to add.
 */
#if 0
static bool code_write_memory(code_t *code, uint8_t **datmem, size_t *sizedat, uint8_t **lnomem, size_t *sizelno) GMQCC_UNUSED {
    prog_header_t code_header;
    uint32_t      offset  = 0;

    if (!datmem)
        return false;

    code_create_header(code, &code_header, "<<memory>>", "<<memory>>");

    #define WRITE_CHUNK(C,X,S)                                     \
        do {                                                       \
            memcpy((void*)(&(*C)[offset]), (const void*)(X), (S)); \
            offset += (S);                                         \
        } while (0)

    /* Calculate size required to store entire file out to memory */
    if (lnomem) {
        uint32_t version = 1;

        *sizelno = code_size_debug(code, &code_header);
        *lnomem  = (uint8_t*)mem_a(*sizelno);

        WRITE_CHUNK(lnomem, "LNOF",                         4);
        WRITE_CHUNK(lnomem, &version,                       sizeof(version));
        WRITE_CHUNK(lnomem, &code_header.defs.length,       sizeof(code_header.defs.length));
        WRITE_CHUNK(lnomem, &code_header.globals.length,    sizeof(code_header.globals.length));
        WRITE_CHUNK(lnomem, &code_header.fields.length,     sizeof(code_header.fields.length));
        WRITE_CHUNK(lnomem, &code_header.statements.length, sizeof(code_header.statements.length));

        /* something went terribly wrong */
        if (offset != *sizelno) {
            mem_d(*lnomem);
            *sizelno = 0;
            return false;
        }
        offset = 0;
    }

    /* Write out the dat */
    *sizedat = code_size_binary(code, &code_header);
    *datmem  = (uint8_t*)mem_a(*sizedat);

    WRITE_CHUNK(datmem, &code_header,     sizeof(prog_header_t));
    WRITE_CHUNK(datmem, code->statements, sizeof(prog_section_statement_t) * vec_size(code->statements));
    WRITE_CHUNK(datmem, code->defs,       sizeof(prog_section_def_t)       * vec_size(code->defs));
    WRITE_CHUNK(datmem, code->fields,     sizeof(prog_section_field_t)     * vec_size(code->fields));
    WRITE_CHUNK(datmem, code->functions,  sizeof(prog_section_function_t)  * vec_size(code->functions));
    WRITE_CHUNK(datmem, code->globals,    sizeof(int32_t)                  * vec_size(code->globals));
    WRITE_CHUNK(datmem, code->chars,      1                                * vec_size(code->chars));

    vec_free(code->statements);
    vec_free(code->linenums);
    vec_free(code->columnnums);
    vec_free(code->defs);
    vec_free(code->fields);
    vec_free(code->functions);
    vec_free(code->globals);
    vec_free(code->chars);

    util_htdel(code->string_cache);
    mem_d(code);
    code_stats("<<memory>>", (lnomem) ? "<<memory>>" : NULL, code, &code_header);
    return true;
}
#endif /*!#if 0 reenable when ready to be used */
#undef WRITE_CHUNK

bool code_write(code_t *code, const char *filename, const char *lnofile) {
    prog_header_t  code_header;
    fs_file_t     *fp = NULL;

    code_create_header(code, &code_header, filename, lnofile);

    if (lnofile) {
        uint32_t version = 1;

        fp = fs_file_open(lnofile, "wb");
        if (!fp)
            return false;

        util_endianswap(&version,         1,                          sizeof(version));
        util_endianswap(code->linenums,   vec_size(code->linenums),   sizeof(code->linenums[0]));
        util_endianswap(code->columnnums, vec_size(code->columnnums), sizeof(code->columnnums[0]));

        if (fs_file_write("LNOF",                          4,                                      1,                          fp) != 1 ||
            fs_file_write(&version,                        sizeof(version),                        1,                          fp) != 1 ||
            fs_file_write(&code_header.defs.length,        sizeof(code_header.defs.length),        1,                          fp) != 1 ||
            fs_file_write(&code_header.globals.length,     sizeof(code_header.globals.length),     1,                          fp) != 1 ||
            fs_file_write(&code_header.fields.length,      sizeof(code_header.fields.length),      1,                          fp) != 1 ||
            fs_file_write(&code_header.statements.length,  sizeof(code_header.statements.length),  1,                          fp) != 1 ||
            fs_file_write(code->linenums,                  sizeof(code->linenums[0]),              vec_size(code->linenums),   fp) != vec_size(code->linenums) ||
            fs_file_write(code->columnnums,                sizeof(code->columnnums[0]),            vec_size(code->columnnums), fp) != vec_size(code->columnnums))
        {
            con_err("failed to write lno file\n");
        }

        fs_file_close(fp);
        fp = NULL;
    }

    fp = fs_file_open(filename, "wb");
    if (!fp)
        return false;

    if (1                          != fs_file_write(&code_header,     sizeof(prog_header_t)           , 1                         , fp) ||
        vec_size(code->statements) != fs_file_write(code->statements, sizeof(prog_section_statement_t), vec_size(code->statements), fp) ||
        vec_size(code->defs)       != fs_file_write(code->defs,       sizeof(prog_section_def_t)      , vec_size(code->defs)      , fp) ||
        vec_size(code->fields)     != fs_file_write(code->fields,     sizeof(prog_section_field_t)    , vec_size(code->fields)    , fp) ||
        vec_size(code->functions)  != fs_file_write(code->functions,  sizeof(prog_section_function_t) , vec_size(code->functions) , fp) ||
        vec_size(code->globals)    != fs_file_write(code->globals,    sizeof(int32_t)                 , vec_size(code->globals)   , fp) ||
        vec_size(code->chars)      != fs_file_write(code->chars,      1                               , vec_size(code->chars)     , fp))
    {
        fs_file_close(fp);
        return false;
    }

    fs_file_close(fp);
    code_stats(filename, lnofile, code, &code_header);
    return true;
}

void code_cleanup(code_t *code) {
    vec_free(code->statements);
    vec_free(code->linenums);
    vec_free(code->columnnums);
    vec_free(code->defs);
    vec_free(code->fields);
    vec_free(code->functions);
    vec_free(code->globals);
    vec_free(code->chars);

    util_htdel(code->string_cache);

    mem_d(code);
}
