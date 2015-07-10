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
#ifndef GMQCC_PARSER_HDR
#define GMQCC_PARSER_HDR
#include "gmqcc.h"
#include "lexer.h"
#include "ast.h"

typedef struct intrin_s intrin_t;
typedef struct parser_s parser_t;

typedef struct {
    struct parser_s *parser;
    ast_value      **imm_float;              /* vector<ast_value*> */
    ast_value      **imm_vector;             /* vector<ast_value*> */
    ast_value      **imm_string;             /* vector<ast_value*> */
    hash_table_t    *imm_string_untranslate; /* map<string, ast_value*> */
    hash_table_t    *imm_string_dotranslate; /* map<string, ast_value*> */
} fold_t;

typedef struct {
    ast_expression *(*intrin)(intrin_t *);
    const char       *name;
    const char       *alias;
    size_t            args;
} intrin_func_t;

struct intrin_s {
    intrin_func_t  *intrinsics;              /* vector<intrin_func_t>   */
    ast_expression **generated;              /* vector<ast_expression*> */
    parser_t       *parser;
    fold_t         *fold;
};

#define parser_ctx(p) ((p)->lex->tok.ctx)

struct parser_s {
    lex_file *lex;
    int      tok;

    bool     ast_cleaned;

    ast_expression **globals;
    ast_expression **fields;
    ast_function **functions;
    size_t         translated;

    /* must be deleted first, they reference immediates and values */
    ast_value    **accessors;

    ast_value *nil;
    ast_value *reserved_version;

    size_t crc_globals;
    size_t crc_fields;

    ast_function *function;
    ht            aliases;

    /* All the labels the function defined...
     * Should they be in ast_function instead?
     */
    ast_label  **labels;
    ast_goto   **gotos;
    const char **breaks;
    const char **continues;

    /* A list of hashtables for each scope */
    ht *variables;
    ht htfields;
    ht htglobals;
    ht *typedefs;

    /* same as above but for the spelling corrector */
    correct_trie_t  **correct_variables;
    size_t         ***correct_variables_score;  /* vector of vector of size_t* */

    /* not to be used directly, we use the hash table */
    ast_expression **_locals;
    size_t          *_blocklocals;
    ast_value      **_typedefs;
    size_t          *_blocktypedefs;
    lex_ctx_t         *_block_ctx;

    /* we store the '=' operator info */
    const oper_info *assign_op;

    /* magic values */
    ast_value *const_vec[3];

    /* pragma flags */
    bool noref;

    /* collected information */
    size_t     max_param_count;

    fold_t   *fold;
    intrin_t *intrin;
};


/* parser.c */
char           *parser_strdup     (const char *str);
ast_expression *parser_find_global(parser_t *parser, const char *name);

/* fold.c */
fold_t         *fold_init           (parser_t *);
void            fold_cleanup        (fold_t *);
ast_expression *fold_constgen_float (fold_t *, qcfloat_t, bool);
ast_expression *fold_constgen_vector(fold_t *, vec3_t);
ast_expression *fold_constgen_string(fold_t *, const char *, bool);
bool            fold_generate       (fold_t *, ir_builder *);
ast_expression *fold_op             (fold_t *, const oper_info *, ast_expression **);
ast_expression *fold_intrin         (fold_t *, const char      *, ast_expression **);

ast_expression *fold_binary         (lex_ctx_t ctx, int, ast_expression *, ast_expression *);
int             fold_cond_ifthen    (ir_value *, ast_function *, ast_ifthen  *);
int             fold_cond_ternary   (ir_value *, ast_function *, ast_ternary *);

/* intrin.c */
intrin_t       *intrin_init            (parser_t *parser);
void            intrin_cleanup         (intrin_t *intrin);
ast_expression *intrin_fold            (intrin_t *intrin, ast_value *, ast_expression **);
ast_expression *intrin_func            (intrin_t *intrin, const char *name);
ast_expression *intrin_debug_typestring(intrin_t *intrin);

#endif
