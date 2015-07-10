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
#include <stdlib.h>
#include <string.h>

#include "gmqcc.h"
#include "ast.h"
#include "parser.h"

#define ast_instantiate(T, ctx, destroyfn)                          \
    T* self = (T*)mem_a(sizeof(T));                                 \
    if (!self) {                                                    \
        return NULL;                                                \
    }                                                               \
    ast_node_init((ast_node*)self, ctx, TYPE_##T);                  \
    ( (ast_node*)self )->destroy = (ast_node_delete*)destroyfn

/*
 * forward declarations, these need not be in ast.h for obvious
 * static reasons.
 */
static bool ast_member_codegen(ast_member*, ast_function*, bool lvalue, ir_value**);
static void ast_array_index_delete(ast_array_index*);
static bool ast_array_index_codegen(ast_array_index*, ast_function*, bool lvalue, ir_value**);
static void ast_argpipe_delete(ast_argpipe*);
static bool ast_argpipe_codegen(ast_argpipe*, ast_function*, bool lvalue, ir_value**);
static void ast_store_delete(ast_store*);
static bool ast_store_codegen(ast_store*, ast_function*, bool lvalue, ir_value**);
static void ast_ifthen_delete(ast_ifthen*);
static bool ast_ifthen_codegen(ast_ifthen*, ast_function*, bool lvalue, ir_value**);
static void ast_ternary_delete(ast_ternary*);
static bool ast_ternary_codegen(ast_ternary*, ast_function*, bool lvalue, ir_value**);
static void ast_loop_delete(ast_loop*);
static bool ast_loop_codegen(ast_loop*, ast_function*, bool lvalue, ir_value**);
static void ast_breakcont_delete(ast_breakcont*);
static bool ast_breakcont_codegen(ast_breakcont*, ast_function*, bool lvalue, ir_value**);
static void ast_switch_delete(ast_switch*);
static bool ast_switch_codegen(ast_switch*, ast_function*, bool lvalue, ir_value**);
static void ast_label_delete(ast_label*);
static void ast_label_register_goto(ast_label*, ast_goto*);
static bool ast_label_codegen(ast_label*, ast_function*, bool lvalue, ir_value**);
static bool ast_goto_codegen(ast_goto*, ast_function*, bool lvalue, ir_value**);
static void ast_goto_delete(ast_goto*);
static void ast_call_delete(ast_call*);
static bool ast_call_codegen(ast_call*, ast_function*, bool lvalue, ir_value**);
static bool ast_block_codegen(ast_block*, ast_function*, bool lvalue, ir_value**);
static void ast_unary_delete(ast_unary*);
static bool ast_unary_codegen(ast_unary*, ast_function*, bool lvalue, ir_value**);
static void ast_entfield_delete(ast_entfield*);
static bool ast_entfield_codegen(ast_entfield*, ast_function*, bool lvalue, ir_value**);
static void ast_return_delete(ast_return*);
static bool ast_return_codegen(ast_return*, ast_function*, bool lvalue, ir_value**);
static void ast_binstore_delete(ast_binstore*);
static bool ast_binstore_codegen(ast_binstore*, ast_function*, bool lvalue, ir_value**);
static void ast_binary_delete(ast_binary*);
static bool ast_binary_codegen(ast_binary*, ast_function*, bool lvalue, ir_value**);
static bool ast_state_codegen(ast_state*, ast_function*, bool lvalue, ir_value**);

/* It must not be possible to get here. */
static GMQCC_NORETURN void _ast_node_destroy(ast_node *self)
{
    (void)self;
    con_err("ast node missing destroy()\n");
    exit(EXIT_FAILURE);
}

/* Initialize main ast node aprts */
static void ast_node_init(ast_node *self, lex_ctx_t ctx, int nodetype)
{
    self->context = ctx;
    self->destroy = &_ast_node_destroy;
    self->keep    = false;
    self->nodetype = nodetype;
    self->side_effects = false;
}

/* weight and side effects */
static void _ast_propagate_effects(ast_node *self, ast_node *other)
{
    if (ast_side_effects(other))
        ast_side_effects(self) = true;
}
#define ast_propagate_effects(s,o) _ast_propagate_effects(((ast_node*)(s)), ((ast_node*)(o)))

/* General expression initialization */
static void ast_expression_init(ast_expression *self,
                                ast_expression_codegen *codegen)
{
    self->codegen  = codegen;
    self->vtype    = TYPE_VOID;
    self->next     = NULL;
    self->outl     = NULL;
    self->outr     = NULL;
    self->params   = NULL;
    self->count    = 0;
    self->varparam = NULL;
    self->flags    = 0;
    if (OPTS_OPTION_BOOL(OPTION_COVERAGE))
        self->flags |= AST_FLAG_BLOCK_COVERAGE;
}

static void ast_expression_delete(ast_expression *self)
{
    size_t i;
    if (self->next)
        ast_delete(self->next);
    for (i = 0; i < vec_size(self->params); ++i) {
        ast_delete(self->params[i]);
    }
    vec_free(self->params);
    if (self->varparam)
        ast_delete(self->varparam);
}

static void ast_expression_delete_full(ast_expression *self)
{
    ast_expression_delete(self);
    mem_d(self);
}

ast_value* ast_value_copy(const ast_value *self)
{
    size_t i;
    const ast_expression *fromex;
    ast_expression       *selfex;
    ast_value *cp = ast_value_new(self->expression.node.context, self->name, self->expression.vtype);
    if (self->expression.next) {
        cp->expression.next = ast_type_copy(self->expression.node.context, self->expression.next);
    }
    fromex   = &self->expression;
    selfex = &cp->expression;
    selfex->count    = fromex->count;
    selfex->flags    = fromex->flags;
    for (i = 0; i < vec_size(fromex->params); ++i) {
        ast_value *v = ast_value_copy(fromex->params[i]);
        vec_push(selfex->params, v);
    }
    return cp;
}

void ast_type_adopt_impl(ast_expression *self, const ast_expression *other)
{
    size_t i;
    const ast_expression *fromex;
    ast_expression       *selfex;
    self->vtype = other->vtype;
    if (other->next) {
        self->next = (ast_expression*)ast_type_copy(ast_ctx(self), other->next);
    }
    fromex = other;
    selfex = self;
    selfex->count    = fromex->count;
    selfex->flags    = fromex->flags;
    for (i = 0; i < vec_size(fromex->params); ++i) {
        ast_value *v = ast_value_copy(fromex->params[i]);
        vec_push(selfex->params, v);
    }
}

static ast_expression* ast_shallow_type(lex_ctx_t ctx, int vtype)
{
    ast_instantiate(ast_expression, ctx, ast_expression_delete_full);
    ast_expression_init(self, NULL);
    self->codegen = NULL;
    self->next    = NULL;
    self->vtype   = vtype;
    return self;
}

ast_expression* ast_type_copy(lex_ctx_t ctx, const ast_expression *ex)
{
    size_t i;
    const ast_expression *fromex;
    ast_expression       *selfex;

    if (!ex)
        return NULL;
    else
    {
        ast_instantiate(ast_expression, ctx, ast_expression_delete_full);
        ast_expression_init(self, NULL);

        fromex = ex;
        selfex = self;

        /* This may never be codegen()d */
        selfex->codegen = NULL;

        selfex->vtype = fromex->vtype;
        if (fromex->next)
            selfex->next = ast_type_copy(ctx, fromex->next);
        else
            selfex->next = NULL;

        selfex->count    = fromex->count;
        selfex->flags    = fromex->flags;
        for (i = 0; i < vec_size(fromex->params); ++i) {
            ast_value *v = ast_value_copy(fromex->params[i]);
            vec_push(selfex->params, v);
        }

        return self;
    }
}

bool ast_compare_type(ast_expression *a, ast_expression *b)
{
    if (a->vtype == TYPE_NIL ||
        b->vtype == TYPE_NIL)
        return true;
    if (a->vtype != b->vtype)
        return false;
    if (!a->next != !b->next)
        return false;
    if (vec_size(a->params) != vec_size(b->params))
        return false;
    if ((a->flags & AST_FLAG_TYPE_MASK) !=
        (b->flags & AST_FLAG_TYPE_MASK) )
    {
        return false;
    }
    if (vec_size(a->params)) {
        size_t i;
        for (i = 0; i < vec_size(a->params); ++i) {
            if (!ast_compare_type((ast_expression*)a->params[i],
                                  (ast_expression*)b->params[i]))
                return false;
        }
    }
    if (a->next)
        return ast_compare_type(a->next, b->next);
    return true;
}

static size_t ast_type_to_string_impl(ast_expression *e, char *buf, size_t bufsize, size_t pos)
{
    const char *typestr;
    size_t typelen;
    size_t i;

    if (!e) {
        if (pos + 6 >= bufsize)
            goto full;
        util_strncpy(buf + pos, "(null)", 6);
        return pos + 6;
    }

    if (pos + 1 >= bufsize)
        goto full;

    switch (e->vtype) {
        case TYPE_VARIANT:
            util_strncpy(buf + pos, "(variant)", 9);
            return pos + 9;

        case TYPE_FIELD:
            buf[pos++] = '.';
            return ast_type_to_string_impl(e->next, buf, bufsize, pos);

        case TYPE_POINTER:
            if (pos + 3 >= bufsize)
                goto full;
            buf[pos++] = '*';
            buf[pos++] = '(';
            pos = ast_type_to_string_impl(e->next, buf, bufsize, pos);
            if (pos + 1 >= bufsize)
                goto full;
            buf[pos++] = ')';
            return pos;

        case TYPE_FUNCTION:
            pos = ast_type_to_string_impl(e->next, buf, bufsize, pos);
            if (pos + 2 >= bufsize)
                goto full;
            if (!vec_size(e->params)) {
                buf[pos++] = '(';
                buf[pos++] = ')';
                return pos;
            }
            buf[pos++] = '(';
            pos = ast_type_to_string_impl((ast_expression*)(e->params[0]), buf, bufsize, pos);
            for (i = 1; i < vec_size(e->params); ++i) {
                if (pos + 2 >= bufsize)
                    goto full;
                buf[pos++] = ',';
                buf[pos++] = ' ';
                pos = ast_type_to_string_impl((ast_expression*)(e->params[i]), buf, bufsize, pos);
            }
            if (pos + 1 >= bufsize)
                goto full;
            buf[pos++] = ')';
            return pos;

        case TYPE_ARRAY:
            pos = ast_type_to_string_impl(e->next, buf, bufsize, pos);
            if (pos + 1 >= bufsize)
                goto full;
            buf[pos++] = '[';
            pos += util_snprintf(buf + pos, bufsize - pos - 1, "%i", (int)e->count);
            if (pos + 1 >= bufsize)
                goto full;
            buf[pos++] = ']';
            return pos;

        default:
            typestr = type_name[e->vtype];
            typelen = strlen(typestr);
            if (pos + typelen >= bufsize)
                goto full;
            util_strncpy(buf + pos, typestr, typelen);
            return pos + typelen;
    }

full:
    buf[bufsize-3] = '.';
    buf[bufsize-2] = '.';
    buf[bufsize-1] = '.';
    return bufsize;
}

void ast_type_to_string(ast_expression *e, char *buf, size_t bufsize)
{
    size_t pos = ast_type_to_string_impl(e, buf, bufsize-1, 0);
    buf[pos] = 0;
}

static bool ast_value_codegen(ast_value *self, ast_function *func, bool lvalue, ir_value **out);
ast_value* ast_value_new(lex_ctx_t ctx, const char *name, int t)
{
    ast_instantiate(ast_value, ctx, ast_value_delete);
    ast_expression_init((ast_expression*)self,
                        (ast_expression_codegen*)&ast_value_codegen);
    self->expression.node.keep = true; /* keep */

    self->name = name ? util_strdup(name) : NULL;
    self->expression.vtype = t;
    self->expression.next  = NULL;
    self->isfield  = false;
    self->cvq      = CV_NONE;
    self->hasvalue = false;
    self->isimm    = false;
    self->inexact  = false;
    self->uses     = 0;
    memset(&self->constval, 0, sizeof(self->constval));
    self->initlist = NULL;

    self->ir_v           = NULL;
    self->ir_values      = NULL;
    self->ir_value_count = 0;

    self->setter = NULL;
    self->getter = NULL;
    self->desc   = NULL;

    self->argcounter = NULL;
    self->intrinsic = false;

    return self;
}

void ast_value_delete(ast_value* self)
{
    if (self->name)
        mem_d((void*)self->name);
    if (self->argcounter)
        mem_d((void*)self->argcounter);
    if (self->hasvalue) {
        switch (self->expression.vtype)
        {
        case TYPE_STRING:
            mem_d((void*)self->constval.vstring);
            break;
        case TYPE_FUNCTION:
            /* unlink us from the function node */
            self->constval.vfunc->vtype = NULL;
            break;
        /* NOTE: delete function? currently collected in
         * the parser structure
         */
        default:
            break;
        }
    }
    if (self->ir_values)
        mem_d(self->ir_values);

    if (self->desc)
        mem_d(self->desc);

    if (self->initlist) {
        if (self->expression.next->vtype == TYPE_STRING) {
            /* strings are allocated, free them */
            size_t i, len = vec_size(self->initlist);
            /* in theory, len should be expression.count
             * but let's not take any chances */
            for (i = 0; i < len; ++i) {
                if (self->initlist[i].vstring)
                    mem_d(self->initlist[i].vstring);
            }
        }
        vec_free(self->initlist);
    }

    ast_expression_delete((ast_expression*)self);
    mem_d(self);
}

void ast_value_params_add(ast_value *self, ast_value *p)
{
    vec_push(self->expression.params, p);
}

bool ast_value_set_name(ast_value *self, const char *name)
{
    if (self->name)
        mem_d((void*)self->name);
    self->name = util_strdup(name);
    return !!self->name;
}

ast_binary* ast_binary_new(lex_ctx_t ctx, int op,
                           ast_expression* left, ast_expression* right)
{
    ast_instantiate(ast_binary, ctx, ast_binary_delete);
    ast_expression_init((ast_expression*)self, (ast_expression_codegen*)&ast_binary_codegen);

    if (ast_istype(right, ast_unary) && OPTS_OPTIMIZATION(OPTIM_PEEPHOLE)) {
        ast_unary      *unary  = ((ast_unary*)right);
        ast_expression *normal = unary->operand;

        /* make a-(-b) => a + b */
        if (unary->op == VINSTR_NEG_F || unary->op == VINSTR_NEG_V) {
            if (op == INSTR_SUB_F) {
                op = INSTR_ADD_F;
                right = normal;
                ++opts_optimizationcount[OPTIM_PEEPHOLE];
            } else if (op == INSTR_SUB_V) {
                op = INSTR_ADD_V;
                right = normal;
                ++opts_optimizationcount[OPTIM_PEEPHOLE];
            }
        }
    }

    self->op = op;
    self->left = left;
    self->right = right;
    self->right_first = false;

    ast_propagate_effects(self, left);
    ast_propagate_effects(self, right);

    if (op >= INSTR_EQ_F && op <= INSTR_GT)
        self->expression.vtype = TYPE_FLOAT;
    else if (op == INSTR_AND || op == INSTR_OR) {
        if (OPTS_FLAG(PERL_LOGIC))
            ast_type_adopt(self, right);
        else
            self->expression.vtype = TYPE_FLOAT;
    }
    else if (op == INSTR_BITAND || op == INSTR_BITOR)
        self->expression.vtype = TYPE_FLOAT;
    else if (op == INSTR_MUL_VF || op == INSTR_MUL_FV)
        self->expression.vtype = TYPE_VECTOR;
    else if (op == INSTR_MUL_V)
        self->expression.vtype = TYPE_FLOAT;
    else
        self->expression.vtype = left->vtype;

    /* references all */
    self->refs = AST_REF_ALL;

    return self;
}

void ast_binary_delete(ast_binary *self)
{
    if (self->refs & AST_REF_LEFT)  ast_unref(self->left);
    if (self->refs & AST_REF_RIGHT) ast_unref(self->right);

    ast_expression_delete((ast_expression*)self);
    mem_d(self);
}

ast_binstore* ast_binstore_new(lex_ctx_t ctx, int storop, int op,
                               ast_expression* left, ast_expression* right)
{
    ast_instantiate(ast_binstore, ctx, ast_binstore_delete);
    ast_expression_init((ast_expression*)self, (ast_expression_codegen*)&ast_binstore_codegen);

    ast_side_effects(self) = true;

    self->opstore = storop;
    self->opbin   = op;
    self->dest    = left;
    self->source  = right;

    self->keep_dest = false;

    ast_type_adopt(self, left);
    return self;
}

void ast_binstore_delete(ast_binstore *self)
{
    if (!self->keep_dest)
        ast_unref(self->dest);
    ast_unref(self->source);
    ast_expression_delete((ast_expression*)self);
    mem_d(self);
}

ast_unary* ast_unary_new(lex_ctx_t ctx, int op,
                         ast_expression *expr)
{
    ast_instantiate(ast_unary, ctx, ast_unary_delete);
    ast_expression_init((ast_expression*)self, (ast_expression_codegen*)&ast_unary_codegen);

    self->op      = op;
    self->operand = expr;


    if (ast_istype(expr, ast_unary) && OPTS_OPTIMIZATION(OPTIM_PEEPHOLE)) {
        ast_unary *prev = (ast_unary*)((ast_unary*)expr)->operand;

        /* Handle for double negation */
        if (((ast_unary*)expr)->op == op)
            prev = (ast_unary*)((ast_unary*)expr)->operand;

        if (ast_istype(prev, ast_unary)) {
            ast_expression_delete((ast_expression*)self);
            mem_d(self);
            ++opts_optimizationcount[OPTIM_PEEPHOLE];
            return prev;
        }
    }

    ast_propagate_effects(self, expr);

    if ((op >= INSTR_NOT_F && op <= INSTR_NOT_FNC) || op == VINSTR_NEG_F) {
        self->expression.vtype = TYPE_FLOAT;
    } else if (op == VINSTR_NEG_V) {
        self->expression.vtype = TYPE_VECTOR;
    } else {
        compile_error(ctx, "cannot determine type of unary operation %s", util_instr_str[op]);
    }

    return self;
}

void ast_unary_delete(ast_unary *self)
{
    if (self->operand) ast_unref(self->operand);
    ast_expression_delete((ast_expression*)self);
    mem_d(self);
}

ast_return* ast_return_new(lex_ctx_t ctx, ast_expression *expr)
{
    ast_instantiate(ast_return, ctx, ast_return_delete);
    ast_expression_init((ast_expression*)self, (ast_expression_codegen*)&ast_return_codegen);

    self->operand = expr;

    if (expr)
        ast_propagate_effects(self, expr);

    return self;
}

void ast_return_delete(ast_return *self)
{
    if (self->operand)
        ast_unref(self->operand);
    ast_expression_delete((ast_expression*)self);
    mem_d(self);
}

ast_entfield* ast_entfield_new(lex_ctx_t ctx, ast_expression *entity, ast_expression *field)
{
    if (field->vtype != TYPE_FIELD) {
        compile_error(ctx, "ast_entfield_new with expression not of type field");
        return NULL;
    }
    return ast_entfield_new_force(ctx, entity, field, field->next);
}

ast_entfield* ast_entfield_new_force(lex_ctx_t ctx, ast_expression *entity, ast_expression *field, const ast_expression *outtype)
{
    ast_instantiate(ast_entfield, ctx, ast_entfield_delete);

    if (!outtype) {
        mem_d(self);
        /* Error: field has no type... */
        return NULL;
    }

    ast_expression_init((ast_expression*)self, (ast_expression_codegen*)&ast_entfield_codegen);

    self->entity = entity;
    self->field  = field;
    ast_propagate_effects(self, entity);
    ast_propagate_effects(self, field);

    ast_type_adopt(self, outtype);
    return self;
}

void ast_entfield_delete(ast_entfield *self)
{
    ast_unref(self->entity);
    ast_unref(self->field);
    ast_expression_delete((ast_expression*)self);
    mem_d(self);
}

ast_member* ast_member_new(lex_ctx_t ctx, ast_expression *owner, unsigned int field, const char *name)
{
    ast_instantiate(ast_member, ctx, ast_member_delete);
    if (field >= 3) {
        mem_d(self);
        return NULL;
    }

    if (owner->vtype != TYPE_VECTOR &&
        owner->vtype != TYPE_FIELD) {
        compile_error(ctx, "member-access on an invalid owner of type %s", type_name[owner->vtype]);
        mem_d(self);
        return NULL;
    }

    ast_expression_init((ast_expression*)self, (ast_expression_codegen*)&ast_member_codegen);
    self->expression.node.keep = true; /* keep */

    if (owner->vtype == TYPE_VECTOR) {
        self->expression.vtype = TYPE_FLOAT;
        self->expression.next  = NULL;
    } else {
        self->expression.vtype = TYPE_FIELD;
        self->expression.next = ast_shallow_type(ctx, TYPE_FLOAT);
    }

    self->rvalue = false;
    self->owner  = owner;
    ast_propagate_effects(self, owner);

    self->field = field;
    if (name)
        self->name = util_strdup(name);
    else
        self->name = NULL;

    return self;
}

void ast_member_delete(ast_member *self)
{
    /* The owner is always an ast_value, which has .keep=true,
     * also: ast_members are usually deleted after the owner, thus
     * this will cause invalid access
    ast_unref(self->owner);
     * once we allow (expression).x to access a vector-member, we need
     * to change this: preferably by creating an alternate ast node for this
     * purpose that is not garbage-collected.
    */
    ast_expression_delete((ast_expression*)self);
    mem_d(self->name);
    mem_d(self);
}

bool ast_member_set_name(ast_member *self, const char *name)
{
    if (self->name)
        mem_d((void*)self->name);
    self->name = util_strdup(name);
    return !!self->name;
}

ast_array_index* ast_array_index_new(lex_ctx_t ctx, ast_expression *array, ast_expression *index)
{
    ast_expression *outtype;
    ast_instantiate(ast_array_index, ctx, ast_array_index_delete);

    outtype = array->next;
    if (!outtype) {
        mem_d(self);
        /* Error: field has no type... */
        return NULL;
    }

    ast_expression_init((ast_expression*)self, (ast_expression_codegen*)&ast_array_index_codegen);

    self->array = array;
    self->index = index;
    ast_propagate_effects(self, array);
    ast_propagate_effects(self, index);

    ast_type_adopt(self, outtype);
    if (array->vtype == TYPE_FIELD && outtype->vtype == TYPE_ARRAY) {
        if (self->expression.vtype != TYPE_ARRAY) {
            compile_error(ast_ctx(self), "array_index node on type");
            ast_array_index_delete(self);
            return NULL;
        }
        self->array = outtype;
        self->expression.vtype = TYPE_FIELD;
    }

    return self;
}

void ast_array_index_delete(ast_array_index *self)
{
    if (self->array)
        ast_unref(self->array);
    if (self->index)
        ast_unref(self->index);
    ast_expression_delete((ast_expression*)self);
    mem_d(self);
}

ast_argpipe* ast_argpipe_new(lex_ctx_t ctx, ast_expression *index)
{
    ast_instantiate(ast_argpipe, ctx, ast_argpipe_delete);
    ast_expression_init((ast_expression*)self, (ast_expression_codegen*)&ast_argpipe_codegen);
    self->index = index;
    self->expression.vtype = TYPE_NOEXPR;
    return self;
}

void ast_argpipe_delete(ast_argpipe *self)
{
    if (self->index)
        ast_unref(self->index);
    ast_expression_delete((ast_expression*)self);
    mem_d(self);
}

ast_ifthen* ast_ifthen_new(lex_ctx_t ctx, ast_expression *cond, ast_expression *ontrue, ast_expression *onfalse)
{
    ast_instantiate(ast_ifthen, ctx, ast_ifthen_delete);
    if (!ontrue && !onfalse) {
        /* because it is invalid */
        mem_d(self);
        return NULL;
    }
    ast_expression_init((ast_expression*)self, (ast_expression_codegen*)&ast_ifthen_codegen);

    self->cond     = cond;
    self->on_true  = ontrue;
    self->on_false = onfalse;
    ast_propagate_effects(self, cond);
    if (ontrue)
        ast_propagate_effects(self, ontrue);
    if (onfalse)
        ast_propagate_effects(self, onfalse);

    return self;
}

void ast_ifthen_delete(ast_ifthen *self)
{
    ast_unref(self->cond);
    if (self->on_true)
        ast_unref(self->on_true);
    if (self->on_false)
        ast_unref(self->on_false);
    ast_expression_delete((ast_expression*)self);
    mem_d(self);
}

ast_ternary* ast_ternary_new(lex_ctx_t ctx, ast_expression *cond, ast_expression *ontrue, ast_expression *onfalse)
{
    ast_expression *exprtype = ontrue;
    ast_instantiate(ast_ternary, ctx, ast_ternary_delete);
    /* This time NEITHER must be NULL */
    if (!ontrue || !onfalse) {
        mem_d(self);
        return NULL;
    }
    ast_expression_init((ast_expression*)self, (ast_expression_codegen*)&ast_ternary_codegen);

    self->cond     = cond;
    self->on_true  = ontrue;
    self->on_false = onfalse;
    ast_propagate_effects(self, cond);
    ast_propagate_effects(self, ontrue);
    ast_propagate_effects(self, onfalse);

    if (ontrue->vtype == TYPE_NIL)
        exprtype = onfalse;
    ast_type_adopt(self, exprtype);

    return self;
}

void ast_ternary_delete(ast_ternary *self)
{
    /* the if()s are only there because computed-gotos can set them
     * to NULL
     */
    if (self->cond)     ast_unref(self->cond);
    if (self->on_true)  ast_unref(self->on_true);
    if (self->on_false) ast_unref(self->on_false);
    ast_expression_delete((ast_expression*)self);
    mem_d(self);
}

ast_loop* ast_loop_new(lex_ctx_t ctx,
                       ast_expression *initexpr,
                       ast_expression *precond, bool pre_not,
                       ast_expression *postcond, bool post_not,
                       ast_expression *increment,
                       ast_expression *body)
{
    ast_instantiate(ast_loop, ctx, ast_loop_delete);
    ast_expression_init((ast_expression*)self, (ast_expression_codegen*)&ast_loop_codegen);

    self->initexpr  = initexpr;
    self->precond   = precond;
    self->postcond  = postcond;
    self->increment = increment;
    self->body      = body;

    self->pre_not   = pre_not;
    self->post_not  = post_not;

    if (initexpr)
        ast_propagate_effects(self, initexpr);
    if (precond)
        ast_propagate_effects(self, precond);
    if (postcond)
        ast_propagate_effects(self, postcond);
    if (increment)
        ast_propagate_effects(self, increment);
    if (body)
        ast_propagate_effects(self, body);

    return self;
}

void ast_loop_delete(ast_loop *self)
{
    if (self->initexpr)
        ast_unref(self->initexpr);
    if (self->precond)
        ast_unref(self->precond);
    if (self->postcond)
        ast_unref(self->postcond);
    if (self->increment)
        ast_unref(self->increment);
    if (self->body)
        ast_unref(self->body);
    ast_expression_delete((ast_expression*)self);
    mem_d(self);
}

ast_breakcont* ast_breakcont_new(lex_ctx_t ctx, bool iscont, unsigned int levels)
{
    ast_instantiate(ast_breakcont, ctx, ast_breakcont_delete);
    ast_expression_init((ast_expression*)self, (ast_expression_codegen*)&ast_breakcont_codegen);

    self->is_continue = iscont;
    self->levels      = levels;

    return self;
}

void ast_breakcont_delete(ast_breakcont *self)
{
    ast_expression_delete((ast_expression*)self);
    mem_d(self);
}

ast_switch* ast_switch_new(lex_ctx_t ctx, ast_expression *op)
{
    ast_instantiate(ast_switch, ctx, ast_switch_delete);
    ast_expression_init((ast_expression*)self, (ast_expression_codegen*)&ast_switch_codegen);

    self->operand = op;
    self->cases   = NULL;

    ast_propagate_effects(self, op);

    return self;
}

void ast_switch_delete(ast_switch *self)
{
    size_t i;
    ast_unref(self->operand);

    for (i = 0; i < vec_size(self->cases); ++i) {
        if (self->cases[i].value)
            ast_unref(self->cases[i].value);
        ast_unref(self->cases[i].code);
    }
    vec_free(self->cases);

    ast_expression_delete((ast_expression*)self);
    mem_d(self);
}

ast_label* ast_label_new(lex_ctx_t ctx, const char *name, bool undefined)
{
    ast_instantiate(ast_label, ctx, ast_label_delete);
    ast_expression_init((ast_expression*)self, (ast_expression_codegen*)&ast_label_codegen);

    self->expression.vtype = TYPE_NOEXPR;

    self->name      = util_strdup(name);
    self->irblock   = NULL;
    self->gotos     = NULL;
    self->undefined = undefined;

    return self;
}

void ast_label_delete(ast_label *self)
{
    mem_d((void*)self->name);
    vec_free(self->gotos);
    ast_expression_delete((ast_expression*)self);
    mem_d(self);
}

static void ast_label_register_goto(ast_label *self, ast_goto *g)
{
    vec_push(self->gotos, g);
}

ast_goto* ast_goto_new(lex_ctx_t ctx, const char *name)
{
    ast_instantiate(ast_goto, ctx, ast_goto_delete);
    ast_expression_init((ast_expression*)self, (ast_expression_codegen*)&ast_goto_codegen);

    self->name    = util_strdup(name);
    self->target  = NULL;
    self->irblock_from = NULL;

    return self;
}

void ast_goto_delete(ast_goto *self)
{
    mem_d((void*)self->name);
    ast_expression_delete((ast_expression*)self);
    mem_d(self);
}

void ast_goto_set_label(ast_goto *self, ast_label *label)
{
    self->target = label;
}

ast_state* ast_state_new(lex_ctx_t ctx, ast_expression *frame, ast_expression *think)
{
    ast_instantiate(ast_state, ctx, ast_state_delete);
    ast_expression_init((ast_expression*)self, (ast_expression_codegen*)&ast_state_codegen);
    self->framenum  = frame;
    self->nextthink = think;
    return self;
}

void ast_state_delete(ast_state *self)
{
    if (self->framenum)
        ast_unref(self->framenum);
    if (self->nextthink)
        ast_unref(self->nextthink);

    ast_expression_delete((ast_expression*)self);
    mem_d(self);
}

ast_call* ast_call_new(lex_ctx_t ctx,
                       ast_expression *funcexpr)
{
    ast_instantiate(ast_call, ctx, ast_call_delete);
    if (!funcexpr->next) {
        compile_error(ctx, "not a function");
        mem_d(self);
        return NULL;
    }
    ast_expression_init((ast_expression*)self, (ast_expression_codegen*)&ast_call_codegen);

    ast_side_effects(self) = true;

    self->params   = NULL;
    self->func     = funcexpr;
    self->va_count = NULL;

    ast_type_adopt(self, funcexpr->next);

    return self;
}

void ast_call_delete(ast_call *self)
{
    size_t i;
    for (i = 0; i < vec_size(self->params); ++i)
        ast_unref(self->params[i]);
    vec_free(self->params);

    if (self->func)
        ast_unref(self->func);

    if (self->va_count)
        ast_unref(self->va_count);

    ast_expression_delete((ast_expression*)self);
    mem_d(self);
}

static bool ast_call_check_vararg(ast_call *self, ast_expression *va_type, ast_expression *exp_type)
{
    char texp[1024];
    char tgot[1024];
    if (!exp_type)
        return true;
    if (!va_type || !ast_compare_type(va_type, exp_type))
    {
        if (va_type && exp_type)
        {
            ast_type_to_string(va_type,  tgot, sizeof(tgot));
            ast_type_to_string(exp_type, texp, sizeof(texp));
            if (OPTS_FLAG(UNSAFE_VARARGS)) {
                if (compile_warning(ast_ctx(self), WARN_UNSAFE_TYPES,
                                    "piped variadic argument differs in type: constrained to type %s, expected type %s",
                                    tgot, texp))
                    return false;
            } else {
                compile_error(ast_ctx(self),
                              "piped variadic argument differs in type: constrained to type %s, expected type %s",
                              tgot, texp);
                return false;
            }
        }
        else
        {
            ast_type_to_string(exp_type, texp, sizeof(texp));
            if (OPTS_FLAG(UNSAFE_VARARGS)) {
                if (compile_warning(ast_ctx(self), WARN_UNSAFE_TYPES,
                                    "piped variadic argument may differ in type: expected type %s",
                                    texp))
                    return false;
            } else {
                compile_error(ast_ctx(self),
                              "piped variadic argument may differ in type: expected type %s",
                              texp);
                return false;
            }
        }
    }
    return true;
}

bool ast_call_check_types(ast_call *self, ast_expression *va_type)
{
    char texp[1024];
    char tgot[1024];
    size_t i;
    bool   retval = true;
    const  ast_expression *func = self->func;
    size_t count = vec_size(self->params);
    if (count > vec_size(func->params))
        count = vec_size(func->params);

    for (i = 0; i < count; ++i) {
        if (ast_istype(self->params[i], ast_argpipe)) {
            /* warn about type safety instead */
            if (i+1 != count) {
                compile_error(ast_ctx(self), "argpipe must be the last parameter to a function call");
                return false;
            }
            if (!ast_call_check_vararg(self, va_type, (ast_expression*)func->params[i]))
                retval = false;
        }
        else if (!ast_compare_type(self->params[i], (ast_expression*)(func->params[i])))
        {
            ast_type_to_string(self->params[i], tgot, sizeof(tgot));
            ast_type_to_string((ast_expression*)func->params[i], texp, sizeof(texp));
            compile_error(ast_ctx(self), "invalid type for parameter %u in function call: expected %s, got %s",
                     (unsigned int)(i+1), texp, tgot);
            /* we don't immediately return */
            retval = false;
        }
    }
    count = vec_size(self->params);
    if (count > vec_size(func->params) && func->varparam) {
        for (; i < count; ++i) {
            if (ast_istype(self->params[i], ast_argpipe)) {
                /* warn about type safety instead */
                if (i+1 != count) {
                    compile_error(ast_ctx(self), "argpipe must be the last parameter to a function call");
                    return false;
                }
                if (!ast_call_check_vararg(self, va_type, func->varparam))
                    retval = false;
            }
            else if (!ast_compare_type(self->params[i], func->varparam))
            {
                ast_type_to_string(self->params[i], tgot, sizeof(tgot));
                ast_type_to_string(func->varparam, texp, sizeof(texp));
                compile_error(ast_ctx(self), "invalid type for variadic parameter %u in function call: expected %s, got %s",
                         (unsigned int)(i+1), texp, tgot);
                /* we don't immediately return */
                retval = false;
            }
        }
    }
    return retval;
}

ast_store* ast_store_new(lex_ctx_t ctx, int op,
                         ast_expression *dest, ast_expression *source)
{
    ast_instantiate(ast_store, ctx, ast_store_delete);
    ast_expression_init((ast_expression*)self, (ast_expression_codegen*)&ast_store_codegen);

    ast_side_effects(self) = true;

    self->op = op;
    self->dest = dest;
    self->source = source;

    ast_type_adopt(self, dest);

    return self;
}

void ast_store_delete(ast_store *self)
{
    ast_unref(self->dest);
    ast_unref(self->source);
    ast_expression_delete((ast_expression*)self);
    mem_d(self);
}

ast_block* ast_block_new(lex_ctx_t ctx)
{
    ast_instantiate(ast_block, ctx, ast_block_delete);
    ast_expression_init((ast_expression*)self,
                        (ast_expression_codegen*)&ast_block_codegen);

    self->locals  = NULL;
    self->exprs   = NULL;
    self->collect = NULL;

    return self;
}

bool ast_block_add_expr(ast_block *self, ast_expression *e)
{
    ast_propagate_effects(self, e);
    vec_push(self->exprs, e);
    if (self->expression.next) {
        ast_delete(self->expression.next);
        self->expression.next = NULL;
    }
    ast_type_adopt(self, e);
    return true;
}

void ast_block_collect(ast_block *self, ast_expression *expr)
{
    vec_push(self->collect, expr);
    expr->node.keep = true;
}

void ast_block_delete(ast_block *self)
{
    size_t i;
    for (i = 0; i < vec_size(self->exprs); ++i)
        ast_unref(self->exprs[i]);
    vec_free(self->exprs);
    for (i = 0; i < vec_size(self->locals); ++i)
        ast_delete(self->locals[i]);
    vec_free(self->locals);
    for (i = 0; i < vec_size(self->collect); ++i)
        ast_delete(self->collect[i]);
    vec_free(self->collect);
    ast_expression_delete((ast_expression*)self);
    mem_d(self);
}

void ast_block_set_type(ast_block *self, ast_expression *from)
{
    if (self->expression.next)
        ast_delete(self->expression.next);
    ast_type_adopt(self, from);
}

ast_function* ast_function_new(lex_ctx_t ctx, const char *name, ast_value *vtype)
{
    ast_instantiate(ast_function, ctx, ast_function_delete);

    if (!vtype) {
        compile_error(ast_ctx(self), "internal error: ast_function_new condition 0");
        goto cleanup;
    } else if (vtype->hasvalue || vtype->expression.vtype != TYPE_FUNCTION) {
        compile_error(ast_ctx(self), "internal error: ast_function_new condition %i %i type=%i (probably 2 bodies?)",
                 (int)!vtype,
                 (int)vtype->hasvalue,
                 vtype->expression.vtype);
        goto cleanup;
    }

    self->vtype  = vtype;
    self->name   = name ? util_strdup(name) : NULL;
    self->blocks = NULL;

    self->labelcount = 0;
    self->builtin = 0;

    self->ir_func = NULL;
    self->curblock = NULL;

    self->breakblocks    = NULL;
    self->continueblocks = NULL;

    vtype->hasvalue = true;
    vtype->constval.vfunc = self;

    self->varargs          = NULL;
    self->argc             = NULL;
    self->fixedparams      = NULL;
    self->return_value     = NULL;

    self->static_names     = NULL;
    self->static_count     = 0;

    return self;

cleanup:
    mem_d(self);
    return NULL;
}

void ast_function_delete(ast_function *self)
{
    size_t i;
    if (self->name)
        mem_d((void*)self->name);
    if (self->vtype) {
        /* ast_value_delete(self->vtype); */
        self->vtype->hasvalue = false;
        self->vtype->constval.vfunc = NULL;
        /* We use unref - if it was stored in a global table it is supposed
         * to be deleted from *there*
         */
        ast_unref(self->vtype);
    }
    for (i = 0; i < vec_size(self->static_names); ++i)
        mem_d(self->static_names[i]);
    vec_free(self->static_names);
    for (i = 0; i < vec_size(self->blocks); ++i)
        ast_delete(self->blocks[i]);
    vec_free(self->blocks);
    vec_free(self->breakblocks);
    vec_free(self->continueblocks);
    if (self->varargs)
        ast_delete(self->varargs);
    if (self->argc)
        ast_delete(self->argc);
    if (self->fixedparams)
        ast_unref(self->fixedparams);
    if (self->return_value)
        ast_unref(self->return_value);
    mem_d(self);
}

const char* ast_function_label(ast_function *self, const char *prefix)
{
    size_t id;
    size_t len;
    char  *from;

    if (!OPTS_OPTION_BOOL(OPTION_DUMP)    &&
        !OPTS_OPTION_BOOL(OPTION_DUMPFIN) &&
        !OPTS_OPTION_BOOL(OPTION_DEBUG))
    {
        return NULL;
    }

    id  = (self->labelcount++);
    len = strlen(prefix);

    from = self->labelbuf + sizeof(self->labelbuf)-1;
    *from-- = 0;
    do {
        *from-- = (id%10) + '0';
        id /= 10;
    } while (id);
    ++from;
    memcpy(from - len, prefix, len);
    return from - len;
}

/*********************************************************************/
/* AST codegen part
 * by convention you must never pass NULL to the 'ir_value **out'
 * parameter. If you really don't care about the output, pass a dummy.
 * But I can't imagine a pituation where the output is truly unnecessary.
 */

static void _ast_codegen_output_type(ast_expression *self, ir_value *out)
{
    if (out->vtype == TYPE_FIELD)
        out->fieldtype = self->next->vtype;
    if (out->vtype == TYPE_FUNCTION)
        out->outtype = self->next->vtype;
}

#define codegen_output_type(a,o) (_ast_codegen_output_type(&((a)->expression),(o)))

bool ast_value_codegen(ast_value *self, ast_function *func, bool lvalue, ir_value **out)
{
    (void)func;
    (void)lvalue;
    if (self->expression.vtype == TYPE_NIL) {
        *out = func->ir_func->owner->nil;
        return true;
    }
    /* NOTE: This is the codegen for a variable used in an expression.
     * It is not the codegen to generate the value. For this purpose,
     * ast_local_codegen and ast_global_codegen are to be used before this
     * is executed. ast_function_codegen should take care of its locals,
     * and the ast-user should take care of ast_global_codegen to be used
     * on all the globals.
     */
    if (!self->ir_v) {
        char tname[1024]; /* typename is reserved in C++ */
        ast_type_to_string((ast_expression*)self, tname, sizeof(tname));
        compile_error(ast_ctx(self), "ast_value used before generated %s %s", tname, self->name);
        return false;
    }
    *out = self->ir_v;
    return true;
}

static bool ast_global_array_set(ast_value *self)
{
    size_t count = vec_size(self->initlist);
    size_t i;

    if (count > self->expression.count) {
        compile_error(ast_ctx(self), "too many elements in initializer");
        count = self->expression.count;
    }
    else if (count < self->expression.count) {
        /* add this?
        compile_warning(ast_ctx(self), "not all elements are initialized");
        */
    }

    for (i = 0; i != count; ++i) {
        switch (self->expression.next->vtype) {
            case TYPE_FLOAT:
                if (!ir_value_set_float(self->ir_values[i], self->initlist[i].vfloat))
                    return false;
                break;
            case TYPE_VECTOR:
                if (!ir_value_set_vector(self->ir_values[i], self->initlist[i].vvec))
                    return false;
                break;
            case TYPE_STRING:
                if (!ir_value_set_string(self->ir_values[i], self->initlist[i].vstring))
                    return false;
                break;
            case TYPE_ARRAY:
                /* we don't support them in any other place yet either */
                compile_error(ast_ctx(self), "TODO: nested arrays");
                return false;
            case TYPE_FUNCTION:
                /* this requiers a bit more work - similar to the fields I suppose */
                compile_error(ast_ctx(self), "global of type function not properly generated");
                return false;
            case TYPE_FIELD:
                if (!self->initlist[i].vfield) {
                    compile_error(ast_ctx(self), "field constant without vfield set");
                    return false;
                }
                if (!self->initlist[i].vfield->ir_v) {
                    compile_error(ast_ctx(self), "field constant generated before its field");
                    return false;
                }
                if (!ir_value_set_field(self->ir_values[i], self->initlist[i].vfield->ir_v))
                    return false;
                break;
            default:
                compile_error(ast_ctx(self), "TODO: global constant type %i", self->expression.vtype);
                break;
        }
    }
    return true;
}

static bool check_array(ast_value *self, ast_value *array)
{
    if (array->expression.flags & AST_FLAG_ARRAY_INIT && !array->initlist) {
        compile_error(ast_ctx(self), "array without size: %s", self->name);
        return false;
    }
    /* we are lame now - considering the way QC works we won't tolerate arrays > 1024 elements */
    if (!array->expression.count || array->expression.count > OPTS_OPTION_U32(OPTION_MAX_ARRAY_SIZE)) {
        compile_error(ast_ctx(self), "Invalid array of size %lu", (unsigned long)array->expression.count);
        return false;
    }
    return true;
}

bool ast_global_codegen(ast_value *self, ir_builder *ir, bool isfield)
{
    ir_value *v = NULL;

    if (self->expression.vtype == TYPE_NIL) {
        compile_error(ast_ctx(self), "internal error: trying to generate a variable of TYPE_NIL");
        return false;
    }

    if (self->hasvalue && self->expression.vtype == TYPE_FUNCTION)
    {
        ir_function *func = ir_builder_create_function(ir, self->name, self->expression.next->vtype);
        if (!func)
            return false;
        func->context = ast_ctx(self);
        func->value->context = ast_ctx(self);

        self->constval.vfunc->ir_func = func;
        self->ir_v = func->value;
        if (self->expression.flags & AST_FLAG_INCLUDE_DEF)
            self->ir_v->flags |= IR_FLAG_INCLUDE_DEF;
        if (self->expression.flags & AST_FLAG_ERASEABLE)
            self->ir_v->flags |= IR_FLAG_ERASABLE;
        if (self->expression.flags & AST_FLAG_BLOCK_COVERAGE)
            func->flags |= IR_FLAG_BLOCK_COVERAGE;
        /* The function is filled later on ast_function_codegen... */
        return true;
    }

    if (isfield && self->expression.vtype == TYPE_FIELD) {
        ast_expression *fieldtype = self->expression.next;

        if (self->hasvalue) {
            compile_error(ast_ctx(self), "TODO: constant field pointers with value");
            goto error;
        }

        if (fieldtype->vtype == TYPE_ARRAY) {
            size_t ai;
            char   *name;
            size_t  namelen;

            ast_expression *elemtype;
            int             vtype;
            ast_value      *array = (ast_value*)fieldtype;

            if (!ast_istype(fieldtype, ast_value)) {
                compile_error(ast_ctx(self), "internal error: ast_value required");
                return false;
            }

            if (!check_array(self, array))
                return false;

            elemtype = array->expression.next;
            vtype = elemtype->vtype;

            v = ir_builder_create_field(ir, self->name, vtype);
            if (!v) {
                compile_error(ast_ctx(self), "ir_builder_create_global failed on `%s`", self->name);
                return false;
            }
            v->context = ast_ctx(self);
            v->unique_life = true;
            v->locked      = true;
            array->ir_v = self->ir_v = v;

            if (self->expression.flags & AST_FLAG_INCLUDE_DEF)
                self->ir_v->flags |= IR_FLAG_INCLUDE_DEF;
            if (self->expression.flags & AST_FLAG_ERASEABLE)
                self->ir_v->flags |= IR_FLAG_ERASABLE;

            namelen = strlen(self->name);
            name    = (char*)mem_a(namelen + 16);
            util_strncpy(name, self->name, namelen);

            array->ir_values = (ir_value**)mem_a(sizeof(array->ir_values[0]) * array->expression.count);
            array->ir_values[0] = v;
            for (ai = 1; ai < array->expression.count; ++ai) {
                util_snprintf(name + namelen, 16, "[%u]", (unsigned int)ai);
                array->ir_values[ai] = ir_builder_create_field(ir, name, vtype);
                if (!array->ir_values[ai]) {
                    mem_d(name);
                    compile_error(ast_ctx(self), "ir_builder_create_global failed on `%s`", name);
                    return false;
                }
                array->ir_values[ai]->context = ast_ctx(self);
                array->ir_values[ai]->unique_life = true;
                array->ir_values[ai]->locked      = true;
                if (self->expression.flags & AST_FLAG_INCLUDE_DEF)
                    self->ir_values[ai]->flags |= IR_FLAG_INCLUDE_DEF;
            }
            mem_d(name);
        }
        else
        {
            v = ir_builder_create_field(ir, self->name, self->expression.next->vtype);
            if (!v)
                return false;
            v->context = ast_ctx(self);
            self->ir_v = v;
            if (self->expression.flags & AST_FLAG_INCLUDE_DEF)
                self->ir_v->flags |= IR_FLAG_INCLUDE_DEF;

            if (self->expression.flags & AST_FLAG_ERASEABLE)
                self->ir_v->flags |= IR_FLAG_ERASABLE;
        }
        return true;
    }

    if (self->expression.vtype == TYPE_ARRAY) {
        size_t ai;
        char   *name;
        size_t  namelen;

        ast_expression *elemtype = self->expression.next;
        int vtype = elemtype->vtype;

        if (self->expression.flags & AST_FLAG_ARRAY_INIT && !self->expression.count) {
            compile_error(ast_ctx(self), "array `%s' has no size", self->name);
            return false;
        }

        /* same as with field arrays */
        if (!check_array(self, self))
            return false;

        v = ir_builder_create_global(ir, self->name, vtype);
        if (!v) {
            compile_error(ast_ctx(self), "ir_builder_create_global failed `%s`", self->name);
            return false;
        }
        v->context = ast_ctx(self);
        v->unique_life = true;
        v->locked      = true;

        if (self->expression.flags & AST_FLAG_INCLUDE_DEF)
            v->flags |= IR_FLAG_INCLUDE_DEF;
        if (self->expression.flags & AST_FLAG_ERASEABLE)
            self->ir_v->flags |= IR_FLAG_ERASABLE;

        namelen = strlen(self->name);
        name    = (char*)mem_a(namelen + 16);
        util_strncpy(name, self->name, namelen);

        self->ir_values = (ir_value**)mem_a(sizeof(self->ir_values[0]) * self->expression.count);
        self->ir_values[0] = v;
        for (ai = 1; ai < self->expression.count; ++ai) {
            util_snprintf(name + namelen, 16, "[%u]", (unsigned int)ai);
            self->ir_values[ai] = ir_builder_create_global(ir, name, vtype);
            if (!self->ir_values[ai]) {
                mem_d(name);
                compile_error(ast_ctx(self), "ir_builder_create_global failed `%s`", name);
                return false;
            }
            self->ir_values[ai]->context = ast_ctx(self);
            self->ir_values[ai]->unique_life = true;
            self->ir_values[ai]->locked      = true;
            if (self->expression.flags & AST_FLAG_INCLUDE_DEF)
                self->ir_values[ai]->flags |= IR_FLAG_INCLUDE_DEF;
        }
        mem_d(name);
    }
    else
    {
        /* Arrays don't do this since there's no "array" value which spans across the
         * whole thing.
         */
        v = ir_builder_create_global(ir, self->name, self->expression.vtype);
        if (!v) {
            compile_error(ast_ctx(self), "ir_builder_create_global failed on `%s`", self->name);
            return false;
        }
        codegen_output_type(self, v);
        v->context = ast_ctx(self);
    }

    /* link us to the ir_value */
    v->cvq = self->cvq;
    self->ir_v = v;

    if (self->expression.flags & AST_FLAG_INCLUDE_DEF)
        self->ir_v->flags |= IR_FLAG_INCLUDE_DEF;
    if (self->expression.flags & AST_FLAG_ERASEABLE)
        self->ir_v->flags |= IR_FLAG_ERASABLE;

    /* initialize */
    if (self->hasvalue) {
        switch (self->expression.vtype)
        {
            case TYPE_FLOAT:
                if (!ir_value_set_float(v, self->constval.vfloat))
                    goto error;
                break;
            case TYPE_VECTOR:
                if (!ir_value_set_vector(v, self->constval.vvec))
                    goto error;
                break;
            case TYPE_STRING:
                if (!ir_value_set_string(v, self->constval.vstring))
                    goto error;
                break;
            case TYPE_ARRAY:
                ast_global_array_set(self);
                break;
            case TYPE_FUNCTION:
                compile_error(ast_ctx(self), "global of type function not properly generated");
                goto error;
                /* Cannot generate an IR value for a function,
                 * need a pointer pointing to a function rather.
                 */
            case TYPE_FIELD:
                if (!self->constval.vfield) {
                    compile_error(ast_ctx(self), "field constant without vfield set");
                    goto error;
                }
                if (!self->constval.vfield->ir_v) {
                    compile_error(ast_ctx(self), "field constant generated before its field");
                    goto error;
                }
                if (!ir_value_set_field(v, self->constval.vfield->ir_v))
                    goto error;
                break;
            default:
                compile_error(ast_ctx(self), "TODO: global constant type %i", self->expression.vtype);
                break;
        }
    }
    return true;

error: /* clean up */
    if(v) ir_value_delete(v);
    return false;
}

static bool ast_local_codegen(ast_value *self, ir_function *func, bool param)
{
    ir_value *v = NULL;

    if (self->expression.vtype == TYPE_NIL) {
        compile_error(ast_ctx(self), "internal error: trying to generate a variable of TYPE_NIL");
        return false;
    }

    if (self->hasvalue && self->expression.vtype == TYPE_FUNCTION)
    {
        /* Do we allow local functions? I think not...
         * this is NOT a function pointer atm.
         */
        return false;
    }

    if (self->expression.vtype == TYPE_ARRAY) {
        size_t ai;
        char   *name;
        size_t  namelen;

        ast_expression *elemtype = self->expression.next;
        int vtype = elemtype->vtype;

        func->flags |= IR_FLAG_HAS_ARRAYS;

        if (param && !(self->expression.flags & AST_FLAG_IS_VARARG)) {
            compile_error(ast_ctx(self), "array-parameters are not supported");
            return false;
        }

        /* we are lame now - considering the way QC works we won't tolerate arrays > 1024 elements */
        if (!check_array(self, self))
            return false;

        self->ir_values = (ir_value**)mem_a(sizeof(self->ir_values[0]) * self->expression.count);
        if (!self->ir_values) {
            compile_error(ast_ctx(self), "failed to allocate array values");
            return false;
        }

        v = ir_function_create_local(func, self->name, vtype, param);
        if (!v) {
            compile_error(ast_ctx(self), "internal error: ir_function_create_local failed");
            return false;
        }
        v->context = ast_ctx(self);
        v->unique_life = true;
        v->locked      = true;

        namelen = strlen(self->name);
        name    = (char*)mem_a(namelen + 16);
        util_strncpy(name, self->name, namelen);

        self->ir_values[0] = v;
        for (ai = 1; ai < self->expression.count; ++ai) {
            util_snprintf(name + namelen, 16, "[%u]", (unsigned int)ai);
            self->ir_values[ai] = ir_function_create_local(func, name, vtype, param);
            if (!self->ir_values[ai]) {
                compile_error(ast_ctx(self), "internal_error: ir_builder_create_global failed on `%s`", name);
                return false;
            }
            self->ir_values[ai]->context = ast_ctx(self);
            self->ir_values[ai]->unique_life = true;
            self->ir_values[ai]->locked      = true;
        }
        mem_d(name);
    }
    else
    {
        v = ir_function_create_local(func, self->name, self->expression.vtype, param);
        if (!v)
            return false;
        codegen_output_type(self, v);
        v->context = ast_ctx(self);
    }

    /* A constant local... hmmm...
     * I suppose the IR will have to deal with this
     */
    if (self->hasvalue) {
        switch (self->expression.vtype)
        {
            case TYPE_FLOAT:
                if (!ir_value_set_float(v, self->constval.vfloat))
                    goto error;
                break;
            case TYPE_VECTOR:
                if (!ir_value_set_vector(v, self->constval.vvec))
                    goto error;
                break;
            case TYPE_STRING:
                if (!ir_value_set_string(v, self->constval.vstring))
                    goto error;
                break;
            default:
                compile_error(ast_ctx(self), "TODO: global constant type %i", self->expression.vtype);
                break;
        }
    }

    /* link us to the ir_value */
    v->cvq = self->cvq;
    self->ir_v = v;

    if (!ast_generate_accessors(self, func->owner))
        return false;
    return true;

error: /* clean up */
    ir_value_delete(v);
    return false;
}

bool ast_generate_accessors(ast_value *self, ir_builder *ir)
{
    size_t i;
    bool warn = OPTS_WARN(WARN_USED_UNINITIALIZED);
    if (!self->setter || !self->getter)
        return true;
    for (i = 0; i < self->expression.count; ++i) {
        if (!self->ir_values) {
            compile_error(ast_ctx(self), "internal error: no array values generated for `%s`", self->name);
            return false;
        }
        if (!self->ir_values[i]) {
            compile_error(ast_ctx(self), "internal error: not all array values have been generated for `%s`", self->name);
            return false;
        }
        if (self->ir_values[i]->life) {
            compile_error(ast_ctx(self), "internal error: function containing `%s` already generated", self->name);
            return false;
        }
    }

    opts_set(opts.warn, WARN_USED_UNINITIALIZED, false);
    if (self->setter) {
        if (!ast_global_codegen  (self->setter, ir, false) ||
            !ast_function_codegen(self->setter->constval.vfunc, ir) ||
            !ir_function_finalize(self->setter->constval.vfunc->ir_func))
        {
            compile_error(ast_ctx(self), "internal error: failed to generate setter for `%s`", self->name);
            opts_set(opts.warn, WARN_USED_UNINITIALIZED, warn);
            return false;
        }
    }
    if (self->getter) {
        if (!ast_global_codegen  (self->getter, ir, false) ||
            !ast_function_codegen(self->getter->constval.vfunc, ir) ||
            !ir_function_finalize(self->getter->constval.vfunc->ir_func))
        {
            compile_error(ast_ctx(self), "internal error: failed to generate getter for `%s`", self->name);
            opts_set(opts.warn, WARN_USED_UNINITIALIZED, warn);
            return false;
        }
    }
    for (i = 0; i < self->expression.count; ++i) {
        vec_free(self->ir_values[i]->life);
    }
    opts_set(opts.warn, WARN_USED_UNINITIALIZED, warn);
    return true;
}

bool ast_function_codegen(ast_function *self, ir_builder *ir)
{
    ir_function *irf;
    ir_value    *dummy;
    ast_expression         *ec;
    ast_expression_codegen *cgen;

    size_t    i;

    (void)ir;

    irf = self->ir_func;
    if (!irf) {
        compile_error(ast_ctx(self), "internal error: ast_function's related ast_value was not generated yet");
        return false;
    }

    /* fill the parameter list */
    ec = &self->vtype->expression;
    for (i = 0; i < vec_size(ec->params); ++i)
    {
        if (ec->params[i]->expression.vtype == TYPE_FIELD)
            vec_push(irf->params, ec->params[i]->expression.next->vtype);
        else
            vec_push(irf->params, ec->params[i]->expression.vtype);
        if (!self->builtin) {
            if (!ast_local_codegen(ec->params[i], self->ir_func, true))
                return false;
        }
    }

    if (self->varargs) {
        if (!ast_local_codegen(self->varargs, self->ir_func, true))
            return false;
        irf->max_varargs = self->varargs->expression.count;
    }

    if (self->builtin) {
        irf->builtin = self->builtin;
        return true;
    }

    /* have a local return value variable? */
    if (self->return_value) {
        if (!ast_local_codegen(self->return_value, self->ir_func, false))
            return false;
    }

    if (!vec_size(self->blocks)) {
        compile_error(ast_ctx(self), "function `%s` has no body", self->name);
        return false;
    }

    irf->first = self->curblock = ir_function_create_block(ast_ctx(self), irf, "entry");
    if (!self->curblock) {
        compile_error(ast_ctx(self), "failed to allocate entry block for `%s`", self->name);
        return false;
    }

    if (self->argc) {
        ir_value *va_count;
        ir_value *fixed;
        ir_value *sub;
        if (!ast_local_codegen(self->argc, self->ir_func, true))
            return false;
        cgen = self->argc->expression.codegen;
        if (!(*cgen)((ast_expression*)(self->argc), self, false, &va_count))
            return false;
        cgen = self->fixedparams->expression.codegen;
        if (!(*cgen)((ast_expression*)(self->fixedparams), self, false, &fixed))
            return false;
        sub = ir_block_create_binop(self->curblock, ast_ctx(self),
                                    ast_function_label(self, "va_count"), INSTR_SUB_F,
                                    ir_builder_get_va_count(ir), fixed);
        if (!sub)
            return false;
        if (!ir_block_create_store_op(self->curblock, ast_ctx(self), INSTR_STORE_F,
                                      va_count, sub))
        {
            return false;
        }
    }

    for (i = 0; i < vec_size(self->blocks); ++i) {
        cgen = self->blocks[i]->expression.codegen;
        if (!(*cgen)((ast_expression*)self->blocks[i], self, false, &dummy))
            return false;
    }

    /* TODO: check return types */
    if (!self->curblock->final)
    {
        if (!self->vtype->expression.next ||
            self->vtype->expression.next->vtype == TYPE_VOID)
        {
            return ir_block_create_return(self->curblock, ast_ctx(self), NULL);
        }
        else if (vec_size(self->curblock->entries) || self->curblock == irf->first)
        {
            if (self->return_value) {
                cgen = self->return_value->expression.codegen;
                if (!(*cgen)((ast_expression*)(self->return_value), self, false, &dummy))
                    return false;
                return ir_block_create_return(self->curblock, ast_ctx(self), dummy);
            }
            else if (compile_warning(ast_ctx(self), WARN_MISSING_RETURN_VALUES,
                                "control reaches end of non-void function (`%s`) via %s",
                                self->name, self->curblock->label))
            {
                return false;
            }
            return ir_block_create_return(self->curblock, ast_ctx(self), NULL);
        }
    }
    return true;
}

static bool starts_a_label(ast_expression *ex)
{
    while (ex && ast_istype(ex, ast_block)) {
        ast_block *b = (ast_block*)ex;
        ex = b->exprs[0];
    }
    if (!ex)
        return false;
    return ast_istype(ex, ast_label);
}

/* Note, you will not see ast_block_codegen generate ir_blocks.
 * To the AST and the IR, blocks are 2 different things.
 * In the AST it represents a block of code, usually enclosed in
 * curly braces {...}.
 * While in the IR it represents a block in terms of control-flow.
 */
bool ast_block_codegen(ast_block *self, ast_function *func, bool lvalue, ir_value **out)
{
    size_t i;

    /* We don't use this
     * Note: an ast-representation using the comma-operator
     * of the form: (a, b, c) = x should not assign to c...
     */
    if (lvalue) {
        compile_error(ast_ctx(self), "not an l-value (code-block)");
        return false;
    }

    if (self->expression.outr) {
        *out = self->expression.outr;
        return true;
    }

    /* output is NULL at first, we'll have each expression
     * assign to out output, thus, a comma-operator represention
     * using an ast_block will return the last generated value,
     * so: (b, c) + a  executed both b and c, and returns c,
     * which is then added to a.
     */
    *out = NULL;

    /* generate locals */
    for (i = 0; i < vec_size(self->locals); ++i)
    {
        if (!ast_local_codegen(self->locals[i], func->ir_func, false)) {
            if (OPTS_OPTION_BOOL(OPTION_DEBUG))
                compile_error(ast_ctx(self), "failed to generate local `%s`", self->locals[i]->name);
            return false;
        }
    }

    for (i = 0; i < vec_size(self->exprs); ++i)
    {
        ast_expression_codegen *gen;
        if (func->curblock->final && !starts_a_label(self->exprs[i])) {
            if (compile_warning(ast_ctx(self->exprs[i]), WARN_UNREACHABLE_CODE, "unreachable statement"))
                return false;
            continue;
        }
        gen = self->exprs[i]->codegen;
        if (!(*gen)(self->exprs[i], func, false, out))
            return false;
    }

    self->expression.outr = *out;

    return true;
}

bool ast_store_codegen(ast_store *self, ast_function *func, bool lvalue, ir_value **out)
{
    ast_expression_codegen *cgen;
    ir_value *left  = NULL;
    ir_value *right = NULL;

    ast_value       *arr;
    ast_value       *idx = 0;
    ast_array_index *ai = NULL;

    if (lvalue && self->expression.outl) {
        *out = self->expression.outl;
        return true;
    }

    if (!lvalue && self->expression.outr) {
        *out = self->expression.outr;
        return true;
    }

    if (ast_istype(self->dest, ast_array_index))
    {

        ai = (ast_array_index*)self->dest;
        idx = (ast_value*)ai->index;

        if (ast_istype(ai->index, ast_value) && idx->hasvalue && idx->cvq == CV_CONST)
            ai = NULL;
    }

    if (ai) {
        /* we need to call the setter */
        ir_value  *iridx, *funval;
        ir_instr  *call;

        if (lvalue) {
            compile_error(ast_ctx(self), "array-subscript assignment cannot produce lvalues");
            return false;
        }

        arr = (ast_value*)ai->array;
        if (!ast_istype(ai->array, ast_value) || !arr->setter) {
            compile_error(ast_ctx(self), "value has no setter (%s)", arr->name);
            return false;
        }

        cgen = idx->expression.codegen;
        if (!(*cgen)((ast_expression*)(idx), func, false, &iridx))
            return false;

        cgen = arr->setter->expression.codegen;
        if (!(*cgen)((ast_expression*)(arr->setter), func, true, &funval))
            return false;

        cgen = self->source->codegen;
        if (!(*cgen)((ast_expression*)(self->source), func, false, &right))
            return false;

        call = ir_block_create_call(func->curblock, ast_ctx(self), ast_function_label(func, "store"), funval, false);
        if (!call)
            return false;
        ir_call_param(call, iridx);
        ir_call_param(call, right);
        self->expression.outr = right;
    }
    else
    {
        /* regular code */

        cgen = self->dest->codegen;
        /* lvalue! */
        if (!(*cgen)((ast_expression*)(self->dest), func, true, &left))
            return false;
        self->expression.outl = left;

        cgen = self->source->codegen;
        /* rvalue! */
        if (!(*cgen)((ast_expression*)(self->source), func, false, &right))
            return false;

        if (!ir_block_create_store_op(func->curblock, ast_ctx(self), self->op, left, right))
            return false;
        self->expression.outr = right;
    }

    /* Theoretically, an assinment returns its left side as an
     * lvalue, if we don't need an lvalue though, we return
     * the right side as an rvalue, otherwise we have to
     * somehow know whether or not we need to dereference the pointer
     * on the left side - that is: OP_LOAD if it was an address.
     * Also: in original QC we cannot OP_LOADP *anyway*.
     */
    *out = (lvalue ? left : right);

    return true;
}

bool ast_binary_codegen(ast_binary *self, ast_function *func, bool lvalue, ir_value **out)
{
    ast_expression_codegen *cgen;
    ir_value *left, *right;

    /* A binary operation cannot yield an l-value */
    if (lvalue) {
        compile_error(ast_ctx(self), "not an l-value (binop)");
        return false;
    }

    if (self->expression.outr) {
        *out = self->expression.outr;
        return true;
    }

    if ((OPTS_FLAG(SHORT_LOGIC) || OPTS_FLAG(PERL_LOGIC)) &&
        (self->op == INSTR_AND || self->op == INSTR_OR))
    {
        /* NOTE: The short-logic path will ignore right_first */

        /* short circuit evaluation */
        ir_block *other, *merge;
        ir_block *from_left, *from_right;
        ir_instr *phi;
        size_t    merge_id;

        /* prepare end-block */
        merge_id = vec_size(func->ir_func->blocks);
        merge    = ir_function_create_block(ast_ctx(self), func->ir_func, ast_function_label(func, "sce_merge"));

        /* generate the left expression */
        cgen = self->left->codegen;
        if (!(*cgen)((ast_expression*)(self->left), func, false, &left))
            return false;
        /* remember the block */
        from_left = func->curblock;

        /* create a new block for the right expression */
        other = ir_function_create_block(ast_ctx(self), func->ir_func, ast_function_label(func, "sce_other"));
        if (self->op == INSTR_AND) {
            /* on AND: left==true -> other */
            if (!ir_block_create_if(func->curblock, ast_ctx(self), left, other, merge))
                return false;
        } else {
            /* on OR: left==false -> other */
            if (!ir_block_create_if(func->curblock, ast_ctx(self), left, merge, other))
                return false;
        }
        /* use the likely flag */
        vec_last(func->curblock->instr)->likely = true;

        /* enter the right-expression's block */
        func->curblock = other;
        /* generate */
        cgen = self->right->codegen;
        if (!(*cgen)((ast_expression*)(self->right), func, false, &right))
            return false;
        /* remember block */
        from_right = func->curblock;

        /* jump to the merge block */
        if (!ir_block_create_jump(func->curblock, ast_ctx(self), merge))
            return false;

        vec_remove(func->ir_func->blocks, merge_id, 1);
        vec_push(func->ir_func->blocks, merge);

        func->curblock = merge;
        phi = ir_block_create_phi(func->curblock, ast_ctx(self),
                                  ast_function_label(func, "sce_value"),
                                  self->expression.vtype);
        ir_phi_add(phi, from_left, left);
        ir_phi_add(phi, from_right, right);
        *out = ir_phi_value(phi);
        if (!*out)
            return false;

        if (!OPTS_FLAG(PERL_LOGIC)) {
            /* cast-to-bool */
            if (OPTS_FLAG(CORRECT_LOGIC) && (*out)->vtype == TYPE_VECTOR) {
                *out = ir_block_create_unary(func->curblock, ast_ctx(self),
                                             ast_function_label(func, "sce_bool_v"),
                                             INSTR_NOT_V, *out);
                if (!*out)
                    return false;
                *out = ir_block_create_unary(func->curblock, ast_ctx(self),
                                             ast_function_label(func, "sce_bool"),
                                             INSTR_NOT_F, *out);
                if (!*out)
                    return false;
            }
            else if (OPTS_FLAG(FALSE_EMPTY_STRINGS) && (*out)->vtype == TYPE_STRING) {
                *out = ir_block_create_unary(func->curblock, ast_ctx(self),
                                             ast_function_label(func, "sce_bool_s"),
                                             INSTR_NOT_S, *out);
                if (!*out)
                    return false;
                *out = ir_block_create_unary(func->curblock, ast_ctx(self),
                                             ast_function_label(func, "sce_bool"),
                                             INSTR_NOT_F, *out);
                if (!*out)
                    return false;
            }
            else {
                *out = ir_block_create_binop(func->curblock, ast_ctx(self),
                                             ast_function_label(func, "sce_bool"),
                                             INSTR_AND, *out, *out);
                if (!*out)
                    return false;
            }
        }

        self->expression.outr = *out;
        codegen_output_type(self, *out);
        return true;
    }

    if (self->right_first) {
        cgen = self->right->codegen;
        if (!(*cgen)((ast_expression*)(self->right), func, false, &right))
            return false;
        cgen = self->left->codegen;
        if (!(*cgen)((ast_expression*)(self->left), func, false, &left))
            return false;
    } else {
        cgen = self->left->codegen;
        if (!(*cgen)((ast_expression*)(self->left), func, false, &left))
            return false;
        cgen = self->right->codegen;
        if (!(*cgen)((ast_expression*)(self->right), func, false, &right))
            return false;
    }

    *out = ir_block_create_binop(func->curblock, ast_ctx(self), ast_function_label(func, "bin"),
                                 self->op, left, right);
    if (!*out)
        return false;
    self->expression.outr = *out;
    codegen_output_type(self, *out);

    return true;
}

bool ast_binstore_codegen(ast_binstore *self, ast_function *func, bool lvalue, ir_value **out)
{
    ast_expression_codegen *cgen;
    ir_value *leftl = NULL, *leftr, *right, *bin;

    ast_value       *arr;
    ast_value       *idx = 0;
    ast_array_index *ai = NULL;
    ir_value        *iridx = NULL;

    if (lvalue && self->expression.outl) {
        *out = self->expression.outl;
        return true;
    }

    if (!lvalue && self->expression.outr) {
        *out = self->expression.outr;
        return true;
    }

    if (ast_istype(self->dest, ast_array_index))
    {

        ai = (ast_array_index*)self->dest;
        idx = (ast_value*)ai->index;

        if (ast_istype(ai->index, ast_value) && idx->hasvalue && idx->cvq == CV_CONST)
            ai = NULL;
    }

    /* for a binstore we need both an lvalue and an rvalue for the left side */
    /* rvalue of destination! */
    if (ai) {
        cgen = idx->expression.codegen;
        if (!(*cgen)((ast_expression*)(idx), func, false, &iridx))
            return false;
    }
    cgen = self->dest->codegen;
    if (!(*cgen)((ast_expression*)(self->dest), func, false, &leftr))
        return false;

    /* source as rvalue only */
    cgen = self->source->codegen;
    if (!(*cgen)((ast_expression*)(self->source), func, false, &right))
        return false;

    /* now the binary */
    bin = ir_block_create_binop(func->curblock, ast_ctx(self), ast_function_label(func, "binst"),
                                self->opbin, leftr, right);
    self->expression.outr = bin;


    if (ai) {
        /* we need to call the setter */
        ir_value  *funval;
        ir_instr  *call;

        if (lvalue) {
            compile_error(ast_ctx(self), "array-subscript assignment cannot produce lvalues");
            return false;
        }

        arr = (ast_value*)ai->array;
        if (!ast_istype(ai->array, ast_value) || !arr->setter) {
            compile_error(ast_ctx(self), "value has no setter (%s)", arr->name);
            return false;
        }

        cgen = arr->setter->expression.codegen;
        if (!(*cgen)((ast_expression*)(arr->setter), func, true, &funval))
            return false;

        call = ir_block_create_call(func->curblock, ast_ctx(self), ast_function_label(func, "store"), funval, false);
        if (!call)
            return false;
        ir_call_param(call, iridx);
        ir_call_param(call, bin);
        self->expression.outr = bin;
    } else {
        /* now store them */
        cgen = self->dest->codegen;
        /* lvalue of destination */
        if (!(*cgen)((ast_expression*)(self->dest), func, true, &leftl))
            return false;
        self->expression.outl = leftl;

        if (!ir_block_create_store_op(func->curblock, ast_ctx(self), self->opstore, leftl, bin))
            return false;
        self->expression.outr = bin;
    }

    /* Theoretically, an assinment returns its left side as an
     * lvalue, if we don't need an lvalue though, we return
     * the right side as an rvalue, otherwise we have to
     * somehow know whether or not we need to dereference the pointer
     * on the left side - that is: OP_LOAD if it was an address.
     * Also: in original QC we cannot OP_LOADP *anyway*.
     */
    *out = (lvalue ? leftl : bin);

    return true;
}

bool ast_unary_codegen(ast_unary *self, ast_function *func, bool lvalue, ir_value **out)
{
    ast_expression_codegen *cgen;
    ir_value *operand;

    /* An unary operation cannot yield an l-value */
    if (lvalue) {
        compile_error(ast_ctx(self), "not an l-value (binop)");
        return false;
    }

    if (self->expression.outr) {
        *out = self->expression.outr;
        return true;
    }

    cgen = self->operand->codegen;
    /* lvalue! */
    if (!(*cgen)((ast_expression*)(self->operand), func, false, &operand))
        return false;

    *out = ir_block_create_unary(func->curblock, ast_ctx(self), ast_function_label(func, "unary"),
                                 self->op, operand);
    if (!*out)
        return false;
    self->expression.outr = *out;

    return true;
}

bool ast_return_codegen(ast_return *self, ast_function *func, bool lvalue, ir_value **out)
{
    ast_expression_codegen *cgen;
    ir_value *operand;

    *out = NULL;

    /* In the context of a return operation, we don't actually return
     * anything...
     */
    if (lvalue) {
        compile_error(ast_ctx(self), "return-expression is not an l-value");
        return false;
    }

    if (self->expression.outr) {
        compile_error(ast_ctx(self), "internal error: ast_return cannot be reused, it bears no result!");
        return false;
    }
    self->expression.outr = (ir_value*)1;

    if (self->operand) {
        cgen = self->operand->codegen;
        /* lvalue! */
        if (!(*cgen)((ast_expression*)(self->operand), func, false, &operand))
            return false;

        if (!ir_block_create_return(func->curblock, ast_ctx(self), operand))
            return false;
    } else {
        if (!ir_block_create_return(func->curblock, ast_ctx(self), NULL))
            return false;
    }

    return true;
}

bool ast_entfield_codegen(ast_entfield *self, ast_function *func, bool lvalue, ir_value **out)
{
    ast_expression_codegen *cgen;
    ir_value *ent, *field;

    /* This function needs to take the 'lvalue' flag into account!
     * As lvalue we provide a field-pointer, as rvalue we provide the
     * value in a temp.
     */

    if (lvalue && self->expression.outl) {
        *out = self->expression.outl;
        return true;
    }

    if (!lvalue && self->expression.outr) {
        *out = self->expression.outr;
        return true;
    }

    cgen = self->entity->codegen;
    if (!(*cgen)((ast_expression*)(self->entity), func, false, &ent))
        return false;

    cgen = self->field->codegen;
    if (!(*cgen)((ast_expression*)(self->field), func, false, &field))
        return false;

    if (lvalue) {
        /* address! */
        *out = ir_block_create_fieldaddress(func->curblock, ast_ctx(self), ast_function_label(func, "efa"),
                                            ent, field);
    } else {
        *out = ir_block_create_load_from_ent(func->curblock, ast_ctx(self), ast_function_label(func, "efv"),
                                             ent, field, self->expression.vtype);
        /* Done AFTER error checking:
        codegen_output_type(self, *out);
        */
    }
    if (!*out) {
        compile_error(ast_ctx(self), "failed to create %s instruction (output type %s)",
                 (lvalue ? "ADDRESS" : "FIELD"),
                 type_name[self->expression.vtype]);
        return false;
    }
    if (!lvalue)
        codegen_output_type(self, *out);

    if (lvalue)
        self->expression.outl = *out;
    else
        self->expression.outr = *out;

    /* Hm that should be it... */
    return true;
}

bool ast_member_codegen(ast_member *self, ast_function *func, bool lvalue, ir_value **out)
{
    ast_expression_codegen *cgen;
    ir_value *vec;

    /* in QC this is always an lvalue */
    if (lvalue && self->rvalue) {
        compile_error(ast_ctx(self), "not an l-value (member access)");
        return false;
    }
    if (self->expression.outl) {
        *out = self->expression.outl;
        return true;
    }

    cgen = self->owner->codegen;
    if (!(*cgen)((ast_expression*)(self->owner), func, false, &vec))
        return false;

    if (vec->vtype != TYPE_VECTOR &&
        !(vec->vtype == TYPE_FIELD && self->owner->next->vtype == TYPE_VECTOR))
    {
        return false;
    }

    *out = ir_value_vector_member(vec, self->field);
    self->expression.outl = *out;

    return (*out != NULL);
}

bool ast_array_index_codegen(ast_array_index *self, ast_function *func, bool lvalue, ir_value **out)
{
    ast_value *arr;
    ast_value *idx;

    if (!lvalue && self->expression.outr) {
        *out = self->expression.outr;
        return true;
    }
    if (lvalue && self->expression.outl) {
        *out = self->expression.outl;
        return true;
    }

    if (!ast_istype(self->array, ast_value)) {
        compile_error(ast_ctx(self), "array indexing this way is not supported");
        /* note this would actually be pointer indexing because the left side is
         * not an actual array but (hopefully) an indexable expression.
         * Once we get integer arithmetic, and GADDRESS/GSTORE/GLOAD instruction
         * support this path will be filled.
         */
        return false;
    }

    arr = (ast_value*)self->array;
    idx = (ast_value*)self->index;

    if (!ast_istype(self->index, ast_value) || !idx->hasvalue || idx->cvq != CV_CONST) {
        /* Time to use accessor functions */
        ast_expression_codegen *cgen;
        ir_value               *iridx, *funval;
        ir_instr               *call;

        if (lvalue) {
            compile_error(ast_ctx(self), "(.2) array indexing here needs a compile-time constant");
            return false;
        }

        if (!arr->getter) {
            compile_error(ast_ctx(self), "value has no getter, don't know how to index it");
            return false;
        }

        cgen = self->index->codegen;
        if (!(*cgen)((ast_expression*)(self->index), func, false, &iridx))
            return false;

        cgen = arr->getter->expression.codegen;
        if (!(*cgen)((ast_expression*)(arr->getter), func, true, &funval))
            return false;

        call = ir_block_create_call(func->curblock, ast_ctx(self), ast_function_label(func, "fetch"), funval, false);
        if (!call)
            return false;
        ir_call_param(call, iridx);

        *out = ir_call_value(call);
        self->expression.outr = *out;
        (*out)->vtype = self->expression.vtype;
        codegen_output_type(self, *out);
        return true;
    }

    if (idx->expression.vtype == TYPE_FLOAT) {
        unsigned int arridx = idx->constval.vfloat;
        if (arridx >= self->array->count)
        {
            compile_error(ast_ctx(self), "array index out of bounds: %i", arridx);
            return false;
        }
        *out = arr->ir_values[arridx];
    }
    else if (idx->expression.vtype == TYPE_INTEGER) {
        unsigned int arridx = idx->constval.vint;
        if (arridx >= self->array->count)
        {
            compile_error(ast_ctx(self), "array index out of bounds: %i", arridx);
            return false;
        }
        *out = arr->ir_values[arridx];
    }
    else {
        compile_error(ast_ctx(self), "array indexing here needs an integer constant");
        return false;
    }
    (*out)->vtype = self->expression.vtype;
    codegen_output_type(self, *out);
    return true;
}

bool ast_argpipe_codegen(ast_argpipe *self, ast_function *func, bool lvalue, ir_value **out)
{
    *out = NULL;
    if (lvalue) {
        compile_error(ast_ctx(self), "argpipe node: not an lvalue");
        return false;
    }
    (void)func;
    (void)out;
    compile_error(ast_ctx(self), "TODO: argpipe codegen not implemented");
    return false;
}

bool ast_ifthen_codegen(ast_ifthen *self, ast_function *func, bool lvalue, ir_value **out)
{
    ast_expression_codegen *cgen;

    ir_value *condval;
    ir_value *dummy;

    ir_block *cond;
    ir_block *ontrue;
    ir_block *onfalse;
    ir_block *ontrue_endblock = NULL;
    ir_block *onfalse_endblock = NULL;
    ir_block *merge = NULL;
    int       fold  = 0;

    /* We don't output any value, thus also don't care about r/lvalue */
    (void)out;
    (void)lvalue;

    if (self->expression.outr) {
        compile_error(ast_ctx(self), "internal error: ast_ifthen cannot be reused, it bears no result!");
        return false;
    }
    self->expression.outr = (ir_value*)1;

    /* generate the condition */
    cgen = self->cond->codegen;
    if (!(*cgen)((ast_expression*)(self->cond), func, false, &condval))
        return false;
    /* update the block which will get the jump - because short-logic or ternaries may have changed this */
    cond = func->curblock;

    /* try constant folding away the condition */
    if ((fold = fold_cond_ifthen(condval, func, self)) != -1)
        return fold;

    if (self->on_true) {
        /* create on-true block */
        ontrue = ir_function_create_block(ast_ctx(self), func->ir_func, ast_function_label(func, "ontrue"));
        if (!ontrue)
            return false;

        /* enter the block */
        func->curblock = ontrue;

        /* generate */
        cgen = self->on_true->codegen;
        if (!(*cgen)((ast_expression*)(self->on_true), func, false, &dummy))
            return false;

        /* we now need to work from the current endpoint */
        ontrue_endblock = func->curblock;
    } else
        ontrue = NULL;

    /* on-false path */
    if (self->on_false) {
        /* create on-false block */
        onfalse = ir_function_create_block(ast_ctx(self), func->ir_func, ast_function_label(func, "onfalse"));
        if (!onfalse)
            return false;

        /* enter the block */
        func->curblock = onfalse;

        /* generate */
        cgen = self->on_false->codegen;
        if (!(*cgen)((ast_expression*)(self->on_false), func, false, &dummy))
            return false;

        /* we now need to work from the current endpoint */
        onfalse_endblock = func->curblock;
    } else
        onfalse = NULL;

    /* Merge block were they all merge in to */
    if (!ontrue || !onfalse || !ontrue_endblock->final || !onfalse_endblock->final)
    {
        merge = ir_function_create_block(ast_ctx(self), func->ir_func, ast_function_label(func, "endif"));
        if (!merge)
            return false;
        /* add jumps ot the merge block */
        if (ontrue && !ontrue_endblock->final && !ir_block_create_jump(ontrue_endblock, ast_ctx(self), merge))
            return false;
        if (onfalse && !onfalse_endblock->final && !ir_block_create_jump(onfalse_endblock, ast_ctx(self), merge))
            return false;

        /* Now enter the merge block */
        func->curblock = merge;
    }

    /* we create the if here, that way all blocks are ordered :)
     */
    if (!ir_block_create_if(cond, ast_ctx(self), condval,
                            (ontrue  ? ontrue  : merge),
                            (onfalse ? onfalse : merge)))
    {
        return false;
    }

    return true;
}

bool ast_ternary_codegen(ast_ternary *self, ast_function *func, bool lvalue, ir_value **out)
{
    ast_expression_codegen *cgen;

    ir_value *condval;
    ir_value *trueval, *falseval;
    ir_instr *phi;

    ir_block *cond = func->curblock;
    ir_block *cond_out = NULL;
    ir_block *ontrue, *ontrue_out = NULL;
    ir_block *onfalse, *onfalse_out = NULL;
    ir_block *merge;
    int       fold  = 0;

    /* Ternary can never create an lvalue... */
    if (lvalue)
        return false;

    /* In theory it shouldn't be possible to pass through a node twice, but
     * in case we add any kind of optimization pass for the AST itself, it
     * may still happen, thus we remember a created ir_value and simply return one
     * if it already exists.
     */
    if (self->expression.outr) {
        *out = self->expression.outr;
        return true;
    }

    /* In the following, contraty to ast_ifthen, we assume both paths exist. */

    /* generate the condition */
    func->curblock = cond;
    cgen = self->cond->codegen;
    if (!(*cgen)((ast_expression*)(self->cond), func, false, &condval))
        return false;
    cond_out = func->curblock;

    /* try constant folding away the condition */
    if ((fold = fold_cond_ternary(condval, func, self)) != -1)
        return fold;

    /* create on-true block */
    ontrue = ir_function_create_block(ast_ctx(self), func->ir_func, ast_function_label(func, "tern_T"));
    if (!ontrue)
        return false;
    else
    {
        /* enter the block */
        func->curblock = ontrue;

        /* generate */
        cgen = self->on_true->codegen;
        if (!(*cgen)((ast_expression*)(self->on_true), func, false, &trueval))
            return false;

        ontrue_out = func->curblock;
    }

    /* create on-false block */
    onfalse = ir_function_create_block(ast_ctx(self), func->ir_func, ast_function_label(func, "tern_F"));
    if (!onfalse)
        return false;
    else
    {
        /* enter the block */
        func->curblock = onfalse;

        /* generate */
        cgen = self->on_false->codegen;
        if (!(*cgen)((ast_expression*)(self->on_false), func, false, &falseval))
            return false;

        onfalse_out = func->curblock;
    }

    /* create merge block */
    merge = ir_function_create_block(ast_ctx(self), func->ir_func, ast_function_label(func, "tern_out"));
    if (!merge)
        return false;
    /* jump to merge block */
    if (!ir_block_create_jump(ontrue_out, ast_ctx(self), merge))
        return false;
    if (!ir_block_create_jump(onfalse_out, ast_ctx(self), merge))
        return false;

    /* create if instruction */
    if (!ir_block_create_if(cond_out, ast_ctx(self), condval, ontrue, onfalse))
        return false;

    /* Now enter the merge block */
    func->curblock = merge;

    /* Here, now, we need a PHI node
     * but first some sanity checking...
     */
    if (trueval->vtype != falseval->vtype && trueval->vtype != TYPE_NIL && falseval->vtype != TYPE_NIL) {
        /* error("ternary with different types on the two sides"); */
        compile_error(ast_ctx(self), "internal error: ternary operand types invalid");
        return false;
    }

    /* create PHI */
    phi = ir_block_create_phi(merge, ast_ctx(self), ast_function_label(func, "phi"), self->expression.vtype);
    if (!phi) {
        compile_error(ast_ctx(self), "internal error: failed to generate phi node");
        return false;
    }
    ir_phi_add(phi, ontrue_out,  trueval);
    ir_phi_add(phi, onfalse_out, falseval);

    self->expression.outr = ir_phi_value(phi);
    *out = self->expression.outr;

    codegen_output_type(self, *out);

    return true;
}

bool ast_loop_codegen(ast_loop *self, ast_function *func, bool lvalue, ir_value **out)
{
    ast_expression_codegen *cgen;

    ir_value *dummy      = NULL;
    ir_value *precond    = NULL;
    ir_value *postcond   = NULL;

    /* Since we insert some jumps "late" so we have blocks
     * ordered "nicely", we need to keep track of the actual end-blocks
     * of expressions to add the jumps to.
     */
    ir_block *bbody      = NULL, *end_bbody      = NULL;
    ir_block *bprecond   = NULL, *end_bprecond   = NULL;
    ir_block *bpostcond  = NULL, *end_bpostcond  = NULL;
    ir_block *bincrement = NULL, *end_bincrement = NULL;
    ir_block *bout       = NULL, *bin            = NULL;

    /* let's at least move the outgoing block to the end */
    size_t    bout_id;

    /* 'break' and 'continue' need to be able to find the right blocks */
    ir_block *bcontinue     = NULL;
    ir_block *bbreak        = NULL;

    ir_block *tmpblock      = NULL;

    (void)lvalue;
    (void)out;

    if (self->expression.outr) {
        compile_error(ast_ctx(self), "internal error: ast_loop cannot be reused, it bears no result!");
        return false;
    }
    self->expression.outr = (ir_value*)1;

    /* NOTE:
     * Should we ever need some kind of block ordering, better make this function
     * move blocks around than write a block ordering algorithm later... after all
     * the ast and ir should work together, not against each other.
     */

    /* initexpr doesn't get its own block, it's pointless, it could create more blocks
     * anyway if for example it contains a ternary.
     */
    if (self->initexpr)
    {
        cgen = self->initexpr->codegen;
        if (!(*cgen)((ast_expression*)(self->initexpr), func, false, &dummy))
            return false;
    }

    /* Store the block from which we enter this chaos */
    bin = func->curblock;

    /* The pre-loop condition needs its own block since we
     * need to be able to jump to the start of that expression.
     */
    if (self->precond)
    {
        bprecond = ir_function_create_block(ast_ctx(self), func->ir_func, ast_function_label(func, "pre_loop_cond"));
        if (!bprecond)
            return false;

        /* the pre-loop-condition the least important place to 'continue' at */
        bcontinue = bprecond;

        /* enter */
        func->curblock = bprecond;

        /* generate */
        cgen = self->precond->codegen;
        if (!(*cgen)((ast_expression*)(self->precond), func, false, &precond))
            return false;

        end_bprecond = func->curblock;
    } else {
        bprecond = end_bprecond = NULL;
    }

    /* Now the next blocks won't be ordered nicely, but we need to
     * generate them this early for 'break' and 'continue'.
     */
    if (self->increment) {
        bincrement = ir_function_create_block(ast_ctx(self), func->ir_func, ast_function_label(func, "loop_increment"));
        if (!bincrement)
            return false;
        bcontinue = bincrement; /* increment comes before the pre-loop-condition */
    } else {
        bincrement = end_bincrement = NULL;
    }

    if (self->postcond) {
        bpostcond = ir_function_create_block(ast_ctx(self), func->ir_func, ast_function_label(func, "post_loop_cond"));
        if (!bpostcond)
            return false;
        bcontinue = bpostcond; /* postcond comes before the increment */
    } else {
        bpostcond = end_bpostcond = NULL;
    }

    bout_id = vec_size(func->ir_func->blocks);
    bout = ir_function_create_block(ast_ctx(self), func->ir_func, ast_function_label(func, "after_loop"));
    if (!bout)
        return false;
    bbreak = bout;

    /* The loop body... */
    /* if (self->body) */
    {
        bbody = ir_function_create_block(ast_ctx(self), func->ir_func, ast_function_label(func, "loop_body"));
        if (!bbody)
            return false;

        /* enter */
        func->curblock = bbody;

        vec_push(func->breakblocks,    bbreak);
        if (bcontinue)
            vec_push(func->continueblocks, bcontinue);
        else
            vec_push(func->continueblocks, bbody);

        /* generate */
        if (self->body) {
            cgen = self->body->codegen;
            if (!(*cgen)((ast_expression*)(self->body), func, false, &dummy))
                return false;
        }

        end_bbody = func->curblock;
        vec_pop(func->breakblocks);
        vec_pop(func->continueblocks);
    }

    /* post-loop-condition */
    if (self->postcond)
    {
        /* enter */
        func->curblock = bpostcond;

        /* generate */
        cgen = self->postcond->codegen;
        if (!(*cgen)((ast_expression*)(self->postcond), func, false, &postcond))
            return false;

        end_bpostcond = func->curblock;
    }

    /* The incrementor */
    if (self->increment)
    {
        /* enter */
        func->curblock = bincrement;

        /* generate */
        cgen = self->increment->codegen;
        if (!(*cgen)((ast_expression*)(self->increment), func, false, &dummy))
            return false;

        end_bincrement = func->curblock;
    }

    /* In any case now, we continue from the outgoing block */
    func->curblock = bout;

    /* Now all blocks are in place */
    /* From 'bin' we jump to whatever comes first */
    if      (bprecond)   tmpblock = bprecond;
    else                 tmpblock = bbody;    /* can never be null */

    /* DEAD CODE
    else if (bpostcond)  tmpblock = bpostcond;
    else                 tmpblock = bout;
    */

    if (!ir_block_create_jump(bin, ast_ctx(self), tmpblock))
        return false;

    /* From precond */
    if (bprecond)
    {
        ir_block *ontrue, *onfalse;
        ontrue = bbody; /* can never be null */

        /* all of this is dead code
        else if (bincrement) ontrue = bincrement;
        else                 ontrue = bpostcond;
        */

        onfalse = bout;
        if (self->pre_not) {
            tmpblock = ontrue;
            ontrue   = onfalse;
            onfalse  = tmpblock;
        }
        if (!ir_block_create_if(end_bprecond, ast_ctx(self), precond, ontrue, onfalse))
            return false;
    }

    /* from body */
    if (bbody)
    {
        if      (bincrement) tmpblock = bincrement;
        else if (bpostcond)  tmpblock = bpostcond;
        else if (bprecond)   tmpblock = bprecond;
        else                 tmpblock = bbody;
        if (!end_bbody->final && !ir_block_create_jump(end_bbody, ast_ctx(self), tmpblock))
            return false;
    }

    /* from increment */
    if (bincrement)
    {
        if      (bpostcond)  tmpblock = bpostcond;
        else if (bprecond)   tmpblock = bprecond;
        else if (bbody)      tmpblock = bbody;
        else                 tmpblock = bout;
        if (!ir_block_create_jump(end_bincrement, ast_ctx(self), tmpblock))
            return false;
    }

    /* from postcond */
    if (bpostcond)
    {
        ir_block *ontrue, *onfalse;
        if      (bprecond)   ontrue = bprecond;
        else                 ontrue = bbody; /* can never be null */

        /* all of this is dead code
        else if (bincrement) ontrue = bincrement;
        else                 ontrue = bpostcond;
        */

        onfalse = bout;
        if (self->post_not) {
            tmpblock = ontrue;
            ontrue   = onfalse;
            onfalse  = tmpblock;
        }
        if (!ir_block_create_if(end_bpostcond, ast_ctx(self), postcond, ontrue, onfalse))
            return false;
    }

    /* Move 'bout' to the end */
    vec_remove(func->ir_func->blocks, bout_id, 1);
    vec_push(func->ir_func->blocks, bout);

    return true;
}

bool ast_breakcont_codegen(ast_breakcont *self, ast_function *func, bool lvalue, ir_value **out)
{
    ir_block *target;

    *out = NULL;

    if (lvalue) {
        compile_error(ast_ctx(self), "break/continue expression is not an l-value");
        return false;
    }

    if (self->expression.outr) {
        compile_error(ast_ctx(self), "internal error: ast_breakcont cannot be reused!");
        return false;
    }
    self->expression.outr = (ir_value*)1;

    if (self->is_continue)
        target = func->continueblocks[vec_size(func->continueblocks)-1-self->levels];
    else
        target = func->breakblocks[vec_size(func->breakblocks)-1-self->levels];

    if (!target) {
        compile_error(ast_ctx(self), "%s is lacking a target block", (self->is_continue ? "continue" : "break"));
        return false;
    }

    if (!ir_block_create_jump(func->curblock, ast_ctx(self), target))
        return false;
    return true;
}

bool ast_switch_codegen(ast_switch *self, ast_function *func, bool lvalue, ir_value **out)
{
    ast_expression_codegen *cgen;

    ast_switch_case *def_case     = NULL;
    ir_block        *def_bfall    = NULL;
    ir_block        *def_bfall_to = NULL;
    bool set_def_bfall_to = false;

    ir_value *dummy     = NULL;
    ir_value *irop      = NULL;
    ir_block *bout      = NULL;
    ir_block *bfall     = NULL;
    size_t    bout_id;
    size_t    c;

    char      typestr[1024];
    uint16_t  cmpinstr;

    if (lvalue) {
        compile_error(ast_ctx(self), "switch expression is not an l-value");
        return false;
    }

    if (self->expression.outr) {
        compile_error(ast_ctx(self), "internal error: ast_switch cannot be reused!");
        return false;
    }
    self->expression.outr = (ir_value*)1;

    (void)lvalue;
    (void)out;

    cgen = self->operand->codegen;
    if (!(*cgen)((ast_expression*)(self->operand), func, false, &irop))
        return false;

    if (!vec_size(self->cases))
        return true;

    cmpinstr = type_eq_instr[irop->vtype];
    if (cmpinstr >= VINSTR_END) {
        ast_type_to_string(self->operand, typestr, sizeof(typestr));
        compile_error(ast_ctx(self), "invalid type to perform a switch on: %s", typestr);
        return false;
    }

    bout_id = vec_size(func->ir_func->blocks);
    bout = ir_function_create_block(ast_ctx(self), func->ir_func, ast_function_label(func, "after_switch"));
    if (!bout)
        return false;

    /* setup the break block */
    vec_push(func->breakblocks, bout);

    /* Now create all cases */
    for (c = 0; c < vec_size(self->cases); ++c) {
        ir_value *cond, *val;
        ir_block *bcase, *bnot;
        size_t bnot_id;

        ast_switch_case *swcase = &self->cases[c];

        if (swcase->value) {
            /* A regular case */
            /* generate the condition operand */
            cgen = swcase->value->codegen;
            if (!(*cgen)((ast_expression*)(swcase->value), func, false, &val))
                return false;
            /* generate the condition */
            cond = ir_block_create_binop(func->curblock, ast_ctx(self), ast_function_label(func, "switch_eq"), cmpinstr, irop, val);
            if (!cond)
                return false;

            bcase = ir_function_create_block(ast_ctx(self), func->ir_func, ast_function_label(func, "case"));
            bnot_id = vec_size(func->ir_func->blocks);
            bnot = ir_function_create_block(ast_ctx(self), func->ir_func, ast_function_label(func, "not_case"));
            if (!bcase || !bnot)
                return false;
            if (set_def_bfall_to) {
                set_def_bfall_to = false;
                def_bfall_to = bcase;
            }
            if (!ir_block_create_if(func->curblock, ast_ctx(self), cond, bcase, bnot))
                return false;

            /* Make the previous case-end fall through */
            if (bfall && !bfall->final) {
                if (!ir_block_create_jump(bfall, ast_ctx(self), bcase))
                    return false;
            }

            /* enter the case */
            func->curblock = bcase;
            cgen = swcase->code->codegen;
            if (!(*cgen)((ast_expression*)swcase->code, func, false, &dummy))
                return false;

            /* remember this block to fall through from */
            bfall = func->curblock;

            /* enter the else and move it down */
            func->curblock = bnot;
            vec_remove(func->ir_func->blocks, bnot_id, 1);
            vec_push(func->ir_func->blocks, bnot);
        } else {
            /* The default case */
            /* Remember where to fall through from: */
            def_bfall = bfall;
            bfall     = NULL;
            /* remember which case it was */
            def_case  = swcase;
            /* And the next case will be remembered */
            set_def_bfall_to = true;
        }
    }

    /* Jump from the last bnot to bout */
    if (bfall && !bfall->final && !ir_block_create_jump(bfall, ast_ctx(self), bout)) {
        /*
        astwarning(ast_ctx(bfall), WARN_???, "missing break after last case");
        */
        return false;
    }

    /* If there was a default case, put it down here */
    if (def_case) {
        ir_block *bcase;

        /* No need to create an extra block */
        bcase = func->curblock;

        /* Insert the fallthrough jump */
        if (def_bfall && !def_bfall->final) {
            if (!ir_block_create_jump(def_bfall, ast_ctx(self), bcase))
                return false;
        }

        /* Now generate the default code */
        cgen = def_case->code->codegen;
        if (!(*cgen)((ast_expression*)def_case->code, func, false, &dummy))
            return false;

        /* see if we need to fall through */
        if (def_bfall_to && !func->curblock->final)
        {
            if (!ir_block_create_jump(func->curblock, ast_ctx(self), def_bfall_to))
                return false;
        }
    }

    /* Jump from the last bnot to bout */
    if (!func->curblock->final && !ir_block_create_jump(func->curblock, ast_ctx(self), bout))
        return false;
    /* enter the outgoing block */
    func->curblock = bout;

    /* restore the break block */
    vec_pop(func->breakblocks);

    /* Move 'bout' to the end, it's nicer */
    vec_remove(func->ir_func->blocks, bout_id, 1);
    vec_push(func->ir_func->blocks, bout);

    return true;
}

bool ast_label_codegen(ast_label *self, ast_function *func, bool lvalue, ir_value **out)
{
    size_t i;
    ir_value *dummy;

    if (self->undefined) {
        compile_error(ast_ctx(self), "internal error: ast_label never defined");
        return false;
    }

    *out = NULL;
    if (lvalue) {
        compile_error(ast_ctx(self), "internal error: ast_label cannot be an lvalue");
        return false;
    }

    /* simply create a new block and jump to it */
    self->irblock = ir_function_create_block(ast_ctx(self), func->ir_func, self->name);
    if (!self->irblock) {
        compile_error(ast_ctx(self), "failed to allocate label block `%s`", self->name);
        return false;
    }
    if (!func->curblock->final) {
        if (!ir_block_create_jump(func->curblock, ast_ctx(self), self->irblock))
            return false;
    }

    /* enter the new block */
    func->curblock = self->irblock;

    /* Generate all the leftover gotos */
    for (i = 0; i < vec_size(self->gotos); ++i) {
        if (!ast_goto_codegen(self->gotos[i], func, false, &dummy))
            return false;
    }

    return true;
}

bool ast_goto_codegen(ast_goto *self, ast_function *func, bool lvalue, ir_value **out)
{
    *out = NULL;
    if (lvalue) {
        compile_error(ast_ctx(self), "internal error: ast_goto cannot be an lvalue");
        return false;
    }

    if (self->target->irblock) {
        if (self->irblock_from) {
            /* we already tried once, this is the callback */
            self->irblock_from->final = false;
            if (!ir_block_create_goto(self->irblock_from, ast_ctx(self), self->target->irblock)) {
                compile_error(ast_ctx(self), "failed to generate goto to `%s`", self->name);
                return false;
            }
        }
        else
        {
            if (!ir_block_create_goto(func->curblock, ast_ctx(self), self->target->irblock)) {
                compile_error(ast_ctx(self), "failed to generate goto to `%s`", self->name);
                return false;
            }
        }
    }
    else
    {
        /* the target has not yet been created...
         * close this block in a sneaky way:
         */
        func->curblock->final = true;
        self->irblock_from = func->curblock;
        ast_label_register_goto(self->target, self);
    }

    return true;
}

#include <stdio.h>
bool ast_state_codegen(ast_state *self, ast_function *func, bool lvalue, ir_value **out)
{
    ast_expression_codegen *cgen;

    ir_value *frameval, *thinkval;

    if (lvalue) {
        compile_error(ast_ctx(self), "not an l-value (state operation)");
        return false;
    }
    if (self->expression.outr) {
        compile_error(ast_ctx(self), "internal error: ast_state cannot be reused!");
        return false;
    }
    *out = NULL;

    cgen = self->framenum->codegen;
    if (!(*cgen)((ast_expression*)(self->framenum), func, false, &frameval))
        return false;
    if (!frameval)
        return false;

    cgen = self->nextthink->codegen;
    if (!(*cgen)((ast_expression*)(self->nextthink), func, false, &thinkval))
        return false;
    if (!frameval)
        return false;

    if (!ir_block_create_state_op(func->curblock, ast_ctx(self), frameval, thinkval)) {
        compile_error(ast_ctx(self), "failed to create STATE instruction");
        return false;
    }

    self->expression.outr = (ir_value*)1;
    return true;
}

bool ast_call_codegen(ast_call *self, ast_function *func, bool lvalue, ir_value **out)
{
    ast_expression_codegen *cgen;
    ir_value              **params;
    ir_instr               *callinstr;
    size_t i;

    ir_value *funval = NULL;

    /* return values are never lvalues */
    if (lvalue) {
        compile_error(ast_ctx(self), "not an l-value (function call)");
        return false;
    }

    if (self->expression.outr) {
        *out = self->expression.outr;
        return true;
    }

    cgen = self->func->codegen;
    if (!(*cgen)((ast_expression*)(self->func), func, false, &funval))
        return false;
    if (!funval)
        return false;

    params = NULL;

    /* parameters */
    for (i = 0; i < vec_size(self->params); ++i)
    {
        ir_value *param;
        ast_expression *expr = self->params[i];

        cgen = expr->codegen;
        if (!(*cgen)(expr, func, false, &param))
            goto error;
        if (!param)
            goto error;
        vec_push(params, param);
    }

    /* varargs counter */
    if (self->va_count) {
        ir_value   *va_count;
        ir_builder *builder = func->curblock->owner->owner;
        cgen = self->va_count->codegen;
        if (!(*cgen)((ast_expression*)(self->va_count), func, false, &va_count))
            return false;
        if (!ir_block_create_store_op(func->curblock, ast_ctx(self), INSTR_STORE_F,
                                      ir_builder_get_va_count(builder), va_count))
        {
            return false;
        }
    }

    callinstr = ir_block_create_call(func->curblock, ast_ctx(self),
                                     ast_function_label(func, "call"),
                                     funval, !!(self->func->flags & AST_FLAG_NORETURN));
    if (!callinstr)
        goto error;

    for (i = 0; i < vec_size(params); ++i) {
        ir_call_param(callinstr, params[i]);
    }

    *out = ir_call_value(callinstr);
    self->expression.outr = *out;

    codegen_output_type(self, *out);

    vec_free(params);
    return true;
error:
    vec_free(params);
    return false;
}
