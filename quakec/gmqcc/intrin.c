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
#include <string.h>
#include "parser.h"

/*
 * Provides all the "intrinsics" / "builtins" for GMQCC. These can do
 * a few things, they can provide fall back implementations for math
 * functions if the definitions don't exist for some given engine. Or
 * then can determine definitions for existing builtins, and simply
 * wrap back to them instead.  This is like a "portable" intrface that
 * is entered when -fintrin is used (causing all existing builtins to
 * be ignored by the compiler and instead interface through here.
 */
#define intrin_ctx(I) parser_ctx((I)->parser)

static GMQCC_INLINE ast_function *intrin_value(intrin_t *intrin, ast_value **out, const char *name, qcint_t vtype) {
    ast_value    *value = NULL;
    ast_function *func  = NULL;
    char          buffer[1024];
    char          stype [1024];

    util_snprintf(buffer, sizeof(buffer), "__builtin_%s", name);
    util_snprintf(stype,  sizeof(stype),   "<%s>",        type_name[vtype]);

    value                    = ast_value_new(intrin_ctx(intrin), buffer, TYPE_FUNCTION);
    value->intrinsic         = true;
    value->expression.next   = (ast_expression*)ast_value_new(intrin_ctx(intrin), stype, vtype);
    func                     = ast_function_new(intrin_ctx(intrin), buffer, value);
    value->expression.flags |= AST_FLAG_ERASEABLE;

    *out = value;
    return func;
}

static GMQCC_INLINE void intrin_reg(intrin_t *intrin, ast_value *const value, ast_function *const func) {
    vec_push(intrin->parser->functions, func);
    vec_push(intrin->parser->globals,   (ast_expression*)value);
}

#define QC_POW_EPSILON 0.00001f

/*
 * since some intrinsics depend on each other there is the possibility
 * that an intrinsic will fail to get a 'depended' function that a
 * builtin needs, causing some dependency in the chain to have a NULL
 * function. This will cause a segmentation fault at code generation,
 * even though an error was raised. To contiue to allow it (instead
 * of stopping compilation right away). We need to return from the
 * parser, before compilation stops after all the collected errors.
 */
static ast_expression *intrin_func_self(intrin_t *intrin, const char *name, const char *from);
static ast_expression *intrin_nullfunc(intrin_t *intrin) {
    ast_value    *value = NULL;
    ast_function *func  = intrin_value(intrin, &value, NULL, TYPE_VOID);
    intrin_reg(intrin, value, func);
    return (ast_expression*)value;
}

static ast_expression *intrin_isfinite(intrin_t *intrin) {
    /*
     * float isfinite(float x) {
     *     return !(isnan(x) || isinf(x));
     * }
     */
    ast_value    *value     = NULL;
    ast_value    *x         = ast_value_new(intrin_ctx(intrin), "x", TYPE_FLOAT);
    ast_function *func      = intrin_value(intrin, &value, "isfinite", TYPE_FLOAT);
    ast_call     *callisnan = ast_call_new(intrin_ctx(intrin), intrin_func_self(intrin, "isnan", "isfinite"));
    ast_call     *callisinf = ast_call_new(intrin_ctx(intrin), intrin_func_self(intrin, "isinf", "isfinite"));
    ast_block    *block     = ast_block_new(intrin_ctx(intrin));

    /* float x; */
    vec_push(value->expression.params, x);

    /* <callisnan> = isnan(x); */
    vec_push(callisnan->params, (ast_expression*)x);

    /* <callisinf> = isinf(x); */
    vec_push(callisinf->params, (ast_expression*)x);

    /* return (!<callisnan> || <callisinf>); */
    vec_push(block->exprs,
        (ast_expression*)ast_return_new(
            intrin_ctx(intrin),
            (ast_expression*)ast_unary_new(
                intrin_ctx(intrin),
                INSTR_NOT_F,
                (ast_expression*)ast_binary_new(
                    intrin_ctx(intrin),
                    INSTR_OR,
                    (ast_expression*)callisnan,
                    (ast_expression*)callisinf
                )
            )
        )
    );

    vec_push(func->blocks, block);
    intrin_reg(intrin, value, func);

    return (ast_expression*)value;;
}

static ast_expression *intrin_isinf(intrin_t *intrin) {
    /*
     * float isinf(float x) {
     *     return (x != 0.0) && (x + x == x);
     * }
     */
    ast_value    *value = NULL;
    ast_value    *x     = ast_value_new(intrin_ctx(intrin), "x", TYPE_FLOAT);
    ast_block    *body  = ast_block_new(intrin_ctx(intrin));
    ast_function *func  = intrin_value(intrin, &value, "isinf", TYPE_FLOAT);

    vec_push(body->exprs,
        (ast_expression*)ast_return_new(
            intrin_ctx(intrin),
            (ast_expression*)ast_binary_new(
                intrin_ctx(intrin),
                INSTR_AND,
                (ast_expression*)ast_binary_new(
                    intrin_ctx(intrin),
                    INSTR_NE_F,
                    (ast_expression*)x,
                    (ast_expression*)intrin->fold->imm_float[0]
                ),
                (ast_expression*)ast_binary_new(
                    intrin_ctx(intrin),
                    INSTR_EQ_F,
                    (ast_expression*)ast_binary_new(
                        intrin_ctx(intrin),
                        INSTR_ADD_F,
                        (ast_expression*)x,
                        (ast_expression*)x
                    ),
                    (ast_expression*)x
                )
            )
        )
    );

    vec_push(value->expression.params, x);
    vec_push(func->blocks, body);

    intrin_reg(intrin, value, func);

    return (ast_expression*)value;
}

static ast_expression *intrin_isnan(intrin_t *intrin) {
    /*
     * float isnan(float x) {
     *   float local;
     *   local = x;
     *
     *   return (x != local);
     * }
     */
    ast_value    *value  = NULL;
    ast_value    *arg1   = ast_value_new(intrin_ctx(intrin), "x",     TYPE_FLOAT);
    ast_value    *local  = ast_value_new(intrin_ctx(intrin), "local", TYPE_FLOAT);
    ast_block    *body   = ast_block_new(intrin_ctx(intrin));
    ast_function *func   = intrin_value(intrin, &value, "isnan", TYPE_FLOAT);

    vec_push(body->locals, local);
    vec_push(body->exprs,
        (ast_expression*)ast_store_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            (ast_expression*)local,
            (ast_expression*)arg1
        )
    );

    vec_push(body->exprs,
        (ast_expression*)ast_return_new(
            intrin_ctx(intrin),
            (ast_expression*)ast_binary_new(
                intrin_ctx(intrin),
                INSTR_NE_F,
                (ast_expression*)arg1,
                (ast_expression*)local
            )
        )
    );

    vec_push(value->expression.params, arg1);
    vec_push(func->blocks, body);

    intrin_reg(intrin, value, func);

    return (ast_expression*)value;
}

static ast_expression *intrin_isnormal(intrin_t *intrin) {
    /*
     * float isnormal(float x) {
     *     return isfinite(x);
     * }
     */
    ast_value    *value         = NULL;
    ast_call     *callisfinite  = ast_call_new (intrin_ctx(intrin), intrin_func_self(intrin, "isfinite", "isnormal"));
    ast_value    *x             = ast_value_new(intrin_ctx(intrin), "x", TYPE_FLOAT);
    ast_block    *body          = ast_block_new(intrin_ctx(intrin));
    ast_function *func          = intrin_value(intrin, &value, "isnormal", TYPE_FLOAT);

    vec_push(value->expression.params, x);
    vec_push(callisfinite->params, (ast_expression*)x);

    /* return <callisfinite> */
    vec_push(body->exprs,
        (ast_expression*)ast_return_new(
            intrin_ctx(intrin),
            (ast_expression*)callisfinite
        )
    );

    vec_push(func->blocks, body);
    intrin_reg(intrin, value, func);
    return (ast_expression*)value;
}

static ast_expression *intrin_signbit(intrin_t *intrin) {
    /*
     * float signbit(float x) {
     *     return (x < 0);
     * }
     */
    ast_value    *value  = NULL;
    ast_value    *x      = ast_value_new(intrin_ctx(intrin), "x", TYPE_FLOAT);
    ast_block    *body   = ast_block_new(intrin_ctx(intrin));
    ast_function *func   = intrin_value(intrin, &value, "signbit", TYPE_FLOAT);

    vec_push(value->expression.params, x);

    /* return (x < 0); */
    vec_push(body->exprs,
        (ast_expression*)ast_return_new(
            intrin_ctx(intrin),
            (ast_expression*)ast_ternary_new(
                intrin_ctx(intrin),
                (ast_expression*)ast_binary_new(
                    intrin_ctx(intrin),
                    INSTR_LT,
                    (ast_expression*)x,
                    (ast_expression*)intrin->fold->imm_float[0]
                ),
                (ast_expression*)intrin->fold->imm_float[1],
                (ast_expression*)intrin->fold->imm_float[0]
            )
        )
    );

    vec_push(func->blocks, body);
    intrin_reg(intrin, value, func);
    return (ast_expression*)value;
}

static ast_expression *intrin_acosh(intrin_t *intrin) {
    /*
     * float acosh(float x) {
     *     return log(x + sqrt((x * x) - 1));
     * }
     */
    ast_value    *value    = NULL;
    ast_value    *x        = ast_value_new(intrin_ctx(intrin), "x", TYPE_FLOAT);
    ast_call     *calllog  = ast_call_new(intrin_ctx(intrin), intrin_func_self(intrin, "log", "acosh"));
    ast_call     *callsqrt = ast_call_new(intrin_ctx(intrin), intrin_func_self(intrin, "sqrt", "acosh"));
    ast_block    *body     = ast_block_new(intrin_ctx(intrin));
    ast_function *func     = intrin_value(intrin, &value, "acosh", TYPE_FLOAT);

    vec_push(value->expression.params, x);

    /* <callsqrt> = sqrt((x * x) - 1); */
    vec_push(callsqrt->params,
        (ast_expression*)ast_binary_new(
            intrin_ctx(intrin),
            INSTR_SUB_F,
            (ast_expression*)ast_binary_new(
                intrin_ctx(intrin),
                INSTR_MUL_F,
                (ast_expression*)x,
                (ast_expression*)x
            ),
            (ast_expression*)intrin->fold->imm_float[1]
        )
    );

    /* <calllog> = log(x + <callsqrt>); */
    vec_push(calllog->params,
        (ast_expression*)ast_binary_new(
            intrin_ctx(intrin),
            INSTR_ADD_F,
            (ast_expression*)x,
            (ast_expression*)callsqrt
        )
    );

    /* return <calllog>; */
    vec_push(body->exprs,
        (ast_expression*)ast_return_new(
            intrin_ctx(intrin),
            (ast_expression*)calllog
        )
    );

    vec_push(func->blocks, body);
    intrin_reg(intrin, value, func);
    return (ast_expression*)value;
}

static ast_expression *intrin_asinh(intrin_t *intrin) {
    /*
     * float asinh(float x) {
     *     return log(x + sqrt((x * x) + 1));
     * }
     */
    ast_value    *value    = NULL;
    ast_value    *x        = ast_value_new(intrin_ctx(intrin), "x", TYPE_FLOAT);
    ast_call     *calllog  = ast_call_new(intrin_ctx(intrin), intrin_func_self(intrin, "log", "asinh"));
    ast_call     *callsqrt = ast_call_new(intrin_ctx(intrin), intrin_func_self(intrin, "sqrt", "asinh"));
    ast_block    *body     = ast_block_new(intrin_ctx(intrin));
    ast_function *func     = intrin_value(intrin, &value, "asinh", TYPE_FLOAT);

    vec_push(value->expression.params, x);

    /* <callsqrt> = sqrt((x * x) + 1); */
    vec_push(callsqrt->params,
        (ast_expression*)ast_binary_new(
            intrin_ctx(intrin),
            INSTR_ADD_F,
            (ast_expression*)ast_binary_new(
                intrin_ctx(intrin),
                INSTR_MUL_F,
                (ast_expression*)x,
                (ast_expression*)x
            ),
            (ast_expression*)intrin->fold->imm_float[1]
        )
    );

    /* <calllog> = log(x + <callsqrt>); */
    vec_push(calllog->params,
        (ast_expression*)ast_binary_new(
            intrin_ctx(intrin),
            INSTR_ADD_F,
            (ast_expression*)x,
            (ast_expression*)callsqrt
        )
    );

    /* return <calllog>; */
    vec_push(body->exprs,
        (ast_expression*)ast_return_new(
            intrin_ctx(intrin),
            (ast_expression*)calllog
        )
    );

    vec_push(func->blocks, body);
    intrin_reg(intrin, value, func);
    return (ast_expression*)value;
}

static ast_expression *intrin_atanh(intrin_t *intrin) {
    /*
     * float atanh(float x) {
     *     return 0.5 * log((1 + x) / (1 - x))
     * }
     */
    ast_value    *value   = NULL;
    ast_value    *x       = ast_value_new(intrin_ctx(intrin), "x", TYPE_FLOAT);
    ast_call     *calllog = ast_call_new(intrin_ctx(intrin), intrin_func_self(intrin, "log", "atanh"));
    ast_block    *body    = ast_block_new(intrin_ctx(intrin));
    ast_function *func    = intrin_value(intrin, &value, "atanh", TYPE_FLOAT);

    vec_push(value->expression.params, x);

    /* <callog> = log((1 + x) / (1 - x)); */
    vec_push(calllog->params,
        (ast_expression*)ast_binary_new(
            intrin_ctx(intrin),
            INSTR_DIV_F,
            (ast_expression*)ast_binary_new(
                intrin_ctx(intrin),
                INSTR_ADD_F,
                (ast_expression*)intrin->fold->imm_float[1],
                (ast_expression*)x
            ),
            (ast_expression*)ast_binary_new(
                intrin_ctx(intrin),
                INSTR_SUB_F,
                (ast_expression*)intrin->fold->imm_float[1],
                (ast_expression*)x
            )
        )
    );

    /* return 0.5 * <calllog>; */
    vec_push(body->exprs,
        (ast_expression*)ast_binary_new(
            intrin_ctx(intrin),
            INSTR_MUL_F,
            (ast_expression*)fold_constgen_float(intrin->fold, 0.5, false),
            (ast_expression*)calllog
        )
    );

    vec_push(func->blocks, body);
    intrin_reg(intrin, value, func);
    return (ast_expression*)value;
}

static ast_expression *intrin_exp(intrin_t *intrin) {
    /*
     * float exp(float x) {
     *     float sum = 1.0;
     *     float acc = 1.0;
     *     float i;
     *     for (i = 1; i < 200; ++i)
     *         sum += (acc *= x / i);
     *
     *     return sum;
     * }
     */
    ast_value    *value = NULL;
    ast_value    *x     = ast_value_new(intrin_ctx(intrin), "x",   TYPE_FLOAT);
    ast_value    *sum   = ast_value_new(intrin_ctx(intrin), "sum", TYPE_FLOAT);
    ast_value    *acc   = ast_value_new(intrin_ctx(intrin), "acc", TYPE_FLOAT);
    ast_value    *i     = ast_value_new(intrin_ctx(intrin), "i",   TYPE_FLOAT);
    ast_block    *body  = ast_block_new(intrin_ctx(intrin));
    ast_function *func  = intrin_value(intrin, &value, "exp", TYPE_FLOAT);

    vec_push(value->expression.params, x);
    vec_push(body->locals, sum);
    vec_push(body->locals, acc);
    vec_push(body->locals, i);

    /* sum = 1.0; */
    vec_push(body->exprs,
        (ast_expression*)ast_store_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            (ast_expression*)sum,
            (ast_expression*)intrin->fold->imm_float[1]
        )
    );

    /* acc = 1.0; */
    vec_push(body->exprs,
        (ast_expression*)ast_store_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            (ast_expression*)acc,
            (ast_expression*)intrin->fold->imm_float[1]
        )
    );

    /*
     * for (i = 1; i < 200; ++i)
     *     sum += (acc *= x / i);
     */
    vec_push(body->exprs,
        (ast_expression*)ast_loop_new(
            intrin_ctx(intrin),
            /* i = 1; */
            (ast_expression*)ast_store_new(
                intrin_ctx(intrin),
                INSTR_STORE_F,
                (ast_expression*)i,
                (ast_expression*)intrin->fold->imm_float[1]
            ),
            /* i < 200; */
            (ast_expression*)ast_binary_new(
                intrin_ctx(intrin),
                INSTR_LT,
                (ast_expression*)i,
                (ast_expression*)fold_constgen_float(intrin->fold, 200.0f, false)
            ),
            false,
            NULL,
            false,
            /* ++i; */
            (ast_expression*)ast_binstore_new(
                intrin_ctx(intrin),
                INSTR_STORE_F,
                INSTR_ADD_F,
                (ast_expression*)i,
                (ast_expression*)intrin->fold->imm_float[1]
            ),
            /* sum += (acc *= (x / i)) */
            (ast_expression*)ast_binstore_new(
                intrin_ctx(intrin),
                INSTR_STORE_F,
                INSTR_ADD_F,
                (ast_expression*)sum,
                (ast_expression*)ast_binstore_new(
                    intrin_ctx(intrin),
                    INSTR_STORE_F,
                    INSTR_MUL_F,
                    (ast_expression*)acc,
                    (ast_expression*)ast_binary_new(
                        intrin_ctx(intrin),
                        INSTR_DIV_F,
                        (ast_expression*)x,
                        (ast_expression*)i
                    )
                )
            )
        )
    );

    /* return sum; */
    vec_push(body->exprs,
        (ast_expression*)ast_return_new(
            intrin_ctx(intrin),
            (ast_expression*)sum
        )
    );

    vec_push(func->blocks, body);

    intrin_reg(intrin, value, func);
    return (ast_expression*)value;
}

static ast_expression *intrin_exp2(intrin_t *intrin) {
    /*
     * float exp2(float x) {
     *     return pow(2, x);
     * }
     */
    ast_value    *value     = NULL;
    ast_call     *callpow   = ast_call_new (intrin_ctx(intrin), intrin_func_self(intrin, "pow", "exp2"));
    ast_value    *arg1      = ast_value_new(intrin_ctx(intrin), "x", TYPE_FLOAT);
    ast_block    *body      = ast_block_new(intrin_ctx(intrin));
    ast_function *func      = intrin_value(intrin, &value, "exp2", TYPE_FLOAT);

    vec_push(value->expression.params, arg1);

    vec_push(callpow->params, (ast_expression*)intrin->fold->imm_float[3]);
    vec_push(callpow->params, (ast_expression*)arg1);

    /* return <callpow> */
    vec_push(body->exprs,
        (ast_expression*)ast_return_new(
            intrin_ctx(intrin),
            (ast_expression*)callpow
        )
    );

    vec_push(func->blocks, body);

    intrin_reg(intrin, value, func);
    return (ast_expression*)value;
}

static ast_expression *intrin_expm1(intrin_t *intrin) {
    /*
     * float expm1(float x) {
     *     return exp(x) - 1;
     * }
     */
    ast_value    *value    = NULL;
    ast_call     *callexp  = ast_call_new (intrin_ctx(intrin), intrin_func_self(intrin, "exp", "expm1"));
    ast_value    *x        = ast_value_new(intrin_ctx(intrin), "x", TYPE_FLOAT);
    ast_block    *body     = ast_block_new(intrin_ctx(intrin));
    ast_function *func     = intrin_value(intrin, &value, "expm1", TYPE_FLOAT);

    vec_push(value->expression.params, x);

    /* <callexp> = exp(x); */
    vec_push(callexp->params, (ast_expression*)x);

    /* return <callexp> - 1; */
    vec_push(body->exprs,
        (ast_expression*)ast_return_new(
            intrin_ctx(intrin),
            (ast_expression*)ast_binary_new(
                intrin_ctx(intrin),
                INSTR_SUB_F,
                (ast_expression*)callexp,
                (ast_expression*)intrin->fold->imm_float[1]
            )
        )
    );

    vec_push(func->blocks, body);
    intrin_reg(intrin, value, func);
    return (ast_expression*)value;
}

static ast_expression *intrin_pow(intrin_t *intrin) {
    /*
     *
     * float pow(float base, float exp) {
     *     float result;
     *     float low;
     *     float high;
     *     float mid;
     *     float square;
     *     float accumulate;
     *
     *     if (exp == 0.0)
     *         return 1;
     *     if (exp == 1.0)
     *         return base;
     *     if (exp < 0)
     *         return 1.0 / pow(base, -exp);
     *     if (exp >= 1) {
     *         result = pow(base, exp / 2);
     *         return result * result;
     *     }
     *
     *     low        = 0.0f;
     *     high       = 1.0f;
     *     square     = sqrt(base);
     *     accumulate = square;
     *     mid        = high / 2.0f
     *
     *     while (fabs(mid - exp) > QC_POW_EPSILON) {
     *         square = sqrt(square);
     *         if (mid < exp) {
     *             low         = mid;
     *             accumulate *= square;
     *         } else {
     *             high        = mid;
     *             accumulate *= (1.0f / square);
     *         }
     *         mid = (low + high) / 2;
     *     }
     *     return accumulate;
     * }
     */
    ast_value    *value = NULL;
    ast_function *func = intrin_value(intrin, &value, "pow", TYPE_FLOAT);

    /* prepare some calls for later */
    ast_call *callpow1  = ast_call_new(intrin_ctx(intrin), (ast_expression*)value);                  /* for pow(base, -exp)    */
    ast_call *callpow2  = ast_call_new(intrin_ctx(intrin), (ast_expression*)value);                  /* for pow(vase, exp / 2) */
    ast_call *callsqrt1 = ast_call_new(intrin_ctx(intrin), intrin_func_self(intrin, "sqrt", "pow")); /* for sqrt(base)         */
    ast_call *callsqrt2 = ast_call_new(intrin_ctx(intrin), intrin_func_self(intrin, "sqrt", "pow")); /* for sqrt(square)       */
    ast_call *callfabs  = ast_call_new(intrin_ctx(intrin), intrin_func_self(intrin, "fabs", "pow")); /* for fabs(mid - exp)    */

    /* prepare some blocks for later */
    ast_block *expgt1       = ast_block_new(intrin_ctx(intrin));
    ast_block *midltexp     = ast_block_new(intrin_ctx(intrin));
    ast_block *midltexpelse = ast_block_new(intrin_ctx(intrin));
    ast_block *whileblock   = ast_block_new(intrin_ctx(intrin));

    /* float pow(float base, float exp) */
    ast_value    *base = ast_value_new(intrin_ctx(intrin), "base", TYPE_FLOAT);
    ast_value    *exp  = ast_value_new(intrin_ctx(intrin), "exp",  TYPE_FLOAT);
    /* { */
    ast_block    *body = ast_block_new(intrin_ctx(intrin));

    /*
     * float result;
     * float low;
     * float high;
     * float square;
     * float accumulate;
     * float mid;
     */
    ast_value *result     = ast_value_new(intrin_ctx(intrin), "result",     TYPE_FLOAT);
    ast_value *low        = ast_value_new(intrin_ctx(intrin), "low",        TYPE_FLOAT);
    ast_value *high       = ast_value_new(intrin_ctx(intrin), "high",       TYPE_FLOAT);
    ast_value *square     = ast_value_new(intrin_ctx(intrin), "square",     TYPE_FLOAT);
    ast_value *accumulate = ast_value_new(intrin_ctx(intrin), "accumulate", TYPE_FLOAT);
    ast_value *mid        = ast_value_new(intrin_ctx(intrin), "mid",        TYPE_FLOAT);
    vec_push(body->locals, result);
    vec_push(body->locals, low);
    vec_push(body->locals, high);
    vec_push(body->locals, square);
    vec_push(body->locals, accumulate);
    vec_push(body->locals, mid);

    vec_push(value->expression.params, base);
    vec_push(value->expression.params, exp);

    /*
     * if (exp == 0.0)
     *     return 1;
     */
    vec_push(body->exprs,
        (ast_expression*)ast_ifthen_new(
            intrin_ctx(intrin),
            (ast_expression*)ast_binary_new(
                intrin_ctx(intrin),
                INSTR_EQ_F,
                (ast_expression*)exp,
                (ast_expression*)intrin->fold->imm_float[0]
            ),
            (ast_expression*)ast_return_new(
                intrin_ctx(intrin),
                (ast_expression*)intrin->fold->imm_float[1]
            ),
            NULL
        )
    );

    /*
     * if (exp == 1.0)
     *     return base;
     */
    vec_push(body->exprs,
        (ast_expression*)ast_ifthen_new(
            intrin_ctx(intrin),
            (ast_expression*)ast_binary_new(
                intrin_ctx(intrin),
                INSTR_EQ_F,
                (ast_expression*)exp,
                (ast_expression*)intrin->fold->imm_float[1]
            ),
            (ast_expression*)ast_return_new(
                intrin_ctx(intrin),
                (ast_expression*)base
            ),
            NULL
        )
    );

    /* <callpow1> = pow(base, -exp) */
    vec_push(callpow1->params, (ast_expression*)base);
    vec_push(callpow1->params,
        (ast_expression*)ast_unary_new(
            intrin_ctx(intrin),
            VINSTR_NEG_F,
            (ast_expression*)exp
        )
    );

    /*
     * if (exp < 0)
     *     return 1.0 / <callpow1>;
     */
    vec_push(body->exprs,
        (ast_expression*)ast_ifthen_new(
            intrin_ctx(intrin),
            (ast_expression*)ast_binary_new(
                intrin_ctx(intrin),
                INSTR_LT,
                (ast_expression*)exp,
                (ast_expression*)intrin->fold->imm_float[0]
            ),
            (ast_expression*)ast_return_new(
                intrin_ctx(intrin),
                (ast_expression*)ast_binary_new(
                    intrin_ctx(intrin),
                    INSTR_DIV_F,
                    (ast_expression*)intrin->fold->imm_float[1],
                    (ast_expression*)callpow1
                )
            ),
            NULL
        )
    );

    /* <callpow2> = pow(base, exp / 2) */
    vec_push(callpow2->params, (ast_expression*)base);
    vec_push(callpow2->params,
        (ast_expression*)ast_binary_new(
            intrin_ctx(intrin),
            INSTR_DIV_F,
            (ast_expression*)exp,
            (ast_expression*)intrin->fold->imm_float[3] /* 2.0f */
        )
    );

    /*
     * <expgt1> = {
     *     result = <callpow2>;
     *     return result * result;
     * }
     */
    vec_push(expgt1->exprs,
        (ast_expression*)ast_store_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            (ast_expression*)result,
            (ast_expression*)callpow2
        )
    );
    vec_push(expgt1->exprs,
        (ast_expression*)ast_return_new(
            intrin_ctx(intrin),
            (ast_expression*)ast_binary_new(
                intrin_ctx(intrin),
                INSTR_MUL_F,
                (ast_expression*)result,
                (ast_expression*)result
            )
        )
    );

    /*
     * if (exp >= 1) {
     *     <expgt1>
     * }
     */
    vec_push(body->exprs,
        (ast_expression*)ast_ifthen_new(
            intrin_ctx(intrin),
            (ast_expression*)ast_binary_new(
                intrin_ctx(intrin),
                INSTR_GE,
                (ast_expression*)exp,
                (ast_expression*)intrin->fold->imm_float[1]
            ),
            (ast_expression*)expgt1,
            NULL
        )
    );

    /*
     * <callsqrt1> = sqrt(base)
     */
    vec_push(callsqrt1->params, (ast_expression*)base);

    /*
     * low        = 0.0f;
     * high       = 1.0f;
     * square     = sqrt(base);
     * accumulate = square;
     * mid        = high / 2.0f;
     */
    vec_push(body->exprs,
        (ast_expression*)ast_store_new(intrin_ctx(intrin),
            INSTR_STORE_F,
            (ast_expression*)low,
            (ast_expression*)intrin->fold->imm_float[0]
        )
    );
    vec_push(body->exprs,
        (ast_expression*)ast_store_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            (ast_expression*)high,
            (ast_expression*)intrin->fold->imm_float[1]
        )
    );

    vec_push(body->exprs,
        (ast_expression*)ast_store_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            (ast_expression*)square,
            (ast_expression*)callsqrt1
        )
    );

    vec_push(body->exprs,
        (ast_expression*)ast_store_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            (ast_expression*)accumulate,
            (ast_expression*)square
        )
    );
    vec_push(body->exprs,
        (ast_expression*)ast_store_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            (ast_expression*)mid,
            (ast_expression*)ast_binary_new(
                intrin_ctx(intrin),
                INSTR_DIV_F,
                (ast_expression*)high,
                (ast_expression*)intrin->fold->imm_float[3] /* 2.0f */
            )
        )
    );

    /*
     * <midltexp> = {
     *     low         = mid;
     *     accumulate *= square;
     * }
     */
    vec_push(midltexp->exprs,
        (ast_expression*)ast_store_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            (ast_expression*)low,
            (ast_expression*)mid
        )
    );
    vec_push(midltexp->exprs,
        (ast_expression*)ast_binstore_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            INSTR_MUL_F,
            (ast_expression*)accumulate,
            (ast_expression*)square
        )
    );

    /*
     * <midltexpelse> = {
     *     high        = mid;
     *     accumulate *= (1.0 / square);
     * }
     */
    vec_push(midltexpelse->exprs,
        (ast_expression*)ast_store_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            (ast_expression*)high,
            (ast_expression*)mid
        )
    );
    vec_push(midltexpelse->exprs,
        (ast_expression*)ast_binstore_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            INSTR_MUL_F,
            (ast_expression*)accumulate,
            (ast_expression*)ast_binary_new(
                intrin_ctx(intrin),
                INSTR_DIV_F,
                (ast_expression*)intrin->fold->imm_float[1],
                (ast_expression*)square
            )
        )
    );

    /*
     * <callsqrt2> = sqrt(square)
     */
    vec_push(callsqrt2->params, (ast_expression*)square);

    /*
     * <whileblock> = {
     *     square = <callsqrt2>;
     *     if (mid < exp)
     *          <midltexp>;
     *     else
     *          <midltexpelse>;
     *
     *     mid = (low + high) / 2;
     * }
     */
    vec_push(whileblock->exprs,
        (ast_expression*)ast_store_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            (ast_expression*)square,
            (ast_expression*)callsqrt2
        )
    );
    vec_push(whileblock->exprs,
        (ast_expression*)ast_ifthen_new(
            intrin_ctx(intrin),
            (ast_expression*)ast_binary_new(
                intrin_ctx(intrin),
                INSTR_LT,
                (ast_expression*)mid,
                (ast_expression*)exp
            ),
            (ast_expression*)midltexp,
            (ast_expression*)midltexpelse
        )
    );
    vec_push(whileblock->exprs,
        (ast_expression*)ast_store_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            (ast_expression*)mid,
            (ast_expression*)ast_binary_new(
                intrin_ctx(intrin),
                INSTR_DIV_F,
                (ast_expression*)ast_binary_new(
                    intrin_ctx(intrin),
                    INSTR_ADD_F,
                    (ast_expression*)low,
                    (ast_expression*)high
                ),
                (ast_expression*)intrin->fold->imm_float[3] /* 2.0f */
            )
        )
    );

    /*
     * <callabs> = fabs(mid - exp)
     */
    vec_push(callfabs->params,
        (ast_expression*)ast_binary_new(
            intrin_ctx(intrin),
            INSTR_SUB_F,
            (ast_expression*)mid,
            (ast_expression*)exp
        )
    );

    /*
     * while (<callfabs>  > epsilon)
     *     <whileblock>
     */
    vec_push(body->exprs,
        (ast_expression*)ast_loop_new(
            intrin_ctx(intrin),
            /* init */
            NULL,
            /* pre condition */
            (ast_expression*)ast_binary_new(
                intrin_ctx(intrin),
                INSTR_GT,
                (ast_expression*)callfabs,
                (ast_expression*)fold_constgen_float(intrin->fold, QC_POW_EPSILON, false)
            ),
            /* pre not */
            false,
            /* post condition */
            NULL,
            /* post not */
            false,
            /* increment expression */
            NULL,
            /* code block */
            (ast_expression*)whileblock
        )
    );

    /* return accumulate */
    vec_push(body->exprs,
        (ast_expression*)ast_return_new(
            intrin_ctx(intrin),
            (ast_expression*)accumulate
        )
    );

    /* } */
    vec_push(func->blocks, body);

    intrin_reg(intrin, value, func);
    return (ast_expression*)value;
}

static ast_expression *intrin_mod(intrin_t *intrin) {
    /*
     * float mod(float a, float b) {
     *     float div = a / b;
     *     float sign = (div < 0.0f) ? -1 : 1;
     *     return a - b * sign * floor(sign * div);
     * }
     */
    ast_value    *value = NULL;
    ast_call     *call  = ast_call_new (intrin_ctx(intrin), intrin_func_self(intrin, "floor", "mod"));
    ast_value    *a     = ast_value_new(intrin_ctx(intrin), "a",    TYPE_FLOAT);
    ast_value    *b     = ast_value_new(intrin_ctx(intrin), "b",    TYPE_FLOAT);
    ast_value    *div   = ast_value_new(intrin_ctx(intrin), "div",  TYPE_FLOAT);
    ast_value    *sign  = ast_value_new(intrin_ctx(intrin), "sign", TYPE_FLOAT);
    ast_block    *body  = ast_block_new(intrin_ctx(intrin));
    ast_function *func  = intrin_value(intrin, &value, "mod", TYPE_FLOAT);

    vec_push(value->expression.params, a);
    vec_push(value->expression.params, b);

    vec_push(body->locals, div);
    vec_push(body->locals, sign);

    /* div = a / b; */
    vec_push(body->exprs,
        (ast_expression*)ast_store_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            (ast_expression*)div,
            (ast_expression*)ast_binary_new(
                intrin_ctx(intrin),
                INSTR_DIV_F,
                (ast_expression*)a,
                (ast_expression*)b
            )
        )
    );

    /* sign = (div < 0.0f) ? -1 : 1; */
    vec_push(body->exprs,
        (ast_expression*)ast_store_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            (ast_expression*)sign,
            (ast_expression*)ast_ternary_new(
                intrin_ctx(intrin),
                (ast_expression*)ast_binary_new(
                    intrin_ctx(intrin),
                    INSTR_LT,
                    (ast_expression*)div,
                    (ast_expression*)intrin->fold->imm_float[0]
                ),
                (ast_expression*)intrin->fold->imm_float[2],
                (ast_expression*)intrin->fold->imm_float[1]
            )
        )
    );

    /* floor(sign * div) */
    vec_push(call->params,
        (ast_expression*)ast_binary_new(
            intrin_ctx(intrin),
            INSTR_MUL_F,
            (ast_expression*)sign,
            (ast_expression*)div
        )
    );

    /* return a - b * sign * <call> */
    vec_push(body->exprs,
        (ast_expression*)ast_return_new(
            intrin_ctx(intrin),
            (ast_expression*)ast_binary_new(
                intrin_ctx(intrin),
                INSTR_SUB_F,
                (ast_expression*)a,
                (ast_expression*)ast_binary_new(
                    intrin_ctx(intrin),
                    INSTR_MUL_F,
                    (ast_expression*)b,
                    (ast_expression*)ast_binary_new(
                        intrin_ctx(intrin),
                        INSTR_MUL_F,
                        (ast_expression*)sign,
                        (ast_expression*)call
                    )
                )
            )
        )
    );

    vec_push(func->blocks, body);
    intrin_reg(intrin, value, func);

    return (ast_expression*)value;
}

static ast_expression *intrin_fabs(intrin_t *intrin) {
    /*
     * float fabs(float x) {
     *     return x < 0 ? -x : x;
     * }
     */
    ast_value    *value  = NULL;
    ast_value    *arg1   = ast_value_new(intrin_ctx(intrin), "x", TYPE_FLOAT);
    ast_block    *body   = ast_block_new(intrin_ctx(intrin));
    ast_function *func   = intrin_value(intrin, &value, "fabs", TYPE_FLOAT);

    vec_push(body->exprs,
        (ast_expression*)ast_return_new(
            intrin_ctx(intrin),
            (ast_expression*)ast_ternary_new(
                intrin_ctx(intrin),
                (ast_expression*)ast_binary_new(
                    intrin_ctx(intrin),
                    INSTR_LE,
                    (ast_expression*)arg1,
                    (ast_expression*)intrin->fold->imm_float[0]
                ),
                (ast_expression*)ast_unary_new(
                    intrin_ctx(intrin),
                    VINSTR_NEG_F,
                    (ast_expression*)arg1
                ),
                (ast_expression*)arg1
            )
        )
    );

    vec_push(value->expression.params, arg1);
    vec_push(func->blocks, body);

    intrin_reg(intrin, value, func);

    return (ast_expression*)value;
}

static ast_expression *intrin_epsilon(intrin_t *intrin) {
    /*
     * float epsilon(void) {
     *     float eps = 1.0f;
     *     do { eps /= 2.0f; } while ((1.0f + (eps / 2.0f)) != 1.0f);
     *     return eps;
     * }
     */
    ast_value    *value  = NULL;
    ast_value    *eps    = ast_value_new(intrin_ctx(intrin), "eps", TYPE_FLOAT);
    ast_block    *body   = ast_block_new(intrin_ctx(intrin));
    ast_function *func   = intrin_value(intrin, &value, "epsilon", TYPE_FLOAT);

    vec_push(body->locals, eps);

    /* eps = 1.0f; */
    vec_push(body->exprs,
        (ast_expression*)ast_store_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            (ast_expression*)eps,
            (ast_expression*)intrin->fold->imm_float[0]
        )
    );

    vec_push(body->exprs,
        (ast_expression*)ast_loop_new(
            intrin_ctx(intrin),
            NULL,
            NULL,
            false,
            (ast_expression*)ast_binary_new(
                intrin_ctx(intrin),
                INSTR_NE_F,
                (ast_expression*)ast_binary_new(
                    intrin_ctx(intrin),
                    INSTR_ADD_F,
                    (ast_expression*)intrin->fold->imm_float[1],
                    (ast_expression*)ast_binary_new(
                        intrin_ctx(intrin),
                        INSTR_MUL_F,
                        (ast_expression*)eps,
                        (ast_expression*)intrin->fold->imm_float[3] /* 2.0f */
                    )
                ),
                (ast_expression*)intrin->fold->imm_float[1]
            ),
            false,
            NULL,
            (ast_expression*)ast_binstore_new(
                intrin_ctx(intrin),
                INSTR_STORE_F,
                INSTR_DIV_F,
                (ast_expression*)eps,
                (ast_expression*)intrin->fold->imm_float[3] /* 2.0f */
            )
        )
    );

    /* return eps; */
    vec_push(body->exprs,
        (ast_expression*)ast_return_new(
            intrin_ctx(intrin),
            (ast_expression*)eps
        )
    );

    vec_push(func->blocks, body);
    intrin_reg(intrin, value, func);

    return (ast_expression*)value;
}

static ast_expression *intrin_nan(intrin_t *intrin) {
    /*
     * float nan(void) {
     *     float x = 0.0f;
     *     return x / x;
     * }
     */
    ast_value    *value  = NULL;
    ast_value    *x      = ast_value_new(intrin_ctx(intrin), "x", TYPE_FLOAT);
    ast_function *func   = intrin_value(intrin, &value, "nan", TYPE_FLOAT);
    ast_block    *block  = ast_block_new(intrin_ctx(intrin));

    vec_push(block->locals, x);

    vec_push(block->exprs,
        (ast_expression*)ast_store_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            (ast_expression*)x,
            (ast_expression*)intrin->fold->imm_float[0]
        )
    );

    vec_push(block->exprs,
        (ast_expression*)ast_return_new(
            intrin_ctx(intrin),
            (ast_expression*)ast_binary_new(
                intrin_ctx(intrin),
                INSTR_DIV_F,
                (ast_expression*)x,
                (ast_expression*)x
            )
        )
    );

    vec_push(func->blocks, block);
    intrin_reg(intrin, value, func);

    return (ast_expression*)value;
}

static ast_expression *intrin_inf(intrin_t *intrin) {
    /*
     * float inf(void) {
     *     float x = 1.0f;
     *     float y = 0.0f;
     *     return x / y;
     * }
     */
    ast_value    *value  = NULL;
    ast_value    *x      = ast_value_new(intrin_ctx(intrin), "x", TYPE_FLOAT);
    ast_value    *y      = ast_value_new(intrin_ctx(intrin), "y", TYPE_FLOAT);
    ast_function *func   = intrin_value(intrin, &value, "inf", TYPE_FLOAT);
    ast_block    *block  = ast_block_new(intrin_ctx(intrin));
    size_t        i;

    vec_push(block->locals, x);
    vec_push(block->locals, y);

    /* to keep code size down */
    for (i = 0; i <= 1; i++) {
        vec_push(block->exprs,
            (ast_expression*)ast_store_new(
                intrin_ctx(intrin),
                INSTR_STORE_F,
                (ast_expression*)((i == 0) ? x : y),
                (ast_expression*)intrin->fold->imm_float[i]
            )
        );
    }

    vec_push(block->exprs,
        (ast_expression*)ast_return_new(
            intrin_ctx(intrin),
            (ast_expression*)ast_binary_new(
                intrin_ctx(intrin),
                INSTR_DIV_F,
                (ast_expression*)x,
                (ast_expression*)y
            )
        )
    );

    vec_push(func->blocks, block);
    intrin_reg(intrin, value, func);

    return (ast_expression*)value;
}

static ast_expression *intrin_ln(intrin_t *intrin) {
    /*
     * float log(float power, float base) {
     *   float whole;
     *   float nth
     *   float sign = 1.0f;
     *   float eps  = epsilon();
     *
     *   if (power <= 1.0f || bbase <= 1.0) {
     *       if (power <= 0.0f || base <= 0.0f)
     *           return nan();
     *
     *       if (power < 1.0f) {
     *           power = 1.0f / power;
     *           sign *= -1.0f;
     *       }
     *
     *       if (base < 1.0f) {
     *           sign *= -1.0f;
     *           base  = 1.0f / base;
     *       }
     *   }
     *
     *   float A_i       = 1;
     *   float B_i       = 0;
     *   float A_iminus1 = 0;
     *   float B_iminus1 = 1;
     *
     *   for (;;) {
     *       whole = power;
     *       nth   = 0.0f;
     *
     *       while (whole >= base) {
     *           float base2    = base;
     *           float n2       = 1.0f;
     *           float newbase2 = base2 * base2;
     *
     *           while (whole >= newbase2) {
     *               base2     = newbase2;
     *               n2       *= 2;
     *               newbase2 *= newbase2;
     *           }
     *
     *           whole /= base2;
     *           nth += n2;
     *       }
     *
     *       float b_iplus1 = n;
     *       float A_iplus1 = b_iplus1 * A_i + A_iminus1;
     *       float B_iplus1 = b_iplus1 * B_i + B_iminus1;
     *
     *       A_iminus1 = A_i;
     *       B_iminus1 = B_i;
     *       A_i       = A_iplus1;
     *       B_i       = B_iplus1;
     *
     *       if (whole <= 1.0f + eps)
     *           break;
     *
     *       power = base;
     *       bower = whole;
     *   }
     *   return sign * A_i / B_i;
     * }
     */

    ast_value    *value      = NULL;
    ast_value    *power      = ast_value_new(intrin_ctx(intrin), "power",     TYPE_FLOAT);
    ast_value    *base       = ast_value_new(intrin_ctx(intrin), "base",      TYPE_FLOAT);
    ast_value    *whole      = ast_value_new(intrin_ctx(intrin), "whole",     TYPE_FLOAT);
    ast_value    *nth        = ast_value_new(intrin_ctx(intrin), "nth",       TYPE_FLOAT);
    ast_value    *sign       = ast_value_new(intrin_ctx(intrin), "sign",      TYPE_FLOAT);
    ast_value    *A_i        = ast_value_new(intrin_ctx(intrin), "A_i",       TYPE_FLOAT);
    ast_value    *B_i        = ast_value_new(intrin_ctx(intrin), "B_i",       TYPE_FLOAT);
    ast_value    *A_iminus1  = ast_value_new(intrin_ctx(intrin), "A_iminus1", TYPE_FLOAT);
    ast_value    *B_iminus1  = ast_value_new(intrin_ctx(intrin), "B_iminus1", TYPE_FLOAT);
    ast_value    *b_iplus1   = ast_value_new(intrin_ctx(intrin), "b_iplus1",  TYPE_FLOAT);
    ast_value    *A_iplus1   = ast_value_new(intrin_ctx(intrin), "A_iplus1",  TYPE_FLOAT);
    ast_value    *B_iplus1   = ast_value_new(intrin_ctx(intrin), "B_iplus1",  TYPE_FLOAT);
    ast_value    *eps        = ast_value_new(intrin_ctx(intrin), "eps",       TYPE_FLOAT);
    ast_value    *base2      = ast_value_new(intrin_ctx(intrin), "base2",     TYPE_FLOAT);
    ast_value    *n2         = ast_value_new(intrin_ctx(intrin), "n2",        TYPE_FLOAT);
    ast_value    *newbase2   = ast_value_new(intrin_ctx(intrin), "newbase2",  TYPE_FLOAT);
    ast_block    *block      = ast_block_new(intrin_ctx(intrin));
    ast_block    *plt1orblt1 = ast_block_new(intrin_ctx(intrin)); /* (power <= 1.0f || base <= 1.0f) */
    ast_block    *plt1       = ast_block_new(intrin_ctx(intrin)); /* (power < 1.0f) */
    ast_block    *blt1       = ast_block_new(intrin_ctx(intrin)); /* (base  < 1.0f) */
    ast_block    *forloop    = ast_block_new(intrin_ctx(intrin)); /* for(;;) */
    ast_block    *whileloop  = ast_block_new(intrin_ctx(intrin)); /* while (whole >= base) */
    ast_block    *nestwhile  = ast_block_new(intrin_ctx(intrin)); /* while (whole >= newbase2) */
    ast_function *func       = intrin_value(intrin, &value, "ln", TYPE_FLOAT);
    size_t        i;

    vec_push(value->expression.params, power);
    vec_push(value->expression.params, base);

    vec_push(block->locals, whole);
    vec_push(block->locals, nth);
    vec_push(block->locals, sign);
    vec_push(block->locals, eps);
    vec_push(block->locals, A_i);
    vec_push(block->locals, B_i);
    vec_push(block->locals, A_iminus1);
    vec_push(block->locals, B_iminus1);

    /* sign = 1.0f; */
    vec_push(block->exprs,
        (ast_expression*)ast_store_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            (ast_expression*)sign,
            (ast_expression*)intrin->fold->imm_float[1]
        )
    );

    /* eps = __builtin_epsilon(); */
    vec_push(block->exprs,
        (ast_expression*)ast_store_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            (ast_expression*)eps,
            (ast_expression*)ast_call_new(
                intrin_ctx(intrin),
                intrin_func_self(intrin, "__builtin_epsilon", "ln")
            )
        )
    );

    /*
     * A_i       = 1;
     * B_i       = 0;
     * A_iminus1 = 0;
     * B_iminus1 = 1;
     */
    for (i = 0; i <= 1; i++) {
        int j;
        for (j = 1; j >= 0; j--) {
            vec_push(block->exprs,
                (ast_expression*)ast_store_new(
                    intrin_ctx(intrin),
                    INSTR_STORE_F,
                    (ast_expression*)((j) ? ((i) ? B_iminus1 : A_i)
                                          : ((i) ? A_iminus1 : B_i)),
                    (ast_expression*)intrin->fold->imm_float[j]
                )
            );
        }
    }

    /*
     * <plt1> = {
     *     power = 1.0f / power;
     *     sign *= -1.0f;
     * }
     * <blt1> = {
     *     base  = 1.0f / base;
     *     sign *= -1.0f;
     * }
     */
    for (i = 0; i <= 1; i++) {
        vec_push(((i) ? blt1 : plt1)->exprs,
            (ast_expression*)ast_store_new(
                intrin_ctx(intrin),
                INSTR_STORE_F,
                (ast_expression*)((i) ? base : power),
                (ast_expression*)ast_binary_new(
                    intrin_ctx(intrin),
                    INSTR_DIV_F,
                    (ast_expression*)intrin->fold->imm_float[1],
                    (ast_expression*)((i) ? base : power)
                )
            )
        );
        vec_push(plt1->exprs,
            (ast_expression*)ast_binstore_new(
                intrin_ctx(intrin),
                INSTR_STORE_F,
                INSTR_MUL_F,
                (ast_expression*)sign,
                (ast_expression*)intrin->fold->imm_float[2]
            )
        );
    }

    /*
     * <plt1orblt1> = {
     *     if (power <= 0.0 || base <= 0.0f)
     *         return __builtin_nan();
     *     if (power < 1.0f)
     *         <plt1>
     *     if (base < 1.0f)
     *         <blt1>
     * }
     */
    vec_push(plt1orblt1->exprs,
        (ast_expression*)ast_ifthen_new(
            intrin_ctx(intrin),
            (ast_expression*)ast_binary_new(
                intrin_ctx(intrin),
                INSTR_OR,
                (ast_expression*)ast_binary_new(
                    intrin_ctx(intrin),
                    INSTR_LE,
                    (ast_expression*)power,
                    (ast_expression*)intrin->fold->imm_float[0]
                ),
                (ast_expression*)ast_binary_new(
                    intrin_ctx(intrin),
                    INSTR_LE,
                    (ast_expression*)base,
                    (ast_expression*)intrin->fold->imm_float[0]
                )
            ),
            (ast_expression*)ast_return_new(
                intrin_ctx(intrin),
                (ast_expression*)ast_call_new(
                    intrin_ctx(intrin),
                    intrin_func_self(intrin, "__builtin_nan", "ln")
                )
            ),
            NULL
        )
    );

    for (i = 0; i <= 1; i++) {
        vec_push(plt1orblt1->exprs,
            (ast_expression*)ast_ifthen_new(
                intrin_ctx(intrin),
                (ast_expression*)ast_binary_new(
                    intrin_ctx(intrin),
                    INSTR_LT,
                    (ast_expression*)((i) ? base : power),
                    (ast_expression*)intrin->fold->imm_float[1]
                ),
                (ast_expression*)((i) ? blt1 : plt1),
                NULL
            )
        );
    }

    vec_push(block->exprs, (ast_expression*)plt1orblt1);


    /* whole = power; */
    vec_push(forloop->exprs,
        (ast_expression*)ast_store_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            (ast_expression*)whole,
            (ast_expression*)power
        )
    );

    /* nth = 0.0f; */
    vec_push(forloop->exprs,
        (ast_expression*)ast_store_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            (ast_expression*)nth,
            (ast_expression*)intrin->fold->imm_float[0]
        )
    );

    /* base2 = base; */
    vec_push(whileloop->exprs,
        (ast_expression*)ast_store_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            (ast_expression*)base2,
            (ast_expression*)base
        )
    );

    /* n2 = 1.0f; */
    vec_push(whileloop->exprs,
        (ast_expression*)ast_store_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            (ast_expression*)n2,
            (ast_expression*)intrin->fold->imm_float[1]
        )
    );

    /* newbase2 = base2 * base2; */
    vec_push(whileloop->exprs,
        (ast_expression*)ast_store_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            (ast_expression*)newbase2,
            (ast_expression*)ast_binary_new(
                intrin_ctx(intrin),
                INSTR_MUL_F,
                (ast_expression*)base2,
                (ast_expression*)base2
            )
        )
    );

    /* while loop locals */
    vec_push(whileloop->locals, base2);
    vec_push(whileloop->locals, n2);
    vec_push(whileloop->locals, newbase2);

    /* base2 = newbase2; */
    vec_push(nestwhile->exprs,
        (ast_expression*)ast_store_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            (ast_expression*)base2,
            (ast_expression*)newbase2
        )
    );

    /* n2 *= 2; */
    vec_push(nestwhile->exprs,
        (ast_expression*)ast_binstore_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            INSTR_MUL_F,
            (ast_expression*)n2,
            (ast_expression*)intrin->fold->imm_float[3] /* 2.0f */
        )
    );

    /* newbase2 *= newbase2; */
    vec_push(nestwhile->exprs,
        (ast_expression*)ast_binstore_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            INSTR_MUL_F,
            (ast_expression*)newbase2,
            (ast_expression*)newbase2
        )
    );

    /* while (whole >= newbase2) */
    vec_push(whileloop->exprs,
        (ast_expression*)ast_loop_new(
            intrin_ctx(intrin),
            NULL,
            (ast_expression*)ast_binary_new(
                intrin_ctx(intrin),
                INSTR_GE,
                (ast_expression*)whole,
                (ast_expression*)newbase2
            ),
            false,
            NULL,
            false,
            NULL,
            (ast_expression*)nestwhile
        )
    );

    /* whole /= base2; */
    vec_push(whileloop->exprs,
        (ast_expression*)ast_binstore_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            INSTR_DIV_F,
            (ast_expression*)whole,
            (ast_expression*)base2
        )
    );

    /* nth += n2; */
    vec_push(whileloop->exprs,
        (ast_expression*)ast_binstore_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            INSTR_ADD_F,
            (ast_expression*)nth,
            (ast_expression*)n2
        )
    );

    /* while (whole >= base) */
    vec_push(forloop->exprs,
        (ast_expression*)ast_loop_new(
            intrin_ctx(intrin),
            NULL,
            (ast_expression*)ast_binary_new(
                intrin_ctx(intrin),
                INSTR_GE,
                (ast_expression*)whole,
                (ast_expression*)base
            ),
            false,
            NULL,
            false,
            NULL,
            (ast_expression*)whileloop
        )
    );

    vec_push(forloop->locals, b_iplus1);
    vec_push(forloop->locals, A_iplus1);
    vec_push(forloop->locals, B_iplus1);

    /* b_iplus1 = nth; */
    vec_push(forloop->exprs,
        (ast_expression*)ast_store_new(
            intrin_ctx(intrin),
            INSTR_STORE_F,
            (ast_expression*)b_iplus1,
            (ast_expression*)nth
        )
    );

    /*
     * A_iplus1 = b_iplus1 * A_i + A_iminus1;
     * B_iplus1 = b_iplus1 * B_i + B_iminus1;
     */
    for (i = 0; i <= 1; i++) {
        vec_push(forloop->exprs,
            (ast_expression*)ast_store_new(
                intrin_ctx(intrin),
                INSTR_STORE_F,
                (ast_expression*)((i) ? B_iplus1 : A_iplus1),
                (ast_expression*)ast_binary_new(
                    intrin_ctx(intrin),
                    INSTR_ADD_F,
                    (ast_expression*)ast_binary_new(
                        intrin_ctx(intrin),
                        INSTR_MUL_F,
                        (ast_expression*)b_iplus1,
                        (ast_expression*) ((i) ? B_i : A_i)
                    ),
                    (ast_expression*)((i) ? B_iminus1 : A_iminus1)
                )
            )
        );
    }

    /*
     * A_iminus1 = A_i;
     * B_iminus1 = B_i;
     */
    for (i = 0; i <= 1; i++) {
        vec_push(forloop->exprs,
            (ast_expression*)ast_store_new(
                intrin_ctx(intrin),
                INSTR_STORE_F,
                (ast_expression*)((i) ? B_iminus1 : A_iminus1),
                (ast_expression*)((i) ? B_i       : A_i)
            )
        );
    }

    /*
     * A_i = A_iplus1;
     * B_i = B_iplus1;
     */
    for (i = 0; i <= 1; i++) {
        vec_push(forloop->exprs,
            (ast_expression*)ast_store_new(
                intrin_ctx(intrin),
                INSTR_STORE_F,
                (ast_expression*)((i) ? B_i      : A_i),
                (ast_expression*)((i) ? B_iplus1 : A_iplus1)
            )
        );
    }

    /*
     * if (whole <= 1.0f + eps)
     *     break;
     */
    vec_push(forloop->exprs,
        (ast_expression*)ast_ifthen_new(
            intrin_ctx(intrin),
            (ast_expression*)ast_binary_new(
                intrin_ctx(intrin),
                INSTR_LE,
                (ast_expression*)whole,
                (ast_expression*)ast_binary_new(
                    intrin_ctx(intrin),
                    INSTR_ADD_F,
                    (ast_expression*)intrin->fold->imm_float[1],
                    (ast_expression*)eps
                )
            ),
            (ast_expression*)ast_breakcont_new(
                intrin_ctx(intrin),
                false,
                0
            ),
            NULL
        )
    );

    /*
     * power = base;
     * base  = whole;
     */
    for (i = 0; i <= 1; i++) {
        vec_push(forloop->exprs,
            (ast_expression*)ast_store_new(
                intrin_ctx(intrin),
                INSTR_STORE_F,
                (ast_expression*)((i) ? base  : power),
                (ast_expression*)((i) ? whole : base)
            )
        );
    }

    /* add the for loop block */
    vec_push(block->exprs,
        (ast_expression*)ast_loop_new(
            intrin_ctx(intrin),
            NULL,
            /* for(; 1; ) ?? (can this be NULL too?) */
            (ast_expression*)intrin->fold->imm_float[1],
            false,
            NULL,
            false,
            NULL,
            (ast_expression*)forloop
        )
    );

    /* return sign * A_i / B_il */
    vec_push(block->exprs,
        (ast_expression*)ast_return_new(
            intrin_ctx(intrin),
            (ast_expression*)ast_binary_new(
                intrin_ctx(intrin),
                INSTR_MUL_F,
                (ast_expression*)sign,
                (ast_expression*)ast_binary_new(
                    intrin_ctx(intrin),
                    INSTR_DIV_F,
                    (ast_expression*)A_i,
                    (ast_expression*)B_i
                )
            )
        )
    );

    vec_push(func->blocks, block);
    intrin_reg(intrin, value, func);

    return (ast_expression*)value;
}

static ast_expression *intrin_log_variant(intrin_t *intrin, const char *name, float base) {
    ast_value    *value  = NULL;
    ast_call     *callln = ast_call_new (intrin_ctx(intrin), intrin_func_self(intrin, "__builtin_ln", name));
    ast_value    *arg1   = ast_value_new(intrin_ctx(intrin), "x", TYPE_FLOAT);
    ast_block    *body   = ast_block_new(intrin_ctx(intrin));
    ast_function *func   = intrin_value(intrin, &value, name, TYPE_FLOAT);

    vec_push(value->expression.params, arg1);

    vec_push(callln->params, (ast_expression*)arg1);
    vec_push(callln->params, (ast_expression*)fold_constgen_float(intrin->fold, base, false));

    vec_push(body->exprs,
        (ast_expression*)ast_return_new(
            intrin_ctx(intrin),
            (ast_expression*)callln
        )
    );

    vec_push(func->blocks, body);
    intrin_reg(intrin, value, func);
    return (ast_expression*)value;
}

static ast_expression *intrin_log(intrin_t *intrin) {
    return intrin_log_variant(intrin, "log", 2.7182818284590452354);
}
static ast_expression *intrin_log10(intrin_t *intrin) {
    return intrin_log_variant(intrin, "log10", 10);
}
static ast_expression *intrin_log2(intrin_t *intrin) {
    return intrin_log_variant(intrin, "log2", 2);
}
static ast_expression *intrin_logb(intrin_t *intrin) {
    /* FLT_RADIX == 2 for now */
    return intrin_log_variant(intrin, "log2", 2);
}

static ast_expression *intrin_shift_variant(intrin_t *intrin, const char *name, size_t instr) {
    /*
     * float [shift] (float a, float b) {
     *   return floor(a [instr] pow(2, b));
     */
    ast_value    *value     = NULL;
    ast_call     *callpow   = ast_call_new (intrin_ctx(intrin), intrin_func_self(intrin, "pow", name));
    ast_call     *callfloor = ast_call_new (intrin_ctx(intrin), intrin_func_self(intrin, "floor", name));
    ast_value    *a         = ast_value_new(intrin_ctx(intrin), "a", TYPE_FLOAT);
    ast_value    *b         = ast_value_new(intrin_ctx(intrin), "b", TYPE_FLOAT);
    ast_block    *body      = ast_block_new(intrin_ctx(intrin));
    ast_function *func      = intrin_value(intrin, &value, name, TYPE_FLOAT);

    vec_push(value->expression.params, a);
    vec_push(value->expression.params, b);

    /* <callpow> = pow(2, b) */
    vec_push(callpow->params, (ast_expression*)intrin->fold->imm_float[3]);
    vec_push(callpow->params, (ast_expression*)b);

    /* <callfloor> = floor(a [instr] <callpow>) */
    vec_push(
        callfloor->params,
        (ast_expression*)ast_binary_new(
            intrin_ctx(intrin),
            instr,
            (ast_expression*)a,
            (ast_expression*)callpow
        )
    );

    /* return <callfloor> */
    vec_push(body->exprs,
        (ast_expression*)ast_return_new(
            intrin_ctx(intrin),
            (ast_expression*)callfloor
        )
    );

    vec_push(func->blocks, body);
    intrin_reg(intrin, value, func);
    return (ast_expression*)value;
}

static ast_expression *intrin_lshift(intrin_t *intrin) {
    return intrin_shift_variant(intrin, "lshift", INSTR_MUL_F);
}

static ast_expression *intrin_rshift(intrin_t *intrin) {
    return intrin_shift_variant(intrin, "rshift", INSTR_DIV_F);
}

/*
 * TODO: make static (and handle ast_type_string) here for the builtin
 * instead of in SYA parse close.
 */
ast_expression *intrin_debug_typestring(intrin_t *intrin) {
    (void)intrin;
    return (ast_expression*)0x1;
}

static const intrin_func_t intrinsics[] = {
    {&intrin_isfinite,         "__builtin_isfinite",         "isfinite", 1},
    {&intrin_isinf,            "__builtin_isinf",            "isinf",    1},
    {&intrin_isnan,            "__builtin_isnan",            "isnan",    1},
    {&intrin_isnormal,         "__builtin_isnormal",         "isnormal", 1},
    {&intrin_signbit,          "__builtin_signbit",          "signbit",  1},
    {&intrin_acosh,            "__builtin_acosh",            "acosh",    1},
    {&intrin_asinh,            "__builtin_asinh",            "asinh",    1},
    {&intrin_atanh,            "__builtin_atanh",            "atanh",    1},
    {&intrin_exp,              "__builtin_exp",              "exp",      1},
    {&intrin_exp2,             "__builtin_exp2",             "exp2",     1},
    {&intrin_expm1,            "__builtin_expm1",            "expm1",    1},
    {&intrin_mod,              "__builtin_mod",              "mod",      2},
    {&intrin_pow,              "__builtin_pow",              "pow",      2},
    {&intrin_fabs,             "__builtin_fabs",             "fabs",     1},
    {&intrin_log,              "__builtin_log",              "log",      1},
    {&intrin_log10,            "__builtin_log10",            "log10",    1},
    {&intrin_log2,             "__builtin_log2",             "log2",     1},
    {&intrin_logb,             "__builtin_logb",             "logb",     1},
    {&intrin_lshift,           "__builtin_lshift",           "",         2},
    {&intrin_rshift,           "__builtin_rshift",           "",         2},
    {&intrin_epsilon,          "__builtin_epsilon",          "",         0},
    {&intrin_nan,              "__builtin_nan",              "",         0},
    {&intrin_inf,              "__builtin_inf",              "",         0},
    {&intrin_ln,               "__builtin_ln",               "",         2},
    {&intrin_debug_typestring, "__builtin_debug_typestring", "",         0},
    {&intrin_nullfunc,         "#nullfunc",                  "",         0}
};

static void intrin_error(intrin_t *intrin, const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vcompile_error(intrin->parser->lex->tok.ctx, fmt, ap);
    va_end(ap);
}

/* exposed */
intrin_t *intrin_init(parser_t *parser) {
    intrin_t *intrin = (intrin_t*)mem_a(sizeof(intrin_t));
    size_t    i;

    intrin->parser     = parser;
    intrin->fold       = parser->fold;
    intrin->intrinsics = NULL;
    intrin->generated  = NULL;

    vec_append(intrin->intrinsics, GMQCC_ARRAY_COUNT(intrinsics), intrinsics);

    /* populate with null pointers for tracking generation */
    for (i = 0; i < GMQCC_ARRAY_COUNT(intrinsics); i++)
        vec_push(intrin->generated, NULL);

    return intrin;
}

void intrin_cleanup(intrin_t *intrin) {
    vec_free(intrin->intrinsics);
    vec_free(intrin->generated);
    mem_d(intrin);
}

ast_expression *intrin_fold(intrin_t *intrin, ast_value *value, ast_expression **exprs) {
    size_t i;
    if (!value || !value->name)
        return NULL;
    for (i = 0; i < vec_size(intrin->intrinsics); i++)
        if (!strcmp(value->name, intrin->intrinsics[i].name))
            return (vec_size(exprs) != intrin->intrinsics[i].args)
                        ? NULL
                        : fold_intrin(intrin->fold, value->name + 10, exprs);
    return NULL;
}

static GMQCC_INLINE ast_expression *intrin_func_try(intrin_t *intrin, size_t offset, const char *compare) {
    size_t i;
    for (i = 0; i < vec_size(intrin->intrinsics); i++) {
        if (strcmp(*(char **)((char *)&intrin->intrinsics[i] + offset), compare))
            continue;
        if (intrin->generated[i])
            return intrin->generated[i];
        return intrin->generated[i] = intrin->intrinsics[i].intrin(intrin);
    }
    return NULL;
}

static ast_expression *intrin_func_self(intrin_t *intrin, const char *name, const char *from) {
    size_t           i;
    ast_expression  *find;

    /* try current first */
    if ((find = parser_find_global(intrin->parser, name)) && ((ast_value*)find)->expression.vtype == TYPE_FUNCTION)
        for (i = 0; i < vec_size(intrin->parser->functions); ++i)
            if (((ast_value*)find)->name && !strcmp(intrin->parser->functions[i]->name, ((ast_value*)find)->name) && intrin->parser->functions[i]->builtin < 0)
                return find;
    /* try name second */
    if ((find = intrin_func_try(intrin, offsetof(intrin_func_t, name),  name)))
        return find;
    /* try alias third */
    if ((find = intrin_func_try(intrin, offsetof(intrin_func_t, alias), name)))
        return find;

    if (from) {
        intrin_error(intrin, "need function `%s', compiler depends on it for `__builtin_%s'", name, from);
        return intrin_func_self(intrin, "#nullfunc", NULL);
    }
    return NULL;
}

ast_expression *intrin_func(intrin_t *intrin, const char *name) {
    return intrin_func_self(intrin, name, NULL);
}
