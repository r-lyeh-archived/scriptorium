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
#include <math.h>

#include "ast.h"
#include "parser.h"

#define FOLD_STRING_UNTRANSLATE_HTSIZE 1024
#define FOLD_STRING_DOTRANSLATE_HTSIZE 1024

/* The options to use for inexact and arithmetic exceptions */
#define FOLD_ROUNDING SFLOAT_ROUND_NEAREST_EVEN
#define FOLD_TINYNESS SFLOAT_TBEFORE

/*
 * Comparing float values is an unsafe operation when the operands to the
 * comparison are floating point values that are inexact. For instance 1/3 is an
 * inexact value. The FPU is meant to raise exceptions when these sorts of things
 * happen, including division by zero, underflows and overflows. The C standard
 * library provides us with the <fenv.h> header to gain access to the floating-
 * point environment and lets us set the rounding mode and check for these exceptions.
 * The problem is the standard C library allows an implementation to leave these
 * stubbed out and does not require they be implemented. Furthermore, depending
 * on implementations there is no control over the FPU. This is an IEE 754
 * conforming implementation in software to compensate.
 */
typedef uint32_t sfloat_t;

typedef union {
    qcfloat_t f;
    sfloat_t  s;
} sfloat_cast_t;

/* Exception flags */
typedef enum {
    SFLOAT_NOEXCEPT  = 0,
    SFLOAT_INVALID   = 1,
    SFLOAT_DIVBYZERO = 4,
    SFLOAT_OVERFLOW  = 8,
    SFLOAT_UNDERFLOW = 16,
    SFLOAT_INEXACT   = 32
} sfloat_exceptionflags_t;

/* Rounding modes */
typedef enum {
    SFLOAT_ROUND_NEAREST_EVEN,
    SFLOAT_ROUND_DOWN,
    SFLOAT_ROUND_UP,
    SFLOAT_ROUND_TO_ZERO
} sfloat_roundingmode_t;

/* Underflow tininess-detection mode */
typedef enum {
    SFLOAT_TAFTER,
    SFLOAT_TBEFORE
} sfloat_tdetect_t;

typedef struct {
    sfloat_roundingmode_t   roundingmode;
    sfloat_exceptionflags_t exceptionflags;
    sfloat_tdetect_t        tiny;
} sfloat_state_t;

/* Counts the number of leading zero bits before the most-significand one bit. */
#ifdef _MSC_VER
/* MSVC has an intrinsic for this */
    static GMQCC_INLINE uint32_t sfloat_clz(uint32_t x) {
        int r = 0;
        _BitScanForward(&r, x);
        return r;
    }
#   define SFLOAT_CLZ(X, SUB) \
        (sfloat_clz((X)) - (SUB))
#elif defined(__GNUC__) || defined(__CLANG__)
/* Clang and GCC have a builtin for this */
#   define SFLOAT_CLZ(X, SUB) \
        (__builtin_clz((X)) - (SUB))
#else
/* Native fallback */
    static GMQCC_INLINE uint32_t sfloat_popcnt(uint32_t x) {
        x -= ((x >> 1) & 0x55555555);
        x  = (((x >> 2) & 0x33333333) + (x & 0x33333333));
        x  = (((x >> 4) + x) & 0x0F0F0F0F);
        x += x >> 8;
        x += x >> 16;
        return x & 0x0000003F;
    }
    static GMQCC_INLINE uint32_t sfloat_clz(uint32_t x) {
        x |= (x >> 1);
        x |= (x >> 2);
        x |= (x >> 4);
        x |= (x >> 8);
        x |= (x >> 16);
        return 32 - sfloat_popcnt(x);
    }
#   define SFLOAT_CLZ(X, SUB) \
        (sfloat_clz((X) - (SUB)))
#endif

/* The value of a NaN */
#define SFLOAT_NAN 0xFFFFFFFF
/* Test if NaN */
#define SFLOAT_ISNAN(A) \
    (0xFF000000 < (uint32_t)((A) << 1))
/* Test if signaling NaN */
#define SFLOAT_ISSNAN(A) \
    (((((A) >> 22) & 0x1FF) == 0x1FE) && ((A) & 0x003FFFFF))
/* Raise exception */
#define SFLOAT_RAISE(STATE, FLAGS) \
    ((STATE)->exceptionflags = (sfloat_exceptionflags_t)((STATE)->exceptionflags | (FLAGS)))
/*
 * Shifts `A' right by the number of bits given in `COUNT'. If any non-zero bits
 * are shifted off they are forced into the least significand bit of the result
 * by setting it to one. As a result of this, the value of `COUNT' can be
 * arbitrarily large; if `COUNT' is greater than 32, the result will be either
 * zero or one, depending on whether `A' is a zero or non-zero. The result is
 * stored into the value pointed by `Z'.
 */
#define SFLOAT_SHIFT(SIZE, A, COUNT, Z)                                      \
    *(Z) = ((COUNT) == 0)                                                    \
        ? 1                                                                  \
        : (((COUNT) < (SIZE))                                                \
            ? ((A) >> (COUNT)) | (((A) << ((-(COUNT)) & ((SIZE) - 1))) != 0) \
            : ((A) != 0))

/* Extract fractional component */
#define SFLOAT_EXTRACT_FRAC(X) \
    ((uint32_t)((X) & 0x007FFFFF))
/* Extract exponent component */
#define SFLOAT_EXTRACT_EXP(X) \
    ((int16_t)((X) >> 23) & 0xFF)
/* Extract sign bit */
#define SFLOAT_EXTRACT_SIGN(X) \
    ((X) >> 31)
/*
 * Normalizes the subnormal value represented by the denormalized significand
 * `SA'. The normalized exponent and significand are stored at the locations
 * pointed by `Z' and `SZ' respectively.
 */
#define SFLOAT_SUBNORMALIZE(SA, Z, SZ) \
    (void)(*(SZ) = (SA) << SFLOAT_CLZ((SA), 8), *(Z) = 1 - SFLOAT_CLZ((SA), 8))
/*
 * Packs the sign `SIGN', exponent `EXP' and significand `SIG' into the value
 * giving the result.
 *
 * After the shifting into their proper positions, the fields are added together
 * to form the result. This means any integer portion of `SIG' will be added
 * to the exponent. Similarly, because a properly normalized significand will
 * always have an integer portion equal to one, the exponent input `EXP' should
 * be one less than the desired result exponent whenever the significant input
 * `SIG' is a complete, normalized significand.
 */
#define SFLOAT_PACK(SIGN, EXP, SIG) \
    (sfloat_t)((((uint32_t)(SIGN)) << 31) + (((uint32_t)(EXP)) << 23) + (SIG))

/*
 * Takes two values `a' and `b', one of which is a NaN, and returns the appropriate
 * NaN result. If either `a' or `b' is a signaling NaN than an invalid exception is
 * raised.
 */
static sfloat_t sfloat_propagate_nan(sfloat_state_t *state, sfloat_t a, sfloat_t b) {
    bool isnan_a  = SFLOAT_ISNAN(a);
    bool issnan_a = SFLOAT_ISSNAN(a);
    bool isnan_b  = SFLOAT_ISNAN(b);
    bool issnan_b = SFLOAT_ISSNAN(b);

    a |= 0x00400000;
    b |= 0x00400000;

    if (issnan_a | issnan_b)
        SFLOAT_RAISE(state, SFLOAT_INVALID);
    if (isnan_a)
        return (issnan_a & isnan_b) ? b : a;
    return b;
}

/*
 * Takes an abstract value having sign `sign_z', exponent `exp_z', and significand
 * `sig_z' and returns the appropriate value corresponding to the abstract input.
 *
 * The abstract value is simply rounded and packed into the format. If the abstract
 * input cannot be represented exactly an inexact exception is raised. If the
 * abstract input is too large, the overflow and inexact exceptions are both raised
 * and an infinity or maximal finite value is returned. If the abstract value is
 * too small, the value is rounded to a subnormal and the underflow and inexact
 * exceptions are only raised if the value cannot be represented exactly with
 * a subnormal.
 *
 * The input significand `sig_z' has it's binary point between bits 30 and 29,
 * this is seven bits to the left of its usual location. The shifted significand
 * must be normalized or smaller than this. If it's not normalized then the exponent
 * `exp_z' must be zero; in that case, the result returned is a subnormal number
 * which must not require rounding. In the more usual case where the significand
 * is normalized, the exponent must be one less than the *true* exponent.
 *
 * The handling of underflow and overflow is otherwise in alignment with IEC/IEEE.
 */
static sfloat_t SFLOAT_PACK_round(sfloat_state_t *state, bool sign_z, int16_t exp_z, uint32_t sig_z) {
    sfloat_roundingmode_t mode      = state->roundingmode;
    bool                  even      = !!(mode == SFLOAT_ROUND_NEAREST_EVEN);
    unsigned char         increment = 0x40;
    unsigned char         bits      = sig_z & 0x7F;

    if (!even) {
        if (mode == SFLOAT_ROUND_TO_ZERO)
            increment = 0;
        else {
            increment = 0x7F;
            if (sign_z) {
                if (mode == SFLOAT_ROUND_UP)
                    increment = 0;
            } else {
                if (mode == SFLOAT_ROUND_DOWN)
                    increment = 0;
            }
        }
    }

    if (0xFD <= (uint16_t)exp_z) {
        if ((0xFD < exp_z) || ((exp_z == 0xFD) && ((int32_t)(sig_z + increment) < 0))) {
            SFLOAT_RAISE(state, SFLOAT_OVERFLOW | SFLOAT_INEXACT);
            return SFLOAT_PACK(sign_z, 0xFF, 0) - (increment == 0);
        }
        if (exp_z < 0) {
            /* Check for underflow */
            bool tiny = (state->tiny == SFLOAT_TBEFORE) || (exp_z < -1) || (sig_z + increment < 0x80000000);
            SFLOAT_SHIFT(32, sig_z, -exp_z, &sig_z);
            exp_z = 0;
            bits = sig_z & 0x7F;
            if (tiny && bits)
                SFLOAT_RAISE(state, SFLOAT_UNDERFLOW);
        }
    }
    if (bits)
        SFLOAT_RAISE(state, SFLOAT_INEXACT);
    sig_z = (sig_z + increment) >> 7;
    sig_z &= ~(((bits ^ 0x40) == 0) & even);
    if (sig_z == 0)
        exp_z = 0;
    return SFLOAT_PACK(sign_z, exp_z, sig_z);
}

/*
 * Takes an abstract value having sign `sign_z', exponent `exp_z' and significand
 * `sig_z' and returns the appropriate value corresponding to the abstract input.
 * This function is exactly like `PACK_round' except the significand does not have
 * to be normalized.
 *
 * Bit 31 of the significand must be zero and the exponent must be one less than
 * the *true* exponent.
 */
static sfloat_t SFLOAT_PACK_normal(sfloat_state_t *state, bool sign_z, int16_t exp_z, uint32_t sig_z) {
    unsigned char c = SFLOAT_CLZ(sig_z, 1);
    return SFLOAT_PACK_round(state, sign_z, exp_z - c, sig_z << c);
}

/*
 * Returns the result of adding the absolute values of `a' and `b'. The sign
 * `sign_z' is ignored if the result is a NaN.
 */
static sfloat_t sfloat_add_impl(sfloat_state_t *state, sfloat_t a, sfloat_t b, bool sign_z) {
    int16_t  exp_a = SFLOAT_EXTRACT_EXP(a);
    int16_t  exp_b = SFLOAT_EXTRACT_EXP(b);
    int16_t  exp_z = 0;
    int16_t  exp_d = exp_a - exp_b;
    uint32_t sig_a = SFLOAT_EXTRACT_FRAC(a) << 6;
    uint32_t sig_b = SFLOAT_EXTRACT_FRAC(b) << 6;
    uint32_t sig_z = 0;

    if (0 < exp_d) {
        if (exp_a == 0xFF)
            return sig_a ? sfloat_propagate_nan(state, a, b) : a;
        if (exp_b == 0)
            --exp_d;
        else
            sig_b |= 0x20000000;
        SFLOAT_SHIFT(32, sig_b, exp_d, &sig_b);
        exp_z = exp_a;
    } else if (exp_d < 0) {
        if (exp_b == 0xFF)
            return sig_b ? sfloat_propagate_nan(state, a, b) : SFLOAT_PACK(sign_z, 0xFF, 0);
        if (exp_a == 0)
            ++exp_d;
        else
            sig_a |= 0x20000000;
        SFLOAT_SHIFT(32, sig_a, -exp_d, &sig_a);
        exp_z = exp_b;
    } else {
        if (exp_a == 0xFF)
            return (sig_a | sig_b) ? sfloat_propagate_nan(state, a, b) : a;
        if (exp_a == 0)
            return SFLOAT_PACK(sign_z, 0, (sig_a + sig_b) >> 6);
        sig_z = 0x40000000 + sig_a + sig_b;
        exp_z = exp_a;
        goto end;
    }
    sig_a |= 0x20000000;
    sig_z = (sig_a + sig_b) << 1;
    --exp_z;
    if ((int32_t)sig_z < 0) {
        sig_z = sig_a + sig_b;
        ++exp_z;
    }
end:
    return SFLOAT_PACK_round(state, sign_z, exp_z, sig_z);
}

/*
 * Returns the result of subtracting the absolute values of `a' and `b'. If the
 * sign `sign_z' is one, the difference is negated before being returned. The
 * sign is ignored if the result is a NaN.
 */
static sfloat_t sfloat_sub_impl(sfloat_state_t *state, sfloat_t a, sfloat_t b, bool sign_z) {
    int16_t  exp_a = SFLOAT_EXTRACT_EXP(a);
    int16_t  exp_b = SFLOAT_EXTRACT_EXP(b);
    int16_t  exp_z = 0;
    int16_t  exp_d = exp_a - exp_b;
    uint32_t sig_a = SFLOAT_EXTRACT_FRAC(a) << 7;
    uint32_t sig_b = SFLOAT_EXTRACT_FRAC(b) << 7;
    uint32_t sig_z = 0;

    if (0 < exp_d) goto exp_greater_a;
    if (exp_d < 0) goto exp_greater_b;

    if (exp_a == 0xFF) {
        if (sig_a | sig_b)
            return sfloat_propagate_nan(state, a, b);
        SFLOAT_RAISE(state, SFLOAT_INVALID);
        return SFLOAT_NAN;
    }

    if (exp_a == 0)
        exp_a = exp_b = 1;

    if (sig_b < sig_a) goto greater_a;
    if (sig_a < sig_b) goto greater_b;

    return SFLOAT_PACK(state->roundingmode == SFLOAT_ROUND_DOWN, 0, 0);

exp_greater_b:
    if (exp_b == 0xFF)
        return (sig_b) ? sfloat_propagate_nan(state, a, b) : SFLOAT_PACK(sign_z ^ 1, 0xFF, 0);
    if (exp_a == 0)
        ++exp_d;
    else
        sig_a |= 0x40000000;
    SFLOAT_SHIFT(32, sig_a, -exp_d, &sig_a);
    sig_b |= 0x40000000;
greater_b:
    sig_z = sig_b - sig_a;
    exp_z = exp_b;
    sign_z ^= 1;
    goto end;

exp_greater_a:
    if (exp_a == 0xFF)
        return (sig_a) ? sfloat_propagate_nan(state, a, b) : a;
    if (exp_b == 0)
        --exp_d;
    else
        sig_b |= 0x40000000;
    SFLOAT_SHIFT(32, sig_b, exp_d, &sig_b);
    sig_a |= 0x40000000;
greater_a:
    sig_z = sig_a - sig_b;
    exp_z = exp_a;

end:
    --exp_z;
    return SFLOAT_PACK_normal(state, sign_z, exp_z, sig_z);
}

static GMQCC_INLINE sfloat_t sfloat_add(sfloat_state_t *state, sfloat_t a, sfloat_t b) {
    bool sign_a = SFLOAT_EXTRACT_SIGN(a);
    bool sign_b = SFLOAT_EXTRACT_SIGN(b);
    return (sign_a == sign_b) ? sfloat_add_impl(state, a, b, sign_a)
                              : sfloat_sub_impl(state, a, b, sign_a);
}

static GMQCC_INLINE sfloat_t sfloat_sub(sfloat_state_t *state, sfloat_t a, sfloat_t b) {
    bool sign_a = SFLOAT_EXTRACT_SIGN(a);
    bool sign_b = SFLOAT_EXTRACT_SIGN(b);
    return (sign_a == sign_b) ? sfloat_sub_impl(state, a, b, sign_a)
                              : sfloat_add_impl(state, a, b, sign_a);
}

static sfloat_t sfloat_mul(sfloat_state_t *state, sfloat_t a, sfloat_t b) {
    int16_t  exp_a   = SFLOAT_EXTRACT_EXP(a);
    int16_t  exp_b   = SFLOAT_EXTRACT_EXP(b);
    int16_t  exp_z   = 0;
    uint32_t sig_a   = SFLOAT_EXTRACT_FRAC(a);
    uint32_t sig_b   = SFLOAT_EXTRACT_FRAC(b);
    uint32_t sig_z   = 0;
    uint64_t sig_z64 = 0;
    bool     sign_a  = SFLOAT_EXTRACT_SIGN(a);
    bool     sign_b  = SFLOAT_EXTRACT_SIGN(b);
    bool     sign_z  = sign_a ^ sign_b;

    if (exp_a == 0xFF) {
        if (sig_a || ((exp_b == 0xFF) && sig_b))
            return sfloat_propagate_nan(state, a, b);
        if ((exp_b | sig_b) == 0) {
            SFLOAT_RAISE(state, SFLOAT_INVALID);
            return SFLOAT_NAN;
        }
        return SFLOAT_PACK(sign_z, 0xFF, 0);
    }
    if (exp_b == 0xFF) {
        if (sig_b)
            return sfloat_propagate_nan(state, a, b);
        if ((exp_a | sig_a) == 0) {
            SFLOAT_RAISE(state, SFLOAT_INVALID);
            return SFLOAT_NAN;
        }
        return SFLOAT_PACK(sign_z, 0xFF, 0);
    }
    if (exp_a == 0) {
        if (sig_a == 0)
            return SFLOAT_PACK(sign_z, 0, 0);
        SFLOAT_SUBNORMALIZE(sig_a, &exp_a, &sig_a);
    }
    if (exp_b == 0) {
        if (sig_b == 0)
            return SFLOAT_PACK(sign_z, 0, 0);
        SFLOAT_SUBNORMALIZE(sig_b, &exp_b, &sig_b);
    }
    exp_z = exp_a + exp_b - 0x7F;
    sig_a = (sig_a | 0x00800000) << 7;
    sig_b = (sig_b | 0x00800000) << 8;
    SFLOAT_SHIFT(64, ((uint64_t)sig_a) * sig_b, 32, &sig_z64);
    sig_z = sig_z64;
    if (0 <= (int32_t)(sig_z << 1)) {
        sig_z <<= 1;
        --exp_z;
    }
    return SFLOAT_PACK_round(state, sign_z, exp_z, sig_z);
}

static sfloat_t sfloat_div(sfloat_state_t *state, sfloat_t a, sfloat_t b) {
    int16_t  exp_a   = SFLOAT_EXTRACT_EXP(a);
    int16_t  exp_b   = SFLOAT_EXTRACT_EXP(b);
    int16_t  exp_z   = 0;
    uint32_t sig_a   = SFLOAT_EXTRACT_FRAC(a);
    uint32_t sig_b   = SFLOAT_EXTRACT_FRAC(b);
    uint32_t sig_z   = 0;
    bool     sign_a  = SFLOAT_EXTRACT_SIGN(a);
    bool     sign_b  = SFLOAT_EXTRACT_SIGN(b);
    bool     sign_z  = sign_a ^ sign_b;

    if (exp_a == 0xFF) {
        if (sig_a)
            return sfloat_propagate_nan(state, a, b);
        if (exp_b == 0xFF) {
            if (sig_b)
                return sfloat_propagate_nan(state, a, b);
            SFLOAT_RAISE(state, SFLOAT_INVALID);
            return SFLOAT_NAN;
        }
        return SFLOAT_PACK(sign_z, 0xFF, 0);
    }
    if (exp_b == 0xFF)
        return (sig_b) ? sfloat_propagate_nan(state, a, b) : SFLOAT_PACK(sign_z, 0, 0);
    if (exp_b == 0) {
        if (sig_b == 0) {
            if ((exp_a | sig_a) == 0) {
                SFLOAT_RAISE(state, SFLOAT_INVALID);
                return SFLOAT_NAN;
            }
            SFLOAT_RAISE(state, SFLOAT_DIVBYZERO);
            return SFLOAT_PACK(sign_z, 0xFF, 0);
        }
        SFLOAT_SUBNORMALIZE(sig_b, &exp_b, &sig_b);
    }
    if (exp_a == 0) {
        if (sig_a == 0)
            return SFLOAT_PACK(sign_z, 0, 0);
        SFLOAT_SUBNORMALIZE(sig_a, &exp_a, &sig_a);
    }
    exp_z = exp_a - exp_b + 0x7D;
    sig_a = (sig_a | 0x00800000) << 7;
    sig_b = (sig_b | 0x00800000) << 8;
    if (sig_b <= (sig_a + sig_a)) {
        sig_a >>= 1;
        ++exp_z;
    }
    sig_z = (((uint64_t)sig_a) << 32) / sig_b;
    if ((sig_z & 0x3F) == 0)
        sig_z |= ((uint64_t)sig_b * sig_z != ((uint64_t)sig_a) << 32);
    return SFLOAT_PACK_round(state, sign_z, exp_z, sig_z);
}

static sfloat_t sfloat_neg(sfloat_state_t *state, sfloat_t a) {
    sfloat_cast_t neg;
    neg.f = -1;
    return sfloat_mul(state, a, neg.s);
}

static GMQCC_INLINE void sfloat_check(lex_ctx_t ctx, sfloat_state_t *state, const char *vec) {
    /* Exception comes from vector component */
    if (vec) {
        if (state->exceptionflags & SFLOAT_DIVBYZERO)
            compile_error(ctx, "division by zero in `%s' component", vec);
        if (state->exceptionflags & SFLOAT_INVALID)
            compile_error(ctx, "undefined (inf) in `%s' component", vec);
        if (state->exceptionflags & SFLOAT_OVERFLOW)
            compile_error(ctx, "arithmetic overflow in `%s' component", vec);
        if (state->exceptionflags & SFLOAT_UNDERFLOW)
            compile_error(ctx, "arithmetic underflow in `%s' component", vec);
            return;
    }
    if (state->exceptionflags & SFLOAT_DIVBYZERO)
        compile_error(ctx, "division by zero");
    if (state->exceptionflags & SFLOAT_INVALID)
        compile_error(ctx, "undefined (inf)");
    if (state->exceptionflags & SFLOAT_OVERFLOW)
        compile_error(ctx, "arithmetic overflow");
    if (state->exceptionflags & SFLOAT_UNDERFLOW)
        compile_error(ctx, "arithmetic underflow");
}

static GMQCC_INLINE void sfloat_init(sfloat_state_t *state) {
    state->exceptionflags = SFLOAT_NOEXCEPT;
    state->roundingmode   = FOLD_ROUNDING;
    state->tiny           = FOLD_TINYNESS;
}

/*
 * There is two stages to constant folding in GMQCC: there is the parse
 * stage constant folding, where, with the help of the AST, operator
 * usages can be constant folded. Then there is the constant folding
 * in the IR for things like eliding if statements, can occur.
 *
 * This file is thus, split into two parts.
 */

#define isfloat(X)      (((ast_expression*)(X))->vtype == TYPE_FLOAT)
#define isvector(X)     (((ast_expression*)(X))->vtype == TYPE_VECTOR)
#define isstring(X)     (((ast_expression*)(X))->vtype == TYPE_STRING)
#define isarray(X)      (((ast_expression*)(X))->vtype == TYPE_ARRAY)
#define isfloats(X,Y)   (isfloat  (X) && isfloat (Y))

/*
 * Implementation of basic vector math for vec3_t, for trivial constant
 * folding.
 *
 * TODO: gcc/clang hinting for autovectorization
 */
typedef enum {
    VEC_COMP_X = 1 << 0,
    VEC_COMP_Y = 1 << 1,
    VEC_COMP_Z = 1 << 2
} vec3_comp_t;

typedef struct {
    sfloat_cast_t x;
    sfloat_cast_t y;
    sfloat_cast_t z;
} vec3_soft_t;

typedef struct {
    vec3_comp_t    faults;
    sfloat_state_t state[3];
} vec3_soft_state_t;

static GMQCC_INLINE vec3_soft_t vec3_soft_convert(vec3_t vec) {
    vec3_soft_t soft;
    soft.x.f = vec.x;
    soft.y.f = vec.y;
    soft.z.f = vec.z;
    return soft;
}

static GMQCC_INLINE bool vec3_soft_exception(vec3_soft_state_t *vstate, size_t index) {
    sfloat_exceptionflags_t flags = vstate->state[index].exceptionflags;
    if (flags & SFLOAT_DIVBYZERO) return true;
    if (flags & SFLOAT_INVALID)   return true;
    if (flags & SFLOAT_OVERFLOW)  return true;
    if (flags & SFLOAT_UNDERFLOW) return true;
    return false;
}

static GMQCC_INLINE void vec3_soft_eval(vec3_soft_state_t *state,
                                        sfloat_t         (*callback)(sfloat_state_t *, sfloat_t, sfloat_t),
                                        vec3_t             a,
                                        vec3_t             b)
{
    vec3_soft_t sa = vec3_soft_convert(a);
    vec3_soft_t sb = vec3_soft_convert(b);
    callback(&state->state[0], sa.x.s, sb.x.s);
    if (vec3_soft_exception(state, 0)) state->faults = (vec3_comp_t)(state->faults | VEC_COMP_X);
    callback(&state->state[1], sa.y.s, sb.y.s);
    if (vec3_soft_exception(state, 1)) state->faults = (vec3_comp_t)(state->faults | VEC_COMP_Y);
    callback(&state->state[2], sa.z.s, sb.z.s);
    if (vec3_soft_exception(state, 2)) state->faults = (vec3_comp_t)(state->faults | VEC_COMP_Z);
}

static GMQCC_INLINE void vec3_check_except(vec3_t     a,
                                           vec3_t     b,
                                           lex_ctx_t  ctx,
                                           sfloat_t (*callback)(sfloat_state_t *, sfloat_t, sfloat_t))
{
    vec3_soft_state_t state;

    if (!OPTS_FLAG(ARITHMETIC_EXCEPTIONS))
        return;

    sfloat_init(&state.state[0]);
    sfloat_init(&state.state[1]);
    sfloat_init(&state.state[2]);

    vec3_soft_eval(&state, callback, a, b);
    if (state.faults & VEC_COMP_X) sfloat_check(ctx, &state.state[0], "x");
    if (state.faults & VEC_COMP_Y) sfloat_check(ctx, &state.state[1], "y");
    if (state.faults & VEC_COMP_Z) sfloat_check(ctx, &state.state[2], "z");
}

static GMQCC_INLINE vec3_t vec3_add(lex_ctx_t ctx, vec3_t a, vec3_t b) {
    vec3_t out;
    vec3_check_except(a, b, ctx, &sfloat_add);
    out.x = a.x + b.x;
    out.y = a.y + b.y;
    out.z = a.z + b.z;
    return out;
}

static GMQCC_INLINE vec3_t vec3_sub(lex_ctx_t ctx, vec3_t a, vec3_t b) {
    vec3_t out;
    vec3_check_except(a, b, ctx, &sfloat_sub);
    out.x = a.x - b.x;
    out.y = a.y - b.y;
    out.z = a.z - b.z;
    return out;
}

static GMQCC_INLINE vec3_t vec3_neg(lex_ctx_t ctx, vec3_t a) {
    vec3_t         out;
    sfloat_cast_t  v[3];
    sfloat_state_t s[3];

    if (!OPTS_FLAG(ARITHMETIC_EXCEPTIONS))
        goto end;

    v[0].f = a.x;
    v[1].f = a.y;
    v[2].f = a.z;

    sfloat_init(&s[0]);
    sfloat_init(&s[1]);
    sfloat_init(&s[2]);

    sfloat_neg(&s[0], v[0].s);
    sfloat_neg(&s[1], v[1].s);
    sfloat_neg(&s[2], v[2].s);

    sfloat_check(ctx, &s[0], NULL);
    sfloat_check(ctx, &s[1], NULL);
    sfloat_check(ctx, &s[2], NULL);

end:
    out.x = -a.x;
    out.y = -a.y;
    out.z = -a.z;
    return out;
}

static GMQCC_INLINE vec3_t vec3_or(vec3_t a, vec3_t b) {
    vec3_t out;
    out.x = (qcfloat_t)(((qcint_t)a.x) | ((qcint_t)b.x));
    out.y = (qcfloat_t)(((qcint_t)a.y) | ((qcint_t)b.y));
    out.z = (qcfloat_t)(((qcint_t)a.z) | ((qcint_t)b.z));
    return out;
}

static GMQCC_INLINE vec3_t vec3_orvf(vec3_t a, qcfloat_t b) {
    vec3_t out;
    out.x = (qcfloat_t)(((qcint_t)a.x) | ((qcint_t)b));
    out.y = (qcfloat_t)(((qcint_t)a.y) | ((qcint_t)b));
    out.z = (qcfloat_t)(((qcint_t)a.z) | ((qcint_t)b));
    return out;
}

static GMQCC_INLINE vec3_t vec3_and(vec3_t a, vec3_t b) {
    vec3_t out;
    out.x = (qcfloat_t)(((qcint_t)a.x) & ((qcint_t)b.x));
    out.y = (qcfloat_t)(((qcint_t)a.y) & ((qcint_t)b.y));
    out.z = (qcfloat_t)(((qcint_t)a.z) & ((qcint_t)b.z));
    return out;
}

static GMQCC_INLINE vec3_t vec3_andvf(vec3_t a, qcfloat_t b) {
    vec3_t out;
    out.x = (qcfloat_t)(((qcint_t)a.x) & ((qcint_t)b));
    out.y = (qcfloat_t)(((qcint_t)a.y) & ((qcint_t)b));
    out.z = (qcfloat_t)(((qcint_t)a.z) & ((qcint_t)b));
    return out;
}

static GMQCC_INLINE vec3_t vec3_xor(vec3_t a, vec3_t b) {
    vec3_t out;
    out.x = (qcfloat_t)(((qcint_t)a.x) ^ ((qcint_t)b.x));
    out.y = (qcfloat_t)(((qcint_t)a.y) ^ ((qcint_t)b.y));
    out.z = (qcfloat_t)(((qcint_t)a.z) ^ ((qcint_t)b.z));
    return out;
}

static GMQCC_INLINE vec3_t vec3_xorvf(vec3_t a, qcfloat_t b) {
    vec3_t out;
    out.x = (qcfloat_t)(((qcint_t)a.x) ^ ((qcint_t)b));
    out.y = (qcfloat_t)(((qcint_t)a.y) ^ ((qcint_t)b));
    out.z = (qcfloat_t)(((qcint_t)a.z) ^ ((qcint_t)b));
    return out;
}

static GMQCC_INLINE vec3_t vec3_not(vec3_t a) {
    vec3_t out;
    out.x = -1-a.x;
    out.y = -1-a.y;
    out.z = -1-a.z;
    return out;
}

static GMQCC_INLINE qcfloat_t vec3_mulvv(lex_ctx_t ctx, vec3_t a, vec3_t b) {
    vec3_soft_t    sa;
    vec3_soft_t    sb;
    sfloat_state_t s[5];
    sfloat_t       r[5];

    if (!OPTS_FLAG(ARITHMETIC_EXCEPTIONS))
        goto end;

    sa = vec3_soft_convert(a);
    sb = vec3_soft_convert(b);

    sfloat_init(&s[0]);
    sfloat_init(&s[1]);
    sfloat_init(&s[2]);
    sfloat_init(&s[3]);
    sfloat_init(&s[4]);

    r[0] = sfloat_mul(&s[0], sa.x.s, sb.x.s);
    r[1] = sfloat_mul(&s[1], sa.y.s, sb.y.s);
    r[2] = sfloat_mul(&s[2], sa.z.s, sb.z.s);
    r[3] = sfloat_add(&s[3], r[0],   r[1]);
    r[4] = sfloat_add(&s[4], r[3],   r[2]);

    sfloat_check(ctx, &s[0], NULL);
    sfloat_check(ctx, &s[1], NULL);
    sfloat_check(ctx, &s[2], NULL);
    sfloat_check(ctx, &s[3], NULL);
    sfloat_check(ctx, &s[4], NULL);

end:
    return (a.x * b.x + a.y * b.y + a.z * b.z);
}

static GMQCC_INLINE vec3_t vec3_mulvf(lex_ctx_t ctx, vec3_t a, qcfloat_t b) {
    vec3_t         out;
    vec3_soft_t    sa;
    sfloat_cast_t  sb;
    sfloat_state_t s[3];

    if (!OPTS_FLAG(ARITHMETIC_EXCEPTIONS))
        goto end;

    sa   = vec3_soft_convert(a);
    sb.f = b;
    sfloat_init(&s[0]);
    sfloat_init(&s[1]);
    sfloat_init(&s[2]);

    sfloat_mul(&s[0], sa.x.s, sb.s);
    sfloat_mul(&s[1], sa.y.s, sb.s);
    sfloat_mul(&s[2], sa.z.s, sb.s);

    sfloat_check(ctx, &s[0], "x");
    sfloat_check(ctx, &s[1], "y");
    sfloat_check(ctx, &s[2], "z");

end:
    out.x = a.x * b;
    out.y = a.y * b;
    out.z = a.z * b;
    return out;
}

static GMQCC_INLINE bool vec3_cmp(vec3_t a, vec3_t b) {
    return a.x == b.x &&
           a.y == b.y &&
           a.z == b.z;
}

static GMQCC_INLINE vec3_t vec3_create(float x, float y, float z) {
    vec3_t out;
    out.x = x;
    out.y = y;
    out.z = z;
    return out;
}

static GMQCC_INLINE qcfloat_t vec3_notf(vec3_t a) {
    return (!a.x && !a.y && !a.z);
}

static GMQCC_INLINE bool vec3_pbool(vec3_t a) {
    return (a.x || a.y || a.z);
}

static GMQCC_INLINE vec3_t vec3_cross(lex_ctx_t ctx, vec3_t a, vec3_t b) {
    vec3_t         out;
    vec3_soft_t    sa;
    vec3_soft_t    sb;
    sfloat_t       r[9];
    sfloat_state_t s[9];

    if (!OPTS_FLAG(ARITHMETIC_EXCEPTIONS))
        goto end;

    sa = vec3_soft_convert(a);
    sb = vec3_soft_convert(b);

    sfloat_init(&s[0]);
    sfloat_init(&s[1]);
    sfloat_init(&s[2]);
    sfloat_init(&s[3]);
    sfloat_init(&s[4]);
    sfloat_init(&s[5]);
    sfloat_init(&s[6]);
    sfloat_init(&s[7]);
    sfloat_init(&s[8]);

    r[0] = sfloat_mul(&s[0], sa.y.s, sb.z.s);
    r[1] = sfloat_mul(&s[1], sa.z.s, sb.y.s);
    r[2] = sfloat_mul(&s[2], sa.z.s, sb.x.s);
    r[3] = sfloat_mul(&s[3], sa.x.s, sb.z.s);
    r[4] = sfloat_mul(&s[4], sa.x.s, sb.y.s);
    r[5] = sfloat_mul(&s[5], sa.y.s, sb.x.s);
    r[6] = sfloat_sub(&s[6], r[0],   r[1]);
    r[7] = sfloat_sub(&s[7], r[2],   r[3]);
    r[8] = sfloat_sub(&s[8], r[4],   r[5]);

    sfloat_check(ctx, &s[0], NULL);
    sfloat_check(ctx, &s[1], NULL);
    sfloat_check(ctx, &s[2], NULL);
    sfloat_check(ctx, &s[3], NULL);
    sfloat_check(ctx, &s[4], NULL);
    sfloat_check(ctx, &s[5], NULL);
    sfloat_check(ctx, &s[6], "x");
    sfloat_check(ctx, &s[7], "y");
    sfloat_check(ctx, &s[8], "z");

end:
    out.x = a.y * b.z - a.z * b.y;
    out.y = a.z * b.x - a.x * b.z;
    out.z = a.x * b.y - a.y * b.x;
    return out;
}

static lex_ctx_t fold_ctx(fold_t *fold) {
    lex_ctx_t ctx;
    if (fold->parser->lex)
        return parser_ctx(fold->parser);

    memset(&ctx, 0, sizeof(ctx));
    return ctx;
}

static GMQCC_INLINE bool fold_immediate_true(fold_t *fold, ast_value *v) {
    switch (v->expression.vtype) {
        case TYPE_FLOAT:
            return !!v->constval.vfloat;
        case TYPE_INTEGER:
            return !!v->constval.vint;
        case TYPE_VECTOR:
            if (OPTS_FLAG(CORRECT_LOGIC))
                return vec3_pbool(v->constval.vvec);
            return !!(v->constval.vvec.x);
        case TYPE_STRING:
            if (!v->constval.vstring)
                return false;
            if (OPTS_FLAG(TRUE_EMPTY_STRINGS))
                return true;
            return !!v->constval.vstring[0];
        default:
            compile_error(fold_ctx(fold), "internal error: fold_immediate_true on invalid type");
            break;
    }
    return !!v->constval.vfunc;
}

/* Handy macros to determine if an ast_value can be constant folded. */
#define fold_can_1(X)  \
    (ast_istype(((ast_expression*)(X)), ast_value) && (X)->hasvalue && ((X)->cvq == CV_CONST) && \
                ((ast_expression*)(X))->vtype != TYPE_FUNCTION)

#define fold_can_2(X, Y) (fold_can_1(X) && fold_can_1(Y))

#define fold_immvalue_float(E)  ((E)->constval.vfloat)
#define fold_immvalue_vector(E) ((E)->constval.vvec)
#define fold_immvalue_string(E) ((E)->constval.vstring)

fold_t *fold_init(parser_t *parser) {
    fold_t *fold                 = (fold_t*)mem_a(sizeof(fold_t));
    fold->parser                 = parser;
    fold->imm_float              = NULL;
    fold->imm_vector             = NULL;
    fold->imm_string             = NULL;
    fold->imm_string_untranslate = util_htnew(FOLD_STRING_UNTRANSLATE_HTSIZE);
    fold->imm_string_dotranslate = util_htnew(FOLD_STRING_DOTRANSLATE_HTSIZE);

    /*
     * prime the tables with common constant values at constant
     * locations.
     */
    (void)fold_constgen_float (fold,  0.0f, false);
    (void)fold_constgen_float (fold,  1.0f, false);
    (void)fold_constgen_float (fold, -1.0f, false);
    (void)fold_constgen_float (fold,  2.0f, false);

    (void)fold_constgen_vector(fold, vec3_create(0.0f, 0.0f, 0.0f));
    (void)fold_constgen_vector(fold, vec3_create(-1.0f, -1.0f, -1.0f));

    return fold;
}

bool fold_generate(fold_t *fold, ir_builder *ir) {
    /* generate globals for immediate folded values */
    size_t     i;
    ast_value *cur;

    for (i = 0; i < vec_size(fold->imm_float);   ++i)
        if (!ast_global_codegen ((cur = fold->imm_float[i]), ir, false)) goto err;
    for (i = 0; i < vec_size(fold->imm_vector);  ++i)
        if (!ast_global_codegen((cur = fold->imm_vector[i]), ir, false)) goto err;
    for (i = 0; i < vec_size(fold->imm_string);  ++i)
        if (!ast_global_codegen((cur = fold->imm_string[i]), ir, false)) goto err;

    return true;

err:
    con_out("failed to generate global %s\n", cur->name);
    ir_builder_delete(ir);
    return false;
}

void fold_cleanup(fold_t *fold) {
    size_t i;

    for (i = 0; i < vec_size(fold->imm_float);  ++i) ast_delete(fold->imm_float[i]);
    for (i = 0; i < vec_size(fold->imm_vector); ++i) ast_delete(fold->imm_vector[i]);
    for (i = 0; i < vec_size(fold->imm_string); ++i) ast_delete(fold->imm_string[i]);

    vec_free(fold->imm_float);
    vec_free(fold->imm_vector);
    vec_free(fold->imm_string);

    util_htdel(fold->imm_string_untranslate);
    util_htdel(fold->imm_string_dotranslate);

    mem_d(fold);
}

ast_expression *fold_constgen_float(fold_t *fold, qcfloat_t value, bool inexact) {
    ast_value  *out = NULL;
    size_t      i;

    for (i = 0; i < vec_size(fold->imm_float); i++) {
        if (!memcmp(&fold->imm_float[i]->constval.vfloat, &value, sizeof(qcfloat_t)))
            return (ast_expression*)fold->imm_float[i];
    }

    out                  = ast_value_new(fold_ctx(fold), "#IMMEDIATE", TYPE_FLOAT);
    out->cvq             = CV_CONST;
    out->hasvalue        = true;
    out->inexact         = inexact;
    out->constval.vfloat = value;

    vec_push(fold->imm_float, out);

    return (ast_expression*)out;
}

ast_expression *fold_constgen_vector(fold_t *fold, vec3_t value) {
    ast_value *out;
    size_t     i;

    for (i = 0; i < vec_size(fold->imm_vector); i++) {
        if (vec3_cmp(fold->imm_vector[i]->constval.vvec, value))
            return (ast_expression*)fold->imm_vector[i];
    }

    out                = ast_value_new(fold_ctx(fold), "#IMMEDIATE", TYPE_VECTOR);
    out->cvq           = CV_CONST;
    out->hasvalue      = true;
    out->constval.vvec = value;

    vec_push(fold->imm_vector, out);

    return (ast_expression*)out;
}

ast_expression *fold_constgen_string(fold_t *fold, const char *str, bool translate) {
    hash_table_t *table = (translate) ? fold->imm_string_untranslate : fold->imm_string_dotranslate;
    ast_value    *out   = NULL;
    size_t        hash  = util_hthash(table, str);

    if ((out = (ast_value*)util_htgeth(table, str, hash)))
        return (ast_expression*)out;

    if (translate) {
        char name[32];
        util_snprintf(name, sizeof(name), "dotranslate_%lu", (unsigned long)(fold->parser->translated++));
        out                    = ast_value_new(parser_ctx(fold->parser), name, TYPE_STRING);
        out->expression.flags |= AST_FLAG_INCLUDE_DEF; /* def needs to be included for translatables */
    } else
        out                    = ast_value_new(fold_ctx(fold), "#IMMEDIATE", TYPE_STRING);

    out->cvq              = CV_CONST;
    out->hasvalue         = true;
    out->isimm            = true;
    out->constval.vstring = parser_strdup(str);

    vec_push(fold->imm_string, out);
    util_htseth(table, str, hash, out);

    return (ast_expression*)out;
}

typedef union {
    void     (*callback)(void);
    sfloat_t (*binary)(sfloat_state_t *, sfloat_t, sfloat_t);
    sfloat_t (*unary)(sfloat_state_t *, sfloat_t);
} float_check_callback_t;

static bool fold_check_except_float_impl(void     (*callback)(void),
                                         fold_t    *fold,
                                         ast_value *a,
                                         ast_value *b)
{
    float_check_callback_t call;
    sfloat_state_t s;
    sfloat_cast_t ca;

    if (!OPTS_FLAG(ARITHMETIC_EXCEPTIONS) && !OPTS_WARN(WARN_INEXACT_COMPARES))
        return false;

    call.callback = callback;
    sfloat_init(&s);
    ca.f = fold_immvalue_float(a);
    if (b) {
        sfloat_cast_t cb;
        cb.f = fold_immvalue_float(b);
        call.binary(&s, ca.s, cb.s);
    } else {
        call.unary(&s, ca.s);
    }

    if (s.exceptionflags == 0)
        return false;

    if (!OPTS_FLAG(ARITHMETIC_EXCEPTIONS))
        goto inexact_possible;

    sfloat_check(fold_ctx(fold), &s, NULL);

inexact_possible:
    return s.exceptionflags & SFLOAT_INEXACT;
}

#define fold_check_except_float(CALLBACK, FOLD, A, B) \
    fold_check_except_float_impl(((void (*)(void))(CALLBACK)), (FOLD), (A), (B))

static bool fold_check_inexact_float(fold_t *fold, ast_value *a, ast_value *b) {
    lex_ctx_t ctx = fold_ctx(fold);
    if (!OPTS_WARN(WARN_INEXACT_COMPARES))
        return false;
    if (!a->inexact && !b->inexact)
        return false;
    return compile_warning(ctx, WARN_INEXACT_COMPARES, "inexact value in comparison");
}

static GMQCC_INLINE ast_expression *fold_op_mul_vec(fold_t *fold, vec3_t vec, ast_value *sel, const char *set) {
    qcfloat_t x = (&vec.x)[set[0]-'x'];
    qcfloat_t y = (&vec.x)[set[1]-'x'];
    qcfloat_t z = (&vec.x)[set[2]-'x'];
    if (!y && !z) {
        ast_expression *out;
        ++opts_optimizationcount[OPTIM_VECTOR_COMPONENTS];
        out                        = (ast_expression*)ast_member_new(fold_ctx(fold), (ast_expression*)sel, set[0]-'x', NULL);
        out->node.keep             = false;
        ((ast_member*)out)->rvalue = true;
        if (x != -1.0f)
            return (ast_expression*)ast_binary_new(fold_ctx(fold), INSTR_MUL_F, fold_constgen_float(fold, x, false), out);
    }
    return NULL;
}


static GMQCC_INLINE ast_expression *fold_op_neg(fold_t *fold, ast_value *a) {
    if (isfloat(a)) {
        if (fold_can_1(a)) {
            /* Negation can produce inexact as well */
            bool inexact = fold_check_except_float(&sfloat_neg, fold, a, NULL);
            return fold_constgen_float(fold, -fold_immvalue_float(a), inexact);
        }
    } else if (isvector(a)) {
        if (fold_can_1(a))
            return fold_constgen_vector(fold, vec3_neg(fold_ctx(fold), fold_immvalue_vector(a)));
    }
    return NULL;
}

static GMQCC_INLINE ast_expression *fold_op_not(fold_t *fold, ast_value *a) {
    if (isfloat(a)) {
        if (fold_can_1(a))
            return fold_constgen_float(fold, !fold_immvalue_float(a), false);
    } else if (isvector(a)) {
        if (fold_can_1(a))
            return fold_constgen_float(fold, vec3_notf(fold_immvalue_vector(a)), false);
    } else if (isstring(a)) {
        if (fold_can_1(a)) {
            if (OPTS_FLAG(TRUE_EMPTY_STRINGS))
                return fold_constgen_float(fold, !fold_immvalue_string(a), false);
            else
                return fold_constgen_float(fold, !fold_immvalue_string(a) || !*fold_immvalue_string(a), false);
        }
    }
    return NULL;
}

static GMQCC_INLINE ast_expression *fold_op_add(fold_t *fold, ast_value *a, ast_value *b) {
    if (isfloat(a)) {
        if (fold_can_2(a, b)) {
            bool inexact = fold_check_except_float(&sfloat_add, fold, a, b);
            return fold_constgen_float(fold, fold_immvalue_float(a) + fold_immvalue_float(b), inexact);
        }
    } else if (isvector(a)) {
        if (fold_can_2(a, b))
            return fold_constgen_vector(fold, vec3_add(fold_ctx(fold),
                                                       fold_immvalue_vector(a),
                                                       fold_immvalue_vector(b)));
    }
    return NULL;
}

static GMQCC_INLINE ast_expression *fold_op_sub(fold_t *fold, ast_value *a, ast_value *b) {
    if (isfloat(a)) {
        if (fold_can_2(a, b)) {
            bool inexact = fold_check_except_float(&sfloat_sub, fold, a, b);
            return fold_constgen_float(fold, fold_immvalue_float(a) - fold_immvalue_float(b), inexact);
        }
    } else if (isvector(a)) {
        if (fold_can_2(a, b))
            return fold_constgen_vector(fold, vec3_sub(fold_ctx(fold),
                                                       fold_immvalue_vector(a),
                                                       fold_immvalue_vector(b)));
    }
    return NULL;
}

static GMQCC_INLINE ast_expression *fold_op_mul(fold_t *fold, ast_value *a, ast_value *b) {
    if (isfloat(a)) {
        if (isvector(b)) {
            if (fold_can_2(a, b))
                return fold_constgen_vector(fold, vec3_mulvf(fold_ctx(fold), fold_immvalue_vector(b), fold_immvalue_float(a)));
        } else {
            if (fold_can_2(a, b)) {
                bool inexact = fold_check_except_float(&sfloat_mul, fold, a, b);
                return fold_constgen_float(fold, fold_immvalue_float(a) * fold_immvalue_float(b), inexact);
            }
        }
    } else if (isvector(a)) {
        if (isfloat(b)) {
            if (fold_can_2(a, b))
                return fold_constgen_vector(fold, vec3_mulvf(fold_ctx(fold), fold_immvalue_vector(a), fold_immvalue_float(b)));
        } else {
            if (fold_can_2(a, b)) {
                return fold_constgen_float(fold, vec3_mulvv(fold_ctx(fold), fold_immvalue_vector(a), fold_immvalue_vector(b)), false);
            } else if (OPTS_OPTIMIZATION(OPTIM_VECTOR_COMPONENTS) && fold_can_1(a)) {
                ast_expression *out;
                if ((out = fold_op_mul_vec(fold, fold_immvalue_vector(a), b, "xyz"))) return out;
                if ((out = fold_op_mul_vec(fold, fold_immvalue_vector(a), b, "yxz"))) return out;
                if ((out = fold_op_mul_vec(fold, fold_immvalue_vector(a), b, "zxy"))) return out;
            } else if (OPTS_OPTIMIZATION(OPTIM_VECTOR_COMPONENTS) && fold_can_1(b)) {
                ast_expression *out;
                if ((out = fold_op_mul_vec(fold, fold_immvalue_vector(b), a, "xyz"))) return out;
                if ((out = fold_op_mul_vec(fold, fold_immvalue_vector(b), a, "yxz"))) return out;
                if ((out = fold_op_mul_vec(fold, fold_immvalue_vector(b), a, "zxy"))) return out;
            }
        }
    }
    return NULL;
}

static GMQCC_INLINE ast_expression *fold_op_div(fold_t *fold, ast_value *a, ast_value *b) {
    if (isfloat(a)) {
        if (fold_can_2(a, b)) {
            bool inexact = fold_check_except_float(&sfloat_div, fold, a, b);
            return fold_constgen_float(fold, fold_immvalue_float(a) / fold_immvalue_float(b), inexact);
        } else if (fold_can_1(b)) {
            return (ast_expression*)ast_binary_new(
                fold_ctx(fold),
                INSTR_MUL_F,
                (ast_expression*)a,
                fold_constgen_float(fold, 1.0f / fold_immvalue_float(b), false)
            );
        }
    } else if (isvector(a)) {
        if (fold_can_2(a, b)) {
            return fold_constgen_vector(fold, vec3_mulvf(fold_ctx(fold), fold_immvalue_vector(a), 1.0f / fold_immvalue_float(b)));
        } else {
            return (ast_expression*)ast_binary_new(
                fold_ctx(fold),
                INSTR_MUL_VF,
                (ast_expression*)a,
                (fold_can_1(b))
                    ? (ast_expression*)fold_constgen_float(fold, 1.0f / fold_immvalue_float(b), false)
                    : (ast_expression*)ast_binary_new(
                                            fold_ctx(fold),
                                            INSTR_DIV_F,
                                            (ast_expression*)fold->imm_float[1],
                                            (ast_expression*)b
                    )
            );
        }
    }
    return NULL;
}

static GMQCC_INLINE ast_expression *fold_op_mod(fold_t *fold, ast_value *a, ast_value *b) {
    return (fold_can_2(a, b))
                ? fold_constgen_float(fold, fmod(fold_immvalue_float(a), fold_immvalue_float(b)), false)
                : NULL;
}

static GMQCC_INLINE ast_expression *fold_op_bor(fold_t *fold, ast_value *a, ast_value *b) {
    if (isfloat(a)) {
        if (fold_can_2(a, b))
            return fold_constgen_float(fold, (qcfloat_t)(((qcint_t)fold_immvalue_float(a)) | ((qcint_t)fold_immvalue_float(b))), false);
    } else {
        if (isvector(b)) {
            if (fold_can_2(a, b))
                return fold_constgen_vector(fold, vec3_or(fold_immvalue_vector(a), fold_immvalue_vector(b)));
        } else {
            if (fold_can_2(a, b))
                return fold_constgen_vector(fold, vec3_orvf(fold_immvalue_vector(a), fold_immvalue_float(b)));
        }
    }
    return NULL;
}

static GMQCC_INLINE ast_expression *fold_op_band(fold_t *fold, ast_value *a, ast_value *b) {
    if (isfloat(a)) {
        if (fold_can_2(a, b))
            return fold_constgen_float(fold, (qcfloat_t)(((qcint_t)fold_immvalue_float(a)) & ((qcint_t)fold_immvalue_float(b))), false);
    } else {
        if (isvector(b)) {
            if (fold_can_2(a, b))
                return fold_constgen_vector(fold, vec3_and(fold_immvalue_vector(a), fold_immvalue_vector(b)));
        } else {
            if (fold_can_2(a, b))
                return fold_constgen_vector(fold, vec3_andvf(fold_immvalue_vector(a), fold_immvalue_float(b)));
        }
    }
    return NULL;
}

static GMQCC_INLINE ast_expression *fold_op_xor(fold_t *fold, ast_value *a, ast_value *b) {
    if (isfloat(a)) {
        if (fold_can_2(a, b))
            return fold_constgen_float(fold, (qcfloat_t)(((qcint_t)fold_immvalue_float(a)) ^ ((qcint_t)fold_immvalue_float(b))), false);
    } else {
        if (fold_can_2(a, b)) {
            if (isvector(b))
                return fold_constgen_vector(fold, vec3_xor(fold_immvalue_vector(a), fold_immvalue_vector(b)));
            else
                return fold_constgen_vector(fold, vec3_xorvf(fold_immvalue_vector(a), fold_immvalue_float(b)));
        }
    }
    return NULL;
}

static GMQCC_INLINE ast_expression *fold_op_lshift(fold_t *fold, ast_value *a, ast_value *b) {
    if (fold_can_2(a, b) && isfloats(a, b))
        return fold_constgen_float(fold, (qcfloat_t)floorf(fold_immvalue_float(a) * powf(2.0f, fold_immvalue_float(b))), false);
    return NULL;
}

static GMQCC_INLINE ast_expression *fold_op_rshift(fold_t *fold, ast_value *a, ast_value *b) {
    if (fold_can_2(a, b) && isfloats(a, b))
        return fold_constgen_float(fold, (qcfloat_t)floorf(fold_immvalue_float(a) / powf(2.0f, fold_immvalue_float(b))), false);
    return NULL;
}

static GMQCC_INLINE ast_expression *fold_op_andor(fold_t *fold, ast_value *a, ast_value *b, float expr) {
    if (fold_can_2(a, b)) {
        if (OPTS_FLAG(PERL_LOGIC)) {
            if (expr)
                return (fold_immediate_true(fold, a)) ? (ast_expression*)a : (ast_expression*)b;
            else
                return (fold_immediate_true(fold, a)) ? (ast_expression*)b : (ast_expression*)a;
        } else {
            return fold_constgen_float (
                fold,
                ((expr) ? (fold_immediate_true(fold, a) || fold_immediate_true(fold, b))
                        : (fold_immediate_true(fold, a) && fold_immediate_true(fold, b)))
                            ? 1
                            : 0,
                false
            );
        }
    }
    return NULL;
}

static GMQCC_INLINE ast_expression *fold_op_tern(fold_t *fold, ast_value *a, ast_value *b, ast_value *c) {
    if (fold_can_1(a)) {
        return fold_immediate_true(fold, a)
                    ? (ast_expression*)b
                    : (ast_expression*)c;
    }
    return NULL;
}

static GMQCC_INLINE ast_expression *fold_op_exp(fold_t *fold, ast_value *a, ast_value *b) {
    if (fold_can_2(a, b))
        return fold_constgen_float(fold, (qcfloat_t)powf(fold_immvalue_float(a), fold_immvalue_float(b)), false);
    return NULL;
}

static GMQCC_INLINE ast_expression *fold_op_lteqgt(fold_t *fold, ast_value *a, ast_value *b) {
    if (fold_can_2(a,b)) {
        fold_check_inexact_float(fold, a, b);
        if (fold_immvalue_float(a) <  fold_immvalue_float(b)) return (ast_expression*)fold->imm_float[2];
        if (fold_immvalue_float(a) == fold_immvalue_float(b)) return (ast_expression*)fold->imm_float[0];
        if (fold_immvalue_float(a) >  fold_immvalue_float(b)) return (ast_expression*)fold->imm_float[1];
    }
    return NULL;
}

static GMQCC_INLINE ast_expression *fold_op_ltgt(fold_t *fold, ast_value *a, ast_value *b, bool lt) {
    if (fold_can_2(a, b)) {
        fold_check_inexact_float(fold, a, b);
        return (lt) ? (ast_expression*)fold->imm_float[!!(fold_immvalue_float(a) < fold_immvalue_float(b))]
                    : (ast_expression*)fold->imm_float[!!(fold_immvalue_float(a) > fold_immvalue_float(b))];
    }
    return NULL;
}

static GMQCC_INLINE ast_expression *fold_op_cmp(fold_t *fold, ast_value *a, ast_value *b, bool ne) {
    if (fold_can_2(a, b)) {
        if (isfloat(a) && isfloat(b)) {
            float la = fold_immvalue_float(a);
            float lb = fold_immvalue_float(b);
            fold_check_inexact_float(fold, a, b);
            return (ast_expression*)fold->imm_float[ne ? la != lb : la == lb];
        } else if (isvector(a) && isvector(b)) {
            vec3_t la = fold_immvalue_vector(a);
            vec3_t lb = fold_immvalue_vector(b);
            bool compare = vec3_cmp(la, lb);
            return (ast_expression*)fold->imm_float[ne ? !compare : compare];
        } else if (isstring(a) && isstring(b)) {
            bool compare = !strcmp(fold_immvalue_string(a), fold_immvalue_string(b));
            return (ast_expression*)fold->imm_float[ne ? !compare : compare];
        }
    }
    return NULL;
}

static GMQCC_INLINE ast_expression *fold_op_bnot(fold_t *fold, ast_value *a) {
    if (isfloat(a)) {
        if (fold_can_1(a))
            return fold_constgen_float(fold, -1-fold_immvalue_float(a), false);
    } else {
        if (isvector(a)) {
            if (fold_can_1(a))
                return fold_constgen_vector(fold, vec3_not(fold_immvalue_vector(a)));
        }
    }
    return NULL;
}

static GMQCC_INLINE ast_expression *fold_op_cross(fold_t *fold, ast_value *a, ast_value *b) {
    if (fold_can_2(a, b))
        return fold_constgen_vector(fold, vec3_cross(fold_ctx(fold),
                                                     fold_immvalue_vector(a),
                                                     fold_immvalue_vector(b)));
    return NULL;
}

static GMQCC_INLINE ast_expression *fold_op_length(fold_t *fold, ast_value *a) {
    if (fold_can_1(a) && isstring(a))
        return fold_constgen_float(fold, strlen(fold_immvalue_string(a)), false);
    if (isarray(a))
        return fold_constgen_float(fold, vec_size(a->initlist), false);
    return NULL;
}

ast_expression *fold_op(fold_t *fold, const oper_info *info, ast_expression **opexprs) {
    ast_value      *a = (ast_value*)opexprs[0];
    ast_value      *b = (ast_value*)opexprs[1];
    ast_value      *c = (ast_value*)opexprs[2];
    ast_expression *e = NULL;

    /* can a fold operation be applied to this operator usage? */
    if (!info->folds)
        return NULL;

    switch(info->operands) {
        case 3: if(!c) return NULL;
        case 2: if(!b) return NULL;
        case 1:
        if(!a) {
            compile_error(fold_ctx(fold), "internal error: fold_op no operands to fold\n");
            return NULL;
        }
    }

    /*
     * we could use a boolean and default case but ironically gcc produces
     * invalid broken assembly from that operation. clang/tcc get it right,
     * but interestingly ignore compiling this to a jump-table when I do that,
     * this happens to be the most efficent method, since you have per-level
     * granularity on the pointer check happening only for the case you check
     * it in. Opposed to the default method which would involve a boolean and
     * pointer check after wards.
     */
    #define fold_op_case(ARGS, ARGS_OPID, OP, ARGS_FOLD)    \
        case opid##ARGS ARGS_OPID:                          \
            if ((e = fold_op_##OP ARGS_FOLD)) {             \
                ++opts_optimizationcount[OPTIM_CONST_FOLD]; \
            }                                               \
            return e

    switch(info->id) {
        fold_op_case(2, ('-', 'P'),      neg,    (fold, a));
        fold_op_case(2, ('!', 'P'),      not,    (fold, a));
        fold_op_case(1, ('+'),           add,    (fold, a, b));
        fold_op_case(1, ('-'),           sub,    (fold, a, b));
        fold_op_case(1, ('*'),           mul,    (fold, a, b));
        fold_op_case(1, ('/'),           div,    (fold, a, b));
        fold_op_case(1, ('%'),           mod,    (fold, a, b));
        fold_op_case(1, ('|'),           bor,    (fold, a, b));
        fold_op_case(1, ('&'),           band,   (fold, a, b));
        fold_op_case(1, ('^'),           xor,    (fold, a, b));
        fold_op_case(1, ('<'),           ltgt,   (fold, a, b, true));
        fold_op_case(1, ('>'),           ltgt,   (fold, a, b, false));
        fold_op_case(2, ('<', '<'),      lshift, (fold, a, b));
        fold_op_case(2, ('>', '>'),      rshift, (fold, a, b));
        fold_op_case(2, ('|', '|'),      andor,  (fold, a, b, true));
        fold_op_case(2, ('&', '&'),      andor,  (fold, a, b, false));
        fold_op_case(2, ('?', ':'),      tern,   (fold, a, b, c));
        fold_op_case(2, ('*', '*'),      exp,    (fold, a, b));
        fold_op_case(3, ('<','=','>'),   lteqgt, (fold, a, b));
        fold_op_case(2, ('!', '='),      cmp,    (fold, a, b, true));
        fold_op_case(2, ('=', '='),      cmp,    (fold, a, b, false));
        fold_op_case(2, ('~', 'P'),      bnot,   (fold, a));
        fold_op_case(2, ('>', '<'),      cross,  (fold, a, b));
        fold_op_case(3, ('l', 'e', 'n'), length, (fold, a));
    }
    #undef fold_op_case
    compile_error(fold_ctx(fold), "internal error: attempted to constant-fold for unsupported operator");
    return NULL;
}

/*
 * Constant folding for compiler intrinsics, similar approach to operator
 * folding, primarily: individual functions for each intrinsics to fold,
 * and a generic selection function.
 */
static GMQCC_INLINE ast_expression *fold_intrin_isfinite(fold_t *fold, ast_value *a) {
    return fold_constgen_float(fold, isfinite(fold_immvalue_float(a)), false);
}
static GMQCC_INLINE ast_expression *fold_intrin_isinf(fold_t *fold, ast_value *a) {
    return fold_constgen_float(fold, isinf(fold_immvalue_float(a)), false);
}
static GMQCC_INLINE ast_expression *fold_intrin_isnan(fold_t *fold, ast_value *a) {
    return fold_constgen_float(fold, isnan(fold_immvalue_float(a)), false);
}
static GMQCC_INLINE ast_expression *fold_intrin_isnormal(fold_t *fold, ast_value *a) {
    return fold_constgen_float(fold, isnormal(fold_immvalue_float(a)), false);
}
static GMQCC_INLINE ast_expression *fold_intrin_signbit(fold_t *fold, ast_value *a) {
    return fold_constgen_float(fold, signbit(fold_immvalue_float(a)), false);
}
static GMQCC_INLINE ast_expression *fold_intirn_acosh(fold_t *fold, ast_value *a) {
    return fold_constgen_float(fold, acoshf(fold_immvalue_float(a)), false);
}
static GMQCC_INLINE ast_expression *fold_intrin_asinh(fold_t *fold, ast_value *a) {
    return fold_constgen_float(fold, asinhf(fold_immvalue_float(a)), false);
}
static GMQCC_INLINE ast_expression *fold_intrin_atanh(fold_t *fold, ast_value *a) {
    return fold_constgen_float(fold, (float)atanh(fold_immvalue_float(a)), false);
}
static GMQCC_INLINE ast_expression *fold_intrin_exp(fold_t *fold, ast_value *a) {
    return fold_constgen_float(fold, expf(fold_immvalue_float(a)), false);
}
static GMQCC_INLINE ast_expression *fold_intrin_exp2(fold_t *fold, ast_value *a) {
    return fold_constgen_float(fold, exp2f(fold_immvalue_float(a)), false);
}
static GMQCC_INLINE ast_expression *fold_intrin_expm1(fold_t *fold, ast_value *a) {
    return fold_constgen_float(fold, expm1f(fold_immvalue_float(a)), false);
}
static GMQCC_INLINE ast_expression *fold_intrin_mod(fold_t *fold, ast_value *lhs, ast_value *rhs) {
    return fold_constgen_float(fold, fmodf(fold_immvalue_float(lhs), fold_immvalue_float(rhs)), false);
}
static GMQCC_INLINE ast_expression *fold_intrin_pow(fold_t *fold, ast_value *lhs, ast_value *rhs) {
    return fold_constgen_float(fold, powf(fold_immvalue_float(lhs), fold_immvalue_float(rhs)), false);
}
static GMQCC_INLINE ast_expression *fold_intrin_fabs(fold_t *fold, ast_value *a) {
    return fold_constgen_float(fold, fabsf(fold_immvalue_float(a)), false);
}


ast_expression *fold_intrin(fold_t *fold, const char *intrin, ast_expression **arg) {
    ast_expression *ret = NULL;
    ast_value      *a   = (ast_value*)arg[0];
    ast_value      *b   = (ast_value*)arg[1];

    if (!strcmp(intrin, "isfinite")) ret = fold_intrin_isfinite(fold, a);
    if (!strcmp(intrin, "isinf"))    ret = fold_intrin_isinf(fold, a);
    if (!strcmp(intrin, "isnan"))    ret = fold_intrin_isnan(fold, a);
    if (!strcmp(intrin, "isnormal")) ret = fold_intrin_isnormal(fold, a);
    if (!strcmp(intrin, "signbit"))  ret = fold_intrin_signbit(fold, a);
    if (!strcmp(intrin, "acosh"))    ret = fold_intirn_acosh(fold, a);
    if (!strcmp(intrin, "asinh"))    ret = fold_intrin_asinh(fold, a);
    if (!strcmp(intrin, "atanh"))    ret = fold_intrin_atanh(fold, a);
    if (!strcmp(intrin, "exp"))      ret = fold_intrin_exp(fold, a);
    if (!strcmp(intrin, "exp2"))     ret = fold_intrin_exp2(fold, a);
    if (!strcmp(intrin, "expm1"))    ret = fold_intrin_expm1(fold, a);
    if (!strcmp(intrin, "mod"))      ret = fold_intrin_mod(fold, a, b);
    if (!strcmp(intrin, "pow"))      ret = fold_intrin_pow(fold, a, b);
    if (!strcmp(intrin, "fabs"))     ret = fold_intrin_fabs(fold, a);

    if (ret)
        ++opts_optimizationcount[OPTIM_CONST_FOLD];

    return ret;
}

/*
 * These are all the actual constant folding methods that happen in between
 * the AST/IR stage of the compiler , i.e eliminating branches for const
 * expressions, which is the only supported thing so far. We undefine the
 * testing macros here because an ir_value is differant than an ast_value.
 */
#undef expect
#undef isfloat
#undef isstring
#undef isvector
#undef fold_immvalue_float
#undef fold_immvalue_string
#undef fold_immvalue_vector
#undef fold_can_1
#undef fold_can_2

#define isfloat(X)              ((X)->vtype == TYPE_FLOAT)
/*#define isstring(X)             ((X)->vtype == TYPE_STRING)*/
/*#define isvector(X)             ((X)->vtype == TYPE_VECTOR)*/
#define fold_immvalue_float(X)  ((X)->constval.vfloat)
#define fold_immvalue_vector(X) ((X)->constval.vvec)
/*#define fold_immvalue_string(X) ((X)->constval.vstring)*/
#define fold_can_1(X)           ((X)->hasvalue && (X)->cvq == CV_CONST)
/*#define fold_can_2(X,Y)         (fold_can_1(X) && fold_can_1(Y))*/

static ast_expression *fold_superfluous(ast_expression *left, ast_expression *right, int op) {
    ast_expression *swapped = NULL; /* using this as bool */
    ast_value *load;

    if (!ast_istype(right, ast_value) || !fold_can_1((load = (ast_value*)right))) {
        swapped = left;
        left    = right;
        right   = swapped;
    }

    if (!ast_istype(right, ast_value) || !fold_can_1((load = (ast_value*)right)))
        return NULL;

    switch (op) {
        case INSTR_DIV_F:
            if (swapped)
                return NULL;
        case INSTR_MUL_F:
            if (fold_immvalue_float(load) == 1.0f) {
                ++opts_optimizationcount[OPTIM_PEEPHOLE];
                ast_unref(right);
                return left;
            }
            break;


        case INSTR_SUB_F:
            if (swapped)
                return NULL;
        case INSTR_ADD_F:
            if (fold_immvalue_float(load) == 0.0f) {
                ++opts_optimizationcount[OPTIM_PEEPHOLE];
                ast_unref(right);
                return left;
            }
            break;

        case INSTR_MUL_V:
            if (vec3_cmp(fold_immvalue_vector(load), vec3_create(1, 1, 1))) {
                ++opts_optimizationcount[OPTIM_PEEPHOLE];
                ast_unref(right);
                return left;
            }
            break;

        case INSTR_SUB_V:
            if (swapped)
                return NULL;
        case INSTR_ADD_V:
            if (vec3_cmp(fold_immvalue_vector(load), vec3_create(0, 0, 0))) {
                ++opts_optimizationcount[OPTIM_PEEPHOLE];
                ast_unref(right);
                return left;
            }
            break;
    }

    return NULL;
}

ast_expression *fold_binary(lex_ctx_t ctx, int op, ast_expression *left, ast_expression *right) {
    ast_expression *ret = fold_superfluous(left, right, op);
    if (ret)
        return ret;
    return (ast_expression*)ast_binary_new(ctx, op, left, right);
}

static GMQCC_INLINE int fold_cond(ir_value *condval, ast_function *func, ast_ifthen *branch) {
    if (isfloat(condval) && fold_can_1(condval) && OPTS_OPTIMIZATION(OPTIM_CONST_FOLD_DCE)) {
        ast_expression_codegen *cgen;
        ir_block               *elide;
        ir_value               *dummy;
        bool                    istrue  = (fold_immvalue_float(condval) != 0.0f && branch->on_true);
        bool                    isfalse = (fold_immvalue_float(condval) == 0.0f && branch->on_false);
        ast_expression         *path    = (istrue)  ? branch->on_true  :
                                          (isfalse) ? branch->on_false : NULL;
        if (!path) {
            /*
             * no path to take implies that the evaluation is if(0) and there
             * is no else block. so eliminate all the code.
             */
            ++opts_optimizationcount[OPTIM_CONST_FOLD_DCE];
            return true;
        }

        if (!(elide = ir_function_create_block(ast_ctx(branch), func->ir_func, ast_function_label(func, ((istrue) ? "ontrue" : "onfalse")))))
            return false;
        if (!(*(cgen = path->codegen))((ast_expression*)path, func, false, &dummy))
            return false;
        if (!ir_block_create_jump(func->curblock, ast_ctx(branch), elide))
            return false;
        /*
         * now the branch has been eliminated and the correct block for the constant evaluation
         * is expanded into the current block for the function.
         */
        func->curblock = elide;
        ++opts_optimizationcount[OPTIM_CONST_FOLD_DCE];
        return true;
    }
    return -1; /* nothing done */
}

int fold_cond_ternary(ir_value *condval, ast_function *func, ast_ternary *branch) {
    return fold_cond(condval, func, (ast_ifthen*)branch);
}

int fold_cond_ifthen(ir_value *condval, ast_function *func, ast_ifthen *branch) {
    return fold_cond(condval, func, branch);
}
