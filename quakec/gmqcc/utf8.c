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
#include "gmqcc.h"

/*
 * Based on the flexible and economical utf8 decoder:
 * http://bjoern.hoehrmann.de/utf-8/decoder/dfa/
 *
 * This is slightly more economical, the fastest way to decode utf8 is
 * with a lookup table as in:
 *
 * first 1-byte lookup
 * if that fails, 2-byte lookup
 * if that fails, 3-byte lookup
 * if that fails, 4-byte lookup
 *
 * The following table can be generated with some interval trickery.
 * consider an interval [a, b):
 *
 *      a must be 0x80 or b must be 0xc0, lower 3 bits
 *      are clear, thus:
 *          interval(a,b) = ((uint32_t)((a==0x80?0x40-b:-a)<<23))
 *
 * The failstate can be represented as interval(0x80,0x80), it's
 * odd to see but this is a full state machine.
 *
 * The table than maps the corresponding sections as a serise of
 * intervals.
 *
 * In this table the transition values are pre-multiplied with 16 to
 * save a shift instruction for every byte, we throw away fillers
 * which makes the table smaller.
 *
 * The first section of the table handles bytes with leading C
 * The second section of the table handles bytes with leading D
 * The third section of the table handles bytes with leading E
 * The last section of the table handles bytes with leading F
 *
 * The values themselfs in the table are arranged so that when you
 * left shift them by 6 to shift continuation characters into place, the
 * new top bits tell you:
 *
 *  1 - if you keep going
 *  2 - the range of valid values for the next byte
 */
static const uint32_t utf8_tab[] = {
    0xC0000002, 0xC0000003, 0xC0000004, 0xC0000005, 0xC0000006,
    0xC0000007, 0xC0000008, 0xC0000009, 0xC000000A, 0xC000000B,
    0xC000000C, 0xC000000D, 0xC000000E, 0xC000000F, 0xC0000010,
    0xC0000011, 0xC0000012, 0xC0000013, 0xC0000014, 0xC0000015,
    0xC0000016, 0xC0000017, 0xC0000018, 0xC0000019, 0xC000001A,
    0xC000001B, 0xC000001C, 0xC000001D, 0xC000001E, 0xC000001F,
    0xB3000000, 0xC3000001, 0xC3000002, 0xC3000003, 0xC3000004,
    0xC3000005, 0xC3000006, 0xC3000007, 0xC3000008, 0xC3000009,
    0xC300000A, 0xC300000B, 0xC300000C, 0xD300000D, 0xC300000E,
    0xC300000F, 0xBB0C0000, 0xC30C0001, 0xC30C0002, 0xC30C0003,
    0xD30C0004
};

int utf8_from(char *s, utf8ch_t ch) {
    if (!s)
        return 0;

    if ((unsigned)ch < 0x80) {
        *s = ch;
        return 1;
    } else if ((unsigned)ch < 0x800) {
        *s++ = 0xC0 | (ch >> 6);
        *s   = 0x80 | (ch & 0x3F);
        return 2;
    } else if ((unsigned)ch < 0xD800 || (unsigned)ch - 0xE000 < 0x2000) {
        *s++ = 0xE0 | (ch >> 12);
        *s++ = 0x80 | ((ch >> 6) & 0x3F);
        *s   = 0x80 | (ch & 0x3F);
        return 3;
    } else if ((unsigned)ch - 0x10000 < 0x100000) {
        *s++ = 0xF0 | (ch >> 18);
        *s++ = 0x80 | ((ch >> 12) & 0x3F);
        *s++ = 0x80 | ((ch >> 6) & 0x3F);
        *s   = 0x80 | (ch & 0x3F);
        return 4;
    }
    return 0;
}

int utf8_to(utf8ch_t *i, const unsigned char *s, size_t n) {
    unsigned c,j;

    if (!s || !n)
        return 0;

    /* This is consistent with mbtowc behaviour. */
    if (!i)
        i = (utf8ch_t*)(void*)&i;

    if (*s < 0x80)
        return !!(*i = *s);
    if (*s-0xC2U > 0x32)
        return 0;

    c = utf8_tab[*s++-0xC2U];

    /*
     * Avoid excessive checks against n.
     *
     * When shifting state `n-1` times does not clear the high bit,
     * then the value of `n` won't satisfy the condition to read a
     * character as it will be insufficent.
     */
    if (n < 4 && ((c<<(6*n-6)) & (1U << 31)))
        return 0;

    /*
     * The upper 6 state bits are negitive integer offset to a bound-check
     * next byte equivlant to: ((b-0x80)+(b+offset))&~0x3f
     */
    if ((((*s>>3)-0x10)|((*s>>3)+((int32_t)c>>26))) & ~7)
        return 0;

    for (j=2; j<3; j++) {
        if (!((c = c<<6 | (*s++-0x80))&(1U<<31))) {
            *i = c;
            return j;
        }
        if (*s-0x80U >= 0x40)
            return 0;
    }

    *i = c<<6 | (*s++-0x80);
    return 4;
}
