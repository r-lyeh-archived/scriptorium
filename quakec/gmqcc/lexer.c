/*
 * Copyright (C) 2012, 2013, 2014, 2015
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
#include <stdlib.h>

#include "gmqcc.h"
#include "lexer.h"

/*
 * List of Keywords
 */

/* original */
static const char *keywords_qc[] = {
    "for", "do", "while",
    "if", "else",
    "local",
    "return",
    "const"
};
/* For fte/gmgqcc */
static const char *keywords_fg[] = {
    "switch", "case", "default",
    "struct", "union",
    "break", "continue",
    "typedef",
    "goto",

    "__builtin_debug_printtype"
};

/*
 * Lexer code
 */
static char* *lex_filenames;

static void lexerror(lex_file *lex, const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    if (lex)
        con_vprintmsg(LVL_ERROR, lex->name, lex->sline, lex->column, "parse error", fmt, ap);
    else
        con_vprintmsg(LVL_ERROR, "", 0, 0, "parse error", fmt, ap);
    va_end(ap);
}

static bool lexwarn(lex_file *lex, int warntype, const char *fmt, ...)
{
    bool      r;
    lex_ctx_t ctx;
    va_list   ap;

    ctx.file   = lex->name;
    ctx.line   = lex->sline;
    ctx.column = lex->column;

    va_start(ap, fmt);
    r = vcompile_warning(ctx, warntype, fmt, ap);
    va_end(ap);
    return r;
}

static void lex_token_new(lex_file *lex)
{
    if (lex->tok.value)
        vec_shrinkto(lex->tok.value, 0);

    lex->tok.constval.t  = 0;
    lex->tok.ctx.line    = lex->sline;
    lex->tok.ctx.file    = lex->name;
    lex->tok.ctx.column  = lex->column;
}

static void lex_ungetch(lex_file *lex, int ch);
static int lex_getch(lex_file *lex);

lex_file* lex_open(const char *file)
{
    lex_file  *lex;
    fs_file_t *in = fs_file_open(file, "rb");
    uint32_t   read;

    if (!in) {
        lexerror(NULL, "open failed: '%s'\n", file);
        return NULL;
    }

    lex = (lex_file*)mem_a(sizeof(*lex));
    if (!lex) {
        fs_file_close(in);
        lexerror(NULL, "out of memory\n");
        return NULL;
    }

    memset(lex, 0, sizeof(*lex));

    lex->file    = in;
    lex->name    = util_strdup(file);
    lex->line    = 1; /* we start counting at 1 */
    lex->column  = 0;
    lex->peekpos = 0;
    lex->eof     = false;

    /* handle BOM */
    if ((read = (lex_getch(lex) << 16) | (lex_getch(lex) << 8) | lex_getch(lex)) != 0xEFBBBF) {
        lex_ungetch(lex, (read & 0x0000FF));
        lex_ungetch(lex, (read & 0x00FF00) >> 8);
        lex_ungetch(lex, (read & 0xFF0000) >> 16);
    } else {
        /*
         * otherwise the lexer has advanced 3 bytes for the BOM, we need
         * to set the column back to 0
         */
        lex->column = 0;
    }

    vec_push(lex_filenames, lex->name);
    return lex;
}

lex_file* lex_open_string(const char *str, size_t len, const char *name)
{
    lex_file *lex;

    lex = (lex_file*)mem_a(sizeof(*lex));
    if (!lex) {
        lexerror(NULL, "out of memory\n");
        return NULL;
    }

    memset(lex, 0, sizeof(*lex));

    lex->file = NULL;
    lex->open_string        = str;
    lex->open_string_length = len;
    lex->open_string_pos    = 0;

    lex->name    = util_strdup(name ? name : "<string-source>");
    lex->line    = 1; /* we start counting at 1 */
    lex->peekpos = 0;
    lex->eof     = false;
    lex->column  = 0;

    vec_push(lex_filenames, lex->name);

    return lex;
}

void lex_cleanup(void)
{
    size_t i;
    for (i = 0; i < vec_size(lex_filenames); ++i)
        mem_d(lex_filenames[i]);
    vec_free(lex_filenames);
}

void lex_close(lex_file *lex)
{
    size_t i;
    for (i = 0; i < vec_size(lex->frames); ++i)
        mem_d(lex->frames[i].name);
    vec_free(lex->frames);

    if (lex->modelname)
        vec_free(lex->modelname);

    if (lex->file)
        fs_file_close(lex->file);

    vec_free(lex->tok.value);

    /* mem_d(lex->name); collected in lex_filenames */
    mem_d(lex);
}



static int lex_fgetc(lex_file *lex)
{
    if (lex->file) {
        lex->column++;
        return fs_file_getc(lex->file);
    }
    if (lex->open_string) {
        if (lex->open_string_pos >= lex->open_string_length)
            return FS_FILE_EOF;
        lex->column++;
        return lex->open_string[lex->open_string_pos++];
    }
    return FS_FILE_EOF;
}

/* Get or put-back data
 * The following to functions do NOT understand what kind of data they
 * are working on.
 * The are merely wrapping get/put in order to count line numbers.
 */
static int lex_try_trigraph(lex_file *lex, int old)
{
    int c2, c3;
    c2 = lex_fgetc(lex);
    if (!lex->push_line && c2 == '\n') {
        lex->line++;
        lex->column = 0;
    }

    if (c2 != '?') {
        lex_ungetch(lex, c2);
        return old;
    }

    c3 = lex_fgetc(lex);
    if (!lex->push_line && c3 == '\n') {
        lex->line++;
        lex->column = 0;
    }

    switch (c3) {
        case '=': return '#';
        case '/': return '\\';
        case '\'': return '^';
        case '(': return '[';
        case ')': return ']';
        case '!': return '|';
        case '<': return '{';
        case '>': return '}';
        case '-': return '~';
        default:
            lex_ungetch(lex, c3);
            lex_ungetch(lex, c2);
            return old;
    }
}

static int lex_try_digraph(lex_file *lex, int ch)
{
    int c2;
    c2 = lex_fgetc(lex);
    /* we just used fgetc() so count lines
     * need to offset a \n the ungetch would recognize
     */
    if (!lex->push_line && c2 == '\n')
        lex->line++;
    if      (ch == '<' && c2 == ':')
        return '[';
    else if (ch == ':' && c2 == '>')
        return ']';
    else if (ch == '<' && c2 == '%')
        return '{';
    else if (ch == '%' && c2 == '>')
        return '}';
    else if (ch == '%' && c2 == ':')
        return '#';
    lex_ungetch(lex, c2);
    return ch;
}

static int lex_getch(lex_file *lex)
{
    int ch;

    if (lex->peekpos) {
        lex->peekpos--;
        if (!lex->push_line && lex->peek[lex->peekpos] == '\n') {
            lex->line++;
            lex->column = 0;
        }
        return lex->peek[lex->peekpos];
    }

    ch = lex_fgetc(lex);
    if (!lex->push_line && ch == '\n') {
        lex->line++;
        lex->column = 0;
    }
    else if (ch == '?')
        return lex_try_trigraph(lex, ch);
    else if (!lex->flags.nodigraphs && (ch == '<' || ch == ':' || ch == '%'))
        return lex_try_digraph(lex, ch);
    return ch;
}

static void lex_ungetch(lex_file *lex, int ch)
{
    lex->peek[lex->peekpos++] = ch;
    lex->column--;
    if (!lex->push_line && ch == '\n') {
        lex->line--;
        lex->column = 0;
    }
}

/* classify characters
 * some additions to the is*() functions of ctype.h
 */

/* Idents are alphanumberic, but they start with alpha or _ */
static bool isident_start(int ch)
{
    return util_isalpha(ch) || ch == '_';
}

static bool isident(int ch)
{
    return isident_start(ch) || util_isdigit(ch);
}

/* isxdigit_only is used when we already know it's not a digit
 * and want to see if it's a hex digit anyway.
 */
static bool isxdigit_only(int ch)
{
    return (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F');
}

/* Append a character to the token buffer */
static void lex_tokench(lex_file *lex, int ch)
{
    vec_push(lex->tok.value, ch);
}

/* Append a trailing null-byte */
static void lex_endtoken(lex_file *lex)
{
    vec_push(lex->tok.value, 0);
    vec_shrinkby(lex->tok.value, 1);
}

static bool lex_try_pragma(lex_file *lex)
{
    int ch;
    char *pragma  = NULL;
    char *command = NULL;
    char *param   = NULL;
    size_t line;

    if (lex->flags.preprocessing)
        return false;

    line = lex->line;

    ch = lex_getch(lex);
    if (ch != '#') {
        lex_ungetch(lex, ch);
        return false;
    }

    for (ch = lex_getch(lex); vec_size(pragma) < 8 && ch >= 'a' && ch <= 'z'; ch = lex_getch(lex))
        vec_push(pragma, ch);
    vec_push(pragma, 0);

    if (ch != ' ' || strcmp(pragma, "pragma")) {
        lex_ungetch(lex, ch);
        goto unroll;
    }

    for (ch = lex_getch(lex); vec_size(command) < 32 && ch >= 'a' && ch <= 'z'; ch = lex_getch(lex))
        vec_push(command, ch);
    vec_push(command, 0);

    if (ch != '(') {
        lex_ungetch(lex, ch);
        goto unroll;
    }

    for (ch = lex_getch(lex); vec_size(param) < 1024 && ch != ')' && ch != '\n'; ch = lex_getch(lex))
        vec_push(param, ch);
    vec_push(param, 0);

    if (ch != ')') {
        lex_ungetch(lex, ch);
        goto unroll;
    }

    if (!strcmp(command, "push")) {
        if (!strcmp(param, "line")) {
            lex->push_line++;
            if (lex->push_line == 1)
                --line;
        }
        else
            goto unroll;
    }
    else if (!strcmp(command, "pop")) {
        if (!strcmp(param, "line")) {
            if (lex->push_line)
                lex->push_line--;
            if (lex->push_line == 0)
                --line;
        }
        else
            goto unroll;
    }
    else if (!strcmp(command, "file")) {
        lex->name = util_strdup(param);
        vec_push(lex_filenames, lex->name);
    }
    else if (!strcmp(command, "line")) {
        line = strtol(param, NULL, 0)-1;
    }
    else
        goto unroll;

    lex->line = line;
    while (ch != '\n' && ch != FS_FILE_EOF)
        ch = lex_getch(lex);
    vec_free(command);
    vec_free(param);
    vec_free(pragma);
    return true;

unroll:
    if (command) {
        vec_pop(command);
        while (vec_size(command)) {
            lex_ungetch(lex, (unsigned char)vec_last(command));
            vec_pop(command);
        }
        vec_free(command);
        lex_ungetch(lex, ' ');
    }
    if (param) {
        vec_pop(param);
        while (vec_size(param)) {
            lex_ungetch(lex, (unsigned char)vec_last(param));
            vec_pop(param);
        }
        vec_free(param);
        lex_ungetch(lex, ' ');
    }
    if (pragma) {
        vec_pop(pragma);
        while (vec_size(pragma)) {
            lex_ungetch(lex, (unsigned char)vec_last(pragma));
            vec_pop(pragma);
        }
        vec_free(pragma);
    }
    lex_ungetch(lex, '#');

    lex->line = line;
    return false;
}

/* Skip whitespace and comments and return the first
 * non-white character.
 * As this makes use of the above getch() ungetch() functions,
 * we don't need to care at all about line numbering anymore.
 *
 * In theory, this function should only be used at the beginning
 * of lexing, or when we *know* the next character is part of the token.
 * Otherwise, if the parser throws an error, the linenumber may not be
 * the line of the error, but the line of the next token AFTER the error.
 *
 * This is currently only problematic when using c-like string-continuation,
 * since comments and whitespaces are allowed between 2 such strings.
 * Example:
printf(   "line one\n"
// A comment
          "A continuation of the previous string"
// This line is skipped
      , foo);

 * In this case, if the parse decides it didn't actually want a string,
 * and uses lex->line to print an error, it will show the ', foo);' line's
 * linenumber.
 *
 * On the other hand, the parser is supposed to remember the line of the next
 * token's beginning. In this case we would want skipwhite() to be called
 * AFTER reading a token, so that the parser, before reading the NEXT token,
 * doesn't store teh *comment's* linenumber, but the actual token's linenumber.
 *
 * THIS SOLUTION
 *    here is to store the line of the first character after skipping
 *    the initial whitespace in lex->sline, this happens in lex_do.
 */
static int lex_skipwhite(lex_file *lex, bool hadwhite)
{
    int ch = 0;
    bool haswhite = hadwhite;

    do
    {
        ch = lex_getch(lex);
        while (ch != FS_FILE_EOF && util_isspace(ch)) {
            if (ch == '\n') {
                if (lex_try_pragma(lex))
                    continue;
            }
            if (lex->flags.preprocessing) {
                if (ch == '\n') {
                    /* end-of-line */
                    /* see if there was whitespace first */
                    if (haswhite) { /* (vec_size(lex->tok.value)) { */
                        lex_ungetch(lex, ch);
                        lex_endtoken(lex);
                        return TOKEN_WHITE;
                    }
                    /* otherwise return EOL */
                    return TOKEN_EOL;
                }
                haswhite = true;
                lex_tokench(lex, ch);
            }
            ch = lex_getch(lex);
        }

        if (ch == '/') {
            ch = lex_getch(lex);
            if (ch == '/')
            {
                /* one line comment */
                ch = lex_getch(lex);

                if (lex->flags.preprocessing) {
                    haswhite = true;
                    lex_tokench(lex, ' ');
                    lex_tokench(lex, ' ');
                }

                while (ch != FS_FILE_EOF && ch != '\n') {
                    if (lex->flags.preprocessing)
                        lex_tokench(lex, ' '); /* ch); */
                    ch = lex_getch(lex);
                }
                if (lex->flags.preprocessing) {
                    lex_ungetch(lex, '\n');
                    lex_endtoken(lex);
                    return TOKEN_WHITE;
                }
                continue;
            }
            if (ch == '*')
            {
                /* multiline comment */
                if (lex->flags.preprocessing) {
                    haswhite = true;
                    lex_tokench(lex, ' ');
                    lex_tokench(lex, ' ');
                }

                while (ch != FS_FILE_EOF)
                {
                    ch = lex_getch(lex);
                    if (ch == '*') {
                        ch = lex_getch(lex);
                        if (ch == '/') {
                            if (lex->flags.preprocessing) {
                                lex_tokench(lex, ' ');
                                lex_tokench(lex, ' ');
                            }
                            break;
                        }
                        lex_ungetch(lex, ch);
                    }
                    if (lex->flags.preprocessing) {
                        if (ch == '\n')
                            lex_tokench(lex, '\n');
                        else
                            lex_tokench(lex, ' ');
                    }
                }
                ch = ' '; /* cause TRUE in the isspace check */
                continue;
            }
            /* Otherwise roll back to the slash and break out of the loop */
            lex_ungetch(lex, ch);
            ch = '/';
            break;
        }
    } while (ch != FS_FILE_EOF && util_isspace(ch));

    if (haswhite) {
        lex_endtoken(lex);
        lex_ungetch(lex, ch);
        return TOKEN_WHITE;
    }
    return ch;
}

/* Get a token */
static bool GMQCC_WARN lex_finish_ident(lex_file *lex)
{
    int ch;

    ch = lex_getch(lex);
    while (ch != FS_FILE_EOF && isident(ch))
    {
        lex_tokench(lex, ch);
        ch = lex_getch(lex);
    }

    /* last ch was not an ident ch: */
    lex_ungetch(lex, ch);

    return true;
}

/* read one ident for the frame list */
static int lex_parse_frame(lex_file *lex)
{
    int ch;

    lex_token_new(lex);

    ch = lex_getch(lex);
    while (ch != FS_FILE_EOF && ch != '\n' && util_isspace(ch))
        ch = lex_getch(lex);

    if (ch == '\n')
        return 1;

    if (!isident_start(ch)) {
        lexerror(lex, "invalid framename, must start with one of a-z or _, got %c", ch);
        return -1;
    }

    lex_tokench(lex, ch);
    if (!lex_finish_ident(lex))
        return -1;
    lex_endtoken(lex);
    return 0;
}

/* read a list of $frames */
static bool lex_finish_frames(lex_file *lex)
{
    do {
        size_t i;
        int    rc;
        frame_macro m;

        rc = lex_parse_frame(lex);
        if (rc > 0) /* end of line */
            return true;
        if (rc < 0) /* error */
            return false;

        for (i = 0; i < vec_size(lex->frames); ++i) {
            if (!strcmp(lex->tok.value, lex->frames[i].name)) {
                lex->frames[i].value = lex->framevalue++;
                if (lexwarn(lex, WARN_FRAME_MACROS, "duplicate frame macro defined: `%s`", lex->tok.value))
                    return false;
                break;
            }
        }
        if (i < vec_size(lex->frames))
            continue;

        m.value = lex->framevalue++;
        m.name = util_strdup(lex->tok.value);
        vec_shrinkto(lex->tok.value, 0);
        vec_push(lex->frames, m);
    } while (true);

    return false;
}

static int GMQCC_WARN lex_finish_string(lex_file *lex, int quote)
{
    utf8ch_t chr = 0;
    int ch = 0, texttype = 0;
    int nextch;
    bool hex;
    bool oct;
    char u8buf[8]; /* way more than enough */
    int  u8len, uc;

    while (ch != FS_FILE_EOF)
    {
        ch = lex_getch(lex);
        if (ch == quote)
            return TOKEN_STRINGCONST;

        if (lex->flags.preprocessing && ch == '\\') {
            lex_tokench(lex, ch);
            ch = lex_getch(lex);
            if (ch == FS_FILE_EOF) {
                lexerror(lex, "unexpected end of file");
                lex_ungetch(lex, FS_FILE_EOF); /* next token to be TOKEN_EOF */
                return (lex->tok.ttype = TOKEN_ERROR);
            }
            lex_tokench(lex, ch);
        }
        else if (ch == '\\') {
            ch = lex_getch(lex);
            if (ch == FS_FILE_EOF) {
                lexerror(lex, "unexpected end of file");
                lex_ungetch(lex, FS_FILE_EOF); /* next token to be TOKEN_EOF */
                return (lex->tok.ttype = TOKEN_ERROR);
            }

            switch (ch) {
            case '\\': break;
            case '\'': break;
            case '"':  break;
            case 'a': ch = '\a'; break;
            case 'r': ch = '\r'; break;
            case 'n': ch = '\n'; break;
            case 't': ch = '\t'; break;
            case 'f': ch = '\f'; break;
            case 'v': ch = '\v'; break;
            case 'x':
            case 'X':
                /* same procedure as in fteqcc */
                ch = 0;
                nextch = lex_getch(lex);
                if      (nextch >= '0' && nextch <= '9')
                    ch += nextch - '0';
                else if (nextch >= 'a' && nextch <= 'f')
                    ch += nextch - 'a' + 10;
                else if (nextch >= 'A' && nextch <= 'F')
                    ch += nextch - 'A' + 10;
                else {
                    lexerror(lex, "bad character code");
                    lex_ungetch(lex, nextch);
                    return (lex->tok.ttype = TOKEN_ERROR);
                }

                ch *= 0x10;
                nextch = lex_getch(lex);
                if      (nextch >= '0' && nextch <= '9')
                    ch += nextch - '0';
                else if (nextch >= 'a' && nextch <= 'f')
                    ch += nextch - 'a' + 10;
                else if (nextch >= 'A' && nextch <= 'F')
                    ch += nextch - 'A' + 10;
                else {
                    lexerror(lex, "bad character code");
                    lex_ungetch(lex, nextch);
                    return (lex->tok.ttype = TOKEN_ERROR);
                }
                break;

            /* fteqcc support */
            case '0': case '1': case '2': case '3':
            case '4': case '5': case '6': case '7':
            case '8': case '9':
                ch = 18 + ch - '0';
                break;
            case '<':  ch = 29; break;
            case '-':  ch = 30; break;
            case '>':  ch = 31; break;
            case '[':  ch = 16; break;
            case ']':  ch = 17; break;
            case '{':
                chr = 0;
                nextch = lex_getch(lex);
                hex = (nextch == 'x');
                oct = (nextch == '0');
                if (!hex && !oct)
                    lex_ungetch(lex, nextch);
                for (nextch = lex_getch(lex); nextch != '}'; nextch = lex_getch(lex)) {
                    if (!hex && !oct) {
                        if (nextch >= '0' && nextch <= '9')
                            chr = chr * 10 + nextch - '0';
                        else {
                            lexerror(lex, "bad character code");
                            return (lex->tok.ttype = TOKEN_ERROR);
                        }
                    } else if (!oct) {
                        if (nextch >= '0' && nextch <= '9')
                            chr = chr * 0x10 + nextch - '0';
                        else if (nextch >= 'a' && nextch <= 'f')
                            chr = chr * 0x10 + nextch - 'a' + 10;
                        else if (nextch >= 'A' && nextch <= 'F')
                            chr = chr * 0x10 + nextch - 'A' + 10;
                        else {
                            lexerror(lex, "bad character code");
                            return (lex->tok.ttype = TOKEN_ERROR);
                        }
                    } else {
                        if (nextch >= '0' && nextch <= '9')
                            chr = chr * 8 + chr - '0';
                        else {
                            lexerror(lex, "bad character code");
                            return (lex->tok.ttype = TOKEN_ERROR);
                        }
                    }
                    if (chr > 0x10FFFF || (!OPTS_FLAG(UTF8) && chr > 255))
                    {
                        lexerror(lex, "character code out of range");
                        return (lex->tok.ttype = TOKEN_ERROR);
                    }
                }
                if (OPTS_FLAG(UTF8) && chr >= 128) {
                    u8len = utf8_from(u8buf, chr);
                    if (!u8len)
                        ch = 0;
                    else {
                        --u8len;
                        lex->column += u8len;
                        for (uc = 0; uc < u8len; ++uc)
                            lex_tokench(lex, u8buf[uc]);
                        /*
                         * the last character will be inserted with the tokench() call
                         * below the switch
                         */
                        ch = u8buf[uc];
                    }
                }
                else
                    ch = chr;
                break;

            /* high bit text */
            case 'b': case 's':
                texttype ^= 128;
                continue;

            case '\n':
                ch = '\n';
                break;

            default:
                lexwarn(lex, WARN_UNKNOWN_CONTROL_SEQUENCE, "unrecognized control sequence: \\%c", ch);
                /* so we just add the character plus backslash no matter what it actually is */
                lex_tokench(lex, '\\');
            }
            /* add the character finally */
            lex_tokench(lex, ch | texttype);
        }
        else
            lex_tokench(lex, ch);
    }
    lexerror(lex, "unexpected end of file within string constant");
    lex_ungetch(lex, FS_FILE_EOF); /* next token to be TOKEN_EOF */
    return (lex->tok.ttype = TOKEN_ERROR);
}

static int GMQCC_WARN lex_finish_digit(lex_file *lex, int lastch)
{
    bool ishex = false;

    int  ch = lastch;

    /* parse a number... */
    if (ch == '.')
        lex->tok.ttype = TOKEN_FLOATCONST;
    else
        lex->tok.ttype = TOKEN_INTCONST;

    lex_tokench(lex, ch);

    ch = lex_getch(lex);
    if (ch != '.' && !util_isdigit(ch))
    {
        if (lastch != '0' || ch != 'x')
        {
            /* end of the number or EOF */
            lex_ungetch(lex, ch);
            lex_endtoken(lex);

            lex->tok.constval.i = lastch - '0';
            return lex->tok.ttype;
        }

        ishex = true;
    }

    /* EOF would have been caught above */

    if (ch != '.')
    {
        lex_tokench(lex, ch);
        ch = lex_getch(lex);
        while (util_isdigit(ch) || (ishex && isxdigit_only(ch)))
        {
            lex_tokench(lex, ch);
            ch = lex_getch(lex);
        }
    }
    /* NOT else, '.' can come from above as well */
    if (lex->tok.ttype != TOKEN_FLOATCONST && ch == '.' && !ishex)
    {
        /* Allow floating comma in non-hex mode */
        lex->tok.ttype = TOKEN_FLOATCONST;
        lex_tokench(lex, ch);

        /* continue digits-only */
        ch = lex_getch(lex);
        while (util_isdigit(ch))
        {
            lex_tokench(lex, ch);
            ch = lex_getch(lex);
        }
    }
    /* put back the last character */
    /* but do not put back the trailing 'f' or a float */
    if (lex->tok.ttype == TOKEN_FLOATCONST && ch == 'f')
        ch = lex_getch(lex);

    /* generally we don't want words to follow numbers: */
    if (isident(ch)) {
        lexerror(lex, "unexpected trailing characters after number");
        return (lex->tok.ttype = TOKEN_ERROR);
    }
    lex_ungetch(lex, ch);

    lex_endtoken(lex);
    if (lex->tok.ttype == TOKEN_FLOATCONST)
        lex->tok.constval.f = strtod(lex->tok.value, NULL);
    else
        lex->tok.constval.i = strtol(lex->tok.value, NULL, 0);
    return lex->tok.ttype;
}

int lex_do(lex_file *lex)
{
    int ch, nextch, thirdch;
    bool hadwhite = false;

    lex_token_new(lex);

    while (true) {
        ch = lex_skipwhite(lex, hadwhite);
        hadwhite = true;
        if (!lex->flags.mergelines || ch != '\\')
            break;
        ch = lex_getch(lex);
        if (ch == '\r')
            ch = lex_getch(lex);
        if (ch != '\n') {
            lex_ungetch(lex, ch);
            ch = '\\';
            break;
        }
        /* we reached a linemerge */
        lex_tokench(lex, '\n');
        continue;
    }

    if (lex->flags.preprocessing && (ch == TOKEN_WHITE || ch == TOKEN_EOL || ch == TOKEN_FATAL)) {
        return (lex->tok.ttype = ch);
    }

    lex->sline = lex->line;
    lex->tok.ctx.line = lex->sline;
    lex->tok.ctx.file = lex->name;

    if (lex->eof)
        return (lex->tok.ttype = TOKEN_FATAL);

    if (ch == FS_FILE_EOF) {
        lex->eof = true;
        return (lex->tok.ttype = TOKEN_EOF);
    }

    /* modelgen / spiritgen commands */
    if (ch == '$' && !lex->flags.preprocessing) {
        const char *v;
        size_t frame;

        ch = lex_getch(lex);
        if (!isident_start(ch)) {
            lexerror(lex, "hanging '$' modelgen/spritegen command line");
            return lex_do(lex);
        }
        lex_tokench(lex, ch);
        if (!lex_finish_ident(lex))
            return (lex->tok.ttype = TOKEN_ERROR);
        lex_endtoken(lex);
        /* skip the known commands */
        v = lex->tok.value;

        if (!strcmp(v, "frame") || !strcmp(v, "framesave"))
        {
            /* frame/framesave command works like an enum
             * similar to fteqcc we handle this in the lexer.
             * The reason for this is that it is sensitive to newlines,
             * which the parser is unaware of
             */
            if (!lex_finish_frames(lex))
                 return (lex->tok.ttype = TOKEN_ERROR);
            return lex_do(lex);
        }

        if (!strcmp(v, "framevalue"))
        {
            ch = lex_getch(lex);
            while (ch != FS_FILE_EOF && util_isspace(ch) && ch != '\n')
                ch = lex_getch(lex);

            if (!util_isdigit(ch)) {
                lexerror(lex, "$framevalue requires an integer parameter");
                return lex_do(lex);
            }

            lex_token_new(lex);
            lex->tok.ttype = lex_finish_digit(lex, ch);
            lex_endtoken(lex);
            if (lex->tok.ttype != TOKEN_INTCONST) {
                lexerror(lex, "$framevalue requires an integer parameter");
                return lex_do(lex);
            }
            lex->framevalue = lex->tok.constval.i;
            return lex_do(lex);
        }

        if (!strcmp(v, "framerestore"))
        {
            int rc;

            lex_token_new(lex);

            rc = lex_parse_frame(lex);

            if (rc > 0) {
                lexerror(lex, "$framerestore requires a framename parameter");
                return lex_do(lex);
            }
            if (rc < 0)
                return (lex->tok.ttype = TOKEN_FATAL);

            v = lex->tok.value;
            for (frame = 0; frame < vec_size(lex->frames); ++frame) {
                if (!strcmp(v, lex->frames[frame].name)) {
                    lex->framevalue = lex->frames[frame].value;
                    return lex_do(lex);
                }
            }
            lexerror(lex, "unknown framename `%s`", v);
            return lex_do(lex);
        }

        if (!strcmp(v, "modelname"))
        {
            int rc;

            lex_token_new(lex);

            rc = lex_parse_frame(lex);

            if (rc > 0) {
                lexerror(lex, "$modelname requires a parameter");
                return lex_do(lex);
            }
            if (rc < 0)
                return (lex->tok.ttype = TOKEN_FATAL);

            if (lex->modelname) {
                frame_macro m;
                m.value = lex->framevalue;
                m.name = lex->modelname;
                lex->modelname = NULL;
                vec_push(lex->frames, m);
            }
            lex->modelname = lex->tok.value;
            lex->tok.value = NULL;
            return lex_do(lex);
        }

        if (!strcmp(v, "flush"))
        {
            size_t fi;
            for (fi = 0; fi < vec_size(lex->frames); ++fi)
                mem_d(lex->frames[fi].name);
            vec_free(lex->frames);
            /* skip line (fteqcc does it too) */
            ch = lex_getch(lex);
            while (ch != FS_FILE_EOF && ch != '\n')
                ch = lex_getch(lex);
            return lex_do(lex);
        }

        if (!strcmp(v, "cd") ||
            !strcmp(v, "origin") ||
            !strcmp(v, "base") ||
            !strcmp(v, "flags") ||
            !strcmp(v, "scale") ||
            !strcmp(v, "skin"))
        {
            /* skip line */
            ch = lex_getch(lex);
            while (ch != FS_FILE_EOF && ch != '\n')
                ch = lex_getch(lex);
            return lex_do(lex);
        }

        for (frame = 0; frame < vec_size(lex->frames); ++frame) {
            if (!strcmp(v, lex->frames[frame].name)) {
                lex->tok.constval.i = lex->frames[frame].value;
                return (lex->tok.ttype = TOKEN_INTCONST);
            }
        }

        lexerror(lex, "invalid frame macro");
        return lex_do(lex);
    }

    /* single-character tokens */
    switch (ch)
    {
        case '[':
            nextch = lex_getch(lex);
            if (nextch == '[') {
                lex_tokench(lex, ch);
                lex_tokench(lex, nextch);
                lex_endtoken(lex);
                return (lex->tok.ttype = TOKEN_ATTRIBUTE_OPEN);
            }
            lex_ungetch(lex, nextch);
            /* FALL THROUGH */
        case '(':
        case ':':
        case '?':
            lex_tokench(lex, ch);
            lex_endtoken(lex);
            if (lex->flags.noops)
                return (lex->tok.ttype = ch);
            else
                return (lex->tok.ttype = TOKEN_OPERATOR);

        case ']':
            if (lex->flags.noops) {
                nextch = lex_getch(lex);
                if (nextch == ']') {
                    lex_tokench(lex, ch);
                    lex_tokench(lex, nextch);
                    lex_endtoken(lex);
                    return (lex->tok.ttype = TOKEN_ATTRIBUTE_CLOSE);
                }
                lex_ungetch(lex, nextch);
            }
            /* FALL THROUGH */
        case ')':
        case ';':
        case '{':
        case '}':

        case '#':
            lex_tokench(lex, ch);
            lex_endtoken(lex);
            return (lex->tok.ttype = ch);
        default:
            break;
    }

    if (ch == '.') {
        nextch = lex_getch(lex);
        /* digits starting with a dot */
        if (util_isdigit(nextch)) {
            lex_ungetch(lex, nextch);
            lex->tok.ttype = lex_finish_digit(lex, ch);
            lex_endtoken(lex);
            return lex->tok.ttype;
        }
        lex_ungetch(lex, nextch);
    }

    if (lex->flags.noops)
    {
        /* Detect characters early which are normally
         * operators OR PART of an operator.
         */
        switch (ch)
        {
            case '*':
            case '/':
            case '<':
            case '>':
            case '=':
            case '&':
            case '|':
            case '^':
            case '~':
            case ',':
            case '!':
                lex_tokench(lex, ch);
                lex_endtoken(lex);
                return (lex->tok.ttype = ch);
            default:
                break;
        }
    }

    if (ch == '.')
    {
        lex_tokench(lex, ch);
        /* peak ahead once */
        nextch = lex_getch(lex);
        if (nextch != '.') {
            lex_ungetch(lex, nextch);
            lex_endtoken(lex);
            if (lex->flags.noops)
                return (lex->tok.ttype = ch);
            else
                return (lex->tok.ttype = TOKEN_OPERATOR);
        }
        /* peak ahead again */
        nextch = lex_getch(lex);
        if (nextch != '.') {
            lex_ungetch(lex, nextch);
            lex_ungetch(lex, '.');
            lex_endtoken(lex);
            if (lex->flags.noops)
                return (lex->tok.ttype = ch);
            else
                return (lex->tok.ttype = TOKEN_OPERATOR);
        }
        /* fill the token to be "..." */
        lex_tokench(lex, ch);
        lex_tokench(lex, ch);
        lex_endtoken(lex);
        return (lex->tok.ttype = TOKEN_DOTS);
    }

    if (ch == ',' || ch == '.') {
        lex_tokench(lex, ch);
        lex_endtoken(lex);
        return (lex->tok.ttype = TOKEN_OPERATOR);
    }

    if (ch == '+' || ch == '-' || /* ++, --, +=, -=  and -> as well! */
        ch == '>' || ch == '<' || /* <<, >>, <=, >=  and >< as well! */
        ch == '=' || ch == '!' || /* <=>, ==, !=                     */
        ch == '&' || ch == '|' || /* &&, ||, &=, |=                  */
        ch == '~' || ch == '^'    /* ~=, ~, ^                        */
    )  {
        lex_tokench(lex, ch);
        nextch = lex_getch(lex);

        if ((nextch == '=' && ch != '<') || (nextch == '<' && ch == '>'))
            lex_tokench(lex, nextch);
        else if (nextch == ch && ch != '!') {
            lex_tokench(lex, nextch);
            if ((thirdch = lex_getch(lex)) == '=')
                lex_tokench(lex, thirdch);
            else
                lex_ungetch(lex, thirdch);
        } else if (ch == '<' && nextch == '=') {
            lex_tokench(lex, nextch);
            if ((thirdch = lex_getch(lex)) == '>')
                lex_tokench(lex, thirdch);
            else
                lex_ungetch(lex, thirdch);

        } else if (ch == '-' && nextch == '>') {
            lex_tokench(lex, nextch);
        } else if (ch == '&' && nextch == '~') {
            thirdch = lex_getch(lex);
            if (thirdch != '=') {
                lex_ungetch(lex, thirdch);
                lex_ungetch(lex, nextch);
            }
            else {
                lex_tokench(lex, nextch);
                lex_tokench(lex, thirdch);
            }
        }
        else if (lex->flags.preprocessing &&
                 ch == '-' && util_isdigit(nextch))
        {
            lex->tok.ttype = lex_finish_digit(lex, nextch);
            if (lex->tok.ttype == TOKEN_INTCONST)
                lex->tok.constval.i = -lex->tok.constval.i;
            else
                lex->tok.constval.f = -lex->tok.constval.f;
            lex_endtoken(lex);
            return lex->tok.ttype;
        } else {
            lex_ungetch(lex, nextch);
        }

        lex_endtoken(lex);
        return (lex->tok.ttype = TOKEN_OPERATOR);
    }

    if (ch == '*' || ch == '/') /* *=, /= */
    {
        lex_tokench(lex, ch);

        nextch = lex_getch(lex);
        if (nextch == '=' || nextch == '*') {
            lex_tokench(lex, nextch);
        } else
            lex_ungetch(lex, nextch);

        lex_endtoken(lex);
        return (lex->tok.ttype = TOKEN_OPERATOR);
    }

    if (ch == '%') {
        lex_tokench(lex, ch);
        lex_endtoken(lex);
        return (lex->tok.ttype = TOKEN_OPERATOR);
    }

    if (isident_start(ch))
    {
        const char *v;

        lex_tokench(lex, ch);
        if (!lex_finish_ident(lex)) {
            /* error? */
            return (lex->tok.ttype = TOKEN_ERROR);
        }
        lex_endtoken(lex);
        lex->tok.ttype = TOKEN_IDENT;

        v = lex->tok.value;
        if (!strcmp(v, "void")) {
            lex->tok.ttype = TOKEN_TYPENAME;
            lex->tok.constval.t = TYPE_VOID;
        } else if (!strcmp(v, "int")) {
            lex->tok.ttype = TOKEN_TYPENAME;
            lex->tok.constval.t = TYPE_INTEGER;
        } else if (!strcmp(v, "float")) {
            lex->tok.ttype = TOKEN_TYPENAME;
            lex->tok.constval.t = TYPE_FLOAT;
        } else if (!strcmp(v, "string")) {
            lex->tok.ttype = TOKEN_TYPENAME;
            lex->tok.constval.t = TYPE_STRING;
        } else if (!strcmp(v, "entity")) {
            lex->tok.ttype = TOKEN_TYPENAME;
            lex->tok.constval.t = TYPE_ENTITY;
        } else if (!strcmp(v, "vector")) {
            lex->tok.ttype = TOKEN_TYPENAME;
            lex->tok.constval.t = TYPE_VECTOR;
        } else if (!strcmp(v, "_length")) {
            lex->tok.ttype = TOKEN_OPERATOR;
        } else {
            size_t kw;
            for (kw = 0; kw < GMQCC_ARRAY_COUNT(keywords_qc); ++kw) {
                if (!strcmp(v, keywords_qc[kw]))
                    return (lex->tok.ttype = TOKEN_KEYWORD);
            }
            if (OPTS_OPTION_U32(OPTION_STANDARD) != COMPILER_QCC) {
                for (kw = 0; kw < GMQCC_ARRAY_COUNT(keywords_fg); ++kw) {
                    if (!strcmp(v, keywords_fg[kw]))
                        return (lex->tok.ttype = TOKEN_KEYWORD);
                }
            }
        }

        return lex->tok.ttype;
    }

    if (ch == '"')
    {
        lex->flags.nodigraphs = true;
        if (lex->flags.preprocessing)
            lex_tokench(lex, ch);
        lex->tok.ttype = lex_finish_string(lex, '"');
        if (lex->flags.preprocessing)
            lex_tokench(lex, ch);
        while (!lex->flags.preprocessing && lex->tok.ttype == TOKEN_STRINGCONST)
        {
            /* Allow c style "string" "continuation" */
            ch = lex_skipwhite(lex, false);
            if (ch != '"') {
                lex_ungetch(lex, ch);
                break;
            }

            lex->tok.ttype = lex_finish_string(lex, '"');
        }
        lex->flags.nodigraphs = false;
        lex_endtoken(lex);
        return lex->tok.ttype;
    }

    if (ch == '\'')
    {
        /* we parse character constants like string,
         * but return TOKEN_CHARCONST, or a vector type if it fits...
         * Likewise actual unescaping has to be done by the parser.
         * The difference is we don't allow 'char' 'continuation'.
         */
        if (lex->flags.preprocessing)
            lex_tokench(lex, ch);
        lex->tok.ttype = lex_finish_string(lex, '\'');
        if (lex->flags.preprocessing)
            lex_tokench(lex, ch);
        lex_endtoken(lex);

        lex->tok.ttype = TOKEN_CHARCONST;

        /* It's a vector if we can successfully scan 3 floats */
        if (util_sscanf(lex->tok.value, " %f %f %f ",
                   &lex->tok.constval.v.x, &lex->tok.constval.v.y, &lex->tok.constval.v.z) == 3)

        {
             lex->tok.ttype = TOKEN_VECTORCONST;
        }
        else
        {
            if (!lex->flags.preprocessing && strlen(lex->tok.value) > 1) {
                utf8ch_t u8char;
                /* check for a valid utf8 character */
                if (!OPTS_FLAG(UTF8) || !utf8_to(&u8char, (const unsigned char *)lex->tok.value, 8)) {
                    if (lexwarn(lex, WARN_MULTIBYTE_CHARACTER,
                                ( OPTS_FLAG(UTF8) ? "invalid multibyte character sequence `%s`"
                                                  : "multibyte character: `%s`" ),
                                lex->tok.value))
                        return (lex->tok.ttype = TOKEN_ERROR);
                }
                else
                    lex->tok.constval.i = u8char;
            }
            else
                lex->tok.constval.i = lex->tok.value[0];
        }

        return lex->tok.ttype;
    }

    if (util_isdigit(ch))
    {
        lex->tok.ttype = lex_finish_digit(lex, ch);
        lex_endtoken(lex);
        return lex->tok.ttype;
    }

    if (lex->flags.preprocessing) {
        lex_tokench(lex, ch);
        lex_endtoken(lex);
        return (lex->tok.ttype = ch);
    }

    lexerror(lex, "unknown token: `%c`", ch);
    return (lex->tok.ttype = TOKEN_ERROR);
}
