#ifndef GML_LEX_HDR
#define GML_LEX_HDR
#include <stddef.h>
#include <setjmp.h>
#include "gml.h"

typedef enum {
    LEX_TOKEN_ERROR,
    LEX_TOKEN_EOF,
    LEX_TOKEN_IDENTIFIER,
    LEX_TOKEN_NUMBER,
    LEX_TOKEN_ATOM,
    LEX_TOKEN_STRING,
    LEX_TOKEN_LPAREN,    /* (      */
    LEX_TOKEN_RPAREN,    /* )      */
    LEX_TOKEN_LBRACE,    /* {      */
    LEX_TOKEN_RBRACE,    /* }      */
    LEX_TOKEN_LBRACKET,  /* [      */
    LEX_TOKEN_RBRACKET,  /* ]      */
    LEX_TOKEN_SEMICOLON, /* ;      */
    LEX_TOKEN_COMMA,     /* ,      */
    LEX_TOKEN_DOT,       /* .      */
    LEX_TOKEN_ASSIGN,    /* =      */
    LEX_TOKEN_PLUS,      /* +      */
    LEX_TOKEN_MINUS,     /* -      */
    LEX_TOKEN_MUL,       /* *      */
    LEX_TOKEN_DIV,       /* /      */
    LEX_TOKEN_MOD,       /* %      */
    LEX_TOKEN_EQUAL,     /* ==     */
    LEX_TOKEN_NEQUAL,    /* !=     */
    LEX_TOKEN_LESS,      /* <      */
    LEX_TOKEN_GREATER,   /* >      */
    LEX_TOKEN_LEQUAL,    /* <=     */
    LEX_TOKEN_GEQUAL,    /* >=     */
    LEX_TOKEN_OR,        /* ||     */
    LEX_TOKEN_AND,       /* &&     */
    LEX_TOKEN_BITAND,    /* &      */
    LEX_TOKEN_BITOR,     /* |      */
    LEX_TOKEN_BITLSHIFT, /* <<     */
    LEX_TOKEN_BITRSHIFT, /* >>     */
    LEX_TOKEN_BITNOT,    /* ~      */
    LEX_TOKEN_BITXOR,    /* ^      */
    LEX_TOKEN_NOT,       /* !      */
    LEX_TOKEN_ARROW,     /* =>     */
    LEX_TOKEN_VAR,       /* var    */
    LEX_TOKEN_IF,        /* if     */
    LEX_TOKEN_ELIF,      /* elif   */
    LEX_TOKEN_ELSE,      /* else   */
    LEX_TOKEN_WHILE,     /* while  */
    LEX_TOKEN_FN,        /* fn     */
    LEX_TOKEN_IS,        /* is     */
    LEX_TOKEN_IN,        /* in     */
    LEX_TOKEN_FOR ,      /* for    */
    LEX_TOKEN_RETURN     /* return */
} lex_token_class_t;

typedef struct {
    lex_token_class_t class;
    char             *string;
    gml_position_t    position;
} lex_token_t;

typedef struct {
    lex_token_t   *token;
    gml_position_t position;
    const char    *source;
    char           buffer[4096];
    size_t         index;
    size_t         size;
    jmp_buf        escape;
} lex_t;

lex_t *lex_create(const char *file, const char *source);
void lex_destroy(lex_t *lex);
lex_token_t *lex_run(lex_t *lex);
const char *lex_token_classname(lex_token_class_t class);
#endif
