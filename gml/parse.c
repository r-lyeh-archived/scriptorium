#include "parse.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

void gml_error(gml_position_t *position, const char *format, ...);

static ast_t *ast_class_create(ast_class_t class, gml_position_t position) {
    ast_t *ast    = malloc(sizeof(*ast));
    if (!ast)
        return NULL;

    ast->class    = class;
    ast->position = position;
    return ast;
}

static ast_t *ast_create(gml_position_t position) {
    return ast_class_create((ast_class_t)-1, position);
}

const char *ast_classname(ast_class_t class) {
    switch (class) {
        case AST_ARRAY:      return "array";
        case AST_ATOM:       return "atom";
        case AST_BINARY:     return "binary";
        case AST_CALL:       return "call";
        case AST_DECLFUN:    return "function decl";
        case AST_DECLVAR:    return "variable decl";
        case AST_IDENT:      return "identifier";
        case AST_IF:         return "if statement";
        case AST_IFCLAUSE:   return "if clause";
        case AST_LAMBDA:     return "lambda";
        case AST_NUMBER:     return "number";
        case AST_STRING:     return "string";
        case AST_SUBSCRIPT:  return "subscript";
        case AST_TABLE:      return "table";
        case AST_TABLEENTRY: return "table entry";
        case AST_TOPLEVEL:   return "top level";
        case AST_UNARY:      return "unary";
        case AST_WHILE:      return "while statement";
        case AST_FOR:        return "for statement";
    }
    return "unknown";
}

static lex_token_t *parse_token(parse_t *parse) {
    return parse->lex->token;
}

static char *parse_token_string(parse_t *parse) {
    return strdup(parse_token(parse)->string);
}

static gml_position_t *parse_position(parse_t *parse) {
    lex_token_t *token = parse_token(parse);
    return &token->position;
}

static void parse_skip(parse_t *parse) {
    lex_run(parse->lex);
}

static int parse_match(parse_t *parse, lex_token_class_t class) {
    return parse_token(parse)->class == class;
}

static int parse_matchskip(parse_t *parse, lex_token_class_t class) {
    if (parse_match(parse, class)) {
        parse_skip(parse);
        return 1;
    }
    return 0;
}

static void parse_expect(parse_t *parse, lex_token_class_t class) {
    if (parse_match(parse, class))
        return;
    gml_error(
        parse_position(parse),
        "Expected %s, got %s.",
        lex_token_classname(class),
        lex_token_classname(parse_token(parse)->class)
    );
    longjmp(parse->escape, 1);
}
static void parse_expectskip(parse_t *parse, lex_token_class_t class) {
    parse_expect(parse, class);
    parse_skip(parse);
}

parse_t *parse_create(const char *filename, const char *source) {
    parse_t *parse = malloc(sizeof(*parse));
    if (!parse)
        return NULL;
    if (!(parse->lex = lex_create(filename, source))) {
        free(parse);
        return NULL;
    }
    lex_run(parse->lex);
    return parse;
}

void ast_destroy(ast_t *ast) {
    if (!ast)
        return;
    list_iterator_t *it;
    switch (ast->class) {
        case AST_IDENT:
            free(ast->ident);
            break;
        case AST_ATOM:
            free(ast->atom);
            break;
        case AST_STRING:
            free(ast->string);
            break;
        case AST_ARRAY:
            it = list_iterator_create(ast->array);
            while (!list_iterator_end(it))
                ast_destroy(list_iterator_next(it));
            list_iterator_destroy(it);
            list_destroy(ast->array);
            break;
        case AST_TABLE:
            it = list_iterator_create(ast->table);
            while (!list_iterator_end(it))
                ast_destroy(list_iterator_next(it));
            list_iterator_destroy(it);
            list_destroy(ast->table);
            break;
        case AST_TABLEENTRY:
            ast_destroy(ast->dictentry.key);
            ast_destroy(ast->dictentry.expr);
            break;
        case AST_BINARY:
            ast_destroy(ast->binary.left);
            ast_destroy(ast->binary.right);
            break;
        case AST_UNARY:
            ast_destroy(ast->unary.expr);
            break;
        case AST_SUBSCRIPT:
            ast_destroy(ast->subscript.key);
            ast_destroy(ast->subscript.expr);
            break;
        case AST_LAMBDA:
            it = list_iterator_create(ast->lambda.formals);
            while (!list_iterator_end(it))
                free(list_iterator_next(it));
            list_iterator_destroy(it);
            list_destroy(ast->lambda.formals);
            it = list_iterator_create(ast->lambda.body);
            while (!list_iterator_end(it))
                ast_destroy(list_iterator_next(it));
            list_iterator_destroy(it);
            list_destroy(ast->lambda.body);
            break;
        case AST_CALL:
            ast_destroy(ast->call.callee);
            it = list_iterator_create(ast->call.args);
            while (!list_iterator_end(it))
                ast_destroy(list_iterator_next(it));
            list_iterator_destroy(it);
            list_destroy(ast->call.args);
            break;
        case AST_IF:
            it = list_iterator_create(ast->ifstmt);
            while (!list_iterator_end(it))
                ast_destroy(list_iterator_next(it));
            list_iterator_destroy(it);
            list_destroy(ast->ifstmt);
            break;
        case AST_IFCLAUSE:
            ast_destroy(ast->ifclause.condition);
            it = list_iterator_create(ast->ifclause.body);
            while (!list_iterator_end(it))
                ast_destroy(list_iterator_next(it));
            list_iterator_destroy(it);
            list_destroy(ast->ifclause.body);
            break;
        case AST_WHILE:
            ast_destroy(ast->whilestmt.condition);
            it = list_iterator_create(ast->whilestmt.body);
            while (!list_iterator_end(it))
                ast_destroy(list_iterator_next(it));
            list_iterator_destroy(it);
            list_destroy(ast->whilestmt.body);
            break;
        case AST_DECLVAR:
            free(ast->vardecl.name);
            ast_destroy(ast->vardecl.initializer);
            break;
        case AST_DECLFUN:
            free(ast->fundecl.name);
            it = list_iterator_create(ast->fundecl.impl.formals);
            while (!list_iterator_end(it))
                free(list_iterator_next(it));
            list_iterator_destroy(it);
            list_destroy(ast->fundecl.impl.formals);
            it = list_iterator_create(ast->fundecl.impl.body);
            while (!list_iterator_end(it))
                ast_destroy(list_iterator_next(it));
            list_iterator_destroy(it);
            list_destroy(ast->fundecl.impl.body);
            break;
        case AST_TOPLEVEL:
            it = list_iterator_create(ast->toplevel);
            while (!list_iterator_end(it))
                ast_destroy(list_iterator_next(it));
            list_iterator_destroy(it);
            list_destroy(ast->toplevel);
            break;
        case AST_FOR:
            it = list_iterator_create(ast->forstmt.impl.formals);
            while (!list_iterator_end(it))
                free(list_iterator_next(it));
            list_iterator_destroy(it);
            list_destroy(ast->forstmt.impl.formals);
            it = list_iterator_create(ast->forstmt.impl.body);
            while (!list_iterator_end(it))
                ast_destroy(list_iterator_next(it));
            list_iterator_destroy(it);
            list_destroy(ast->forstmt.impl.body);
            ast_destroy(ast->forstmt.expr);
            break;
        default:
            break;
    }
    free(ast);
}

void parse_destroy(parse_t *parse) {
    lex_destroy(parse->lex);
    free(parse);
}

static int parse_matchsimple(parse_t *parse) {
    return parse_match(parse, LEX_TOKEN_NUMBER) ||
           parse_match(parse, LEX_TOKEN_STRING) ||
           parse_match(parse, LEX_TOKEN_ATOM);
}

static int parse_matchliteral(parse_t *parse) {
    return parse_matchsimple(parse)               ||
           parse_match(parse, LEX_TOKEN_LBRACKET) ||
           parse_match(parse, LEX_TOKEN_LBRACE);
}

/* Parser */
static ast_t *parse_statement(parse_t *parse);
static ast_t *parse_expression(parse_t *parse);
static ast_t *parse_array(parse_t *parse);
static ast_t *parse_dict(parse_t *parse);
static ast_t *parse_literal(parse_t *parse);
static list_t *parse_block(parse_t *parse);
static list_t *parse_formals(parse_t *parse);

static int parse_precedence(lex_token_class_t class) {
    switch (class) {
        case LEX_TOKEN_ASSIGN:    return 0;

        case LEX_TOKEN_OR:        return 1;
        case LEX_TOKEN_AND:       return 2;

        case LEX_TOKEN_BITOR:     return 3;
        case LEX_TOKEN_BITXOR:    return 4;
        case LEX_TOKEN_BITAND:    return 5;

        case LEX_TOKEN_IS:        return 6;
        case LEX_TOKEN_EQUAL:     return 6;
        case LEX_TOKEN_NEQUAL:    return 6;

        case LEX_TOKEN_LESS:      return 7;
        case LEX_TOKEN_GREATER:   return 7;
        case LEX_TOKEN_LEQUAL:    return 7;
        case LEX_TOKEN_GEQUAL:    return 7;

        case LEX_TOKEN_BITLSHIFT: return 8;
        case LEX_TOKEN_BITRSHIFT: return 8;

        case LEX_TOKEN_MINUS:     return 9;
        case LEX_TOKEN_PLUS:      return 9;
        case LEX_TOKEN_MUL:       return 9;
        case LEX_TOKEN_DIV:       return 9;
        case LEX_TOKEN_MOD:       return 9;

        default:                  return -1;
    }
}
static ast_t *parse_subscript(parse_t *parse, ast_t *ast);
static ast_t *parse_subscript_sugar(parse_t *parse, ast_t *ast);
static ast_t *parse_call(parse_t *parse, ast_t *ast) {
    /* Function calls */
    list_t *args = list_create();
    while (!parse_match(parse, LEX_TOKEN_RPAREN)) {
        list_push(args, (void*)parse_expression(parse));
        if (!parse_matchskip(parse, LEX_TOKEN_COMMA))
            break;
    }
    parse_expectskip(parse, LEX_TOKEN_RPAREN);
    ast_t *call       = ast_class_create(AST_CALL, ast->position);
    call->call.callee = ast;
    call->call.args   = args;
    /* chaining calls */
    if (parse_matchskip(parse, LEX_TOKEN_LPAREN))
        return parse_call(parse, call);
    else if (parse_matchskip(parse, LEX_TOKEN_LBRACKET))
        return parse_subscript(parse, call);
    else if (parse_matchskip(parse, LEX_TOKEN_DOT))
        return parse_subscript_sugar(parse, call);
    return call;
}

static ast_t *parse_subscript(parse_t *parse, ast_t *ast) {
    ast_t *subscript = ast_class_create(AST_SUBSCRIPT, ast->position);
    subscript->subscript.expr = ast;
    subscript->subscript.key  = parse_expression(parse);
    parse_expectskip(parse, LEX_TOKEN_RBRACKET);
    if (parse_matchskip(parse, LEX_TOKEN_LPAREN))
        return parse_call(parse, subscript);
    else if (parse_matchskip(parse, LEX_TOKEN_LBRACKET))
        return parse_subscript(parse, subscript);
    else if (parse_matchskip(parse, LEX_TOKEN_DOT))
        return parse_subscript_sugar(parse, subscript);
    return subscript;
}
static ast_t *parse_subscript_sugar(parse_t *parse, ast_t *ast) {
    /* Dot syntax sugar */
    ast_t *subscript = ast_class_create(AST_SUBSCRIPT, ast->position);
    ast_t *key       = ast_class_create(AST_ATOM, *parse_position(parse));
    parse_expect(parse, LEX_TOKEN_IDENTIFIER);
    key->atom = parse_token_string(parse);
    parse_skip(parse);
    subscript->subscript.expr = ast;
    subscript->subscript.key  = key;
    if (parse_matchskip(parse, LEX_TOKEN_LPAREN))
        return parse_call(parse, subscript);
    else if (parse_matchskip(parse, LEX_TOKEN_LBRACKET))
        return parse_subscript(parse, subscript);
    else if (parse_matchskip(parse, LEX_TOKEN_DOT))
        return parse_subscript_sugar(parse, subscript);
    return subscript;
}
static ast_t *parse_lambda(parse_t *parse) {
    ast_t *ast = ast_class_create(AST_LAMBDA, *parse_position(parse));
    ast->lambda.formals = parse_formals(parse);
    ast->lambda.body    = parse_block(parse);
    return ast;
}
static ast_t *parse_expression_primary(parse_t *parse) {
    ast_t *ast = NULL;
    if (parse_matchliteral(parse)) {
        ast = parse_literal(parse);
    } else if (parse_match(parse, LEX_TOKEN_IDENTIFIER)) {
        ast        = ast_class_create(AST_IDENT, *parse_position(parse));
        ast->ident = parse_token_string(parse);
        parse_skip(parse);
    } else if (parse_matchskip(parse, LEX_TOKEN_FN)) {
        ast = parse_lambda(parse);
    } else if (parse_match(parse, LEX_TOKEN_MINUS) || parse_match(parse, LEX_TOKEN_PLUS)
           ||  parse_match(parse, LEX_TOKEN_NOT)   || parse_match(parse, LEX_TOKEN_BITNOT)) {
        /* Unary operations can be one of - + ! or ~ */
        lex_token_class_t op = parse_token(parse)->class;
        parse_skip(parse);
        ast                 = ast_class_create(AST_UNARY, *parse_position(parse));
        ast->unary.op       = op;
        ast->unary.expr     = parse_expression(parse);
    } else if (parse_matchskip(parse, LEX_TOKEN_LPAREN)) {
        ast = parse_expression(parse);
        parse_expectskip(parse, LEX_TOKEN_RPAREN);
    } else {
        gml_error(parse_position(parse), "Expected expression. (%s)",
            lex_token_classname(parse_token(parse)->class));
        longjmp(parse->escape, 1);
    }

    if (parse_matchskip(parse, LEX_TOKEN_LPAREN))
        return parse_call(parse, ast);
    else if (parse_matchskip(parse, LEX_TOKEN_LBRACKET))
        return parse_subscript(parse, ast);
    else if (parse_matchskip(parse, LEX_TOKEN_DOT))
        return parse_subscript_sugar(parse, ast);
    return ast;
}

static ast_t *parse_expression_last(parse_t *parse, ast_t *lhs, int minprec) {
    for (;;) {
        lex_token_class_t op = parse_token(parse)->class;
        int prec = parse_precedence(parse_token(parse)->class);
        if (prec < minprec)
            return lhs;

        parse_skip(parse);

        ast_t *rhs = parse_expression_primary(parse);
        int nextprec = parse_precedence(parse_token(parse)->class);
        if (prec < nextprec)
            rhs = parse_expression_last(parse, rhs, prec + 1);

        /* Constant folding */
        if (rhs->class == AST_NUMBER && lhs->class == AST_NUMBER) {
            switch (op) {
                case LEX_TOKEN_PLUS:
                    lhs->number += rhs->number;
                    continue;
                case LEX_TOKEN_MINUS:
                    lhs->number -= rhs->number;
                    continue;
                case LEX_TOKEN_MUL:
                    lhs->number *= rhs->number;
                    continue;
                case LEX_TOKEN_DIV:
                    lhs->number /= rhs->number;
                    continue;
                case LEX_TOKEN_MOD:
                    lhs->number = (uint32_t)lhs->number % (uint32_t)rhs->number;
                    continue;
                case LEX_TOKEN_BITAND:
                    lhs->number = (uint32_t)lhs->number & (uint32_t)rhs->number;
                    continue;
                case LEX_TOKEN_BITOR:
                    lhs->number = (uint32_t)lhs->number | (uint32_t)rhs->number;
                    continue;
                case LEX_TOKEN_BITLSHIFT:
                    lhs->number = (uint32_t)lhs->number << (uint32_t)rhs->number;
                    continue;
                case LEX_TOKEN_BITRSHIFT:
                    lhs->number = (uint32_t)lhs->number >> (uint32_t)rhs->number;
                    continue;
                case LEX_TOKEN_BITXOR:
                    lhs->number = (uint32_t)lhs->number ^ (uint32_t)rhs->number;
                    continue;
                case LEX_TOKEN_LESS:
                    lhs->number = !!(lhs->number < rhs->number);
                    continue;
                case LEX_TOKEN_GREATER:
                    lhs->number = !!(lhs->number > rhs->number);
                    continue;
                case LEX_TOKEN_LEQUAL:
                    lhs->number = !!(lhs->number <= rhs->number);
                    continue;
                case LEX_TOKEN_GEQUAL:
                    lhs->number = !!(lhs->number >= rhs->number);
                    continue;
                case LEX_TOKEN_EQUAL:
                    lhs->number = lhs->number == rhs->number;
                    continue;
                case LEX_TOKEN_NEQUAL:
                    lhs->number = lhs->number != rhs->number;
                    continue;
                default:
                    break;
            }
        }

        ast_t *newlhs        = ast_class_create(AST_BINARY, *parse_position(parse));
        newlhs->binary.left  = lhs;
        newlhs->binary.right = rhs;
        newlhs->binary.op    = op;
        lhs                  = newlhs;
    }
}

static ast_t *parse_expression(parse_t *parse) {
    ast_t *lhs = parse_expression_primary(parse);
    return parse_expression_last(parse, lhs, 0);
}
static list_t *parse_formals(parse_t *parse) {
    list_t *formals = list_create();
    parse_expectskip(parse, LEX_TOKEN_LPAREN);
    while (!parse_match(parse, LEX_TOKEN_RPAREN)) {
        parse_expect(parse, LEX_TOKEN_IDENTIFIER);
        list_push(formals, parse_token_string(parse));
        parse_skip(parse);
        if (!parse_matchskip(parse, LEX_TOKEN_COMMA))
            break;
    }
    parse_expectskip(parse, LEX_TOKEN_RPAREN);
    return formals;
}

static ast_t *parse_simpleliteral(parse_t *parse) {
    ast_t *ast = ast_create(*parse_position(parse));
    if (parse_match(parse, LEX_TOKEN_NUMBER)) {
        ast->class  = AST_NUMBER;
        ast->number = atof(parse_token(parse)->string);
        parse_skip(parse);
    } else if (parse_match(parse, LEX_TOKEN_ATOM)) {
        ast->class  = AST_ATOM;
        ast->atom   = parse_token_string(parse);
        parse_skip(parse);
    } else if (parse_match(parse, LEX_TOKEN_STRING)) {
        ast->class  = AST_STRING;
        ast->string = parse_token_string(parse);
        parse_skip(parse);
    } else {
        free(ast);
        gml_error(parse_position(parse), "Expected number, string or atom.");
        longjmp(parse->escape, 1);
    }
    return ast;
}

static ast_t *parse_literal(parse_t *parse) {
    if (parse_matchsimple(parse))
        return parse_simpleliteral(parse);
    else if (parse_match(parse, LEX_TOKEN_LBRACKET))
        return parse_array(parse);
    else if (parse_match(parse, LEX_TOKEN_LBRACE))
        return parse_dict(parse);
    return NULL;
}

static ast_t *parse_array(parse_t *parse) {
    ast_t *ast = ast_class_create(AST_ARRAY, *parse_position(parse));
    ast->array = list_create();
    parse_expectskip(parse, LEX_TOKEN_LBRACKET);
    while (!parse_match(parse, LEX_TOKEN_RBRACKET)) {
        list_push(ast->array, (void*)parse_expression(parse));
        if (!parse_matchskip(parse, LEX_TOKEN_COMMA))
            break;
    }
    parse_expectskip(parse, LEX_TOKEN_RBRACKET);
    return ast;
}

static ast_t *parse_dict(parse_t *parse) {
    ast_t *ast = ast_class_create(AST_TABLE, *parse_position(parse));
    ast->table = list_create();
    parse_expectskip(parse, LEX_TOKEN_LBRACE);
    while (!parse_match(parse, LEX_TOKEN_RBRACE)) {
        ast_t *entry = ast_class_create(AST_TABLEENTRY, *parse_position(parse));
        entry->dictentry.key = parse_simpleliteral(parse);
        parse_expectskip(parse, LEX_TOKEN_ASSIGN);
        entry->dictentry.expr = parse_expression(parse);
        list_push(ast->table, entry);
        if (!parse_matchskip(parse, LEX_TOKEN_COMMA))
            break;
    }
    parse_expectskip(parse, LEX_TOKEN_RBRACE);
    return ast;
}

static list_t *parse_block(parse_t *parse) {
    list_t *statements = list_create();
    if (parse_matchskip(parse, LEX_TOKEN_ARROW)) {
        list_push(statements, (void*)parse_statement(parse));
        return statements;
    }
    parse_expectskip(parse, LEX_TOKEN_LBRACE);
    while (!parse_match(parse, LEX_TOKEN_RBRACE))
        list_push(statements, (void*)parse_statement(parse));
    parse_expectskip(parse, LEX_TOKEN_RBRACE);
    return statements;
}

static ast_t *parse_decl_var(parse_t *parse) {
    gml_position_t position = *parse_position(parse);
    ast_t *ast = ast_class_create(AST_DECLVAR, position);

    /* var name */
    parse_expectskip(parse, LEX_TOKEN_VAR);
    parse_expect(parse, LEX_TOKEN_IDENTIFIER);
    ast->vardecl.name = parse_token_string(parse);
    parse_skip(parse);

    /* optional assignment */
    ast->vardecl.initializer = parse_matchskip(parse, LEX_TOKEN_ASSIGN)
        ? parse_expression(parse)
        : NULL;

    return ast;
}
static ast_t *parse_decl_fun(parse_t *parse) {
    gml_position_t position = *parse_position(parse);
    ast_t *ast;

    /* function name */
    parse_expectskip(parse, LEX_TOKEN_FN);
    /* no identifier means we're parsing a lambda */
    if (!parse_match(parse, LEX_TOKEN_IDENTIFIER)) {
        ast = parse_lambda(parse);
        parse_expectskip(parse, LEX_TOKEN_SEMICOLON);
        return ast;
    }
    ast = ast_class_create(AST_DECLFUN, position);
    ast->fundecl.name = parse_token_string(parse);
    parse_skip(parse);

    /* argument list */
    ast->fundecl.impl.formals = parse_formals(parse);
    ast->fundecl.impl.body    = parse_block(parse);
    return ast;
}

static ast_t *parse_if(parse_t *parse) {
    ast_t *ast = ast_class_create(AST_IF, *parse_position(parse));
    ast->ifstmt = list_create();
    parse_expect(parse, LEX_TOKEN_IF);

    while (parse_match(parse, LEX_TOKEN_IF) ||
           parse_match(parse, LEX_TOKEN_ELIF)) {
        ast_t *clause = ast_class_create(AST_IFCLAUSE, *parse_position(parse));
        parse_skip(parse);
        clause->ifclause.condition = parse_expression(parse);
        clause->ifclause.body      = parse_block(parse);
        list_push(ast->ifstmt, clause);
    }

    if (parse_match(parse, LEX_TOKEN_ELSE)) {
        ast_t *clause = ast_class_create(AST_IFCLAUSE, *parse_position(parse));
        parse_skip(parse);
        clause->ifclause.condition = NULL;
        clause->ifclause.body      = parse_block(parse);
        list_push(ast->ifstmt, clause);
    }

    return ast;
}
static ast_t *parse_while(parse_t *parse) {
    ast_t *ast = ast_class_create(AST_WHILE, *parse_position(parse));
    parse_expectskip(parse, LEX_TOKEN_WHILE);
    ast->whilestmt.condition = parse_expression(parse);
    ast->whilestmt.body      = parse_block(parse);
    return ast;
}
static ast_t *parse_for(parse_t *parse) {
    ast_t *ast = ast_class_create(AST_FOR, *parse_position(parse));
    parse_expectskip(parse, LEX_TOKEN_FOR);
    ast->forstmt.impl.formals = list_create();
    while (!parse_match(parse, LEX_TOKEN_IN)) {
        parse_expect(parse, LEX_TOKEN_IDENTIFIER);
        list_push(ast->forstmt.impl.formals, parse_token_string(parse));
        parse_skip(parse);
        if (!parse_matchskip(parse, LEX_TOKEN_COMMA))
            break;
    }
    parse_expectskip(parse, LEX_TOKEN_IN);
    ast->forstmt.expr      = parse_expression(parse);
    ast->forstmt.impl.body = parse_block(parse);
    return ast;
}

static ast_t *parse_statement(parse_t *parse) {
    ast_t *statement;
    if (parse_match(parse, LEX_TOKEN_VAR))
        statement = parse_decl_var(parse);
    else if (parse_match(parse, LEX_TOKEN_FN))
        return parse_decl_fun(parse);
    else if (parse_match(parse, LEX_TOKEN_IF))
        return parse_if(parse);
    else if (parse_match(parse, LEX_TOKEN_WHILE))
        return parse_while(parse);
    else if (parse_match(parse, LEX_TOKEN_FOR))
        return parse_for(parse);
    else
        statement = parse_expression(parse);
    parse_expectskip(parse, LEX_TOKEN_SEMICOLON);
    return statement;
}

static ast_t *parse_toplevel(parse_t *parse) {
    ast_t *ast = ast_class_create(AST_TOPLEVEL, *parse_position(parse));
    ast->toplevel = list_create();
    while (!parse_match(parse, LEX_TOKEN_EOF))
        list_push(ast->toplevel, parse_statement(parse));
    parse_expectskip(parse, LEX_TOKEN_EOF);
    return ast;
}

ast_t *parse_run(parse_t *parse) {
    return (setjmp(parse->escape) != 0)
               ? NULL
               : parse_toplevel(parse);
}
