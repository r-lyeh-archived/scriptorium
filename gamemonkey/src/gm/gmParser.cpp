
/*  A Bison parser, made from gmparser.y with Bison version GNU Bison version 1.24
  */

#define YYBISON 1  /* Identify Bison output.  */

#define yyparse gmparse
#define yylex gmlex
#define yyerror gmerror
#define yylval gmlval
#define yychar gmchar
#define yydebug gmdebug
#define yynerrs gmnerrs
#define	KEYWORD_LOCAL	258
#define	KEYWORD_GLOBAL	259
#define	KEYWORD_MEMBER	260
#define	KEYWORD_AND	261
#define	KEYWORD_OR	262
#define	KEYWORD_IF	263
#define	KEYWORD_ELSE	264
#define	KEYWORD_WHILE	265
#define	KEYWORD_FOR	266
#define	KEYWORD_FOREACH	267
#define	KEYWORD_IN	268
#define	KEYWORD_BREAK	269
#define	KEYWORD_CONTINUE	270
#define	KEYWORD_NULL	271
#define	KEYWORD_DOWHILE	272
#define	KEYWORD_RETURN	273
#define	KEYWORD_FUNCTION	274
#define	KEYWORD_TABLE	275
#define	KEYWORD_THIS	276
#define	KEYWORD_TRUE	277
#define	KEYWORD_FALSE	278
#define	KEYWORD_FORK	279
#define	IDENTIFIER	280
#define	CONSTANT_HEX	281
#define	CONSTANT_BINARY	282
#define	CONSTANT_INT	283
#define	CONSTANT_CHAR	284
#define	CONSTANT_FLOAT	285
#define	CONSTANT_STRING	286
#define	SYMBOL_ASGN_BSR	287
#define	SYMBOL_ASGN_BSL	288
#define	SYMBOL_ASGN_ADD	289
#define	SYMBOL_ASGN_MINUS	290
#define	SYMBOL_ASGN_TIMES	291
#define	SYMBOL_ASGN_DIVIDE	292
#define	SYMBOL_ASGN_REM	293
#define	SYMBOL_ASGN_BAND	294
#define	SYMBOL_ASGN_BOR	295
#define	SYMBOL_ASGN_BXOR	296
#define	SYMBOL_RIGHT_SHIFT	297
#define	SYMBOL_LEFT_SHIFT	298
#define	SYMBOL_INC	299
#define	SYMBOL_DEC	300
#define	SYMBOL_LTE	301
#define	SYMBOL_GTE	302
#define	SYMBOL_EQ	303
#define	SYMBOL_NEQ	304
#define	TOKEN_ERROR	305



#define YYPARSER
#include "gmConfig.h"
#include "gmCodeTree.h"
#define YYSTYPE gmCodeTreeNode *

extern gmCodeTreeNode * g_codeTree;

#define GM_BISON_DEBUG
#ifdef GM_BISON_DEBUG
#define YYDEBUG 1
#define YYERROR_VERBOSE
#endif // GM_BISON_DEBUG

//
// HELPERS
//

void ATTACH(gmCodeTreeNode * &a_res, gmCodeTreeNode * a_a, gmCodeTreeNode * a_b)
{
  YYSTYPE t = a_a;
  if(t != NULL)
  {
    while(t->m_sibling != NULL)
    {
      t = t->m_sibling;
    }
    t->m_sibling = a_b;
    if(a_b) { a_b->m_parent = t; }
    a_res = a_a;
  }
  else
  {
    a_res = a_b;
  }
}

gmCodeTreeNode * CreateOperation(int a_subTypeType, gmCodeTreeNode * a_left = NULL, gmCodeTreeNode * a_right = NULL)
{
  gmCodeTreeNode * node = gmCodeTreeNode::Create(CTNT_EXPRESSION, CTNET_OPERATION, gmlineno, a_subTypeType);
  node->SetChild(0, a_left);
  node->SetChild(1, a_right);
  return node;
}

gmCodeTreeNode * CreateAsignExpression(int a_subTypeType, gmCodeTreeNode * a_left, gmCodeTreeNode * a_right)
{
  // we need to evaluate the complexety of the l-value... if it is a function call, index or dot to the left of a dot or index, we need to cache
  // into a hidden variable.

  // todo

  gmCodeTreeNode * opNode = CreateOperation(a_subTypeType, a_left, a_right);
  return CreateOperation(CTNOT_ASSIGN, a_left, opNode);
}


#ifndef YYLTYPE
typedef
  struct yyltype
    {
      int timestamp;
      int first_line;
      int first_column;
      int last_line;
      int last_column;
      char *text;
   }
  yyltype;

#define YYLTYPE yyltype
#endif

#ifndef YYSTYPE
#define YYSTYPE int
#endif
#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		264
#define	YYFLAG		-32768
#define	YYNTBASE	74

#define YYTRANSLATE(x) ((unsigned)(x) <= 305 ? yytranslate[x] : 113)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,    69,     2,     2,     2,    67,    60,     2,    53,
    54,    65,    63,    73,    64,    55,    66,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,    72,    56,    61,
    57,    62,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
    70,     2,    71,    59,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,    51,    58,    52,    68,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     2,     3,     4,     5,
     6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
    26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
    36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    46,    47,    48,    49,    50
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     2,     4,     7,     9,    11,    13,    15,    17,    19,
    22,    26,    32,    39,    45,    52,    56,    60,    64,    70,
    77,    85,    87,    89,    91,    93,    96,   102,   110,   118,
   122,   125,   131,   137,   144,   152,   160,   170,   173,   176,
   179,   183,   185,   189,   193,   197,   201,   205,   209,   213,
   217,   221,   225,   229,   231,   234,   236,   238,   242,   244,
   248,   250,   254,   256,   260,   262,   266,   268,   272,   276,
   278,   282,   286,   290,   294,   296,   300,   304,   306,   310,
   314,   316,   320,   324,   328,   330,   333,   336,   339,   341,
   343,   345,   347,   349,   354,   358,   363,   369,   376,   380,
   382,   386,   390,   395,   398,   402,   407,   413,   418,   420,
   424,   426,   430,   432,   436,   438,   442,   444,   447,   449,
   451,   453,   455,   459,   461,   463,   465,   467,   469,   471,
   473,   475,   477,   479,   481
};

static const short yyrhs[] = {    75,
     0,    76,     0,    75,    76,     0,    82,     0,    80,     0,
    83,     0,    84,     0,    85,     0,    78,     0,    51,    52,
     0,    51,    75,    52,     0,    19,   110,    53,    54,    77,
     0,    19,   110,    53,   107,    54,    77,     0,    19,    79,
    53,    54,    77,     0,    19,    79,    53,   107,    54,    77,
     0,   110,    55,   110,     0,    79,    55,   110,     0,    81,
   110,    56,     0,    81,   110,    57,    88,    56,     0,    81,
    19,   110,    53,    54,    77,     0,    81,    19,   110,    53,
   107,    54,    77,     0,     3,     0,     4,     0,     5,     0,
    56,     0,    86,    56,     0,     8,    53,    88,    54,    77,
     0,     8,    53,    88,    54,    77,     9,    77,     0,     8,
    53,    88,    54,    77,     9,    83,     0,    24,   110,    77,
     0,    24,    77,     0,    10,    53,    88,    54,    77,     0,
    17,    53,    88,    54,    77,     0,    11,    53,    82,    87,
    54,    77,     0,    11,    53,    82,    87,    86,    54,    77,
     0,    12,    53,   110,    13,    88,    54,    77,     0,    12,
    53,   110,     6,   110,    13,    88,    54,    77,     0,    15,
    56,     0,    14,    56,     0,    18,    56,     0,    18,    88,
    56,     0,    89,     0,   101,    57,    89,     0,   101,    32,
    89,     0,   101,    33,    89,     0,   101,    34,    89,     0,
   101,    35,    89,     0,   101,    36,    89,     0,   101,    37,
    89,     0,   101,    38,    89,     0,   101,    39,    89,     0,
   101,    40,    89,     0,   101,    41,    89,     0,    56,     0,
    88,    56,     0,    89,     0,    90,     0,    89,     7,    90,
     0,    91,     0,    90,     6,    91,     0,    92,     0,    91,
    58,    92,     0,    93,     0,    92,    59,    93,     0,    94,
     0,    93,    60,    94,     0,    95,     0,    94,    48,    95,
     0,    94,    49,    95,     0,    96,     0,    95,    61,    96,
     0,    95,    62,    96,     0,    95,    46,    96,     0,    95,
    47,    96,     0,    97,     0,    96,    43,    97,     0,    96,
    42,    97,     0,    98,     0,    97,    63,    98,     0,    97,
    64,    98,     0,    99,     0,    98,    65,    99,     0,    98,
    66,    99,     0,    98,    67,    99,     0,   101,     0,    44,
    99,     0,    45,    99,     0,   100,    99,     0,    63,     0,
    64,     0,    68,     0,    69,     0,   109,     0,   101,    70,
    88,    71,     0,   101,    53,    54,     0,   101,    53,   102,
    54,     0,   101,    72,   110,    53,    54,     0,   101,    72,
   110,    53,   102,    54,     0,   101,    55,   110,     0,    88,
     0,   102,    73,    88,     0,    20,    53,    54,     0,    20,
    53,   105,    54,     0,    51,    52,     0,    51,   105,    52,
     0,    51,   105,    73,    52,     0,    19,    53,   107,    54,
    77,     0,    19,    53,    54,    77,     0,   106,     0,   105,
    73,   106,     0,    88,     0,   110,    57,    88,     0,   108,
     0,   107,    73,   108,     0,   110,     0,   110,    57,    88,
     0,   110,     0,    55,   110,     0,    21,     0,   111,     0,
   103,     0,   104,     0,    53,    88,    54,     0,    25,     0,
    26,     0,    27,     0,    28,     0,    22,     0,    23,     0,
    29,     0,    30,     0,   112,     0,    16,     0,    31,     0,
   112,    31,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
   125,   132,   136,   143,   147,   151,   155,   159,   163,   170,
   174,   183,   194,   206,   214,   226,   230,   237,   242,   248,
   257,   270,   274,   278,   284,   288,   295,   301,   308,   315,
   321,   329,   335,   341,   348,   356,   363,   374,   378,   382,
   386,   394,   402,   406,   410,   414,   418,   422,   426,   430,
   434,   438,   442,   450,   454,   461,   468,   472,   479,   483,
   490,   494,   502,   506,   514,   518,   526,   530,   534,   541,
   545,   549,   553,   557,   564,   568,   573,   581,   585,   590,
   598,   602,   607,   612,   620,   624,   628,   632,   641,   645,
   649,   653,   660,   664,   668,   673,   679,   685,   692,   699,
   703,   710,   714,   719,   723,   728,   736,   742,   750,   754,
   761,   765,   772,   776,   783,   788,   797,   801,   806,   810,
   814,   818,   822,   829,   838,   843,   848,   853,   858,   863,
   909,   914,   918,   926,   940
};

static const char * const yytname[] = {   "$","error","$undefined.","KEYWORD_LOCAL",
"KEYWORD_GLOBAL","KEYWORD_MEMBER","KEYWORD_AND","KEYWORD_OR","KEYWORD_IF","KEYWORD_ELSE",
"KEYWORD_WHILE","KEYWORD_FOR","KEYWORD_FOREACH","KEYWORD_IN","KEYWORD_BREAK",
"KEYWORD_CONTINUE","KEYWORD_NULL","KEYWORD_DOWHILE","KEYWORD_RETURN","KEYWORD_FUNCTION",
"KEYWORD_TABLE","KEYWORD_THIS","KEYWORD_TRUE","KEYWORD_FALSE","KEYWORD_FORK",
"IDENTIFIER","CONSTANT_HEX","CONSTANT_BINARY","CONSTANT_INT","CONSTANT_CHAR",
"CONSTANT_FLOAT","CONSTANT_STRING","SYMBOL_ASGN_BSR","SYMBOL_ASGN_BSL","SYMBOL_ASGN_ADD",
"SYMBOL_ASGN_MINUS","SYMBOL_ASGN_TIMES","SYMBOL_ASGN_DIVIDE","SYMBOL_ASGN_REM",
"SYMBOL_ASGN_BAND","SYMBOL_ASGN_BOR","SYMBOL_ASGN_BXOR","SYMBOL_RIGHT_SHIFT",
"SYMBOL_LEFT_SHIFT","SYMBOL_INC","SYMBOL_DEC","SYMBOL_LTE","SYMBOL_GTE","SYMBOL_EQ",
"SYMBOL_NEQ","TOKEN_ERROR","'{'","'}'","'('","')'","'.'","';'","'='","'|'","'^'",
"'&'","'<'","'>'","'+'","'-'","'*'","'/'","'%'","'~'","'!'","'['","']'","':'",
"','","program","statement_list","statement","compound_statement","function_statement",
"tablemember_expression","var_statement","var_type","expression_statement","selection_statement",
"iteration_statement","jump_statement","assignment_expression","constant_expression_statement",
"constant_expression","logical_or_expression","logical_and_expression","inclusive_or_expression",
"exclusive_or_expression","and_expression","equality_expression","relational_expression",
"shift_expression","additive_expression","multiplicative_expression","unary_expression",
"unary_operator","postfix_expression","argument_expression_list","table_constructor",
"function_constructor","field_list","field","parameter_list","parameter","primary_expression",
"identifier","constant","constant_string_list",""
};
#endif

static const short yyr1[] = {     0,
    74,    75,    75,    76,    76,    76,    76,    76,    76,    77,
    77,    78,    78,    78,    78,    79,    79,    80,    80,    80,
    80,    81,    81,    81,    82,    82,    83,    83,    83,    83,
    83,    84,    84,    84,    84,    84,    84,    85,    85,    85,
    85,    86,    86,    86,    86,    86,    86,    86,    86,    86,
    86,    86,    86,    87,    87,    88,    89,    89,    90,    90,
    91,    91,    92,    92,    93,    93,    94,    94,    94,    95,
    95,    95,    95,    95,    96,    96,    96,    97,    97,    97,
    98,    98,    98,    98,    99,    99,    99,    99,   100,   100,
   100,   100,   101,   101,   101,   101,   101,   101,   101,   102,
   102,   103,   103,   103,   103,   103,   104,   104,   105,   105,
   106,   106,   107,   107,   108,   108,   109,   109,   109,   109,
   109,   109,   109,   110,   111,   111,   111,   111,   111,   111,
   111,   111,   111,   112,   112
};

static const short yyr2[] = {     0,
     1,     1,     2,     1,     1,     1,     1,     1,     1,     2,
     3,     5,     6,     5,     6,     3,     3,     3,     5,     6,
     7,     1,     1,     1,     1,     2,     5,     7,     7,     3,
     2,     5,     5,     6,     7,     7,     9,     2,     2,     2,
     3,     1,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     1,     2,     1,     1,     3,     1,     3,
     1,     3,     1,     3,     1,     3,     1,     3,     3,     1,
     3,     3,     3,     3,     1,     3,     3,     1,     3,     3,
     1,     3,     3,     3,     1,     2,     2,     2,     1,     1,
     1,     1,     1,     4,     3,     4,     5,     6,     3,     1,
     3,     3,     4,     2,     3,     4,     5,     4,     1,     3,
     1,     3,     1,     3,     1,     3,     1,     2,     1,     1,
     1,     1,     3,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     2
};

static const short yydefact[] = {     0,
    22,    23,    24,     0,     0,     0,     0,     0,     0,   133,
     0,     0,     0,     0,   119,   128,   129,     0,   124,   125,
   126,   127,   130,   131,   134,     0,     0,     0,     0,     0,
    25,    89,    90,    91,    92,     1,     2,     9,     5,     0,
     4,     6,     7,     8,     0,    42,    57,    59,    61,    63,
    65,    67,    70,    75,    78,    81,     0,    85,   121,   122,
    93,   117,   120,   132,     0,     0,     0,     0,    39,    38,
     0,     0,    40,     0,    56,    85,     0,     0,     0,     0,
     0,    31,     0,    86,    87,   104,   111,     0,   109,   117,
     0,   118,     3,     0,     0,    26,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,    88,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   135,     0,     0,     0,     0,     0,    41,     0,     0,   113,
   115,     0,     0,     0,     0,   102,     0,    10,     0,    30,
   105,     0,     0,   123,     0,    18,     0,    58,    60,    62,
    64,    66,    68,    69,    73,    74,    71,    72,    77,    76,
    79,    80,    82,    83,    84,    44,    45,    46,    47,    48,
    49,    50,    51,    52,    53,    95,   100,     0,    99,    43,
     0,     0,     0,     0,    54,     0,     0,     0,     0,     0,
   108,     0,     0,     0,     0,     0,    17,     0,     0,    16,
   103,     0,    11,   106,   110,   112,     0,     0,    96,     0,
    94,     0,    27,    32,     0,     0,    55,     0,     0,    33,
   107,   114,   116,    14,     0,    12,     0,     0,     0,    19,
   101,    97,     0,     0,    34,     0,     0,     0,    15,    13,
    20,     0,    98,    28,    29,    35,     0,    36,    21,     0,
    37,     0,     0,     0
};

static const short yydefgoto[] = {   262,
    36,    37,    82,    38,    78,    39,    40,    41,    42,    43,
    44,    45,   196,    87,    75,    47,    48,    49,    50,    51,
    52,    53,    54,    55,    56,    57,    76,   188,    59,    60,
    88,    89,   139,   140,    61,    62,    63,    64
};

static const short yypact[] = {   394,
-32768,-32768,-32768,   -32,    -2,    11,    18,   -31,   -29,-32768,
    32,   445,    -5,    36,-32768,-32768,-32768,    -9,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,   904,   904,   496,   904,    54,
-32768,-32768,-32768,-32768,-32768,   394,-32768,-32768,-32768,    30,
-32768,-32768,-32768,-32768,    13,    50,    95,    55,    53,    60,
    48,    26,    56,    40,    28,-32768,   904,   145,-32768,-32768,
-32768,-32768,-32768,    85,   904,   904,   547,    54,-32768,-32768,
   904,    68,-32768,    67,    50,   -40,   -16,     7,    23,   598,
   270,-32768,    73,-32768,-32768,-32768,-32768,   -42,-32768,    69,
    74,-32768,-32768,    54,    49,-32768,   904,   904,   904,   904,
   904,   904,   904,   904,   904,   904,   904,   904,   904,   904,
   904,   904,   904,   904,-32768,   904,   904,   904,   904,   904,
   904,   904,   904,   904,   904,   649,    54,   904,   904,    54,
-32768,    75,    76,   700,    16,    77,-32768,    73,   -36,-32768,
    70,   -14,    54,   -13,    54,-32768,   -28,-32768,   332,-32768,
-32768,   751,   904,-32768,    79,-32768,   904,    95,    55,    53,
    60,    48,    26,    26,    56,    56,    56,    56,    40,    40,
    28,    28,-32768,-32768,-32768,    50,    50,    50,    50,    50,
    50,    50,    50,    50,    50,-32768,-32768,   -26,-32768,    50,
    63,    82,    73,    73,-32768,   802,    81,    54,   904,    73,
-32768,    73,    54,   904,    73,   -21,-32768,    73,     2,-32768,
-32768,   904,-32768,-32768,-32768,-32768,   -11,    83,-32768,   904,
-32768,   853,   129,-32768,    73,    86,-32768,   130,    88,-32768,
-32768,-32768,-32768,-32768,    73,-32768,    73,    73,     4,-32768,
-32768,-32768,     9,    -1,-32768,    73,   904,    73,-32768,-32768,
-32768,    73,-32768,-32768,-32768,-32768,    90,-32768,-32768,    73,
-32768,   146,   147,-32768
};

static const short yypgoto[] = {-32768,
    72,   -34,    19,-32768,-32768,-32768,-32768,    87,   -89,-32768,
-32768,   -37,-32768,   -12,    44,    59,    78,    71,    89,    93,
     5,   -39,     1,     8,   -22,-32768,     3,   -51,-32768,-32768,
    94,  -151,  -136,   -30,-32768,     6,-32768,-32768
};


#define	YYLAST		973


static const short yytable[] = {    74,
   215,    93,    58,    84,    85,   206,     4,   209,    19,   151,
    19,    19,   126,    19,   127,    19,    91,   202,    79,    19,
    65,   198,    18,    83,    69,   211,    70,   219,   199,   129,
   152,   130,   235,    90,   115,    92,   203,   138,    58,   205,
   208,    81,   238,    46,   212,    95,   220,    77,    94,    81,
    66,   203,   132,   133,    19,   237,    97,   252,   136,   142,
   215,   143,   253,    67,   165,   166,   167,   168,    96,    58,
    68,   104,   105,   135,   203,   144,   203,   145,    19,    46,
   239,   220,   141,    58,    71,    90,   106,   107,    80,   173,
   174,   175,   112,   113,   114,   102,   103,   108,   109,   155,
    98,   150,   110,   111,   156,   157,   163,   164,   169,   170,
    46,   100,    99,   187,    93,   131,   191,   171,   172,   101,
    77,   197,   137,    81,    46,   153,   204,   154,   193,   194,
   200,   217,   189,   221,   222,   192,   227,   244,   240,   246,
   216,   248,   247,   260,   218,   263,   264,   141,   207,   141,
   210,    58,   149,   134,   255,   158,   201,    90,   226,   176,
   177,   178,   179,   180,   181,   182,   183,   184,   185,   160,
   243,   190,   232,   147,     0,   159,   116,   117,   118,   119,
   120,   121,   122,   123,   124,   125,   229,     0,   161,     0,
     0,   233,    46,   162,     0,     0,     0,   126,    58,   127,
     0,   128,     0,   228,     0,     0,     0,   241,   141,   187,
     0,   223,   224,     0,   129,     0,   130,    90,   230,     0,
   231,     0,   141,   234,     0,     0,   236,     0,     0,     0,
     0,     0,     0,     0,   257,     0,     0,     0,     0,    46,
     0,     0,     0,   245,     0,     0,     0,     0,     0,     0,
     0,     0,     0,   249,     0,   250,   251,     0,     0,     0,
     0,     0,   254,     0,   256,     0,   258,     0,     0,     0,
   259,     0,     1,     2,     3,     0,     0,     4,   261,     5,
     6,     7,     0,     8,     9,    10,    11,    12,    13,    14,
    15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
    25,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,    26,    27,     0,     0,     0,     0,     0,
    28,   148,    29,     0,    30,    31,     0,     0,     0,     0,
     0,     0,    32,    33,     1,     2,     3,    34,    35,     4,
     0,     5,     6,     7,     0,     8,     9,    10,    11,    12,
    13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
    23,    24,    25,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,    26,    27,     0,     0,     0,
     0,     0,    28,   213,    29,     0,    30,    31,     0,     0,
     0,     0,     0,     0,    32,    33,     1,     2,     3,    34,
    35,     4,     0,     5,     6,     7,     0,     8,     9,    10,
    11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
    21,    22,    23,    24,    25,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,    26,    27,     0,
     0,     0,     0,     0,    28,     0,    29,     0,    30,    31,
     0,     0,     0,     0,     0,     0,    32,    33,     0,     0,
    10,    34,    35,    72,    14,    15,    16,    17,     0,    19,
    20,    21,    22,    23,    24,    25,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,    26,    27,
     0,     0,     0,     0,     0,    28,     0,    29,     0,    30,
    73,     0,     0,     0,     0,     0,     0,    32,    33,     0,
     0,    10,    34,    35,    72,    14,    15,    16,    17,     0,
    19,    20,    21,    22,    23,    24,    25,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,    26,
    27,     0,     0,     0,     0,     0,    28,    86,    29,     0,
    30,     0,     0,     0,     0,     0,     0,     0,    32,    33,
     0,     0,    10,    34,    35,    72,    14,    15,    16,    17,
     0,    19,    20,    21,    22,    23,    24,    25,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    26,    27,     0,     0,     0,     0,     0,    28,     0,    29,
     0,    30,    31,     0,     0,     0,     0,     0,     0,    32,
    33,     0,     0,    10,    34,    35,    72,    14,    15,    16,
    17,     0,    19,    20,    21,    22,    23,    24,    25,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,    26,    27,     0,     0,     0,     0,     0,    28,     0,
    29,   146,    30,     0,     0,     0,     0,     0,     0,     0,
    32,    33,     0,     0,    10,    34,    35,    72,    14,    15,
    16,    17,     0,    19,    20,    21,    22,    23,    24,    25,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,    26,    27,     0,     0,     0,     0,     0,    28,
     0,    29,   186,    30,     0,     0,     0,     0,     0,     0,
     0,    32,    33,     0,     0,    10,    34,    35,    72,    14,
    15,    16,    17,     0,    19,    20,    21,    22,    23,    24,
    25,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,    26,    27,     0,     0,     0,     0,     0,
    28,     0,    29,     0,    30,   195,     0,     0,     0,     0,
     0,     0,    32,    33,     0,     0,    10,    34,    35,    72,
    14,    15,    16,    17,     0,    19,    20,    21,    22,    23,
    24,    25,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,    26,    27,     0,     0,     0,     0,
     0,    28,   214,    29,     0,    30,     0,     0,     0,     0,
     0,     0,     0,    32,    33,     0,     0,    10,    34,    35,
    72,    14,    15,    16,    17,     0,    19,    20,    21,    22,
    23,    24,    25,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,    26,    27,     0,     0,     0,
     0,     0,    28,     0,    29,   225,    30,     0,     0,     0,
     0,     0,     0,     0,    32,    33,     0,     0,    10,    34,
    35,    72,    14,    15,    16,    17,     0,    19,    20,    21,
    22,    23,    24,    25,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,    26,    27,     0,     0,
     0,     0,     0,    28,     0,    29,   242,    30,     0,     0,
     0,     0,     0,     0,     0,    32,    33,     0,     0,    10,
    34,    35,    72,    14,    15,    16,    17,     0,    19,    20,
    21,    22,    23,    24,    25,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,    26,    27,     0,
     0,     0,     0,     0,    28,     0,    29,     0,    30,     0,
     0,     0,     0,     0,     0,     0,    32,    33,     0,     0,
     0,    34,    35
};

static const short yycheck[] = {    12,
   152,    36,     0,    26,    27,   142,     8,   144,    25,    52,
    25,    25,    53,    25,    55,    25,    29,    54,    13,    25,
    53,     6,    24,    18,    56,    54,    56,    54,    13,    70,
    73,    72,    54,    28,    57,    30,    73,    54,    36,    54,
    54,    51,    54,     0,    73,    40,    73,    53,    19,    51,
    53,    73,    65,    66,    25,    54,     7,    54,    71,    53,
   212,    55,    54,    53,   104,   105,   106,   107,    56,    67,
    53,    46,    47,    68,    73,    53,    73,    55,    25,    36,
   217,    73,    77,    81,    53,    80,    61,    62,    53,   112,
   113,   114,    65,    66,    67,    48,    49,    42,    43,    94,
     6,    83,    63,    64,    56,    57,   102,   103,   108,   109,
    67,    59,    58,   126,   149,    31,   129,   110,   111,    60,
    53,   134,    56,    51,    81,    57,    57,    54,    54,    54,
    54,    53,   127,    71,    53,   130,    56,     9,    56,    54,
   153,    54,    13,    54,   157,     0,     0,   142,   143,   144,
   145,   149,    81,    67,   244,    97,   138,   152,   196,   116,
   117,   118,   119,   120,   121,   122,   123,   124,   125,    99,
   222,   128,   203,    80,    -1,    98,    32,    33,    34,    35,
    36,    37,    38,    39,    40,    41,   199,    -1,   100,    -1,
    -1,   204,   149,   101,    -1,    -1,    -1,    53,   196,    55,
    -1,    57,    -1,   198,    -1,    -1,    -1,   220,   203,   222,
    -1,   193,   194,    -1,    70,    -1,    72,   212,   200,    -1,
   202,    -1,   217,   205,    -1,    -1,   208,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,   247,    -1,    -1,    -1,    -1,   196,
    -1,    -1,    -1,   225,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,   235,    -1,   237,   238,    -1,    -1,    -1,
    -1,    -1,   244,    -1,   246,    -1,   248,    -1,    -1,    -1,
   252,    -1,     3,     4,     5,    -1,    -1,     8,   260,    10,
    11,    12,    -1,    14,    15,    16,    17,    18,    19,    20,
    21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
    31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    44,    45,    -1,    -1,    -1,    -1,    -1,
    51,    52,    53,    -1,    55,    56,    -1,    -1,    -1,    -1,
    -1,    -1,    63,    64,     3,     4,     5,    68,    69,     8,
    -1,    10,    11,    12,    -1,    14,    15,    16,    17,    18,
    19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
    29,    30,    31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    44,    45,    -1,    -1,    -1,
    -1,    -1,    51,    52,    53,    -1,    55,    56,    -1,    -1,
    -1,    -1,    -1,    -1,    63,    64,     3,     4,     5,    68,
    69,     8,    -1,    10,    11,    12,    -1,    14,    15,    16,
    17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
    27,    28,    29,    30,    31,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    44,    45,    -1,
    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    55,    56,
    -1,    -1,    -1,    -1,    -1,    -1,    63,    64,    -1,    -1,
    16,    68,    69,    19,    20,    21,    22,    23,    -1,    25,
    26,    27,    28,    29,    30,    31,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    44,    45,
    -1,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    55,
    56,    -1,    -1,    -1,    -1,    -1,    -1,    63,    64,    -1,
    -1,    16,    68,    69,    19,    20,    21,    22,    23,    -1,
    25,    26,    27,    28,    29,    30,    31,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    44,
    45,    -1,    -1,    -1,    -1,    -1,    51,    52,    53,    -1,
    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    63,    64,
    -1,    -1,    16,    68,    69,    19,    20,    21,    22,    23,
    -1,    25,    26,    27,    28,    29,    30,    31,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    44,    45,    -1,    -1,    -1,    -1,    -1,    51,    -1,    53,
    -1,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    63,
    64,    -1,    -1,    16,    68,    69,    19,    20,    21,    22,
    23,    -1,    25,    26,    27,    28,    29,    30,    31,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    44,    45,    -1,    -1,    -1,    -1,    -1,    51,    -1,
    53,    54,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    63,    64,    -1,    -1,    16,    68,    69,    19,    20,    21,
    22,    23,    -1,    25,    26,    27,    28,    29,    30,    31,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    44,    45,    -1,    -1,    -1,    -1,    -1,    51,
    -1,    53,    54,    55,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    63,    64,    -1,    -1,    16,    68,    69,    19,    20,
    21,    22,    23,    -1,    25,    26,    27,    28,    29,    30,
    31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    44,    45,    -1,    -1,    -1,    -1,    -1,
    51,    -1,    53,    -1,    55,    56,    -1,    -1,    -1,    -1,
    -1,    -1,    63,    64,    -1,    -1,    16,    68,    69,    19,
    20,    21,    22,    23,    -1,    25,    26,    27,    28,    29,
    30,    31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    44,    45,    -1,    -1,    -1,    -1,
    -1,    51,    52,    53,    -1,    55,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    63,    64,    -1,    -1,    16,    68,    69,
    19,    20,    21,    22,    23,    -1,    25,    26,    27,    28,
    29,    30,    31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    44,    45,    -1,    -1,    -1,
    -1,    -1,    51,    -1,    53,    54,    55,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    63,    64,    -1,    -1,    16,    68,
    69,    19,    20,    21,    22,    23,    -1,    25,    26,    27,
    28,    29,    30,    31,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    44,    45,    -1,    -1,
    -1,    -1,    -1,    51,    -1,    53,    54,    55,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    63,    64,    -1,    -1,    16,
    68,    69,    19,    20,    21,    22,    23,    -1,    25,    26,
    27,    28,    29,    30,    31,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    44,    45,    -1,
    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    55,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    63,    64,    -1,    -1,
    -1,    68,    69
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */


/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

#ifndef alloca
#ifdef __GNUC__
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi)
#include <alloca.h>
#else /* not sparc */
#if defined (MSDOS) && !defined (__TURBOC__)
#include <malloc.h>
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
#include <malloc.h>
 #pragma alloca
#else /* not MSDOS, __TURBOC__, or _AIX */
#ifdef __hpux
#ifdef __cplusplus
extern "C" {
void *alloca (unsigned int);
};
#else /* not __cplusplus */
void *alloca ();
#endif /* not __cplusplus */
#endif /* __hpux */
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc.  */
#endif /* not GNU C.  */
#endif /* alloca not defined.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         -2
#define YYEOF           0
#define YYACCEPT        return(0)
#define YYABORT         return(1)
#define YYERROR         goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL          goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do                                                              \
  if (yychar == YYEMPTY && yylen == 1)                          \
    { yychar = (token), yylval = (value);                       \
      yychar1 = YYTRANSLATE (yychar);                           \
      YYPOPSTACK;                                               \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    { yyerror ("syntax error: cannot back up"); YYERROR; }      \
while (0)

#define YYTERROR        1
#define YYERRCODE       256

#ifndef YYPURE
#define YYLEX           yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#ifdef YYLEX_PARAM
#define YYLEX           yylex(&yylval, &yylloc, YYLEX_PARAM)
#else
#define YYLEX           yylex(&yylval, &yylloc)
#endif
#else /* not YYLSP_NEEDED */
#ifdef YYLEX_PARAM
#define YYLEX           yylex(&yylval, YYLEX_PARAM)
#else
#define YYLEX           yylex(&yylval)
#endif
#endif /* not YYLSP_NEEDED */
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int     yychar;                 /*  the lookahead symbol                */
YYSTYPE yylval;                 /*  the semantic value of the           */
                                /*  lookahead symbol                    */

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;                 /*  location data for the lookahead     */
                                /*  symbol                              */
#endif

int yynerrs;                    /*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int yydebug;                    /*  nonzero means print parse trace     */
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks       */

#ifndef YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
int yyparse (void);
#endif

#if __GNUC__ > 1                /* GNU C and GNU C++ define this.  */
#define __yy_memcpy(FROM,TO,COUNT)      __builtin_memcpy(TO,FROM,COUNT)
#else                           /* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (from, to, count)
     char *from;
     char *to;
     int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (char *from, char *to, int count)
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif



/* The user can define YYPARSE_PARAM as the name of an argument to be passed
   into yyparse.  The argument should have type void *.
   It should actually point to an object.
   Grammar actions can access the variable by casting it
   to the proper pointer type.  */

#ifdef YYPARSE_PARAM
#define YYPARSE_PARAM_DECL void *YYPARSE_PARAM;
#else
#define YYPARSE_PARAM
#define YYPARSE_PARAM_DECL
#endif

int
yyparse(YYPARSE_PARAM)
     YYPARSE_PARAM_DECL
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;      /*  number of tokens to shift before error messages enabled */
  int yychar1 = 0;              /*  lookahead token as an internal (translated) token number */

  short yyssa[YYINITDEPTH];     /*  the state stack                     */
  YYSTYPE yyvsa[YYINITDEPTH];   /*  the semantic value stack            */

  short *yyss = yyssa;          /*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;        /*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE yylsa[YYINITDEPTH];   /*  the location stack                  */
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;

#define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  int yystacksize = YYINITDEPTH;

#ifdef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;                /*  the variable used to return         */
                                /*  semantic values from the action     */
                                /*  routines                            */

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;             /* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = (short) yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = (int)(yyssp - yyss + 1); // _GD_ cast for 64bit build

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
         the data in use in that stack, in bytes.  */
#ifdef YYLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
         but that might be undefined if yyoverflow is a macro.  */
      yyoverflow("parser stack overflow",
                 &yyss1, size * sizeof (*yyssp),
                 &yyvs1, size * sizeof (*yyvsp),
                 &yyls1, size * sizeof (*yylsp),
                 &yystacksize);
#else
      yyoverflow("parser stack overflow",
                 &yyss1, size * sizeof (*yyssp),
                 &yyvs1, size * sizeof (*yyvsp),
                 &yystacksize);
#endif

      yyss = yyss1; yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
        {
          yyerror("parser stack overflow");
          return 2;
        }
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
        yystacksize = YYMAXDEPTH;
      yyss = (short *) alloca (yystacksize * sizeof (*yyssp));
      __yy_memcpy ((char *)yyss1, (char *)yyss, size * sizeof (*yyssp));
      yyvs = (YYSTYPE *) alloca (yystacksize * sizeof (*yyvsp));
      __yy_memcpy ((char *)yyvs1, (char *)yyvs, size * sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) alloca (yystacksize * sizeof (*yylsp));
      __yy_memcpy ((char *)yyls1, (char *)yyls, size * sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
        fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
        YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

  goto yybackup;
 yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
        fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)              /* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;           /* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
        fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
      if (yydebug)
        {
          fprintf (stderr, "Next token is %d (%s", yychar, yytname[yychar1]);
          /* Give the individual parser a way to print the precise meaning
             of a token, for further debugging info.  */
#ifdef YYPRINT
          YYPRINT (stderr, yychar, yylval);
#endif
          fprintf (stderr, ")\n");
        }
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus) yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  if (yylen > 0)
    yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
               yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
        fprintf (stderr, "%s ", yytname[yyrhs[i]]);
      fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif


  switch (yyn) {

case 1:
{
      g_codeTree = yyvsp[0];
    ;
    break;}
case 2:
{
      yyval = yyvsp[0];
    ;
    break;}
case 3:
{
      ATTACH(yyval, yyvsp[-1], yyvsp[0]);
    ;
    break;}
case 4:
{
      yyval = yyvsp[0];
    ;
    break;}
case 5:
{
      yyval = yyvsp[0];
    ;
    break;}
case 6:
{
      yyval = yyvsp[0];
    ;
    break;}
case 7:
{
      yyval = yyvsp[0];
    ;
    break;}
case 8:
{
      yyval = yyvsp[0];
    ;
    break;}
case 9:
{
	  yyval = yyvsp[0];
	;
    break;}
case 10:
{
      yyval = gmCodeTreeNode::Create(CTNT_STATEMENT, CTNST_COMPOUND, gmlineno);
    ;
    break;}
case 11:
{
      yyval = gmCodeTreeNode::Create(CTNT_STATEMENT, CTNST_COMPOUND, gmlineno);
      yyval->SetChild(0, yyvsp[-1]);
    ;
    break;}
case 12:
{
      gmCodeTreeNode* func = gmCodeTreeNode::Create(CTNT_EXPRESSION, CTNET_FUNCTION, gmlineno);
      func->SetChild(1, yyvsp[0]);
      

      yyval = gmCodeTreeNode::Create(CTNT_DECLARATION, CTNDT_VARIABLE, gmlineno, (int)GMMACHINE_DEFAULT_FUNCTION);
      yyval->SetChild(0, yyvsp[-3]);
      ATTACH(yyval, yyval, CreateOperation(CTNOT_ASSIGN, yyvsp[-3], func));
   ;
    break;}
case 13:
{
      gmCodeTreeNode* func = gmCodeTreeNode::Create(CTNT_EXPRESSION, CTNET_FUNCTION, gmlineno);
      func->SetChild(0, yyvsp[-2]);
      func->SetChild(1, yyvsp[0]);
      

      yyval = gmCodeTreeNode::Create(CTNT_DECLARATION, CTNDT_VARIABLE, gmlineno, (int)GMMACHINE_DEFAULT_FUNCTION);
      yyval->SetChild(0, yyvsp[-4]);
      ATTACH(yyval, yyval, CreateOperation(CTNOT_ASSIGN, yyvsp[-4], func));
   ;
    break;}
case 14:
{
      gmCodeTreeNode* func = gmCodeTreeNode::Create(CTNT_EXPRESSION, CTNET_FUNCTION, gmlineno);
      func->SetChild(1, yyvsp[0]);
      
      ATTACH(yyval, yyval, CreateOperation(CTNOT_ASSIGN, yyvsp[-3], func));
   ;
    break;}
case 15:
{
      gmCodeTreeNode* func = gmCodeTreeNode::Create(CTNT_EXPRESSION, CTNET_FUNCTION, gmlineno);
      func->SetChild(0, yyvsp[-2]);
      func->SetChild(1, yyvsp[0]);
      
      ATTACH(yyval, yyval, CreateOperation(CTNOT_ASSIGN, yyvsp[-4], func));
   ;
    break;}
case 16:
{
      yyval = CreateOperation(CTNOT_DOT, yyvsp[-2], yyvsp[0]);
    ;
    break;}
case 17:
{
      yyval = CreateOperation(CTNOT_DOT, yyvsp[-2], yyvsp[0]);
    ;
    break;}
case 18:
{
      yyval = gmCodeTreeNode::Create(CTNT_DECLARATION, CTNDT_VARIABLE, gmlineno, (int) yyvsp[-2]);
      yyval->SetChild(0, yyvsp[-1]);
    ;
    break;}
case 19:
{
      yyval = gmCodeTreeNode::Create(CTNT_DECLARATION, CTNDT_VARIABLE, gmlineno, (int) yyvsp[-4]);
      yyval->SetChild(0, yyvsp[-3]);
      ATTACH(yyval, yyval, CreateOperation(CTNOT_ASSIGN, yyvsp[-3], yyvsp[-1]));
    ;
    break;}
case 20:
{
      gmCodeTreeNode* func = gmCodeTreeNode::Create(CTNT_EXPRESSION, CTNET_FUNCTION, gmlineno);
      func->SetChild(1, yyvsp[0]);

      yyval = gmCodeTreeNode::Create(CTNT_DECLARATION, CTNDT_VARIABLE, gmlineno, (int) yyvsp[-5]);
      yyval->SetChild(0, yyvsp[-3]);
      ATTACH(yyval, yyval, CreateOperation(CTNOT_ASSIGN, yyvsp[-3], func));
    ;
    break;}
case 21:
{
      gmCodeTreeNode* func = gmCodeTreeNode::Create(CTNT_EXPRESSION, CTNET_FUNCTION, gmlineno);
      func->SetChild(0, yyvsp[-2]);
      func->SetChild(1, yyvsp[0]);

      yyval = gmCodeTreeNode::Create(CTNT_DECLARATION, CTNDT_VARIABLE, gmlineno, (int) yyvsp[-6]);
      yyval->SetChild(0, yyvsp[-4]);
      ATTACH(yyval, yyval, CreateOperation(CTNOT_ASSIGN, yyvsp[-4], func));
    ;
    break;}
case 22:
{
      yyval = (YYSTYPE) CTVT_LOCAL;
    ;
    break;}
case 23:
{
      yyval = (YYSTYPE) CTVT_GLOBAL;
    ;
    break;}
case 24:
{
      yyval = (YYSTYPE) CTVT_MEMBER;
    ;
    break;}
case 25:
{
      yyval = NULL;
    ;
    break;}
case 26:
{
      yyval = yyvsp[-1];
    ;
    break;}
case 27:
{
      yyval = gmCodeTreeNode::Create(CTNT_STATEMENT, CTNST_IF, (yyvsp[-2]) ? yyvsp[-2]->m_lineNumber : gmlineno);
      yyval->SetChild(0, yyvsp[-2]);
      yyval->SetChild(1, yyvsp[0]);
    ;
    break;}
case 28:
{
      yyval = gmCodeTreeNode::Create(CTNT_STATEMENT, CTNST_IF, (yyvsp[-4]) ? yyvsp[-4]->m_lineNumber : gmlineno);
      yyval->SetChild(0, yyvsp[-4]);
      yyval->SetChild(1, yyvsp[-2]);
      yyval->SetChild(2, yyvsp[0]);
    ;
    break;}
case 29:
{
      yyval = gmCodeTreeNode::Create(CTNT_STATEMENT, CTNST_IF, (yyvsp[-4]) ? yyvsp[-4]->m_lineNumber : gmlineno);
      yyval->SetChild(0, yyvsp[-4]);
      yyval->SetChild(1, yyvsp[-2]);
      yyval->SetChild(2, yyvsp[0]);
    ;
    break;}
case 30:
{
      yyval = gmCodeTreeNode::Create(CTNT_STATEMENT, CTNST_FORK, (yyvsp[-1]) ? yyvsp[-1]->m_lineNumber : gmlineno );
      yyval->SetChild(0, yyvsp[0] );
      yyval->SetChild(1, yyvsp[-1] );
    ;
    break;}
case 31:
{
      yyval = gmCodeTreeNode::Create(CTNT_STATEMENT, CTNST_FORK, (yyvsp[0]) ? yyvsp[0]->m_lineNumber : gmlineno );
      yyval->SetChild(0, yyvsp[0] );
    ;
    break;}
case 32:
{
      yyval = gmCodeTreeNode::Create(CTNT_STATEMENT, CTNST_WHILE, (yyvsp[-2]) ? yyvsp[-2]->m_lineNumber : gmlineno);
      yyval->SetChild(0, yyvsp[-2]);
      yyval->SetChild(1, yyvsp[0]);
    ;
    break;}
case 33:
{
      yyval = gmCodeTreeNode::Create(CTNT_STATEMENT, CTNST_DOWHILE, (yyvsp[-2]) ? yyvsp[-2]->m_lineNumber : gmlineno);
      yyval->SetChild(0, yyvsp[-2]);
      yyval->SetChild(1, yyvsp[0]);
    ;
    break;}
case 34:
{
      yyval = gmCodeTreeNode::Create(CTNT_STATEMENT, CTNST_FOR, (yyvsp[-3]) ? yyvsp[-3]->m_lineNumber : gmlineno);
      yyval->SetChild(0, yyvsp[-3]);
      yyval->SetChild(1, yyvsp[-2]);
      yyval->SetChild(3, yyvsp[0]);
    ;
    break;}
case 35:
{
      yyval = gmCodeTreeNode::Create(CTNT_STATEMENT, CTNST_FOR, (yyvsp[-4]) ? yyvsp[-4]->m_lineNumber : gmlineno);
      yyval->SetChild(0, yyvsp[-4]);
      yyval->SetChild(1, yyvsp[-3]);
      yyval->SetChild(2, yyvsp[-2]);
      yyval->SetChild(3, yyvsp[0]);
    ;
    break;}
case 36:
{
      yyval = gmCodeTreeNode::Create(CTNT_STATEMENT, CTNST_FOREACH, (yyvsp[-2]) ? yyvsp[-2]->m_lineNumber : gmlineno);
      yyval->SetChild(0, yyvsp[-2]);
      yyval->SetChild(1, yyvsp[-4]);
      yyval->SetChild(3, yyvsp[0]);
    ;
    break;}
case 37:
{
      yyval = gmCodeTreeNode::Create(CTNT_STATEMENT, CTNST_FOREACH, (yyvsp[-2]) ? yyvsp[-2]->m_lineNumber : gmlineno);
      yyval->SetChild(0, yyvsp[-2]);
      yyval->SetChild(1, yyvsp[-4]);
      yyval->SetChild(2, yyvsp[-6]);
      yyval->SetChild(3, yyvsp[0]);
    ;
    break;}
case 38:
{
      yyval = gmCodeTreeNode::Create(CTNT_STATEMENT, CTNST_CONTINUE, gmlineno);
    ;
    break;}
case 39:
{
      yyval = gmCodeTreeNode::Create(CTNT_STATEMENT, CTNST_BREAK, gmlineno);
    ;
    break;}
case 40:
{
      yyval = gmCodeTreeNode::Create(CTNT_STATEMENT, CTNST_RETURN, gmlineno);
    ;
    break;}
case 41:
{
      yyval = gmCodeTreeNode::Create(CTNT_STATEMENT, CTNST_RETURN, gmlineno);
      yyval->SetChild(0, yyvsp[-1]);
    ;
    break;}
case 42:
{
      yyval = yyvsp[0];
      if(yyval)
      {
        yyval->m_flags |= gmCodeTreeNode::CTN_POP;
      }
    ;
    break;}
case 43:
{
      yyval = CreateOperation(CTNOT_ASSIGN, yyvsp[-2], yyvsp[0]);
    ;
    break;}
case 44:
{
      yyval = CreateAsignExpression(CTNOT_SHIFT_RIGHT, yyvsp[-2], yyvsp[0]);
    ;
    break;}
case 45:
{
      yyval = CreateAsignExpression(CTNOT_SHIFT_LEFT, yyvsp[-2], yyvsp[0]);
    ;
    break;}
case 46:
{
      yyval = CreateAsignExpression(CTNOT_ADD, yyvsp[-2], yyvsp[0]);
    ;
    break;}
case 47:
{
      yyval = CreateAsignExpression(CTNOT_MINUS, yyvsp[-2], yyvsp[0]);
    ;
    break;}
case 48:
{
      yyval = CreateAsignExpression(CTNOT_TIMES, yyvsp[-2], yyvsp[0]);
    ;
    break;}
case 49:
{
      yyval = CreateAsignExpression(CTNOT_DIVIDE, yyvsp[-2], yyvsp[0]);
    ;
    break;}
case 50:
{
      yyval = CreateAsignExpression(CTNOT_REM, yyvsp[-2], yyvsp[0]);
    ;
    break;}
case 51:
{
      yyval = CreateAsignExpression(CTNOT_BIT_AND, yyvsp[-2], yyvsp[0]);
    ;
    break;}
case 52:
{
      yyval = CreateAsignExpression(CTNOT_BIT_OR, yyvsp[-2], yyvsp[0]);
    ;
    break;}
case 53:
{
      yyval = CreateAsignExpression(CTNOT_BIT_XOR, yyvsp[-2], yyvsp[0]);
    ;
    break;}
case 54:
{
      yyval = NULL;
    ;
    break;}
case 55:
{
      yyval = yyvsp[-1];
    ;
    break;}
case 56:
{
      yyval = yyvsp[0];
    ;
    break;}
case 57:
{
      yyval = yyvsp[0];
    ;
    break;}
case 58:
{
      yyval = CreateOperation(CTNOT_OR, yyvsp[-2], yyvsp[0]);
    ;
    break;}
case 59:
{
      yyval = yyvsp[0];
    ;
    break;}
case 60:
{
      yyval = CreateOperation(CTNOT_AND, yyvsp[-2], yyvsp[0]);
    ;
    break;}
case 61:
{
      yyval = yyvsp[0];
    ;
    break;}
case 62:
{
      yyval = CreateOperation(CTNOT_BIT_OR, yyvsp[-2], yyvsp[0]);
      yyval->ConstantFold();
    ;
    break;}
case 63:
{
      yyval = yyvsp[0];
    ;
    break;}
case 64:
{
      yyval = CreateOperation(CTNOT_BIT_XOR, yyvsp[-2], yyvsp[0]);
      yyval->ConstantFold();
    ;
    break;}
case 65:
{
      yyval = yyvsp[0];
    ;
    break;}
case 66:
{
      yyval = CreateOperation(CTNOT_BIT_AND, yyvsp[-2], yyvsp[0]);
      yyval->ConstantFold();
    ;
    break;}
case 67:
{
      yyval = yyvsp[0];
    ;
    break;}
case 68:
{
      yyval = CreateOperation(CTNOT_EQ, yyvsp[-2], yyvsp[0]);
    ;
    break;}
case 69:
{
      yyval = CreateOperation(CTNOT_NEQ, yyvsp[-2], yyvsp[0]);
    ;
    break;}
case 70:
{
      yyval = yyvsp[0];
    ;
    break;}
case 71:
{
      yyval = CreateOperation(CTNOT_LT, yyvsp[-2], yyvsp[0]);
    ;
    break;}
case 72:
{
      yyval = CreateOperation(CTNOT_GT, yyvsp[-2], yyvsp[0]);
    ;
    break;}
case 73:
{
      yyval = CreateOperation(CTNOT_LTE, yyvsp[-2], yyvsp[0]);
    ;
    break;}
case 74:
{
      yyval = CreateOperation(CTNOT_GTE, yyvsp[-2], yyvsp[0]);
    ;
    break;}
case 75:
{
      yyval = yyvsp[0];
    ;
    break;}
case 76:
{
      yyval = CreateOperation(CTNOT_SHIFT_LEFT, yyvsp[-2], yyvsp[0]);
      yyval->ConstantFold();
    ;
    break;}
case 77:
{
      yyval = CreateOperation(CTNOT_SHIFT_RIGHT, yyvsp[-2], yyvsp[0]);
      yyval->ConstantFold();
    ;
    break;}
case 78:
{
      yyval = yyvsp[0];
    ;
    break;}
case 79:
{
      yyval = CreateOperation(CTNOT_ADD, yyvsp[-2], yyvsp[0]);
      yyval->ConstantFold();
    ;
    break;}
case 80:
{
      yyval = CreateOperation(CTNOT_MINUS, yyvsp[-2], yyvsp[0]);
      yyval->ConstantFold();
    ;
    break;}
case 81:
{
      yyval = yyvsp[0];
    ;
    break;}
case 82:
{
      yyval = CreateOperation(CTNOT_TIMES, yyvsp[-2], yyvsp[0]);
      yyval->ConstantFold();
    ;
    break;}
case 83:
{
      yyval = CreateOperation(CTNOT_DIVIDE, yyvsp[-2], yyvsp[0]);
      yyval->ConstantFold();
    ;
    break;}
case 84:
{
      yyval = CreateOperation(CTNOT_REM, yyvsp[-2], yyvsp[0]);
      yyval->ConstantFold();
    ;
    break;}
case 85:
{
      yyval = yyvsp[0];
    ;
    break;}
case 86:
{
      yyval = CreateOperation(CTNOT_PRE_INC, yyvsp[0]);
    ;
    break;}
case 87:
{
      yyval = CreateOperation(CTNOT_PRE_DEC, yyvsp[0]);
    ;
    break;}
case 88:
{
      yyval = yyvsp[-1];
      yyval->SetChild(0, yyvsp[0]);
      yyval->ConstantFold();
    ;
    break;}
case 89:
{
      yyval = CreateOperation(CTNOT_UNARY_PLUS);
    ;
    break;}
case 90:
{
      yyval = CreateOperation(CTNOT_UNARY_MINUS);
    ;
    break;}
case 91:
{
      yyval = CreateOperation(CTNOT_UNARY_COMPLEMENT);
    ;
    break;}
case 92:
{
      yyval = CreateOperation(CTNOT_UNARY_NOT);
    ;
    break;}
case 93:
{
      yyval = yyvsp[0];
    ;
    break;}
case 94:
{
      yyval = CreateOperation(CTNOT_ARRAY_INDEX, yyvsp[-3], yyvsp[-1]);
    ;
    break;}
case 95:
{
      yyval = gmCodeTreeNode::Create(CTNT_EXPRESSION, CTNET_CALL, gmlineno);
      yyval->SetChild(0, yyvsp[-2]);
    ;
    break;}
case 96:
{
      yyval = gmCodeTreeNode::Create(CTNT_EXPRESSION, CTNET_CALL, gmlineno);
      yyval->SetChild(0, yyvsp[-3]);
      yyval->SetChild(1, yyvsp[-1]);
    ;
    break;}
case 97:
{
      yyval = gmCodeTreeNode::Create(CTNT_EXPRESSION, CTNET_CALL, gmlineno);
      yyval->SetChild(0, yyvsp[-2]);
      yyval->SetChild(2, yyvsp[-4]);
    ;
    break;}
case 98:
{
      yyval = gmCodeTreeNode::Create(CTNT_EXPRESSION, CTNET_CALL, gmlineno);
      yyval->SetChild(0, yyvsp[-3]);
      yyval->SetChild(1, yyvsp[-1]);
      yyval->SetChild(2, yyvsp[-5]);
    ;
    break;}
case 99:
{
      yyval = CreateOperation(CTNOT_DOT, yyvsp[-2], yyvsp[0]);
    ;
    break;}
case 100:
{
      yyval = yyvsp[0];
    ;
    break;}
case 101:
{
      ATTACH(yyval, yyvsp[-2], yyvsp[0]);
    ;
    break;}
case 102:
{
      yyval = gmCodeTreeNode::Create(CTNT_EXPRESSION, CTNET_TABLE, gmlineno);
    ;
    break;}
case 103:
{
      yyval = gmCodeTreeNode::Create(CTNT_EXPRESSION, CTNET_TABLE, gmlineno);
      yyval->SetChild(0, yyvsp[-1]);
    ;
    break;}
case 104:
{
      yyval = gmCodeTreeNode::Create(CTNT_EXPRESSION, CTNET_TABLE, gmlineno);
    ;
    break;}
case 105:
{
      yyval = gmCodeTreeNode::Create(CTNT_EXPRESSION, CTNET_TABLE, gmlineno);
      yyval->SetChild(0, yyvsp[-1]);
    ;
    break;}
case 106:
{
      yyval = gmCodeTreeNode::Create(CTNT_EXPRESSION, CTNET_TABLE, gmlineno);
      yyval->SetChild(0, yyvsp[-2]);
    ;
    break;}
case 107:
{
      yyval = gmCodeTreeNode::Create(CTNT_EXPRESSION, CTNET_FUNCTION, gmlineno);
      yyval->SetChild(0, yyvsp[-2]);
      yyval->SetChild(1, yyvsp[0]);
    ;
    break;}
case 108:
{
      yyval = gmCodeTreeNode::Create(CTNT_EXPRESSION, CTNET_FUNCTION, gmlineno);
      yyval->SetChild(1, yyvsp[0]);
    ;
    break;}
case 109:
{
      yyval = yyvsp[0];
    ;
    break;}
case 110:
{
      ATTACH(yyval, yyvsp[-2], yyvsp[0]);
    ;
    break;}
case 111:
{
      yyval = yyvsp[0];
    ;
    break;}
case 112:
{
      yyval = CreateOperation(CTNOT_ASSIGN_FIELD, yyvsp[-2], yyvsp[0]);
    ;
    break;}
case 113:
{
      yyval = yyvsp[0];
    ;
    break;}
case 114:
{
      ATTACH(yyval, yyvsp[-2], yyvsp[0]);
    ;
    break;}
case 115:
{
      yyval = gmCodeTreeNode::Create(CTNT_DECLARATION, CTNDT_PARAMETER, gmlineno);
      yyval->SetChild(0, yyvsp[0]);
    ;
    break;}
case 116:
{
      yyval = gmCodeTreeNode::Create(CTNT_DECLARATION, CTNDT_PARAMETER, gmlineno);
      yyval->SetChild(0, yyvsp[-2]);
      yyval->SetChild(1, yyvsp[0]);
    ;
    break;}
case 117:
{
      yyval = yyvsp[0];
    ;
    break;}
case 118:
{
      yyval = yyvsp[0];
      yyval->m_flags |= gmCodeTreeNode::CTN_MEMBER;
    ;
    break;}
case 119:
{
      yyval = gmCodeTreeNode::Create(CTNT_EXPRESSION, CTNET_THIS, gmlineno);
    ;
    break;}
case 120:
{
      yyval = yyvsp[0];
    ;
    break;}
case 121:
{
      yyval = yyvsp[0];
    ;
    break;}
case 122:
{
      yyval = yyvsp[0];
    ;
    break;}
case 123:
{
      yyval = yyvsp[-1];
    ;
    break;}
case 124:
{
      yyval = gmCodeTreeNode::Create(CTNT_EXPRESSION, CTNET_IDENTIFIER, gmlineno);
      yyval->m_data.m_string = (char *) gmCodeTree::Get().Alloc((int)strlen(gmtext) + 1);
      strcpy(yyval->m_data.m_string, gmtext);
    ;
    break;}
case 125:
{
      yyval = gmCodeTreeNode::Create(CTNT_EXPRESSION, CTNET_CONSTANT, gmlineno, CTNCT_INT);
      yyval->m_data.m_iValue = strtoul(gmtext + 2, NULL, 16);
    ;
    break;}
case 126:
{
      yyval = gmCodeTreeNode::Create(CTNT_EXPRESSION, CTNET_CONSTANT, gmlineno, CTNCT_INT);
      yyval->m_data.m_iValue = strtoul(gmtext + 2, NULL, 2);
    ;
    break;}
case 127:
{
      yyval = gmCodeTreeNode::Create(CTNT_EXPRESSION, CTNET_CONSTANT, gmlineno, CTNCT_INT);
      yyval->m_data.m_iValue = atoi(gmtext);
    ;
    break;}
case 128:
{
      yyval = gmCodeTreeNode::Create(CTNT_EXPRESSION, CTNET_CONSTANT, gmlineno, CTNCT_INT);
      yyval->m_data.m_iValue = 1;
    ;
    break;}
case 129:
{
      yyval = gmCodeTreeNode::Create(CTNT_EXPRESSION, CTNET_CONSTANT, gmlineno, CTNCT_INT);
      yyval->m_data.m_iValue = 0;
    ;
    break;}
case 130:
{
      yyval = gmCodeTreeNode::Create(CTNT_EXPRESSION, CTNET_CONSTANT, gmlineno, CTNCT_INT);

      char * c = (char *) gmCodeTree::Get().Alloc((int)strlen(gmtext) + 1);
      strcpy(c, gmtext);
      int result = 0;
      int shr = 0;

      while(*c)
      {
        if(c[0] == '\'')
        {
          ++c;
          continue;
        }
        else if(c[0] == '\\')
        {
          if(shr) result <<= 8;
          switch(c[1])
          {
            case 'a' : result |= (unsigned char) '\a'; break;
            case 'b' : result |= (unsigned char) '\b'; break;
            case 'f' : result |= (unsigned char) '\f'; break;
            case 'n' : result |= (unsigned char) '\n'; break;
            case 'r' : result |= (unsigned char) '\r'; break;
            case 't' : result |= (unsigned char) '\t'; break;
            case 'v' : result |= (unsigned char) '\v'; break;
            case '\'' : result |= (unsigned char) '\''; break;
            case '\"' : result |= (unsigned char) '\"'; break;
            case '\\' : result |= (unsigned char) '\\'; break;
            default: result |= (unsigned char) c[1];
          }
          ++shr;
          c += 2;
          continue;
        }
        if(shr) result <<= 8;
        result |= (unsigned char) *(c++);
        ++shr;
      }

      if(shr > 4 && gmCodeTree::Get().GetLog()) gmCodeTree::Get().GetLog()->LogEntry("truncated char, line %d", gmlineno);

      yyval->m_data.m_iValue = result;
    ;
    break;}
case 131:
{
      yyval = gmCodeTreeNode::Create(CTNT_EXPRESSION, CTNET_CONSTANT, gmlineno, CTNCT_FLOAT);
      yyval->m_data.m_fValue = (float) atof(gmtext);
    ;
    break;}
case 132:
{
      yyval = yyvsp[0];
    ;
    break;}
case 133:
{
      yyval = gmCodeTreeNode::Create(CTNT_EXPRESSION, CTNET_CONSTANT, gmlineno, CTNCT_NULL);
      yyval->m_data.m_iValue = 0;
    ;
    break;}
case 134:
{
      yyval = gmCodeTreeNode::Create(CTNT_EXPRESSION, CTNET_CONSTANT, gmlineno, CTNCT_STRING);
      yyval->m_data.m_string = (char *) gmCodeTree::Get().Alloc((int)strlen(gmtext) + 1);
      strcpy(yyval->m_data.m_string, gmtext);
      if(gmtext[0] == '"')
      {
        gmProcessDoubleQuoteString(yyval->m_data.m_string);
      }
      else if(gmtext[0] == '`')
      {
        gmProcessSingleQuoteString(yyval->m_data.m_string);
      }
    ;
    break;}
case 135:
{
      yyval = yyvsp[-1];
      int alen = (int)strlen(yyval->m_data.m_string);
      int blen = (int)strlen(gmtext);
      char * str = (char *) gmCodeTree::Get().Alloc(alen + blen + 1);
      if(str)
      {
        memcpy(str, yyvsp[-1]->m_data.m_string, alen);
        memcpy(str + alen, gmtext, blen);
        str[alen + blen] = '\0';
        if(str[alen] == '"')
        {
          gmProcessDoubleQuoteString(str + alen);
        }
        else if(str[alen] == '`')
        {
          gmProcessSingleQuoteString(str + alen);
        }
        yyval->m_data.m_string = str;
      }
    ;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */


  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
        fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp-1)->last_line;
      yylsp->last_column = (yylsp-1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp+yylen-1)->last_line;
      yylsp->last_column = (yylsp+yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:   /* here on detecting error */

  if (! yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
        {
          int size = 0;
          char *msg;
          int x, count;

          count = 0;
          /* Start X at -yyn if nec to avoid negative indexes in yycheck.  */
          for (x = (yyn < 0 ? -yyn : 0);
               x < (int)(sizeof(yytname) / sizeof(char *)); x++) //_GD_
            if (yycheck[x + yyn] == x)
              size += (int)strlen(yytname[x]) + 15, count++; // _GD_ add cast for 64bit build
          //_GD_ msg = (char *) malloc(size + 15);
          msg = GM_NEW( char [size + 15] );
          if (msg != 0)
            {
              strcpy(msg, "parse error");

              if (count < 5)
                {
                  count = 0;
                  for (x = (yyn < 0 ? -yyn : 0);
                       x < (sizeof(yytname) / sizeof(char *)); x++)
                    if (yycheck[x + yyn] == x)
                      {
                        strcat(msg, count == 0 ? ", expecting `" : " or `");
                        strcat(msg, yytname[x]);
                        strcat(msg, "'");
                        count++;
                      }
                }
              yyerror(msg);
              //_GD_ free(msg);
              delete [] msg;
            }
          else
            yyerror ("parse error; also virtual memory exceeded");
        }
      else
#endif /* YYERROR_VERBOSE */
        yyerror("parse error");
    }

  goto yyerrlab1;
yyerrlab1:   /* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
        YYABORT;

#if YYDEBUG != 0
      if (yydebug)
        fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;              /* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
        fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
        goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;
}


#include <stdio.h>











