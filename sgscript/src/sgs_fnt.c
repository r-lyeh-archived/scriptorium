

#include "sgs_int.h"


static int isoneofN( char ch, const char* what, int size )
{
	const char* end = what + size;
	while( what < end )
	{
		if( ch == *what++ )
			return 1;
	}
	return 0;
}


/* debugging */


static void dumpnode( sgs_FTNode* N )
{
	switch( N->type )
	{
	case SGS_SFT_FCALL: printf( "FCALL" ); break;
	case SGS_SFT_INDEX: printf( "INDEX" ); break;
	case SGS_SFT_MIDXSET: printf( "MULTI_INDEX_SET" ); break;
	case SGS_SFT_MPROPSET: printf( "MULTI_PROPERTY_SET" ); break;
	case SGS_SFT_ARGMT: printf( "ARG " ); sgsT_DumpToken( N->token ); break;
	case SGS_SFT_ARGLIST: printf( "ARG_LIST" ); break;
	case SGS_SFT_VARLIST: printf( "VAR_LIST" ); break;
	case SGS_SFT_GVLIST: printf( "GLOBAL_VAR_LIST" ); break;
	case SGS_SFT_USELIST: printf( "USE_LIST" ); break;
	case SGS_SFT_EXPLIST: printf( "EXPR_LIST" ); break;
	case SGS_SFT_ARRLIST: printf( "ARRAY_LIST" ); break;
	case SGS_SFT_DCTLIST: printf( "DICT_LIST" ); break;
	case SGS_SFT_MAPLIST: printf( "MAP_LIST" ); break;
	case SGS_SFT_RETURN: printf( "RETURN" ); break;
	case SGS_SFT_BLOCK: printf( "BLOCK" ); break;
	case SGS_SFT_IFELSE: printf( "IF/ELSE" ); break;
	case SGS_SFT_WHILE: printf( "WHILE" ); break;
	case SGS_SFT_DOWHILE: printf( "DO/WHILE" ); break;
	case SGS_SFT_FOR: printf( "FOR" ); break;
	case SGS_SFT_FOREACH: printf( "FOR_EACH" ); break;
	case SGS_SFT_BREAK: printf( "BREAK" );
		if( *sgsT_Next( N->token ) == SGS_ST_NUMINT )
		{
			sgs_Int val;
			SGS_AS_INTEGER( val, sgsT_Next( N->token ) + 1 );
			printf( " %" PRId64, val );
		}
		break;
	case SGS_SFT_CONT: printf( "CONTINUE" );
		if( *sgsT_Next( N->token ) == SGS_ST_NUMINT )
		{
			sgs_Int val;
			SGS_AS_INTEGER( val, sgsT_Next( N->token ) + 1 );
			printf( " %" PRId64, val );
		}
		break;
	case SGS_SFT_FUNC: printf( "FUNC" ); break;
	default:
		if( N->token ) sgsT_DumpToken( N->token );
		if( N->type == SGS_SFT_OPER_P ) printf( " [post]" );
		break;
	}
}

static void ft_dump( sgs_FTNode* node, int level )
{
	int i;
	sgs_FTNode* N = node;
	if( !node ) return;

	for( i = 0; i < level; ++i ) printf( "  " );
	dumpnode( N );
	printf( "\n" );

	if( node->child )
	{
		for( i = 0; i < level; ++i ) printf( "  " );
		printf( "{\n" );

		ft_dump( node->child, level + 1 );

		for( i = 0; i < level; ++i ) printf( "  " );
		printf( "}\n" );
	}

	ft_dump( node->next, level );
}

void sgsFT_Dump( sgs_FTNode* tree )
{
	ft_dump( tree, 0 );
}


/*
// C O M P I L E R
*/


typedef struct _ftcomp
{
	SGS_CTX;
	sgs_TokenList at;
	sgs_FTNode* heap;
	sgs_FTNode* heapend;
}
FTComp;

#define SFTC FTComp* F
#define SFTRET static sgs_FTNode*
#define SFTC_AT F->at
#define SFTC_NEXT F->at = sgsT_Next( F->at )
#define SFTC_IS( type ) (*F->at == (type))
/* WP: char/uchar conversion */
#define SFTC_IN( buf, sz ) isoneofN( (char) *F->at, buf, sz )
#define SFTC_HASERR ( F->C->state & SGS_HAS_ERRORS )
#define SFTC_SETERR F->C->state |= SGS_HAS_ERRORS
#define SFTC_ISKEY( name ) sgsT_IsKeyword( F->at, name )
#define SFTC_IS_ID( name ) sgsT_IsIdent( F->at, name )
#define SFTC_LINENUM sgsT_LineNum( F->at )
#define SFTC_PRINTERR( what ) sgs_Msg( F->C, SGS_ERROR, "[line %d] " what, SFTC_LINENUM )
#define SFTC_UNEXP sgs_Msg( F->C, SGS_ERROR, "Unexpected end of code", SFTC_LINENUM )


static sgs_FTNode* _make_heap( SGS_CTX )
{
	sgs_FTNode* node = sgs_Alloc_n( sgs_FTNode, 33 );
	node->type = SGS_SFT_HEAPBIT;
	node->token = NULL;
	node->next = NULL;
	node->child = NULL;
	return node;
}

static sgs_FTNode* _make_node( SFTC, int type, sgs_TokenList token, sgs_FTNode* next, sgs_FTNode* child )
{
	sgs_FTNode* node = NULL;
	if( ( F->heapend->type >> 8 ) == 32 )
	{
		F->heapend = F->heapend->next = _make_heap( F->C );
	}
	node = F->heapend + 1 + ( F->heapend->type >> 8 );
	F->heapend->type += ( 1 << 8 );
	
	node->type = type;
	node->token = token;
	node->next = next;
	node->child = child;
	return node;
}
#define make_node( ty, tok, next, ch ) _make_node( F, ty, tok, next, ch )

void sgsFT_Destroy( SGS_CTX, sgs_FTNode* tree )
{
	while( tree )
	{
		sgs_FTNode* next = tree->next;
		/* if( tree->child ) sgsFT_Destroy( C, tree->child ); HEAP CHILD IS NOT A SEPARATE ALLOCATION */
		sgs_Dealloc( tree );
		tree = next;
	}
}
#define SFTC_DESTROY( node ) (void)0 /* sgsFT_Destroy( F->C, node ) */





SFTRET parse_exp( SFTC, char* endtoklist, int etlsize );
SFTRET parse_stmt( SFTC );
SFTRET parse_stmtlist( SFTC, char end );
SFTRET parse_function( SFTC, int inexp );





SFTRET parse_arg( SFTC, int argid, char end )
{
	sgs_FTNode* node = NULL;
	char toks[ 3 ] = { ',', 0, 0 };
	toks[1] = end;

	SGS_FN_BEGIN;

	if( SFTC_IS(0) )
	{
		SFTC_UNEXP;
		goto fail;
	}

	if( SFTC_IS( SGS_ST_KEYWORD ) )
	{
		SFTC_PRINTERR( "Argument name cannot be a reserved keyword" );
		goto fail;
	}

	if( !SFTC_IS( SGS_ST_IDENT ) && !SFTC_IS( SGS_ST_KEYWORD ) )
	{
		sgs_Msg( F->C, SGS_ERROR, "[line %d] Unexpected token while parsing argument %d", SFTC_LINENUM, argid );
		goto fail;
	}

	node = make_node( SGS_SFT_ARGMT, SFTC_AT, NULL, NULL );
	SFTC_NEXT;

	if( SFTC_IS( SGS_ST_OP_SET ) )
	{
		SFTC_NEXT;
		if( SFTC_IS( end ) || SFTC_IS( ',' ) )
		{
			SFTC_PRINTERR( "Expected initializing expression" );
			goto fail;
		}
		node->child = parse_exp( F, toks, 2 );
		if( !node->child )
			goto fail;
	}

	SGS_FN_END;
	return node;

fail:
	if( node ) SFTC_DESTROY( node );
	SFTC_SETERR;
	SGS_FN_END;
	return 0;
}

/*
	FUNC / parses a list of arguments
	ARGS / context, function tree, token stream
	ERRS / unexpected token, @parse_arg
*/
SFTRET parse_arglist( SFTC, char end )
{
	sgs_FTNode* arglist = make_node( SGS_SFT_ARGLIST, NULL, NULL, NULL );
	sgs_FTNode* curnode = NULL;
	sgs_FTNode* argnode;
	int id = 1;

	SGS_FN_BEGIN;

	for(;;)
	{
		if( SFTC_IS( end ) )
		{
			break;
		}
		else if( SFTC_IS( 0 ) )
		{
			SFTC_UNEXP;
			goto fail;
		}
		else if( id == 1 || SFTC_IS( ',' ) )
		{
			if( id != 1 )
				SFTC_NEXT;
			argnode = parse_arg( F, id, end );
			if( !argnode && F->C->state & SGS_MUST_STOP )
			{
				goto fail;
			}
			if( curnode ) curnode->next = argnode;
			else arglist->child = argnode;
			curnode = argnode;

			id++;
		}
		else
		{
			sgs_Msg( F->C, SGS_ERROR, "[line %d] Expected ',' or '%c'", SFTC_LINENUM, end );
			goto fail;
		}
	}

	SGS_FN_END;
	return arglist;

fail:
	SFTC_SETERR;
	SFTC_DESTROY( arglist );
	return NULL;
}

/*
	FUNC / calculates the weight of the part
	ARGS / context, part of function tree
	ERRS / none
*/
static int part_weight( sgs_FTNode* part, int isfcall, int binary )
{
	sgs_BreakIf( !part );

	if( part->type == SGS_SFT_OPER && SGS_ST_OP_ASSIGN( *part->token ) )
		return 40;

	if( isfcall )
		return 7;

	if( part->type == SGS_SFT_OPER )
	{
		sgs_TokenType type = *part->token;
		if( binary )
		{
			if( SGS_ST_OP_BOOL( type ) )	return 30;
			if( type == SGS_ST_OP_NLOR )    return 29;
			if( type == SGS_ST_OP_RWCMP )   return 28; /* lower split prio .. */
			if( SGS_ST_OP_COMP( type ) )	return 29; /* .. than other comp ops */
			if( type == SGS_ST_OP_CAT )		return 27;
			if( SGS_ST_OP_BINOPS( type ) )	return 26;
			if( SGS_ST_OP_BINADD( type ) )	return 25;
			if( SGS_ST_OP_BINMUL( type ) )	return 24;
			if( type == SGS_ST_OP_MMBR )	return 7;
			if( type == SGS_ST_OP_NOT )     return 7;
			return 11;
		}

		/* unary operators */
		return 10;
	}

	/* everything else */
	return -1;
}



static sgs_LineNum findlinenum( sgs_FTNode* node ) /* local, next, child */
{
	sgs_LineNum ln = -1;

	if( node->token )
		return sgsT_LineNum( node->token );

	ln = findlinenum( node->next );
	if( ln != -1 ) return ln;

	ln = findlinenum( node->child );
	if( ln != -1 ) return ln;

	return -1;
}

static sgs_LineNum predictlinenum( sgs_FTNode* node ) /* next, child, local */
{
	sgs_LineNum ln = -1;

	ln = findlinenum( node->next );
	if( ln != -1 ) return ln;

	ln = predictlinenum( node->child );
	if( ln != -1 ) return ln;

	if( node->token )
		return sgsT_LineNum( node->token );

	return -1;
}


static int level_exp( SFTC, sgs_FTNode** tree )
{
	sgs_FTNode* node = *tree, *prev = NULL, *mpp = NULL;
	int weight = 0, isfcall, binary, count = 0;
	int threadmode = 0; /* 0 = none, 1 = thread, 2 = subthread */

	SGS_FN_BEGIN;
	sgs_BreakIf( !tree );

	if( !*tree )
	{
		SGS_FN_END;
		return 0;
	}
	
	if( sgsT_IsKeyword( node->token, "thread" ) )
	{
		threadmode = 1;
		node = node->next;
		(*tree)->next = NULL;
		SFTC_DESTROY( *tree );
		*tree = node;
	}
	if( sgsT_IsKeyword( node->token, "subthread" ) )
	{
		threadmode = 2;
		node = node->next;
		(*tree)->next = NULL;
		SFTC_DESTROY( *tree );
		*tree = node;
	}
	
	/* find the most powerful part (mpp) */
	while( node )
	{
		int leftmostsplit, curwt;
		
		count++;
		
		/* only interested in operators and subexpressions */
		if( node->type != SGS_SFT_OPER && node->type != SGS_SFT_EXPLIST && node->type != SGS_SFT_ARRLIST )
			goto _continue;
		
		/* function tree test */
		isfcall = node->type == SGS_SFT_EXPLIST || node->type == SGS_SFT_ARRLIST;
		if( isfcall )	isfcall = !!prev;
		if( isfcall )	isfcall = prev->type != SGS_SFT_OPER || !SGS_ST_OP_BINARY( *prev->token );
		
		/* op tests */
		binary = node->type == SGS_SFT_OPER;
		if( binary )	binary = prev && node->next;
		if( binary )	binary = prev->type != SGS_SFT_OPER || 
			*prev->token == SGS_ST_OP_INC || *prev->token == SGS_ST_OP_DEC;
		
		/* weighting */
		curwt = part_weight( node, isfcall, binary );
		leftmostsplit = 
			( node->type == SGS_SFT_OPER && SGS_ST_OP_ASSIGN( *node->token ) ) || /* assignment ops */
			( node->type == SGS_SFT_OPER && !binary ); /* unary ops */
		if( ( leftmostsplit && curwt > weight ) || ( !leftmostsplit && curwt >= weight ) )
		{
			weight = curwt;
			mpp = node;
		}
		
		/* move to next */
_continue:
		prev = node;
		node = node->next;
	}
	
	if( mpp )
	{
		/* function call */
		if( mpp->type == SGS_SFT_EXPLIST || mpp->type == SGS_SFT_ARRLIST )
		{
			int ret1, ret2;
			sgs_TokenList mpp_token = mpp->token;
			sgs_FTNode* se1 = *tree, *se2 = mpp, *se1i = *tree;
			while( se1i )
			{
				if( se1i->next == mpp )
					se1i->next = NULL;
				se1i = se1i->next;
			}
			
			SGS_FN_ENTER;
			ret1 = level_exp( F, &se1 );
			if( !ret1 )
			{
				*tree = NULL;
				if( se1 ) SFTC_DESTROY( se1 );
				if( se2 ) SFTC_DESTROY( se2 );
				SGS_FN_END;
				return 0;
			}
			
			if( mpp->type == SGS_SFT_ARRLIST && !mpp->child && mpp->next && mpp->next->type == SGS_SFT_DCTLIST )
			{
				/* a multiset (index) expression */
				mpp->type = SGS_SFT_MIDXSET;
				mpp->child = se1;
				mpp->child->next = mpp->next;
				mpp->next = NULL;
				*tree = mpp;
				goto retsuccess;
			}
			
			SGS_FN_ENTER;
			ret2 = level_exp( F, &se2 );
			if( !ret2 )
			{
				*tree = NULL;
				if( se1 ) SFTC_DESTROY( se1 );
				if( se2 ) SFTC_DESTROY( se2 );
				SGS_FN_END;
				return 0;
			}
			if( *mpp_token == SGS_ST_SBRKL )
			{
				/* array */
				if( !se2->child || se2->child->next )
				{
					sgs_Msg( F->C, SGS_ERROR, "[line %d] Invalid number of arguments "
						"in an array accessor", sgsT_LineNum( mpp_token ) );
					*tree = NULL;
					if( se1 ) SFTC_DESTROY( se1 );
					/* if( se2 ) */ SFTC_DESTROY( se2 );
					SGS_FN_END;
					return 0;
				}
				se1->next = se2->child;
				se2->child = NULL;
				SFTC_DESTROY( se2 );
				*tree = make_node( SGS_SFT_INDEX, mpp_token, NULL, se1 );
				goto retsuccess;
			}
			se1->next = se2;
			*tree = make_node( SGS_SFT_FCALL, mpp_token, NULL, se1 );
			goto retsuccess;
		}
		
		/* operators */
		if( mpp->type == SGS_SFT_OPER )
		{
			if( mpp == *tree )
				prev = NULL;
			else
			{
				prev = *tree;
				while( prev->next != mpp )
					prev = prev->next;
			}
			
			/* binary operators */
			if( mpp != *tree && mpp->next && 
				( prev->type != SGS_SFT_OPER || *prev->token == SGS_ST_OP_INC || *prev->token == SGS_ST_OP_DEC ) )
			{
				sgs_TokenList mpptoken = mpp->token;
				int ret1, ret2;
				sgs_FTNode* se1 = *tree, *se2 = mpp->next, *se1i = *tree;
				while( se1i )
				{
					if( se1i->next == mpp )
						se1i->next = NULL;
					se1i = se1i->next;
				}
				mpp->next = NULL;
				
				SGS_FN_ENTER;
				ret1 = level_exp( F, &se1 );
				if( !ret1 )
				{
					*tree = NULL;
					if( se1 ) SFTC_DESTROY( se1 );
					if( se2 ) SFTC_DESTROY( se2 );
					SFTC_DESTROY( mpp );
					SGS_FN_END;
					return 0;
				}
				
				if( mpptoken && *mpptoken == SGS_ST_OP_MMBR && !mpp->child && se2->type == SGS_SFT_DCTLIST )
				{
					/* a multiset (property) expression */
					mpp->type = SGS_SFT_MPROPSET;
					mpp->child = se1;
					mpp->child->next = se2;
					mpp->next = NULL;
					*tree = mpp;
					SGS_FN_END;
					goto retsuccess;
				}
				
				SFTC_DESTROY( mpp );
				
				SGS_FN_ENTER;
				ret2 = level_exp( F, &se2 );
				if( !ret2 )
				{
					*tree = NULL;
					if( se1 ) SFTC_DESTROY( se1 );
					if( se2 ) SFTC_DESTROY( se2 );
					SGS_FN_END;
					return 0;
				}
				
				se1->next = se2;
				*tree = make_node( SGS_SFT_OPER, mpptoken, NULL, se1 );
				SGS_FN_END;
				
				if( *mpptoken == SGS_ST_OP_CAT || *mpptoken == SGS_ST_OP_CATEQ )
				{
					/* merge in CAT on first operand (works only with non-assignment op) */
					if( *mpptoken == SGS_ST_OP_CAT && *se1->token == SGS_ST_OP_CAT )
					{
						/* target tree: tree { se1:children, se2 } */
						sgs_FTNode* tmp = se1->child;
						while( tmp->next )
							tmp = tmp->next;
						tmp->next = se2;
						(*tree)->child = se1->child;
						
						se1->child = NULL;
						se1->next = NULL;
						SFTC_DESTROY( se1 );
					}
					/* merge in CAT on second operand */
					if( *se2->token == SGS_ST_OP_CAT )
					{
						/* target tree: tree { children without se2, se2:children } */
						sgs_FTNode* tmp = (*tree)->child;
						while( tmp->next && tmp->next != se2 )
							tmp = tmp->next;
						sgs_BreakIf( tmp->next == NULL );
						tmp->next = se2->child;
						
						se2->child = NULL;
						SFTC_DESTROY( se2 );
					}
				}
				
				goto retsuccess;
			}
			/* unary operators
				must be one of these cases:
				- no tokens before operator
				- is inc|dec and there are tokens either before or after the operator
					- can't have both, can't have neither
			*/
			else if( SGS_ST_OP_UNARY( *mpp->token ) && ( mpp == *tree || 
				( ( *mpp->token == SGS_ST_OP_INC || *mpp->token == SGS_ST_OP_DEC ) &&
				( mpp != *tree ) != ( !!mpp->next ) ) ) )
			{
				int ret1;
				if( !mpp->next )
				{
					sgs_FTNode* pp = *tree;
					if( pp == mpp ) goto fail;

					while( pp->next != mpp )
						pp = pp->next;
					pp->next = NULL;
					mpp->child = *tree;
					*tree = mpp;

					mpp->type = SGS_SFT_OPER_P;
				}
				else
				{
					mpp->child = mpp->next;
					mpp->next = NULL;
				}
				SGS_FN_ENTER;
				ret1 = level_exp( F, &mpp->child );
				if( !ret1 )
				{
					SGS_FN_END;
					return 0;
				}
				goto retsuccess;
			}
			/* problems */
			else goto fail;
		}
	}
	
	if( count <= 1 )
	{
retsuccess:
		if( threadmode )
		{
			if( (*tree)->type != SGS_SFT_FCALL )
			{
				if( threadmode == 1 )
					SFTC_PRINTERR( "expected function call after 'thread'" );
				else
					SFTC_PRINTERR( "expected function call after 'subthread'" );
				goto fail;
			}
			if( threadmode == 1 )
				(*tree)->type = SGS_SFT_THRCALL;
			else
				(*tree)->type = SGS_SFT_STHCALL;
		}
		
		SGS_FN_END;
		return 1;
	}

	/* in case we failed unexpectedly, dump & debug */
	sgs_Msg( F->C, SGS_ERROR, "[line %d] Missing operators or separators", predictlinenum( *tree ) );
	F->C->state |= SGS_HAS_ERRORS;
#if SGS_DEBUG && SGS_DEBUG_DATA
	sgsFT_Dump( *tree );
#endif
	SGS_FN_END;
	return 0;

fail:
	if( mpp == NULL )
		mpp = *tree;
	sgs_Msg( F->C, SGS_ERROR, "[line %d] Invalid expression", mpp ? sgsT_LineNum( mpp->token ) : 0 );
#if SGS_DEBUG && SGS_DEBUG_DATA
	sgsFT_Dump( *tree );
#endif
	SGS_FN_END;
	return 0;
}


SFTRET parse_dict( SFTC )
{
	sgs_TokenList startok = SFTC_AT;
	sgs_FTNode* expr = NULL, *fexp = NULL, *cur;
	/* dictionary expression */
	SFTC_NEXT;
	while( !SFTC_IS( '}' ) )
	{
		int is_ident = SFTC_IS( SGS_ST_IDENT );
		int is_varkey = SFTC_IS( '[' );
		if( !is_ident && !is_varkey && !SFTC_IS( SGS_ST_STRING ) )
		{
			SFTC_PRINTERR( "expected key identifier, string or '[' in dictionary expression" );
			goto fail;
		}
		
		/* make key */
		if( is_varkey )
		{
			SFTC_NEXT;
			cur = parse_exp( F, "]", 1 );
			if( !cur )
				goto fail;
		}
		else
		{
			cur = make_node( SGS_SFT_ARGMT, SFTC_AT, NULL, NULL );
		}
		SFTC_NEXT;
		
		/* add key to node list */
		if( !fexp )
			expr = fexp = cur;
		else
		{
			expr->next = cur;
			expr = expr->next;
		}
		
		if( !SFTC_IS( SGS_ST_OP_SET ) )
		{
			if( is_ident )
			{
				if( SFTC_IS( ',' ) || SFTC_IS( '}' ) )
				{
					expr->next = make_node( SGS_SFT_IDENT, expr->token, NULL, NULL );
					expr = expr->next;
				}
				else
				{
					SFTC_PRINTERR( "Expected '=', ',' or '}' after dictionary key" );
					goto fail;
				}
			}
			else
			{
				SFTC_PRINTERR( "Expected '=' in dictionary expression "
					"/ missing closing bracket before '{'" );
				goto fail;
			}
		}
		else
		{
			SFTC_NEXT;
			
			expr->next = parse_exp( F, ",}", 2 );
			if( !expr->next )
				goto fail;
			else
				expr = expr->next;
		}
		
		if( SFTC_IS( ',' ) )
			SFTC_NEXT;
	}
	return make_node( SGS_SFT_DCTLIST, startok, NULL, fexp );
	
fail:
	if( fexp )
		SFTC_DESTROY( fexp );
	return NULL;
}


SFTRET parse_exp( SFTC, char* endtoklist, int etlsize )
{
	sgs_FTNode* node, *cur;
	char prev = 0;

	SGS_FN_BEGIN;

	if( SFTC_IS( 0 ) )
	{
		SFTC_UNEXP;
		SFTC_SETERR;
		SGS_FN_END;
		return NULL;
	}

	/* special cases */
	if( SFTC_ISKEY( "var" ) )
	{
		SFTC_NEXT;
		node = parse_arglist( F, endtoklist[ endtoklist[0] == ',' ] );
		if( node )
			node->type = SGS_SFT_VARLIST;
		SGS_FN_END;
		return node;
	}
	if( SFTC_ISKEY( "global" ) )
	{
		SFTC_NEXT;
		node = parse_arglist( F, endtoklist[ endtoklist[0] == ',' ] );
		if( node )
			node->type = SGS_SFT_GVLIST;
		SGS_FN_END;
		return node;
	}

	cur = node = make_node( 0, NULL, NULL, NULL );

	for(;;)
	{
		if( SFTC_IN( endtoklist, etlsize ) )
		{
			break;
		}
		else if( SFTC_IS( 0 ) )
		{
			SFTC_UNEXP;
			goto fail;
		}
		else if( SFTC_IS( SGS_ST_STRING )
			  || SFTC_IS( SGS_ST_NUMINT )
			  || SFTC_IS( SGS_ST_NUMREAL ) )
			cur = cur->next = make_node( SGS_SFT_CONST, SFTC_AT, NULL, NULL );
		else if( SFTC_IS( SGS_ST_IDENT ) )
		{
			if( SFTC_IS_ID( "map" ) && sgsT_Next( SFTC_AT ) && *sgsT_Next( SFTC_AT ) == '{' )
			{
				SFTC_NEXT;
				cur->next = parse_dict( F );
				if( !cur->next )
					goto fail;
				cur = cur->next;
				cur->type = SGS_SFT_MAPLIST;
			}
			else
				cur = cur->next = make_node( SGS_SFT_IDENT, SFTC_AT, NULL, NULL );
		}
		else if( SFTC_IS( SGS_ST_KEYWORD ) )
		{
			if( SFTC_ISKEY( "function" ) )
			{
				cur->next = parse_function( F, 1 );
				if( !cur->next )
					goto fail;
				cur = cur->next;
				continue;
			}
			else
				cur = cur->next = make_node( SGS_SFT_KEYWORD, SFTC_AT, NULL, NULL );
		}
		else if( SGS_ST_ISOP( *SFTC_AT ) )
			cur = cur->next = make_node( SGS_SFT_OPER, SFTC_AT, NULL, NULL );
		else if( SGS_ST_ISSPEC( *SFTC_AT ) )
		{
			/* array accesor / argument list / subexpression */
			if( SFTC_IS( '(' ) || SFTC_IS( '[' ) )
			{
				int isidx = prev == SGS_ST_IDENT || prev == ')' || prev == ']';
				char cend = SFTC_IS( '(' ) ? ')' : ']';
				sgs_FTNode* exprlist = make_node( SFTC_IS( '(' ) ? SGS_SFT_EXPLIST : SGS_SFT_ARRLIST, SFTC_AT, NULL, NULL );
				sgs_FTNode* expr, * curexpr = NULL;
				char endcstr[ 3 ] = { ',', 0, 0 };
				endcstr[1] = cend;

				SFTC_NEXT;
				/* if this is an empty expression (for a function call), do not process it further */
				if( !SFTC_IS( cend ) )
				{
					for(;;)
					{
						/* if not index, extra ',' is allowed */
						if( !isidx && SFTC_IS( cend ) )
						{
							SFTC_NEXT;
							break;
						}

						SGS_FN_ENTER;
						expr = parse_exp( F, endcstr, 2 );
						if( !expr )
						{
							SFTC_DESTROY( exprlist );
							goto fail;
						}

						if( curexpr )
							curexpr->next = expr;
						else
							exprlist->child = expr;
						curexpr = expr;

						if( SFTC_IS( cend ) )
						{
							SFTC_NEXT;
							break;
						}

						SFTC_NEXT;
					}
				}
				else
					SFTC_NEXT;

				cur = cur->next = exprlist;
				continue;
			}
			/* dictionary */
			else if( SFTC_IS( '{' ) )
			{
				cur->next = parse_dict( F );
				if( !cur->next )
					goto fail;
				cur = cur->next;
			}
			else
			{
				sgs_Msg( F->C, SGS_ERROR, "[line %d] Unexpected token '%c' found!", SFTC_LINENUM, *SFTC_AT );
				F->C->state |= SGS_MUST_STOP;
			}
		}
		else
		{
			SFTC_PRINTERR( "INTERNAL ERROR in parse_exp: unknown token found!" );
			F->C->state |= SGS_MUST_STOP;
		}

		if( F->C->state & SGS_MUST_STOP )
		{
			SFTC_DESTROY( node );
			SGS_FN_END;
			return NULL;
		}
		
		/* WP: char/uchar conversion */
		prev = (char) *SFTC_AT;
		SFTC_NEXT;
	}

	cur = node->next;
	{
		node->child = NULL;
		node->next = NULL;
		SFTC_DESTROY( node );
	}
	node = cur;
	if( !node )
	{
		SFTC_PRINTERR( "Empty expression found" );
		goto fail;
	}

	if( !level_exp( F, &node ) )
		goto fail;
	
	SGS_FN_END;
	return node;

fail:
	SFTC_SETERR;
	if( node ) SFTC_DESTROY( node );
	SGS_FN_END;
	return NULL;
}





SFTRET parse_explist( SFTC, char endtok )
{
	sgs_FTNode* explist = make_node( SGS_SFT_EXPLIST, SFTC_AT, NULL, NULL );
	sgs_FTNode* curexp = NULL, *node;
	char endtoklist[] = { ',', 0, 0 };
	endtoklist[1] = endtok;

	SGS_FN_BEGIN;

	for(;;)
	{
		if( SFTC_IS( endtok ) )
		{
			break;
		}
		else if( SFTC_IS( 0 ) )
		{
			SFTC_UNEXP;
			goto fail;
		}
		else if( SFTC_IS( ',' ) || SFTC_AT == explist->token )
		{
			if( SFTC_AT != explist->token )
				SFTC_NEXT;
			node = parse_exp( F, endtoklist, 2 );
			if( !node )
				goto fail;
			if( curexp )
				curexp->next = node;
			else
				explist->child = node;
			curexp = node;
		}
		else
		{
			sgs_Msg( F->C, SGS_ERROR, "[line %d] Expected ',' or '%c'", SFTC_LINENUM, endtok );
			goto fail;
		}
	}

	SGS_FN_END;
	return explist;

fail:
	SFTC_SETERR;
	SFTC_DESTROY( explist );
	SGS_FN_END;
	return NULL;
}

SFTRET parse_if( SFTC )
{
	sgs_FTNode *node = NULL, *nexp = NULL, *nif = NULL, *nelse = NULL;
	sgs_TokenList begin = SFTC_AT;

	SGS_FN_BEGIN;

	SFTC_NEXT;
	if( !SFTC_IS( '(' ) )
	{
		SFTC_PRINTERR( "Expected '(' after 'if'" );
		goto fail;
	}

	SFTC_NEXT;

	nexp = parse_exp( F, ")", 1 );
	if( !nexp ) goto fail;
	SFTC_NEXT;

	nif = parse_stmt( F );
	if( !nif ) goto fail;

	if( SFTC_ISKEY( "else" ) )
	{
		SFTC_NEXT;
		nelse = parse_stmt( F );
		if( !nelse ) goto fail;
	}

	nexp->next = nif;
	nif->next = nelse;
	node = make_node( SGS_SFT_IFELSE, begin, NULL, nexp );

	SGS_FN_END;
	return node;

fail:
	if( nexp ) SFTC_DESTROY( nexp );
	if( nif ) SFTC_DESTROY( nif );
	if( nelse ) SFTC_DESTROY( nelse );
	SFTC_SETERR;
	SGS_FN_END;
	return NULL;
}

SFTRET parse_while( SFTC )
{
	sgs_FTNode *node, *nexp = NULL, *nwhile = NULL;
	sgs_TokenList begin = SFTC_AT;

	SGS_FN_BEGIN;

	SFTC_NEXT;
	if( !SFTC_IS( '(' ) )
	{
		SFTC_PRINTERR( "Expected '(' after 'while'" );
		goto fail;
	}

	SFTC_NEXT;

	nexp = parse_exp( F, ")", 1 );
	if( !nexp ) goto fail;
	SFTC_NEXT;

	nwhile = parse_stmt( F );
	if( !nwhile ) goto fail;

	nexp->next = nwhile;
	node = make_node( SGS_SFT_WHILE, begin, NULL, nexp );

	SGS_FN_END;
	return node;

fail:
	if( nexp ) SFTC_DESTROY( nexp );
	if( nwhile ) SFTC_DESTROY( nwhile );
	SFTC_SETERR;
	SGS_FN_END;
	return NULL;
}

SFTRET parse_dowhile( SFTC )
{
	sgs_FTNode *node, *nexp = NULL, *nwhile = NULL;
	sgs_TokenList begin = SFTC_AT;

	SGS_FN_BEGIN;

	SFTC_NEXT;
	nwhile = parse_stmt( F );
	if( !nwhile ) goto fail;

	if( !SFTC_ISKEY( "while" ) )
	{
		SFTC_PRINTERR( "Expected 'while' after statement in do/while" );
		goto fail;
	}

	SFTC_NEXT;
	if( !SFTC_IS( '(' ) )
	{
		SFTC_PRINTERR( "Expected '(' after 'while'" );
		goto fail;
	}

	SFTC_NEXT;

	nexp = parse_exp( F, ")", 1 );
	if( !nexp ) goto fail;
	SFTC_NEXT;

	nexp->next = nwhile;
	node = make_node( SGS_SFT_DOWHILE, begin, NULL, nexp );

	SGS_FN_END;
	return node;

fail:
	if( nexp ) SFTC_DESTROY( nexp );
	if( nwhile ) SFTC_DESTROY( nwhile );
	SFTC_SETERR;
	SGS_FN_END;
	return NULL;
}

SFTRET parse_for( SFTC )
{
	sgs_FTNode *node, *ninit = NULL, *nexp = NULL, *nincr = NULL, *nwhile = NULL;
	sgs_TokenList begin = SFTC_AT;

	SGS_FN_BEGIN;

	SFTC_NEXT;
	if( !SFTC_IS( '(' ) )
	{
		SFTC_PRINTERR( "Expected '(' after 'for'" );
		goto fail;
	}

	SFTC_NEXT;

	ninit = parse_explist( F, ';' );
	if( !ninit ) goto fail;
	SFTC_NEXT;

	nexp = parse_explist( F, ';' );
	if( !nexp ) goto fail;
	SFTC_NEXT;

	nincr = parse_explist( F, ')' );
	if( !nincr ) goto fail;
	SFTC_NEXT;

	nwhile = parse_stmt( F );
	if( !nwhile ) goto fail;

	ninit->next = nexp;
	nexp->next = nincr;
	nincr->next = nwhile;
	node = make_node( SGS_SFT_FOR, begin, NULL, ninit );

	SGS_FN_END;
	return node;

fail:
	if( ninit ) SFTC_DESTROY( ninit );
	if( nexp ) SFTC_DESTROY( nexp );
	if( nincr ) SFTC_DESTROY( nincr );
	if( nwhile ) SFTC_DESTROY( nwhile );
	SFTC_SETERR;
	SGS_FN_END;
	return NULL;
}

SFTRET parse_foreach( SFTC )
{
	/*
		(x: => null=>ident
		(x,x: => ident=>ident
		(x,: => ident=>null
	*/
	sgs_FTNode *node, *nvar = NULL, *nkey = NULL, *nexp = NULL, *nwhile = NULL;
	sgs_TokenList begin = SFTC_AT;

	SGS_FN_BEGIN;

	SFTC_NEXT;
	if( !SFTC_IS( '(' ) )
	{
		SFTC_PRINTERR( "Expected '(' after 'foreach'" );
		goto fail;
	}

	SFTC_NEXT;
	if( !SFTC_IS( SGS_ST_IDENT ) )
	{
		SFTC_PRINTERR( "Expected identifier after '(' in 'foreach'" );
		goto fail;
	}
	/* (x:e) */
	nkey = make_node( SGS_SFT_NULL, SFTC_AT, NULL, NULL );
	nvar = make_node( SGS_SFT_IDENT, SFTC_AT, NULL, NULL );
	SFTC_NEXT;

	if( !SFTC_IS( ':' ) && !SFTC_IS( ',' ) )
	{
		SFTC_PRINTERR( "Expected ':' or ',' after identifier in 'foreach'" );
		goto fail;
	}

	if( SFTC_IS( ',' ) )
	{
		SFTC_NEXT;
		if( !SFTC_IS( SGS_ST_IDENT ) && !SFTC_IS( ':' ) )
		{
			SFTC_PRINTERR( "Expected identifier or ':' after ',' in 'foreach'" );
			goto fail;
		}

		if( SFTC_IS( SGS_ST_IDENT ) )
		{
			/* (x,x:e) */
			nkey->type = SGS_SFT_IDENT;
			nvar->token = SFTC_AT;
			SFTC_NEXT;
		}
		else
		{
			/* (x,:e) */
			nkey->type = SGS_SFT_IDENT;
			nvar->type = SGS_SFT_NULL;
		}

		if( !SFTC_IS( ':' ) )
		{
			SFTC_PRINTERR( "Expected ':' after identifier #2 or ',' in 'foreach'" );
			goto fail;
		}
	}

	SFTC_NEXT;

	nexp = parse_explist( F, ')' );
	if( !nexp ) goto fail;

	SFTC_NEXT;
	nwhile = parse_stmt( F );
	if( !nwhile ) goto fail;

	nkey->next = nvar;
	nvar->next = nexp;
	nexp->next = nwhile;
	node = make_node( SGS_SFT_FOREACH, begin, NULL, nkey );

	SGS_FN_END;
	return node;

fail:
	if( nvar ) SFTC_DESTROY( nvar );
	if( nkey ) SFTC_DESTROY( nkey );
	if( nexp ) SFTC_DESTROY( nexp );
	if( nwhile ) SFTC_DESTROY( nwhile );
	SGS_FN_END;
	return NULL;
}

SFTRET parse_function( SFTC, int inexp )
{
	int hasname = 0;
	sgs_FTNode *node, *nname = NULL, *nargs = NULL, *nbody = NULL, *nclos = NULL;
	sgs_TokenList begin = SFTC_AT;
	
	SGS_FN_BEGIN;
	
	SFTC_NEXT;
	if( !inexp )
	{
		if( !SFTC_IS( SGS_ST_IDENT ) )
		{
			SFTC_PRINTERR( "Expected identifier after 'function'" );
			goto fail;
		}
	}
	
	if( SFTC_IS( SGS_ST_IDENT ) )
	{
		hasname = 1;
		nname = make_node( SGS_SFT_IDENT, SFTC_AT, NULL, NULL );
		SFTC_NEXT;
		if( SFTC_IS( SGS_ST_OP_MMBR ) )
		{
			nname = make_node( SGS_SFT_OPER, SFTC_AT, NULL, nname );
			SFTC_NEXT;
			if( !SFTC_IS( SGS_ST_IDENT ) )
			{
				SFTC_PRINTERR( "Expected identifier after 'function', identifier and '.'" );
				goto fail;
			}
			else
			{
				nname->child->next = make_node( SGS_SFT_IDENT, SFTC_AT, NULL, NULL );
				SFTC_NEXT;
			}
		}
	}
	
	if( !SFTC_IS( '(' ) )
	{
		if( !hasname )
			SFTC_PRINTERR( "Expected '(' after 'function'" );
		else
			SFTC_PRINTERR( "Expected '(' after 'function' and its name" );
		goto fail;
	}
	
	SFTC_NEXT;
	
	nargs = parse_arglist( F, ')' );
	if( !nargs ) goto fail;
	SFTC_NEXT;
	
	if( SFTC_ISKEY( "use" ) )
	{
		/* closure */
		SFTC_NEXT;
		if( !SFTC_IS( '(' ) )
		{
			SFTC_PRINTERR( "Expected '(' after 'use' in 'function'" );
			goto fail;
		}
		SFTC_NEXT;
		nclos = parse_arglist( F, ')' );
		if( !nclos ) goto fail;
		nclos->type = SGS_SFT_USELIST;
		SFTC_NEXT;
	}
	else
		nclos = make_node( SGS_SFT_USELIST, SFTC_AT, NULL, NULL );
	
	if( !SFTC_IS( '{' ) && !SFTC_IS( SGS_ST_OP_SET ) )
	{
		SFTC_PRINTERR( "Expected '{', '=' or 'use'" );
		goto fail;
	}
	
	if( SFTC_IS( SGS_ST_OP_SET ) )
	{
		SFTC_NEXT;
		nbody = parse_explist( F, ';' );
		if( !nbody ) goto fail;
		nbody->type = SGS_SFT_RETURN;
		SFTC_NEXT;
	}
	else
	{
		nbody = parse_stmt( F );
		if( !nbody ) goto fail;
	}
	
	nargs->next = nclos;
	nclos->next = nbody;
	nbody->next = nname;
	node = make_node( SGS_SFT_FUNC, begin, NULL, nargs );
	
	SGS_FN_END;
	return node;
	
fail:
	if( nname ) SFTC_DESTROY( nname );
	if( nargs ) SFTC_DESTROY( nargs );
	if( nclos ) SFTC_DESTROY( nclos );
	if( nbody ) SFTC_DESTROY( nbody );
	SFTC_SETERR;
	SGS_FN_END;
	return NULL;
}

SFTRET parse_command( SFTC, int multi )
{
	sgs_FTNode *nargs = NULL;
	sgs_TokenList begin = SFTC_AT;

	SGS_FN_BEGIN;
	SFTC_NEXT;
	
	nargs = parse_explist( F, ';' );
	if( !nargs ) goto fail;
	SFTC_NEXT;
	
	if( multi )
	{
		/* one argument to one function call */
		sgs_FTNode *r = NULL, *n = NULL, *p = nargs->child;
		
		if( !p )
		{
			if( nargs ) SFTC_DESTROY( nargs );
			SGS_FN_END;
			return make_node( SGS_SFT_BLOCK, begin, NULL, NULL );
		}
		
		nargs->child = NULL;
		SFTC_DESTROY( nargs );
		
		while( p )
		{
			sgs_FTNode* nn = make_node( SGS_SFT_FCALL, begin, NULL,
				make_node( SGS_SFT_IDENT, begin, 
					make_node( SGS_SFT_EXPLIST, p->token, NULL, p ),
						NULL ) );
			sgs_FTNode* pp = p;
			p = p->next;
			pp->next = NULL;
			if( !r )
				r = n = nn;
			else
				n = n->next = nn;
		}
		
		return make_node( SGS_SFT_BLOCK, begin, NULL, r );
	}
	else
	{
		/* one function call */
		sgs_FTNode* nname = make_node( SGS_SFT_IDENT, begin, nargs, NULL );
		SGS_FN_END;
		return make_node( SGS_SFT_FCALL, begin, NULL, nname );
	}
	
fail:
	if( nargs ) SFTC_DESTROY( nargs );
	SFTC_SETERR;
	SGS_FN_END;
	return NULL;
}

SFTRET parse_stmt( SFTC )
{
	sgs_FTNode* node;

	SGS_FN_BEGIN;

	if( SFTC_IS(0) )
	{
		SFTC_UNEXP;
		goto fail;
	}

	/* IF / ELSE */
	if( SFTC_ISKEY( "if" ) ) { node = parse_if( F ); SGS_FN_END; return node; }
	else if( SFTC_ISKEY( "else" ) )
	{
		SFTC_PRINTERR( "Found 'else' without matching 'if'" );
		goto fail;
	}
	/* WHILE */
	else if( SFTC_ISKEY( "while" ) ) { node = parse_while( F ); SGS_FN_END; return node; }
	/* DO / WHILE */
	else if( SFTC_ISKEY( "do" ) ) { node = parse_dowhile( F ); SGS_FN_END; return node; }
	/* FOR */
	else if( SFTC_ISKEY( "for" ) ) { node = parse_for( F ); SGS_FN_END; return node; }
	/* FOREACH */
	else if( SFTC_ISKEY( "foreach" ) ) { node = parse_foreach( F ); SGS_FN_END; return node; }
	/* BREAK */
	else if( SFTC_ISKEY( "break" ) )
	{
		sgs_TokenList orig = SFTC_AT;
		SFTC_NEXT;

		if( SFTC_IS( SGS_ST_NUMINT ) )
		{
			sgs_Int blev;
			SGS_AS_INTEGER( blev, SFTC_AT + 1 );
			if( blev < 1 || blev > 255 )
			{
				SFTC_PRINTERR( "Invalid break level (can be between 1 and 255)" );
				goto fail;
			}
			SFTC_NEXT;
		}
		
		if( !SFTC_IS( ';' ) )
		{
			SFTC_UNEXP;
			goto fail;
		}
		SFTC_NEXT;

		node = make_node( SGS_SFT_BREAK, orig, NULL, NULL );

		SGS_FN_END;
		return node;
	}
	/* CONTINUE */
	else if( SFTC_ISKEY( "continue" ) )
	{
		sgs_TokenList orig = SFTC_AT;
		SFTC_NEXT;

		if( SFTC_IS( SGS_ST_NUMINT ) )
		{
			sgs_Int blev;
			SGS_AS_INTEGER( blev, SFTC_AT + 1 );
			if( blev < 1 || blev > 255 )
			{
				SFTC_PRINTERR( "Invalid continue level (can be between 1 and 255)" );
				goto fail;
			}
			SFTC_NEXT;
		}
		
		if( !SFTC_IS( ';' ) )
		{
			SFTC_UNEXP;
			goto fail;
		}
		SFTC_NEXT;

		node = make_node( SGS_SFT_CONT, orig, NULL, NULL );

		SGS_FN_END;
		return node;
	}
	/* FUNCTION */
	else if( SFTC_ISKEY( "function" ) ) { node = parse_function( F, 0 ); SGS_FN_END; return node; }
	/* RETURN */
	else if( SFTC_ISKEY( "return" ) )
	{
		SFTC_NEXT;
		node = parse_explist( F, ';' );
		if( !node )
			goto fail;

		node->type = SGS_SFT_RETURN;

		SFTC_NEXT;
		SGS_FN_END;
		return node;
	}
	/* VAR / GLOBAL - reuse code in parse_exp */
	else if( SFTC_ISKEY( "var" ) || SFTC_ISKEY( "global" ) )
	{
		node = parse_exp( F, ";", 1 );
		if( !node )
			goto fail;
		if( !SFTC_IS( ';' ) )
		{
			SFTC_UNEXP;
			SFTC_DESTROY( node );
			goto fail;
		}

		SFTC_NEXT;
		SGS_FN_END;
		return node;
	}
	/* COMMAND HELPERS */
#define NOT_FCALL ( !sgsT_Next( F->at ) || '(' != *sgsT_Next( F->at ) )
	/* SIMPLE COMMANDS */
	else if((
		SFTC_IS_ID( "print" ) ||
		SFTC_IS_ID( "println" ) ||
		SFTC_IS_ID( "yield" )
		) && NOT_FCALL )
	{
		node = parse_command( F, 0 );
		SGS_FN_END;
		return node;
	}
	/* MULTIPLIED COMMANDS */
	else if( SFTC_IS_ID( "include" ) && NOT_FCALL )
	{
		node = parse_command( F, 1 );
		SGS_FN_END;
		return node;
	}
	/* BLOCK OF STATEMENTS */
	else if( SFTC_IS( SGS_ST_CBRKL ) )
	{
		SFTC_NEXT;
		node = parse_stmtlist( F, '}' );
		if( !node ) goto fail;

		SFTC_NEXT;
		SGS_FN_END;
		return node;
	}
	/* SEPARATED STATEMENTS */
	else
	{
		node = parse_explist( F, ';' );
		if( node )
		{
			SFTC_NEXT;
			SGS_FN_END;
			return node;
		}
		else
			goto fail;
	}

fail:
	SFTC_SETERR;
	SGS_FN_END;
	return NULL;
}

SFTRET parse_stmtlist( SFTC, char end )
{
	sgs_FTNode* stmtlist = make_node( SGS_SFT_BLOCK, NULL, NULL, NULL );
	sgs_FTNode* curstmt = NULL;

	SGS_FN_BEGIN;

	for(;;)
	{
		if( SFTC_IS( end ) )
		{
			break;
		}
		else if( SFTC_IS( 0 ) )
		{
			SFTC_UNEXP;
			SFTC_SETERR;
			goto fail;
		}
		else
		{
			sgs_FTNode* stmt = parse_stmt( F );
			if( curstmt )
				curstmt->next = stmt;
			else
				stmtlist->child = stmt;
			curstmt = stmt;
		}

		if( F->C->state & SGS_MUST_STOP )
			goto fail;
	}

	SGS_FN_END;
	return stmtlist;

fail:
	SFTC_DESTROY( stmtlist );
	SFTC_SETERR;
	SGS_FN_END;
	return NULL;
}

sgs_FTNode* sgsFT_Compile( SGS_CTX, sgs_TokenList tlist )
{
	sgs_FTNode* ret;
	FTComp F;
	{
		F.C = C;
		F.at = tlist;
		F.heap = F.heapend = _make_heap( C );
	}
	ret = parse_stmtlist( &F, 0 );
	if( !ret )
	{
		sgsFT_Destroy( C, F.heap );
		return NULL;
	}
	F.heap->child = ret;
	return F.heap;
}


