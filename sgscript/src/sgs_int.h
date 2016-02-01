
#ifndef SGS_INT_H_INCLUDED
#define SGS_INT_H_INCLUDED


#include "sgscript.h"
#include "sgs_util.h"


/*
	L I M I T S
	
	! some limits may cancel or reduce others, ..
	.. these are kept for the purposes of knowing the size of variables ..
	.. to use in various systems
	
	- number of constants in a function: 0 - 65535
	- number of instructions in a function: 0 - 65535
	- string size: 0 - 2^31-1
	- max. jump distance: 32767 instructions
	- max. loop depth: 65535 nested loops
		(no need to test for it since instruction count ..
		.. effectively limits this further)
	- bytecode size limit: 14 - 2^32-1
	- source size limit: 0 - 2^31-1
	- variable size limit: string size + overhead (<=256 bytes)
	- item type value limits: 8 bits
	
	- identifier size: 0 - 255
	- argument count: 0 - 255
	- returned value count: 0 - 255
	- total closure count in one function: 0 - 255
	- temporary variable count (incl. args): 0 - 255
	- useful line count limits in source file: 0 - 32767
	
	- stack size: 0 - 2^31-1
	- closure stack size: 0 - 2^31-1
	- array size: 0 - 2^31-1
	- hash table size: 0 - 2^31-1
*/


#ifdef __cplusplus
extern "C" {
#endif



/*
	Token stream
	<1 byte: type> <additional data> <2 bytes: line number>
*/
/*     special          id */
#define SGS_ST_NULL	    '\0'
#define SGS_ST_RBRKL    '('
#define SGS_ST_RBRKR    ')'
#define SGS_ST_SBRKL    '['
#define SGS_ST_SBRKR    ']'
#define SGS_ST_CBRKL    '{'
#define SGS_ST_CBRKR    '}'
#define SGS_ST_ARGSEP   ','
#define SGS_ST_STSEP    ';'
#define SGS_ST_PICKSEP  ':'
/*     other            id    additional data */
#define SGS_ST_IDENT    'N' /* 1 byte (string size), N bytes (string), not null-terminated */
#define SGS_ST_KEYWORD  'K' /* same as IDENT */
#define SGS_ST_NUMREAL  'R' /* 8 bytes (double) */
#define SGS_ST_NUMINT   'I' /* 8 bytes (int64) */
#define SGS_ST_STRING   'S' /* 4 bytes (string size), N bytes (string), not null-terminated */
/*     operators        id    type  */
#define SGS_ST_OP_SEQ   200 /* ===  */
#define SGS_ST_OP_SNEQ  201 /* !==  */
#define SGS_ST_OP_EQ    202 /* ==   */
#define SGS_ST_OP_NEQ   203 /* !=   */
#define SGS_ST_OP_LEQ   204 /* <=   */
#define SGS_ST_OP_GEQ   205 /* >=   */
#define SGS_ST_OP_LESS  206 /* <    */
#define SGS_ST_OP_GRTR  207 /* >    */
#define SGS_ST_OP_RWCMP 208 /* <=>  */
#define SGS_ST_OP_ADDEQ 209 /* +=   */
#define SGS_ST_OP_SUBEQ 210 /* -=   */
#define SGS_ST_OP_MULEQ 211 /* *=   */
#define SGS_ST_OP_DIVEQ 212 /* /=   */
#define SGS_ST_OP_MODEQ 213 /* %=   */
#define SGS_ST_OP_ANDEQ 214 /* &=   */
#define SGS_ST_OP_OREQ  215 /* |=   */
#define SGS_ST_OP_XOREQ 216 /* ^=   */
#define SGS_ST_OP_LSHEQ 217 /* <<=  */
#define SGS_ST_OP_RSHEQ 218 /* >>=  */
#define SGS_ST_OP_BLAEQ 219 /* &&=  */
#define SGS_ST_OP_BLOEQ 220 /* ||=  */
#define SGS_ST_OP_NLOEQ 221 /* ??=  */
#define SGS_ST_OP_CATEQ 222 /* $=   */
#define SGS_ST_OP_SET   223 /* =    */
#define SGS_ST_OP_ERSUP 224 /* @    */
#define SGS_ST_OP_BLAND 225 /* &&   */
#define SGS_ST_OP_BLOR  226 /* ||   */
#define SGS_ST_OP_NLOR  227 /* ??   */
#define SGS_ST_OP_ADD   228 /* +    */
#define SGS_ST_OP_SUB   229 /* -    */
#define SGS_ST_OP_MUL   230 /* *    */
#define SGS_ST_OP_DIV   231 /* /    */
#define SGS_ST_OP_MOD   232 /* %    */
#define SGS_ST_OP_AND   233 /* &    */
#define SGS_ST_OP_OR    234 /* |    */
#define SGS_ST_OP_XOR   235 /* ^    */
#define SGS_ST_OP_LSH   236 /* <<   */
#define SGS_ST_OP_RSH   237 /* >>   */
#define SGS_ST_OP_MMBR  238 /* .    */
#define SGS_ST_OP_CAT   239 /* $    */
#define SGS_ST_OP_NOT   240 /* !    */
#define SGS_ST_OP_INV   241 /* ~    */
#define SGS_ST_OP_INC   242 /* ++   */
#define SGS_ST_OP_DEC   243 /* --   */

#define SGS_ST_ISOP( chr )      ( (chr) >= 200 && (chr) <= 243 )
#define SGS_ST_OP_UNARY( chr )  ( (chr) == SGS_ST_OP_ERSUP || (chr) == SGS_ST_OP_ADD || \
	(chr) == SGS_ST_OP_SUB || ( (chr) >= SGS_ST_OP_NOT && (chr) <= SGS_ST_OP_DEC ) )
#define SGS_ST_OP_BINARY( chr ) ( (chr) >= 200 && (chr) <= 239 && (chr) != 224 )
#define SGS_ST_OP_ASSIGN( chr ) ( (chr) >= 209 && (chr) <= 223 )
#define SGS_ST_OP_BINMUL( chr ) ( (chr) == SGS_ST_OP_MUL || (chr) == SGS_ST_OP_DIV || (chr) == SGS_ST_OP_MOD )
#define SGS_ST_OP_BINADD( chr ) ( (chr) == SGS_ST_OP_ADD || (chr) == SGS_ST_OP_SUB )
#define SGS_ST_OP_BINOPS( chr ) ( (chr) >= SGS_ST_OP_AND && (chr) <= SGS_ST_OP_RSH )
#define SGS_ST_OP_COMP( chr )   ( (chr) >= 200 && (chr) <= 208 )
#define SGS_ST_OP_BOOL( chr )   ( (chr) == SGS_ST_OP_BLAEQ || (chr) == SGS_ST_OP_BLOEQ || \
	(chr) == SGS_ST_OP_BLAND || (chr) == SGS_ST_OP_BLOR )
#define SGS_ST_OP_FNN( chr )    ( (chr) == SGS_ST_OP_NLOEQ || (chr) == SGS_ST_OP_NLOR )

#define SGS_ST_ISSPEC( chr )    sgs_isoneof( (chr), "()[]{},;:" )

#define SGS_ST_READINT( tgt, pos )   SGS_AS_INT32( tgt, pos )
#define SGS_ST_READLN( tgt, pos )    SGS_AS_( tgt, pos, sgs_LineNum )


typedef unsigned char sgs_TokenType;
typedef unsigned char* sgs_TokenList;

SGS_APIFUNC int sgsT_IsKeyword( sgs_TokenList tok, const char* text );
SGS_APIFUNC int sgsT_IsIdent( sgs_TokenList tok, const char* text );

SGS_APIFUNC sgs_TokenList sgsT_Gen( SGS_CTX, const char* code, size_t length );
SGS_APIFUNC void          sgsT_Free( SGS_CTX, sgs_TokenList tlist );
SGS_APIFUNC sgs_TokenList sgsT_Next( sgs_TokenList tok );
SGS_APIFUNC sgs_LineNum   sgsT_LineNum( sgs_TokenList tok );

SGS_APIFUNC size_t sgsT_ListSize( sgs_TokenList tlist );
SGS_APIFUNC size_t sgsT_ListMemSize( sgs_TokenList tlist );

SGS_APIFUNC void sgsT_TokenString( SGS_CTX, sgs_MemBuf* out, sgs_TokenList tlist, sgs_TokenList tend, int xs );

SGS_APIFUNC void sgsT_DumpToken( sgs_TokenList tok );
SGS_APIFUNC void sgsT_DumpList( sgs_TokenList tlist, sgs_TokenList tend );


/*
	Function tree
*/

#define SGS_SFT_NULL    0
/* data */
#define SGS_SFT_IDENT   1
#define SGS_SFT_KEYWORD 2
#define SGS_SFT_CONST   3
/* expression parts */
#define SGS_SFT_OPER    4
#define SGS_SFT_OPER_P  5 /* post-op (inc/dec) */
#define SGS_SFT_FCALL   6
#define SGS_SFT_INDEX   7
#define SGS_SFT_MIDXSET 8
#define SGS_SFT_MPROPSET 9
/* statement data */
#define SGS_SFT_ARGMT   10
#define SGS_SFT_ARGLIST 11
#define SGS_SFT_VARLIST 12
#define SGS_SFT_GVLIST  13
#define SGS_SFT_USELIST 14
#define SGS_SFT_EXPLIST 15
#define SGS_SFT_ARRLIST 16
#define SGS_SFT_DCTLIST 17
#define SGS_SFT_MAPLIST 18
#define SGS_SFT_RETURN  19
/* statement types */
#define SGS_SFT_BLOCK   21
#define SGS_SFT_IFELSE  22
#define SGS_SFT_WHILE   23
#define SGS_SFT_DOWHILE 24
#define SGS_SFT_FOR     25
#define SGS_SFT_FOREACH 26
#define SGS_SFT_BREAK   27
#define SGS_SFT_CONT    28
#define SGS_SFT_FUNC    30
/* threading addons */
#define SGS_SFT_THRCALL 40 /* thread */
#define SGS_SFT_STHCALL 41 /* subthread */
#define SGS_SFT_HEAPBIT 255

typedef struct _sgs_FTNode sgs_FTNode;
struct _sgs_FTNode
{
	sgs_TokenList token;
	sgs_FTNode*   next;
	sgs_FTNode*   child;
	int           type;
};

SGS_APIFUNC void sgsFT_Destroy( SGS_CTX, sgs_FTNode* tree );

SGS_APIFUNC sgs_FTNode* sgsFT_Compile( SGS_CTX, sgs_TokenList tlist );
SGS_APIFUNC void sgsFT_Dump( sgs_FTNode* tree );



/*
	Intermediate function
	- keeps the function data between compiler stages
*/

typedef
struct _sgs_CompFunc
{
	sgs_MemBuf consts;
	sgs_MemBuf code;
	sgs_MemBuf lnbuf;
	uint8_t gotthis; /* bool */
	uint8_t numargs; /* guaranteed to be <= 255 by a test in `preparse_arglist` */
	uint8_t numtmp; /* reg. count (0-255, incl. numargs) - numargs (0-255) */
	uint8_t numclsr;
}
sgs_CompFunc;


/* - bytecode generator */
SGS_APIFUNC sgs_iFunc* sgsBC_ConvertFunc( SGS_CTX, sgs_CompFunc* nf,
	const char* funcname, size_t fnsize, sgs_LineNum lnum );
SGS_APIFUNC sgs_CompFunc* sgsBC_Generate( SGS_CTX, sgs_FTNode* tree );
SGS_APIFUNC void sgsBC_Dump( sgs_CompFunc* func );
SGS_APIFUNC void sgsBC_DumpEx( const char* constptr, size_t constsize,
	const char* codeptr, size_t codesize );
SGS_APIFUNC void sgsBC_Free( SGS_CTX, sgs_CompFunc* func );


/*
	Serialized bytecode
*/
SGS_APIFUNC int sgsBC_Func2Buf( SGS_CTX, sgs_CompFunc* func, sgs_MemBuf* outbuf );

/* assumes headers have already been validated (except size) but are still in the buffer */
SGS_APIFUNC const char* sgsBC_Buf2Func( SGS_CTX, const char* fn,
	const char* buf, size_t size, sgs_CompFunc** outfunc );

/* validates header size and bytes one by one (except last flag byte)
-- will return header_size on success and failed byte position on failure */
SGS_APIFUNC int sgsBC_ValidateHeader( const char* buf, size_t size );
#define SGS_HEADER_SIZE 14
#define SGS_MIN_BC_SIZE 8 + SGS_HEADER_SIZE
#define SGSBC_FLAG_LITTLE_ENDIAN 0x01


/*
	Virtual Machine
*/

#define SGS_CONSTVAR( x ) ((x)&0x100)
#define SGS_CONSTENC( x ) ((x)|0x100)
#define SGS_CONSTDEC( x ) ((x)&0xff)

#define SGS_INT_ERRSUP_INC 1
#define SGS_INT_ERRSUP_DEC 2
#define SGS_INT_RESET_WAIT_TIMER 3

typedef enum sgs_Instruction_e
{
	SGS_SI_NOP = 0,

	SGS_SI_PUSH,     /* (B:src)                 push register/constant */
	SGS_SI_INT,      /* (C:type)                invoke a system state change */

	SGS_SI_RETN,     /* (A:N)                   exit current frame of execution, preserve N output arguments */
	SGS_SI_JUMP,     /* (E:off)                 add to instruction pointer */
	SGS_SI_JMPT,     /* (C:src, E:off)          jump (add to instr.ptr.) if true */
	SGS_SI_JMPF,     /* (C:src, E:off)          jump (add to instr.ptr.) if false */
	SGS_SI_JMPN,     /* (C:src, E:off)          jump (add to instr.ptr.) if null */
	SGS_SI_CALL,     /* (A:exp, B:from, C:to)   call a variable */

	SGS_SI_FORPREP,  /* (A:out, B:src)          retrieves the iterator to work the object */
	SGS_SI_FORLOAD,  /* (A:iter, B:oky, C:ovl)  retrieves current key & value from iterator */
	SGS_SI_FORJUMP,  /* (C:iter, E:off)         advances and jumps if next key exists */

	SGS_SI_LOADCONST,/* (C:out, E:off)          load a constant to a register */
	SGS_SI_GETVAR,   /* (A:out, B:name)         <varname> => <value> */
	SGS_SI_SETVAR,   /* (B:name, C:src)         <varname> <value> => set <value> to <varname> */
	SGS_SI_GETPROP,  /* (A:out, B:var, C:name)  <var> <prop> => <var> */
	SGS_SI_SETPROP,  /* (A:var, B:name, C:src)  <var> <prop> <value> => set a <prop> of <var> to <value> */
	SGS_SI_GETINDEX, /* -- || -- */
	SGS_SI_SETINDEX, /* -- || -- */

	/* closures */
	SGS_SI_GENCLSR,  /* (A:N)                   generates closure variables for the stack frame */
	SGS_SI_PUSHCLSR, /* (A:src)                 pick closure variable for closure creation */
	SGS_SI_MAKECLSR, /* (A:out, B:from, C:N)    creates a closure object containing closure vars */
	SGS_SI_GETCLSR,  /* (A:out, B:which)        loads data from the specified closure */
	SGS_SI_SETCLSR,  /* (B:which, C:src)        stores data to the specified closure */

	/* operators */
	/*
		A: (A:out, B:s1, C:s2)
		B: (A:out, B:s1)
	*/
	SGS_SI_SET,      /* B */
	SGS_SI_MCONCAT,  /* (A:out, B:N) */
	SGS_SI_CONCAT,   /* A */
	SGS_SI_NEGATE,   /* B */
	SGS_SI_BOOL_INV,
	SGS_SI_INVERT,

	SGS_SI_INC,      /* B */
	SGS_SI_DEC,
	SGS_SI_ADD,      /* A */
	SGS_SI_SUB,
	SGS_SI_MUL,
	SGS_SI_DIV,
	SGS_SI_MOD,

	SGS_SI_AND,      /* A */
	SGS_SI_OR,
	SGS_SI_XOR,
	SGS_SI_LSH,
	SGS_SI_RSH,

	SGS_SI_SEQ,      /* A */
	SGS_SI_SNEQ,
	SGS_SI_EQ,
	SGS_SI_NEQ,
	SGS_SI_LT,
	SGS_SI_GTE,
	SGS_SI_GT,
	SGS_SI_LTE,
	SGS_SI_RAWCMP,

	/* specials */
	SGS_SI_ARRAY,    /* (C:out, E:args) */
	SGS_SI_DICT,     /* -- || -- */
	SGS_SI_MAP,      /* -- || -- */
	SGS_SI_RSYM,     /* (B:name, C:var)         performs dual registration to symbol table */
	SGS_SI_COTRT,    /* (A:to, B:from)          sets A to true if `from` is finished */
	SGS_SI_COTRF,    /* (A:to, B:from)          sets A to false if `from` is not finished */
	SGS_SI_COABORT,  /* (C:arg, E:from)         if arg = true, look for COTR* and abort threads */
	SGS_SI_YLDJMP,   /* (C:arg, E:off)          if arg = false, yield and jump */

	SGS_SI_COUNT
}
sgs_Instruction;


typedef uint32_t sgs_instr_t;

/*
	instruction data: 32 bits
	OOOOOOAAAAAAAABBBBBBBBBCCCCCCCCC
	OOOOOOEEEEEEEEEEEEEEEEECCCCCCCCC
*/

#define SGS_INSTR_SIZE    4

#define SGS_INSTR_OFF_OP  0
#define SGS_INSTR_OFF_A   6
#define SGS_INSTR_OFF_B   14
#define SGS_INSTR_OFF_C   23
#define SGS_INSTR_OFF_E   6

#define SGS_INSTR_MASK_OP 0x003f /* OP: 6 bits */
#define SGS_INSTR_MASK_A  0x00ff /* A:  8 bits */
#define SGS_INSTR_MASK_B  0x01ff /* B:  9 bits */
#define SGS_INSTR_MASK_C  0x01ff /* C:  9 bits */
#define SGS_INSTR_MASK_E  0x0001ffff /* E: 17 bits */

#define SGS_INSTR_GET_OP( x )  ((int) (((x) >> SGS_INSTR_OFF_OP) & SGS_INSTR_MASK_OP))
#define SGS_INSTR_GET_A( x )   ((int) (((x) >> SGS_INSTR_OFF_A ) & SGS_INSTR_MASK_A ))
#define SGS_INSTR_GET_B( x )   ((int) (((x) >> SGS_INSTR_OFF_B ) & SGS_INSTR_MASK_B ))
#define SGS_INSTR_GET_C( x )   ((int) (((x) >> SGS_INSTR_OFF_C ) & SGS_INSTR_MASK_C ))
#define SGS_INSTR_GET_E( x )   ((int) (((x) >> SGS_INSTR_OFF_E ) & SGS_INSTR_MASK_E ))

#define SGS_INSTR_MAKE_OP( x ) ((sgs_instr_t) (((x) & SGS_INSTR_MASK_OP) << SGS_INSTR_OFF_OP))
#define SGS_INSTR_MAKE_A( x )  ((sgs_instr_t) (((x) & SGS_INSTR_MASK_A ) << SGS_INSTR_OFF_A ))
#define SGS_INSTR_MAKE_B( x )  ((sgs_instr_t) (((x) & SGS_INSTR_MASK_B ) << SGS_INSTR_OFF_B ))
#define SGS_INSTR_MAKE_C( x )  ((sgs_instr_t) (((x) & SGS_INSTR_MASK_C ) << SGS_INSTR_OFF_C ))
#define SGS_INSTR_MAKE_E( x )  ((sgs_instr_t) (((x) & SGS_INSTR_MASK_E ) << SGS_INSTR_OFF_E ))

#define SGS_INSTR_MAKE( op, a, b, c ) \
	( SGS_INSTR_MAKE_OP( op ) | SGS_INSTR_MAKE_A( a ) | SGS_INSTR_MAKE_B( b ) | SGS_INSTR_MAKE_C( c ) )
#define SGS_INSTR_MAKE_EX( op, ex, c ) \
	( SGS_INSTR_MAKE_OP( op ) | SGS_INSTR_MAKE_E( ex ) | SGS_INSTR_MAKE_C( c ) )
#define SGS_INSTR_RECOMB_E( a, b ) ( ( ( b ) << 8 ) | ( a ) )


struct _sgs_iFunc
{
	int32_t refcount;
	uint32_t size;
	uint32_t instr_off;
	uint8_t gotthis;
	uint8_t numargs;
	uint8_t numtmp;
	uint8_t numclsr;
	sgs_LineNum linenum;
	sgs_LineNum* lineinfo;
	sgs_iStr* sfuncname;
	sgs_iStr* sfilename;
};

SGS_CASSERT( sizeof(sgs_Variable) % 4 == 0, variable_object_chaining_issue );
SGS_CASSERT( sizeof(sgs_iFunc) % 4 == 0, ifunc_object_chaining_issue );
SGS_CASSERT( sizeof(sgs_iStr) % 4 == 0, istr_object_storage_compat_issue );

#define sgs_func_consts( pfn )   ((sgs_Variable*)SGS_ASSUME_ALIGNED(((sgs_iFunc*)(pfn))+1,16))
#define sgs_func_bytecode( pfn ) ((sgs_instr_t*)(sgs_func_consts(pfn)+pfn->instr_off/sizeof(sgs_Variable)))
#define sgs_func_c_consts( pfn )   ((const sgs_Variable*)SGS_ASSUME_ALIGNED(((const sgs_iFunc*)(pfn))+1,16))
#define sgs_func_c_bytecode( pfn ) \
	((const sgs_instr_t*)(sgs_func_c_consts(pfn)+pfn->instr_off/sizeof(sgs_Variable)))


typedef struct _sgs_Closure sgs_Closure;
struct _sgs_Closure
{
	int32_t refcount;
	sgs_Variable var;
};


typedef struct _sgs_ShCtx sgs_ShCtx;
#define SGS_SHCTX sgs_ShCtx* S
#define SGS_SHCTX_USE SGS_SHCTX = C->shared


/* VM interface */
void sgsVM_VarCreateString( SGS_CTX, sgs_Variable* out, const char* str, sgs_SizeVal len );
void sgsVM_VarDestroyObject( SGS_CTX, sgs_VarObj* O );

size_t sgsVM_VarSize( const sgs_Variable* var );
void sgsVM_VarDump( const sgs_Variable* var );

void sgsVM_StackDump( SGS_CTX );

int sgsVM_PushStackFrame( SGS_CTX, sgs_Variable* func );
int sgsVM_VarCall( SGS_CTX, sgs_Variable* var, int args, int clsr, int* outrvc, int gotthis );
void sgsVM_PushClosures( SGS_CTX, sgs_Closure** cls, int num );


sgs_Variable* sgsVM_VarMake_Dict();

int sgsVM_RegStdLibs( SGS_CTX );

int sgs_specfn_call( SGS_CTX );
int sgs_specfn_apply( SGS_CTX );


/*
	Context handling
*/

typedef struct _sgs_BreakInfo sgs_BreakInfo;
struct _sgs_BreakInfo
{
	sgs_BreakInfo* next;
	uint32_t jdoff;  /* jump data offset */
	uint16_t numlp;  /* which loop */
	uint8_t  iscont; /* is a "continue"? */
};

/* register / constant position */
typedef int32_t sgs_rcpos_t;

typedef
struct _sgs_FuncCtx
{
	int32_t func;
	sgs_rcpos_t regs, lastreg;
	sgs_MemBuf vars;
	sgs_MemBuf gvars;
	sgs_MemBuf clsr;
	int inclsr, outclsr, syncdepth;
	int32_t loops;
	sgs_BreakInfo* binfo;
}
sgs_FuncCtx;

typedef
struct _sgs_ObjPoolItem
{
	sgs_VarObj* obj;
	uint32_t appsize;
}
sgs_ObjPoolItem;

typedef sgs_Variable* sgs_VarPtr;

#define SGS_SF_METHOD  0x01
#define SGS_SF_HASTHIS 0x02
#define SGS_SF_ABORTED 0x04
#define SGS_SF_REENTER 0x08
#define SGS_SF_PAUSED  0x10

struct _sgs_StackFrame
{
	sgs_Variable    func;
	const uint32_t* code;
	const uint32_t* iptr;
	const uint32_t* iend;
	const uint32_t* lptr;
	sgs_Variable*   cptr;
	const char*     nfname;
	sgs_StackFrame* prev;
	sgs_StackFrame* next;
	sgs_StkIdx argbeg;
	sgs_StkIdx argend;
	sgs_StkIdx argsfrom;
	sgs_StkIdx stkoff;
	sgs_StkIdx clsoff;
	int32_t constcount;
	int32_t errsup;
	uint8_t argcount;
	uint8_t inexp;
	uint8_t flags;
};

struct _sgs_ShCtx
{
	uint32_t      version;
	sgs_Context*  ctx_root;
	sgs_Context*  state_list;
	int32_t       statecount;
	
	/* script file system */
	sgs_ScriptFSFunc sfs_fn;
	void*         sfs_ctx;
	
	/* output */
	sgs_OutputFunc output_fn; /* output function */
	void*         output_ctx; /* output context */
	sgs_OutputFunc erroutput_fn; /* error output function */
	void*         erroutput_ctx; /* error output context */
	
	/* memory */
	sgs_MemFunc   memfunc;
	void*         mfuserdata;
	size_t        memsize;
	size_t        numallocs;
	size_t        numfrees;
	size_t        numblocks;
	
	/* > object info */
	sgs_VarObj*   objs;
	int32_t       objcount;
	/* >> object GC */
	uint8_t       redblue;
	uint16_t      gcrun;
	/* >> object pool */
	sgs_ObjPoolItem* objpool_data;
	int32_t       objpool_size;
	
	/* more pools */
	sgs_Context* ctx_pool;
	sgs_StackFrame* sf_pool;
	int ctx_pool_size;
	int sf_pool_size;
	
	/* tables / cache */
	sgs_VHTable   typetable; /* type interface table */
	sgs_VHTable   stringtable; /* string constant caching hash table */
	sgs_VHTable   ifacetable; /* interface generator => object table */
	
	/* > _R (global registry) */
	sgs_VarObj*   _R;
	sgs_VarObj*   _SYM;
	sgs_VarObj*   _INC;
};

/* Virtual machine state */
#define SGS_HAS_ERRORS          0x00010000
#define SGS_MUST_STOP          (0x00020000 | SGS_HAS_ERRORS)
#define SGS_STATE_LASTFUNCPAUSE 0x0002
#define SGS_SERIALIZE_MODE2     0x0004
#define SGS_STATE_DESTROYING    0x0010
#define SGS_STATE_LASTFUNCABORT 0x0020
#define SGS_STATE_INSIDE_API    0x0040
#define SGS_STATE_COROSTART     0x0080 /* function is pushed to stack */

struct _sgs_Context
{
	int32_t       refcount;
	sgs_ShCtx*    shared;
	sgs_Context*  prev;
	sgs_Context*  next;
	
	void*         serialize_state; /* current serialization state */
	
	/* info output */
	int32_t       minlev;
	int32_t       apilev;
	sgs_MsgFunc   msg_fn;  /* messaging function */
	void*         msg_ctx; /* messaging context */
	int           last_errno;
	int           object_arg;
	
	/* hook */
	sgs_HookFunc  hook_fn;
	void*         hook_ctx;
	
	/* compilation */
	uint32_t      state;
	sgs_FuncCtx*  fctx;      /* ByteCodeGen */
	const char*   filename;  /* filename of currently compiled code */
	
	/* virtual machine */
	/* > coop micro-threading */
	sgs_Context*  parent; /* owning (parent) context */
	sgs_VarObj*   _T; /* subthreads */
	sgs_VarObj*   _E; /* end events */
	sgs_Real      wait_timer; /* sync/race */
	
	/* > main stack */
	sgs_VarPtr    stack_base;
	uint32_t      stack_mem;
	sgs_VarPtr    stack_off;
	sgs_VarPtr    stack_top;
	
	/* > closure pointer stack */
	sgs_Closure** clstk_base;
	sgs_Closure** clstk_off;
	sgs_Closure** clstk_top;
	uint32_t      clstk_mem;
	
	/* > stack frame info */
	sgs_StackFrame* sf_first;
	sgs_StackFrame* sf_last;
	int           sf_count;
	int           num_last_returned;
	
	/* > _G (global variable dictionary) */
	sgs_VarObj*   _G;
};

#define SGS_STACKFRAMESIZE ((sgs_StkIdx)(C->stack_top - C->stack_off))


#ifdef SGS_INTERNAL_STRINGTABLES

static const char* sgs_ErrNames[] =
{
	"SUCCESS", "ENOTFND", "ECOMP", "<UNUSED>",
	"ENOTSUP", "EBOUNDS", "EINVAL", "EINPROC"
};

static const char* sgs_VarNames[] =
{
	"null",
	"bool",
	"int",
	"real",
	"string",
	"function",
	"C function",
	"object",
	"ptr",
	"thread",
};

static const char* sgs_OpNames[] =
{
	"nop", "push", "int",
	"return", "jump", "jump_if_true", "jump_if_false", "jump_if_null", "call",
	"for_prep", "for_load", "for_jump", "loadconst", "getvar", "setvar",
	"getprop", "setprop", "getindex", "setindex",
	"genclsr", "pushclsr", "makeclsr", "getclsr", "setclsr",
	"set", "mconcat", "concat", "negate", "bool_inv", "invert",
	"inc", "dec", "add", "sub", "mul", "div", "mod",
	"and", "or", "xor", "lsh", "rsh",
	"seq", "sneq", "eq", "neq", "lt", "gte", "gt", "lte", "rawcmp",
	"array", "dict", "map", "rsym", "cotrt", "cotrf", "coabort", "yldjmp",
};

#endif


sgs_Context* sgsCTX_ForkState( SGS_CTX, int copystate );
void sgsCTX_FreeState( SGS_CTX );
sgs_StackFrame* sgsCTX_AllocFrame( SGS_CTX );
void sgsCTX_FreeFrame( SGS_CTX, sgs_StackFrame* F );

void sgsSTD_PostInit( SGS_CTX );
SGSBOOL sgsSTD_MakeArray( SGS_CTX, sgs_Variable* out, sgs_SizeVal cnt );
SGSBOOL sgsSTD_MakeDict( SGS_CTX, sgs_Variable* out, sgs_SizeVal cnt );
SGSBOOL sgsSTD_MakeMap( SGS_CTX, sgs_Variable* out, sgs_SizeVal cnt );
void sgsSTD_MakeClosure( SGS_CTX, sgs_Variable* func, uint32_t clc );
void sgsSTD_ThreadsFree( SGS_CTX );
void sgsSTD_ThreadsGC( SGS_CTX );
void sgsSTD_RegistryInit( SGS_CTX );
void sgsSTD_RegistryFree( SGS_CTX );
void sgsSTD_RegistryGC( SGS_CTX );
void sgsSTD_RegistryIter( SGS_CTX, int subtype, sgs_VHTVar** outp, sgs_VHTVar** outpend );
void sgsSTD_GlobalInit( SGS_CTX );
void sgsSTD_GlobalFree( SGS_CTX );
SGSBOOL sgsSTD_GlobalGet( SGS_CTX, sgs_Variable* out, sgs_Variable* idx );
SGSBOOL sgsSTD_GlobalSet( SGS_CTX, sgs_Variable* idx, sgs_Variable* val );
void sgsSTD_GlobalGC( SGS_CTX );
void sgsSTD_GlobalIter( SGS_CTX, sgs_VHTVar** outp, sgs_VHTVar** outpend );



/* STRUCTS */

typedef struct sgsstd_array_header_s
{
	sgs_SizeVal size;
	sgs_SizeVal mem;
	sgs_Variable* data;
}
sgsstd_array_header_t;

typedef struct sgsstd_array_iter_s
{
	sgs_Variable ref;
	sgs_SizeVal size;
	sgs_SizeVal off;
}
sgsstd_array_iter_t;

extern SGS_APIFUNC sgs_ObjInterface sgsstd_array_iface[1];
extern SGS_APIFUNC sgs_ObjInterface sgsstd_array_iter_iface[1];
extern SGS_APIFUNC sgs_ObjInterface sgsstd_dict_iface[1];
extern SGS_APIFUNC sgs_ObjInterface sgsstd_dict_iter_iface[1];
extern SGS_APIFUNC sgs_ObjInterface sgsstd_map_iface[1];
extern SGS_APIFUNC sgs_ObjInterface sgsstd_closure_iface[1];




#ifdef __cplusplus
}
#endif


#endif /* SGS_INT_H_INCLUDED */
