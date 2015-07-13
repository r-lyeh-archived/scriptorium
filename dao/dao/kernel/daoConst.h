/*
// Dao Virtual Machine
// http://www.daovm.net
//
// Copyright (c) 2006-2015, Limin Fu
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without modification,
// are permitted provided that the following conditions are met:
//
// * Redistributions of source code must retain the above copyright notice,
//   this list of conditions and the following disclaimer.
// * Redistributions in binary form must reproduce the above copyright notice,
//   this list of conditions and the following disclaimer in the documentation
//   and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED  BY THE COPYRIGHT HOLDERS AND  CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED  WARRANTIES,  INCLUDING,  BUT NOT LIMITED TO,  THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
// IN NO EVENT SHALL  THE COPYRIGHT HOLDER OR CONTRIBUTORS  BE LIABLE FOR ANY DIRECT,
// INDIRECT,  INCIDENTAL, SPECIAL,  EXEMPLARY,  OR CONSEQUENTIAL  DAMAGES (INCLUDING,
// BUT NOT LIMITED TO,  PROCUREMENT OF  SUBSTITUTE  GOODS OR  SERVICES;  LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION)  HOWEVER CAUSED  AND ON ANY THEORY OF
// LIABILITY,  WHETHER IN CONTRACT,  STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
// OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
// OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef DAO_CONST_H
#define DAO_CONST_H

#define DAO_MAX_PARAM      32
#define DAO_MAX_SECTDEPTH  64

#define DAO_KERNEL

#include"dao.h"
#include"daoBase.h"

enum DaoExtraTypes
{
	DAO_VARIANT = END_CORE_TYPES, /* variant or disjoint union type */
	DAO_CONSTANT ,
	DAO_VARIABLE ,
	DAO_ROUTBODY ,
	DAO_TYPEKERNEL ,
	DAO_CODEBLOCK ,

	DAO_PAR_NAMED ,   /* name:type */
	DAO_PAR_DEFAULT , /* name=type */
	DAO_PAR_VALIST ,  /* ... */

	/* bit (1<<6) to indicate inexact checking: */
	DAO_ANY = (1<<6)|0, /* any type */
	DAO_THT = (1<<6)|1, /* type holder type */
	DAO_UDT = (1<<6)|2, /* undefined type */

	END_EXTRA_TYPES 
};

enum DaoSubTypes
{
	DAO_ENUM_ANY = END_EXTRA_TYPES,
	DAO_ENUM_SYM ,
	DAO_ENUM_STATE ,
	DAO_ENUM_FLAG ,
	DAO_IFACE_ABS ,
	DAO_IFACE_CON ,
	DAO_CDATA_PTR ,  /* opaque C/C++ data, not owned by the wrapper */
	DAO_CDATA_CXX ,  /* opaque C/C++ data, owned by the wrapper */
	DAO_PAIR ,
	DAO_CFUNCTION ,
	DAO_ROUTINES ,
	DAO_INVAR ,
	END_SUB_TYPES
};

enum DaoContainerDataTypes
{
	DAO_DATA_NULL ,
	DAO_DATA_VALUE ,   /* garbage collectable items, managed by the container; */
	DAO_DATA_VALUE2 ,  /* same as DAO_DATA_VALUE, except more strict comparing as map keys; */
	DAO_DATA_VALUE3 ,  /* same as DAO_DATA_VALUE2,except more strict comparing as map keys; */
	DAO_DATA_VMCODE ,
	DAO_DATA_VMCODE2 , /* for DMap, compare code and operands only; */
	DAO_DATA_TOKEN ,   /* for DList only; */
	DAO_DATA_COMPLEX ,
	DAO_DATA_STRING ,
	DAO_DATA_ARRAY ,
	DAO_DATA_LIST ,
	DAO_DATA_MAP ,
	DAO_DATA_VOID2 /* a pair of pointer; */
};


/*
// Dao type matching states:
// -- These states give scores to each type of type matching.
//    Useful for resolving overloaded routines;
// -- For loose matching, the state with smaller score is used in case
//    that multiple states can be assigned to the same matching.
//    For example, matching a type holder type "@T" to "any" should give
//    DAO_MT_THTX instead of DAO_MT_ANY;
// -- Undefined types are treated as type holder types.
//    They simply hold some types that are not successfully inferred;
// -- DAO_MT_THT should have higher score than DAO_MT_SUB and DAO_MT_SIM.
//    Because DAO_MT_THT means starting a type association for the type
//    holder type, and should be considered more or less as precise as
//    DAO_MT_EQ;
// -- DAO_MT_EXACT is more precise than DAO_MT_EQ, as it means not only
//    type matching but also value matching.
*/
enum DaoTypeMatchState
{
	DAO_MT_NOT   = 0  , /* Type not matching; */
	DAO_MT_LOOSE = 10 , /* Loose matching not covered by the following cases; */
	DAO_MT_THTX  = 20 , /* Loose matching of a type holder type (THT) to any other type; */
	DAO_MT_ANYX  = 30 , /* Loose matching of the "any" type to any other type; */
	DAO_MT_EMPTY = 40 , /* Loose matching of an empty container value to a container type; */
	DAO_MT_ANY   = 50 , /* Matching of any type to the "any" type; */
	DAO_MT_SUB   = 60 , /* Matching of a sub type to a parent type; */
	DAO_MT_SIM   = 70 , /* Matching of a type to a compatible type (eg, int to float); */
	DAO_MT_THT   = 80 , /* Matching of any type to a type holder type; */
	DAO_MT_EQ    = 90 , /* Type precisely matching; */
	DAO_MT_EXACT = 100  /* Type and value precisely matching; */
};

enum DaoVarDeclaration
{
	DAO_DECL_LOCAL      = (1<<0), /* for compiling only */
	DAO_DECL_MEMBER     = (1<<1), /* for compiling only */
	DAO_DECL_GLOBAL     = (1<<2), /* for compiling only */
	DAO_DECL_STATIC     = (1<<3), /* for compiling only */
	DAO_DECL_VAR        = (1<<4), /* for compiling only */
	DAO_DECL_INVAR      = (1<<5), /* for compiling only */
	DAO_DECL_CONST      = (1<<7)  /* using the highest bit in the trait field */
};

/* Lowest bit set to 1 for constant storage: */
enum DaoVarStorage
{
	DAO_LOCAL_VARIABLE   = 0,
	DAO_LOCAL_CONSTANT   = 1,
	DAO_OBJECT_VARIABLE  = 2,
	DAO_CLASS_VARIABLE   = 4,
	DAO_CLASS_CONSTANT   = 5,
	DAO_GLOBAL_VARIABLE  = 6,
	DAO_GLOBAL_CONSTANT  = 7,
	DAO_STATIC_VARIABLE  = 8,
	DAO_CLOSURE_VARIABLE = 14 /* for compiling only; */
};

enum DaoValueTrait
{
	DAO_VALUE_CONST   = (1<<1), /* constant data object */
	DAO_VALUE_NOCOPY  = (1<<2), /* data object not for copying */
	DAO_VALUE_DELAYGC = (1<<3), /* objects with this trait are scanned less frequently by GC */
	DAO_VALUE_BROKEN  = (1<<4)  /* reference already broken (may not yet set to NULL) by GC */
};
enum DaoTypeAttribs
{
	DAO_TYPE_SPEC      = (1<<0), /* specializable type, with at least one type holder; */
	DAO_TYPE_UNDEF     = (1<<1), /* undefined type, with at least one undefined type; */
	DAO_TYPE_SELF      = (1<<2), /* routine type that has self parameter; */
	DAO_TYPE_VARIADIC  = (1<<3), /* variadic type (routine or tuple); */
	DAO_TYPE_PARNAMED  = (1<<4), /* name:type or name=type; */
	DAO_TYPE_SELFNAMED = (1<<5), /* self:type; */
	DAO_TYPE_CODESECT  = (1<<6)
};
enum DaoCaseMode
{
	DAO_CASE_ORDERED ,
	DAO_CASE_UNORDERED ,
	DAO_CASE_TYPES ,
	DAO_CASE_INTS , /* ordered integer cases; TODO optimize, enums */
	DAO_CASE_TABLE
};

enum DaoCallMode
{
	DAO_CALL_INIT   = (1<<8),  /* call to initialize a parent object; */
	DAO_CALL_NOSELF = (1<<9),  /* call without implicit self; */
	DAO_CALL_EXPAR  = (1<<10), /* expand the last parameter of tuple type; */
	DAO_CALL_BLOCK  = (1<<11), /* call with code block; */
	DAO_CALL_DECSUB = (1<<12), /* call decorated function; */
	DAO_CALL_ASYNC  = (1<<13), /* asynchronous call; */
	DAO_CALL_TAIL   = (1<<14), /* may do tail call; */
	DAO_CALL_FAST   = (1<<15)  /* may do fast call; */
};
enum DaoProcessPauseType
{
	DAO_PAUSE_NONE ,
	DAO_PAUSE_FUTURE_VALUE ,    /* future::value(); */
	DAO_PAUSE_FUTURE_WAIT ,     /* future::wait(); */
	DAO_PAUSE_CHANNEL_SEND ,    /* channel::send(); */
	DAO_PAUSE_CHANNEL_RECEIVE , /* channel::send(); */
	DAO_PAUSE_CHANFUT_SELECT ,  /* mt::select(); */
	DAO_PAUSE_COROUTINE_YIELD   /* coroutine; */
};

enum DaoFieldPermission
{
	DAO_PERM_NONE = 0,
	DAO_PERM_PRIVATE ,
	DAO_PERM_PROTECTED ,
	DAO_PERM_PUBLIC
};
enum DaoClassAttrib
{
	DAO_CLS_INVAR         = (1<<0),
	DAO_CLS_AUTO_INITOR   = (1<<1),
	DAO_CLS_PRIVATE_VAR   = (1<<2),
	DAO_CLS_PROTECTED_VAR = (1<<3),
	DAO_CLS_ASYNCHRONOUS  = (1<<4)
};
enum DaoRoutineAttrib
{
	DAO_ROUT_PARSELF   = (1<<0),   /* need self parameter */
	DAO_ROUT_INVAR     = (1<<1),   /* invariable method */
	DAO_ROUT_STATIC    = (1<<2),   /* static function */
	DAO_ROUT_INTERFACE = (1<<3),   /* interface function */
	DAO_ROUT_DEFER     = (1<<4),   /* defer block as a closure */
	DAO_ROUT_DEFER_RET = (1<<5),   /* defer block that may return values */
	DAO_ROUT_CODESECT  = (1<<6),   /* code section routine */
	DAO_ROUT_DECORATOR = (1<<7),   /* function decorator */
	DAO_ROUT_INITOR    = (1<<8),   /* class/ctype constructor */
	DAO_ROUT_CASTOR    = (1<<9),   /* user defined casting method */
	DAO_ROUT_MIXIN     = (1<<10),  /* methods from mixin */
	DAO_ROUT_MAIN      = (1<<11),  /* main function */
	DAO_ROUT_PRIVATE   = (1<<12),  /* private method */
	DAO_ROUT_PROTECTED = (1<<13),  /* protected method */
	DAO_ROUT_REUSABLE  = (1<<14)   /* stack data for the routine is reusable */
};

enum DaoConstEvalMode
{
	DAO_CONST_EVAL_METHDEF  = 1,
	DAO_CONST_EVAL_GETVALUE = 2
};

enum DaoRoutineModes
{
	DAO_ROUT_MODE_DEBUG = 1
};

enum DaoTypeKernelAttribs
{
	DAO_TYPEKERNEL_FREE = 1 ,
	DAO_TYPEKERNEL_TEMPLATE = 2
};


enum DaoGlobalConstOffset
{
	DVR_NSC_NONE = LOOKUP_BIND( DAO_GLOBAL_CONSTANT, DAO_PERM_PUBLIC, 0, 1 ) ,
	DVR_NSC_MAIN = LOOKUP_BIND( DAO_GLOBAL_CONSTANT, DAO_PERM_PUBLIC, 0, 4 )
};

enum DaoExceptionType
{
	DAO_EXCEPTION = 0,
	DAO_WARNING ,
	DAO_ERROR ,

	DAO_ERROR_FIELD ,
	DAO_ERROR_FIELD_NOTEXIST ,
	DAO_ERROR_FIELD_NOTPERMIT ,
	DAO_ERROR_FLOAT ,
	DAO_ERROR_FLOAT_DIVBYZERO ,
	DAO_ERROR_FLOAT_OVERFLOW ,
	DAO_ERROR_FLOAT_UNDERFLOW ,
	DAO_ERROR_INDEX ,
	DAO_ERROR_INDEX_OUTOFRANGE ,
	DAO_ERROR_KEY ,
	DAO_ERROR_KEY_NOTEXIST ,
	DAO_ERROR_PARAM ,
	DAO_ERROR_SYNTAX ,
	DAO_ERROR_TYPE ,
	DAO_ERROR_VALUE ,
	DAO_ERROR_FILE ,

	ENDOF_BASIC_EXCEPT
};

enum DaoArithOperType
{
	DAO_OPER_NONE ,
	DAO_OPER_ASSN ,
	DAO_OPER_ASSN_ADD ,
	DAO_OPER_ASSN_SUB ,
	DAO_OPER_ASSN_MUL ,
	DAO_OPER_ASSN_DIV ,
	DAO_OPER_ASSN_MOD ,
	DAO_OPER_ASSN_AND ,
	DAO_OPER_ASSN_OR ,
	DAO_OPER_ASSN_XOR ,

	DAO_OPER_IF ,
	DAO_OPER_COLON ,

	DAO_OPER_LLT,
	DAO_OPER_GGT,

	DAO_OPER_BIT_AND ,
	DAO_OPER_BIT_OR ,
	DAO_OPER_BIT_XOR ,

	DAO_OPER_AND ,
	DAO_OPER_OR ,

	DAO_OPER_IN ,
	DAO_OPER_NOTIN ,

	DAO_OPER_LT ,
	DAO_OPER_GT ,
	DAO_OPER_EQ ,
	DAO_OPER_NE ,
	DAO_OPER_LE ,
	DAO_OPER_GE ,
	DAO_OPER_TEQ ,
	DAO_OPER_TISA ,

	DAO_OPER_ADD ,
	DAO_OPER_SUB ,
	DAO_OPER_DIV ,
	DAO_OPER_MUL ,
	DAO_OPER_MOD ,
	DAO_OPER_POW ,

	DAO_OPER_NOT ,
	DAO_OPER_INCR ,
	DAO_OPER_DECR ,
	DAO_OPER_NEGAT ,
	DAO_OPER_TILDE
};

enum DaoCtInfoId
{
	DAO_CTW_NULL =0,
	DAO_CTW_INTERNAL ,
	DAO_CTW_LIMIT_PAR_NUM ,
	DAO_CTW_IS_EXPECTED ,
	DAO_CTW_WAS_DEFINED ,
	DAO_WARN_STATEMENT_SEPARATION ,
	DAO_WARN_ASSIGNMENT ,
	DAO_WARN_GET_SETTER ,
	DAO_VALUE_WAS_USED ,
	DAO_SYMBOL_POSSIBLY_UNDEFINED ,
	DAO_SYMBOL_NOT_DEFINED ,
	DAO_SYMBOL_WAS_DEFINED ,
	DAO_SYMBOL_NEED_CONSTANT ,
	DAO_SYMBOL_NEED_CLASS ,
	DAO_SYMBOL_NEED_CLASS_CTYPE ,
	DAO_SYMBOL_NEED_INTERFACE ,
	DAO_SYMBOL_NEED_BINDABLE ,
	DAO_TOKEN_NEED_STRING ,
	DAO_TOKEN_NEED_NAME ,
	DAO_TOKEN_EXPECTING ,
	DAO_TOKEN_NOT_FOUND ,
	DAO_TOKEN_NOT_EXPECTED ,
	DAO_TOKENS_NOT_PAIRED ,
	DAO_TOKENS_NOT_EXPECTED ,
	DAO_EXPR_NEED_CONST_NUMBER ,
	DAO_EXPR_NEED_CONST_EXPR ,
	DAO_EXPR_MODIFY_CONSTANT ,
	DAO_INTERFACE_NOT_COMPATIBLE,
	DAO_MISSING_INTERFACE_METHOD,
	DAO_FAILED_INTERFACE_BIND ,
	DAO_FAILED_INSTANTIATION ,
	DAO_UNDEFINED_SCOPE_NAME ,
	DAO_FIELD_NOT_EXIST ,
	DAO_FIELD_NOT_PERMIT ,
	DAO_INVALID_FIELD ,
	DAO_INVALID_TOKEN ,
	DAO_INVALID_PATH ,
	DAO_INVALID_ACCESS ,
	DAO_INVALID_STORAGE ,
	DAO_INVALID_SHARED ,
	DAO_INVALID_LITERAL ,
	DAO_INVALID_INDEX ,
	DAO_INVALID_TYPE_NAME ,
	DAO_INVALID_TYPE_FORM ,
	DAO_INVALID_REFERENCE ,
	DAO_INVALID_EXPRESSION ,
	DAO_INVALID_STATEMENT ,
	DAO_INVALID_SELF_IN_STATIC,
	DAO_INVALID_UNCLOSED_SCOPE ,
	DAO_INVALID_SCOPE_ENDING ,
	DAO_INVALID_FUNCTIONAL ,
	DAO_INVALID_DECO_PATTERN ,
	DAO_INVALID_DECLARATION ,
	DAO_INVALID_DEFINITION ,
	DAO_INVALID_ENUM_DEFINITION ,
	DAO_INVALID_CLASS_DEFINITION ,
	DAO_INVALID_FUNCTION_DEFINITION ,
	DAO_INVALID_NAMESPACE_DEFINITION ,
	DAO_INVALID_INTERFACE_DEFINITION ,
	DAO_INVALID_FUNCTION_DECORATION ,
	DAO_INVALID_INTERFACE_TARGET ,
	DAO_INVALID_PARENT_INTERFACE ,
	DAO_INVALID_PARENT_CLASS ,
	DAO_INVALID_MIXIN_CLASS ,
	DAO_INVALID_IMPORT_STMT ,
	DAO_INVALID_TYPE_ALIAS ,
	DAO_INVALID_BINDING ,
	DAO_INVALID_TYPEDEF ,
	DAO_INVALID_FOR ,
	DAO_INVALID_FORIN ,
	DAO_INVALID_PARAM_LIST ,
	DAO_PARAM_INVALID ,
	DAO_PARAM_INVALID_MUTABLE ,
	DAO_PARAM_NEED_SEPARATOR ,
	DAO_PARAM_MIDDLE_VALIST ,
	DAO_PARAM_REDUNDANT_TYPE ,
	DAO_PARAM_NEED_TYPE ,
	DAO_PARAM_NEED_DEFAULT ,
	DAO_PARAM_VARIABLE_DEFAULT ,
	DAO_PARAM_INVALID_DEFAULT ,
	DAO_PARAM_MIDDLE_DEFAULT ,
	DAO_PARAM_IMPROPER_DEFAULT ,
	DAO_PARAM_TOO_MANY ,
	DAO_PARAM_INVALID_RETURN ,
	DAO_INCOMPLETE_INTERFACE_IMPL ,
	DAO_TOO_MANY_PARENT_TYPES ,
	DAO_SECTION_TOO_DEEP ,
	DAO_STATEMENT_IN_CLASS ,
	DAO_STATEMENT_IN_INTERFACE ,
	DAO_STATEMENT_OUT_OF_CONTEXT ,
	DAO_VARIABLE_OUT_OF_CONTEXT ,
	DAO_VARIABLE_WITHOUT_INIT ,
	DAO_TYPE_NOT_MATCHING ,
	DAO_TYPE_PRESENTED ,
	DAO_TYPE_EXPECTED ,
	DAO_TYPE_NO_DEFAULT ,
	DAO_ROUT_NEED_RETURN_TYPE ,
	DAO_ROUT_INVALID_DECORATOR ,
	DAO_ROUT_INVALID_OPERATOR ,
	DAO_ROUT_INVALID_DECO_PARAM ,
	DAO_ROUT_INVALID_RETURN ,
	DAO_ROUT_NEED_IMPLEMENTATION ,
	DAO_ROUT_REDUNDANT_IMPLEMENTATION ,
	DAO_ROUT_MISPLACED_IMPLEMENTATION ,
	DAO_ROUT_NOT_DECLARED ,
	DAO_ROUT_WAS_IMPLEMENTED ,
	DAO_ROUT_DEFINED_SIGNATURE ,
	DAO_ROUT_DECLARED_SIGNATURE ,
	DAO_ROUT_USED_SIGNATURE ,
	DAO_ROUT_WRONG_SIGNATURE ,
	DAO_CONSTR_NOT_DEFINED ,
	DAO_CASE_NOT_VALID ,
	DAO_CASE_NOT_TYPE ,
	DAO_CASE_NOT_CONSTANT ,
	DAO_CASE_DUPLICATED ,
	DAO_DEFAULT_DUPLICATED ,
	DAO_EVAL_EXCEPTION ,
	DAO_LOAD_CYCLIC ,
	DAO_DISABLED_REGEX ,
	DAO_DISABLED_NUMARRAY ,
	DAO_DISABLED_DECORATOR ,
	DAO_CTW_LOAD_INVALID ,
	DAO_CTW_LOAD_INVA_MOD_NAME ,
	DAO_CTW_LOAD_FAILED ,
	DAO_CTW_LOAD_CANCELLED ,
	DAO_CTW_ENUM_INVALID ,
	DAO_CTW_ENUM_LIMIT ,
	DAO_CTW_INVA_SYNTAX ,
	DAO_CTW_INV_CONST_EXPR ,
	DAO_CTW_OBSOLETE_SYNTAX ,
	DAO_CTW_END
};

extern const char* getCtInfo( int tp );
extern const char* getRtInfo( int tp );

extern const char* const daoExceptionNames[];
extern const char* const daoExceptionTitles[];

extern const char* const coreTypeNames[];
extern const char *const daoBitBoolArithOpers[];
extern const char *const daoBitBoolArithOpers2[];

DAO_DLL const char *daoRoutineCodeHeader;
DAO_DLL const char *daoRoutineCodeFormat;

#endif
