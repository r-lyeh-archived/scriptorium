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

#ifndef DAO_VMCODE_H
#define DAO_VMCODE_H

#include"daoBase.h"

/* TODO: more detailed description! */
enum DaoOpcode
{
	DVM_DATA  , /* Create primitive data:  C = B;  A: data type id;  B: direct value; */
	DVM_GETCL , /* Get local const:  C = A:::B;  B, index;  A=0, implicit;  A=1, explicit; */
	DVM_GETCK , /* Get class const:  C = A:::B;  B, index;  A=0; */
	DVM_GETCG , /* Get global const:  C = A:::B;  B, index;  A=0; */
	DVM_GETVH , /* Get host variable:  C = A:::B;  B, index;  A, section level; */
	DVM_GETVS , /* Get closure variable:  C = A:::B;  B, index;  A=0; */
	DVM_GETVO , /* Get instance object variable:  C = A:::B;  B, index;  A=0; */
	DVM_GETVK , /* Get class static variable:  C = A:::B;  B, index;  A=0; */
	DVM_GETVG , /* Get global/static variable:  C = A:::B;  B, index;  A=0/1; */
	DVM_GETI  , /* Get item(s):  C = A[B];  A, register;  B, register; */
	DVM_GETDI , /* Get item(s):  C = A[B];  A, register;  B, direct index; */
	DVM_GETMI , /* Get item(s):  C = A[A+1, ..., A+B]; */
	DVM_GETF  , /* Get field:  C = A.B or C = A::B; */
	DVM_SETVH , /* Set host variable:  C:::B = A;  B, index;  C, section level; */
	DVM_SETVS , /* Set closure variable:  C:::B = A;  B, index;  C=0; */
	DVM_SETVO , /* Set instance object variable:  C:::B = A;  B, index;  C=0; */
	DVM_SETVK , /* Set class static variable:  C:::B = A;  B, index;  C=0; */
	DVM_SETVG , /* Set global/static variable:  C:::B = A;  C: C1, 0/1; C2, decl; C3, invar; */
	DVM_SETI  , /* Set item(s):  C[B] = A; */
	DVM_SETDI , /* Set item(s):  C[B] = A;  B, direct index; */
	DVM_SETMI , /* Set item(s):  C[C+1, ..., C+B] = A; */
	DVM_SETF  , /* Set field:  C.B = A or C::B = A; */
	DVM_LOAD ,  /* Put local value A as reference at C; */
	DVM_MOVE ,  /* Move A to C:  C = A;  B: B1, explicit; B2, decl; B3, invar; */
	DVM_UNTAG , /* Untag A of type X|none to X type at C; */
	DVM_CAST ,  /* Cast A to B and store at C:  C = (B)A;  B, local const index; */
	DVM_NOT ,   /* Not:  C = ! A; */
	DVM_MINUS , /* Unary minus:  C = - A; */
	DVM_TILDE , /* Bitwise not:  C = ~ A */
	DVM_SIZE ,  /* Size:  C = % A; */
	DVM_ADD ,   /* Addition:  C = A + B;  */
	DVM_SUB ,   /* Subtraction:  C = A - B;  */
	DVM_MUL ,   /* Multiplication:  C = A * B;  */
	DVM_DIV ,   /* Division:  C = A / B;  */
	DVM_MOD ,   /* Modulo:  C = A % B;  */
	DVM_POW ,   /* Power:  C = A ** B; */
	DVM_AND ,   /* And:  C = A && B; */
	DVM_OR ,    /* Or:  C = A || B; */
	DVM_LT ,    /* Less than:  C = A <  B; */
	DVM_LE ,    /* Less or equal:  C = A <= B; */
	DVM_EQ ,    /* Equal:  C = A == B; */
	DVM_NE ,    /* Not equal:  C = A != B; */
	DVM_IN ,    /* In:  C = A in B; */
	DVM_BITAND , /* Bit and:  C = A & B */
	DVM_BITOR ,  /* Bit or:  C = A | B */
	DVM_BITXOR , /* Bit xor:  C = A ^ B */
	DVM_BITLFT , /* Bit shift left:  C = A << B */
	DVM_BITRIT , /* Bit shift right:  C = A >> B */
	DVM_SAME ,   /* Same: C = A ?= B, same type; A, B, both are data objects or type objects; */
	DVM_ISA ,    /* Isa: C = A ?< B; B, type; A (data) is a B; A (type) is a sub type of B; */
	DVM_NAMEVA , /* C = (A = B): name A, local constant, value B, local register */
	DVM_PAIR ,   /* C = A : B; create a pair of index, as a tuple; */
	DVM_TUPLE ,  /* tuple: C = ( A, A+1, ..., A+B-1 ); items can be: name=value */
	DVM_LIST ,   /* list: C = { A, A+1, ..., A+B-1 }; */
	DVM_MAP ,    /* map:  C = { A => A+1, ..., A+B-2 => A+B-1 }; if B==0, empty; */
	DVM_VECTOR , /* vector: C = [ A, A+1, ..., A+B-1 ]; */
	DVM_MATRIX , /* matrix: C=[A,..,A+c-1;..;A+c*(r-1),..,A+c*r-1]; B=rc;r,c:8-bits each.*/
	DVM_PACK ,    /* packing: A::{ A+1, ..., A+B }; A, routine, class or type object; */
	DVM_MPACK ,   /* packing: (A+1).A::{ A+2, ..., A+B }; (A+1).A, routine, class or type; */
	DVM_ROUTINE , /* create a function, possibly with closure */
	DVM_GOTO  ,   /* go to B; */
	DVM_SWITCH , /* A: variable, B: location of default block, C: number of cases */
	DVM_CASE ,   /* A: constant of the case, B: location of the case block, C: case mode */
	DVM_ITER ,   /* create an iterator at C for A if B==0, else test an array of iterators; */
	DVM_TEST ,   /* if A, go to the next one; else, goto B-th instruction; */
	DVM_MATH ,   /* C = A( B ); A: sin,cos,...; B: double,complex */
	DVM_CALL ,   /* function call: C = A( A+1, A+2, ..., A+B ); If B==0, no parameters; */
	DVM_MCALL ,  /* method call: x.y(...), pass x as the first parameter; */
	DVM_RETURN , /* return A,A+1,..,A+B-1; B==0: no returns; C==1: return from functional; */
	DVM_YIELD , /* yield A, A+1,.., A+B-1; return data at C when resumed; */
	DVM_SECT ,  /* code section label, parameters: A,A+1,...,A+B-1; C, #explicit params; */
	DVM_JITC ,  /* run Just-In-Time compiled Code A, and skip the next B instructions; */

	/* optimized opcodes: */
	DVM_DATA_B , DVM_DATA_I , DVM_DATA_F , DVM_DATA_C ,

	DVM_GETCL_B , DVM_GETCL_I , DVM_GETCL_F , DVM_GETCL_C ,
	DVM_GETCK_B , DVM_GETCK_I , DVM_GETCK_F , DVM_GETCK_C ,
	DVM_GETCG_B , DVM_GETCG_I , DVM_GETCG_F , DVM_GETCG_C ,

	DVM_GETVH_B , DVM_GETVH_I , DVM_GETVH_F , DVM_GETVH_C ,
	DVM_GETVS_B , DVM_GETVS_I , DVM_GETVS_F , DVM_GETVS_C ,
	DVM_GETVO_B , DVM_GETVO_I , DVM_GETVO_F , DVM_GETVO_C ,
	DVM_GETVK_B , DVM_GETVK_I , DVM_GETVK_F , DVM_GETVK_C ,
	DVM_GETVG_B , DVM_GETVG_I , DVM_GETVG_F , DVM_GETVG_C ,

	DVM_SETVH_BB , DVM_SETVH_II , DVM_SETVH_FF , DVM_SETVH_CC ,
	DVM_SETVS_BB , DVM_SETVS_II , DVM_SETVS_FF , DVM_SETVS_CC ,
	DVM_SETVO_BB , DVM_SETVO_II , DVM_SETVO_FF , DVM_SETVO_CC ,
	DVM_SETVK_BB , DVM_SETVK_II , DVM_SETVK_FF , DVM_SETVK_CC ,
	DVM_SETVG_BB , DVM_SETVG_II , DVM_SETVG_FF , DVM_SETVG_CC ,

	DVM_MOVE_BB , DVM_MOVE_BI , DVM_MOVE_BF ,
	DVM_MOVE_IB , DVM_MOVE_II , DVM_MOVE_IF ,
	DVM_MOVE_FB , DVM_MOVE_FI , DVM_MOVE_FF ,

	DVM_MOVE_CF ,
	DVM_MOVE_CC , /* complex = complex */
	DVM_MOVE_SS , /* string = string */
	DVM_MOVE_PP , /* C = A; C and A are of the same non-primitive type, A must not be const; */
	DVM_MOVE_XX , /* C = A; C and A are of the same type, or C is of any type; */

	DVM_NOT_B ,
	DVM_NOT_I ,
	DVM_NOT_F ,
	DVM_MINUS_I ,
	DVM_MINUS_F ,
	DVM_MINUS_C ,
	DVM_TILDE_I ,
	DVM_TILDE_C ,
	/*
	// C = A + B: will be compiled into: ADD, MOVE,
	// and the C operand of ADD is always an intermediate data with type to be inferred,
	// so it is only necessary to add specialized opcode according the A,B operands.
	*/
	DVM_AND_BBB ,
	DVM_OR_BBB ,
	DVM_LT_BBB ,
	DVM_LE_BBB ,
	DVM_EQ_BBB ,
	DVM_NE_BBB ,

	DVM_ADD_III ,
	DVM_SUB_III ,
	DVM_MUL_III ,
	DVM_DIV_III ,
	DVM_MOD_III ,
	DVM_POW_III ,

	DVM_AND_BII ,
	DVM_OR_BII ,
	DVM_LT_BII ,
	DVM_LE_BII ,
	DVM_EQ_BII ,
	DVM_NE_BII ,

	DVM_BITAND_III ,
	DVM_BITOR_III ,
	DVM_BITXOR_III ,
	DVM_BITLFT_III ,
	DVM_BITRIT_III ,

	DVM_ADD_FFF ,
	DVM_SUB_FFF ,
	DVM_MUL_FFF ,
	DVM_DIV_FFF ,
	DVM_MOD_FFF ,
	DVM_POW_FFF ,

	DVM_AND_BFF ,
	DVM_OR_BFF ,
	DVM_LT_BFF ,
	DVM_LE_BFF ,
	DVM_EQ_BFF ,
	DVM_NE_BFF ,

	DVM_ADD_CCC ,
	DVM_SUB_CCC ,
	DVM_MUL_CCC ,
	DVM_DIV_CCC ,

	DVM_EQ_BCC ,
	DVM_NE_BCC ,

	/* string */
	DVM_ADD_SSS ,
	DVM_LT_BSS ,
	DVM_LE_BSS ,
	DVM_EQ_BSS ,
	DVM_NE_BSS ,

	/* single indexing C=A[B]: GETI and MOVE */
	/* index should be integer, may be casted from float/double by the typing system */
	DVM_GETI_LI ,   /* get item : C = A[B]; X=list<X>[int] */
	DVM_SETI_LI ,   /* set item : C[B] = A; list<X>[int]=X, or list<any>[int]=X; */
	DVM_GETI_SI ,   /* get char from a string: string[int] */
	DVM_SETI_SII ,  /* set char to a string: string[int]=int */
	DVM_GETI_LBI ,  /* get item : C = A[B]; list<bool>[int] */
	DVM_GETI_LII ,  /* get item : C = A[B]; list<int>[int] */
	DVM_GETI_LFI ,  /* get item : C = A[B]; list<float>[int] */
	DVM_GETI_LCI ,  /* get item : C = A[B]; list<complex>[int] */
	DVM_GETI_LSI ,  /* get item : C = A[B]; list<string>[int] */
	DVM_SETI_LBIB , /* set item : C[B] = A; list<int>[bool]=bool */
	DVM_SETI_LIII , /* set item : C[B] = A; list<int>[int]=int */
	DVM_SETI_LFIF , /* set item : C[B] = A;  */
	DVM_SETI_LCIC , /* set item : C[B] = A;  */
	DVM_SETI_LSIS , /* set item : C[B] = A;  */

	DVM_GETI_ABI ,  /* get item : C = A[B]; array<bool>[int] */
	DVM_GETI_AII ,  /* get item : C = A[B]; array<int>[int] */
	DVM_GETI_AFI ,  /* get item : C = A[B]; array<float>[int] */
	DVM_GETI_ACI ,  /* get item : C = A[B]; array<complex>[int] */
	DVM_SETI_ABIB , /* set item : C[B] = A;  */
	DVM_SETI_AIII , /* set item : C[B] = A;  */
	DVM_SETI_AFIF , /* set item : C[B] = A;  */
	DVM_SETI_ACIC , /* set item : C[B] = A;  */

	DVM_GETI_TI , /* get item : C = A[B]; tuple<...>[int] */
	DVM_SETI_TI , /* set item : C[B] = A; tuple<...>[int]=X; */

	/* access field by constant index; specialized from GETI[const] or GETF */
	DVM_GETF_TB , /* get boolean field by constant index; */
	DVM_GETF_TI , /* get integer field by constant index; */
	DVM_GETF_TF , /* get float field by constant index; */
	DVM_GETF_TC , /* get complex field by constant index; */
	DVM_GETF_TX , /* get type checked field by constant index; */
	DVM_SETF_TBB , /* set boolean field to boolean. */
	DVM_SETF_TII , /* set integer field to integer. */
	DVM_SETF_TFF , /* set float field to float. */
	DVM_SETF_TCC , /* set complex field to double. */
	DVM_SETF_TSS , /* set string field to string. */
	DVM_SETF_TPP , /* set item: C[B]=A or C.B=A; tuple<..X..>[int]=X, or tuple<..any..>[int]=X; */
	DVM_SETF_TXX , /* set item: C[B]=A or C.B=A; tuple<..X..>[int]=X, or tuple<..any..>[int]=X; */

	/* multiple indexing a[i,j] */
	DVM_GETMI_ABI , /* array: get item(s) : C = A[B]; B,C: integer, A boolean array; */
	DVM_GETMI_AII , /* array: get item(s) : C = A[B]; B,C: integer, A integer array; */
	DVM_GETMI_AFI , /* array: get item(s) : C = A[B]; B,C: integer, A float array; */
	DVM_GETMI_ACI , /* array: get item(s) : C = A[B]; B,C: integer, A complex array; */
	DVM_SETMI_ABIB , /* set item(s) : C[B] = A; A,B: integer, C boolean array; */
	DVM_SETMI_AIII , /* set item(s) : C[B] = A; A,B: integer, C integer array; */
	DVM_SETMI_AFIF , /* set item(s) : C[B] = A; A,B: integer, C float array; */
	DVM_SETMI_ACIC , /* set item(s) : C[B] = A; A,B: integer, C complex array; */

	DVM_GETF_CX , /* get complex field: real/imag; */
	DVM_SETF_CX , /* set complex field: real/imag; */

	/* setters and getters */
	/* get/set member of class instance by index instead of name: */
	DVM_GETF_KC , /* get class field, const; code: GET Member Field Const*/
	DVM_GETF_KG , /* get class field, global */
	DVM_GETF_OC , /* get class instance field, const; code: GET Member Field Const*/
	DVM_GETF_OG , /* get class instance field, global */
	DVM_GETF_OV , /* get class instance field, variable */
	DVM_SETF_KG , /* set class static field: field type equals to opa type, or is "any" type; */
	DVM_SETF_OG , /* set class static field: field type equals to opa type, or is "any" type; */
	DVM_SETF_OV , /* set class instance field: field type equals to opa type, or is "any" type; */

	/* C=A.B : GETF and MOVE */
	DVM_GETF_KCB , DVM_GETF_KCI , DVM_GETF_KCF , DVM_GETF_KCC ,
	DVM_GETF_KGB , DVM_GETF_KGI , DVM_GETF_KGF , DVM_GETF_KGC ,
	DVM_GETF_OCB , DVM_GETF_OCI , DVM_GETF_OCF , DVM_GETF_OCC ,
	DVM_GETF_OGB , DVM_GETF_OGI , DVM_GETF_OGF , DVM_GETF_OGC ,
	DVM_GETF_OVB , DVM_GETF_OVI , DVM_GETF_OVF , DVM_GETF_OVC ,
	/* C.B=A specialize according to both: C.B and A */
	DVM_SETF_KGBB , DVM_SETF_KGII , DVM_SETF_KGFF , DVM_SETF_KGCC ,
	DVM_SETF_OGBB , DVM_SETF_OGII , DVM_SETF_OGFF , DVM_SETF_OGCC ,
	DVM_SETF_OVBB , DVM_SETF_OVII , DVM_SETF_OVFF , DVM_SETF_OVCC ,

	DVM_TEST_B ,
	DVM_TEST_I ,
	DVM_TEST_F ,

	DVM_MATH_B ,
	DVM_MATH_I ,
	DVM_MATH_F ,

	DVM_CAST_B ,
	DVM_CAST_I ,
	DVM_CAST_F ,
	DVM_CAST_C ,
	DVM_CAST_S ,
	DVM_CAST_VE , /* cast variant to one of its distinct enum types; */
	DVM_CAST_VX , /* cast variant to one of its distinct and non primitve member types; */

	DVM_ISA_ST , /* check against simple types: int, float, double, complex, long, string; */

	DVM_TUPLE_SIM ,

	DVM_NULL
};
typedef enum DaoOpcode DaoOpcode;


/*
// Additional notes for the virtual machine instructions:
//
// DVM_GETVH and DVM_SETVH:
//
// These two instructions are for accessing up-values from closures or from code sections.
// The value of operand ::a (DVM_GETVH) or ::c (DVM_SETVH) instructs what kind of up-value
// is accessed; and the value of operand ::b tells which up-value is accessed.
//
// Zero value of the operand ::a or ::c indicates the up-value is accessed from a closure.
// The up-value is a local variable of the routine in which the closure was created.
//
// A non-zero value of ::a or ::c indicates the up-value
// TODO
//
// DVM_ROUTINE
// TODO
*/


/*
// Extra vmcode type for compiling:
// They are used to setup proper indexing, branching or jumping. After this,
// they will be removed or replaced with proper instructions from DaoOpcode.
*/
enum DaoOpcodeExtra
{
	DVM_NOP = DVM_NULL + 1 ,
	DVM_LABEL ,
	DVM_LOAD2 ,
	DVM_LOOP ,
	DVM_BRANCH ,
	DVM_DO ,
	DVM_LBRA ,
	DVM_RBRA ,
	DVM_DEFAULT ,
	DVM_UNUSED
};

/*
// Enum modes for DVM_LIST, DVM_VECTOR, DVM_MAP, DVM_TUPLE:
*/
enum DaoEnumCodeModes
{
	DVM_ENUM_MODE0 , /* Default mode; */
	DVM_ENUM_MODE1 , /* LIST/VECTOR: arithmetic progression; MAP: hashing; TUPLE: arguments; */
	DVM_ENUM_MODE2 , /* TUPLE: code section arguments; */
	DVM_ENUM_MODE3   /* Reserved; */
};


enum DaoMathFunct
{
	DVM_MATH_CEIL ,
	DVM_MATH_FLOOR ,
	DVM_MATH_ABS ,
	DVM_MATH_ARG ,
	DVM_MATH_IMAG ,
	DVM_MATH_NORM ,
	DVM_MATH_REAL ,
	DVM_MATH_ACOS ,
	DVM_MATH_ASIN ,
	DVM_MATH_ATAN ,
	DVM_MATH_COS ,
	DVM_MATH_COSH ,
	DVM_MATH_EXP ,
	DVM_MATH_LOG ,
	DVM_MATH_SIN ,
	DVM_MATH_SINH ,
	DVM_MATH_SQRT ,
	DVM_MATH_TAN ,
	DVM_MATH_TANH
};

enum DaoFunctMeth
{
	DVM_FUNCT_APPLY ,
	DVM_FUNCT_SORT ,
	DVM_FUNCT_MAP ,
	DVM_FUNCT_COLLECT ,
	DVM_FUNCT_ASSOCIATE ,
	DVM_FUNCT_FOLD ,
	DVM_FUNCT_FIND ,
	DVM_FUNCT_SELECT ,
	DVM_FUNCT_INDEX ,
	DVM_FUNCT_COUNT ,
	DVM_FUNCT_ITERATE ,
	DVM_FUNCT_STRING ,
	DVM_FUNCT_ARRAY ,
	DVM_FUNCT_LIST ,
	DVM_FUNCT_NULL
};

enum DaoCodeType
{
	DAO_CODE_NOP ,      /*  Local variable operands: None; */
	DAO_CODE_GETC ,     /*  C; local variable operand; */
	DAO_CODE_GETG ,     /*  C;     */
	DAO_CODE_GETU ,     /*  B,C;   */
	DAO_CODE_GETF ,     /*  A,C;   */
	DAO_CODE_GETI ,     /*  A,B,C; */
	DAO_CODE_GETM ,     /*  C,A,A+1,...,A+B; */
	DAO_CODE_SETG ,     /*  A;   */
	DAO_CODE_SETU ,     /*  A,B;   */
	DAO_CODE_SETF ,     /*  A,C;   */
	DAO_CODE_SETI ,     /*  A,B,C; */
	DAO_CODE_SETM ,     /*  A,C,C+1,...,C+B; */
	DAO_CODE_MOVE ,     /*  A,C;   */
	DAO_CODE_UNARY ,    /*  A,C;   */
	DAO_CODE_BINARY ,   /*  A,B,C; */
	DAO_CODE_UNARY2 ,   /*  B,C;   */
	DAO_CODE_MATRIX ,   /*  C,A,A+1,...,A+N-1; where N=(B>>8)*(B&0xff); */
	DAO_CODE_ENUM ,     /*  C,A,A+1,...,A+B-1; */
	DAO_CODE_ENUM2 ,    /*  C,A,A+1,...,A+B; */
	DAO_CODE_CALL ,     /*  C,A,A+1,...,A+N; where N=B&0xff*/
	DAO_CODE_ROUTINE ,  /*  C,A,A+1,...,A+B; */
	DAO_CODE_YIELD ,    /*  C,A,A+1,...,A+B-1; */
	DAO_CODE_EXPLIST ,  /*  A,A+1,...,A+B-1; */
	DAO_CODE_BRANCH ,   /*  A;   */
	DAO_CODE_JUMP
};

enum DaoCodeState
{
	DAO_CODE_BREAKING  = 1
};

struct DaoVmCode
{
	unsigned short  code;    /* opcode; */
	unsigned short  a, b, c; /* operands; */

#ifdef DAO_USE_CODE_STATE
	unsigned short  state;   /* state; */
#endif
};

DAO_DLL const char* DaoVmCode_GetOpcodeName( int code );
DAO_DLL ushort_t    DaoVmCode_GetOpcodeBase( int code );
DAO_DLL uchar_t     DaoVmCode_CheckPermutable( int code );
DAO_DLL uchar_t     DaoVmCode_GetOpcodeType( DaoVmCode *self );
DAO_DLL DaoVmCode   DaoVmCode_CheckOperands( DaoVmCode *self );
DAO_DLL int         DaoVmCode_MayCreateReference( int code );

DAO_DLL DaoVmCode*  DArray_PushCode( DArray *self, DaoVmCode code );


struct DaoVmCodeX
{
	unsigned short  code;    /* opcode */
	unsigned short  a, b, c; /* register ids for operands */
	unsigned short  state;   /* state; */
	unsigned short  level;   /* lexical level */
	unsigned short  line;    /* line number in the source file */
	unsigned int    first;   /* first token */
	unsigned short  middle;  /* middle token, with respect to first */
	unsigned short  last;    /* last token, with respect to first */
};
void DaoVmCode_Print( DaoVmCode self, char *buffer );
void DaoVmCodeX_Print( DaoVmCodeX self, char *annot, char *buffer );


#endif
