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

#include"daoVmcode.h"

typedef struct DaoVmCodeInfo DaoVmCodeInfo;

struct DaoVmCodeInfo
{
	const char     *name;
	unsigned short  base;
	unsigned char   type;
	unsigned char   perm; /* Used only by parser for compiling expression lists: */
};

static DaoVmCodeInfo dao_code_infolist[] =
{
	{ "DATA",       DVM_DATA,       DAO_CODE_GETC,    1 },
	{ "GETCL",      DVM_GETCL,      DAO_CODE_GETC,    1 },
	{ "GETCK",      DVM_GETCK,      DAO_CODE_GETC,    1 },
	{ "GETCG",      DVM_GETCG,      DAO_CODE_GETC,    1 },
	{ "GETVH",      DVM_GETVH,      DAO_CODE_GETU,    1 },
	{ "GETVS",      DVM_GETVS,      DAO_CODE_GETG,    1 },
	{ "GETVO",      DVM_GETVO,      DAO_CODE_GETG,    1 },
	{ "GETVK",      DVM_GETVK,      DAO_CODE_GETG,    1 },
	{ "GETVG",      DVM_GETVG,      DAO_CODE_GETG,    1 },
	{ "GETI",       DVM_GETI,       DAO_CODE_GETI,    0 },
	{ "GETDI",      DVM_GETDI,      DAO_CODE_GETF,    0 },
	{ "GETMI",      DVM_GETMI,      DAO_CODE_GETM,    0 },
	{ "GETF",       DVM_GETF,       DAO_CODE_GETF,    0 },
	{ "SETVH",      DVM_SETVH,      DAO_CODE_SETU,    1 },
	{ "SETVS",      DVM_SETVS,      DAO_CODE_SETG,    1 },
	{ "SETVO",      DVM_SETVO,      DAO_CODE_SETG,    1 },
	{ "SETVK",      DVM_SETVK,      DAO_CODE_SETG,    1 },
	{ "SETVG",      DVM_SETVG,      DAO_CODE_SETG,    1 },
	{ "SETI",       DVM_SETI,       DAO_CODE_SETI,    0 },
	{ "SETDI",      DVM_SETDI,      DAO_CODE_SETF,    0 },
	{ "SETMI",      DVM_SETMI,      DAO_CODE_SETM,    0 },
	{ "SETF",       DVM_SETF,       DAO_CODE_SETF,    0 },
	{ "LOAD",       DVM_LOAD,       DAO_CODE_MOVE,    1 },
	{ "MOVE",       DVM_MOVE,       DAO_CODE_MOVE,    0 },
	{ "UNTAG",      DVM_UNTAG,      DAO_CODE_MOVE,    0 },
	{ "CAST",       DVM_CAST,       DAO_CODE_MOVE,    0 },
	{ "NOT",        DVM_NOT,        DAO_CODE_UNARY,   0 },
	{ "MINUS",      DVM_MINUS,      DAO_CODE_UNARY,   0 },
	{ "TILDE",      DVM_TILDE,      DAO_CODE_UNARY,   0 },
	{ "SIZE",       DVM_SIZE,       DAO_CODE_UNARY,   0 },
	{ "ADD",        DVM_ADD,        DAO_CODE_BINARY,  0 },
	{ "SUB",        DVM_SUB,        DAO_CODE_BINARY,  0 },
	{ "MUL",        DVM_MUL,        DAO_CODE_BINARY,  0 },
	{ "DIV",        DVM_DIV,        DAO_CODE_BINARY,  0 },
	{ "MOD",        DVM_MOD,        DAO_CODE_BINARY,  0 },
	{ "POW",        DVM_POW,        DAO_CODE_BINARY,  0 },
	{ "AND",        DVM_AND,        DAO_CODE_BINARY,  0 },
	{ "OR",         DVM_OR,         DAO_CODE_BINARY,  0 },
	{ "LT",         DVM_LT,         DAO_CODE_BINARY,  0 },
	{ "LE",         DVM_LE,         DAO_CODE_BINARY,  0 },
	{ "EQ",         DVM_EQ,         DAO_CODE_BINARY,  0 },
	{ "NE",         DVM_NE,         DAO_CODE_BINARY,  0 },
	{ "IN",         DVM_IN,         DAO_CODE_BINARY,  1 },
	{ "BITAND",     DVM_BITAND,     DAO_CODE_BINARY,  0 },
	{ "BITOR",      DVM_BITOR,      DAO_CODE_BINARY,  0 },
	{ "BITXOR",     DVM_BITXOR,     DAO_CODE_BINARY,  0 },
	{ "BITLFT",     DVM_BITLFT,     DAO_CODE_BINARY,  0 },
	{ "BITRIT",     DVM_BITRIT,     DAO_CODE_BINARY,  0 },
	{ "SAME",       DVM_SAME,       DAO_CODE_BINARY,  1 },
	{ "ISA",        DVM_ISA,        DAO_CODE_BINARY,  1 },
	{ "NAMEVA",     DVM_NAMEVA,     DAO_CODE_UNARY2,  1 },
	{ "PAIR",       DVM_PAIR,       DAO_CODE_BINARY,  1 },
	{ "TUPLE",      DVM_TUPLE,      DAO_CODE_ENUM,    1 },
	{ "LIST",       DVM_LIST,       DAO_CODE_ENUM,    1 },
	{ "MAP",        DVM_MAP,        DAO_CODE_ENUM,    1 },
	{ "VECTOR",     DVM_VECTOR,     DAO_CODE_ENUM,    1 },
	{ "MATRIX",     DVM_MATRIX,     DAO_CODE_MATRIX,  1 },
	{ "PACK",       DVM_PACK,       DAO_CODE_ENUM2,   1 },
	{ "MPACK",      DVM_MPACK,      DAO_CODE_ENUM2,   1 },
	{ "ROUTINE",    DVM_ROUTINE,    DAO_CODE_ROUTINE, 1 },
	{ "GOTO",       DVM_GOTO,       DAO_CODE_JUMP,    0 },
	{ "SWITCH",     DVM_SWITCH,     DAO_CODE_BRANCH,  0 },
	{ "CASE",       DVM_CASE,       DAO_CODE_JUMP,    0 },
	{ "ITER",       DVM_ITER,       DAO_CODE_MOVE,    0 },
	{ "TEST",       DVM_TEST,       DAO_CODE_BRANCH,  0 },
	{ "MATH",       DVM_MATH,       DAO_CODE_UNARY2,  1 },
	{ "CALL",       DVM_CALL,       DAO_CODE_CALL,    0 },
	{ "MCALL",      DVM_MCALL,      DAO_CODE_CALL,    0 },
	{ "RETURN",     DVM_RETURN,     DAO_CODE_EXPLIST, 0 },
	{ "YIELD",      DVM_YIELD,      DAO_CODE_YIELD,   0 },
	{ "SECT",       DVM_SECT,       DAO_CODE_EXPLIST, 0 },
	{ "JITC",       DVM_JITC,       DAO_CODE_NOP,     0 },
	{ "DATA_B",     DVM_DATA_B,     DAO_CODE_GETC,    0 },
	{ "DATA_I",     DVM_DATA_I,     DAO_CODE_GETC,    0 },
	{ "DATA_F",     DVM_DATA_F,     DAO_CODE_GETC,    0 },
	{ "DATA_C",     DVM_DATA_C,     DAO_CODE_GETC,    0 },
	{ "GETCL_B",    DVM_GETCL_B,    DAO_CODE_GETC,    0 },
	{ "GETCL_I",    DVM_GETCL_I,    DAO_CODE_GETC,    0 },
	{ "GETCL_F",    DVM_GETCL_F,    DAO_CODE_GETC,    0 },
	{ "GETCL_C",    DVM_GETCL_C,    DAO_CODE_GETC,    0 },
	{ "GETCK_B",    DVM_GETCK_B,    DAO_CODE_GETC,    0 },
	{ "GETCK_I",    DVM_GETCK_I,    DAO_CODE_GETC,    0 },
	{ "GETCK_F",    DVM_GETCK_F,    DAO_CODE_GETC,    0 },
	{ "GETCK_C",    DVM_GETCK_C,    DAO_CODE_GETC,    0 },
	{ "GETCG_B",    DVM_GETCG_B,    DAO_CODE_GETC,    0 },
	{ "GETCG_I",    DVM_GETCG_I,    DAO_CODE_GETC,    0 },
	{ "GETCG_F",    DVM_GETCG_F,    DAO_CODE_GETC,    0 },
	{ "GETCG_C",    DVM_GETCG_C,    DAO_CODE_GETC,    0 },
	{ "GETVH_B",    DVM_GETVH_B,    DAO_CODE_GETU,    0 },
	{ "GETVH_I",    DVM_GETVH_I,    DAO_CODE_GETU,    0 },
	{ "GETVH_F",    DVM_GETVH_F,    DAO_CODE_GETU,    0 },
	{ "GETVH_C",    DVM_GETVH_C,    DAO_CODE_GETU,    0 },
	{ "GETVS_B",    DVM_GETVS_B,    DAO_CODE_GETG,    0 },
	{ "GETVS_I",    DVM_GETVS_I,    DAO_CODE_GETG,    0 },
	{ "GETVS_F",    DVM_GETVS_F,    DAO_CODE_GETG,    0 },
	{ "GETVS_C",    DVM_GETVS_C,    DAO_CODE_GETG,    0 },
	{ "GETVO_B",    DVM_GETVO_B,    DAO_CODE_GETG,    0 },
	{ "GETVO_I",    DVM_GETVO_I,    DAO_CODE_GETG,    0 },
	{ "GETVO_F",    DVM_GETVO_F,    DAO_CODE_GETG,    0 },
	{ "GETVO_C",    DVM_GETVO_C,    DAO_CODE_GETG,    0 },
	{ "GETVK_B",    DVM_GETVK_B,    DAO_CODE_GETG,    0 },
	{ "GETVK_I",    DVM_GETVK_I,    DAO_CODE_GETG,    0 },
	{ "GETVK_F",    DVM_GETVK_F,    DAO_CODE_GETG,    0 },
	{ "GETVK_C",    DVM_GETVK_C,    DAO_CODE_GETG,    0 },
	{ "GETVG_B",    DVM_GETVG_B,    DAO_CODE_GETG,    0 },
	{ "GETVG_I",    DVM_GETVG_I,    DAO_CODE_GETG,    0 },
	{ "GETVG_F",    DVM_GETVG_F,    DAO_CODE_GETG,    0 },
	{ "GETVG_C",    DVM_GETVG_C,    DAO_CODE_GETG,    0 },
	{ "SETVH_BB",   DVM_SETVH_BB,   DAO_CODE_SETU,    0 },
	{ "SETVH_II",   DVM_SETVH_II,   DAO_CODE_SETU,    0 },
	{ "SETVH_FF",   DVM_SETVH_FF,   DAO_CODE_SETU,    0 },
	{ "SETVH_CC",   DVM_SETVH_CC,   DAO_CODE_SETU,    0 },
	{ "SETVS_BB",   DVM_SETVS_BB,   DAO_CODE_SETG,    0 },
	{ "SETVS_II",   DVM_SETVS_II,   DAO_CODE_SETG,    0 },
	{ "SETVS_FF",   DVM_SETVS_FF,   DAO_CODE_SETG,    0 },
	{ "SETVS_CC",   DVM_SETVS_CC,   DAO_CODE_SETG,    0 },
	{ "SETVO_BB",   DVM_SETVO_BB,   DAO_CODE_SETG,    0 },
	{ "SETVO_II",   DVM_SETVO_II,   DAO_CODE_SETG,    0 },
	{ "SETVO_FF",   DVM_SETVO_FF,   DAO_CODE_SETG,    0 },
	{ "SETVO_CC",   DVM_SETVO_CC,   DAO_CODE_SETG,    0 },
	{ "SETVK_BB",   DVM_SETVK_BB,   DAO_CODE_SETG,    0 },
	{ "SETVK_II",   DVM_SETVK_II,   DAO_CODE_SETG,    0 },
	{ "SETVK_FF",   DVM_SETVK_FF,   DAO_CODE_SETG,    0 },
	{ "SETVK_CC",   DVM_SETVK_CC,   DAO_CODE_SETG,    0 },
	{ "SETVG_BB",   DVM_SETVG_BB,   DAO_CODE_SETG,    0 },
	{ "SETVG_II",   DVM_SETVG_II,   DAO_CODE_SETG,    0 },
	{ "SETVG_FF",   DVM_SETVG_FF,   DAO_CODE_SETG,    0 },
	{ "SETVG_CC",   DVM_SETVG_CC,   DAO_CODE_SETG,    0 },
	{ "MOVE_BB",    DVM_MOVE_BB,    DAO_CODE_MOVE,    0 },
	{ "MOVE_BI",    DVM_MOVE_BI,    DAO_CODE_MOVE,    0 },
	{ "MOVE_BF",    DVM_MOVE_BF,    DAO_CODE_MOVE,    0 },
	{ "MOVE_IB",    DVM_MOVE_IB,    DAO_CODE_MOVE,    0 },
	{ "MOVE_II",    DVM_MOVE_II,    DAO_CODE_MOVE,    0 },
	{ "MOVE_IF",    DVM_MOVE_IF,    DAO_CODE_MOVE,    0 },
	{ "MOVE_FB",    DVM_MOVE_FB,    DAO_CODE_MOVE,    0 },
	{ "MOVE_FI",    DVM_MOVE_FI,    DAO_CODE_MOVE,    0 },
	{ "MOVE_FF",    DVM_MOVE_FF,    DAO_CODE_MOVE,    0 },
	{ "MOVE_CF",    DVM_MOVE_CF,    DAO_CODE_MOVE,    0 },
	{ "MOVE_CC",    DVM_MOVE_CC,    DAO_CODE_MOVE,    0 },
	{ "MOVE_SS",    DVM_MOVE_SS,    DAO_CODE_MOVE,    0 },
	{ "MOVE_PP",    DVM_MOVE_PP,    DAO_CODE_MOVE,    0 },
	{ "MOVE_XX",    DVM_MOVE_XX,    DAO_CODE_MOVE,    0 },
	{ "NOT_B",      DVM_NOT_B,      DAO_CODE_UNARY,   0 },
	{ "NOT_I",      DVM_NOT_I,      DAO_CODE_UNARY,   0 },
	{ "NOT_F",      DVM_NOT_F,      DAO_CODE_UNARY,   0 },
	{ "MINUS_I",    DVM_MINUS_I,    DAO_CODE_UNARY,   0 },
	{ "MINUS_F",    DVM_MINUS_F,    DAO_CODE_UNARY,   0 },
	{ "MINUS_C",    DVM_MINUS_C,    DAO_CODE_UNARY,   0 },
	{ "TILDE_I",    DVM_TILDE_I,    DAO_CODE_UNARY,   0 },
	{ "TILDE_C",    DVM_TILDE_C,    DAO_CODE_UNARY,   0 },
	{ "AND_BBB",    DVM_AND_BBB,    DAO_CODE_BINARY,  0 },
	{ "OR_BBB",     DVM_OR_BBB,     DAO_CODE_BINARY,  0 },
	{ "LT_BBB",     DVM_LT_BBB,     DAO_CODE_BINARY,  0 },
	{ "LE_BBB",     DVM_LE_BBB,     DAO_CODE_BINARY,  0 },
	{ "EQ_BBB",     DVM_EQ_BBB,     DAO_CODE_BINARY,  0 },
	{ "NE_BBB",     DVM_NE_BBB,     DAO_CODE_BINARY,  0 },
	{ "ADD_III",    DVM_ADD_III,    DAO_CODE_BINARY,  0 },
	{ "SUB_III",    DVM_SUB_III,    DAO_CODE_BINARY,  0 },
	{ "MUL_III",    DVM_MUL_III,    DAO_CODE_BINARY,  0 },
	{ "DIV_III",    DVM_DIV_III,    DAO_CODE_BINARY,  0 },
	{ "MOD_III",    DVM_MOD_III,    DAO_CODE_BINARY,  0 },
	{ "POW_III",    DVM_POW_III,    DAO_CODE_BINARY,  0 },
	{ "AND_BII",    DVM_AND_BII,    DAO_CODE_BINARY,  0 },
	{ "OR_BII",     DVM_OR_BII,     DAO_CODE_BINARY,  0 },
	{ "LT_BII",     DVM_LT_BII,     DAO_CODE_BINARY,  0 },
	{ "LE_BII",     DVM_LE_BII,     DAO_CODE_BINARY,  0 },
	{ "EQ_BII",     DVM_EQ_BII,     DAO_CODE_BINARY,  0 },
	{ "NE_BII",     DVM_NE_BII,     DAO_CODE_BINARY,  0 },
	{ "BITAND_III", DVM_BITAND_III, DAO_CODE_BINARY,  0 },
	{ "BITOR_III",  DVM_BITOR_III,  DAO_CODE_BINARY,  0 },
	{ "BITXOR_III", DVM_BITXOR_III, DAO_CODE_BINARY,  0 },
	{ "BITLFT_III", DVM_BITLFT_III, DAO_CODE_BINARY,  0 },
	{ "BITRIT_III", DVM_BITRIT_III, DAO_CODE_BINARY,  0 },
	{ "ADD_FFF",    DVM_ADD_FFF,    DAO_CODE_BINARY,  0 },
	{ "SUB_FFF",    DVM_SUB_FFF,    DAO_CODE_BINARY,  0 },
	{ "MUL_FFF",    DVM_MUL_FFF,    DAO_CODE_BINARY,  0 },
	{ "DIV_FFF",    DVM_DIV_FFF,    DAO_CODE_BINARY,  0 },
	{ "MOD_FFF",    DVM_MOD_FFF,    DAO_CODE_BINARY,  0 },
	{ "POW_FFF",    DVM_POW_FFF,    DAO_CODE_BINARY,  0 },
	{ "AND_BFF",    DVM_AND_BFF,    DAO_CODE_BINARY,  0 },
	{ "OR_BFF",     DVM_OR_BFF,     DAO_CODE_BINARY,  0 },
	{ "LT_BFF",     DVM_LT_BFF,     DAO_CODE_BINARY,  0 },
	{ "LE_BFF",     DVM_LE_BFF,     DAO_CODE_BINARY,  0 },
	{ "EQ_BFF",     DVM_EQ_BFF,     DAO_CODE_BINARY,  0 },
	{ "NE_BFF",     DVM_NE_BFF,     DAO_CODE_BINARY,  0 },
	{ "ADD_CCC",    DVM_ADD_CCC,    DAO_CODE_BINARY,  0 },
	{ "SUB_CCC",    DVM_SUB_CCC,    DAO_CODE_BINARY,  0 },
	{ "MUL_CCC",    DVM_MUL_CCC,    DAO_CODE_BINARY,  0 },
	{ "DIV_CCC",    DVM_DIV_CCC,    DAO_CODE_BINARY,  0 },
	{ "EQ_BCC",     DVM_EQ_BCC,     DAO_CODE_BINARY,  0 },
	{ "NE_BCC",     DVM_NE_BCC,     DAO_CODE_BINARY,  0 },
	{ "ADD_SSS",    DVM_ADD_SSS,    DAO_CODE_BINARY,  0 },
	{ "LT_BSS",     DVM_LT_BSS,     DAO_CODE_BINARY,  0 },
	{ "LE_BSS",     DVM_LE_BSS,     DAO_CODE_BINARY,  0 },
	{ "EQ_BSS",     DVM_EQ_BSS,     DAO_CODE_BINARY,  0 },
	{ "NE_BSS",     DVM_NE_BSS,     DAO_CODE_BINARY,  0 },
	{ "GETI_LI",    DVM_GETI_LI,    DAO_CODE_GETI,    0 },
	{ "SETI_LI",    DVM_SETI_LI,    DAO_CODE_SETI,    0 },
	{ "GETI_SI",    DVM_GETI_SI,    DAO_CODE_GETI,    0 },
	{ "SETI_SII",   DVM_SETI_SII,   DAO_CODE_SETI,    0 },
	{ "GETI_LBI",   DVM_GETI_LBI,   DAO_CODE_GETI,    0 },
	{ "GETI_LII",   DVM_GETI_LII,   DAO_CODE_GETI,    0 },
	{ "GETI_LFI",   DVM_GETI_LFI,   DAO_CODE_GETI,    0 },
	{ "GETI_LCI",   DVM_GETI_LCI,   DAO_CODE_GETI,    0 },
	{ "GETI_LSI",   DVM_GETI_LSI,   DAO_CODE_GETI,    0 },
	{ "SETI_LBIB",  DVM_SETI_LBIB,  DAO_CODE_SETI,    0 },
	{ "SETI_LIII",  DVM_SETI_LIII,  DAO_CODE_SETI,    0 },
	{ "SETI_LFIF",  DVM_SETI_LFIF,  DAO_CODE_SETI,    0 },
	{ "SETI_LCIC",  DVM_SETI_LCIC,  DAO_CODE_SETI,    0 },
	{ "SETI_LSIS",  DVM_SETI_LSIS,  DAO_CODE_SETI,    0 },
	{ "GETI_ABI",   DVM_GETI_ABI,   DAO_CODE_GETI,    0 },
	{ "GETI_AII",   DVM_GETI_AII,   DAO_CODE_GETI,    0 },
	{ "GETI_AFI",   DVM_GETI_AFI,   DAO_CODE_GETI,    0 },
	{ "GETI_ACI",   DVM_GETI_ACI,   DAO_CODE_GETI,    0 },
	{ "SETI_ABIB",  DVM_SETI_ABIB,  DAO_CODE_SETI,    0 },
	{ "SETI_AIII",  DVM_SETI_AIII,  DAO_CODE_SETI,    0 },
	{ "SETI_AFIF",  DVM_SETI_AFIF,  DAO_CODE_SETI,    0 },
	{ "SETI_ACIC",  DVM_SETI_ACIC,  DAO_CODE_SETI,    0 },
	{ "GETI_TI",    DVM_GETI_TI,    DAO_CODE_GETI,    0 },
	{ "SETI_TI",    DVM_SETI_TI,    DAO_CODE_SETI,    0 },
	{ "GETF_TB",    DVM_GETF_TB,    DAO_CODE_GETF,    0 },
	{ "GETF_TI",    DVM_GETF_TI,    DAO_CODE_GETF,    0 },
	{ "GETF_TF",    DVM_GETF_TF,    DAO_CODE_GETF,    0 },
	{ "GETF_TC",    DVM_GETF_TC,    DAO_CODE_GETF,    0 },
	{ "GETF_TX",    DVM_GETF_TX,    DAO_CODE_GETF,    0 },
	{ "SETF_TBB",   DVM_SETF_TBB,   DAO_CODE_SETF,    0 },
	{ "SETF_TII",   DVM_SETF_TII,   DAO_CODE_SETF,    0 },
	{ "SETF_TFF",   DVM_SETF_TFF,   DAO_CODE_SETF,    0 },
	{ "SETF_TCC",   DVM_SETF_TCC,   DAO_CODE_SETF,    0 },
	{ "SETF_TSS",   DVM_SETF_TSS,   DAO_CODE_SETF,    0 },
	{ "SETF_TPP",   DVM_SETF_TPP,   DAO_CODE_SETF,    0 },
	{ "SETF_TXX",   DVM_SETF_TXX,   DAO_CODE_SETF,    0 },
	{ "GETMI_ABI",  DVM_GETMI_ABI,  DAO_CODE_GETM,    0 },
	{ "GETMI_AII",  DVM_GETMI_AII,  DAO_CODE_GETM,    0 },
	{ "GETMI_AFI",  DVM_GETMI_AFI,  DAO_CODE_GETM,    0 },
	{ "GETMI_ACI",  DVM_GETMI_ACI,  DAO_CODE_GETM,    0 },
	{ "SETMI_ABIB", DVM_SETMI_ABIB, DAO_CODE_SETM,    0 },
	{ "SETMI_AIII", DVM_SETMI_AIII, DAO_CODE_SETM,    0 },
	{ "SETMI_AFIF", DVM_SETMI_AFIF, DAO_CODE_SETM,    0 },
	{ "SETMI_ACIC", DVM_SETMI_ACIC, DAO_CODE_SETM,    0 },
	{ "GETF_CX",    DVM_GETF_CX,    DAO_CODE_GETF,    0 },
	{ "SETF_CX",    DVM_SETF_CX,    DAO_CODE_SETF,    0 },
	{ "GETF_KC",    DVM_GETF_KC,    DAO_CODE_GETF,    0 },
	{ "GETF_KG",    DVM_GETF_KG,    DAO_CODE_GETF,    0 },
	{ "GETF_OC",    DVM_GETF_OC,    DAO_CODE_GETF,    0 },
	{ "GETF_OG",    DVM_GETF_OG,    DAO_CODE_GETF,    0 },
	{ "GETF_OV",    DVM_GETF_OV,    DAO_CODE_GETF,    0 },
	{ "SETF_KG",    DVM_SETF_KG,    DAO_CODE_SETF,    0 },
	{ "SETF_OG",    DVM_SETF_OG,    DAO_CODE_SETF,    0 },
	{ "SETF_OV",    DVM_SETF_OV,    DAO_CODE_SETF,    0 },
	{ "GETF_KCB",   DVM_GETF_KCB,   DAO_CODE_GETF,    0 },
	{ "GETF_KCI",   DVM_GETF_KCI,   DAO_CODE_GETF,    0 },
	{ "GETF_KCF",   DVM_GETF_KCF,   DAO_CODE_GETF,    0 },
	{ "GETF_KCC",   DVM_GETF_KCC,   DAO_CODE_GETF,    0 },
	{ "GETF_KGB",   DVM_GETF_KGB,   DAO_CODE_GETF,    0 },
	{ "GETF_KGI",   DVM_GETF_KGI,   DAO_CODE_GETF,    0 },
	{ "GETF_KGF",   DVM_GETF_KGF,   DAO_CODE_GETF,    0 },
	{ "GETF_KGC",   DVM_GETF_KGC,   DAO_CODE_GETF,    0 },
	{ "GETF_OCB",   DVM_GETF_OCB,   DAO_CODE_GETF,    0 },
	{ "GETF_OCI",   DVM_GETF_OCI,   DAO_CODE_GETF,    0 },
	{ "GETF_OCF",   DVM_GETF_OCF,   DAO_CODE_GETF,    0 },
	{ "GETF_OCC",   DVM_GETF_OCC,   DAO_CODE_GETF,    0 },
	{ "GETF_OGB",   DVM_GETF_OGB,   DAO_CODE_GETF,    0 },
	{ "GETF_OGI",   DVM_GETF_OGI,   DAO_CODE_GETF,    0 },
	{ "GETF_OGF",   DVM_GETF_OGF,   DAO_CODE_GETF,    0 },
	{ "GETF_OGC",   DVM_GETF_OGC,   DAO_CODE_GETF,    0 },
	{ "GETF_OVB",   DVM_GETF_OVB,   DAO_CODE_GETF,    0 },
	{ "GETF_OVI",   DVM_GETF_OVI,   DAO_CODE_GETF,    0 },
	{ "GETF_OVF",   DVM_GETF_OVF,   DAO_CODE_GETF,    0 },
	{ "GETF_OVC",   DVM_GETF_OVC,   DAO_CODE_GETF,    0 },
	{ "SETF_KGBB",  DVM_SETF_KGBB,  DAO_CODE_SETF,    0 },
	{ "SETF_KGII",  DVM_SETF_KGII,  DAO_CODE_SETF,    0 },
	{ "SETF_KGFF",  DVM_SETF_KGFF,  DAO_CODE_SETF,    0 },
	{ "SETF_KGCC",  DVM_SETF_KGCC,  DAO_CODE_SETF,    0 },
	{ "SETF_OGBB",  DVM_SETF_OGBB,  DAO_CODE_SETF,    0 },
	{ "SETF_OGII",  DVM_SETF_OGII,  DAO_CODE_SETF,    0 },
	{ "SETF_OGFF",  DVM_SETF_OGFF,  DAO_CODE_SETF,    0 },
	{ "SETF_OGCC",  DVM_SETF_OGCC,  DAO_CODE_SETF,    0 },
	{ "SETF_OVBB",  DVM_SETF_OVBB,  DAO_CODE_SETF,    0 },
	{ "SETF_OVII",  DVM_SETF_OVII,  DAO_CODE_SETF,    0 },
	{ "SETF_OVFF",  DVM_SETF_OVFF,  DAO_CODE_SETF,    0 },
	{ "SETF_OVCC",  DVM_SETF_OVCC,  DAO_CODE_SETF,    0 },
	{ "TEST_B",     DVM_TEST_B,     DAO_CODE_BRANCH,  0 },
	{ "TEST_I",     DVM_TEST_I,     DAO_CODE_BRANCH,  0 },
	{ "TEST_F",     DVM_TEST_F,     DAO_CODE_BRANCH,  0 },
	{ "MATH_B",     DVM_MATH_B,     DAO_CODE_UNARY2,  1 },
	{ "MATH_I",     DVM_MATH_I,     DAO_CODE_UNARY2,  1 },
	{ "MATH_F",     DVM_MATH_F,     DAO_CODE_UNARY2,  1 },
	{ "CAST_B",     DVM_CAST_B,     DAO_CODE_MOVE,    0 },
	{ "CAST_I",     DVM_CAST_I,     DAO_CODE_MOVE,    0 },
	{ "CAST_F",     DVM_CAST_F,     DAO_CODE_MOVE,    0 },
	{ "CAST_C",     DVM_CAST_C,     DAO_CODE_MOVE,    0 },
	{ "CAST_S",     DVM_CAST_S,     DAO_CODE_MOVE,    0 },
	{ "CAST_VE",    DVM_CAST_VE,    DAO_CODE_MOVE,    0 },
	{ "CAST_VX",    DVM_CAST_VX,    DAO_CODE_MOVE,    0 },
	{ "ISA_ST",     DVM_ISA_ST,     DAO_CODE_BINARY,  0 },
	{ "TUPLE_SIM",  DVM_TUPLE_SIM,  DAO_CODE_ENUM,    1 },
	{ "???",        DVM_UNUSED,     DAO_CODE_NOP,     0 },

	/* for compiling only */
	{ "nop",        DVM_UNUSED,     DAO_CODE_NOP, 0 },
	{ "label",      DVM_UNUSED,     DAO_CODE_NOP, 0 },
	{ "load2",      DVM_UNUSED,     DAO_CODE_NOP, 0 },
	{ "loop",       DVM_UNUSED,     DAO_CODE_NOP, 0 },
	{ "branch",     DVM_UNUSED,     DAO_CODE_NOP, 0 },
	{ "do",         DVM_UNUSED,     DAO_CODE_NOP, 0 },
	{ "lbra",       DVM_UNUSED,     DAO_CODE_NOP, 0 },
	{ "rbra",       DVM_UNUSED,     DAO_CODE_NOP, 0 },
	{ "default",    DVM_UNUSED,     DAO_CODE_NOP, 0 },
	{ "unused",     DVM_UNUSED,     DAO_CODE_NOP, 0 }
};


const char* DaoVmCode_GetOpcodeName( int code )
{
	if( code >= 0 && code <= DVM_UNUSED ) return dao_code_infolist[ code ].name;
	return "???";
}
ushort_t DaoVmCode_GetOpcodeBase( int code )
{
	if( code >= 0 && code <= DVM_UNUSED ) return dao_code_infolist[ code ].base;
	return DVM_DATA;
}
uchar_t DaoVmCode_CheckPermutable( int code )
{
	if( code >= 0 && code <= DVM_UNUSED ) return dao_code_infolist[ code ].perm;
	return 0;
}
uchar_t DaoVmCode_GetOpcodeType( DaoVmCode *self )
{
	int code = self->code;
	if( code >= 0 && code <= DVM_UNUSED ) return dao_code_infolist[ code ].type;
	return DAO_CODE_NOP;
}
int DaoVmCode_MayCreateReference( int code )
{
	switch( code ){
	case DVM_GETI_LI : case DVM_GETI_LSI :
	case DVM_GETI_TI : case DVM_GETF_TX :
	case DVM_GETF_KC : case DVM_GETF_KG :
	case DVM_GETF_OC : case DVM_GETF_OG : case DVM_GETF_OV :
		return 1;
	default : return code < DVM_JITC;
	}
	return 0;
}
DaoVmCode DaoVmCode_CheckOperands( DaoVmCode *self )
{
	DaoVmCode vmc = { 0, 0, 0, 0 };
	switch( DaoVmCode_GetOpcodeType( self ) ){
	case DAO_CODE_NOP :
		break;
	case DAO_CODE_GETC :
	case DAO_CODE_GETG :
		vmc.c = 1;
		break;
	case DAO_CODE_SETU :
		vmc.a = 1;
		if( self->c != 0 ) vmc.b = 1;
		break;
	case DAO_CODE_SETG :
	case DAO_CODE_BRANCH :
		vmc.a = 1;
		break;
	case DAO_CODE_EXPLIST :
		if( self->b ) vmc.a = 1;
		break;
	case DAO_CODE_GETF : case DAO_CODE_SETF :
	case DAO_CODE_MOVE : case DAO_CODE_UNARY :
		vmc.a = 1;
		vmc.c = 1;
		break;
	case DAO_CODE_GETM :
	case DAO_CODE_ENUM2 : case DAO_CODE_MATRIX :
	case DAO_CODE_ROUTINE : case DAO_CODE_CALL :
		vmc.a = 1;
		vmc.c = 1;
		break;
	case DAO_CODE_SETM:
		vmc.a = 1;
		vmc.c = 1;
		break;
	case DAO_CODE_ENUM :
	case DAO_CODE_YIELD :
		if( self->b ) vmc.a = 1;
		vmc.c = 1;
		break;
	case DAO_CODE_SETI :
	case DAO_CODE_GETI :
	case DAO_CODE_BINARY :
		vmc.a = 1;
		vmc.b = 1;
		vmc.c = 1;
		break;
	case DAO_CODE_GETU :
		vmc.c = 1;
		if( self->a ) vmc.b = 1;
		break;
	case DAO_CODE_UNARY2 :
		vmc.b = 1;
		vmc.c = 1;
		break;
	default: break;
	}
	return vmc;
}

void DaoVmCode_Print( DaoVmCode self, char *buffer )
{
	const char *name = DaoVmCode_GetOpcodeName( self.code );
	static const char *fmt = "%-11s : %6i , %6i , %6i ;\n";
	if( buffer == NULL )
		printf( fmt, name, self.a, self.b, self.c );
	else
		sprintf( buffer, fmt, name, self.a, self.b, self.c );
}
void DaoVmCodeX_Print( DaoVmCodeX self, char *annot, char *buffer )
{
	const char *name = DaoVmCode_GetOpcodeName( self.code );
	static const char *fmt = "%-11s : %6i , %6i , %6i ;  %4i,  %s\n";
	if( buffer == NULL )
		printf( fmt, name, self.a, self.b, self.c, self.line, annot ? annot : "" );
	else
		sprintf( buffer, fmt, name, self.a, self.b, self.c, self.line, annot ? annot : "" );
}
