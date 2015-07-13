/*
AUTO-GENERATED FILE. DO NOT MODIFY.

Note: this header is a header template
and must NOT have multiple-inclusion protection.
*/

/******************************************************************************
* Copyright (C) 2012-2014 Evgeniy Golovin (evgeniy.golovin@unitpoint.ru)
*
* Please feel free to contact me at anytime, 
* my email is evgeniy.golovin@unitpoint.ru, skype: egolovin
*
* Latest source code: https://github.com/unitpoint/objectscript
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************/

#if OS_BIND_FUNC_NUM_ARGS == 0

#define OS_BIND_FUNC_PARMS_COMMA
#define OS_BIND_FUNC_TEMPLATE_PARMS
#define OS_BIND_FUNC_PARMS
#define OS_BIND_FUNC_ARGS
#define OS_BIND_FUNC_GET_ARGS

#elif OS_BIND_FUNC_NUM_ARGS == 1

#define OS_BIND_FUNC_PARMS_COMMA ,
#define OS_BIND_FUNC_TEMPLATE_PARMS class ARG_TYPE_1
#define OS_BIND_FUNC_PARMS ARG_TYPE_1
#define OS_BIND_FUNC_ARGS arg1
#define OS_BIND_FUNC_GET_ARGS  \
	int cur_param_offs = -params; \
	OS_GET_TEMPLATE_ARG(1, ARG_TYPE_1)

#elif OS_BIND_FUNC_NUM_ARGS == 2

#define OS_BIND_FUNC_PARMS_COMMA ,
#define OS_BIND_FUNC_TEMPLATE_PARMS class ARG_TYPE_1, class ARG_TYPE_2
#define OS_BIND_FUNC_PARMS ARG_TYPE_1, ARG_TYPE_2
#define OS_BIND_FUNC_ARGS arg1, arg2
#define OS_BIND_FUNC_GET_ARGS  \
	int cur_param_offs = -params; \
	OS_GET_TEMPLATE_ARG(1, ARG_TYPE_1); \
	OS_GET_TEMPLATE_ARG(2, ARG_TYPE_2)

#elif OS_BIND_FUNC_NUM_ARGS == 3

#define OS_BIND_FUNC_PARMS_COMMA ,
#define OS_BIND_FUNC_TEMPLATE_PARMS class ARG_TYPE_1, class ARG_TYPE_2, class ARG_TYPE_3
#define OS_BIND_FUNC_PARMS ARG_TYPE_1, ARG_TYPE_2, ARG_TYPE_3
#define OS_BIND_FUNC_ARGS arg1, arg2, arg3
#define OS_BIND_FUNC_GET_ARGS  \
	int cur_param_offs = -params; \
	OS_GET_TEMPLATE_ARG(1, ARG_TYPE_1); \
	OS_GET_TEMPLATE_ARG(2, ARG_TYPE_2); \
	OS_GET_TEMPLATE_ARG(3, ARG_TYPE_3)

#elif OS_BIND_FUNC_NUM_ARGS == 4

#define OS_BIND_FUNC_PARMS_COMMA ,
#define OS_BIND_FUNC_TEMPLATE_PARMS class ARG_TYPE_1, class ARG_TYPE_2, class ARG_TYPE_3, class ARG_TYPE_4
#define OS_BIND_FUNC_PARMS ARG_TYPE_1, ARG_TYPE_2, ARG_TYPE_3, ARG_TYPE_4
#define OS_BIND_FUNC_ARGS arg1, arg2, arg3, arg4
#define OS_BIND_FUNC_GET_ARGS  \
	int cur_param_offs = -params; \
	OS_GET_TEMPLATE_ARG(1, ARG_TYPE_1); \
	OS_GET_TEMPLATE_ARG(2, ARG_TYPE_2); \
	OS_GET_TEMPLATE_ARG(3, ARG_TYPE_3); \
	OS_GET_TEMPLATE_ARG(4, ARG_TYPE_4)

#elif OS_BIND_FUNC_NUM_ARGS == 5

#define OS_BIND_FUNC_PARMS_COMMA ,
#define OS_BIND_FUNC_TEMPLATE_PARMS class ARG_TYPE_1, class ARG_TYPE_2, class ARG_TYPE_3, class ARG_TYPE_4, class ARG_TYPE_5
#define OS_BIND_FUNC_PARMS ARG_TYPE_1, ARG_TYPE_2, ARG_TYPE_3, ARG_TYPE_4, ARG_TYPE_5
#define OS_BIND_FUNC_ARGS arg1, arg2, arg3, arg4, arg5
#define OS_BIND_FUNC_GET_ARGS  \
	int cur_param_offs = -params; \
	OS_GET_TEMPLATE_ARG(1, ARG_TYPE_1); \
	OS_GET_TEMPLATE_ARG(2, ARG_TYPE_2); \
	OS_GET_TEMPLATE_ARG(3, ARG_TYPE_3); \
	OS_GET_TEMPLATE_ARG(4, ARG_TYPE_4); \
	OS_GET_TEMPLATE_ARG(5, ARG_TYPE_5)

#elif OS_BIND_FUNC_NUM_ARGS == 6

#define OS_BIND_FUNC_PARMS_COMMA ,
#define OS_BIND_FUNC_TEMPLATE_PARMS class ARG_TYPE_1, class ARG_TYPE_2, class ARG_TYPE_3, class ARG_TYPE_4, class ARG_TYPE_5, class ARG_TYPE_6
#define OS_BIND_FUNC_PARMS ARG_TYPE_1, ARG_TYPE_2, ARG_TYPE_3, ARG_TYPE_4, ARG_TYPE_5, ARG_TYPE_6
#define OS_BIND_FUNC_ARGS arg1, arg2, arg3, arg4, arg5, arg6
#define OS_BIND_FUNC_GET_ARGS  \
	int cur_param_offs = -params; \
	OS_GET_TEMPLATE_ARG(1, ARG_TYPE_1); \
	OS_GET_TEMPLATE_ARG(2, ARG_TYPE_2); \
	OS_GET_TEMPLATE_ARG(3, ARG_TYPE_3); \
	OS_GET_TEMPLATE_ARG(4, ARG_TYPE_4); \
	OS_GET_TEMPLATE_ARG(5, ARG_TYPE_5); \
	OS_GET_TEMPLATE_ARG(6, ARG_TYPE_6)

#elif OS_BIND_FUNC_NUM_ARGS == 7

#define OS_BIND_FUNC_PARMS_COMMA ,
#define OS_BIND_FUNC_TEMPLATE_PARMS class ARG_TYPE_1, class ARG_TYPE_2, class ARG_TYPE_3, class ARG_TYPE_4, class ARG_TYPE_5, class ARG_TYPE_6, class ARG_TYPE_7
#define OS_BIND_FUNC_PARMS ARG_TYPE_1, ARG_TYPE_2, ARG_TYPE_3, ARG_TYPE_4, ARG_TYPE_5, ARG_TYPE_6, ARG_TYPE_7
#define OS_BIND_FUNC_ARGS arg1, arg2, arg3, arg4, arg5, arg6, arg7
#define OS_BIND_FUNC_GET_ARGS  \
	int cur_param_offs = -params; \
	OS_GET_TEMPLATE_ARG(1, ARG_TYPE_1); \
	OS_GET_TEMPLATE_ARG(2, ARG_TYPE_2); \
	OS_GET_TEMPLATE_ARG(3, ARG_TYPE_3); \
	OS_GET_TEMPLATE_ARG(4, ARG_TYPE_4); \
	OS_GET_TEMPLATE_ARG(5, ARG_TYPE_5); \
	OS_GET_TEMPLATE_ARG(6, ARG_TYPE_6); \
	OS_GET_TEMPLATE_ARG(7, ARG_TYPE_7)

#elif OS_BIND_FUNC_NUM_ARGS == 8

#define OS_BIND_FUNC_PARMS_COMMA ,
#define OS_BIND_FUNC_TEMPLATE_PARMS class ARG_TYPE_1, class ARG_TYPE_2, class ARG_TYPE_3, class ARG_TYPE_4, class ARG_TYPE_5, class ARG_TYPE_6, class ARG_TYPE_7, class ARG_TYPE_8
#define OS_BIND_FUNC_PARMS ARG_TYPE_1, ARG_TYPE_2, ARG_TYPE_3, ARG_TYPE_4, ARG_TYPE_5, ARG_TYPE_6, ARG_TYPE_7, ARG_TYPE_8
#define OS_BIND_FUNC_ARGS arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8
#define OS_BIND_FUNC_GET_ARGS  \
	int cur_param_offs = -params; \
	OS_GET_TEMPLATE_ARG(1, ARG_TYPE_1); \
	OS_GET_TEMPLATE_ARG(2, ARG_TYPE_2); \
	OS_GET_TEMPLATE_ARG(3, ARG_TYPE_3); \
	OS_GET_TEMPLATE_ARG(4, ARG_TYPE_4); \
	OS_GET_TEMPLATE_ARG(5, ARG_TYPE_5); \
	OS_GET_TEMPLATE_ARG(6, ARG_TYPE_6); \
	OS_GET_TEMPLATE_ARG(7, ARG_TYPE_7); \
	OS_GET_TEMPLATE_ARG(8, ARG_TYPE_8)

#elif OS_BIND_FUNC_NUM_ARGS == 9

#define OS_BIND_FUNC_PARMS_COMMA ,
#define OS_BIND_FUNC_TEMPLATE_PARMS class ARG_TYPE_1, class ARG_TYPE_2, class ARG_TYPE_3, class ARG_TYPE_4, class ARG_TYPE_5, class ARG_TYPE_6, class ARG_TYPE_7, class ARG_TYPE_8, class ARG_TYPE_9
#define OS_BIND_FUNC_PARMS ARG_TYPE_1, ARG_TYPE_2, ARG_TYPE_3, ARG_TYPE_4, ARG_TYPE_5, ARG_TYPE_6, ARG_TYPE_7, ARG_TYPE_8, ARG_TYPE_9
#define OS_BIND_FUNC_ARGS arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9
#define OS_BIND_FUNC_GET_ARGS  \
	int cur_param_offs = -params; \
	OS_GET_TEMPLATE_ARG(1, ARG_TYPE_1); \
	OS_GET_TEMPLATE_ARG(2, ARG_TYPE_2); \
	OS_GET_TEMPLATE_ARG(3, ARG_TYPE_3); \
	OS_GET_TEMPLATE_ARG(4, ARG_TYPE_4); \
	OS_GET_TEMPLATE_ARG(5, ARG_TYPE_5); \
	OS_GET_TEMPLATE_ARG(6, ARG_TYPE_6); \
	OS_GET_TEMPLATE_ARG(7, ARG_TYPE_7); \
	OS_GET_TEMPLATE_ARG(8, ARG_TYPE_8); \
	OS_GET_TEMPLATE_ARG(9, ARG_TYPE_9)

#elif OS_BIND_FUNC_NUM_ARGS == 10

#define OS_BIND_FUNC_PARMS_COMMA ,
#define OS_BIND_FUNC_TEMPLATE_PARMS class ARG_TYPE_1, class ARG_TYPE_2, class ARG_TYPE_3, class ARG_TYPE_4, class ARG_TYPE_5, class ARG_TYPE_6, class ARG_TYPE_7, class ARG_TYPE_8, class ARG_TYPE_9, class ARG_TYPE_10
#define OS_BIND_FUNC_PARMS ARG_TYPE_1, ARG_TYPE_2, ARG_TYPE_3, ARG_TYPE_4, ARG_TYPE_5, ARG_TYPE_6, ARG_TYPE_7, ARG_TYPE_8, ARG_TYPE_9, ARG_TYPE_10
#define OS_BIND_FUNC_ARGS arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10
#define OS_BIND_FUNC_GET_ARGS  \
	int cur_param_offs = -params; \
	OS_GET_TEMPLATE_ARG(1, ARG_TYPE_1); \
	OS_GET_TEMPLATE_ARG(2, ARG_TYPE_2); \
	OS_GET_TEMPLATE_ARG(3, ARG_TYPE_3); \
	OS_GET_TEMPLATE_ARG(4, ARG_TYPE_4); \
	OS_GET_TEMPLATE_ARG(5, ARG_TYPE_5); \
	OS_GET_TEMPLATE_ARG(6, ARG_TYPE_6); \
	OS_GET_TEMPLATE_ARG(7, ARG_TYPE_7); \
	OS_GET_TEMPLATE_ARG(8, ARG_TYPE_8); \
	OS_GET_TEMPLATE_ARG(9, ARG_TYPE_9); \
	OS_GET_TEMPLATE_ARG(10, ARG_TYPE_10)

#elif OS_BIND_FUNC_NUM_ARGS == 11

#define OS_BIND_FUNC_PARMS_COMMA ,
#define OS_BIND_FUNC_TEMPLATE_PARMS class ARG_TYPE_1, class ARG_TYPE_2, class ARG_TYPE_3, class ARG_TYPE_4, class ARG_TYPE_5, class ARG_TYPE_6, class ARG_TYPE_7, class ARG_TYPE_8, class ARG_TYPE_9, class ARG_TYPE_10, class ARG_TYPE_11
#define OS_BIND_FUNC_PARMS ARG_TYPE_1, ARG_TYPE_2, ARG_TYPE_3, ARG_TYPE_4, ARG_TYPE_5, ARG_TYPE_6, ARG_TYPE_7, ARG_TYPE_8, ARG_TYPE_9, ARG_TYPE_10, ARG_TYPE_11
#define OS_BIND_FUNC_ARGS arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11
#define OS_BIND_FUNC_GET_ARGS  \
	int cur_param_offs = -params; \
	OS_GET_TEMPLATE_ARG(1, ARG_TYPE_1); \
	OS_GET_TEMPLATE_ARG(2, ARG_TYPE_2); \
	OS_GET_TEMPLATE_ARG(3, ARG_TYPE_3); \
	OS_GET_TEMPLATE_ARG(4, ARG_TYPE_4); \
	OS_GET_TEMPLATE_ARG(5, ARG_TYPE_5); \
	OS_GET_TEMPLATE_ARG(6, ARG_TYPE_6); \
	OS_GET_TEMPLATE_ARG(7, ARG_TYPE_7); \
	OS_GET_TEMPLATE_ARG(8, ARG_TYPE_8); \
	OS_GET_TEMPLATE_ARG(9, ARG_TYPE_9); \
	OS_GET_TEMPLATE_ARG(10, ARG_TYPE_10); \
	OS_GET_TEMPLATE_ARG(11, ARG_TYPE_11)

#endif

#ifdef OS_BIND_FUNC_CDECL

#ifdef __GNUC__
#define OS_BIND_FUNC_CC
#define OS_BIND_FUNC_CC_GNUC __attribute__((cdecl))
#else
#define OS_BIND_FUNC_CC __cdecl
#define OS_BIND_FUNC_CC_GNUC
#endif

#if OS_BIND_FUNC_NUM_ARGS == 0
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst0 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst0_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 1
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst1 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst1_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 2
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst2 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst2_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 3
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst3 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst3_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 4
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst4 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst4_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 5
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst5 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst5_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 6
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst6 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst6_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 7
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst7 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst7_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 8
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst8 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst8_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 9
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst9 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst9_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 10
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst10 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst10_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 11
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst11 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst11_run ## _cdecl
#endif

#elif defined OS_BIND_FUNC_STDCALL

#ifdef __GNUC__
#define OS_BIND_FUNC_CC
#define OS_BIND_FUNC_CC_GNUC __attribute__((stdcall))
#else
#define OS_BIND_FUNC_CC __stdcall
#define OS_BIND_FUNC_CC_GNUC
#endif

#if OS_BIND_FUNC_NUM_ARGS == 0
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst0 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst0_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 1
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst1 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst1_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 2
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst2 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst2_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 3
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst3 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst3_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 4
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst4 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst4_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 5
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst5 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst5_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 6
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst6 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst6_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 7
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst7 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst7_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 8
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst8 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst8_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 9
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst9 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst9_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 10
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst10 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst10_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 11
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst11 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst11_run ## _stdcall
#endif

#elif defined OS_BIND_FUNC_FASTCALL

#ifdef __GNUC__
#define OS_BIND_FUNC_CC
#define OS_BIND_FUNC_CC_GNUC __attribute__((fastcall))
#else
#define OS_BIND_FUNC_CC __fastcall
#define OS_BIND_FUNC_CC_GNUC
#endif

#if OS_BIND_FUNC_NUM_ARGS == 0
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst0 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst0_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 1
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst1 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst1_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 2
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst2 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst2_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 3
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst3 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst3_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 4
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst4 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst4_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 5
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst5 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst5_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 6
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst6 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst6_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 7
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst7 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst7_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 8
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst8 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst8_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 9
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst9 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst9_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 10
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst10 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst10_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 11
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst11 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst11_run ## _fastcall
#endif

#elif defined OS_BIND_FUNC_THISCALL

#ifdef __GNUC__
#define OS_BIND_FUNC_CC
#define OS_BIND_FUNC_CC_GNUC __attribute__((thiscall))
#else
#define OS_BIND_FUNC_CC __thiscall
#define OS_BIND_FUNC_CC_GNUC
#endif

#if OS_BIND_FUNC_NUM_ARGS == 0
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst0 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst0_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 1
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst1 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst1_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 2
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst2 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst2_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 3
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst3 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst3_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 4
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst4 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst4_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 5
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst5 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst5_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 6
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst6 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst6_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 7
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst7 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst7_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 8
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst8 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst8_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 9
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst9 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst9_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 10
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst10 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst10_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 11
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst11 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst11_run ## _thiscall
#endif

#else

#define OS_BIND_FUNC_CC
#define OS_BIND_FUNC_CC_GNUC

#if OS_BIND_FUNC_NUM_ARGS == 0
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst0
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst0_run
#elif OS_BIND_FUNC_NUM_ARGS == 1
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst1
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst1_run
#elif OS_BIND_FUNC_NUM_ARGS == 2
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst2
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst2_run
#elif OS_BIND_FUNC_NUM_ARGS == 3
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst3
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst3_run
#elif OS_BIND_FUNC_NUM_ARGS == 4
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst4
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst4_run
#elif OS_BIND_FUNC_NUM_ARGS == 5
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst5
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst5_run
#elif OS_BIND_FUNC_NUM_ARGS == 6
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst6
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst6_run
#elif OS_BIND_FUNC_NUM_ARGS == 7
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst7
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst7_run
#elif OS_BIND_FUNC_NUM_ARGS == 8
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst8
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst8_run
#elif OS_BIND_FUNC_NUM_ARGS == 9
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst9
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst9_run
#elif OS_BIND_FUNC_NUM_ARGS == 10
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst10
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst10_run
#elif OS_BIND_FUNC_NUM_ARGS == 11
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImpConst11
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImpConst11_run
#endif

#endif

template <class R, class T OS_BIND_FUNC_PARMS_COMMA OS_BIND_FUNC_TEMPLATE_PARMS> 
struct OS_BIND_FUNC_RUN_CLASS_NAME
{
	typedef R(OS_BIND_FUNC_CC T::*F)(OS_BIND_FUNC_PARMS)const OS_BIND_FUNC_CC_GNUC;

	static int run(OS * os, int params, int, int, void * user_param)
	{
		OS_GET_TEMPLATE_SELF(T*);
		OS_BIND_FUNC_GET_ARGS;
		typedef typename RemoveConst<R>::type type;
		F& f = *(F*)user_param;
		// CtypeValue<type>::push(os, CtypeValue<type>::to((self->*f)(OS_BIND_FUNC_ARGS)));
		CtypeValue<type>::push(os, (self->*f)(OS_BIND_FUNC_ARGS));
		return 1;
	}
};

template <class T OS_BIND_FUNC_PARMS_COMMA OS_BIND_FUNC_TEMPLATE_PARMS> 
struct OS_BIND_FUNC_RUN_CLASS_NAME<void, T OS_BIND_FUNC_PARMS_COMMA OS_BIND_FUNC_PARMS>
{
	typedef void(OS_BIND_FUNC_CC T::*F)(OS_BIND_FUNC_PARMS)const OS_BIND_FUNC_CC_GNUC;

	static int run(OS * os, int params, int, int, void * user_param)
	{
		OS_GET_TEMPLATE_SELF(T*);
		OS_BIND_FUNC_GET_ARGS;
		F& f = *(F*)user_param;
		(self->*f)(OS_BIND_FUNC_ARGS);
		return 0;
	}
};

template <class R, class T OS_BIND_FUNC_PARMS_COMMA OS_BIND_FUNC_TEMPLATE_PARMS>
struct OS_BIND_FUNC_CLASS_NAME
{
	typedef R(OS_BIND_FUNC_CC T::*F)(OS_BIND_FUNC_PARMS)const OS_BIND_FUNC_CC_GNUC;

	const char * name;
	F f;

	OS_BIND_FUNC_CLASS_NAME(const char * _name, F _f): name(_name), f(_f){}
	
	operator OS::FuncDef() const 
	{ 
		OS::FuncDef def = {name, 
			OS_BIND_FUNC_RUN_CLASS_NAME<R, T OS_BIND_FUNC_PARMS_COMMA OS_BIND_FUNC_PARMS>::run, 
			&(FunctionData<F>::create(f))->f}; 
		return def; 
	}
};

// namespace ObjectScript {

template <class R, class T OS_BIND_FUNC_PARMS_COMMA OS_BIND_FUNC_TEMPLATE_PARMS> 
OS::FuncDef def(const char * name, R(OS_BIND_FUNC_CC T::*f)(OS_BIND_FUNC_PARMS)const OS_BIND_FUNC_CC_GNUC)
{
	typedef OS_BIND_FUNC_CLASS_NAME<R, T OS_BIND_FUNC_PARMS_COMMA OS_BIND_FUNC_PARMS> Func; 
	return Func(name, f);
}

// } // namespace ObjectScript

#undef OS_BIND_FUNC_CLASS_NAME
#undef OS_BIND_FUNC_RUN_CLASS_NAME
#undef OS_BIND_FUNC_CC

#undef OS_BIND_FUNC_CC_GNUC

#ifdef OS_BIND_FUNC_CDECL

#ifdef __GNUC__
#define OS_BIND_FUNC_CC
#define OS_BIND_FUNC_CC_GNUC __attribute__((cdecl))
#else
#define OS_BIND_FUNC_CC __cdecl
#define OS_BIND_FUNC_CC_GNUC
#endif

#if OS_BIND_FUNC_NUM_ARGS == 0
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp0 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp0_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 1
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp1 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp1_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 2
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp2 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp2_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 3
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp3 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp3_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 4
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp4 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp4_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 5
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp5 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp5_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 6
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp6 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp6_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 7
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp7 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp7_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 8
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp8 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp8_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 9
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp9 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp9_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 10
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp10 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp10_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 11
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp11 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp11_run ## _cdecl
#endif

#elif defined OS_BIND_FUNC_STDCALL

#ifdef __GNUC__
#define OS_BIND_FUNC_CC
#define OS_BIND_FUNC_CC_GNUC __attribute__((stdcall))
#else
#define OS_BIND_FUNC_CC __stdcall
#define OS_BIND_FUNC_CC_GNUC
#endif

#if OS_BIND_FUNC_NUM_ARGS == 0
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp0 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp0_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 1
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp1 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp1_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 2
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp2 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp2_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 3
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp3 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp3_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 4
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp4 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp4_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 5
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp5 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp5_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 6
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp6 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp6_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 7
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp7 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp7_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 8
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp8 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp8_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 9
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp9 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp9_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 10
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp10 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp10_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 11
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp11 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp11_run ## _stdcall
#endif

#elif defined OS_BIND_FUNC_FASTCALL

#ifdef __GNUC__
#define OS_BIND_FUNC_CC
#define OS_BIND_FUNC_CC_GNUC __attribute__((fastcall))
#else
#define OS_BIND_FUNC_CC __fastcall
#define OS_BIND_FUNC_CC_GNUC
#endif

#if OS_BIND_FUNC_NUM_ARGS == 0
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp0 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp0_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 1
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp1 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp1_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 2
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp2 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp2_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 3
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp3 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp3_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 4
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp4 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp4_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 5
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp5 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp5_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 6
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp6 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp6_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 7
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp7 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp7_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 8
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp8 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp8_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 9
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp9 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp9_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 10
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp10 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp10_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 11
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp11 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp11_run ## _fastcall
#endif

#elif defined OS_BIND_FUNC_THISCALL

#ifdef __GNUC__
#define OS_BIND_FUNC_CC
#define OS_BIND_FUNC_CC_GNUC __attribute__((thiscall))
#else
#define OS_BIND_FUNC_CC __thiscall
#define OS_BIND_FUNC_CC_GNUC
#endif

#if OS_BIND_FUNC_NUM_ARGS == 0
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp0 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp0_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 1
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp1 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp1_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 2
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp2 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp2_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 3
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp3 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp3_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 4
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp4 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp4_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 5
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp5 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp5_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 6
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp6 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp6_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 7
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp7 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp7_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 8
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp8 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp8_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 9
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp9 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp9_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 10
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp10 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp10_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 11
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp11 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp11_run ## _thiscall
#endif

#else

#define OS_BIND_FUNC_CC
#define OS_BIND_FUNC_CC_GNUC

#if OS_BIND_FUNC_NUM_ARGS == 0
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp0
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp0_run
#elif OS_BIND_FUNC_NUM_ARGS == 1
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp1
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp1_run
#elif OS_BIND_FUNC_NUM_ARGS == 2
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp2
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp2_run
#elif OS_BIND_FUNC_NUM_ARGS == 3
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp3
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp3_run
#elif OS_BIND_FUNC_NUM_ARGS == 4
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp4
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp4_run
#elif OS_BIND_FUNC_NUM_ARGS == 5
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp5
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp5_run
#elif OS_BIND_FUNC_NUM_ARGS == 6
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp6
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp6_run
#elif OS_BIND_FUNC_NUM_ARGS == 7
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp7
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp7_run
#elif OS_BIND_FUNC_NUM_ARGS == 8
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp8
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp8_run
#elif OS_BIND_FUNC_NUM_ARGS == 9
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp9
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp9_run
#elif OS_BIND_FUNC_NUM_ARGS == 10
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp10
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp10_run
#elif OS_BIND_FUNC_NUM_ARGS == 11
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionClassImp11
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionClassImp11_run
#endif

#endif

template <class R, class T OS_BIND_FUNC_PARMS_COMMA OS_BIND_FUNC_TEMPLATE_PARMS> 
struct OS_BIND_FUNC_RUN_CLASS_NAME
{
	typedef R(OS_BIND_FUNC_CC T::*F)(OS_BIND_FUNC_PARMS) OS_BIND_FUNC_CC_GNUC;

	static int run(OS * os, int params, int, int, void * user_param)
	{
		OS_GET_TEMPLATE_SELF(T*);
		OS_BIND_FUNC_GET_ARGS;
		typedef typename RemoveConst<R>::type type;
		F& f = *(F*)user_param;
		// CtypeValue<type>::push(os, CtypeValue<type>::to((self->*f)(OS_BIND_FUNC_ARGS)));
		CtypeValue<type>::push(os, (self->*f)(OS_BIND_FUNC_ARGS));
		return 1;
	}
};

template <class T OS_BIND_FUNC_PARMS_COMMA OS_BIND_FUNC_TEMPLATE_PARMS> 
struct OS_BIND_FUNC_RUN_CLASS_NAME<void, T OS_BIND_FUNC_PARMS_COMMA OS_BIND_FUNC_PARMS>
{
	typedef void(OS_BIND_FUNC_CC T::*F)(OS_BIND_FUNC_PARMS) OS_BIND_FUNC_CC_GNUC;

	static int run(OS * os, int params, int, int, void * user_param)
	{
		OS_GET_TEMPLATE_SELF(T*);
		OS_BIND_FUNC_GET_ARGS;
		F& f = *(F*)user_param;
		(self->*f)(OS_BIND_FUNC_ARGS);
		return 0;
	}
};

template <class R, class T OS_BIND_FUNC_PARMS_COMMA OS_BIND_FUNC_TEMPLATE_PARMS>
struct OS_BIND_FUNC_CLASS_NAME
{
	typedef R(OS_BIND_FUNC_CC T::*F)(OS_BIND_FUNC_PARMS) OS_BIND_FUNC_CC_GNUC;

	const char * name;
	F f;

	OS_BIND_FUNC_CLASS_NAME(const char * _name, F _f): name(_name), f(_f){}
	
	operator OS::FuncDef() const 
	{ 
		OS::FuncDef def = {name, 
			OS_BIND_FUNC_RUN_CLASS_NAME<R, T OS_BIND_FUNC_PARMS_COMMA OS_BIND_FUNC_PARMS>::run, 
			&(FunctionData<F>::create(f))->f}; 
		return def; 
	}
};

// namespace ObjectScript {

template <class R, class T OS_BIND_FUNC_PARMS_COMMA OS_BIND_FUNC_TEMPLATE_PARMS> 
OS::FuncDef def(const char * name, R(OS_BIND_FUNC_CC T::*f)(OS_BIND_FUNC_PARMS) OS_BIND_FUNC_CC_GNUC)
{
	typedef OS_BIND_FUNC_CLASS_NAME<R, T OS_BIND_FUNC_PARMS_COMMA OS_BIND_FUNC_PARMS> Func; 
	return Func(name, f);
}

// } // namespace ObjectScript

#undef OS_BIND_FUNC_CLASS_NAME
#undef OS_BIND_FUNC_RUN_CLASS_NAME
#undef OS_BIND_FUNC_CC

#undef OS_BIND_FUNC_CC_GNUC

#ifdef OS_BIND_FUNC_CDECL

#ifdef __GNUC__
#define OS_BIND_FUNC_CC
#define OS_BIND_FUNC_CC_GNUC __attribute__((cdecl))
#else
#define OS_BIND_FUNC_CC __cdecl
#define OS_BIND_FUNC_CC_GNUC
#endif

#if OS_BIND_FUNC_NUM_ARGS == 0
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp0 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp0_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 1
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp1 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp1_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 2
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp2 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp2_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 3
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp3 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp3_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 4
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp4 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp4_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 5
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp5 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp5_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 6
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp6 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp6_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 7
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp7 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp7_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 8
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp8 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp8_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 9
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp9 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp9_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 10
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp10 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp10_run ## _cdecl
#elif OS_BIND_FUNC_NUM_ARGS == 11
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp11 ## _cdecl
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp11_run ## _cdecl
#endif

#elif defined OS_BIND_FUNC_STDCALL

#ifdef __GNUC__
#define OS_BIND_FUNC_CC
#define OS_BIND_FUNC_CC_GNUC __attribute__((stdcall))
#else
#define OS_BIND_FUNC_CC __stdcall
#define OS_BIND_FUNC_CC_GNUC
#endif

#if OS_BIND_FUNC_NUM_ARGS == 0
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp0 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp0_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 1
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp1 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp1_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 2
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp2 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp2_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 3
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp3 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp3_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 4
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp4 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp4_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 5
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp5 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp5_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 6
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp6 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp6_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 7
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp7 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp7_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 8
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp8 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp8_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 9
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp9 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp9_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 10
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp10 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp10_run ## _stdcall
#elif OS_BIND_FUNC_NUM_ARGS == 11
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp11 ## _stdcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp11_run ## _stdcall
#endif

#elif defined OS_BIND_FUNC_FASTCALL

#ifdef __GNUC__
#define OS_BIND_FUNC_CC
#define OS_BIND_FUNC_CC_GNUC __attribute__((fastcall))
#else
#define OS_BIND_FUNC_CC __fastcall
#define OS_BIND_FUNC_CC_GNUC
#endif

#if OS_BIND_FUNC_NUM_ARGS == 0
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp0 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp0_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 1
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp1 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp1_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 2
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp2 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp2_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 3
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp3 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp3_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 4
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp4 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp4_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 5
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp5 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp5_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 6
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp6 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp6_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 7
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp7 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp7_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 8
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp8 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp8_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 9
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp9 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp9_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 10
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp10 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp10_run ## _fastcall
#elif OS_BIND_FUNC_NUM_ARGS == 11
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp11 ## _fastcall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp11_run ## _fastcall
#endif

#elif defined OS_BIND_FUNC_THISCALL

#ifdef __GNUC__
#define OS_BIND_FUNC_CC
#define OS_BIND_FUNC_CC_GNUC __attribute__((thiscall))
#else
#define OS_BIND_FUNC_CC __thiscall
#define OS_BIND_FUNC_CC_GNUC
#endif

#if OS_BIND_FUNC_NUM_ARGS == 0
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp0 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp0_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 1
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp1 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp1_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 2
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp2 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp2_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 3
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp3 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp3_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 4
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp4 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp4_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 5
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp5 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp5_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 6
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp6 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp6_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 7
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp7 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp7_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 8
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp8 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp8_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 9
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp9 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp9_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 10
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp10 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp10_run ## _thiscall
#elif OS_BIND_FUNC_NUM_ARGS == 11
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp11 ## _thiscall
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp11_run ## _thiscall
#endif

#else

#define OS_BIND_FUNC_CC
#define OS_BIND_FUNC_CC_GNUC

#if OS_BIND_FUNC_NUM_ARGS == 0
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp0
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp0_run
#elif OS_BIND_FUNC_NUM_ARGS == 1
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp1
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp1_run
#elif OS_BIND_FUNC_NUM_ARGS == 2
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp2
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp2_run
#elif OS_BIND_FUNC_NUM_ARGS == 3
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp3
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp3_run
#elif OS_BIND_FUNC_NUM_ARGS == 4
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp4
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp4_run
#elif OS_BIND_FUNC_NUM_ARGS == 5
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp5
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp5_run
#elif OS_BIND_FUNC_NUM_ARGS == 6
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp6
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp6_run
#elif OS_BIND_FUNC_NUM_ARGS == 7
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp7
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp7_run
#elif OS_BIND_FUNC_NUM_ARGS == 8
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp8
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp8_run
#elif OS_BIND_FUNC_NUM_ARGS == 9
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp9
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp9_run
#elif OS_BIND_FUNC_NUM_ARGS == 10
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp10
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp10_run
#elif OS_BIND_FUNC_NUM_ARGS == 11
#define OS_BIND_FUNC_CLASS_NAME OS_FunctionImp11
#define OS_BIND_FUNC_RUN_CLASS_NAME OS_FunctionImp11_run
#endif

#endif

#if OS_BIND_FUNC_NUM_ARGS > 0

template <class R OS_BIND_FUNC_PARMS_COMMA OS_BIND_FUNC_TEMPLATE_PARMS> 
struct OS_BIND_FUNC_RUN_CLASS_NAME
{
	typedef R(OS_BIND_FUNC_CC *F)(OS_BIND_FUNC_PARMS) OS_BIND_FUNC_CC_GNUC;

	static int run(OS * os, int params, int, int, void * user_param)
	{
		OS_BIND_FUNC_GET_ARGS;
		typedef typename RemoveConst<R>::type type;
		F& f = *(F*)user_param;
		CtypeValue<type>::push(os, (*f)(OS_BIND_FUNC_ARGS));
		return 1;
	}
};

template <OS_BIND_FUNC_TEMPLATE_PARMS> 
struct OS_BIND_FUNC_RUN_CLASS_NAME<void OS_BIND_FUNC_PARMS_COMMA OS_BIND_FUNC_PARMS>
{
	typedef void(OS_BIND_FUNC_CC *F)(OS_BIND_FUNC_PARMS) OS_BIND_FUNC_CC_GNUC;

	static int run(OS * os, int params, int, int, void * user_param)
	{
		OS_BIND_FUNC_GET_ARGS;
		F& f = *(F*)user_param;
		(*f)(OS_BIND_FUNC_ARGS);
		return 0;
	}
};

template <class R OS_BIND_FUNC_PARMS_COMMA OS_BIND_FUNC_TEMPLATE_PARMS>
struct OS_BIND_FUNC_CLASS_NAME
{
	typedef R(OS_BIND_FUNC_CC *F)(OS_BIND_FUNC_PARMS) OS_BIND_FUNC_CC_GNUC;

	const char * name;
	F f;

	OS_BIND_FUNC_CLASS_NAME(const char * _name, F _f): name(_name), f(_f){}

	operator OS::FuncDef() const 
	{ 
		OS::FuncDef def = {name, 
			OS_BIND_FUNC_RUN_CLASS_NAME<R OS_BIND_FUNC_PARMS_COMMA OS_BIND_FUNC_PARMS>::run, 
			&(FunctionData<F>::create(f))->f}; 
		return def; 
	}
};

// namespace ObjectScript {

template <class R OS_BIND_FUNC_PARMS_COMMA OS_BIND_FUNC_TEMPLATE_PARMS> 
OS::FuncDef def(const char * name, R(OS_BIND_FUNC_CC *f)(OS_BIND_FUNC_PARMS) OS_BIND_FUNC_CC_GNUC)
{
	typedef OS_BIND_FUNC_CLASS_NAME<R OS_BIND_FUNC_PARMS_COMMA OS_BIND_FUNC_PARMS> Func; 
	return Func(name, f);
}

// } // namespace ObjectScript

#else

template <class R> 
struct OS_BIND_FUNC_RUN_CLASS_NAME
{
	typedef R(OS_BIND_FUNC_CC *F)() OS_BIND_FUNC_CC_GNUC;

	static int run(OS * os, int params, int, int, void * user_param)
	{
		OS_BIND_FUNC_GET_ARGS;
		typedef typename RemoveConst<R>::type type;
		F& f = *(F*)user_param;
		CtypeValue<type>::push(os, (*f)());
		return 1;
	}
};

template <> 
struct OS_BIND_FUNC_RUN_CLASS_NAME<void>
{
	typedef void(OS_BIND_FUNC_CC *F)() OS_BIND_FUNC_CC_GNUC;

	static int run(OS * os, int params, int, int, void * user_param)
	{
		OS_BIND_FUNC_GET_ARGS;
		F& f = *(F*)user_param;
		(*f)();
		return 0;
	}
};

template <class R>
struct OS_BIND_FUNC_CLASS_NAME
{
	typedef R(OS_BIND_FUNC_CC *F)() OS_BIND_FUNC_CC_GNUC;

	const char * name;
	F f;

	OS_BIND_FUNC_CLASS_NAME(const char * _name, F _f): name(_name), f(_f){}

	operator OS::FuncDef() const 
	{ 
		OS::FuncDef def = {name, 
			OS_BIND_FUNC_RUN_CLASS_NAME<R>::run, 
			&(FunctionData<F>::create(f))->f}; 
		return def; 
	}
};

// namespace ObjectScript {

template <class R> 
OS::FuncDef def(const char * name, R(OS_BIND_FUNC_CC *f)() OS_BIND_FUNC_CC_GNUC)
{
	typedef OS_BIND_FUNC_CLASS_NAME<R> Func; 
	return Func(name, f);
}

// } // namespace ObjectScript

#endif



#undef OS_BIND_FUNC_CLASS_NAME
#undef OS_BIND_FUNC_RUN_CLASS_NAME
#undef OS_BIND_FUNC_CC

#undef OS_BIND_FUNC_CC_GNUC

#undef OS_BIND_FUNC_PARMS_COMMA
#undef OS_BIND_FUNC_TEMPLATE_PARMS
#undef OS_BIND_FUNC_PARMS
#undef OS_BIND_FUNC_ARGS
#undef OS_BIND_FUNC_GET_ARGS
