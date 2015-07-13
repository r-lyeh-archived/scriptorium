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

#if defined __GNUC__ || !(defined(__i386__) || defined(__i386) || defined(__X86__))
#include "os-binder-function.h"
#else

#define OS_BIND_FUNC_CDECL
#include "os-binder-function.h"
#undef OS_BIND_FUNC_CDECL

#define OS_BIND_FUNC_STDCALL
#include "os-binder-function.h"
#undef OS_BIND_FUNC_STDCALL

#define OS_BIND_FUNC_FASTCALL
#include "os-binder-function.h"
#undef OS_BIND_FUNC_FASTCALL

#define OS_BIND_FUNC_THISCALL
#include "os-binder-function.h"
#undef OS_BIND_FUNC_THISCALL

#endif
