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

#ifndef DAO_REGEX_H
#define DAO_REGEX_H

#include"daoType.h"

typedef struct DaoRgxItem DaoRgxItem;

struct DaoRgxItem
{
	uchar_t  type; /* type of the pattern */
	uchar_t  config;
	short    gid;
	short    next;
	short    jump;
	short    from;
	short    min;
	short    max;
	daoint   count;
	daoint   pos;
	daoint   offset;
	daoint   posave;
	short    fromsave;
	short    length; /* length of the pattern */
	short    word;
};

struct DaoRegex
{
	char   *source;
	daoint  start;
	daoint  end;
	DaoRgxItem *items;
	short  count; /* total number of items; or free space in the buffer as input; */
	short  config;
	short  attrib;
	short  group;
	short  indexed;
	char  *wordbuf;
	int    itemlen; /* in bytes */
	int    wordlen; /* in bytes */
	int    length;
};

DAO_DLL DaoRegex* DaoRegex_New( DString *src );
#define DaoRegex_Delete( self ) dao_free( self )
DAO_DLL void DaoRegex_Copy( DaoRegex *self, DaoRegex *src );

/* compute the number of bytes needed for storing the compiled pattern */
DAO_DLL int DaoRegex_CheckSize( DString *src );

DAO_DLL int DaoRegex_Match( DaoRegex *self, DString *src, daoint *start, daoint *end );
DAO_DLL int DaoRegex_SubMatch( DaoRegex *self, int gid, daoint *start, daoint *end );

DAO_DLL int DaoRegex_Change( DaoRegex *self, DString *src, DString *target, int index );
DAO_DLL int DaoRegex_ChangeExt( DaoRegex *self, DString *input, DString *output,
		DString *target, int index, daoint *start2, daoint *end2 );

#endif
