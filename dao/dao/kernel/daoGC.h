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

#ifndef DAO_GC_H
#define DAO_GC_H

#include"daoType.h"

/*
// The DaoCdata and DaoCinValue objects must be globally unique for the wrapped data.
// Because relations regarding to the inheritance can only be set when the wrappers
// are initialized at the first time.
*/
DAO_DLL DaoCdata* DaoWrappers_MakeCdata( DaoType *type, void *data, int owned );
DAO_DLL DaoCinValue* DaoWrappers_MakeCinValue( DaoCinType *type, DaoValue *value );


#ifdef DAO_USE_GC_LOGGER
DAO_DLL void DaoObjectLogger_Init();
DAO_DLL void DaoObjectLogger_Quit();
DAO_DLL void DaoObjectLogger_LogNew( DaoValue *object );
DAO_DLL void DaoObjectLogger_LogDelete( DaoValue *object );
DAO_DLL void DaoObjectLogger_PrintProfile();
#endif

DAO_DLL int DaoGC_Min( int n /*=-1*/ );
DAO_DLL int DaoGC_Max( int n /*=-1*/ );

DAO_DLL void DaoGC_Start();
DAO_DLL void DaoGC_Finish();
DAO_DLL void DaoGC_TryInvoke();

DAO_DLL void DaoCGC_Start();

DAO_DLL void DaoGC_IncRC( DaoValue *dbase );
DAO_DLL void DaoGC_DecRC( DaoValue *dbase );
DAO_DLL void DaoGC_Assign( DaoValue **dest, DaoValue *src );

DAO_DLL void DaoGC_IncRCs( DList *dbases );
DAO_DLL void DaoGC_DecRCs( DList *dbases );

#define GC_IncRC( p )        DaoGC_IncRC( (DaoValue*)(p) )
#define GC_DecRC( p )        DaoGC_DecRC( (DaoValue*)(p) )
#define GC_Assign(dest,src)  DaoGC_Assign( (DaoValue**)(dest), (DaoValue*)(src) );


DAO_DLL void DaoGC_LockData();
DAO_DLL void DaoGC_UnlockData();


#endif
