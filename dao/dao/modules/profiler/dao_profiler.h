/*
// Dao Profiler
//
// Copyright (c) 2013, Limin Fu
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

#ifndef DAO_PROFILER_H
#define DAO_PROFILER_H

#include "daoValue.h"
#include "daoStream.h"
#include "daoRoutine.h"
#include "daoProcess.h"
#include "daoNamespace.h"
#include "daoVmspace.h"


typedef struct DaoxProfiler DaoxProfiler;

struct DaoxProfiler
{
	DaoProfiler base;

	DMutex  mutex;
	DMap   *profile; /* map<DaoRoutine*,map<DaoRoutine*,DaoComplex*>> */
	DMap   *one;     /* map<DaoRoutine*,DaoComplex*> */
};

DAO_DLL DaoxProfiler* DaoxProfiler_New();
DAO_DLL void DaoxProfiler_Delete( DaoxProfiler *self );

DAO_DLL dao_complex DaoProfiler_Sum( DMap *profile );


DAO_DLL void DaoRoutine_MakeName( DaoRoutine *self, DString *name, int max1, int max2, int max3 );

DAO_DLL int DaoProfiler_OnLoad( DaoVmSpace *vmSpace, DaoNamespace *ns );

#endif
