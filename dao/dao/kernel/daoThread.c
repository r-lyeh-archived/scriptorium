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

#include<string.h>
#include<math.h>

#include"daoThread.h"
#include"daoMap.h"
#include"daoGC.h"
#include"daoProcess.h"
#include"daoVmspace.h"
#include"daoRoutine.h"
#include"daoObject.h"
#include"daoClass.h"
#include"daoValue.h"
#include"daoTasklet.h"

#ifdef DAO_WITH_THREAD
/* Basic threading interfaces */


static void DThread_Detach( DThread *self );
static void DThread_Cancel( DThread *self );
static void DThread_TestCancel( DThread *self );

static dao_thdspec_t thdSpecKey = 0;

#ifdef UNIX

void DMutex_Init( DMutex *self )
{
	pthread_mutex_init( & self->myMutex, NULL );
}
void DMutex_Destroy( DMutex *self )
{
	pthread_mutex_destroy( & self->myMutex );
}
void DMutex_Lock( DMutex *self )
{
	pthread_mutex_lock( & self->myMutex );
}
void DMutex_Unlock( DMutex *self )
{
	pthread_mutex_unlock( & self->myMutex );
}
int DMutex_TryLock( DMutex *self )
{
	return (pthread_mutex_trylock( & self->myMutex ) ==0);
}

void DCondVar_Init( DCondVar *self )
{
	pthread_cond_init( & self->myCondVar, NULL );
}
void DCondVar_Destroy( DCondVar *self )
{
	pthread_cond_destroy( & self->myCondVar );
}
void DCondVar_Wait( DCondVar *self, DMutex *mutex )
{
	pthread_cond_wait( & self->myCondVar, & mutex->myMutex );
}
int DCondVar_TimedWait( DCondVar *self, DMutex *mutex, double seconds )
{
	long sec = floor( seconds );
	long nsec = (long)(( seconds - sec ) * 1E9);
	struct timeval now;
	struct timespec timeout;
	int retc = 0;

	gettimeofday(&now, NULL);
	timeout.tv_sec = now.tv_sec + sec;
	timeout.tv_nsec = now.tv_usec * 1000 + nsec;
	if( timeout.tv_nsec >= 1E9 ){
		timeout.tv_sec ++;
		timeout.tv_nsec -= 1E9;
	}

	retc = pthread_cond_timedwait( & self->myCondVar, & mutex->myMutex, & timeout );
	return ( retc == ETIMEDOUT );
}

void DCondVar_Signal( DCondVar *self )
{
	pthread_cond_signal( & self->myCondVar );
}
void DCondVar_BroadCast( DCondVar *self )
{
	pthread_cond_broadcast( & self->myCondVar );
}


void DThread_Init( DThread *self )
{
	self->cleaner = NULL;
	self->myThread = 0;
	self->taskFunc = NULL;
	self->taskArg = NULL;
	self->thdSpecData = NULL;
	DCondVar_Init( & self->condv );
}
void DThread_Destroy( DThread *self )
{
	DCondVar_Destroy( & self->condv );
}

static void* DThread_Wrapper( void *p )
{
	DThread *self = (DThread*) p;
	if( self->thdSpecData == NULL ){
		self->thdSpecData = (DThreadData*)dao_calloc( 1, sizeof(DThreadData) );
		self->thdSpecData->thdObject = self;
	}
	self->thdSpecData->state = 0;
	pthread_setspecific( thdSpecKey, self->thdSpecData );

	if( self->cleaner ){
		pthread_cleanup_push( self->cleaner, self->taskArg );
		if( self->taskFunc ) self->taskFunc( self->taskArg );
		pthread_cleanup_pop( 1 );
	}else{
		if( self->taskFunc ) self->taskFunc( self->taskArg );
	}
	pthread_exit( 0 );
	return NULL;
}

int DThread_Start( DThread *self, DThreadTask task, void *arg )
{
	self->taskFunc = task;
	self->taskArg = arg;
	return pthread_create( & self->myThread, NULL, & DThread_Wrapper, (void*)self ) == 0;
}
void DThread_Join( DThread *self )
{
	pthread_join( self->myThread, NULL );
}
void DThread_Detach( DThread *self )
{
	pthread_detach( self->myThread );
}
void DThread_Cancel( DThread *self )
{
	pthread_cancel( self->myThread );
}
void DThread_TestCancel( DThread *self )
{
	pthread_testcancel();
}
void DThread_Exit( DThread *self )
{
	pthread_exit( NULL );
}

dao_thread_t DThread_Self()
{
	return pthread_self();
}
int DThread_Equal( dao_thread_t x, dao_thread_t y )
{
	return pthread_equal( x, y );
}
DThreadData* DThread_GetSpecific()
{
	return (DThreadData*) pthread_getspecific( thdSpecKey );
}

static void DaoThread_SysInit()
{
	pthread_key_create( & thdSpecKey, free );
}
static void DaoThread_SysQuit()
{
	pthread_key_delete( thdSpecKey );
}

#elif _WIN32

#if _WIN32_WINNT < 0x0400
#define _WIN32_WINNT 0x0400
#endif

void DMutex_Init( DMutex *self )
{
	InitializeCriticalSection( & self->myMutex );
}
void DMutex_Destroy( DMutex *self )
{
	DeleteCriticalSection( & self->myMutex );
}
void DMutex_Lock( DMutex *self )
{
	EnterCriticalSection( & self->myMutex );
}
void DMutex_Unlock( DMutex *self )
{
	LeaveCriticalSection( & self->myMutex );
}
int DMutex_TryLock( DMutex *self )
{
	return TryEnterCriticalSection( & self->myMutex );
}

void DCondVar_Init( DCondVar *self )
{
	self->thdWaiting = DList_New(0);
	DMutex_Init( & self->thdMutex );
	/* manual reset, when signaled, all waiting threads will be waked up: */
	self->myCondVar = CreateEvent( NULL, TRUE, FALSE, NULL );
}
void DCondVar_Destroy( DCondVar *self )
{
	DList_Delete( self->thdWaiting );
	DMutex_Destroy( & self->thdMutex );
	CloseHandle( self->myCondVar );
}

void DCondVar_Wait( DCondVar *self, DMutex *mtx )
{
	DThreadData *p = (DThreadData*)TlsGetValue( thdSpecKey );

	DMutex_Lock( & self->thdMutex );
	DList_PushBack( self->thdWaiting, (void*) p->thdObject );
	DMutex_Unlock( & self->thdMutex );

	if( mtx ) DMutex_Unlock( mtx );
	WaitForSingleObject( p->thdObject->condv.myCondVar, INFINITE );
	ResetEvent( p->thdObject->condv.myCondVar );
	if( mtx ) DMutex_Lock( mtx );

	if( p->state & DTHREAD_CANCELED ) DThread_Exit( p->thdObject );
}
int DCondVar_TimedWait( DCondVar *self, DMutex *mtx, double seconds )
{
	DWORD retc;
	DThreadData *p = (DThreadData*)TlsGetValue( thdSpecKey );

	DMutex_Lock( & self->thdMutex );
	DList_PushBack( self->thdWaiting, (void*) p->thdObject );
	DMutex_Unlock( & self->thdMutex );

	if( mtx ) DMutex_Unlock( mtx );
	retc = WaitForSingleObject( p->thdObject->condv.myCondVar, (DWORD)( seconds * 1000 ) );
	ResetEvent( p->thdObject->condv.myCondVar );
	if( mtx ) DMutex_Lock( mtx );

	if( p->state & DTHREAD_CANCELED ) DThread_Exit( p->thdObject );
	return ( retc == WAIT_TIMEOUT );
}
void DCondVar_Signal( DCondVar *self )
{
	DThread *thread;
	DMutex_Lock( & self->thdMutex );
	if( self->thdWaiting->size > 0 ){
		thread = (DThread*) self->thdWaiting->items.pVoid[0];
		SetEvent( thread->condv.myCondVar );
		DList_PopFront( self->thdWaiting );
	}
	DMutex_Unlock( & self->thdMutex );

}
void DCondVar_BroadCast( DCondVar *self )
{
	DThread *thread;
	int i;
	DMutex_Lock( & self->thdMutex );
	for( i=0; i<self->thdWaiting->size; i++ ){
		thread = (DThread*) self->thdWaiting->items.pVoid[i];
		SetEvent( thread->condv.myCondVar );
	}
	DList_Clear( self->thdWaiting );
	DMutex_Unlock( & self->thdMutex );
}


void DThread_Init( DThread *self )
{
	self->myThread = 0;
	self->thdSpecData = NULL;
	self->cleaner = NULL;
	self->taskFunc = NULL;
	self->taskArg = NULL;
	DCondVar_Init( & self->condv );
}
void DThread_Destroy( DThread *self )
{
	if( self->myThread ) CloseHandle( self->myThread );
	DCondVar_Destroy( & self->condv );
	GlobalFree( self->thdSpecData );
	self->thdSpecData = NULL;
}
void DThread_Wrapper( void *object )
{
	DThread *self = (DThread*)object;
	self->running = 1;

	if( self->thdSpecData == NULL ){
		self->thdSpecData = (DThreadData*)GlobalAlloc( GPTR, sizeof(DThreadData) );
		self->thdSpecData->thdObject = self;
	}
	self->thdSpecData->state = 0;
	TlsSetValue( thdSpecKey, self->thdSpecData );

	if( self->taskFunc ) self->taskFunc( self->taskArg );
	DThread_Exit( self );
}
int DThread_Start( DThread *self, DThreadTask task, void *arg )
{
	self->taskFunc = task;
	self->taskArg = arg;
	self->myThread = (HANDLE)_beginthread( DThread_Wrapper, 0, (void*)self );
	return (self->myThread != 0);
}
void DThread_Join( DThread *self )
{
	if( self->running ) DCondVar_Wait( & self->condv, NULL );
}
void DThread_Detach( DThread *self )
{
	DCondVar_Signal( & self->condv );
}
void DThread_Cancel( DThread *self )
{
	self->thdSpecData->state |= DTHREAD_CANCELED;
	DCondVar_Signal( & self->condv );
}
void DThread_TestCancel( DThread *self )
{
	if( self->thdSpecData->state & DTHREAD_CANCELED ){
		self->thdSpecData->state = 0;
		DThread_Exit( self );
	}
}
dao_thread_t DThread_Self()
{
	return GetCurrentThread();
}
int DThread_Equal( dao_thread_t x, dao_thread_t y )
{
	return ( x == y );
}
void DThread_Exit( DThread *thd )
{
	thd->running = 0;
	DCondVar_Signal( & thd->condv );
	if( thd->cleaner ) (*(thd->cleaner))( thd->taskArg );
	thd->myThread = NULL; /* it will be closed by _endthread() */
	_endthread();
}

DThreadData* DThread_GetSpecific()
{
	return (DThreadData*) TlsGetValue( thdSpecKey );
}

/* DThread object for the main thread, used for join() */
DThread mainThread;

static void DaoThread_SysInit()
{
	thdSpecKey = (dao_thdspec_t)TlsAlloc();
	DThread_Init( & mainThread );

	mainThread.thdSpecData = (DThreadData*)GlobalAlloc( GPTR, sizeof(DThreadData) );
	mainThread.thdSpecData->thdObject = & mainThread;
	mainThread.thdSpecData->state = 0;

	TlsSetValue( thdSpecKey, mainThread.thdSpecData );
}
static void DaoThread_SysQuit()
{
	DThread_Destroy( & mainThread );
	TlsFree( thdSpecKey );
}
#endif /* WIN32	*/

static dao_thread_t daoMainThreadID;
void DaoInitThread()
{
	DaoThread_SysInit();
	daoMainThreadID = DThread_Self();
}
void DaoQuitThread()
{
	DaoThread_SysQuit();
}
int DThread_IsMain()
{
	dao_thread_t threadid = DThread_Self();
	return DThread_Equal( threadid, daoMainThreadID );
}
#else

int DThread_IsMain()
{
	return 1;
}
#endif /* DAO_WITH_THREAD */





#ifdef DAO_WITH_CONCURRENT
/* mt module: */


typedef struct DaoTaskData DaoTaskData;
struct DaoTaskData
{
	DaoValue    *param; /* parameter container: list, map or array; */
	DaoValue    *result; /* result container: list or array; */
	DaoProcess  *proto; /* caller's process; */
	DaoProcess  *clone; /* spawned process; */
	DaoVmCode   *sect; /* DVM_SECT */

	DCondVar  *condv;
	DMutex    *mutex;

	uint_t   funct; /* type of functional; */
	uint_t   entry; /* entry code; */
	uint_t   first; /* first index; */
	uint_t   step; /* index step; */
	uint_t   status; /* execution status; */
	daoint  *joined; /* number of joined threads; */
	daoint  *index; /* smallest index found by all threads; */
	DNode  **node; /* smallest key found by all threads; */
};

static void DaoMT_InitProcess( DaoProcess *proto, DaoProcess *clone, int argcount )
{
	DaoProcess_PushRoutine( clone, proto->activeRoutine, proto->activeObject );
	clone->activeCode = proto->activeCode;
	DaoProcess_PushFunction( clone, proto->topFrame->routine );
	DaoProcess_InitCodeSection( clone, argcount );
	clone->topFrame->outer = proto;
	clone->topFrame->host = proto->topFrame->prev;
	clone->topFrame->returning = -1;
}
static void DaoMT_RunIterateFunctional( void *p )
{
	DaoInteger idint = {DAO_INTEGER,0,0,0,0,0};
	DaoInteger tidint = {DAO_INTEGER,0,0,0,0,0};
	DaoValue *index = (DaoValue*)(void*)&idint;
	DaoValue *threadid = (DaoValue*)(void*)&tidint;
	DaoTaskData *self = (DaoTaskData*)p;
	DaoProcess *clone = self->clone;
	DaoVmCode *sect = self->sect;
	daoint i, n = self->param->xInteger.value;

	DaoMT_InitProcess( self->proto, clone, 2 );
	tidint.value = self->first;
	for(i=self->first; i<n; i+=self->step){
		idint.value = i;
		if( sect->b >0 ) DaoProcess_SetValue( clone, sect->a, index );
		if( sect->b >1 ) DaoProcess_SetValue( clone, sect->a+1, threadid );
		clone->topFrame->entry = self->entry;
		DaoProcess_Execute( clone );
		if( clone->status != DAO_PROCESS_FINISHED ) break;
	}
}
static void DaoMT_RunListFunctional( void *p )
{
	DaoValue *res;
	DaoInteger idint = {DAO_INTEGER,0,0,0,0,0};
	DaoInteger tidint = {DAO_INTEGER,0,0,0,0,0};
	DaoValue *index = (DaoValue*)(void*)&idint;
	DaoValue *threadid = (DaoValue*)(void*)&tidint;
	DaoTaskData *self = (DaoTaskData*)p;
	DaoList *list = (DaoList*) self->param;
	DaoList *list2 = (DaoList*) self->result;
	DaoProcess *clone = self->clone;
	DaoVmCode *sect = self->sect;
	DaoValue **items = list->value->items.pValue;
	daoint i, n = list->value->size;

	DaoMT_InitProcess( self->proto, clone, 3 );
	tidint.value = self->first;
	for(i=self->first; i<n; i+=self->step){
		idint.value = i;
		if( sect->b >0 ) DaoProcess_SetValue( clone, sect->a, items[i] );
		if( sect->b >1 ) DaoProcess_SetValue( clone, sect->a+1, index );
		if( sect->b >2 ) DaoProcess_SetValue( clone, sect->a+2, threadid );
		clone->topFrame->entry = self->entry;
		DaoProcess_Execute( clone );
		if( clone->status != DAO_PROCESS_FINISHED ) break;
		res = clone->stackValues[0];
		if( self->funct == DVM_FUNCT_MAP ){
			self->status |= DaoList_SetItem( list2, res, i );
		}else if( self->funct == DVM_FUNCT_APPLY ){
			self->status |= DaoList_SetItem( list, res, i );
		}else if( self->funct == DVM_FUNCT_FIND ){
			if( *self->index >= 0 && *self->index < i ) break;
			if( res->xInteger.value ){
				DMutex_Lock( self->mutex );
				if( *self->index < 0 || i < *self->index ) *self->index = i;
				DMutex_Unlock( self->mutex );
				break;
			}
		}
	}
}
static void DaoMT_RunMapFunctional( void *p )
{
	DaoValue *res;
	DaoInteger tidint = {DAO_INTEGER,0,0,0,0,0};
	DaoValue *threadid = (DaoValue*)(void*)&tidint;
	DaoTaskData *self = (DaoTaskData*)p;
	DaoMap *map = (DaoMap*) self->param;
	DaoList *list2 = (DaoList*) self->result;
	DaoProcess *clone = self->clone;
	DaoVmCode *sect = self->sect;
	DaoType *type = map->ctype;
	DNode *node = NULL;
	daoint i = 0;

	DaoMT_InitProcess( self->proto, clone, 3 );
	tidint.value = self->first;
	type = type && type->nested->size > 1 ? type->nested->items.pType[1] : NULL;
	for(node=DMap_First( map->value ); node; node=DMap_Next(map->value, node) ){
		if( (i++) % self->step != self->first ) continue;
		if( sect->b >0 ) DaoProcess_SetValue( clone, sect->a, node->key.pValue );
		if( sect->b >1 ) DaoProcess_SetValue( clone, sect->a+1, node->value.pValue );
		if( sect->b >2 ) DaoProcess_SetValue( clone, sect->a+2, threadid );
		clone->topFrame->entry = self->entry;
		DaoProcess_Execute( clone );
		if( clone->status != DAO_PROCESS_FINISHED ) break;
		res = clone->stackValues[0];
		if( self->funct == DVM_FUNCT_MAP ){
			self->status |= DaoList_SetItem( list2, res, i-1 );
		}else if( self->funct == DVM_FUNCT_APPLY ){
			self->status |= DaoValue_Move( res, & node->value.pValue, type ) == 0;
		}else if( self->funct == DVM_FUNCT_FIND ){
			DNode **p = self->node;
			/* XXX: 2014-11-11 */
			if( *p && DaoValue_Compare( (*p)->key.pValue, node->key.pValue ) < 0 ) break;
			if( res->xInteger.value ){
				DMutex_Lock( self->mutex );
				if( *p == NULL || DaoValue_Compare( (*p)->key.pValue, node->key.pValue ) >0 ) *p = node;
				DMutex_Unlock( self->mutex );
				break;
			}
		}
	}
}

#ifdef DAO_WITH_NUMARRAY
void DaoArray_GetSliceShape( DaoArray *self, daoint **dims, short *ndim );
int DaoArray_IndexFromSlice( DaoArray *self, DList *slice, daoint sid );
DaoValue* DaoArray_GetValue( DaoArray *self, daoint i, DaoValue *res );
void DaoArray_SetValue( DaoArray *self, daoint i, DaoValue *value );

static void DaoMT_RunArrayFunctional( void *p )
{
	DaoValue **idval;
	DaoValue *elem, *res = NULL;
	DaoValue tidint = {DAO_INTEGER};
	DaoValue com = {DAO_COMPLEX};
	DaoValue *threadid = (DaoValue*)(void*)&tidint;
	DaoTaskData *self = (DaoTaskData*)p;
	DaoProcess *clone = self->clone;
	DaoVmCode *sect = self->sect;
	DaoArray *param = (DaoArray*) self->param;
	DaoArray *result = (DaoArray*) self->result;
	DaoArray *array = DaoArray_GetWorkArray( param );
	daoint size = DaoArray_GetWorkSize( param );
	daoint start = DaoArray_GetWorkStart( param );
	daoint len = DaoArray_GetWorkIntervalSize( param );
	daoint step = DaoArray_GetWorkStep( param );
	daoint *dims = param->dims;
	daoint i, id, id2, n = size;
	int j, D = array->ndim;
	int isvec = (D == 2 && (dims[0] ==1 || dims[1] == 1));
	int stackBase, vdim = sect->b - 1;

	DaoMT_InitProcess( self->proto, clone, array->ndim + 1 );
	tidint.xInteger.value = self->first;

	stackBase = clone->topFrame->active->stackBase;
	idval = clone->activeValues + sect->a + 1;
	for(j=0; j<vdim; j++) idval[j]->xInteger.value = 0;
	for(i=self->first; i<n; i+=self->step){
		idval = clone->stackValues + stackBase + sect->a + 1;
		id = id2 = start + (i / len) * step + (i % len);
		if( isvec ){
			if( vdim >0 ) idval[0]->xInteger.value = id2;
			if( vdim >1 ) idval[1]->xInteger.value = id2;
		}else{
			for( j=D-1; j>=0; j--){
				int k = id2 % dims[j];
				id2 /= dims[j];
				if( j < vdim ) idval[j]->xInteger.value = k;
			}
		}
		elem = clone->stackValues[ stackBase + sect->a ];
		if( elem == NULL || elem->type != array->etype ){
			elem = (DaoValue*)&com;
			elem->type = array->etype;
			elem = DaoProcess_SetValue( clone, sect->a, elem );
		}
		DaoArray_GetValue( array, id, elem );
		if( sect->b > 6 ) DaoProcess_SetValue( clone, sect->a+6, threadid );
		clone->topFrame->entry = self->entry;
		DaoProcess_Execute( clone );
		if( clone->status != DAO_PROCESS_FINISHED ) break;
		res = clone->stackValues[0];
		if( self->funct == DVM_FUNCT_MAP ){
			DaoArray_SetValue( result, i, res );
		}else if( self->funct == DVM_FUNCT_APPLY ){
			DaoArray_SetValue( array, id, res );
		}
	}
}
#endif
static void DaoMT_RunFunctional( void *p )
{
	DaoTaskData *self = (DaoTaskData*)p;
	DaoProcess *clone = self->clone;
	switch( self->param->type ){
	case DAO_INTEGER : DaoMT_RunIterateFunctional( p ); break;
	case DAO_LIST  : DaoMT_RunListFunctional( p ); break;
	case DAO_MAP   : DaoMT_RunMapFunctional( p ); break;
#ifdef DAO_WITH_NUMARRAY
	case DAO_ARRAY : DaoMT_RunArrayFunctional( p ); break;
#endif
	}
	self->status |= clone->status != DAO_PROCESS_FINISHED;
	DMutex_Lock( self->mutex );
	*self->joined += 1;
	if( clone->exceptions->size ) DaoProcess_PrintException( clone, NULL, 1 );
	DCondVar_Signal( self->condv );
	DMutex_Unlock( self->mutex );
}
static void DaoMT_Functional( DaoProcess *proc, DaoValue *P[], int N, int F )
{
	DMutex mutex;
	DCondVar condv;
	DaoTaskData *tasks;
	DaoValue *param = P[0];
	DaoValue *result = NULL;
	DaoList *list = NULL;
	DaoArray *array = NULL;
	DaoVmCode *sect = NULL;
	DaoStackFrame *frame = DaoProcess_FindSectionFrame( proc );
	int i, entry, threads = P[1]->xInteger.value;
	daoint index = -1, status = 0, joined = 0;
	DNode *node = NULL;

	switch( F ){
	case DVM_FUNCT_MAP :
		if( param->type == DAO_ARRAY ){
			array = DaoProcess_PutArray( proc );
			result = (DaoValue*) array;
		}else{
			list = DaoProcess_PutList( proc );
			result = (DaoValue*) list;
		}
		break;
	case DVM_FUNCT_FIND : DaoProcess_PutValue( proc, dao_none_value ); break;
	}
	if( threads <= 0 ) threads = 2;
	if( frame != proc->topFrame->prev ){
		DaoProcess_RaiseError( proc, NULL, "Invalid code section from non-immediate caller" );
		return;
	}
	sect = DaoProcess_InitCodeSection( proc, 0 );
	if( sect == NULL ) return;
	if( list ){
		DList_Clear( list->value );
		if( param->type == DAO_LIST ) DList_Resize( list->value, param->xList.value->size, NULL );
		if( param->type == DAO_MAP ) DList_Resize( list->value, param->xMap.value->size, NULL );
#ifdef DAO_WITH_NUMARRAY
	}else if( array && F == DVM_FUNCT_MAP ){
		DaoArray_GetSliceShape( (DaoArray*) param, & array->dims, & array->ndim );
		DaoArray_ResizeArray( array, array->dims, array->ndim );
#endif
	}

	DMutex_Init( & mutex );
	DCondVar_Init( & condv );
	entry = proc->topFrame->entry;
	tasks = (DaoTaskData*) dao_calloc( threads, sizeof(DaoTaskData) );
	DaoProcess_PopFrame( proc );
	for(i=0; i<threads; i++){
		DaoTaskData *task = tasks + i;
		task->param = param;
		task->result = result;
		task->proto = proc;
		task->sect = sect;
		task->funct = F;
		task->entry = entry;
		task->first = i;
		task->step = threads;
		task->index = & index;
		task->node = & node;
		task->joined = & joined;
		task->condv = & condv;
		task->mutex = & mutex;
		task->clone = DaoVmSpace_AcquireProcess( proc->vmSpace );
		if( i ) DaoCallServer_AddTask( DaoMT_RunFunctional, task, 1 );
	}
	DaoMT_RunFunctional( tasks );

	DMutex_Lock( & mutex );
	while( joined < threads ) DCondVar_TimedWait( & condv, & mutex, 0.01 );
	DMutex_Unlock( & mutex );

	for(i=0; i<threads; i++){
		DaoTaskData *task = tasks + i;
		DaoVmSpace_ReleaseProcess( proc->vmSpace, task->clone );
		status |= task->status;
	}
	if( F == DVM_FUNCT_FIND ){
		DaoTuple *tuple = DaoProcess_PutTuple( proc, 2 );
		if( param->type == DAO_LIST && index != -1 ){
			DaoValue **items = param->xList.value->items.pValue;
			GC_Assign( & tuple->values[1], items[index] );
			tuple->values[0]->xInteger.value = index;
		}else if( param->type == DAO_MAP && node ){
			GC_Assign( & tuple->values[0], node->key.pValue );
			GC_Assign( & tuple->values[1], node->value.pValue );
		}
	}
	if( status ) DaoProcess_RaiseError( proc, NULL, "code section execution failed!" );
	DMutex_Destroy( & mutex );
	DCondVar_Destroy( & condv );
	dao_free( tasks );
}
static void DaoMT_Start0( void *p )
{
	DaoProcess *proc = (DaoProcess*)p;
	int count = proc->exceptions->size;
	DaoProcess_Start( proc );
	DaoProcess_ReturnFutureValue( proc, proc->future );
	if( proc->exceptions->size > count ) DaoProcess_PrintException( proc, NULL, 1 );
	if( proc->future->state == DAO_CALL_FINISHED ){
		DaoFuture_ActivateEvent( proc->future );
		DaoVmSpace_ReleaseProcess( proc->vmSpace, proc );
	}
}
static void DaoMT_Start( DaoProcess *proc, DaoValue *p[], int n )
{
	DaoProcess *clone;
	DaoVmCode *vmc, *end, *sect;
	DaoType *type = DaoProcess_GetReturnType( proc );
	DaoFuture *future = DaoFuture_New( type, 0 );
	int entry, nop = proc->activeCode[1].code == DVM_NOP;

	DaoProcess_PutValue( proc, (DaoValue*) future );
	sect = DaoProcess_InitCodeSection( proc, 0 );
	if( sect == NULL ) return;

	entry = proc->topFrame->entry;
	end = proc->activeRoutine->body->vmCodes->data.codes + proc->activeCode[nop+1].b;
	clone = DaoVmSpace_AcquireProcess( proc->vmSpace );
	DaoProcess_PopFrame( proc );
	DaoMT_InitProcess( proc, clone, 0 );
	clone->topFrame->entry = entry;
	/*
	// Use the cloned process instead of the parent process, in case that
	// the cloned process will not be joined by the parent process:
	*/
	clone->topFrame->outer = clone;
	future->process = clone;
	GC_IncRC( clone );
	GC_Assign( & clone->future, future );
	future->state = DAO_CALL_RUNNING;

	for(vmc=sect; vmc!=end; vmc++){
		int i = -1, code = vmc->code;
		if( code == DVM_GETVH || (code >= DVM_GETVH_I && code <= DVM_GETVH_C) ){
			i = vmc->b;
		}else if( code == DVM_SETVH || (code >= DVM_SETVH_II && code <= DVM_SETVH_CC) ){
			i = vmc->b;
		}
		if( i >= 0 ) DaoValue_Move( proc->activeValues[i], & clone->activeValues[i], NULL );
	}
	DaoCallServer_AddTask( DaoMT_Start0, clone, p[0]->xEnum.value );
}
static void DaoMT_Iterate( DaoProcess *proc, DaoValue *p[], int n )
{
	DaoMT_Functional( proc, p, n, DVM_FUNCT_NULL );
}
static void DaoMT_ListIterate( DaoProcess *proc, DaoValue *p[], int n )
{
	DaoMT_Functional( proc, p, n, DVM_FUNCT_ITERATE );
}
static void DaoMT_ListMap( DaoProcess *proc, DaoValue *p[], int n )
{
	DaoMT_Functional( proc, p, n, DVM_FUNCT_MAP );
}
static void DaoMT_ListApply( DaoProcess *proc, DaoValue *p[], int n )
{
	DaoMT_Functional( proc, p, n, DVM_FUNCT_APPLY );
}
static void DaoMT_ListFind( DaoProcess *proc, DaoValue *p[], int n )
{
	DaoMT_Functional( proc, p, n, DVM_FUNCT_FIND );
}
static void DaoMT_MapIterate( DaoProcess *proc, DaoValue *p[], int n )
{
	DaoMT_Functional( proc, p, n, DVM_FUNCT_ITERATE );
}
static void DaoMT_MapMap( DaoProcess *proc, DaoValue *p[], int n )
{
	DaoMT_Functional( proc, p, n, DVM_FUNCT_MAP );
}
static void DaoMT_MapApply( DaoProcess *proc, DaoValue *p[], int n )
{
	DaoMT_Functional( proc, p, n, DVM_FUNCT_APPLY );
}
static void DaoMT_MapFind( DaoProcess *proc, DaoValue *p[], int n )
{
	DaoMT_Functional( proc, p, n, DVM_FUNCT_FIND );
}
static void DaoMT_ArrayIterate( DaoProcess *proc, DaoValue *p[], int n )
{
	DaoMT_Functional( proc, p, n, DVM_FUNCT_ITERATE );
}
static void DaoMT_ArrayMap( DaoProcess *proc, DaoValue *p[], int n )
{
	DaoMT_Functional( proc, p, n, DVM_FUNCT_MAP );
}
static void DaoMT_ArrayApply( DaoProcess *proc, DaoValue *p[], int n )
{
	DaoMT_Functional( proc, p, n, DVM_FUNCT_APPLY );
}

static void DaoMT_RoutMutexSet( DMap *mutexes )
{
	DNode *it;
	for(it=DMap_First(mutexes); it; it=DMap_Next(mutexes,it)){
		DMutex *mutex = (DMutex*) it->value.pVoid;
		DMutex_Destroy( mutex );
		dao_free( mutex );
	}
	DMap_Delete( mutexes );
}
static DMutex* DaoMT_GetMutex( DaoRoutine *routine, void *key )
{
	DNode *it;
	DMap *mutexes;
	DMutex *mutex = NULL;

	DMutex_Lock( & mainVmSpace->mutexMisc );
	it = DMap_Find( routine->body->aux, DaoMT_RoutMutexSet );
	if( it == NULL ) it = DMap_Insert( routine->body->aux, DaoMT_RoutMutexSet, DHash_New(0,0) );
	mutexes = (DMap*) it->value.pVoid;
	it = DMap_Find( mutexes, key );
	if( it == NULL ){
		mutex = (DMutex*) dao_calloc(1,sizeof(DMutex));
		DMutex_Init( mutex );
		DMap_Insert( mutexes, key, mutex );
	}else{
		mutex = (DMutex*) it->value.pVoid;
	}
	DMutex_Unlock( & mainVmSpace->mutexMisc );
	return mutex;
}
static void DaoMT_ProcMutexCache( DMap *self )
{
	DMap_Delete( self );
}
static void DaoMT_Critical( DaoProcess *proc, DaoValue *p[], int n )
{
	void *key;
	DNode *it;
	DMap *cache = (DMap*) DaoProcess_GetAuxData( proc, DaoMT_ProcMutexCache );
	DaoVmCode *sect = DaoProcess_InitCodeSection( proc, 0 );
	DaoRoutine *routine = proc->activeRoutine;

	if( sect == NULL ) return;

	/* Get the original routine, if this one is a specialized copy: */
	while( routine->original ) routine = routine->original;

	/*
	// Use "routine + sect->c" instead of "sect" as the key for mutex,
	// as "sect" may be different for different copy of specialized routine.
	// But "sect->c" won't change after being set during compiling.
	*/
	key = routine + sect->c;

	if( cache == NULL ){
		cache = DHash_New(0,0); /* Local cache is used to avoid extra locking; */
		DaoProcess_SetAuxData( proc, DaoMT_ProcMutexCache, cache );
	}
	it = DMap_Find( cache, key ); /* Check local cache first; */
	if( it == NULL ) it = DMap_Insert( cache, key, DaoMT_GetMutex( routine, key ) );
	DMutex_Lock( (DMutex*) it->value.pVoid );
	DaoProcess_Execute( proc );
	DMutex_Unlock( (DMutex*) it->value.pVoid );
	DaoProcess_PopFrame( proc );
}

void DaoMT_Select( DaoProcess *proc, DaoValue *p[], int n );

DaoFuncItem dao_mt_methods[] =
{
	{ DaoMT_Critical,
		"critical()[]"
	},
	{ DaoMT_Start,
		"start( when: enum<auto,now> = $auto ) [ => @V|none] => Future<@V>"
	},
	{ DaoMT_Iterate,
		"iterate( times: int, threads = 2 ) [index: int, threadid: int]"
	},
	{ DaoMT_Select,
		"select( invar group: map<@T,int>, timeout = -1.0 )"
			"=> tuple<selected: none|@T, value: any, status: enum<selected,timeout,finished>>"
	},

	{ DaoMT_ListIterate,
		"iterate( alist: list<@T>, threads = 2 ) [item: @T, index: int, threadid: int]"
	},
	{ DaoMT_ListIterate,
		"iterate( invar alist: list<@T>, threads = 2 )"
			"[invar item: @T, index: int, threadid: int]"
	},
	{ DaoMT_ListMap,
		"map( invar alist: list<@T>, threads = 2 ) [item: @T, index: int, threadid: int => @V]"
			"=> list<@V>"
	},
	{ DaoMT_ListApply,
		"apply( alist: list<@T>, threads = 2 ) [item: @T, index: int, threadid: int => @T]"
	},
	{ DaoMT_ListFind,
		"find( invar alist: list<@T>, threads = 2 )"
			"[invar item: @T, index: int, threadid: int => int]"
			"=> tuple<index: int, item: @T> | none"
	},

	{ DaoMT_MapIterate,
		"iterate( amap: map<@K,@V>, threads = 2 ) [key: @K, value: @V, threadid: int]"
	},
	{ DaoMT_MapIterate,
		"iterate( invar amap: map<@K,@V>, threads = 2 )"
			"[invar key: @K, invar value: @V, threadid: int]"
	},
	{ DaoMT_MapMap,
		"map( invar amap: map<@K,@V>, threads = 2 ) [key: @K, value: @V, threadid: int => @T]"
			"=> list<@T>"
	},
	{ DaoMT_MapApply,
		"apply( amap: map<@K,@V>, threads = 2 ) [key: @K, value: @V, threadid: int => @V]"
	},
	{ DaoMT_MapFind,
		"find( invar amap: map<@K,@V>, threads = 2 )"
			"[invar key: @K, invar value: @V, threadid: int => int]"
			"=> tuple<key: @K, value: @V>|none"
	},

	{ DaoMT_ArrayIterate,
		"iterate( invar aarray: array<@T>, threads = 2 )"
			"[item: @T, I: int, J: int, K: int, L: int, M: int, threadid: int]"
	},
	{ DaoMT_ArrayMap,
		"map( invar aarray: array<@T>, threads = 2 )"
			"[item: @T, I: int, J: int, K: int, L: int, M: int, threadid: int => @T2]"
			"=> array<@T2>"
	},
	{ DaoMT_ArrayApply,
		"apply( aarray: array<@T>, threads = 2 )"
			"[item: @T, I: int, J: int, K: int, L: int, M: int, threadid: int => @T]"
	},
	{ NULL, NULL }
};


#endif /* DAO_WITH_CONCURRENT */

