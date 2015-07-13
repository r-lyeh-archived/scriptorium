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


#include<stdio.h>
#include<string.h>
#include<time.h>
#include<math.h>
#include<assert.h>
#include"daoType.h"
#include"daoValue.h"
#include"daoThread.h"
#include"daoRoutine.h"
#include"daoObject.h"
#include"daoVmspace.h"
#include"daoProcess.h"
#include"daoGC.h"
#include"daoTasklet.h"

#define MIN_TIME  1E-27

#ifdef DAO_WITH_CONCURRENT


DaoTaskEvent* DaoTaskEvent_New()
{
	DaoTaskEvent *self = (DaoTaskEvent*) dao_calloc( 1, sizeof(DaoTaskEvent) );
	return self;
}
void DaoTaskEvent_Reset( DaoTaskEvent *self )
{
	self->type = 0;
	self->state = 0;
	self->timeout = 0;
	self->auxiliary = 0;
	self->expiring = -1.0;
	GC_DecRC( self->future );
	GC_DecRC( self->channel );
	GC_DecRC( self->selected );
	GC_DecRC( self->message );
	GC_DecRC( self->selects );
	self->future = NULL;
	self->channel = NULL;
	self->selected = NULL;
	self->message = NULL;
	self->selects = NULL;
}
void DaoTaskEvent_Delete( DaoTaskEvent *self )
{
	DaoTaskEvent_Reset( self );
	dao_free( self );
}
void DaoTaskEvent_Init( DaoTaskEvent *self, int T, int S, DaoFuture *F, DaoChannel *C )
{
	GC_Assign( & self->future, F );
	GC_Assign( & self->channel, C );
	self->type = T;
	self->state = S;
}



DaoChannel* DaoChannel_New( DaoType *type, int dtype )
{
	DaoChannel *self = (DaoChannel*) dao_calloc( 1, sizeof(DaoChannel) );
	if( dtype ) type = DaoType_Specialize( dao_type_channel, & type, type != NULL );
	DaoCstruct_Init( (DaoCstruct*) self, type );
	self->buffer = DList_New( DAO_DATA_VALUE );
	return self;
}




static int DaoValue_CheckCtype( DaoValue *self, DaoType *type )
{
	if( self->type != DAO_CSTRUCT && self->type != DAO_CDATA ) return 0;
	if( self->type != type->tid ) return 0;
	return self->xCstruct.ctype->typer == type->typer;
}




typedef struct DaoCallThread   DaoCallThread;
typedef struct DaoCallServer   DaoCallServer;


struct DaoCallThread
{
	DThread       thread;
	DThreadData  *thdData;

	DThreadTask   taskFunc;  /* first task; */
	void         *taskParam;
};

static DaoCallThread* DaoCallThread_New( DThreadTask func, void *param );
static void DaoCallThread_Run( DaoCallThread *self );

struct DaoCallServer
{
	DThread  timer;
	DMutex   mutex;
	DCondVar condv;
	DCondVar condv2;

	volatile int finishing;
	volatile int timing;
	volatile int total;
	volatile int idle;
	volatile int stopped;

	DList  *threads;

	DList  *functions;  /* list of DThreadTask function pointers */
	DList  *parameters; /* list of void* */
	DList  *events;     /* list of DaoTaskEvent* */
	DList  *events2;    /* list of DaoTaskEvent* */
	DMap   *waitings;   /* timed waiting: <dao_complex,DaoTaskEvent*> */
	DMap   *active;     /* map of DaoObject* or DaoProcess* keys */
	DMap   *pending;    /* map of pointers from ::parameters, ::events and ::events2 */

	DList  *caches;

	dao_complex   timestamp;  /* (time,index); */
	DaoVmSpace   *vmspace;
};
static DaoCallServer *daoCallServer = NULL;

static DaoCallThread* DaoCallThread_New( DThreadTask func, void *param )
{
	DaoCallThread *self = (DaoCallThread*)dao_malloc( sizeof(DaoCallThread) );
	self->thdData = NULL;
	self->taskFunc = func;
	self->taskParam = param;
	DThread_Init( & self->thread );
	return self;
}
static void DaoCallThread_Delete( DaoCallThread *self )
{
	// XXX self->thdData
	DThread_Destroy( & self->thread );
	dao_free( self );
}
static DaoCallServer* DaoCallServer_New( DaoVmSpace *vms )
{
	DaoCallServer *self = (DaoCallServer*)dao_malloc( sizeof(DaoCallServer) );
	DMutex_Init( & self->mutex );
	DCondVar_Init( & self->condv );
	DCondVar_Init( & self->condv2 );
	DThread_Init( & self->timer );
	self->finishing = 0;
	self->timing = 0;
	self->total = 0;
	self->idle = 0;
	self->stopped = 0;
	self->threads = DList_New(0);
	self->functions = DList_New(0);
	self->parameters = DList_New(0);
	self->events = DList_New(0);
	self->events2 = DList_New(0);
	self->waitings = DMap_New( DAO_DATA_COMPLEX, 0 );
	self->pending = DHash_New(0,0);
	self->active = DHash_New(0,0);
	self->caches = DList_New(0);
	self->vmspace = vms;
	self->timestamp.real = 0.0;
	self->timestamp.imag = 0.0;
	return self;
}
static void DaoCallServer_Delete( DaoCallServer *self )
{
	daoint i;
	for(i=0; i<self->threads->size; i++){
		DaoCallThread_Delete( (DaoCallThread*)self->threads->items.pVoid[i] );
	}
	for(i=0; i<self->caches->size; ++i){
		DaoTaskEvent_Delete( (DaoTaskEvent*) self->caches->items.pVoid[i] );
	}
	DList_Delete( self->threads );
	DList_Delete( self->functions );
	DList_Delete( self->parameters );
	DList_Delete( self->events );
	DList_Delete( self->events2 );
	DList_Delete( self->caches );
	DMap_Delete( self->waitings );
	DMap_Delete( self->pending );
	DMap_Delete( self->active );
	DMutex_Destroy( & self->mutex );
	DCondVar_Destroy( & self->condv );
	DCondVar_Destroy( & self->condv2 );
	DThread_Destroy( & self->timer );
	dao_free( self );
}

static void DaoCallServer_Timer( void *p );

static void DaoCallServer_Init( DaoVmSpace *vms )
{
	DaoCGC_Start();
	daoCallServer = DaoCallServer_New( vms );
	/* Set it here, so that DaoCallServer_Stop() will not stop prematurally: */
	daoCallServer->timing = 1;
	if( DThread_Start( & daoCallServer->timer, (DThreadTask) DaoCallServer_Timer, NULL ) ==0 ){
		dao_abort( "failed to create the timer thread" );
	}
}
static DaoCallServer* DaoCallServer_TryInit( DaoVmSpace *vms )
{
	if( daoCallServer == NULL ) DaoCallServer_Init( vms );
	return daoCallServer;
}

static DaoTaskEvent* DaoCallServer_MakeEvent()
{
	DaoTaskEvent *event;
	DaoCallServer *server = daoCallServer;
	DMutex_Lock( & server->mutex );
	event = (DaoTaskEvent*) DList_PopBack( server->caches );
	if( event == NULL ) event = DaoTaskEvent_New();
	DMutex_Unlock( & server->mutex );
	return event;
}
/* Lock daoCallServer::mutex before calling this function: */
static void DaoCallServer_CacheEvent( DaoTaskEvent *event )
{
	DaoCallServer *server = daoCallServer;
	DaoTaskEvent_Reset( event );
	DList_PushBack( server->caches, event );
}
int DaoCallServer_GetThreadCount()
{
	DaoCallServer_TryInit( mainVmSpace );
	return daoCallServer->total;
}
void DaoCallServer_AddThread( DThreadTask func, void *param )
{
	DaoCallThread *calth;
	DaoCallServer_TryInit( mainVmSpace );
	calth = DaoCallThread_New( func, param );
	DMutex_Lock( & daoCallServer->mutex );
	daoCallServer->total += 1;
	DList_Append( daoCallServer->threads, calth );
	DMutex_Unlock( & daoCallServer->mutex );
	if( DThread_Start( & calth->thread, (DThreadTask) DaoCallThread_Run, calth ) == 0 ){
		if( func != NULL || daoCallServer->total == 0 ){
			dao_abort( "failed to create a task thread" );
		}
	}
}
static void DaoCallServer_TryAddThread( DThreadTask func, void *param, int todo )
{
	int max = daoConfig.cpu > 2 ? daoConfig.cpu : 2;
	int total = daoCallServer->total;
	if( total < 2 ) DaoCallServer_AddThread( NULL, NULL );
	if( todo == 0 ) return;
	if( total < max || todo > pow( total + max + 10, 5 ) ) DaoCallServer_AddThread( NULL, NULL );
}
static int DaoTaskEvent_CheckSelect( DaoTaskEvent *self )
{
	DNode *it;
	int closed = 0;
	int move = 0;
	for(it=DaoMap_First(self->selects); it; it=DaoMap_Next(self->selects,it)){
		if( DaoValue_CheckCtype( it->key.pValue, dao_type_channel ) ){
			DaoChannel *chan = (DaoChannel*) it->key.pValue;
			move = chan->buffer->size > 0;
			closed += chan->cap == 0;
		}else{
			DaoFuture *fut = (DaoFuture*) it->key.pValue;
			move = fut->state == DAO_CALL_FINISHED;
		}
		if( move ) break;
	}
	if( self->selects->value->size == closed ) move = 1;
	return move;
}
static void DaoCallServer_ActivateEvents()
{
	DaoCallServer *server = daoCallServer;
	char message[128];
	daoint i, j, count = 0;

	if( server->idle != server->total ) return;
	if( server->events->size != 0 ) return;
	if( server->events2->size == 0 ) return;

#ifdef DEBUG
	sprintf( message, "WARNING: try activating events (%i,%i,%i,%i)!\n", server->total,
			server->idle, (int)server->events->size, (int)server->events2->size );
	DaoStream_WriteChars( mainVmSpace->errorStream, message );
#endif
	for(i=0; i<server->events2->size; ++i){
		DaoTaskEvent *event = (DaoTaskEvent*) server->events2->items.pVoid[i];
		DaoChannel *chan = event->channel;
		DaoFuture *fut = event->future;
		int move = 0, closed = 0;
		switch( event->type ){
		case DAO_EVENT_WAIT_TASKLET :
			move = fut->precond == NULL || fut->precond->state == DAO_CALL_FINISHED;
			break;
		case DAO_EVENT_WAIT_RECEIVING :
			move = chan->buffer->size > 0;
			if( chan->cap <= 0 && chan->buffer->size == 0 ) move = 1;
			break;
		case DAO_EVENT_WAIT_SENDING :
			move = chan->buffer->size < chan->cap;
			break;
		case DAO_EVENT_WAIT_SELECT :
			if( event->selects == NULL ) continue;
			move = DaoTaskEvent_CheckSelect( event );
			break;
		default: break;
		}
		if( move ){
			DList_Append( server->events, event );
			DList_Erase( server->events2, i, 1 );
			count += 1;
			i -= 1;
		}
	}
	DCondVar_Signal( & server->condv );
	if( count == 0 ){
		DaoStream *stream = mainVmSpace->errorStream;
		DaoStream_WriteChars( stream, "ERROR: All tasklets are suspended - deadlock!\n" );
		exit(1);
	}
}
static void DaoCallServer_Timer( void *p )
{
	DaoCallServer *server = daoCallServer;
	double time = 0.0;
	daoint i, timeout;

	while( server->finishing == 0 || server->stopped != server->total ){
		DMutex_Lock( & server->mutex );
		while( server->waitings->size == 0 ){
			if( server->idle == server->total && server->events2->size ){
				DaoCallServer_ActivateEvents();
			}
			if( server->finishing && server->stopped == server->total ) break;
			DCondVar_TimedWait( & server->condv2, & server->mutex, 0.01 );
		}
		if( server->waitings->size ){
			DNode *node = DMap_First( server->waitings );
			time = node->key.pComplex->real;
			time -= Dao_GetCurrentTime();
			/* wait the right amount of time for the closest arriving timeout: */
			if( time > 0 ) DCondVar_TimedWait( & server->condv2, & server->mutex, time );
		}
		DMutex_Unlock( & server->mutex );
		if( server->finishing && server->stopped == server->total ) break;

		DMutex_Lock( & server->mutex );
		if( server->waitings->size ){ /* a new wait timed out: */
			DNode *node = DMap_First( server->waitings );
			time = Dao_GetCurrentTime();
			if( node->key.pComplex->real < time ){
				DaoTaskEvent *event = (DaoTaskEvent*) node->value.pVoid;
				event->state = DAO_EVENT_RESUME;
				event->timeout = 1;
				event->expiring = MIN_TIME;
				DList_Append( server->events, node->value.pVoid );
				DMap_EraseNode( server->waitings, node );
			}
		}
		DCondVar_Signal( & server->condv );
		DMutex_Unlock( & server->mutex );
	}
	server->timing = 0;
}

void DaoCallServer_AddTask( DThreadTask func, void *param, int now )
{
	int scheduled = 0;
	DaoCallServer *server = DaoCallServer_TryInit( mainVmSpace );
	DMutex_Lock( & server->mutex );
	if( server->idle > server->parameters->size || now == 0 ){
		scheduled = 1;
		DList_Append( server->functions, func );
		DList_Append( server->parameters, param );
		DMap_Insert( server->pending, param, NULL );
		DCondVar_Signal( & server->condv );
	}
	DMutex_Unlock( & server->mutex );
	if( scheduled ){
		if( now == 0 ) DaoCallServer_TryAddThread( NULL, NULL, server->parameters->size );
	}else{
		DaoCallServer_AddThread( func, param );
	}
}
static void DaoCallServer_AddEvent( DaoTaskEvent *event )
{
	DaoCallServer *server = daoCallServer;
	DList_Append( server->events, event );
	DMap_Insert( server->pending, event, NULL );
}
static void DaoCallServer_Add( DaoTaskEvent *event )
{
	DaoCallServer *server = daoCallServer;
	DMutex_Lock( & server->mutex );
	DaoCallServer_AddEvent( event );
	DCondVar_Signal( & server->condv );
	DMutex_Unlock( & server->mutex );
	DaoCallServer_TryAddThread( NULL, NULL, server->pending->size );
}
#endif

void DaoProcess_ReturnFutureValue( DaoProcess *self, DaoFuture *future )
{
	DaoType *type;
	if( future == NULL ) return;
	type = future->ctype;
	type = type && type->nested->size ? type->nested->items.pType[0] : NULL;
	switch( self->status ){
	case DAO_PROCESS_ABORTED :
		future->state = DAO_CALL_ABORTED;
		break;
	case DAO_PROCESS_FINISHED :
		DaoValue_Move( self->stackValues[0], & future->value, type );
		future->state = DAO_CALL_FINISHED;
		break;
	case DAO_PROCESS_SUSPENDED : future->state = DAO_CALL_PAUSED; break;
	case DAO_PROCESS_RUNNING :
	case DAO_PROCESS_STACKED : future->state = DAO_CALL_RUNNING; break;
	default : break;
	}
}

void DaoCallServer_AddCall( DaoProcess *caller )
{
	DaoFuture *future;
	DaoTaskEvent *event;
	DaoProcess *callee = DaoVmSpace_AcquireProcess( caller->vmSpace );
	DaoStackFrame *frame = caller->topFrame;
	DaoRoutine *routine = frame->routine;
	DaoType *type = (DaoType*) routine->routType->aux;
	DaoValue **params = caller->stackValues + caller->topFrame->stackBase;
	int i, count = caller->topFrame->parCount;

	if( caller->activeCode->b & DAO_CALL_BLOCK ){
		DaoValue **calleeValues, **callerValues = caller->activeValues;
		DaoStackFrame *sectFrame = DaoProcess_FindSectionFrame( caller );
		DaoStackFrame *callerFrame = caller->topFrame->prev;
		DaoVmCode *vmc, *end, *sect;
		if( sectFrame != callerFrame ){
			DaoVmSpace_ReleaseProcess( caller->vmSpace, callee );
			DaoProcess_RaiseError( caller, NULL, "Invalid code section" );
			return;
		}
		if( routine->body ){
			DaoProcess_PushRoutine( callee, callerFrame->routine, callerFrame->object );
			callerValues = caller->stackValues + callerFrame->stackBase;
		}else{
			DaoProcess_PushRoutine( callee, caller->activeRoutine, caller->activeObject );
		}
		DaoProcess_SetActiveFrame( callee, callee->topFrame );
		calleeValues = callee->stackValues + callee->topFrame->stackBase;
		callee->activeCode = caller->activeCode;
		vmc = callerFrame->routine->body->vmCodes->data.codes + callerFrame->entry;
		end = callerFrame->routine->body->vmCodes->data.codes + vmc->b;
		sect = vmc + 1;
		for(vmc=sect; vmc!=end; vmc++){
			int i = -1, code = vmc->code;
			if( code == DVM_GETVH || (code >= DVM_GETVH_I && code <= DVM_GETVH_C) ){
				i = vmc->b;
			}else if( code == DVM_SETVH || (code >= DVM_SETVH_II && code <= DVM_SETVH_CC) ){
				i = vmc->b;
			}
			if( i >= 0 ) DaoValue_Move( callerValues[i], & calleeValues[i], NULL );
		}
	}

	future = DaoFuture_New( type, 1 );
	future->state = DAO_CALL_PAUSED;
	future->actor = caller->topFrame->object;
	GC_IncRC( future->actor );

	GC_Assign( & future->process, callee );
 	GC_Assign( & callee->future, future );

	callee->parCount = count;
	/* Use routine->parCount instead of caller->topFrame->parCount, for default parameters: */
	for(i=0; i<routine->parCount; ++i) DaoValue_Copy( params[i], & callee->paramValues[i] );
	if( routine->body ){
		DaoProcess_PushRoutine( callee, routine, future->actor );
	}else{
		DaoProcess_PushFunction( callee, routine );
		callee->activeNamespace = caller->activeNamespace;
	}
	if( caller->activeCode->b & DAO_CALL_BLOCK ){
		callee->topFrame->host = callee->topFrame;
		callee->topFrame->returning = -1;
	}

#ifdef DAO_WITH_CONCURRENT
	DaoCallServer_TryInit( mainVmSpace );
	event = DaoCallServer_MakeEvent();
	DaoTaskEvent_Init( event, DAO_EVENT_RESUME_TASKLET, DAO_EVENT_RESUME, future, NULL );

	DaoProcess_PopFrame( caller );
	DaoProcess_PutValue( caller, (DaoValue*) future );

	DaoCallServer_Add( event );
#else
	DaoProcess_PopFrame( caller );
	DaoProcess_PutValue( caller, (DaoValue*) future );
	DaoProcess_InterceptReturnValue( callee );
	DaoProcess_Execute( callee );
	DaoProcess_ReturnFutureValue( callee, future );
	DaoVmSpace_ReleaseProcess( caller->vmSpace, callee );
#endif
}

#ifdef DAO_WITH_CONCURRENT
DaoFuture* DaoProcess_GetInitFuture( DaoProcess *self )
{
	DaoFuture *future;
	if( self->future ) return self->future;

	future = DaoFuture_New( NULL, 1 );
	GC_Assign( & self->future, future );
	GC_Assign( & future->process, self );
	return future;
}
void DaoCallServer_MarkActiveProcess( DaoProcess *process, int active )
{
	DaoCallServer *server = daoCallServer;

	if( daoCallServer == NULL ) return;
	if( process->active == active ) return;

	DMutex_Lock( & server->mutex );
	if( active ){
		DMap_Insert( server->active, process, NULL );
		process->active = 1;
	}else{
		DMap_Erase( server->active, process );
		process->active = 0;
	}
	DMutex_Unlock( & server->mutex );
}
void DaoCallServer_AddTimedWait( DaoProcess *wait, DaoTaskEvent *event, double timeout )
{
	DaoCallServer *server = daoCallServer;

	/*
	// The "wait" process may not be running in the thread pool,
	// so it may have not been added to active process list.
	// It is necessary to add it to the active list now,
	// to avoid it being activated immediately after it is blocked.
	// Activating it immediately may cause a race condition,
	// because it may have not been blocked completely
	// (namely, it may be still running).
	*/
	DaoCallServer_MarkActiveProcess( wait, 1 );

	DMutex_Lock( & server->mutex );
	if( timeout >= 1E-27 ){
		server->timestamp.real = timeout + Dao_GetCurrentTime();
		server->timestamp.imag += 1;
		event->expiring = server->timestamp.real;
		DMap_Insert( server->waitings, & server->timestamp, event );
		DMap_Insert( server->pending, event, NULL );
		DCondVar_Signal( & server->condv2 );
	}else{
		event->expiring = -1.0;
		DaoCallServer_AddEvent( event );
		DCondVar_Signal( & server->condv );
	}
	DMutex_Unlock( & server->mutex );
}
void DaoCallServer_AddWait( DaoProcess *wait, DaoFuture *pre, double timeout )
{
	DaoTaskEvent *event;
	DaoCallServer *server = DaoCallServer_TryInit( mainVmSpace );;
	DaoFuture *future = DaoProcess_GetInitFuture( wait );

	GC_Assign( & future->precond, pre );
	future->state = DAO_CALL_PAUSED;

	event = DaoCallServer_MakeEvent();
	DaoTaskEvent_Init( event, DAO_EVENT_WAIT_TASKLET, DAO_EVENT_WAIT, future, NULL );

	DaoCallServer_AddTimedWait( wait, event, timeout );
}
static int DaoCallServer_CheckEvent( DaoTaskEvent *event, DaoFuture *fut, DaoChannel *chan )
{
	DaoTaskEvent event2 = *event;
	daoint i, move = 0, closed = 0;
	switch( event->type ){
	case DAO_EVENT_WAIT_TASKLET :
		move = event->future->precond == fut;
		break;
	case DAO_EVENT_WAIT_RECEIVING :
		if( event->channel == chan ){
			move = chan->buffer->size > 0;
			move |= chan->cap <= 0 && chan->buffer->size == 0;
		}
		break;
	case DAO_EVENT_WAIT_SENDING :
		move = event->channel == chan && chan->buffer->size < chan->cap;
		break;
	case DAO_EVENT_WAIT_SELECT :
		if( event->selects == NULL ) break;
		if( fut  ) move |= DMap_Find( event->selects->value, fut ) != NULL;
		if( chan ) move |= DMap_Find( event->selects->value, chan ) != NULL;
		//move = DaoTaskEvent_CheckSelect( event );
		break;
	default: break;
	}
	return move;
}
/*
// Only activate one event per channel:
*/
void DaoChannel_ActivateEvent( DaoChannel *self, int type )
{
	DaoCallServer *server = daoCallServer;
	DNode *node;
	daoint i;

	for(i=0; i<server->events2->size; ++i){
		DaoTaskEvent *event = (DaoTaskEvent*) server->events2->items.pVoid[i];
		if( event->type != type ) continue;
		if( DaoCallServer_CheckEvent( event, NULL, self ) ){
			DList_Append( server->events, event );
			DList_Erase( server->events2, i, 1 );
			return;
		}
	}
	for(node=DMap_First(server->waitings); node; node=DMap_Next(server->waitings,node)){
		DaoTaskEvent *event = (DaoTaskEvent*) node->value.pValue;
		if( event->type != type ) continue;
		if( DaoCallServer_CheckEvent( event, NULL, self ) ){
			DList_Append( server->events, event );
			DMap_EraseNode( server->waitings, node );
			return;
		}
	}
}
/*
// Activate all events waiting on a future value:
*/
void DaoFuture_ActivateEvent( DaoFuture *self )
{
	DaoCallServer *server = daoCallServer;
	DList *array = DList_New(0);
	DNode *node;
	daoint i;

	DMutex_Lock( & server->mutex );
	for(i=0; i<server->events2->size; ++i){
		DaoTaskEvent *event = (DaoTaskEvent*) server->events2->items.pVoid[i];
		if( DaoCallServer_CheckEvent( event, self, NULL ) ){
			event->state = DAO_EVENT_RESUME;
			DList_Append( server->events, event );
			DList_Erase( server->events2, i, 1 );
			i -= 1;
		}
	}
	for(node=DMap_First(server->waitings); node; node=DMap_Next(server->waitings,node)){
		DaoTaskEvent *event = (DaoTaskEvent*) node->value.pValue;
		/* remove from timed waiting list: */
		if( DaoCallServer_CheckEvent( event, self, NULL ) ){
			event->state = DAO_EVENT_RESUME;
			DList_Append( server->events, event );
			DList_Append( array, node->key.pVoid );
		}
	}
	for(i=0; i<array->size; i++) DMap_Erase( server->waitings, array->items.pVoid[i] );
	DCondVar_Signal( & server->condv );
	DMutex_Unlock( & server->mutex );
	DList_Delete( array );
}
static DaoFuture* DaoCallServer_GetNextFuture()
{
	DaoCallServer *server = daoCallServer;
	DaoFuture *first, *future, *precond;
	DList *events = server->events;
	DMap *pending = server->pending;
	DMap *active = server->active;
	DNode *it;
	daoint i, j;

	for(i=0; i<events->size; i++){
		DaoTaskEvent *event = (DaoTaskEvent*) events->items.pVoid[i];
		DaoFuture *future = event->future;
		DaoObject *actor = future->actor;
		DaoChannel *channel = event->channel;
		DaoChannel *closed = NULL;
		DaoChannel *chselect = NULL;
		DaoFuture *futselect = NULL;
		DaoValue *selected = NULL;
		DaoValue *message = NULL;
		int type = event->type;

		if( event->state == DAO_EVENT_WAIT && future->precond != NULL ){
			if( future->precond->state != DAO_CALL_FINISHED ) goto MoveToWaiting;
		}
		switch( event->type ){
		case DAO_EVENT_WAIT_SENDING :
			if( channel->buffer->size >= channel->cap ){
				if( event->state == DAO_EVENT_WAIT ){
					DaoChannel_ActivateEvent( channel, DAO_EVENT_WAIT_RECEIVING );
					DaoChannel_ActivateEvent( channel, DAO_EVENT_WAIT_SELECT );
					goto MoveToWaiting;
				}
			}
			event->type = DAO_EVENT_RESUME_TASKLET;
			break;
		case DAO_EVENT_WAIT_RECEIVING :
			if( channel->buffer->size == 0 ){
				if( channel->cap > 0 && event->state == DAO_EVENT_WAIT ){
					DaoChannel_ActivateEvent( channel, DAO_EVENT_WAIT_SENDING );
					goto MoveToWaiting;
				}
				message = dao_none_value;
			}else{
				message = channel->buffer->items.pValue[0];
			}
			GC_Assign( & event->message, message );
			event->auxiliary = channel->cap <= 0 && channel->buffer->size == 0;
			event->type = DAO_EVENT_RESUME_TASKLET;
			DList_PopFront( channel->buffer );
			if( channel->buffer->size < channel->cap )
				DaoChannel_ActivateEvent( channel, DAO_EVENT_WAIT_SENDING );
			if( channel->buffer->size )
				DaoChannel_ActivateEvent( channel, DAO_EVENT_WAIT_RECEIVING );
			break;
		case DAO_EVENT_WAIT_SELECT :
			message = dao_none_value;
			for(it=DaoMap_First(event->selects); it; it=DaoMap_Next(event->selects,it)){
				if( DaoValue_CheckCtype( it->key.pValue, dao_type_channel ) ){
					DaoChannel *chan = (DaoChannel*) it->key.pValue;
					if( chan->buffer->size > 0 ){
						chselect = chan;
						selected = it->key.pValue;
						message = chan->buffer->items.pValue[0];
						closed = NULL;
						break;
					}else if( chan->cap == 0 ){
						closed = chan;
					}
				}else{
					DaoFuture *fut = (DaoFuture*) it->key.pValue;
					if( fut->state == DAO_CALL_FINISHED ){
						futselect = fut;
						selected = it->key.pValue;
						message = fut->value;
						break;
					}
				}
			}
			if( selected == NULL ) selected = (DaoValue*) closed;
			if( event->state == DAO_EVENT_WAIT && event->selects->value->size ){
				if( selected == NULL ) goto MoveToWaiting;
			}

			GC_Assign( & event->message, message );
			GC_Assign( & event->selected, selected );
			event->auxiliary = event->selects->value->size == 0;
			event->type = DAO_EVENT_RESUME_TASKLET;
			/* change status to not finished: */
			if( chselect != NULL || futselect != NULL ) event->auxiliary = 0;
			if( chselect ){
				DList_PopFront( chselect->buffer );
				if( chselect->buffer->size < chselect->cap )
					DaoChannel_ActivateEvent( chselect, DAO_EVENT_WAIT_SENDING );
				if( chselect->buffer->size )
					DaoChannel_ActivateEvent( chselect, DAO_EVENT_WAIT_SELECT );
			}
			if( futselect != NULL || closed != NULL ){
				void *key = futselect ? (void*)futselect : (void*)closed;
				DMap_Erase( event->selects->value, key );
			}
			break;
		default: break;
		}
		if( actor ){
			DNode *it = DMap_Find( active, actor->rootObject );
			if( actor->rootObject->isAsync ){
				if( it && it->value.pVoid != (void*) future ) continue;
			}else if( it ){
				continue;
			}
		}
		if( future->process && DMap_Find( active, future->process ) ) continue;
		DList_Erase( events, i, 1 );
		DMap_Erase( pending, event );
		if( actor ){
			void *value = actor->rootObject->isAsync ? future : NULL;
			DMap_Insert( active, actor->rootObject, value );
		}
		if( future->process ){
			DMap_Insert( active, future->process, NULL );
			future->process->active = 1;
		}

		/*
		// DaoValue_Move() should be used instead of GC_Assign() for thread safety.
		// Because using GC_Assign() here, may caused "future->message" of primitive
		// type being deleted, right after DaoFuture_GetGCFields() has retrieved it
		// for GC scanning.
		 */
		DaoValue_Move( event->message, & future->message, NULL );
		DaoValue_Move( event->selected, & future->selected, NULL );
		future->aux1 = event->auxiliary;
		future->timeout = event->timeout;

		GC_IncRC( future ); /* To be decreased at the end of tasklet; */
		DaoCallServer_CacheEvent( event );
		return future;
MoveToWaiting:
		if( event->expiring >= 0.0 && event->expiring < MIN_TIME ) continue;
		if( event->expiring >= MIN_TIME ){
			dao_complex com = {0.0,0.0};
			com.real = event->expiring;
			DMap_Insert( server->waitings, & com, event );
			DCondVar_Signal( & server->condv2 );
		}else{
			DList_Append( server->events2, event );
		}
		DList_Erase( server->events, i, 1 );
		i -= 1;
	}
	return NULL;
}
static void DaoCallThread_Run( DaoCallThread *self )
{
	DaoCallServer *server = daoCallServer;
	double wt = 0.001;
	daoint i, count, timeout;

	self->thdData = DThread_GetSpecific();
	if( self->taskFunc ) self->taskFunc( self->taskParam );
	while( server->vmspace->stopit == 0 ){
		DaoProcess *process = NULL;
		DaoFuture *future = NULL;
		DThreadTask function = NULL;
		void *parameter = NULL;

		if( self->thdData != NULL ) self->thdData->state = 0;
		DMutex_Lock( & server->mutex );
		server->idle += 1;
		while( server->pending->size == (server->events2->size + server->waitings->size) ){
			//printf( "%p %i %i %i %i\n", self, server->events->size, server->pending->size, server->events2->size, server->waitings->size );
			if( server->vmspace->stopit ) break;
			if( server->finishing && server->idle == server->total ){
				if( (server->events2->size + server->waitings->size) == 0 ) break;
			}
			wt = 0.01*(server->idle == server->total) + 0.001;
			timeout = DCondVar_TimedWait( & server->condv, & server->mutex, wt );
		}
		for(i=0; i<server->parameters->size; ++i){
			void *param = server->parameters->items.pVoid[i];
			if( DMap_Find( server->active, param ) ) continue;
			DMap_Insert( server->active, param, NULL );
			function = (DThreadTask) server->functions->items.pVoid[i];
			parameter = param;
			DList_Erase( server->functions, i, 1 );
			DList_Erase( server->parameters, i, 1 );
			DMap_Erase( server->pending, parameter );
			server->idle -= 1;
			break;
		}
		DMutex_Unlock( & server->mutex );

		if( server->vmspace->stopit ) break;
		if( function ){
			(*function)( parameter );
			DMutex_Lock( & server->mutex );
			DMap_Erase( server->active, parameter );
			DMutex_Unlock( & server->mutex );
			continue;
		}

		if( server->pending->size == 0 && server->finishing && server->idle == server->total ) break;

		DMutex_Lock( & server->mutex );
		server->idle -= 1;
		future = DaoCallServer_GetNextFuture();
		DMutex_Unlock( & server->mutex );

		if( future == NULL ) continue;

		process = future->process;
		if( process == NULL ){
			GC_DecRC( future );
			continue;
		}

		count = process->exceptions->size;
		future->state = DAO_CALL_RUNNING;
		DaoProcess_InterceptReturnValue( process );
		DaoProcess_Start( process );
		if( process->exceptions->size > count ) DaoProcess_PrintException( process, NULL, 1 );

		if( future->actor ){
			int erase = 1;
			DMutex_Lock( & server->mutex );
			if( future->actor->rootObject->isAsync ){
				erase = process->status == DAO_PROCESS_FINISHED;
			}
			if( erase ) DMap_Erase( server->active, future->actor->rootObject );
			DMutex_Unlock( & server->mutex );
		}
		DMutex_Lock( & server->mutex );
		DMap_Erase( server->active, process );
		process->active = 0;
		DMutex_Unlock( & server->mutex );

		DaoProcess_ReturnFutureValue( process, future );
		if( future->state == DAO_CALL_FINISHED ) DaoFuture_ActivateEvent( future );
		GC_DecRC( future );
	}
	DMutex_Lock( & server->mutex );
	server->stopped += 1;
	DMutex_Unlock( & server->mutex );
}
void DaoCallServer_Join()
{
	DCondVar condv;
	if( daoCallServer == NULL ) return;
	DCondVar_Init( & condv );
	DMutex_Lock( & daoCallServer->mutex );
	while( daoCallServer->pending->size || daoCallServer->idle != daoCallServer->total ){
		DCondVar_TimedWait( & condv, & daoCallServer->mutex, 0.01 );
	}
	DMutex_Unlock( & daoCallServer->mutex );
	DCondVar_Destroy( & condv );
}
void DaoCallServer_Stop()
{
	DCondVar condv;
	DaoCallThread *calth;
	if( daoCallServer == NULL ) return;
	DCondVar_Init( & condv );
	daoCallServer->finishing = 1;

	calth = DaoCallThread_New( NULL, NULL );
	DMutex_Lock( & daoCallServer->mutex );
	daoCallServer->total += 1;
	DMutex_Unlock( & daoCallServer->mutex );

	DaoCallThread_Run( calth );  /* process tasks in the main thread; */

	DMutex_Lock( & daoCallServer->mutex );
	while( daoCallServer->stopped != daoCallServer->total || daoCallServer->timing ){
		DCondVar_TimedWait( & condv, & daoCallServer->mutex, 0.01 );
	}
	DMutex_Unlock( & daoCallServer->mutex );

	DCondVar_Destroy( & condv );
	DaoCallThread_Delete( calth );
	DaoCallServer_Delete( daoCallServer );
	daoCallServer = NULL;
}






static int DaoType_CheckPrimitiveType( DaoType *self )
{
	daoint i;

	if( self == NULL || self->tid == DAO_NONE ) return 0;
	if( self->tid <= DAO_ARRAY ) return 1;
	if( self->tid > DAO_TUPLE && self->tid != DAO_VARIANT ) return 0;

	if( self->tid != DAO_TUPLE && (self->nested == NULL || self->nested->size == 0) ) return 0;
	for(i=0; i<self->nested->size; ++i){
		DaoType *type = self->nested->items.pType[i];
		if( type == NULL ) return 0;
		if( type->tid == DAO_PAR_NAMED ) type = (DaoType*) type->aux;
		if( DaoType_CheckPrimitiveType( type ) == 0 ) return 0;
	}
	return 1;
}
static DaoValue* DaoValue_DeepCopy( DaoValue *self )
{
	DNode *it;
	daoint i;
	if( self == NULL ) return NULL;
	if( self->type <= DAO_ENUM ) return self; /* simple types will be copied at use; */
	if( self->type == DAO_ARRAY ) return (DaoValue*) DaoArray_Copy( (DaoArray*) self );
	if( self->type == DAO_LIST ){
		DaoList *list = (DaoList*) self;
		DaoList *copy = DaoList_New();
		GC_Assign( & copy->ctype, list->ctype );
		for(i=0; i<list->value->size; ++i){
			DaoValue *value = DaoValue_DeepCopy( list->value->items.pValue[i] );
			DaoList_Append( copy, value );
		}
		return (DaoValue*) copy;
	}else if( self->type == DAO_MAP ){
		DaoMap *map = (DaoMap*) self;
		DaoMap *copy = DaoMap_New( map->value->hashing );
		GC_Assign( & copy->ctype, map->ctype );
		for(it=DMap_First(map->value); it; it=DMap_Next(map->value,it)){
			DaoValue *key = DaoValue_DeepCopy( it->key.pValue );
			DaoValue *value = DaoValue_DeepCopy( it->value.pValue );
			DaoMap_Insert( copy, key, value );
		}
		return (DaoValue*) copy;
	}else if( self->type == DAO_TUPLE ){
		DaoTuple *tuple = (DaoTuple*) self;
		DaoTuple *copy = DaoTuple_New( tuple->size );
		GC_Assign( & copy->ctype, tuple->ctype );
		for(i=0; i<tuple->size; ++i){
			DaoValue *value = DaoValue_DeepCopy( tuple->values[i] );
			DaoTuple_SetItem( copy, value, i );
		}
		return (DaoValue*) copy;
	}
	return NULL;
}


static void CHANNEL_SetCap( DaoChannel *self, DaoValue *value, DaoProcess *proc )
{
	self->cap = value->xInteger.value;
	if( self->cap > 0 ) return;
	self->cap = 1;
	DaoProcess_RaiseError( proc, "Param", "channel capacity must be greater than 0" );
}
static void CHANNEL_New( DaoProcess *proc, DaoValue *par[], int N )
{
	DaoType *retype = DaoProcess_GetReturnType( proc );
	DaoChannel *self = DaoChannel_New( retype, 0 );
	CHANNEL_SetCap( self, par[0], proc );
	if( DaoType_CheckPrimitiveType( retype->nested->items.pType[0] ) == 0 ){
		DString *s = DString_New();
		DString_AppendChars( s, "data type " );
		DString_Append( s, retype->nested->items.pType[0]->name );
		DString_AppendChars( s, " is not supported for channel" );
		DaoProcess_RaiseError( proc, NULL, s->chars );
		DString_Delete( s );
	}
	DaoProcess_PutValue( proc, (DaoValue*) self );
	DaoCallServer_TryInit( mainVmSpace );
}
static void CHANNEL_Buffer( DaoProcess *proc, DaoValue *par[], int N )
{
	DaoChannel *self = (DaoChannel*) par[0];
	DaoProcess_PutInteger( proc, self->buffer->size );
}
static void CHANNEL_Cap( DaoProcess *proc, DaoValue *par[], int N )
{
	DaoChannel *self = (DaoChannel*) par[0];
	daoint i;

	DaoProcess_PutInteger( proc, self->cap );
	if( N == 1 ) return;

	/* Closing the channel: */
	DMutex_Lock( & daoCallServer->mutex );
	self->cap = par[1]->xInteger.value;
	if( self->cap == 0 ){
		DaoChannel_ActivateEvent( self, DAO_EVENT_WAIT_RECEIVING );
		DaoChannel_ActivateEvent( self, DAO_EVENT_WAIT_SELECT );
		DCondVar_Signal( & daoCallServer->condv );
	}
	DMutex_Unlock( & daoCallServer->mutex );
}
void DaoChannel_Send( DaoChannel *self, DaoValue *data )
{
	DMutex_Lock( & daoCallServer->mutex );
	DList_Append( self->buffer, data );
	DaoChannel_ActivateEvent( self, DAO_EVENT_WAIT_RECEIVING );
	DaoChannel_ActivateEvent( self, DAO_EVENT_WAIT_SELECT );
	DCondVar_Signal( & daoCallServer->condv );
	DMutex_Unlock( & daoCallServer->mutex );
}
static void CHANNEL_Send( DaoProcess *proc, DaoValue *par[], int N )
{
	DaoValue *data;
	DaoFuture *future = DaoProcess_GetInitFuture( proc );
	DaoChannel *self = (DaoChannel*) par[0];
	float timeout = par[2]->xFloat.value;

	DaoProcess_PutBoolean( proc, 1 );
	if( self->cap <= 0 ){
		DaoProcess_RaiseError( proc, "Param", "channel is closed" );
		return;
	}

	data = DaoValue_DeepCopy( par[1] );
	if( data == NULL ){
		DaoProcess_RaiseError( proc, "Param", "invalid data for the channel" );
		return;
	}

	//printf( "CHANNEL_Send: %p\n", event );
	DaoChannel_Send( self, data );

	if( self->buffer->size >= self->cap ){
		DaoTaskEvent *event = DaoCallServer_MakeEvent();
		DaoTaskEvent_Init( event, DAO_EVENT_WAIT_SENDING, DAO_EVENT_WAIT, future, self );
		proc->status = DAO_PROCESS_SUSPENDED;
		proc->pauseType = DAO_PAUSE_CHANNEL_SEND;
		DaoCallServer_AddTimedWait( proc, event, timeout );
	}
}
static void CHANNEL_Receive( DaoProcess *proc, DaoValue *par[], int N )
{
	DaoTaskEvent *event = NULL;
	DaoFuture *future = DaoProcess_GetInitFuture( proc );
	DaoChannel *self = (DaoChannel*) par[0];
	float timeout = par[1]->xFloat.value;

	event = DaoCallServer_MakeEvent();
	DaoTaskEvent_Init( event, DAO_EVENT_WAIT_RECEIVING, DAO_EVENT_WAIT, future, self );
	proc->status = DAO_PROCESS_SUSPENDED;
	proc->pauseType = DAO_PAUSE_CHANNEL_RECEIVE;
	DaoCallServer_AddTimedWait( proc, event, timeout );

	/* Message may have been sent before this call: */
	if( self->buffer->size ){
		DMutex_Lock( & daoCallServer->mutex );
		DaoChannel_ActivateEvent( self, DAO_EVENT_WAIT_RECEIVING );
		DCondVar_Signal( & daoCallServer->condv );
		DMutex_Unlock( & daoCallServer->mutex );
	}
}

static DaoFuncItem channelMeths[] =
{
	{ CHANNEL_New,      "Channel<@V>( cap = 1 )" },
	{ CHANNEL_Buffer,   "buffer( self: Channel<@V> ) => int" },
	{ CHANNEL_Cap,      "cap( self: Channel<@V> ) => int" },
	{ CHANNEL_Cap,      "cap( self: Channel<@V>, cap: int ) => int" },
	{ CHANNEL_Send,     "send( self: Channel<@V>, data: @V, timeout: float = -1 ) => bool" },
	{ CHANNEL_Receive,  "receive( self: Channel<@V>, timeout: float = -1 ) => tuple<data: @V|none, status: enum<received,timeout,finished>>" },
	{ NULL, NULL }
};
static void DaoChannel_Delete( DaoChannel *self )
{
	DaoCstruct_Free( (DaoCstruct*) self );
	DList_Delete( self->buffer );
	dao_free( self );
}
static void DaoChannel_GetGCFields( void *p, DList *vs, DList *lists, DList *ms, int rm )
{
	DaoChannel *self = (DaoChannel*) p;
	DList_Append( lists, self->buffer );
}

DaoTypeBase channelTyper =
{
	"Channel<@V>", NULL, NULL, (DaoFuncItem*) channelMeths, {0}, {0},
	(FuncPtrDel) DaoChannel_Delete, DaoChannel_GetGCFields
};





void DaoMT_Select( DaoProcess *proc, DaoValue *par[], int n )
{
	DNode *it;
	DaoTaskEvent *event = NULL;
	DaoFuture *future = DaoProcess_GetInitFuture( proc );
	DaoMap *selects = (DaoMap*) par[0];
	float timeout = par[1]->xFloat.value;

	for(it=DaoMap_First(selects); it; it=DaoMap_Next(selects,it)){
		DaoValue *value = it->key.pValue;
		int isfut = DaoValue_CheckCtype( value, dao_type_future );
		int ischan = DaoValue_CheckCtype( value, dao_type_channel );
		if( isfut == 0 && ischan == 0 ){
			DaoProcess_RaiseError( proc, "Param", "invalid type selection" );
			return;
		}
	}

	event = DaoCallServer_MakeEvent();
	DaoTaskEvent_Init( event, DAO_EVENT_WAIT_SELECT, DAO_EVENT_WAIT, future, NULL );
	GC_Assign( & event->selects, selects );
	proc->status = DAO_PROCESS_SUSPENDED;
	proc->pauseType = DAO_PAUSE_CHANFUT_SELECT;
	DaoCallServer_AddTimedWait( proc, event, timeout );

	/* Message may have been sent before this call: */
	DMutex_Lock( & daoCallServer->mutex );
	DaoChannel_ActivateEvent( NULL, DAO_EVENT_WAIT_SELECT );
	DCondVar_Signal( & daoCallServer->condv );
	DMutex_Unlock( & daoCallServer->mutex );
}

#endif





DaoFuture* DaoFuture_New( DaoType *type, int vatype )
{
	DaoFuture *self = (DaoFuture*) dao_calloc( 1, sizeof(DaoFuture) );
	if( vatype ) type = DaoType_Specialize( dao_type_future, & type, type != NULL );
	DaoCstruct_Init( (DaoCstruct*) self, type );
	GC_IncRC( dao_none_value );
	self->state = DAO_CALL_PAUSED;
	self->value = dao_none_value;
	return self;
}
static void DaoFuture_Delete( DaoFuture *self )
{
	DaoCstruct_Free( (DaoCstruct*) self );
	GC_DecRC( self->value );
	GC_DecRC( self->actor );
	GC_DecRC( self->message );
	GC_DecRC( self->selected );
	GC_DecRC( self->process );
	GC_DecRC( self->precond );
	dao_free( self );
}


static void FUTURE_Value( DaoProcess *proc, DaoValue *par[], int N )
{
	DaoFuture *self = (DaoFuture*) par[0];
	if( self->state == DAO_CALL_FINISHED ){
		DaoProcess_PutValue( proc, self->value );
		return;
	}
#ifdef DAO_WITH_CONCURRENT
	proc->status = DAO_PROCESS_SUSPENDED;
	proc->pauseType = DAO_PAUSE_FUTURE_VALUE;
	DaoCallServer_AddWait( proc, self, -1 );
#else
	DaoProcess_RaiseError( proc, NULL, "Invalid future value" );
#endif
}
static void FUTURE_Wait( DaoProcess *proc, DaoValue *par[], int N )
{
	DaoFuture *self = (DaoFuture*) par[0];
	float timeout = par[1]->xFloat.value;
	DaoProcess_PutBoolean( proc, self->state == DAO_CALL_FINISHED );
	if( self->state == DAO_CALL_FINISHED || timeout == 0 ) return;
#ifdef DAO_WITH_CONCURRENT
	proc->status = DAO_PROCESS_SUSPENDED;
	proc->pauseType = DAO_PAUSE_FUTURE_WAIT;
	DaoCallServer_AddWait( proc, self, timeout );
#else
	DaoProcess_RaiseError( proc, NULL, "Invalid future value" );
#endif
}
static DaoFuncItem futureMeths[] =
{
	{ FUTURE_Value,   "value( self: Future<@V> )=>@V" },
	{ FUTURE_Wait,    "wait( self: Future<@V>, timeout: float = -1 ) => bool" },
	{ NULL, NULL }
};

static void DaoFuture_GetGCFields( void *p, DList *values, DList *lists, DList *maps, int remove )
{
	DaoFuture *self = (DaoFuture*) p;
	if( self->value ) DList_Append( values, self->value );
	if( self->actor ) DList_Append( values, self->actor );
	if( self->message ) DList_Append( values, self->message );
	if( self->selected ) DList_Append( values, self->selected );
	if( self->process ) DList_Append( values, self->process );
	if( self->precond ) DList_Append( values, self->precond );
	if( remove ){
		self->value = NULL;
		self->actor = NULL;
		self->message = NULL;
		self->selected = NULL;
		self->process = NULL;
		self->precond = NULL;
	}
}

DaoTypeBase futureTyper =
{
	"Future<@V=none>", NULL, NULL, (DaoFuncItem*) futureMeths, {0}, {0},
	(FuncPtrDel) DaoFuture_Delete, DaoFuture_GetGCFields
};


