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

#include<assert.h>
#include<string.h>
#include<math.h>
#include<time.h>

#include"daoGC.h"
#include"daoMap.h"
#include"daoClass.h"
#include"daoObject.h"
#include"daoNumtype.h"
#include"daoProcess.h"
#include"daoRoutine.h"
#include"daoNamespace.h"
#include"daoVmspace.h"
#include"daoThread.h"
#include"daoValue.h"


#ifdef DAO_TRACE_ADDRESS
#define DAO_TRACE_ADDRESS ((DaoValue*)0x7fac2cf646a0)
void DaoGC_TraceValue( DaoValue *value )
{
	if( value == DAO_TRACE_ADDRESS ){
		int uninitialized; /* for valgrind; */
		uninitialized += time(NULL);
		if( uninitialized % 1000 == 0 ) printf( "%i\n", uninitialized );
		printf( "DaoGC_TraceValue: %i %i\n", value->type, value->xBase.refCount );
	}
}
#endif

#if defined(DEBUG) && defined(UNIX)
#if 0
#define DEBUG_TRACE
#endif
#endif

#ifdef DEBUG_TRACE
#include <execinfo.h>
void print_trace( const char *info )
{
	void  *array[100];
	daoint i, size = backtrace (array, 100);
	char **strings = backtrace_symbols (array, size);
	FILE *debug = fopen( "debug.txt", "w+" );
	fprintf (debug, "===========================================\n");
	fprintf (debug, "Obtained %" DAO_I64 " stack frames.\n", size);
	printf ("=====================%s======================\n", info);
	printf ("Obtained %" DAO_I64 " stack frames.\n", size);
	for (i = 0; i < size; i++){
		printf ("%s\n", strings[i]);
		fprintf (debug,"%s\n", strings[i]);
	}
	/* comment this to cause leaking, so that valgrind will print the trace with line numbers */
	//free (strings);
	fflush( debug );
	fclose( debug );
	fflush( stdout );
}
#endif


#if DEBUG
static void DaoGC_PrintValueInfo( DaoValue *value )
{
	if( value->type == DAO_TYPE ){
		printf( "type: %s %i %p\t", value->xType.name->chars, value->xType.tid, value );
	}else if( value->type == DAO_CDATA || value->type == DAO_CSTRUCT || value->type == DAO_CTYPE ){
		printf( "cdata: %s\t", value->xCdata.ctype->name->chars );
	}else if( value->type == DAO_CLASS ){
		printf( "class: %s\t", value->xClass.className->chars );
	}else if( value->type == DAO_TYPEKERNEL ){
		printf( "tkernal: %s\t", ((DaoTypeKernel*)value)->typer->name );
	}else if( value->type == DAO_ROUTINE ){
		printf( "routine: %s %s\n", value->xRoutine.routName->chars, value->xRoutine.routType->name->chars );
	}else if( value->type == DAO_OBJECT ){
		printf( "object: %s\n", value->xObject.defClass->className->chars );
	}
	printf( "%16p %2i %i %i\n", value, value->type, value->xGC.refCount, value->xGC.cycRefCount );
}
#else
#define DaoGC_PrintValueInfo( value )
#endif

static void DaoValue_Delete( DaoValue *self );
static int DaoGC_DecRC2( DaoValue *p );
static void DaoGC_CycRefCountDecrements( DaoValue **values, daoint size );
static void DaoGC_CycRefCountIncrements( DaoValue **values, daoint size );
static void DaoGC_RefCountDecrements( DaoValue **values, daoint size );
static void cycRefCountDecrement( DaoValue *value );
static void cycRefCountIncrement( DaoValue *value );
static void cycRefCountDecrements( DList *values );
static void cycRefCountIncrements( DList *values );
static void directRefCountDecrement( DaoValue **value );
static void directRefCountDecrements( DList *values );

static int DaoGC_CycRefCountDecScan( DaoValue *value );
static int DaoGC_CycRefCountIncScan( DaoValue *value );
static int DaoGC_RefCountDecScan( DaoValue *value );

static void DaoGC_PrepareCandidates();

static void DaoCGC_FreeGarbage();
static void DaoCGC_CycRefCountDecScan();
static void DaoCGC_CycRefCountIncScan();
static void DaoCGC_DeregisterModules();
static int  DaoCGC_AliveObjectScan();
static void DaoCGC_RefCountDecScan();
static void DaoCGC_Finish();

static void DaoIGC_FreeGarbage();
static void DaoIGC_CycRefCountDecScan();
static void DaoIGC_DeregisterModules();
static void DaoIGC_CycRefCountIncScan();
static int  DaoIGC_AliveObjectScan();
static void DaoIGC_RefCountDecScan();
static void DaoIGC_Finish();
static void DaoIGC_TryInvoke();

static void DaoGC_Init();

#ifdef DAO_WITH_THREAD
static void DaoCGC_Recycle( void * );
static void DaoCGC_TryBlock();
#endif



#ifdef DAO_USE_GC_LOGGER
typedef struct DaoObjectLogger DaoObjectLogger;

struct DaoObjectLogger
{
	DMap    *allObjects;
	daoint  *newCounts;
	daoint  *delCounts;
	daoint   allCounts[END_EXTRA_TYPES];
	daoint   newCounts1[END_EXTRA_TYPES];
	daoint   newCounts2[END_EXTRA_TYPES];
	daoint   delCounts1[END_EXTRA_TYPES];
	daoint   delCounts2[END_EXTRA_TYPES];
	DList   *cdataValues;
	DList   *cdataLists;
	DList   *cdataMaps;
	DList   *objects;
	DMutex   mutex;
};

DaoObjectLogger dao_object_logger = { NULL, NULL, NULL };

void DaoObjectLogger_SwitchBuffer();

void DaoObjectLogger_Init()
{
	DMutex_Init( & dao_object_logger.mutex );
	dao_object_logger.allObjects = DHash_New(0,0);
	DaoObjectLogger_SwitchBuffer();
}
void DaoObjectLogger_LogNew( DaoValue *object )
{
	if( dao_object_logger.allObjects == NULL ) return;
	DMutex_Lock( & dao_object_logger.mutex );
	dao_object_logger.newCounts[ object->type ] += 1;
	DMap_Insert( dao_object_logger.allObjects, object, object );
	DMutex_Unlock( & dao_object_logger.mutex );
}
void DaoObjectLogger_LogDelete( DaoValue *object )
{
	if( dao_object_logger.allObjects == NULL ) return;
	DMutex_Lock( & dao_object_logger.mutex );
	dao_object_logger.delCounts[ object->type ] += 1;
	DMap_Erase( dao_object_logger.allObjects, object );
	DMutex_Unlock( & dao_object_logger.mutex );
}
#ifdef _WIN32
const char *format = "Type = %9Ii;  Newed = %9Ii;  Deleted = %9Ii;  All = %9Ii\n";
#else
const char *format = "Type = %9ti;  Newed = %9ti;  Deleted = %9ti;  All = %9ti\n";
#endif
void DaoObjectLogger_PrintProfile()
{
	int i;
	printf("=======================================\n");
	for(i=0; i<END_EXTRA_TYPES; ++i){
		daoint N = dao_object_logger.newCounts[i];
		daoint D = dao_object_logger.delCounts[i];
		daoint A = dao_object_logger.allCounts[i] + N - D;
		dao_object_logger.allCounts[i] = A;
		if( N != 0 || D != 0 || A != 0 ) printf( format, i, N, D, A );
	}
}
void DaoObjectLogger_SwitchBuffer()
{
	DMutex_Lock( & dao_object_logger.mutex );
	if( dao_object_logger.newCounts != NULL ) DaoObjectLogger_PrintProfile();
	if( dao_object_logger.newCounts == dao_object_logger.newCounts1 ){
		dao_object_logger.newCounts = dao_object_logger.newCounts2;
		dao_object_logger.delCounts = dao_object_logger.delCounts2;
	}else{
		dao_object_logger.newCounts = dao_object_logger.newCounts1;
		dao_object_logger.delCounts = dao_object_logger.delCounts1;
	}
	memset( dao_object_logger.newCounts, 0, END_EXTRA_TYPES*sizeof(daoint) );
	memset( dao_object_logger.delCounts, 0, END_EXTRA_TYPES*sizeof(daoint) );
	DMutex_Unlock( & dao_object_logger.mutex );
}
static void DaoObjectLogger_ScanValue( DaoValue *object )
{
	DMap *objmap = dao_object_logger.allObjects;
	DNode *it = DMap_Find( objmap, object );
	if( object == NULL || object == dao_none_value ) return;
	object->xBase.refCount --;
	if( it != NULL ) return;
	DList_Append( dao_object_logger.objects, object );
}
static void DaoObjectLogger_ScanValues( DaoValue **values, daoint size )
{
	DMap *objmap = dao_object_logger.allObjects;
	daoint i;
	for(i=0; i<size; ++i) DaoObjectLogger_ScanValue( values[i] );
}
static void DaoObjectLogger_ScanArray2( DList *list )
{
	if( list == NULL ) return;
	DaoObjectLogger_ScanValues( list->items.pValue, list->size );
}
static void DaoObjectLogger_ScanArray( DList *list )
{
	if( list == NULL || list->type != DAO_DATA_VALUE ) return;
	DaoObjectLogger_ScanValues( list->items.pValue, list->size );
}
static void DaoObjectLogger_ScanMap( DMap *map, int gckey, int gcval )
{
	DMap *objmap = dao_object_logger.allObjects;
	DNode *it;
	if( map == NULL || map->size == 0 ) return;
	gckey &= map->keytype == DAO_DATA_VALUE;
	gcval &= map->valtype == DAO_DATA_VALUE;
	for(it = DMap_First(map); it != NULL; it = DMap_Next(map, it) ){
		if( gckey ) DaoObjectLogger_ScanValue( it->key.pValue );
		if( gcval ) DaoObjectLogger_ScanValue( it->value.pValue );
	}
}
static void DaoObjectLogger_ScanCdata( DaoCdata *cdata )
{
	DaoTypeBase *typer = cdata->ctype ? cdata->ctype->typer : NULL;
	DList *cvalues = dao_object_logger.cdataValues;
	DList *clists = dao_object_logger.cdataLists;
	DList *cmaps = dao_object_logger.cdataMaps;
	daoint i, n;

	if( cdata->type == DAO_CTYPE || cdata->subtype == DAO_CDATA_PTR ) return;
	if( typer == NULL || typer->GetGCFields == NULL ) return;
	cvalues->size = clists->size = cmaps->size = 0;
	if( cdata->type == DAO_CSTRUCT ){
		typer->GetGCFields( cdata, cvalues, clists, cmaps, 0 );
	}else if( cdata->data ){
		typer->GetGCFields( cdata->data, cvalues, clists, cmaps, 0 );
	}else{
		return;
	}
	DaoObjectLogger_ScanArray2( cvalues );
	for(i=0,n=clists->size; i<n; i++) DaoObjectLogger_ScanArray( clists->items.pList[i] );
	for(i=0,n=cmaps->size; i<n; i++) DaoObjectLogger_ScanMap( cmaps->items.pMap[i], 1, 1 );
}
void DaoObjectLogger_Quit()
{
	daoint i;
	DNode *it;
	DMap *objmap = dao_object_logger.allObjects;
	DList *objects = DList_New(0);
	DList *cvalues = DList_New(0);
	DList *clists = DList_New(0);
	DList *cmaps = DList_New(0);

	dao_object_logger.objects = objects;
	dao_object_logger.cdataValues = cvalues;
	dao_object_logger.cdataLists = clists;
	dao_object_logger.cdataMaps = cmaps;
	DaoObjectLogger_PrintProfile();

	for(it=DMap_First(objmap); it; it=DMap_Next(objmap, it)){
		DaoValue *object = it->key.pValue;
		DList_Append( objects, object );
	}
	for(i=0; i<objects->size; ++i){
		DaoValue *value = objects->items.pValue[i];
		switch( value->type ){
		case DAO_ENUM :
			{
				DaoEnum *en = (DaoEnum*) value;
				DaoObjectLogger_ScanValue( (DaoValue*) en->etype );
				break;
			}
		case DAO_CONSTANT :
			{
				DaoObjectLogger_ScanValue( value->xConst.value );
				break;
			}
		case DAO_VARIABLE :
			{
				DaoObjectLogger_ScanValue( value->xVar.value );
				DaoObjectLogger_ScanValue( (DaoValue*) value->xVar.dtype );
				break;
			}
		case DAO_PAR_NAMED :
			{
				DaoObjectLogger_ScanValue( value->xNameValue.value );
				DaoObjectLogger_ScanValue( (DaoValue*) value->xNameValue.ctype );
				break;
			}
#ifdef DAO_WITH_NUMARRAY
		case DAO_ARRAY :
			{
				DaoArray *array = (DaoArray*) value;
				DaoObjectLogger_ScanValue( (DaoValue*) array->original );
				break;
			}
#endif
		case DAO_TUPLE :
			{
				DaoTuple *tuple = (DaoTuple*) value;
				DaoObjectLogger_ScanValue( (DaoValue*) tuple->ctype );
				DaoObjectLogger_ScanValues( tuple->values, tuple->size );
				break;
			}
		case DAO_LIST :
			{
				DaoList *list = (DaoList*) value;
				DaoObjectLogger_ScanValue( (DaoValue*) list->ctype );
				DaoObjectLogger_ScanArray( list->value );
				break;
			}
		case DAO_MAP :
			{
				DaoMap *map = (DaoMap*) value;
				DaoObjectLogger_ScanValue( (DaoValue*) map->ctype );
				DaoObjectLogger_ScanMap( map->value, 1, 1 );
				break;
			}
		case DAO_OBJECT :
			{
				DaoObject *obj = (DaoObject*) value;
				if( obj->isRoot ){
					DaoObjectLogger_ScanValues( obj->objValues, obj->valueCount );
				}
				DaoObjectLogger_ScanValue( (DaoValue*) obj->parent );
				DaoObjectLogger_ScanValue( (DaoValue*) obj->rootObject );
				DaoObjectLogger_ScanValue( (DaoValue*) obj->defClass );
				break;
			}
		case DAO_CSTRUCT : case DAO_CDATA : case DAO_CTYPE :
			{
				DaoCdata *cdata = (DaoCdata*) value;
				DaoObjectLogger_ScanValue( (DaoValue*) cdata->object );
				DaoObjectLogger_ScanValue( (DaoValue*) cdata->ctype );
				if( value->type == DAO_CDATA || value->type == DAO_CSTRUCT ){
					DaoObjectLogger_ScanCdata( cdata );
				}else{
					DaoObjectLogger_ScanValue( (DaoValue*) value->xCtype.cdtype );
				}
				break;
			}
		case DAO_ROUTINE :
			{
				DaoRoutine *rout = (DaoRoutine*)value;
				DaoObjectLogger_ScanValue( (DaoValue*) rout->routType );
				DaoObjectLogger_ScanValue( (DaoValue*) rout->routHost );
				DaoObjectLogger_ScanValue( (DaoValue*) rout->nameSpace );
				DaoObjectLogger_ScanValue( (DaoValue*) rout->original );
				DaoObjectLogger_ScanValue( (DaoValue*) rout->routConsts );
				DaoObjectLogger_ScanValue( (DaoValue*) rout->body );
				if( rout->overloads ) DaoObjectLogger_ScanArray( rout->overloads->array );
				if( rout->specialized ) DaoObjectLogger_ScanArray( rout->specialized->array );
				break;
			}
		case DAO_ROUTBODY :
			{
				DaoRoutineBody *rout = (DaoRoutineBody*)value;
				DaoObjectLogger_ScanArray( rout->regType );
				DaoObjectLogger_ScanArray( rout->upValues );
				break;
			}
		case DAO_CLASS :
			{
				DaoClass *klass = (DaoClass*)value;
				DaoObjectLogger_ScanValue( (DaoValue*) klass->clsType );
				DaoObjectLogger_ScanValue( (DaoValue*) klass->castOperators );
				DaoObjectLogger_ScanValue( (DaoValue*) klass->initRoutine );
				DaoObjectLogger_ScanArray( klass->constants );
				DaoObjectLogger_ScanArray( klass->variables );
				DaoObjectLogger_ScanArray( klass->instvars );
				DaoObjectLogger_ScanArray( klass->allBases );
				DaoObjectLogger_ScanArray( klass->references );
				break;
			}
		case DAO_INTERFACE :
			{
				DaoInterface *inter = (DaoInterface*)value;
				DaoObjectLogger_ScanArray( inter->supers );
				DaoObjectLogger_ScanValue( (DaoValue*) inter->abtype );
				DaoObjectLogger_ScanMap( inter->concretes, 0, 1 );
				DaoObjectLogger_ScanMap( inter->methods, 0, 1 );
				break;
			}
		case DAO_CINTYPE :
			{
				DaoCinType *cintype = (DaoCinType*)value;
				DaoObjectLogger_ScanArray( cintype->supers );
				DaoObjectLogger_ScanValue( (DaoValue*) cintype->citype );
				DaoObjectLogger_ScanValue( (DaoValue*) cintype->vatype );
				DaoObjectLogger_ScanValue( (DaoValue*) cintype->target );
				DaoObjectLogger_ScanValue( (DaoValue*) cintype->abstract );
				DaoObjectLogger_ScanMap( cintype->methods, 0, 1 );
				break;
			}
		case DAO_CINVALUE :
			{
				DaoObjectLogger_ScanValue( value->xCinValue.value );
				DaoObjectLogger_ScanValue( (DaoValue*) value->xCinValue.cintype );
				break;
			}
		case DAO_NAMESPACE :
			{
				DaoNamespace *ns = (DaoNamespace*) value;
				DaoObjectLogger_ScanArray( ns->constants );
				DaoObjectLogger_ScanArray( ns->variables );
				DaoObjectLogger_ScanArray( ns->auxData );
				DaoObjectLogger_ScanArray( ns->mainRoutines );
				DaoObjectLogger_ScanMap( ns->abstypes, 0, 1 );
				break;
			}
		case DAO_TYPE :
			{
				DaoType *abtp = (DaoType*) value;
				DaoObjectLogger_ScanValue( abtp->aux );
				DaoObjectLogger_ScanValue( abtp->value );
				DaoObjectLogger_ScanValue( (DaoValue*) abtp->kernel );
				DaoObjectLogger_ScanValue( (DaoValue*) abtp->cbtype );
				DaoObjectLogger_ScanValue( (DaoValue*) abtp->quadtype );
				DaoObjectLogger_ScanArray( abtp->nested );
				DaoObjectLogger_ScanArray( abtp->bases );
				DaoObjectLogger_ScanMap( abtp->interfaces, 1, 0 );
				break;
			}
		case DAO_TYPEKERNEL :
			{
				DaoTypeKernel *kernel = (DaoTypeKernel*) value;
				DaoObjectLogger_ScanValue( (DaoValue*) kernel->abtype );
				DaoObjectLogger_ScanValue( (DaoValue*) kernel->nspace );
				DaoObjectLogger_ScanValue( (DaoValue*) kernel->initRoutines );
				DaoObjectLogger_ScanValue( (DaoValue*) kernel->castOperators );
				DaoObjectLogger_ScanMap( kernel->values, 0, 1 );
				DaoObjectLogger_ScanMap( kernel->methods, 0, 1 );
				if( kernel->sptree ){
					DaoObjectLogger_ScanArray( kernel->sptree->holders );
					DaoObjectLogger_ScanArray( kernel->sptree->defaults );
					DaoObjectLogger_ScanArray( kernel->sptree->sptypes );
				}
				break;
			}
		case DAO_PROCESS :
			{
				DaoProcess *vmp = (DaoProcess*) value;
				DaoStackFrame *frame = vmp->firstFrame;
				DaoObjectLogger_ScanValue( (DaoValue*) vmp->future );
				DaoObjectLogger_ScanArray( vmp->exceptions );
				DaoObjectLogger_ScanArray( vmp->defers );
				DaoObjectLogger_ScanArray( vmp->factory );
				DaoObjectLogger_ScanValues( vmp->stackValues, vmp->stackSize );
				while( frame ){
					DaoObjectLogger_ScanValue( (DaoValue*) frame->routine );
					DaoObjectLogger_ScanValue( (DaoValue*) frame->object );
					DaoObjectLogger_ScanValue( (DaoValue*) frame->retype );
					frame = frame->next;
				}
				break;
			}
		default: break;
		}
	}
	for(i=0; i<objects->size; ++i){
		DaoValue *value = objects->items.pValue[i];
		if( value->xBase.refCount != 0 ){
			DaoGC_PrintValueInfo( value );
		}
	}
	DMutex_Destroy( & dao_object_logger.mutex );
	DMap_Delete( dao_object_logger.allObjects );
	DList_Delete( objects );
	DList_Delete( cvalues );
	DList_Delete( clists );
	DList_Delete( cmaps );
}
#else
void DaoObjectLogger_PrintProfile(){}
void DaoObjectLogger_SwitchBuffer(){}
#endif


typedef struct DaoGarbageCollector  DaoGarbageCollector;
struct DaoGarbageCollector
{
	DList   *idleList;
	DList   *workList;
	DList   *idleList2;
	DList   *workList2;
	DList   *delayList;
	DList   *freeList;
	DList   *auxList;
	DList   *auxList2;
	DList   *nsList;
	DList   *cdataValues;
	DList   *cdataLists;
	DList   *cdataMaps;
	DList   *temporary;
	DMap    *wrappers;

	uchar_t   finalizing;
	uchar_t   delayMask;
	daoint    gcMin, gcMax;
	daoint    ii, jj, kk;
	daoint    cycle;
	daoint    mdelete;
	short     busy;
	short     locked;
	short     workType;
	short     concurrent;

#ifdef DAO_WITH_THREAD
	DThread   thread;

	DMutex    mutex_idle_list;
	DMutex    mutex_start_gc;
	DMutex    mutex_block_mutator;

	DMutex    data_lock;
	DMutex    generic_lock;

	DCondVar  condv_start_gc;
	DCondVar  condv_block_mutator;
#endif
};
static DaoGarbageCollector gcWorker = { NULL, NULL, NULL };


int DaoGC_Min( int n /*=-1*/ )
{
	int prev = gcWorker.gcMin;
	if( n > 0 ) gcWorker.gcMin = n;
	return prev;
}
int DaoGC_Max( int n /*=-1*/ )
{
	int prev = gcWorker.gcMax;
	if( n > 0 ) gcWorker.gcMax = n;
	return prev;
}

void DaoGC_Init()
{
	if( gcWorker.idleList != NULL ) return;

	gcWorker.idleList = DList_New(0);
	gcWorker.workList = DList_New(0);
	gcWorker.idleList2 = DList_New(0);
	gcWorker.workList2 = DList_New(0);
	gcWorker.delayList = DList_New(0);
	gcWorker.freeList = DList_New(0);
	gcWorker.auxList = DList_New(0);
	gcWorker.auxList2 = DList_New(0);
	gcWorker.nsList = DList_New(0);
	gcWorker.cdataValues = DList_New(0);
	gcWorker.cdataLists = DList_New(0);
	gcWorker.cdataMaps = DList_New(0);
	gcWorker.temporary = DList_New(0);
	gcWorker.wrappers = DHash_New(0,DAO_DATA_VALUE);

	gcWorker.delayMask = DAO_VALUE_DELAYGC;
	gcWorker.finalizing = 0;
	gcWorker.cycle = 0;
	gcWorker.mdelete = 0;

	gcWorker.gcMin = 1000;
	gcWorker.gcMax = 100 * gcWorker.gcMin;
	gcWorker.workType = 0;
	gcWorker.ii = 0;
	gcWorker.jj = 0;
	gcWorker.kk = 0;
	gcWorker.busy = 0;
	gcWorker.locked = 0;
	gcWorker.concurrent = 0;
}
void DaoGC_Start()
{
	DaoGC_Init();
}
void DaoCGC_Start()
{
	if( gcWorker.concurrent ) return;
#ifdef DAO_WITH_THREAD
	DThread_Init( & gcWorker.thread );
	DMutex_Init( & gcWorker.data_lock );
	DMutex_Init( & gcWorker.generic_lock );
	DMutex_Init( & gcWorker.mutex_idle_list );
	DMutex_Init( & gcWorker.mutex_start_gc );
	DMutex_Init( & gcWorker.mutex_block_mutator );
	DCondVar_Init( & gcWorker.condv_start_gc );
	DCondVar_Init( & gcWorker.condv_block_mutator );
	DaoIGC_Finish();
	gcWorker.concurrent = 1;
	gcWorker.finalizing = 0;
	gcWorker.cycle = 0;
	if( DThread_Start( & gcWorker.thread, DaoCGC_Recycle, NULL ) == 0 ){
		dao_abort( "failed to create the GC thread" );
	}
#endif
}



static void DaoWrappers_ClearGarbage2()
{
	int count = 0;
	void *keys[1000];
	DNode *it;

	for(it=DMap_First(gcWorker.wrappers); it; it=DMap_Next(gcWorker.wrappers,it)){
		if( it->value.pValue->xBase.refCount == 1 ){
			keys[count++] = it->key.pVoid;
			if( count >= 1000 ) break;
		}
	}
	while( count ) DMap_Erase( gcWorker.wrappers, keys[--count] );
}

#ifdef DAO_WITH_THREAD
static DaoValue* DaoWrappers_Find( void *data )
{
	DNode *node;
	DaoValue *value = NULL;

	if( gcWorker.concurrent ) DMutex_Lock( & gcWorker.generic_lock );
	node = DMap_Find( gcWorker.wrappers, data );
	if( node ) value = (DaoValue*) node->value.pVoid;
	if( gcWorker.concurrent ) DMutex_Unlock( & gcWorker.generic_lock );
	return value;
}
static void DaoWrappers_Insert( void *data, DaoValue *wrap )
{
	if( data == NULL ) return;
	if( gcWorker.concurrent ) DMutex_Lock( & gcWorker.generic_lock );
	DMap_Insert( gcWorker.wrappers, data, wrap );
	if( gcWorker.concurrent ) DMutex_Unlock( & gcWorker.generic_lock );
}
static void DaoWrappers_Erase( void *data )
{
	if( data == NULL ) return;
	if( gcWorker.concurrent ) DMutex_Lock( & gcWorker.generic_lock );
	DMap_Erase( gcWorker.wrappers, data );
	if( gcWorker.concurrent ) DMutex_Unlock( & gcWorker.generic_lock );
}
static void DaoWrappers_ClearGarbage()
{
	if( gcWorker.concurrent ) DMutex_Lock( & gcWorker.generic_lock );
	DaoWrappers_ClearGarbage2();
	if( gcWorker.concurrent ) DMutex_Unlock( & gcWorker.generic_lock );
}
#else
static DaoValue* DaoWrappers_Find( void *data )
{
	DNode *node = DMap_Find( gcWorker.wrappers, data );
	if( node ) return (DaoValue*) node->value.pVoid;
	return NULL;
}
static void DaoWrappers_Insert( void *data, DaoValue *wrap )
{
	if( data == NULL ) return;
	DMap_Insert( gcWorker.wrappers, data, wrap );
}
static void DaoWrappers_Erase( void *data )
{
	if( data == NULL ) return;
	DMap_Erase( gcWorker.wrappers, data );
}
static void DaoWrappers_ClearGarbage()
{
	DaoWrappers_ClearGarbage2();
}
#endif



DaoCdata* DaoWrappers_MakeCdata( DaoType *type, void *data, int owned )
{
	DaoCdata *cdata = (DaoCdata*) DaoWrappers_Find( data );
	if( cdata && cdata->type == DAO_CDATA && cdata->ctype == type ) return cdata;
	cdata = owned ? DaoCdata_New( type, data ) : DaoCdata_Wrap( type, data );
	DaoWrappers_Insert( data, (DaoValue*) cdata );
	return cdata;
}
DaoCinValue* DaoWrappers_MakeCinValue( DaoCinType *type, DaoValue *value )
{
	DaoCinValue *cinva = (DaoCinValue*) DaoWrappers_Find( value );
	if( cinva && cinva->type == DAO_CINVALUE && cinva->cintype == type && cinva->value == value ){
		return cinva;
	}
	cinva = DaoCinValue_New( type, value );
	DaoWrappers_Insert( value, (DaoValue*) cinva );
	return cinva;
}



static void DaoGC_DeleteSimpleData( DaoValue *value )
{
	if( value == NULL || value->xGC.refCount ) return;
	switch( value->type ){
	case DAO_NONE :
	case DAO_BOOLEAN :
	case DAO_INTEGER :
	case DAO_FLOAT :
	case DAO_COMPLEX :
#ifdef DAO_USE_GC_LOGGER
		DaoObjectLogger_LogDelete( value );
#endif
		dao_free( value );
		break;
	case DAO_STRING :
		DaoString_Delete( & value->xString );
		break;
	default: break;
	}
}
static void DaoValue_Delete( DaoValue *self )
{
	if( self->type == DAO_CDATA ){
		DaoCdata_Delete( (DaoCdata*) self );
	}else if( self->type == DAO_CTYPE ){
		DaoCtype_Delete( (DaoCtype*) self );
	}else if( self->type == DAO_ROUTBODY ){
		DaoRoutineBody_Delete( (DaoRoutineBody*) self );
	}else if( self->type == DAO_CONSTANT ){
		DaoConstant_Delete( (DaoConstant*)  self);
	}else if( self->type == DAO_VARIABLE ){
		DaoVariable_Delete( (DaoVariable*)  self);
	}else if( self->type < DAO_ENUM ){
		DaoGC_DeleteSimpleData( self );
	}else{
		DaoTypeBase *typer = DaoValue_GetTyper( self );
		typer->Delete( self );
	}
}


void DList_PushBack2( DList *self, void *val )
{
	if( (daoint)(self->size + 1) >= self->bufsize ){
		void **buf = self->items.pVoid;
		self->bufsize += self->bufsize/5 + 5;
		self->items.pVoid = (void**) dao_realloc( buf, (self->bufsize+1)*sizeof(void*) );
	}
	self->items.pVoid[ self->size ] = val;
	self->size++;
}


static int DaoGC_DecRC2( DaoValue *p )
{
	daoint i, n;
	p->xGC.refCount --;
#ifdef DAO_TRACE_ADDRESS
	DaoGC_TraceValue( p );
#endif
	if( p->xGC.refCount == 0 ){
		switch( p->xGC.type ){
		case DAO_NONE :
		case DAO_BOOLEAN:
		case DAO_INTEGER :
		case DAO_FLOAT :
		case DAO_COMPLEX :
		case DAO_STRING :
#if 0
			if( gcWorker.concurrent ){
				DList_PushBack2( gcWorker.idleList2, p );
			}else{
				DaoGC_DeleteSimpleData( p );
			}
#else
			DaoGC_DeleteSimpleData( p );
#endif
			return 0;
#ifdef DAO_WITH_NUMARRAY
		case DAO_ARRAY :
			DaoArray_ResizeVector( & p->xArray, 0 ); break;
#endif
		case DAO_TUPLE :
			if( p->xTuple.ctype && p->xTuple.ctype->noncyclic ){
				DaoTuple *tuple = & p->xTuple;
				for(i=0,n=tuple->size; i<n; i++){
					if( tuple->values[i] ){
						DaoGC_DecRC2( tuple->values[i] );
						tuple->values[i] = NULL;
					}
				}
				tuple->size = 0;
			}
			break;
		case DAO_LIST : // TODO same for map
			if( p->xList.ctype && p->xList.ctype->noncyclic ){
				DList *array = p->xList.value;
				DaoValue **items = array->items.pValue;
				for(i=0,n=array->size; i<n; i++) if( items[i] ) DaoGC_DecRC2( items[i] );
				array->size = 0;
				array->type = 0; /* To avoid locking in DList_Clear(); */
				DList_Clear( array );
			}
			break;
		default :
			/* No safe way to delete other types of objects here, since they might be
			 * being concurrently scanned by the GC! */
			break;
		}
	}

	/* never push simple data types into GC queue,
	 * because they cannot form cyclic referencing structure: */
	if( p->type < DAO_ENUM ) return 0;
	if( p->xGC.delay ) return 0;
	DList_PushBack2( gcWorker.idleList, p );
	return 1;
}

void DaoGC_Finish()
{
	daoint i;

	DMap_Clear( gcWorker.wrappers );
	if( gcWorker.concurrent ){
#ifdef DAO_WITH_THREAD
		DaoCGC_Finish();
#endif
	}else{
		DaoIGC_Finish();
	}

	DaoObjectLogger_SwitchBuffer();

	DMap_Delete( gcWorker.wrappers );

#ifdef DAO_WITH_THREAD
	if( gcWorker.concurrent ){
		DThread_Destroy( & gcWorker.thread );
		DMutex_Destroy( & gcWorker.data_lock );
		DMutex_Destroy( & gcWorker.generic_lock );
		DMutex_Destroy( & gcWorker.mutex_idle_list );
		DMutex_Destroy( & gcWorker.mutex_start_gc );
		DMutex_Destroy( & gcWorker.mutex_block_mutator );
		DCondVar_Destroy( & gcWorker.condv_start_gc );
		DCondVar_Destroy( & gcWorker.condv_block_mutator );
	}
#endif

	DList_Delete( gcWorker.idleList );
	DList_Delete( gcWorker.workList );
	DList_Delete( gcWorker.idleList2 );
	DList_Delete( gcWorker.workList2 );
	DList_Delete( gcWorker.delayList );
	DList_Delete( gcWorker.freeList );
	DList_Delete( gcWorker.auxList );
	DList_Delete( gcWorker.auxList2 );
	DList_Delete( gcWorker.nsList );
	DList_Delete( gcWorker.cdataValues );
	DList_Delete( gcWorker.cdataLists );
	DList_Delete( gcWorker.cdataMaps );
	DList_Delete( gcWorker.temporary );
	gcWorker.idleList = NULL;
}




#ifdef DAO_WITH_THREAD


void DaoGC_IncRC( DaoValue *value )
{
	if( value == NULL ) return;
	if( value->type >= DAO_ENUM ) value->xGC.cycRefCount ++;
	if( gcWorker.concurrent ){
		DMutex_Lock( & gcWorker.mutex_idle_list );
		value->xGC.refCount ++;
		DMutex_Unlock( & gcWorker.mutex_idle_list );
#ifdef DAO_TRACE_ADDRESS
		DaoGC_TraceValue( value );
#endif
		return;
	}
	value->xGC.refCount ++;
#ifdef DAO_TRACE_ADDRESS
	DaoGC_TraceValue( value );
#endif
}
void DaoGC_DecRC( DaoValue *value )
{
	if( value == NULL ) return;
	if( gcWorker.concurrent ){
		int bl;
		DMutex_Lock( & gcWorker.mutex_idle_list );
		bl = DaoGC_DecRC2( value );
		DMutex_Unlock( & gcWorker.mutex_idle_list );
		if( bl ) DaoCGC_TryBlock();
		return;
	}
	DaoGC_DecRC2( value );
}
void DaoGC_Assign( DaoValue **dest, DaoValue *src )
{
	DaoValue *value = *dest;
	if( src == value ) return;
	if( src && src->type >= DAO_ENUM ) src->xGC.cycRefCount ++;
	if( gcWorker.concurrent ){
		int bl;
		DMutex_Lock( & gcWorker.mutex_idle_list );
		if( src ) src->xGC.refCount ++;
#ifdef DAO_TRACE_ADDRESS
		DaoGC_TraceValue( src );
#endif
		bl = value ? DaoGC_DecRC2( value ) : 0;
		*dest = src;
		DMutex_Unlock( & gcWorker.mutex_idle_list );
		if( bl ) DaoCGC_TryBlock();
		return;
	}
	if( src ) src->xGC.refCount ++;
#ifdef DAO_TRACE_ADDRESS
	DaoGC_TraceValue( src );
#endif
	*dest = src;
	if( value ) DaoGC_DecRC2( value );
}
void DaoGC_TryInvoke()
{
	if( gcWorker.concurrent ){
		DaoCGC_TryBlock();
		return;
	}
	DaoIGC_TryInvoke();
}


#else


void DaoGC_IncRC( DaoValue *value )
{
	if( value ){
		value->xGC.refCount ++;
		if( value->type >= DAO_ENUM ) value->xGC.cycRefCount ++;
	}
#ifdef DAO_TRACE_ADDRESS
	DaoGC_TraceValue( value );
#endif
}
void DaoGC_DecRC( DaoValue *value )
{
	if( value ) DaoGC_DecRC2( value );
}
void DaoGC_Assign( DaoValue **dest, DaoValue *src )
{
	DaoValue *value = *dest;
	if( src == value ) return;
	if( src ){
		src->xGC.refCount ++;
		if( src->type >= DAO_ENUM ) src->xGC.cycRefCount ++;
	}
#ifdef DAO_TRACE_ADDRESS
	DaoGC_TraceValue( src );
#endif
	*dest = src;
	if( value ) DaoGC_DecRC2( value );
}
void DaoGC_TryInvoke()
{
	DaoIGC_TryInvoke();
}
#endif

void DaoGC_TryDelete( DaoValue *value )
{
	GC_IncRC( value );
	GC_DecRC( value );
}

static void DaoGC_FreeSimple()
{
	DaoValue *value;
	DList *workList2 = gcWorker.workList2;
	daoint i, k = 0;
	for(i=0,k=0; i<workList2->size; i++){
		value = workList2->items.pValue[i];
		if( value->xGC.work ) continue;
		value->xGC.work = 1;
		workList2->items.pValue[k++] = value;
	}
	for(i=0; i<k; i++) DaoValue_Delete( workList2->items.pValue[i] );
	workList2->size = 0;
}

#define DAO_FULL_GC_SCAN_CYCLE 16

/* The implementation makes sure the GC fields are modified only by the GC thread! */
void DaoGC_PrepareCandidates()
{
	DaoValue *value;
	DList *workList = gcWorker.workList;
	DList *freeList = gcWorker.freeList;
	DList *delayList = gcWorker.delayList;
	DList *types = gcWorker.temporary;
	uchar_t cycle = (++gcWorker.cycle) % DAO_FULL_GC_SCAN_CYCLE;
	uchar_t delay = cycle && gcWorker.finalizing == 0 ? DAO_VALUE_DELAYGC : 0;
	daoint i, k = 0;
	int delay2;
	for(i=0; i<freeList->size; ++i) freeList->items.pValue[i]->xGC.work = 1;
	gcWorker.delayMask = delay;
	/* damping to avoid "delay2" changing too dramatically: */
	gcWorker.mdelete = 0.5*gcWorker.mdelete + 0.5*freeList->size;
	delay2 = gcWorker.cycle % (1 + 100 / (1 + gcWorker.mdelete));
	if( gcWorker.finalizing ) delay2 = 0;
	if( delay == 0 ){
		/* push delayed objects into the working list for full GC scan: */
		for(i=0; i<delayList->size; ++i){
			value = delayList->items.pValue[i];
			if( value->xGC.work ) continue;
			value->xGC.delay = 0;
			DList_PushBack2( workList, value );
		}
		delayList->size = 0;
	}else if( freeList->size ){
		for(i=0,k=0; i<delayList->size; ++i){
			value = delayList->items.pValue[i];
			if( value->xGC.work ) continue;
			delayList->items.pValue[k++] = value;
		}
		delayList->size = k;
	}
	/* Remove possible redundant items: */
	for(i=0,k=0; i<workList->size; ++i){
		value = workList->items.pValue[i];
		if( value->xGC.work | value->xGC.delay ) continue;
		if( (value->xBase.trait & delay) || (delay2 && value->xBase.refCount) ){
			/*
			// for non full scan cycles, delay scanning on objects with DAO_VALUE_DELAYGC trait;
			// and delay scanning on objects with reference count >= 1:
			*/
			value->xGC.delay = 1;
			DList_PushBack2( delayList, value );
			continue;
		}else if( value->type == DAO_PROCESS && value->xProcess.status > DAO_PROCESS_ABORTED ){
			if( gcWorker.finalizing == 0 ){
				value->xGC.delay = 1;
				DList_PushBack2( delayList, value );
				continue;
			}
		}
		workList->items.pValue[k++] = value;
		value->xGC.cycRefCount = value->xGC.refCount;
		value->xGC.work = 1;
		value->xGC.alive = 0;
	}
#if 0
	printf( "%9i %6i %9i %9i\n", gcWorker.cycle, delay, workList->size, k );
#endif
	workList->size = k;
	types->size = 0;
	for(i=0; i<freeList->size; i++){
		if( freeList->items.pValue[i]->type == DAO_TYPE ){
			DList_PushBack2( types, freeList->items.pValue[i] ); /* should be freed after cdata; */
			continue;
		}
		DaoValue_Delete( freeList->items.pValue[i] );
	}
	freeList->size = 0;
	for(i=0; i<types->size; ++i) DaoValue_Delete( types->items.pValue[i] );
}

enum DaoGCActions{ DAO_GC_DEC, DAO_GC_INC, DAO_GC_BREAK };

void DaoGC_LockData()
{
	if( gcWorker.concurrent == 0 ) return;
#ifdef DAO_WITH_THREAD
	DMutex_Lock( & gcWorker.data_lock );
	gcWorker.locked = 1;
#endif
}
void DaoGC_UnlockData()
{
#ifdef DAO_WITH_THREAD
	if( gcWorker.locked == 0 ) return;
	gcWorker.locked = 0;
	DMutex_Unlock( & gcWorker.data_lock );
#endif
}
static void DaoGC_ScanArray( DList *array, int action, int valueArrayOnly )
{
	if( array == NULL || array->size == 0 ) return;
	if( valueArrayOnly && array->type != DAO_DATA_VALUE ) return;
	switch( action ){
	case DAO_GC_DEC : cycRefCountDecrements( array ); break;
	case DAO_GC_INC : cycRefCountIncrements( array ); break;
	case DAO_GC_BREAK : directRefCountDecrements( array ); array->size = 0; break;
	}
}
static void DaoGC_ScanValue( DaoValue **value, int action )
{
	switch( action ){
	case DAO_GC_DEC : cycRefCountDecrement( *value ); break;
	case DAO_GC_INC : cycRefCountIncrement( *value ); break;
	case DAO_GC_BREAK : directRefCountDecrement( value ); break;
	}
}
static int DaoGC_ScanMap( DMap *map, int action, int gckey, int gcvalue )
{
	int count = 0;
	DNode *it;
	if( map == NULL || map->size == 0 ) return 0;
	gckey &= map->keytype == DAO_DATA_VALUE;
	gcvalue &= map->valtype == DAO_DATA_VALUE;
	if( action != DAO_GC_BREAK ){
		/* if action == DAO_GC_BREAK, no mutator can access this map: */
		DaoGC_LockData();
	}
	for(it = DMap_First( map ); it != NULL; it = DMap_Next( map, it ) ){
		if( gckey ) DaoGC_ScanValue( & it->key.pValue, action );
		if( gcvalue ) DaoGC_ScanValue( & it->value.pValue, action );
		count += gckey + gcvalue;
	}
	if( action == DAO_GC_BREAK ){
		if( map->keytype == DAO_DATA_VALUE ) map->keytype = 0;
		if( map->valtype == DAO_DATA_VALUE ) map->valtype = 0;
		DMap_Clear( map );
	}else{
		DaoGC_UnlockData();
	}
	return count;
}
static void DaoGC_ScanCdata( DaoCdata *cdata, int action )
{
	DaoTypeBase *typer = cdata->ctype ? cdata->ctype->typer : NULL;
	DList *cvalues = gcWorker.cdataValues;
	DList *clists = gcWorker.cdataLists;
	DList *cmaps = gcWorker.cdataMaps;
	daoint i, n;

	if( cdata->type == DAO_CTYPE || cdata->subtype == DAO_CDATA_PTR ) return;
	if( typer == NULL || typer->GetGCFields == NULL ) return;
	cvalues->size = clists->size = cmaps->size = 0;
	if( cdata->type == DAO_CSTRUCT ){
		typer->GetGCFields( cdata, cvalues, clists, cmaps, action == DAO_GC_BREAK );
	}else if( cdata->data ){
		typer->GetGCFields( cdata->data, cvalues, clists, cmaps, action == DAO_GC_BREAK );
	}else{
		return;
	}
	DaoGC_ScanArray( cvalues, action, 0 );
	for(i=0,n=clists->size; i<n; i++) DaoGC_ScanArray( clists->items.pList[i], action, 0 );
	for(i=0,n=cmaps->size; i<n; i++) DaoGC_ScanMap( cmaps->items.pMap[i], action, 1, 1 );
}

#ifdef DAO_WITH_THREAD

/* Concurrent Garbage Collector */


void DaoCGC_Finish()
{
	gcWorker.gcMin = 0;
	gcWorker.finalizing = 1;
	DThread_Join( & gcWorker.thread );
}
void DaoCGC_TryBlock()
{
	if( gcWorker.idleList->size >= gcWorker.gcMax ){
		DThreadData *thdData = DThread_GetSpecific();
		if( thdData && ! ( thdData->state & DTHREAD_NO_PAUSE ) ){
			DMutex_Lock( & gcWorker.mutex_block_mutator );
			DCondVar_TimedWait( & gcWorker.condv_block_mutator, & gcWorker.mutex_block_mutator, 0.001 );
			DMutex_Unlock( & gcWorker.mutex_block_mutator );
		}
	}
}


void DaoCGC_Recycle( void *p )
{
	DList *works = gcWorker.workList;
	DList *idles = gcWorker.idleList;
	DList *works2 = gcWorker.workList2;
	DList *idles2 = gcWorker.idleList2;
	DList *frees = gcWorker.freeList;
	DList *delays = gcWorker.delayList;
	daoint N;
	while(1){
		N = idles->size + works->size + idles2->size + works2->size + frees->size + delays->size;
		if( gcWorker.finalizing && N == 0 ) break;
		gcWorker.busy = 0;
		while( ! gcWorker.finalizing && (idles->size + idles->size) < gcWorker.gcMin ){
			daoint gcount = idles->size + idles2->size;
			double wtime = 3.0 * gcount / (double)gcWorker.gcMin;
			wtime = 0.01 * exp( - wtime * wtime );
			DMutex_Lock( & gcWorker.mutex_start_gc );
			DCondVar_TimedWait( & gcWorker.condv_start_gc, & gcWorker.mutex_start_gc, wtime );
			DMutex_Unlock( & gcWorker.mutex_start_gc );
			if( idles2->size > 10 ){
				DMutex_Lock( & gcWorker.mutex_idle_list );
				DList_Swap( idles2, works2 );
				DMutex_Unlock( & gcWorker.mutex_idle_list );
				DaoGC_FreeSimple();
			}
		}
		gcWorker.busy = 1;

		DMutex_Lock( & gcWorker.mutex_idle_list );
		DList_Swap( idles, works );
		DList_Swap( idles2, works2 );
		DMutex_Unlock( & gcWorker.mutex_idle_list );
		DaoGC_FreeSimple();

		gcWorker.kk = 0;
		DaoGC_PrepareCandidates();
		DaoCGC_CycRefCountDecScan();
		DaoCGC_CycRefCountIncScan();
		DaoCGC_DeregisterModules();
		DaoCGC_CycRefCountIncScan();
		DaoCGC_RefCountDecScan();
		DaoCGC_FreeGarbage();
	}
	DThread_Exit( & gcWorker.thread );
}
void DaoCGC_CycRefCountDecScan()
{
	DList *workList = gcWorker.workList;
	uchar_t delay = gcWorker.delayMask;
	daoint i, k;

	for(i=0; i<workList->size; i++){
		DaoValue *value = workList->items.pValue[i];
		if( value->xGC.delay ) continue;
		DaoGC_CycRefCountDecScan( value );
	}
}
void DaoCGC_DeregisterModules()
{
	daoint i;
	DList *workList = gcWorker.workList;

	DaoWrappers_ClearGarbage();

	for( i=0; i<workList->size; i++ ){
		DaoValue *value = workList->items.pValue[i];
		if( value->xGC.alive ) continue;
		if( value->type == DAO_NAMESPACE ){
			DaoNamespace *NS = (DaoNamespace*) value;
			DaoVmSpace_Lock( NS->vmSpace );
			if( NS->cycRefCount == 0 ) DMap_Erase( NS->vmSpace->nsModules, NS->name );
			DaoVmSpace_Unlock( NS->vmSpace );
		}
	}
}
void DaoCGC_CycRefCountIncScan()
{
	daoint i;
	DList *workList = gcWorker.workList;
	DList *auxList = gcWorker.auxList;

#if 0
	if( gcWorker.finalizing ){
		for( i=0; i<workList->size; i++ ){
			DaoValue *value = workList->items.pValue[i];
			if( value->xGC.cycRefCount >0 ) DaoGC_PrintValueInfo( value );
		}
	}
#endif
	for( i=0; i<workList->size; i++ ){
		DaoValue *value = workList->items.pValue[i];
		if( value->xGC.alive ) continue;
		if( value->xGC.cycRefCount >0 ){
			auxList->size = 0;
			value->xGC.alive = 1;
			DList_PushBack2( auxList, value );
			DaoCGC_AliveObjectScan();
		}
	}
}
int DaoCGC_AliveObjectScan()
{
	DList *auxList = gcWorker.auxList;
	uchar_t delay = gcWorker.delayMask;
	daoint i, k;

	for( i=0; i<auxList->size; i++){
		DaoValue *value = auxList->items.pValue[i];
		if( value->xGC.delay ) continue;
		DaoGC_CycRefCountIncScan( value );
	}
	return auxList->size;
}

void DaoCGC_RefCountDecScan()
{
	DList *workList = gcWorker.workList;
	uchar_t delay = gcWorker.delayMask;
	daoint i, k;

	for( i=0; i<workList->size; i++ ){
		DaoValue *value = workList->items.pValue[i];
		if( value->xGC.cycRefCount && value->xGC.refCount ) continue;
		if( value->xGC.delay ) continue;

		DaoGC_RefCountDecScan( value );
	}
}
static void DaoCGC_FreeGarbage()
{
	DList *idleList = gcWorker.idleList;
	DList *workList = gcWorker.workList;
	uchar_t delay = gcWorker.delayMask;
	daoint i, n = 0;

	for(i=0; i<gcWorker.auxList2->size; i++) gcWorker.auxList2->items.pValue[i]->xGC.alive = 0;
	gcWorker.auxList2->size = 0;

	for(i=0; i<workList->size; i++){
		DaoValue *value = workList->items.pValue[i];
		value->xGC.work = value->xGC.alive = 0;
		if( value->xGC.cycRefCount && value->xGC.refCount ) continue;
		if( value->xGC.refCount !=0 ){
			printf(" refCount not zero %i: %i\n", value->type, value->xGC.refCount );
			DaoGC_PrintValueInfo( value );

			value->xGC.delay = 1;
			DList_PushBack2( gcWorker.delayList, value );
			continue;
		}
		DList_PushBack2( gcWorker.freeList, value );
	}
	DaoObjectLogger_SwitchBuffer();
	workList->size = 0;
}


#endif

/* Incremental Garbage Collector */
enum DaoGCWorkType
{
	GC_RESET_RC ,
	GC_DEC_RC ,
	GC_INC_RC ,
	GC_DEREG ,
	GC_INC_RC2 ,
	GC_DIR_DEC_RC ,
	GC_FREE
};

static void DaoIGC_Switch();
static void DaoIGC_Continue();
static void DaoIGC_RefCountDecScan();

static int counts = 1000;
static void DaoIGC_TryInvoke()
{
	if( gcWorker.busy ) return;
	if( -- counts ) return;
	if( gcWorker.idleList->size < gcWorker.gcMax ){
		counts = 1000;
	}else{
		counts = 100;
	}

	if( gcWorker.workList->size )
		DaoIGC_Continue();
	else if( gcWorker.idleList->size > gcWorker.gcMin )
		DaoIGC_Switch();
}

void DaoIGC_Switch()
{
	if( gcWorker.busy ) return;
	DList_Swap( gcWorker.idleList, gcWorker.workList );
	gcWorker.workType = 0;
	gcWorker.ii = 0;
	gcWorker.jj = 0;
	gcWorker.kk = 0;
	DaoIGC_Continue();
}
void DaoIGC_Continue()
{
	if( gcWorker.busy ) return;
	gcWorker.busy = 1;
	switch( gcWorker.workType ){
	case GC_RESET_RC :
		DaoGC_PrepareCandidates();
		gcWorker.workType = GC_DEC_RC;
		gcWorker.ii = 0;
		break;
	case GC_DEC_RC :
		DaoIGC_CycRefCountDecScan();
#if 0
		if( gcWorker.finalizing && gcWorker.workType == GC_INC_RC ){
			daoint i;
			for( i=0; i<gcWorker.workList->size; i++ ){
				DaoValue *value = gcWorker.workList->items.pValue[i];
				if( value->xGC.cycRefCount >0 ) DaoGC_PrintValueInfo( value );
			}
		}
#endif
		break;
	case GC_DEREG :
		DaoIGC_DeregisterModules();
		break;
	case GC_INC_RC :
	case GC_INC_RC2 :
		DaoIGC_CycRefCountIncScan();
		break;
	case GC_DIR_DEC_RC :
		DaoIGC_RefCountDecScan();
		break;
	case GC_FREE :
		DaoIGC_FreeGarbage();
		break;
	default : break;
	}
	gcWorker.busy = 0;
}
void DaoIGC_Finish()
{
	DList *works = gcWorker.workList;
	DList *idles = gcWorker.idleList;
	DList *works2 = gcWorker.workList2;
	DList *idles2 = gcWorker.idleList2;
	DList *frees = gcWorker.freeList;
	DList *delays = gcWorker.delayList;
	gcWorker.finalizing = 1;
	while( idles->size + works->size + idles2->size + works2->size + frees->size + delays->size ){
		while( works->size ) DaoIGC_Continue();
		DaoIGC_Switch();
	}
}
void DaoIGC_CycRefCountDecScan()
{
	DList *workList = gcWorker.workList;
	uchar_t delay = gcWorker.delayMask;
	daoint min = workList->size >> 2;
	daoint i = gcWorker.ii;
	daoint j = 0, k;

	if( min < gcWorker.gcMin ) min = gcWorker.gcMin;
	for( ; i<workList->size; i++ ){
		DaoValue *value = workList->items.pValue[i];
		if( value->xGC.delay ) continue;
		j += DaoGC_CycRefCountDecScan( value );
		if( (++j) >= min ) break;
	}
	if( i >= workList->size ){
		gcWorker.ii = 0;
		gcWorker.workType = GC_INC_RC;
	}else{
		gcWorker.ii = i+1;
	}
}
void DaoIGC_DeregisterModules()
{
	DList *workList = gcWorker.workList;
	daoint min = workList->size >> 2;
	daoint i = gcWorker.ii;
	daoint k = 0;

	DaoWrappers_ClearGarbage();

	if( min < gcWorker.gcMin ) min = gcWorker.gcMin;

	for( ; i<workList->size; i++ ){
		DaoValue *value = workList->items.pValue[i];
		if( value->xGC.alive ) continue;
		if( value->type == DAO_NAMESPACE ){
			DaoNamespace *NS = (DaoNamespace*) value;
			DaoVmSpace_Lock( NS->vmSpace );
			if( NS->cycRefCount == 0 ) DMap_Erase( NS->vmSpace->nsModules, NS->name );
			DaoVmSpace_Unlock( NS->vmSpace );
		}
	}
	if( i >= workList->size ){
		gcWorker.ii = 0;
		gcWorker.workType ++;
	}else{
		gcWorker.ii = i+1;
	}
}
void DaoIGC_CycRefCountIncScan()
{
	DList *workList = gcWorker.workList;
	DList *auxList = gcWorker.auxList;
	daoint min = workList->size >> 2;
	daoint i = gcWorker.ii;
	daoint k = 0;

	if( min < gcWorker.gcMin ) min = gcWorker.gcMin;
	if( gcWorker.jj ){
		k += DaoIGC_AliveObjectScan();
		if( gcWorker.jj ) return;
	}

	for( ; i<workList->size; i++ ){
		DaoValue *value = workList->items.pValue[i];
		if( value->xGC.alive ) continue;
		if( value->xGC.cycRefCount >0 ){
			auxList->size = 0;
			value->xGC.alive = 1;
			DList_PushBack2( auxList, value );
			k += DaoIGC_AliveObjectScan();
			if( gcWorker.jj || k >= min ) break;
		}
	}
	if( i >= workList->size ){
		gcWorker.ii = 0;
		gcWorker.workType ++;
	}else{
		gcWorker.ii = i+1;
	}
}
int DaoIGC_AliveObjectScan()
{
	daoint i, k = 9;
	daoint j = gcWorker.jj;
	daoint min = gcWorker.workList->size >> 2;
	uchar_t delay = gcWorker.delayMask;
	DList *auxList = gcWorker.auxList;

	if( min < gcWorker.gcMin ) min = gcWorker.gcMin;
	for( ; j<auxList->size; j++){
		DaoValue *value = auxList->items.pValue[j];
		if( value->xGC.delay ) continue;
		k += DaoGC_CycRefCountIncScan( value );
		if( (++k) >= min ) break;
	}
	if( j >= auxList->size ){
		gcWorker.jj = 0;
	}else{
		gcWorker.jj = j+1;
	}
	return k;
}
void DaoIGC_RefCountDecScan()
{
	DList *workList = gcWorker.workList;
	uchar_t delay = gcWorker.delayMask;
	daoint min = workList->size >> 2;
	daoint i = gcWorker.ii;
	daoint j = 0, k;

	if( min < gcWorker.gcMin ) min = gcWorker.gcMin;
	for(; i<workList->size; i++, j++){
		DaoValue *value = workList->items.pValue[i];
		if( value->xGC.cycRefCount && value->xGC.refCount ) continue;
		if( value->xGC.delay ) continue;
		j += DaoGC_RefCountDecScan( value );
		if( j >= min ) break;
	}
	if( i >= workList->size ){
		gcWorker.ii = 0;
		gcWorker.workType = GC_FREE;
	}else{
		gcWorker.ii = i+1;
	}
}

void DaoIGC_FreeGarbage()
{
	DList *idleList = gcWorker.idleList;
	DList *workList = gcWorker.workList;
	daoint min = workList->size >> 2;
	daoint i = gcWorker.ii;
	daoint j = 0;

	if( min < gcWorker.gcMin ) min = gcWorker.gcMin;
	for(; i<workList->size; i++, j++){
		DaoValue *value = workList->items.pValue[i];
		value->xGC.work = value->xGC.alive = 0;
		if( value->xGC.cycRefCount && value->xGC.refCount ) continue;
		if( value->xGC.refCount !=0 ){
			printf(" refCount not zero %p %i: %i\n", value, value->type, value->xGC.refCount);
			DaoGC_PrintValueInfo( value );
			value->xGC.delay = 1;
			DList_PushBack2( gcWorker.delayList, value );
			continue;
		}
		DList_PushBack2( gcWorker.freeList, value );
		if( j >= min ) break;
	}
	if( i >= workList->size ){
		gcWorker.ii = 0;
		gcWorker.workType = GC_RESET_RC;
		workList->size = 0;
	}else{
		gcWorker.ii = i+1;
	}
	for(i=0; i<gcWorker.auxList2->size; i++) gcWorker.auxList2->items.pValue[i]->xGC.alive = 0;
	gcWorker.auxList2->size = 0;
	DaoObjectLogger_SwitchBuffer();
}
void cycRefCountDecrement( DaoValue *value )
{
	if( value == NULL ) return;
	/* do not scan simple data types, as they cannot from cyclic structure: */
	if( value->type < DAO_ENUM ) return;
	if( (value->xBase.trait & gcWorker.delayMask) && value->xGC.delay == 0 ){
		DList_PushBack2( gcWorker.delayList, value );
		value->xGC.cycRefCount = value->xGC.refCount;
		value->xGC.delay = 1;
		return;
	}else if( ! value->xGC.work ){
		DList_PushBack2( gcWorker.workList, value );
		value->xGC.cycRefCount = value->xGC.refCount;
		value->xGC.work = 1;
	}
	value->xGC.cycRefCount --;

	if( value->xGC.cycRefCount<0 ){
#if DEBUG
		printf( "cycRefCount<0 : %2i %p\n", value->type, value );
		DaoGC_PrintValueInfo( value );
#endif
		/*
		 */
		value->xGC.cycRefCount = 0;
	}
}
void cycRefCountIncrement( DaoValue *value )
{
	if( value == NULL ) return;
	/* do not scan simple data types, as they cannot from cyclic structure: */
	if( value->type < DAO_ENUM ) return;
	value->xGC.cycRefCount++;
	if( ! value->xGC.alive ){
		value->xGC.alive = 1;
		DList_PushBack2( gcWorker.auxList, value );
		DList_PushBack2( gcWorker.auxList2, value );
	}
}
void DaoGC_CycRefCountDecrements( DaoValue **values, daoint size )
{
	daoint i;
	for(i=0; i<size; i++) cycRefCountDecrement( values[i] );
}
void DaoGC_CycRefCountIncrements( DaoValue **values, daoint size )
{
	daoint i;
	for(i=0; i<size; i++) cycRefCountIncrement( values[i] );
}
void DaoGC_RefCountDecrements( DaoValue **values, daoint size )
{
	daoint i;
	if( gcWorker.concurrent ) DMutex_Lock( & gcWorker.mutex_idle_list );
	for(i=0; i<size; i++){
		DaoValue *p = values[i];
		if( p == NULL ) continue;
		p->xGC.refCount --;
		if( p->xGC.refCount == 0 && p->type < DAO_ENUM ) DaoGC_DeleteSimpleData( p );
		values[i] = 0;
	}
	if( gcWorker.concurrent ) DMutex_Unlock( & gcWorker.mutex_idle_list );
}
void cycRefCountDecrements( DList *list )
{
	if( list == NULL ) return;
	DaoGC_LockData();
	DaoGC_CycRefCountDecrements( list->items.pValue, list->size );
	DaoGC_UnlockData();
}
void cycRefCountIncrements( DList *list )
{
	if( list == NULL ) return;
	DaoGC_LockData();
	DaoGC_CycRefCountIncrements( list->items.pValue, list->size );
	DaoGC_UnlockData();
}
void directRefCountDecrement( DaoValue **value )
{
	DaoValue *p = *value;
	if( p == NULL ) return;
	if( gcWorker.concurrent ) DMutex_Lock( & gcWorker.mutex_idle_list );
	p->xGC.refCount --;
	*value = NULL;
	if( p->xGC.refCount == 0 && p->type < DAO_ENUM ) DaoGC_DeleteSimpleData( p );
	if( gcWorker.concurrent ) DMutex_Unlock( & gcWorker.mutex_idle_list );
}
void directRefCountDecrements( DList *list )
{
	if( list == NULL ) return;
	DaoGC_LockData();
	DaoGC_RefCountDecrements( list->items.pValue, list->size );
	list->size = 0;
	DaoGC_UnlockData();
}

static int DaoGC_CycRefCountDecScan( DaoValue *value )
{
	int count = 1;
	switch( value->type ){
	case DAO_ENUM :
		{
			DaoEnum *en = (DaoEnum*) value;
			cycRefCountDecrement( (DaoValue*) en->etype );
			break;
		}
	case DAO_CONSTANT :
		{
			cycRefCountDecrement( value->xConst.value );
			break;
		}
	case DAO_VARIABLE :
		{
			cycRefCountDecrement( value->xVar.value );
			cycRefCountDecrement( (DaoValue*) value->xVar.dtype );
			break;
		}
	case DAO_PAR_NAMED :
		{
			cycRefCountDecrement( value->xNameValue.value );
			cycRefCountDecrement( (DaoValue*) value->xNameValue.ctype );
			break;
		}
#ifdef DAO_WITH_NUMARRAY
	case DAO_ARRAY :
		{
			DaoArray *array = (DaoArray*) value;
			cycRefCountDecrement( (DaoValue*) array->original );
			break;
		}
#endif
	case DAO_TUPLE :
		{
			DaoTuple *tuple = (DaoTuple*) value;
			cycRefCountDecrement( (DaoValue*) tuple->ctype );
			if( tuple->ctype == NULL || tuple->ctype->noncyclic ==0 ){
				DaoGC_CycRefCountDecrements( tuple->values, tuple->size );
				count += tuple->size;
			}
			break;
		}
	case DAO_LIST :
		{
			DaoList *list = (DaoList*) value;
			cycRefCountDecrement( (DaoValue*) list->ctype );
			if( list->ctype == NULL || list->ctype->noncyclic ==0 ){
				cycRefCountDecrements( list->value );
				count += list->value->size;
			}
			break;
		}
	case DAO_MAP :
		{
			DaoMap *map = (DaoMap*) value;
			cycRefCountDecrement( (DaoValue*) map->ctype );
			count += DaoGC_ScanMap( map->value, DAO_GC_DEC, 1, 1 );
			break;
		}
	case DAO_OBJECT :
		{
			DaoObject *obj = (DaoObject*) value;
			if( obj->isRoot ){
				DaoGC_CycRefCountDecrements( obj->objValues, obj->valueCount );
				count += obj->valueCount;
			}
			cycRefCountDecrement( (DaoValue*) obj->parent );
			cycRefCountDecrement( (DaoValue*) obj->rootObject );
			cycRefCountDecrement( (DaoValue*) obj->defClass );
			break;
		}
	case DAO_CSTRUCT : case DAO_CDATA : case DAO_CTYPE :
		{
			DaoCdata *cdata = (DaoCdata*) value;
			cycRefCountDecrement( (DaoValue*) cdata->object );
			cycRefCountDecrement( (DaoValue*) cdata->ctype );
			if( value->type == DAO_CDATA || value->type == DAO_CSTRUCT ){
				DaoGC_ScanCdata( cdata, DAO_GC_DEC );
			}else{
				cycRefCountDecrement( (DaoValue*) value->xCtype.cdtype );
			}
			break;
		}
	case DAO_ROUTINE :
		{
			DaoRoutine *rout = (DaoRoutine*)value;
			cycRefCountDecrement( (DaoValue*) rout->routType );
			cycRefCountDecrement( (DaoValue*) rout->routHost );
			cycRefCountDecrement( (DaoValue*) rout->nameSpace );
			cycRefCountDecrement( (DaoValue*) rout->original );
			cycRefCountDecrement( (DaoValue*) rout->routConsts );
			cycRefCountDecrement( (DaoValue*) rout->body );
			if( rout->overloads ) cycRefCountDecrements( rout->overloads->array );
			if( rout->specialized ) cycRefCountDecrements( rout->specialized->array );
			break;
		}
	case DAO_ROUTBODY :
		{
			DaoRoutineBody *rout = (DaoRoutineBody*)value;
			count += rout->regType->size;
			count += rout->upValues ? rout->upValues->size : 0;
			cycRefCountDecrements( rout->regType );
			cycRefCountDecrements( rout->upValues );
			break;
		}
	case DAO_CLASS :
		{
			DaoClass *klass = (DaoClass*)value;
			cycRefCountDecrement( (DaoValue*) klass->clsType );
			cycRefCountDecrement( (DaoValue*) klass->castOperators );
			cycRefCountDecrement( (DaoValue*) klass->initRoutine );
			cycRefCountDecrements( klass->constants );
			cycRefCountDecrements( klass->variables );
			cycRefCountDecrements( klass->instvars );
			cycRefCountDecrements( klass->allBases );
			cycRefCountDecrements( klass->references );
			count += klass->constants->size + klass->variables->size + klass->instvars->size;
			count += klass->allBases->size + klass->references->size;
			break;
		}
	case DAO_INTERFACE :
		{
			DaoInterface *inter = (DaoInterface*)value;
			cycRefCountDecrements( inter->supers );
			cycRefCountDecrement( (DaoValue*) inter->abtype );
			count += DaoGC_ScanMap( inter->concretes, DAO_GC_DEC, 0, 1 );
			count += DaoGC_ScanMap( inter->methods, DAO_GC_DEC, 0, 1 );
			count += inter->supers->size;
			break;
		}
	case DAO_CINTYPE :
		{
			DaoCinType *cintype = (DaoCinType*)value;
			cycRefCountDecrements( cintype->supers );
			cycRefCountDecrement( (DaoValue*) cintype->citype );
			cycRefCountDecrement( (DaoValue*) cintype->vatype );
			cycRefCountDecrement( (DaoValue*) cintype->target );
			cycRefCountDecrement( (DaoValue*) cintype->abstract );
			count += DaoGC_ScanMap( cintype->methods, DAO_GC_DEC, 0, 1 );
			count += cintype->supers->size;
			break;
		}
	case DAO_CINVALUE :
		{
			cycRefCountDecrement( value->xCinValue.value );
			cycRefCountDecrement( (DaoValue*) value->xCinValue.cintype );
			break;
		}
	case DAO_NAMESPACE :
		{
			DaoNamespace *ns = (DaoNamespace*) value;
			cycRefCountDecrements( ns->constants );
			cycRefCountDecrements( ns->variables );
			cycRefCountDecrements( ns->auxData );
			cycRefCountDecrements( ns->mainRoutines );
			count += DaoGC_ScanMap( ns->abstypes, DAO_GC_DEC, 0, 1 );
			count += ns->constants->size + ns->variables->size;
			count += ns->auxData->size + ns->mainRoutines->size;
			break;
		}
	case DAO_TYPE :
		{
			DaoType *abtp = (DaoType*) value;
			cycRefCountDecrement( abtp->aux );
			cycRefCountDecrement( abtp->value );
			cycRefCountDecrement( (DaoValue*) abtp->kernel );
			cycRefCountDecrement( (DaoValue*) abtp->cbtype );
			cycRefCountDecrement( (DaoValue*) abtp->quadtype );
			cycRefCountDecrements( abtp->nested );
			cycRefCountDecrements( abtp->bases );
			count += DaoGC_ScanMap( abtp->interfaces, DAO_GC_DEC, 1, 0 );
			break;
		}
	case DAO_TYPEKERNEL :
		{
			DaoTypeKernel *kernel = (DaoTypeKernel*) value;
			cycRefCountDecrement( (DaoValue*) kernel->abtype );
			cycRefCountDecrement( (DaoValue*) kernel->nspace );
			cycRefCountDecrement( (DaoValue*) kernel->initRoutines );
			cycRefCountDecrement( (DaoValue*) kernel->castOperators );
			count += DaoGC_ScanMap( kernel->values, DAO_GC_DEC, 0, 1 );
			count += DaoGC_ScanMap( kernel->methods, DAO_GC_DEC, 0, 1 );
			if( kernel->sptree ){
				cycRefCountDecrements( kernel->sptree->holders );
				cycRefCountDecrements( kernel->sptree->defaults );
				cycRefCountDecrements( kernel->sptree->sptypes );
			}
			break;
		}
	case DAO_PROCESS :
		{
			DaoProcess *vmp = (DaoProcess*) value;
			DaoStackFrame *frame = vmp->firstFrame;
			cycRefCountDecrement( (DaoValue*) vmp->future );
			cycRefCountDecrements( vmp->exceptions );
			cycRefCountDecrements( vmp->defers );
			cycRefCountDecrements( vmp->factory );
			DaoGC_CycRefCountDecrements( vmp->stackValues, vmp->stackSize );
			count += vmp->stackSize;
			while( frame ){
				count += 3;
				cycRefCountDecrement( (DaoValue*) frame->routine );
				cycRefCountDecrement( (DaoValue*) frame->object );
				cycRefCountDecrement( (DaoValue*) frame->retype );
				frame = frame->next;
			}
			break;
		}
	default: break;
	}
	return count;
}
static int DaoGC_CycRefCountIncScan( DaoValue *value )
{
	int count = 1;
	switch( value->type ){
	case DAO_ENUM :
		{
			DaoEnum *en = (DaoEnum*) value;
			cycRefCountIncrement( (DaoValue*) en->etype );
			break;
		}
	case DAO_CONSTANT :
		{
			cycRefCountIncrement( value->xConst.value );
			break;
		}
	case DAO_VARIABLE :
		{
			cycRefCountIncrement( value->xVar.value );
			cycRefCountIncrement( (DaoValue*) value->xVar.dtype );
			break;
		}
	case DAO_PAR_NAMED :
		{
			cycRefCountIncrement( value->xNameValue.value );
			cycRefCountIncrement( (DaoValue*) value->xNameValue.ctype );
			break;
		}
#ifdef DAO_WITH_NUMARRAY
	case DAO_ARRAY :
		{
			DaoArray *array = (DaoArray*) value;
			cycRefCountIncrement( (DaoValue*) array->original );
			break;
		}
#endif
	case DAO_TUPLE :
		{
			DaoTuple *tuple= (DaoTuple*) value;
			cycRefCountIncrement( (DaoValue*) tuple->ctype );
			if( tuple->ctype == NULL || tuple->ctype->noncyclic ==0 ){
				DaoGC_CycRefCountIncrements( tuple->values, tuple->size );
				count += tuple->size;
			}
			break;
		}
	case DAO_LIST :
		{
			DaoList *list= (DaoList*) value;
			cycRefCountIncrement( (DaoValue*) list->ctype );
			if( list->ctype == NULL || list->ctype->noncyclic ==0 ){
				cycRefCountIncrements( list->value );
				count += list->value->size;
			}
			break;
		}
	case DAO_MAP :
		{
			DaoMap *map = (DaoMap*)value;
			cycRefCountIncrement( (DaoValue*) map->ctype );
			count += DaoGC_ScanMap( map->value, DAO_GC_INC, 1, 1 );
			break;
		}
	case DAO_OBJECT :
		{
			DaoObject *obj = (DaoObject*) value;
			if( obj->isRoot ){
				DaoGC_CycRefCountIncrements( obj->objValues, obj->valueCount );
				count += obj->valueCount;
			}
			cycRefCountIncrement( (DaoValue*) obj->parent );
			cycRefCountIncrement( (DaoValue*) obj->rootObject );
			cycRefCountIncrement( (DaoValue*) obj->defClass );
			break;
		}
	case DAO_CSTRUCT : case DAO_CDATA : case DAO_CTYPE :
		{
			DaoCdata *cdata = (DaoCdata*) value;
			cycRefCountIncrement( (DaoValue*) cdata->object );
			cycRefCountIncrement( (DaoValue*) cdata->ctype );
			if( value->type == DAO_CDATA || value->type == DAO_CSTRUCT ){
				DaoGC_ScanCdata( cdata, DAO_GC_INC );
			}else{
				cycRefCountIncrement( (DaoValue*) value->xCtype.cdtype );
			}
			break;
		}
	case DAO_ROUTINE :
		{
			DaoRoutine *rout = (DaoRoutine*) value;
			cycRefCountIncrement( (DaoValue*) rout->routType );
			cycRefCountIncrement( (DaoValue*) rout->routHost );
			cycRefCountIncrement( (DaoValue*) rout->nameSpace );
			cycRefCountIncrement( (DaoValue*) rout->original );
			cycRefCountIncrement( (DaoValue*) rout->routConsts );
			cycRefCountIncrement( (DaoValue*) rout->body );
			if( rout->overloads ) cycRefCountIncrements( rout->overloads->array );
			if( rout->specialized ) cycRefCountIncrements( rout->specialized->array );
			break;
		}
	case DAO_ROUTBODY :
		{
			DaoRoutineBody *rout = (DaoRoutineBody*)value;
			count += rout->regType->size;
			count += rout->upValues ? rout->upValues->size : 0;
			cycRefCountIncrements( rout->regType );
			cycRefCountIncrements( rout->upValues );
			break;
		}
	case DAO_CLASS :
		{
			DaoClass *klass = (DaoClass*) value;
			cycRefCountIncrement( (DaoValue*) klass->clsType );
			cycRefCountIncrement( (DaoValue*) klass->castOperators );
			cycRefCountIncrement( (DaoValue*) klass->initRoutine );
			cycRefCountIncrements( klass->constants );
			cycRefCountIncrements( klass->variables );
			cycRefCountIncrements( klass->instvars );
			cycRefCountIncrements( klass->allBases );
			cycRefCountIncrements( klass->references );
			count += klass->constants->size + klass->variables->size + klass->instvars->size;
			count += klass->allBases->size + klass->references->size;
			break;
		}
	case DAO_INTERFACE :
		{
			DaoInterface *inter = (DaoInterface*)value;
			cycRefCountIncrements( inter->supers );
			cycRefCountIncrement( (DaoValue*) inter->abtype );
			count += DaoGC_ScanMap( inter->concretes, DAO_GC_INC, 0, 1 );
			count += DaoGC_ScanMap( inter->methods, DAO_GC_INC, 0, 1 );
			count += inter->supers->size;
			break;
		}
	case DAO_CINTYPE :
		{
			DaoCinType *cintype = (DaoCinType*)value;
			cycRefCountIncrements( cintype->supers );
			cycRefCountIncrement( (DaoValue*) cintype->citype );
			cycRefCountIncrement( (DaoValue*) cintype->vatype );
			cycRefCountIncrement( (DaoValue*) cintype->target );
			cycRefCountIncrement( (DaoValue*) cintype->abstract );
			count += DaoGC_ScanMap( cintype->methods, DAO_GC_INC, 0, 1 );
			count += cintype->supers->size;
			break;
		}
	case DAO_CINVALUE :
		{
			cycRefCountIncrement( value->xCinValue.value );
			cycRefCountIncrement( (DaoValue*) value->xCinValue.cintype );
			break;
		}
	case DAO_NAMESPACE :
		{
			DaoNamespace *ns = (DaoNamespace*) value;
			cycRefCountIncrements( ns->constants );
			cycRefCountIncrements( ns->variables );
			cycRefCountIncrements( ns->auxData );
			cycRefCountIncrements( ns->mainRoutines );
			count += DaoGC_ScanMap( ns->abstypes, DAO_GC_INC, 0, 1 );
			count += ns->constants->size + ns->variables->size;
			count += ns->auxData->size + ns->mainRoutines->size;
			break;
		}
	case DAO_TYPE :
		{
			DaoType *abtp = (DaoType*) value;
			cycRefCountIncrement( abtp->aux );
			cycRefCountIncrement( abtp->value );
			cycRefCountIncrement( (DaoValue*) abtp->kernel );
			cycRefCountIncrement( (DaoValue*) abtp->cbtype );
			cycRefCountIncrement( (DaoValue*) abtp->quadtype );
			cycRefCountIncrements( abtp->nested );
			cycRefCountIncrements( abtp->bases );
			count += DaoGC_ScanMap( abtp->interfaces, DAO_GC_INC, 1, 0 );
			break;
		}
	case DAO_TYPEKERNEL :
		{
			DaoTypeKernel *kernel = (DaoTypeKernel*) value;
			cycRefCountIncrement( (DaoValue*) kernel->abtype );
			cycRefCountIncrement( (DaoValue*) kernel->nspace );
			cycRefCountIncrement( (DaoValue*) kernel->initRoutines );
			cycRefCountIncrement( (DaoValue*) kernel->castOperators );
			count += DaoGC_ScanMap( kernel->values, DAO_GC_INC, 0, 1 );
			count += DaoGC_ScanMap( kernel->methods, DAO_GC_INC, 0, 1 );
			if( kernel->sptree ){
				cycRefCountIncrements( kernel->sptree->holders );
				cycRefCountIncrements( kernel->sptree->defaults );
				cycRefCountIncrements( kernel->sptree->sptypes );
			}
			break;
		}
	case DAO_PROCESS :
		{
			DaoProcess *vmp = (DaoProcess*) value;
			DaoStackFrame *frame = vmp->firstFrame;
			cycRefCountIncrement( (DaoValue*) vmp->future );
			cycRefCountIncrements( vmp->exceptions );
			cycRefCountIncrements( vmp->defers );
			cycRefCountIncrements( vmp->factory );
			DaoGC_CycRefCountIncrements( vmp->stackValues, vmp->stackSize );
			count += vmp->stackSize;
			while( frame ){
				count += 3;
				cycRefCountIncrement( (DaoValue*) frame->routine );
				cycRefCountIncrement( (DaoValue*) frame->object );
				cycRefCountIncrement( (DaoValue*) frame->retype );
				frame = frame->next;
			}
			break;
		}
	default: break;
	}
	return count;
}
static int DaoGC_RefCountDecScan( DaoValue *value )
{
	int count = 1;
	switch( value->type ){
	case DAO_ENUM :
		{
			DaoEnum *en = (DaoEnum*) value;
			directRefCountDecrement( (DaoValue**) & en->etype );
			break;
		}
	case DAO_CONSTANT :
		{
			directRefCountDecrement( & value->xConst.value );
			break;
		}
	case DAO_VARIABLE :
		{
			directRefCountDecrement( & value->xVar.value );
			directRefCountDecrement( (DaoValue**) & value->xVar.dtype );
			break;
		}
	case DAO_PAR_NAMED :
		{
			directRefCountDecrement( & value->xNameValue.value );
			directRefCountDecrement( (DaoValue**) & value->xNameValue.ctype );
			break;
		}
#ifdef DAO_WITH_NUMARRAY
	case DAO_ARRAY :
		{
			DaoArray *array = (DaoArray*) value;
			directRefCountDecrement( (DaoValue**) & array->original );
			break;
		}
#endif
	case DAO_TUPLE :
		{
			DaoTuple *tuple = (DaoTuple*) value;
			count += tuple->size;
			directRefCountDecrement( (DaoValue**) & tuple->ctype );
			DaoGC_RefCountDecrements( tuple->values, tuple->size );
			tuple->size = 0;
			break;
		}
	case DAO_LIST :
		{
			DaoList *list = (DaoList*) value;
			count += list->value->size;
			directRefCountDecrements( list->value );
			directRefCountDecrement( (DaoValue**) & list->ctype );
			break;
		}
	case DAO_MAP :
		{
			DaoMap *map = (DaoMap*) value;
			count += DaoGC_ScanMap( map->value, DAO_GC_BREAK, 1, 1 );
			directRefCountDecrement( (DaoValue**) & map->ctype );
			break;
		}
	case DAO_OBJECT :
		{
			DaoObject *obj = (DaoObject*) value;
			if( obj->isRoot ){
				DaoGC_RefCountDecrements( obj->objValues, obj->valueCount );
				count += obj->valueCount;
			}
			directRefCountDecrement( (DaoValue**) & obj->parent );
			directRefCountDecrement( (DaoValue**) & obj->rootObject );
			directRefCountDecrement( (DaoValue**) & obj->defClass );
			obj->valueCount = 0;
			break;
		}
	case DAO_CSTRUCT : case DAO_CDATA : case DAO_CTYPE :
		{
			DaoCdata *cdata = (DaoCdata*) value;
			DaoType *ctype = cdata->ctype;
			directRefCountDecrement( (DaoValue**) & cdata->object );
			directRefCountDecrement( (DaoValue**) & cdata->ctype );
			cdata->ctype = ctype;
			cdata->trait |= DAO_VALUE_BROKEN;
			if( value->type == DAO_CDATA || value->type == DAO_CSTRUCT ){
				DaoGC_ScanCdata( cdata, DAO_GC_BREAK );
			}else{
				directRefCountDecrement( (DaoValue**) & value->xCtype.cdtype );
			}
			DaoWrappers_Erase( cdata->data );
			break;
		}
	case DAO_ROUTINE :
		{
			DaoRoutine *rout = (DaoRoutine*)value;
			directRefCountDecrement( (DaoValue**) & rout->nameSpace );
			/* may become NULL, if it has already become garbage
			 * in the last cycle */
			directRefCountDecrement( (DaoValue**) & rout->routType );
			/* may become NULL, if it has already become garbage
			 * in the last cycle */
			directRefCountDecrement( (DaoValue**) & rout->routHost );
			directRefCountDecrement( (DaoValue**) & rout->original );
			directRefCountDecrement( (DaoValue**) & rout->routConsts );
			directRefCountDecrement( (DaoValue**) & rout->body );
			if( rout->overloads ) directRefCountDecrements( rout->overloads->array );
			if( rout->specialized ) directRefCountDecrements( rout->specialized->array );
			break;
		}
	case DAO_ROUTBODY :
		{
			DaoRoutineBody *rout = (DaoRoutineBody*)value;
			count += rout->regType->size;
			count += rout->upValues ? rout->upValues->size : 0;
			directRefCountDecrements( rout->regType );
			directRefCountDecrements( rout->upValues );
			break;
		}
	case DAO_CLASS :
		{
			DaoClass *klass = (DaoClass*)value;
			count += klass->constants->size + klass->variables->size + klass->instvars->size;
			count += klass->allBases->size + klass->references->size;
			directRefCountDecrement( (DaoValue**) & klass->clsType );
			directRefCountDecrement( (DaoValue**) & klass->castOperators );
			directRefCountDecrement( (DaoValue**) & klass->initRoutine );
			directRefCountDecrements( klass->constants );
			directRefCountDecrements( klass->variables );
			directRefCountDecrements( klass->instvars );
			directRefCountDecrements( klass->allBases );
			directRefCountDecrements( klass->references );
			break;
		}
	case DAO_INTERFACE :
		{
			DaoInterface *inter = (DaoInterface*)value;
			directRefCountDecrements( inter->supers );
			directRefCountDecrement( (DaoValue**) & inter->abtype );
			count += DaoGC_ScanMap( inter->concretes, DAO_GC_BREAK, 0, 1 );
			count += DaoGC_ScanMap( inter->methods, DAO_GC_BREAK, 0, 1 );
			count += inter->supers->size;
			break;
		}
	case DAO_CINTYPE :
		{
			DaoCinType *cintype = (DaoCinType*)value;
			directRefCountDecrements( cintype->supers );
			directRefCountDecrement( (DaoValue**) & cintype->citype );
			directRefCountDecrement( (DaoValue**) & cintype->vatype );
			directRefCountDecrement( (DaoValue**) & cintype->target );
			directRefCountDecrement( (DaoValue**) & cintype->abstract );
			count += DaoGC_ScanMap( cintype->methods, DAO_GC_BREAK, 0, 1 );
			count += cintype->supers->size;
			break;
		}
	case DAO_CINVALUE :
		{
			DaoWrappers_Erase( value->xCinValue.value );
			directRefCountDecrement( & value->xCinValue.value );
			directRefCountDecrement( (DaoValue**) & value->xCinValue.cintype );
			break;
		}
	case DAO_NAMESPACE :
		{
			DaoNamespace *ns = (DaoNamespace*) value;
			count += ns->auxData->size + ns->mainRoutines->size;
			count += ns->constants->size + ns->variables->size;
			count += DaoGC_ScanMap( ns->abstypes, DAO_GC_BREAK, 0, 1 );
			directRefCountDecrements( ns->constants );
			directRefCountDecrements( ns->variables );
			directRefCountDecrements( ns->auxData );
			directRefCountDecrements( ns->mainRoutines );
			break;
		}
	case DAO_TYPE :
		{
			DaoType *abtp = (DaoType*) value;
			directRefCountDecrements( abtp->nested );
			directRefCountDecrements( abtp->bases );
			directRefCountDecrement( (DaoValue**) & abtp->aux );
			directRefCountDecrement( (DaoValue**) & abtp->value );
			directRefCountDecrement( (DaoValue**) & abtp->kernel );
			directRefCountDecrement( (DaoValue**) & abtp->cbtype );
			directRefCountDecrement( (DaoValue**) & abtp->quadtype );
			count += DaoGC_ScanMap( abtp->interfaces, DAO_GC_BREAK, 1, 0 );
			break;
		}
	case DAO_TYPEKERNEL :
		{
			DaoTypeKernel *kernel = (DaoTypeKernel*) value;
			directRefCountDecrement( (DaoValue**) & kernel->abtype );
			directRefCountDecrement( (DaoValue**) & kernel->nspace );
			directRefCountDecrement( (DaoValue**) & kernel->initRoutines );
			directRefCountDecrement( (DaoValue**) & kernel->castOperators );
			count += DaoGC_ScanMap( kernel->values, DAO_GC_BREAK, 0, 1 );
			count += DaoGC_ScanMap( kernel->methods, DAO_GC_BREAK, 0, 1 );
			if( kernel->sptree ){
				directRefCountDecrements( kernel->sptree->holders );
				directRefCountDecrements( kernel->sptree->defaults );
				directRefCountDecrements( kernel->sptree->sptypes );
			}
			break;
		}
	case DAO_PROCESS :
		{
			DaoProcess *vmp = (DaoProcess*) value;
			DaoStackFrame *frame = vmp->firstFrame;
			directRefCountDecrement( (DaoValue**) & vmp->future );
			directRefCountDecrements( vmp->exceptions );
			directRefCountDecrements( vmp->defers );
			directRefCountDecrements( vmp->factory );
			DaoGC_RefCountDecrements( vmp->stackValues, vmp->stackSize );
			count += vmp->stackSize;
			vmp->stackSize = 0;
			while( frame ){
				count += 3;
				if( gcWorker.concurrent ) DMutex_Lock( & gcWorker.mutex_idle_list );
				if( frame->routine ) frame->routine->refCount --;
				if( frame->object ) frame->object->refCount --;
				if( frame->retype ) frame->retype->refCount --;
				frame->routine = NULL;
				frame->object = NULL;
				frame->retype = NULL;
				if( gcWorker.concurrent ) DMutex_Unlock( & gcWorker.mutex_idle_list );
				frame = frame->next;
			}
			break;
		}
	default: break;
	}
	return count;
}


