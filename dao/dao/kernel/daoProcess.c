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
#include<assert.h>
#include<ctype.h>
#include<math.h>

#include"daoProcess.h"
#include"daoGC.h"
#include"daoStdlib.h"
#include"daoClass.h"
#include"daoObject.h"
#include"daoRoutine.h"
#include"daoVmspace.h"
#include"daoNamespace.h"
#include"daoNumtype.h"
#include"daoRegex.h"
#include"daoStream.h"
#include"daoParser.h"
#include"daoValue.h"
#include"daoTasklet.h"


extern DMutex mutex_routine_specialize;
extern DMutex mutex_routine_specialize2;

struct DaoJIT dao_jit = { NULL, NULL, NULL, NULL };


DaoTuple* DaoProcess_GetTuple( DaoProcess *self, DaoType *type, int size, int init );
static DaoArray* DaoProcess_GetArray( DaoProcess *self, DaoVmCode *vmc );
static DaoList* DaoProcess_GetList( DaoProcess *self, DaoVmCode *vmc );
static DaoMap* DaoProcess_GetMap( DaoProcess *self, DaoVmCode *vmc, unsigned int hashing );

static void DaoProcess_DoMap( DaoProcess *self, DaoVmCode *vmc );
static void DaoProcess_DoList( DaoProcess *self, DaoVmCode *vmc );
static void DaoProcess_DoPair( DaoProcess *self, DaoVmCode *vmc );
static void DaoProcess_DoTuple( DaoProcess *self, DaoVmCode *vmc );
static void DaoProcess_DoTupleSim( DaoProcess *self, DaoVmCode *vmc );
static void DaoProcess_DoVector( DaoProcess *self, DaoVmCode *vmc );
static void DaoProcess_DoMatrix( DaoProcess *self, DaoVmCode *vmc );
static void DaoProcess_DoAPList(  DaoProcess *self, DaoVmCode *vmc );
static void DaoProcess_DoAPVector( DaoProcess *self, DaoVmCode *vmc );
static void DaoProcess_DoPacking( DaoProcess *self, DaoVmCode *vmc );
static void DaoProcess_DoCheckSame( DaoProcess *self, DaoVmCode *vmc );
static void DaoProcess_DoCheckIsa( DaoProcess *self, DaoVmCode *vmc );
static void DaoProcess_BindNameValue( DaoProcess *self, DaoVmCode *vmc );

static void DaoProcess_DoGetItem( DaoProcess *self, DaoVmCode *vmc );
static void DaoProcess_DoSetItem( DaoProcess *self, DaoVmCode *vmc );
static void DaoProcess_DoGetField( DaoProcess *self, DaoVmCode *vmc );
static void DaoProcess_DoSetField( DaoProcess *self, DaoVmCode *vmc );

static void DaoProcess_DoIter( DaoProcess *self, DaoVmCode *vmc );
static void DaoProcess_DoInTest( DaoProcess *self, DaoVmCode *vmc );
static void DaoProcess_DoBinArith( DaoProcess *self, DaoVmCode *vmc );
static void DaoProcess_DoBinBool(  DaoProcess *self, DaoVmCode *vmc );
static void DaoProcess_DoUnaArith( DaoProcess *self, DaoVmCode *vmc );
static void DaoProcess_DoUnaBool( DaoProcess *self, DaoVmCode *vmc );
static void DaoProcess_DoBitLogic( DaoProcess *self, DaoVmCode *vmc );
static void DaoProcess_DoBitShift( DaoProcess *self, DaoVmCode *vmc );
static void DaoProcess_DoBitFlip( DaoProcess *self, DaoVmCode *vmc );
static void DaoProcess_DoBitFlip( DaoProcess *self, DaoVmCode *vmc );

static void DaoProcess_DoCast( DaoProcess *self, DaoVmCode *vmc );
static void DaoProcess_DoCall( DaoProcess *self, DaoVmCode *vmc );

static void DaoProcess_RaiseTypeError( DaoProcess *self, DaoType *from, DaoType *to, const char *op );

static void DaoProcess_MakeRoutine( DaoProcess *self, DaoVmCode *vmc );

static DaoVmCode* DaoProcess_DoSwitch( DaoProcess *self, DaoVmCode *vmc );
static DaoValue* DaoProcess_DoReturn( DaoProcess *self, DaoVmCode *vmc );
static int DaoVM_DoMath( DaoProcess *self, DaoVmCode *vmc, DaoValue *C, DaoValue *A );

static int DaoProcess_TryUserArith( DaoProcess *self, DaoValue *A, DaoValue *B, DaoValue *C, DaoType *TA, DaoType *TB );

int DaoArray_number_op_array( DaoArray *C, DaoValue *A, DaoArray *B, short op, DaoProcess *ctx );
int DaoArray_array_op_number( DaoArray *C, DaoArray *A, DaoValue *B, short op, DaoProcess *ctx );
int DaoArray_ArrayArith( DaoArray *s, DaoArray *l, DaoArray *r, short p, DaoProcess *c );
void DaoProcess_ShowCallError( DaoProcess *self, DaoRoutine *rout, DaoValue *selfobj, DaoValue *ps[], int np, int callmode );


static DaoStackFrame* DaoStackFrame_New()
{
	DaoStackFrame *self = (DaoStackFrame*) dao_calloc( 1, sizeof(DaoStackFrame) );
	return self;
}
#define DaoStackFrame_Delete( p ) dao_free( p )

DaoTypeBase vmpTyper =
{
	"process",
	& baseCore, NULL, NULL, {0}, {0},
	(FuncPtrDel) DaoProcess_Delete, NULL
};

static DaoType  *dummyType = NULL;
static DaoVmCode dummyCode = {0,0,0,0};
static DaoVmCode dummyCallCode = {DVM_CALL,0,0,0};

DaoProcess* DaoProcess_New( DaoVmSpace *vms )
{
	int i;
	DaoProcess *self = (DaoProcess*)dao_calloc( 1, sizeof( DaoProcess ) );
	DaoValue_Init( self, DAO_PROCESS );
	self->trait |= DAO_VALUE_DELAYGC;
	self->vmSpace = vms;
	self->status = DAO_PROCESS_SUSPENDED;
	self->exceptions = DList_New( DAO_DATA_VALUE );
	self->defers = DList_New( DAO_DATA_VALUE );

	self->firstFrame = self->baseFrame = self->topFrame = DaoStackFrame_New();
	self->firstFrame->active = self->firstFrame;
	self->firstFrame->types = & dummyType;
	self->firstFrame->codes = & dummyCode;
	self->firstFrame->entry = 1;
	self->stackSize = self->stackTop = 1 + DAO_MAX_PARAM;
	self->stackValues = (DaoValue**)dao_calloc( self->stackSize, sizeof(DaoValue*) );
	self->paramValues = self->stackValues + 1;
	self->factory = DList_New( DAO_DATA_VALUE );

	self->string = DString_New();
	self->list = DList_New(0);
	self->pauseType = 0;
	self->active = 0;
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogNew( (DaoValue*) self );
#endif
	return self;
}

void DaoProcess_Delete( DaoProcess *self )
{
	DaoStackFrame *frame = self->firstFrame;
	daoint i;
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogDelete( (DaoValue*) self );
#endif
	while( frame ){
		DaoStackFrame *p = frame;
		if( frame->object ) GC_DecRC( frame->object );
		if( frame->routine ) GC_DecRC( frame->routine );
		frame = frame->next;
		dao_free( p );
	}
	for(i=0; i<self->stackSize; i++) GC_DecRC( self->stackValues[i] );
	if( self->stackValues ) dao_free( self->stackValues );

	DString_Delete( self->string );
	DList_Delete( self->list );
	DList_Delete( self->exceptions );
	DList_Delete( self->defers );
	if( self->future ) GC_DecRC( self->future );
	if( self->factory ) DList_Delete( self->factory );
	if( self->aux ) DaoAux_Delete( self->aux );
	dao_free( self );
}


DaoStackFrame* DaoProcess_PushFrame( DaoProcess *self, int size )
{
	daoint i, N = self->stackTop + size;
	DaoStackFrame *f, *frame = self->topFrame->next;
	DaoProfiler *profiler = self->vmSpace->profiler;

	if( profiler && self->topFrame ) profiler->LeaveFrame( profiler, self, self->topFrame, 0 );

	if( N > self->stackSize ){
		daoint offset = self->activeValues - self->stackValues;
		self->stackValues = (DaoValue**)dao_realloc( self->stackValues, N*sizeof(DaoValue*) );
		self->paramValues = self->stackValues + 1;
		memset( self->stackValues + self->stackSize, 0, (N-self->stackSize)*sizeof(DaoValue*) );
		if( self->activeValues ) self->activeValues = self->stackValues +  offset;
		self->stackSize = N;
	}
	if( frame == NULL ){
		frame = DaoStackFrame_New();
		frame->prev = self->topFrame;
		self->topFrame->next = frame;
	}

	/*
	// Each stack frame uses ::varCount number of local variables that are allocated
	// on the stack starting from ::stackBase. DaoProcess_InitTopFrame() may check
	// if the routine to be called is the same as the previous one called on this
	// frame, if yes, it will assume these variables initialized and used by the
	// previous call can be reused without re-initialization.
	//
	// Here it checks if the frame has the right stack offset and variable count,
	// if no, unset ::routine to force DaoProcess_InitTopFrame() redo the
	// initialization.
	//
	// A frame that is invalidated by previous frames will have its ::varCount set
	// to zero, so that this checking will always be sucessful (if size!=0).
	*/
	if( frame->routine && (frame->stackBase != self->stackTop || frame->varCount != size) ){
		GC_DecRC( frame->routine );
		frame->routine = NULL;
	}
	frame->process = self;
	frame->host = NULL;
	frame->stackBase = self->stackTop;
	frame->varCount = size;
	frame->entry = 0;
	frame->state = 0;
	frame->returning = 0xffff;
	frame->parCount = 0;
	frame->deferBase = self->defers->size;
	frame->exceptBase = self->exceptions->size;
	if( self->topFrame->routine && self->topFrame->routine->body && self->activeCode ){
		self->topFrame->entry = (int)(self->activeCode - self->topFrame->codes) + 1;
		frame->returning = self->activeCode->c;
	}
	self->topFrame = frame;
	self->stackTop += size;

	/*
	// Check and reset frames that have the stack values invalidated for reusing.
	// A frame is invalidated if the range of its stack values is partially covered
	// by this frame.
	*/
	f = frame->next;
	while( f && f->stackBase < self->stackTop ){
		f->stackBase = self->stackTop;
		f->varCount = 0; /* To make sure this frame is re-initialized; */
		f = f->next;
	}
	return frame;
}
void DaoProcess_PopFrame( DaoProcess *self )
{
	int att = 0;
	DaoProfiler *profiler = self->vmSpace->profiler;

	if( self->topFrame == NULL ) return;
	if( profiler ){
		profiler->LeaveFrame( profiler, self, self->topFrame, 1 );
		if( self->topFrame->prev ) profiler->EnterFrame( profiler, self, self->topFrame->prev, 0 );
	}

	if( self->topFrame->routine ){
		att = self->topFrame->routine->attribs;
		if( !(self->topFrame->routine->attribs & DAO_ROUT_REUSABLE) ){
			GC_DecRC( self->topFrame->routine );
			self->topFrame->routine = NULL;
		}
	}
	self->topFrame->outer = NULL;
	GC_DecRC( self->topFrame->retype );
	GC_DecRC( self->topFrame->object );
	self->topFrame->retype = NULL;
	self->topFrame->object = NULL;
	if( self->topFrame->state & DVM_FRAME_SECT ){
		self->topFrame = self->topFrame->prev;
		if( self->topFrame ) DaoProcess_SetActiveFrame( self, self->topFrame->active );
		return;
	}
	if( att & DAO_ROUT_DEFER ) DList_PopBack( self->defers );
	self->status = DAO_PROCESS_RUNNING;
	self->stackTop = self->topFrame->stackBase;
	self->topFrame = self->topFrame->prev;
	if( self->topFrame ) DaoProcess_SetActiveFrame( self, self->topFrame->active );
}
void DaoProcess_PopFrames( DaoProcess *self, DaoStackFrame *rollback )
{
	while( self->topFrame != rollback ) DaoProcess_PopFrame( self );
}
void DaoProcess_InitTopFrame( DaoProcess *self, DaoRoutine *routine, DaoObject *object )
{
	DaoStackFrame *frame = self->topFrame;
	DaoRoutineBody *body = routine->body;
	DaoValue **values = self->stackValues + frame->stackBase;
	DaoType **types = body->regType->items.pType;
	daoint *id = body->simpleVariables->items.pInt;
	daoint *end = id + body->simpleVariables->size;

	if( object && routine->routHost ){
		object = (DaoObject*)DaoObject_CastToBase( object->rootObject, routine->routHost );
#ifdef DEBUG
		assert( object && object != (DaoObject*)object->defClass->objType->value );
#endif
		GC_Assign( & frame->object, object );
	}

	if( routine == frame->routine ) return;
	GC_Assign( & frame->routine, routine );
	frame->codes = body->vmCodes->data.codes;
	frame->types = types;
	for(; id != end; id++){
		daoint i = *id, tid = types[i]->tid;
		DaoValue *value = values[i], *value2;
		if( value && value->type == tid && value->xGC.refCount == 1 && value->xGC.trait == 0 ) continue;
		value2 = NULL;
		switch( tid ){
		case DAO_NONE    : value2 = dao_none_value; break;
		case DAO_BOOLEAN : value2 = (DaoValue*) DaoBoolean_New(0); break;
		case DAO_INTEGER : value2 = (DaoValue*) DaoInteger_New(0); break;
		case DAO_FLOAT   : value2 = (DaoValue*) DaoFloat_New(0.0); break;
		case DAO_COMPLEX : value2 = (DaoValue*) DaoComplex_New2(0.0,0.0); break;
		case DAO_STRING  : value2 = (DaoValue*) DaoString_New(); break;
		case DAO_ENUM    : value2 = (DaoValue*) DaoEnum_New( types[i], 0 ); break;
		}
		if( value2 == NULL ) continue;
		GC_Assign( & values[i], value2 );
	}
}
void DaoProcess_SetActiveFrame( DaoProcess *self, DaoStackFrame *frame )
{
	frame = frame->active;
	self->activeObject = frame->object;
	self->activeCode = frame->codes + frame->entry - 1;
	self->activeValues = self->stackValues + frame->stackBase;
	self->activeTypes = frame->types;
	self->activeRoutine = frame->routine;
	if( frame->routine ) self->activeNamespace = frame->routine->nameSpace;
}
static void DaoProcess_CopyStackParams( DaoProcess *self )
{
	DaoValue **frameValues = self->stackValues + self->topFrame->stackBase;
	uchar_t i, defCount = self->topFrame->routine->parCount;
	self->topFrame->parCount = self->parCount;
	for(i=0; i<defCount; ++i){
		DaoValue *value = self->paramValues[i];
		if( value == NULL ) break;
		/*
		// DO NOT swap the values for optimziation!
		// frameValues[i] could be a primitive constant, swapping it
		// to self->paramValues may allow it to be modified!
		*/
		GC_Assign( & frameValues[i], value );
	}
}
void DaoProcess_PushRoutine( DaoProcess *self, DaoRoutine *routine, DaoObject *object )
{
	DaoStackFrame *frame;
	DaoProfiler *profiler = self->vmSpace->profiler;

	if( routine->routHost && object == NULL ){
		if( routine->routHost->tid == DAO_OBJECT && !(routine->attribs & DAO_ROUT_STATIC) ){
			DaoValue *firstpar = self->parCount ? self->paramValues[0] : NULL;
			if( firstpar && firstpar->type == DAO_OBJECT ) object = (DaoObject*)firstpar;
#if 0
			printf( "%s %s\n", routine->routName->chars, routine->routType->name->chars );
			printf( "%s\n", routine->routHost->name->chars );
#endif
		}
	}

	if( routine->attribs & DAO_ROUT_INTERFACE ){
		DaoObject *that = object->rootObject;
		DMap *vtable = that->defClass->interMethods;
		DNode *it = vtable ? DMap_Find( vtable, routine ) : NULL;
		if( it && it->value.pRoutine != routine ){
			routine = it->value.pRoutine;
			object = that;
		}
	}

	frame = DaoProcess_PushFrame( self, routine->body->regCount );
	DaoProcess_InitTopFrame( self, routine, object );
	frame->active = frame;
	self->status = DAO_PROCESS_STACKED;
	DaoProcess_CopyStackParams( self );
	if( profiler ) profiler->EnterFrame( profiler, self, self->topFrame, 1 );
}
void DaoProcess_PushFunction( DaoProcess *self, DaoRoutine *routine )
{
	DaoProfiler *profiler = self->vmSpace->profiler;
	DaoStackFrame *frame = DaoProcess_PushFrame( self, routine->parCount );
	frame->active = frame->prev->active;
	self->status = DAO_PROCESS_STACKED;
	GC_Assign( & frame->routine, routine );
	DaoProcess_CopyStackParams( self );
	if( profiler ) profiler->EnterFrame( profiler, self, self->topFrame, 1 );
}
static int DaoRoutine_PassDefault( DaoRoutine *routine, DaoValue *dest[], int passed, DMap *defs )
{
	DaoType *tp, *routype = routine->routType;
	DaoType **types = routype->nested->items.pType;
	DaoValue **consts = routine->routConsts->value->items.pValue;
	int i, ndef = routine->parCount;
	for(i=0; i<ndef; i++){
		int m = types[i]->tid;
		if( m == DAO_PAR_VALIST ) break;
		if( passed & (1<<i) ) continue;
		if( m != DAO_PAR_DEFAULT ) return 0;
		tp = & types[i]->aux->xType;
		if( DaoValue_Move2( consts[i], & dest[i], tp, defs ) == 0 ) return 0;
		if( defs && (tp->tid == DAO_UDT || tp->tid == DAO_THT) ){
			DaoType *type = DaoNamespace_GetType( routine->nameSpace, consts[i] );
			if( !(type->attrib & DAO_TYPE_SPEC) ){
				if( DMap_Find( defs, tp ) == NULL ) DMap_Insert( defs, tp, type );
			}
		}
	}
	return 1;
}
/* Return the routine or its specialized version on success, and NULL on failure: */
DaoRoutine* DaoProcess_PassParams( DaoProcess *self, DaoRoutine *routine, DaoType *hostype, DaoValue *svalue, DaoValue *values[], DaoType *types[], int count, int code )
{
	DMap *defs = NULL;
	DaoValue *argvalue;
	DaoValue **argvalues = values;
	DaoType *argtype, *partype;
	DaoType *routype = routine->routType;
	DaoType **partypes = routype->nested->items.pType;
	DaoType **argtypes = types;
	DaoValue **dest = self->paramValues;
	size_t passed = 0;
	int mcall = code == DVM_MCALL;
	int need_self = routype->attrib & DAO_TYPE_SELF;
	int need_spec = routype->attrib & DAO_TYPE_SPEC;
	int parcount = routine->parCount;
	int argcount = count;
	int argindex, parindex;
	int selfChecked = 0;
#if 0
	int i;
	printf( "%s: %i %i %i\n", routine->routName->chars, parcount, np, svalue ? svalue->type : 0 );
	for(i=0; i<argcount; i++){
		tp = DaoNamespace_GetType( routine->nameSpace, p[i] );
		printf( "%i  %s\n", i, tp->name->chars );
	}
#endif

	self->parCount = 0;
	if( need_spec ){
		defs = DHash_New(0,0);
		if( hostype && routine->routHost && (routine->routHost->attrib & DAO_TYPE_SPEC) ){
			//XXX printf( "%s %s\n", hostype->name->chars, routine->routHost->name->chars );
			/* Init type specialization mapping for static methods: */
			DaoType_MatchTo( hostype, routine->routHost, defs );
		}
	}

	if( mcall && ! need_self ){
		argcount --;
		argvalues ++;
		if( argtypes ) argtypes ++;
	}else if( svalue && need_self && ! mcall ){
		/* class DaoClass : CppClass{ cppmethod(); } */
		partype = (DaoType*) partypes[0]->aux;
		if( DaoValue_Move2( svalue, & dest[0], partype, defs ) ){
			passed = 1;
			selfChecked = 1;
			if( defs && (partype->tid == DAO_UDT || partype->tid == DAO_THT) ){
				DaoType *type = DaoNamespace_GetType( routine->nameSpace, svalue );
				if( !(type->attrib & DAO_TYPE_SPEC) ){
					if( DMap_Find( defs, partype ) == NULL ) DMap_Insert( defs, partype, type );
				}
			}
		}
	}
	/*
	   printf( "%s, rout = %s; parcount = %i; argcount = %i, %i\n", routine->routName->chars, routine->routType->name->chars, parcount, argcount, selfChecked );
	 */
	if( argcount > parcount ) goto ReturnNull;
	if( (argcount|parcount) ==0 ) goto ReturnRoutine;

	/* pass from p[argindex] to dest[parindex], with type checking by partypes[parindex] */
	for(argindex=0; argindex<argcount; argindex++){
		argtype  = argtypes ? argtypes[argindex] : NULL;
		argvalue = argvalues[argindex];
		parindex = argindex + selfChecked;
		if( parindex >= parcount ) goto ReturnNull;
		partype = partypes[parindex];
		if( partype->tid == DAO_PAR_VALIST ){
			partype = partype->aux ? (DaoType*) partype->aux : dao_type_any;
			for(; argindex<argcount; argindex++){
				argtype  = argtypes ? argtypes[argindex] : NULL;
				argvalue = argvalues[argindex];
				parindex = argindex + selfChecked;
				if( argtype && argvalue->type > DAO_ENUM ){
					if( DaoType_CheckInvarMatch( argtype, partype, 0 ) == 0 ) goto ReturnNull;
				}
				if( DaoValue_Move2( argvalue, & dest[parindex], partype, defs ) == 0 ) goto ReturnNull;
				passed |= (size_t)1<<parindex;
			}
			break;
		}
		if( partype->attrib & DAO_TYPE_PARNAMED ) partype = (DaoType*) partype->aux;

		if( argtype && argvalue->type > DAO_ENUM ){
			if( DaoType_CheckInvarMatch( argtype, partype, 0 ) == 0 ) goto ReturnNull;
		}

		passed |= (size_t)1<<parindex;
		if( need_self && parindex == 0 ){
			if( DaoType_MatchValue( partype, argvalue, defs ) >= DAO_MT_EQ ){
				GC_Assign( & dest[parindex], argvalue );
				continue;
			}
		}
		if( DaoValue_Move2( argvalue, & dest[parindex], partype, defs ) == 0 ) goto ReturnNull;
		if( defs && (partype->tid == DAO_UDT || partype->tid == DAO_THT) ){
			DaoType *type = DaoNamespace_GetType( routine->nameSpace, argvalue );
			if( !(type->attrib & DAO_TYPE_SPEC) ){
				if( DMap_Find( defs, partype ) == NULL ) DMap_Insert( defs, partype, type );
			}
		}
	}
	if( defs && defs->size ){ /* Need specialization */
		DaoRoutine *original = routine->original ? routine->original : routine;
		DaoRoutine *current = routine;
		/* Do not share function body. It may be thread unsafe to share: */
		routine = DaoRoutine_Copy( original, 0, 1, 0 );
		DaoRoutine_Finalize( routine, original, routine->routHost, defs );

		if( routine->routType->attrib & DAO_TYPE_SPEC ){
			DaoGC_TryDelete( (DaoValue*) routine );
			routine = current;
		}else{
			if( routine->body ){
				DMap *defs2 = DHash_New(0,0);
				DaoType_MatchTo( routine->routType, original->routType, defs2 );
				/* Only specialize explicitly declared variables: */
				DaoRoutine_MapTypes( routine, original, defs2 );
				DMap_Delete( defs2 );
				if( DaoRoutine_DoTypeInference( routine, 1 ) == 0 ){
					/*
					// Specialization may fail at unreachable parts for certain parameters.
					// Example: binary tree benchmark using list (binary_tree2.dao).
					// But DO NOT revert back to the original function body,
					// to avoid repeatly invoking of this specialization!
					 */
				}
			}
			DMutex_Lock( & mutex_routine_specialize );
			if( original->specialized == NULL ) original->specialized = DRoutines_New();
			DMutex_Unlock( & mutex_routine_specialize );

			GC_Assign( & routine->original, original );
			DRoutines_Add( original->specialized, routine );
		}
	}
	if( (selfChecked + argcount) < parcount ){
		if( DaoRoutine_PassDefault( routine, dest, passed, defs ) == 0 ) goto ReturnNull;
	}
ReturnRoutine:
	if( defs ) DMap_Delete( defs );
	self->parCount = argcount + selfChecked;
	return routine;
ReturnNull:
	if( defs ) DMap_Delete( defs );
	return NULL;
}
static int DaoProcess_CheckInvarMethod( DaoProcess *self, DaoRoutine *routine )
{
	if( self->activeRoutine == NULL ) return 1;
	if( ! (self->activeRoutine->attribs & DAO_ROUT_INVAR) ) return 1;
	if( routine->attribs & (DAO_ROUT_INITOR|DAO_ROUT_INVAR) ) return 1;
	if( DaoType_ChildOf( routine->routHost, self->activeRoutine->routHost ) ){
		DaoProcess_RaiseError( self, "Type", "cannot call normal method inside invar method" );
		return 0;
	}
	return 1;
}
/* If the callable is a constructor, and O is a derived type of the constructor's type,
 * cast O to the constructor's type and then call the constructor on the casted object: */
static int DaoProcess_PushCallableX( DaoProcess *self, DaoRoutine *R, DaoValue *O, DaoValue *P[], DaoType *T[], int N )
{
	if( R == NULL ) return DAO_ERROR;
	R = DaoRoutine_Resolve( R, O, NULL, P, T, N, DVM_CALL );
	if( R != NULL && DaoProcess_CheckInvarMethod( self, R ) == 0 ) return DAO_ERROR;
	if( R ) R = DaoProcess_PassParams( self, R, NULL, O, P, T, N, DVM_CALL );
	if( R == NULL ) return DAO_ERROR_PARAM;

	if( R->body ){
		int need_self = R->routType->attrib & DAO_TYPE_SELF;
		if( need_self && R->routHost && R->routHost->tid == DAO_OBJECT ){
			if( O == NULL && P[0]->type == DAO_OBJECT ) O = P[0];
			if( O ) O = DaoObject_CastToBase( O->xObject.rootObject, R->routHost );
			if( O == NULL || O == O->xObject.defClass->objType->value ) return DAO_ERROR;
		}
		DaoProcess_PushRoutine( self, R, DaoValue_CastObject( O ) );
	}else{
		DaoProcess_PushFunction( self, R );
	}
	return 0;
}
int DaoProcess_PushCallable( DaoProcess *self, DaoRoutine *R, DaoValue *O, DaoValue *P[], int N )
{
	return DaoProcess_PushCallableX( self, R, O, P, NULL, N );
}
/* Special case: mt.start()!!{} */
void DaoProcess_InterceptReturnValue( DaoProcess *self )
{
	self->topFrame->returning = 0xffff;
	if( self->topFrame->routine->body == NULL ){
		self->activeValues = self->stackValues;
		self->activeTypes = self->firstFrame->types;
		self->activeCode = & dummyCallCode;
	}
}

static DaoStackFrame* DaoProcess_FindSectionFrame2( DaoProcess *self, DaoStackFrame *frame )
{
	DaoVmCode *codes;

	if( frame->routine->routType->cbtype == NULL ) return NULL;
	if( frame->host ){
		/* yield inside code section should execute code section for the routine: */
		frame = frame->host->prev;
	}else{
		frame = frame->prev->active;
	}
	if( frame == NULL || frame->routine == NULL ) return NULL;
	codes = frame->codes + frame->entry;
	if( codes[0].code == DVM_GOTO && codes[1].code == DVM_SECT ){
		if( codes[2].code == DVM_GOTO && codes[2].b <= frame->entry ){ /* {yield} */
			return DaoProcess_FindSectionFrame2( self, frame );
		}
		return frame;
	}
	return NULL;
}
DaoStackFrame* DaoProcess_FindSectionFrame( DaoProcess *self )
{
	return DaoProcess_FindSectionFrame2( self, self->topFrame );
}
static DaoStackFrame* DaoProcess_PushSectionFrame( DaoProcess *self )
{
	DaoStackFrame *next, *frame = DaoProcess_FindSectionFrame( self );
	DaoProfiler *profiler = self->vmSpace->profiler;
	int returning = 0xffff;

	if( frame == NULL ) return NULL;
	if( self->topFrame->routine->body ){
		self->topFrame->entry = 1 + self->activeCode - self->topFrame->codes;
		returning = self->activeCode->c;
	}
	next = DaoProcess_PushFrame( self, 0 );
	next->entry = frame->entry + 2;
	next->state = DVM_FRAME_SECT | DVM_FRAME_KEEP;

	GC_Assign( & next->object, frame->object );
	GC_Assign( & next->routine, frame->routine );
	next->parCount = frame->parCount;
	next->stackBase = frame->stackBase;
	next->types = frame->types;
	next->codes = frame->codes;
	next->active = next;
	next->host = frame;
	next->outer = self;
	next->returning = returning;
	DaoProcess_SetActiveFrame( self, frame );
	if( profiler ) profiler->EnterFrame( profiler, self, self->topFrame, 0 );
	return frame;
}
DaoVmCode* DaoProcess_InitCodeSection( DaoProcess *self, int argcount )
{
	DaoType *cbtype = self->topFrame->routine->routType->cbtype;
	DaoStackFrame *topFrame = self->topFrame;
	DaoStackFrame *frame = DaoProcess_PushSectionFrame( self );
	DaoVmCode *sect = NULL;
	if( frame && cbtype ){
		sect = frame->codes + frame->entry + 1;
		if( sect->b == sect->c ){ /* no variadic destination argument list: */
			if( sect->b > cbtype->nested->size ){
				if( cbtype->nested->size > 0 ){
					DaoType *type = (DaoType*) DList_Back( cbtype->nested );
					if( type->tid != DAO_PAR_VALIST ) frame = NULL;
				}else{
					frame = NULL;
				}
			}
		}
	}
	if( frame && frame->process != self ){
		DaoProcess_PopFrames( self, topFrame );
		DaoProcess_RaiseError( self, NULL, "Invalid code section from different process" );
		return NULL;
	}else if( frame == NULL ){
		DaoProcess_PopFrames( self, topFrame );
		DaoProcess_RaiseError( self, NULL, "Invalid code section" );
		return NULL;
	}
	self->topFrame->parCount = argcount < sect->b ? argcount : sect->b;
	return sect;
}
DAO_DLL void DaoProcess_FlushStdStreams( DaoProcess *self );
void DaoProcess_FlushStdStreams( DaoProcess *self )
{
	if( self->stdioStream ) DaoStream_Flush( self->stdioStream );
	DaoStream_Flush( self->vmSpace->stdioStream );
	DaoStream_Flush( self->vmSpace->errorStream );
	fflush( stdout );
	fflush( stderr );
}
int DaoProcess_Compile( DaoProcess *self, DaoNamespace *ns, const char *src )
{
	DaoParser *parser = DaoVmSpace_AcquireParser( self->vmSpace );
	int res;

	parser->nameSpace = ns;
	DString_Assign( parser->fileName, ns->name );
	res = DaoParser_LexCode( parser, src, 1 ) && DaoParser_ParseScript( parser );
	DaoVmSpace_ReleaseParser( self->vmSpace, parser );
	DaoProcess_FlushStdStreams( self );
	return res;
}
int DaoProcess_Eval( DaoProcess *self, DaoNamespace *ns, const char *source )
{
	DaoParser *parser = DaoVmSpace_AcquireParser( self->vmSpace );
	DaoRoutine *rout;
	int res;

	parser->autoReturn = 1;
	parser->nameSpace = ns;
	DString_SetChars( parser->fileName, "code string" );
	res = DaoParser_LexCode( parser, source, 1 ) && DaoParser_ParseScript( parser );
	DaoVmSpace_ReleaseParser( self->vmSpace, parser );
	DaoProcess_FlushStdStreams( self );
	if( res == 0 ) return 0;
	rout = ns->mainRoutines->items.pRoutine[ ns->mainRoutines->size-1 ];
	if( DaoProcess_Call( self, rout, NULL, NULL, 0 ) ) return 0;
	return ns->mainRoutines->size;
}
int DaoProcess_Call( DaoProcess *self, DaoRoutine *M, DaoValue *O, DaoValue *P[], int N )
{
	int ret = DaoProcess_PushCallable( self, M, O, P, N );
	if( ret ) goto Done;
	/* no return value to the previous stack frame */
	DaoProcess_InterceptReturnValue( self );
	ret = DaoProcess_Execute( self ) == 0 ? DAO_ERROR : 0;
Done:
	DaoProcess_FlushStdStreams( self );
	return ret;
}
void DaoProcess_CallFunction( DaoProcess *self, DaoRoutine *func, DaoValue *p[], int n )
{
	daoint m = self->factory->size;
	daoint cur = self->stackReturn;

	self->stackReturn = -1;
	func->pFunc( self, p, n );
	if( self->stackReturn == -1 ){
		if( self->topFrame != self->topFrame->active && self->topFrame->returning == 0xffff ){
			GC_Assign( self->stackValues, dao_none_value );
		}else{
			int opc = self->activeCode->c;
			int optype = DaoVmCode_GetOpcodeType( self->activeCode );
			int ret = (optype >= DAO_CODE_GETC) & (optype <= DAO_CODE_GETM);
			ret |= (optype >= DAO_CODE_MOVE) & (optype <= DAO_CODE_YIELD);
			if( ret ){
				DaoProcess_SetValue( self, opc, dao_none_value );
				self->stackReturn = opc + (self->activeValues - self->stackValues);
			}
		}
	}
	if( self->topFrame->returning == 0xffff && self->stackReturn > 0 ){
		DaoValue *returned = self->stackValues[ self->stackReturn ];
		GC_Assign( self->stackValues, returned );
	}
	if( self->factory->size > m ) DList_Erase( self->factory, m, -1 );
	self->stackReturn = cur;
}
DaoValue* DaoProcess_GetReturned( DaoProcess *self )
{
	return self->stackValues[0];
}
static int DaoProcess_PushDefers( DaoProcess *self, DaoValue *result )
{
	DaoStackFrame *f, *frame = self->topFrame;
	daoint deferCount = self->defers->size - frame->deferBase;
	daoint errorCount = self->exceptions->size - frame->exceptBase;
	daoint i, j;
	self->activeCode = NULL;
	self->list->size = 0;
	for(i=self->defers->size-1; i>=frame->deferBase; --i){
		DaoRoutine *rout, *closure = self->defers->items.pRoutine[i];
		DaoValue *param = NULL;
		DaoType *type = NULL;
		if( closure->routType->nested->size ) type = closure->routType->nested->items.pType[0];
		if( type && type->tid == DAO_PAR_NAMED ) type = (DaoType*) type->aux;
		if( type == NULL ){
		}else if( type->tid == DAO_NONE ){
			if( errorCount ) continue;
			param = dao_none_value;
		}else{
			for(j=self->exceptions->size-1; j>=frame->exceptBase; --j){
				param = self->exceptions->items.pValue[j];
				if( type->tid == DAO_CSTRUCT ){
					param = (DaoValue*) DaoValue_CastCstruct( param, type );
				}else if( type->tid == DAO_OBJECT ){
					if( param->type == DAO_OBJECT ){
						param = DaoObject_CastToBase( (DaoObject*) param, type );
					}else{
						param = NULL;
					}
				}
				if( param ){
					DList_Erase( self->exceptions, j, 1 );
					break;
				}
			}
			if( param == NULL ) continue;
		}
		DList_Append( self->list, param );
		DList_Append( self->defers, closure );
	}
	for(i=0; i<self->list->size; ++i){
		DaoValue *param = self->list->items.pValue[ self->list->size - i - 1 ];
		DaoRoutine *closure = self->defers->items.pRoutine[ self->defers->size - i - 1 ];

		self->parCount = param != NULL;
		if( param ) DaoValue_Copy( param, self->paramValues );
		DaoProcess_PushRoutine( self, closure, NULL );
		self->topFrame->deferBase -= deferCount;
		self->topFrame->returning = 0xffff;
		self->topFrame->host = frame;
	}
	DList_Erase( self->defers, frame->deferBase, deferCount );
	for(f=self->topFrame; f!=frame; f=f->prev) f->exceptBase = self->exceptions->size;
	return self->topFrame != frame;
}

static daoint DaoArray_ComputeIndex( DaoArray *self, DaoValue *ivalues[], int count )
{
	daoint *dims, *dmac, i, j, id = 0;
	if( count > self->ndim ) return -1;
	dims = self->dims;
	dmac = self->dims + self->ndim;
	for(i=0; i<count; i++){
		j = ivalues[i]->xInteger.value;
		if( j <0 ) j += dims[i];
		if( j <0 || j >= dims[i] ) return -1;
		id += j * dmac[i];
	}
	return id;
}


#define LocalInt( i )     locVars[i]->xInteger.value
#define LocalFloat( i )   locVars[i]->xFloat.value
#define LocalComplex( i ) locVars[i]->xComplex.value


static int DaoProcess_Move( DaoProcess *self, DaoValue *A, DaoValue **C, DaoType *t );

#ifdef DAO_USE_CODE_STATE
static void DaoProcess_AdjustCodes( DaoProcess *self, int options )
{
	DaoDebugger *debugger = self->vmSpace->debugger;
	DaoRoutine *routine = self->topFrame->routine;
	DaoVmCode *c = self->topFrame->codes;
	int i, n = routine->body->vmCodes->size;
	int mode = routine->body->exeMode;
	if( options & DAO_OPTION_DEBUG ){
		routine->body->exeMode |= DAO_ROUT_MODE_DEBUG;
		if( debugger && debugger->BreakPoints ) debugger->BreakPoints( debugger, routine );
	}else if( mode & DAO_OPTION_DEBUG ){
		routine->body->exeMode &= ~DAO_ROUT_MODE_DEBUG;
		for(i=0; i<n; i++) c[i].state &= ~ DAO_CODE_BREAKING;
	}
}
#endif

#ifdef DAO_WITH_CONCURRENT
int DaoCallServer_MarkActiveProcess( DaoProcess *process, int active );
#endif


#ifdef DAO_DEBUG_VM
#define DAO_DEBUG_VM
#define WITHOUT_DIRECT_THREADING
#endif

#ifndef WITHOUT_DIRECT_THREADING
#if defined(DAO_USE_CODE_STATE) || !defined( __GNUC__ ) || defined( __STRICT_ANSI__ )
#define WITHOUT_DIRECT_THREADING
#endif
#endif


#ifndef WITHOUT_DIRECT_THREADING

#define OPBEGIN() goto *labels[ vmc->code ];
#define OPCASE( name ) LAB_##name :
#define OPNEXT() goto *labels[ (++vmc)->code ];
#define OPJUMP() goto *labels[ vmc->code ];
#define OPDEFAULT()
#define OPEND()

#else

#ifndef DAO_USE_CODE_STATE
#if defined( __GNUC__ ) && !defined( __STRICT_ANSI__ )
#warning "=========================================="
#warning "=========================================="
#warning "  NOT USING DIRECT THREADING"
#warning "=========================================="
#warning "=========================================="
#endif
#endif

#ifdef DAO_USE_CODE_STATE
#define HANDLE_BREAK_POINT() \
	if( vmc->state & DAO_CODE_BREAKING ){ \
		self->activeCode = vmc; \
		if( debugger && debugger->Debug ) debugger->Debug( debugger, self, NULL ); \
		goto CheckException; \
	}
#else
#define HANDLE_BREAK_POINT()
#endif

#define OPBEGIN() for(;;){ HANDLE_BREAK_POINT() switch( vmc->code )
#define OPCASE( name ) case DVM_##name :
#define OPNEXT() break;
#define OPJUMP() continue;
#define OPDEFAULT() default:
#define OPEND() vmc++; }

#ifdef DAO_DEBUG_VM
#undef OPBEGIN
#define OPBEGIN() for(;;){ printf("%3i:", (i=vmc-vmcBase) ); DaoVmCodeX_Print( *topFrame->routine->body->annotCodes->items.pVmc[i], NULL, NULL ); fflush(stdout); switch( vmc->code )
#endif

#endif


int DaoProcess_Start( DaoProcess *self )
{
	DaoJitCallData jitCallData = {NULL};
	DaoStackFrame *rollback = NULL;
	DaoDebugger *debugger = self->vmSpace->debugger;
	DaoProfiler *profiler = self->vmSpace->profiler;
	DaoUserHandler *handler = self->vmSpace->userHandler;
	DaoVmSpace *vmSpace = self->vmSpace;
	DaoVmCode *vmcBase, *sect, *vmc = NULL;
	DaoVmCode operands = {0};
	DaoStackFrame *topFrame;
	DaoStackFrame *base;
	DaoRoutine *routine;
	DaoClass *host = NULL;
	DaoClass *klass = NULL;
	DaoObject *othis = NULL;
	DaoObject *object = NULL;
	DaoArray *array;
	DList   *typeVO = NULL;
	DList   *glbVars = NULL;
	DList   *clsVars = NULL;
	DList   *glbConsts = NULL;
	DList   *clsConsts = NULL;
	DaoProcess *dataVH[DAO_MAX_SECTDEPTH+1] = {NULL};
	DaoVariable *variable = NULL;
	DaoVariable **upValues = NULL;
	DaoValue  **dataVO = NULL;
	DaoValue **dataCL = NULL;
	DaoValue *value, *vA, *vB, *vC = NULL;
	DaoValue **vA2, **vB2, **vC2 = NULL;
	DaoValue **vref;
	DaoValue **locVars;
	DaoType **locTypes;
	DaoType *abtp, *ta, *tb;
	DaoTuple *tuple;
	DaoList *list;
	DString *str;
	dao_complex com = {0,0};
	dao_complex czero = {0,0};
	dao_complex acom, bcom;
	double AA, BB, dnum=0;
	int invokehost = handler && handler->InvokeHost;
	int active = self->active;
	daoint exceptCount0 = self->exceptions->size;
	daoint exceptCount = 0;
	daoint count = 0;
	daoint i, j, id, size;
	daoint inum=0;
	float fnum=0;

#ifndef WITHOUT_DIRECT_THREADING
	static void *labels[] = {
		&& LAB_DATA ,
		&& LAB_GETCL , && LAB_GETCK , && LAB_GETCG ,
		&& LAB_GETVH , && LAB_GETVS , && LAB_GETVO , && LAB_GETVK , && LAB_GETVG ,
		&& LAB_GETI  , && LAB_GETDI , && LAB_GETMI , && LAB_GETF  ,
		&& LAB_SETVH , && LAB_SETVS , && LAB_SETVO , && LAB_SETVK , && LAB_SETVG ,
		&& LAB_SETI  , && LAB_SETDI , && LAB_SETMI , && LAB_SETF  ,
		&& LAB_LOAD  , && LAB_MOVE , && LAB_UNTAG , && LAB_CAST ,
		&& LAB_NOT , && LAB_MINUS , && LAB_TILDE , && LAB_SIZE ,
		&& LAB_ADD , && LAB_SUB ,
		&& LAB_MUL , && LAB_DIV ,
		&& LAB_MOD , && LAB_POW ,
		&& LAB_AND , && LAB_OR ,
		&& LAB_LT , && LAB_LE ,
		&& LAB_EQ , && LAB_NE , && LAB_IN ,
		&& LAB_BITAND , && LAB_BITOR ,
		&& LAB_BITXOR , && LAB_BITLFT ,
		&& LAB_BITRIT , && LAB_SAME , && LAB_ISA ,
		&& LAB_NAMEVA , && LAB_PAIR ,
		&& LAB_TUPLE  , && LAB_LIST , && LAB_MAP ,
		&& LAB_VECTOR , && LAB_MATRIX ,
		&& LAB_PACK  , && LAB_MPACK ,
		&& LAB_ROUTINE ,
		&& LAB_GOTO ,
		&& LAB_SWITCH , && LAB_CASE ,
		&& LAB_ITER , && LAB_TEST ,
		&& LAB_MATH ,
		&& LAB_CALL , && LAB_MCALL ,
		&& LAB_RETURN , && LAB_YIELD ,
		&& LAB_SECT ,
		&& LAB_JITC ,

		&& LAB_DATA_B , && LAB_DATA_I , && LAB_DATA_F , && LAB_DATA_C ,
		&& LAB_GETCL_B , && LAB_GETCL_I , && LAB_GETCL_F , && LAB_GETCL_C ,
		&& LAB_GETCK_B , && LAB_GETCK_I , && LAB_GETCK_F , && LAB_GETCK_C ,
		&& LAB_GETCG_B , && LAB_GETCG_I , && LAB_GETCG_F , && LAB_GETCG_C ,
		&& LAB_GETVH_B , && LAB_GETVH_I , && LAB_GETVH_F , && LAB_GETVH_C ,
		&& LAB_GETVS_B , && LAB_GETVS_I , && LAB_GETVS_F , && LAB_GETVS_C ,
		&& LAB_GETVO_B , && LAB_GETVO_I , && LAB_GETVO_F , && LAB_GETVO_C ,
		&& LAB_GETVK_B , && LAB_GETVK_I , && LAB_GETVK_F , && LAB_GETVK_C ,
		&& LAB_GETVG_B , && LAB_GETVG_I , && LAB_GETVG_F , && LAB_GETVG_C ,
		&& LAB_SETVH_BB , && LAB_SETVH_II , && LAB_SETVH_FF , && LAB_SETVH_CC ,
		&& LAB_SETVS_BB , && LAB_SETVS_II , && LAB_SETVS_FF , && LAB_SETVS_CC ,
		&& LAB_SETVO_BB , && LAB_SETVO_II , && LAB_SETVO_FF , && LAB_SETVO_CC ,
		&& LAB_SETVK_BB , && LAB_SETVK_II , && LAB_SETVK_FF , && LAB_SETVK_CC ,
		&& LAB_SETVG_BB , && LAB_SETVG_II , && LAB_SETVG_FF , && LAB_SETVG_CC ,

		&& LAB_MOVE_BB , && LAB_MOVE_BI , && LAB_MOVE_BF ,
		&& LAB_MOVE_IB , && LAB_MOVE_II , && LAB_MOVE_IF ,
		&& LAB_MOVE_FB , && LAB_MOVE_FI , && LAB_MOVE_FF ,

		&& LAB_MOVE_CF , && LAB_MOVE_CC , 
		&& LAB_MOVE_SS , && LAB_MOVE_PP , && LAB_MOVE_XX ,

		&& LAB_NOT_B , && LAB_NOT_I , && LAB_NOT_F ,
		&& LAB_MINUS_I , && LAB_MINUS_F , && LAB_MINUS_C ,
		&& LAB_TILDE_I , && LAB_TILDE_C ,

		&& LAB_AND_BBB , && LAB_OR_BBB , && LAB_LT_BBB , 
		&& LAB_LE_BBB , && LAB_EQ_BBB , && LAB_NE_BBB ,

		&& LAB_ADD_III , && LAB_SUB_III , && LAB_MUL_III ,
		&& LAB_DIV_III , && LAB_MOD_III , && LAB_POW_III ,

		&& LAB_AND_BII , && LAB_OR_BII , && LAB_LT_BII , 
		&& LAB_LE_BII , && LAB_EQ_BII , && LAB_NE_BII ,

		&& LAB_BITAND_III , && LAB_BITOR_III , && LAB_BITXOR_III ,
		&& LAB_BITLFT_III , && LAB_BITRIT_III ,

		&& LAB_ADD_FFF , && LAB_SUB_FFF , && LAB_MUL_FFF ,
		&& LAB_DIV_FFF , && LAB_MOD_FFF , && LAB_POW_FFF ,

		&& LAB_AND_BFF , && LAB_OR_BFF , && LAB_LT_BFF ,
		&& LAB_LE_BFF , && LAB_EQ_BFF , && LAB_NE_BFF ,

		&& LAB_ADD_CCC , && LAB_SUB_CCC ,
		&& LAB_MUL_CCC , && LAB_DIV_CCC ,
		&& LAB_EQ_BCC , && LAB_NE_BCC ,

		&& LAB_ADD_SSS ,
		&& LAB_LT_BSS , && LAB_LE_BSS ,
		&& LAB_EQ_BSS , && LAB_NE_BSS ,

		&& LAB_GETI_LI , && LAB_SETI_LI , && LAB_GETI_SI , && LAB_SETI_SII ,
		&& LAB_GETI_LBI , && LAB_GETI_LII , && LAB_GETI_LFI ,
		&& LAB_GETI_LCI , && LAB_GETI_LSI ,
		&& LAB_SETI_LBIB , && LAB_SETI_LIII , && LAB_SETI_LFIF ,
		&& LAB_SETI_LCIC , && LAB_SETI_LSIS ,
		&& LAB_GETI_ABI , && LAB_GETI_AII , && LAB_GETI_AFI , && LAB_GETI_ACI ,
		&& LAB_SETI_ABIB , && LAB_SETI_AIII , && LAB_SETI_AFIF , && LAB_SETI_ACIC ,

		&& LAB_GETI_TI , && LAB_SETI_TI ,

		&& LAB_GETF_TB , && LAB_GETF_TI , && LAB_GETF_TF ,
		&& LAB_GETF_TC , && LAB_GETF_TX ,
		&& LAB_SETF_TBB , && LAB_SETF_TII , && LAB_SETF_TFF ,
		&& LAB_SETF_TCC , && LAB_SETF_TSS ,
		&& LAB_SETF_TPP , && LAB_SETF_TXX ,

		&& LAB_GETMI_ABI , && LAB_GETMI_AII , && LAB_GETMI_AFI , && LAB_GETMI_ACI ,
		&& LAB_SETMI_ABIB , && LAB_SETMI_AIII , && LAB_SETMI_AFIF , && LAB_SETMI_ACIC ,

		&& LAB_GETF_CX , && LAB_SETF_CX ,

		&& LAB_GETF_KC , && LAB_GETF_KG ,
		&& LAB_GETF_OC , && LAB_GETF_OG , && LAB_GETF_OV ,
		&& LAB_SETF_KG , && LAB_SETF_OG , && LAB_SETF_OV ,

		&& LAB_GETF_KCB , && LAB_GETF_KCI , && LAB_GETF_KCF , && LAB_GETF_KCC ,
		&& LAB_GETF_KGB , && LAB_GETF_KGI , && LAB_GETF_KGF , && LAB_GETF_KGC ,
		&& LAB_GETF_OCB , && LAB_GETF_OCI , && LAB_GETF_OCF , && LAB_GETF_OCC ,
		&& LAB_GETF_OGB , && LAB_GETF_OGI , && LAB_GETF_OGF , && LAB_GETF_OGC ,
		&& LAB_GETF_OVB , && LAB_GETF_OVI , && LAB_GETF_OVF , && LAB_GETF_OVC ,

		&& LAB_SETF_KGBB , && LAB_SETF_KGII , && LAB_SETF_KGFF , && LAB_SETF_KGCC ,
		&& LAB_SETF_OGBB , && LAB_SETF_OGII , && LAB_SETF_OGFF , && LAB_SETF_OGCC ,
		&& LAB_SETF_OVBB , && LAB_SETF_OVII , && LAB_SETF_OVFF , && LAB_SETF_OVCC ,

		&& LAB_TEST_B , && LAB_TEST_I , && LAB_TEST_F ,
		&& LAB_MATH_B , && LAB_MATH_I , && LAB_MATH_F ,
		&& LAB_CAST_B , && LAB_CAST_I , && LAB_CAST_F ,
		&& LAB_CAST_C , && LAB_CAST_S , 
		&& LAB_CAST_VE , && LAB_CAST_VX ,
		&& LAB_ISA_ST ,
		&& LAB_TUPLE_SIM
	};
#endif


	self->depth += 1;
	if( self->depth > 1024 ){
		DaoStream *stream = vmSpace->errorStream;
		DaoStream_WriteChars( stream, "ERROR: too deeply nested process execution!\n" );
		goto AbortProcess;
	}

	if( self->topFrame == self->firstFrame ) goto ReturnFalse;
	rollback = self->topFrame->prev;
	base = self->topFrame;
	if( self->status == DAO_PROCESS_SUSPENDED ) base = self->firstFrame->next;
	if( self->topFrame->state & DVM_FRAME_KEEP ) rollback = self->topFrame;

CallEntry:

	topFrame = self->topFrame;
	routine = topFrame->routine;

	if( topFrame == base->prev ){
		self->status = DAO_PROCESS_FINISHED;
		if( self->exceptions->size > 0 ) goto FinishProcess;
		/*if( eventHandler ) eventHandler->mainRoutineExit(); */
		goto ReturnTrue;
	}
	if( self->topFrame->state & DVM_FRAME_FINISHED ) goto FinishCall;

	if( routine->pFunc ){
		DaoValue **p = self->stackValues + topFrame->stackBase;
		if( self->status == DAO_PROCESS_STACKED ){
			DaoProcess_CallFunction( self, topFrame->routine, p, topFrame->parCount );
		}
		DaoProcess_PopFrame( self );
		goto CallEntry;
	}


#ifdef DAO_DEBUG_VM
	if( ROUT_HOST_TID( routine ) == DAO_OBJECT )
		printf("class name = %s\n", routine->routHost->aux->xClass.className->chars);
	printf("routine name = %s\n", routine->routName->chars);
	printf("number of instruction: %i\n", routine->body->vmCodes->size );
	printf("entry instruction: %i\n", self->topFrame->entry );
	if( routine->routType ) printf("routine type = %s\n", routine->routType->name->chars);
#endif

	if( vmSpace->stopit ) goto FinishProcess;
	if( invokehost ) handler->InvokeHost( handler, self );

#ifdef DAO_USE_CODE_STATE
	if( (vmSpace->options&DAO_OPTION_DEBUG) | (routine->body->exeMode&DAO_ROUT_MODE_DEBUG) )
		DaoProcess_AdjustCodes( self, vmSpace->options );
#endif

	vmcBase = topFrame->codes;
	id = self->topFrame->entry;
	vmc = vmcBase + id;
	self->activeCode = vmc;
	self->activeRoutine = routine;
	self->activeObject = topFrame->object;
	self->activeValues = self->stackValues + topFrame->stackBase;
	self->activeTypes = routine->body->regType->items.pType;
	self->activeNamespace = routine->nameSpace;

	if( id >= routine->body->vmCodes->size ){
		if( id == 0 ){
			DString_SetChars( self->string, "Not implemented function, " );
			DString_Append( self->string, routine->routName );
			DString_AppendChars( self->string, "()" );
			DaoProcess_RaiseError( self, NULL, self->string->chars );
			goto FinishProcess;
		}
		goto FinishCall;
	}

	if( !(topFrame->state & DVM_FRAME_RUNNING) ){
		topFrame->deferBase = self->defers->size;
		topFrame->exceptBase = self->exceptions->size;
	}

	exceptCount = self->exceptions->size;
	if( self->exceptions->size > topFrame->exceptBase ) goto FinishCall;

	if( self->status == DAO_PROCESS_SUSPENDED &&
			(vmc->code == DVM_CALL || vmc->code == DVM_MCALL || vmc->code == DVM_YIELD) ){
		DaoType *type = self->activeTypes[vmc->c];
		DaoFuture *future = self->future;
		DaoRoutine *meth;
		DaoTuple *tuple;
		int finished;
		if( profiler ) profiler->EnterFrame( profiler, self, self->topFrame, 0 );
		switch( self->pauseType ){
		case DAO_PAUSE_NONE :
			break;
		case DAO_PAUSE_FUTURE_VALUE :
			/*
			// No need to check abortion of the precondition tasklet.
			// If it happened, this one should not have been activated.
			*/
			finished = future->precond->state == DAO_CALL_FINISHED;
			DaoProcess_PutValue( self, finished ? future->precond->value : dao_none_value );
			break;
		case DAO_PAUSE_FUTURE_WAIT :
			DaoProcess_PutBoolean( self, future->precond->state == DAO_CALL_FINISHED );
			break;
		case DAO_PAUSE_CHANNEL_SEND :
			DaoProcess_PutBoolean( self, future->timeout == 0 );
			break;
		case DAO_PAUSE_CHANNEL_RECEIVE :
			/*
			// Do not use DaoProcess_PutTuple(), because it will use
			// self->topFrame->routine->routType->aux
			// to obtain the returning tuple type.
			// That is valid only during the call when "self->topFrame" is still
			// for the callee.
			*/
			if( type->tid != DAO_TUPLE ){
				meth = (DaoRoutine*) self->activeValues[ vmc->a ];
				type = (DaoType*) meth->routType->aux;
			}
			tuple = DaoProcess_GetTuple( self, type, type->nested->size, 1 );
			DaoTuple_SetItem( tuple, future->message ? future->message : dao_none_value, 0 );
			tuple->values[1]->xEnum.value = future->aux1 ? 2 : future->timeout != 0;
			break;
		case DAO_PAUSE_CHANFUT_SELECT :
			if( type->tid != DAO_TUPLE ){
				meth = (DaoRoutine*) self->activeValues[ vmc->a ];
				type = (DaoType*) meth->routType->aux;
			}
			tuple = DaoProcess_GetTuple( self, type, type->nested->size, 1 );
			DaoTuple_SetItem( tuple, future->selected ? future->selected : dao_none_value, 0 );
			DaoTuple_SetItem( tuple, future->message ? future->message : dao_none_value, 1 );
			tuple->values[2]->xEnum.value = future->aux1 ? 2 : future->timeout != 0;
			break;
		default: break;
		}
		vmc ++;
	}
	topFrame->state |= DVM_FRAME_RUNNING;
	self->status = DAO_PROCESS_RUNNING;
	self->pauseType = DAO_PAUSE_NONE;
	host = NULL;
	glbVars = routine->nameSpace->variables;
	glbConsts = routine->nameSpace->constants;
	othis = topFrame->object;
	locVars = self->activeValues;
	locTypes = self->activeTypes;
	dataCL = routine->routConsts->value->items.pValue;
	upValues = routine->body->upValues ? routine->body->upValues->items.pVar : NULL;
	if( routine->body->jitData ){
		jitCallData.localValues = locVars;
		jitCallData.localConsts = routine->routConsts->value->items.pValue;
		jitCallData.globalValues = glbVars->items.pVar;
		jitCallData.globalConsts = glbConsts->items.pConst;
		jitCallData.processes = dataVH;
	}
	if( ROUT_HOST_TID( routine ) == DAO_OBJECT ){
		host = & routine->routHost->aux->xClass;
		clsVars = host->variables;
		clsConsts = host->constants;
		jitCallData.classValues = clsVars->items.pVar;
		jitCallData.classConsts = clsConsts->items.pConst;
		if( !(routine->attribs & DAO_ROUT_STATIC) ){
			dataVO = othis->objValues;
			typeVO = host->instvars;
			jitCallData.objectValues = dataVO;
		}
	}
	if( topFrame->outer ){
		DaoStackFrame *frame = topFrame;
		for(i=1; (i<=DAO_MAX_SECTDEPTH) && frame->outer; i++){
			dataVH[i] = frame->outer;
			frame = frame->host;
		}
	}

	OPBEGIN(){
		OPCASE( DATA ){
			if( vmc->a == DAO_NONE ){
				GC_Assign( & locVars[vmc->c], dao_none_value );
			}else{
				value = locVars[vmc->c];
				if( value == NULL || value->type != vmc->a ){
					value = (DaoValue*) DaoComplex_New(czero);
					value->type = vmc->a;
					GC_Assign( & locVars[vmc->c], value );
				}
				switch( vmc->a ){
				case DAO_COMPLEX :
					value->xComplex.value.real = 0;
					value->xComplex.value.imag = vmc->b;
					break;
				case DAO_BOOLEAN : value->xInteger.value = vmc->b != 0; break;
				case DAO_INTEGER : value->xInteger.value = vmc->b; break;
				case DAO_FLOAT  : value->xFloat.value = vmc->b; break;
				default : break;
				}
			}
		}OPNEXT() OPCASE( GETCL ){
			/* All GETX instructions assume the C regisgter is an intermediate register! */
			value = dataCL[vmc->b];
			GC_Assign( & locVars[vmc->c], value );
		}OPNEXT() OPCASE( GETCK ){
			value = clsConsts->items.pConst[vmc->b]->value;
			GC_Assign( & locVars[vmc->c], value );
		}OPNEXT() OPCASE( GETCG ){
			value = glbConsts->items.pConst[vmc->b]->value;
			GC_Assign( & locVars[vmc->c], value );
		}OPNEXT() OPCASE( GETVH ){
			value = dataVH[vmc->a]->activeValues[vmc->b];
			GC_Assign( & locVars[vmc->c], value );
		}OPNEXT() OPCASE( GETVS ){
			value = upValues[vmc->b]->value;
			GC_Assign( & locVars[vmc->c], value );
		}OPNEXT() OPCASE( GETVO ){
			GC_Assign( & locVars[vmc->c], dataVO[vmc->b] );
		}OPNEXT() OPCASE( GETVK ){
			value = clsVars->items.pVar[vmc->b]->value;
			GC_Assign( & locVars[vmc->c], value );
		}OPNEXT() OPCASE( GETVG ){
			value = glbVars->items.pVar[vmc->b]->value;
			GC_Assign( & locVars[vmc->c], value );
		}OPNEXT() OPCASE( GETI ) OPCASE( GETDI ) OPCASE( GETMI ){
			DaoProcess_DoGetItem( self, vmc );
			goto CheckException;
		}OPNEXT() OPCASE( GETF ){
			DaoProcess_DoGetField( self, vmc );
			goto CheckException;
		}OPNEXT() OPCASE( SETVH ){
			self->activeCode = vmc;
			abtp = dataVH[vmc->c]->activeTypes[vmc->b];
			if( DaoProcess_Move( self, locVars[vmc->a], dataVH[vmc->c]->activeValues + vmc->b, abtp ) ==0 )
				goto CheckException;
		}OPNEXT() OPCASE( SETVS ){
			self->activeCode = vmc;
			variable = upValues[vmc->b];
			if( DaoProcess_Move( self, locVars[vmc->a], & variable->value, variable->dtype ) ==0 )
				goto CheckException;
		}OPNEXT() OPCASE( SETVO ){
			self->activeCode = vmc;
			abtp = typeVO->items.pVar[vmc->b]->dtype;
			if( DaoProcess_Move( self, locVars[vmc->a], dataVO + vmc->b, abtp ) ==0 )
				goto CheckException;
		}OPNEXT() OPCASE( SETVK ){
			self->activeCode = vmc;
			variable = clsVars->items.pVar[vmc->b];
			if( DaoProcess_Move( self, locVars[vmc->a], & variable->value, variable->dtype ) ==0 ) goto CheckException;
		}OPNEXT() OPCASE( SETVG ){
			self->activeCode = vmc;
			variable = glbVars->items.pVar[vmc->b];
			if( DaoProcess_Move( self, locVars[vmc->a], & variable->value, variable->dtype ) ==0 )
				goto CheckException;
		}OPNEXT() OPCASE( SETI ) OPCASE( SETDI ) OPCASE( SETMI ){
			DaoProcess_DoSetItem( self, vmc );
			goto CheckException;
		}OPNEXT() OPCASE( SETF ){
			DaoProcess_DoSetField( self, vmc );
			goto CheckException;
		}OPNEXT() OPCASE( LOAD ){
			if( (vA = locVars[vmc->a]) ){
				/*
				// mt.run(3){ mt.critical{} }: the inner functional will be compiled as
				// a LOAD and RETURN, but the inner functional will not return anything,
				// so the first operand of LOAD will be NULL!
				*/
				if( (vA->xBase.trait & DAO_VALUE_CONST) == 0 ){
					GC_Assign( & locVars[vmc->c], vA );
				}else{
					DaoValue_Copy( vA, & locVars[vmc->c] );
				}
			}
		}OPNEXT() OPCASE( MOVE ) OPCASE( UNTAG ) {
			self->activeCode = vmc;
			DaoProcess_Move( self, locVars[vmc->a], & locVars[vmc->c], locTypes[vmc->c] );
			goto CheckException;
		}OPNEXT() OPCASE( CAST ){
			DaoProcess_DoCast( self, vmc );
			goto CheckException;
		}OPNEXT()
		OPCASE( ADD )
		OPCASE( SUB )
		OPCASE( MUL )
		OPCASE( DIV )
		OPCASE( MOD )
		OPCASE( POW ){
			self->activeCode = vmc;
			DaoProcess_DoBinArith( self, vmc );
			goto CheckException;
		}OPNEXT()
		OPCASE( AND )
		OPCASE( OR )
		OPCASE( LT )
		OPCASE( LE )
		OPCASE( EQ )
		OPCASE( NE ){
			self->activeCode = vmc;
			DaoProcess_DoBinBool( self, vmc );
			goto CheckException;
		}OPNEXT()
		OPCASE( IN ){
			self->activeCode = vmc;
			DaoProcess_DoInTest( self, vmc );
			goto CheckException;
		}OPNEXT() OPCASE( NOT ){
			self->activeCode = vmc;
			DaoProcess_DoUnaBool( self, vmc );
			goto CheckException;
		}OPNEXT() OPCASE( MINUS ){
			self->activeCode = vmc;
			DaoProcess_DoUnaArith( self, vmc );
			goto CheckException;
		}OPNEXT() OPCASE( BITAND ) OPCASE( BITOR ) OPCASE( BITXOR ){
			self->activeCode = vmc;
			DaoProcess_DoBitLogic( self, vmc );
			goto CheckException;
		}OPNEXT() OPCASE( BITLFT ) OPCASE( BITRIT ){
			self->activeCode = vmc;
			DaoProcess_DoBitShift( self, vmc );
			goto CheckException;
		}OPNEXT() OPCASE( TILDE ){
			self->activeCode = vmc;
			DaoProcess_DoBitFlip( self, vmc );
			goto CheckException;
		}OPNEXT() OPCASE( SIZE ){
			vA = locVars[vmc->a];
			vC = locVars[vmc->c];
			switch( vA->type ){
			case DAO_CSTRUCT :
			case DAO_CDATA :
			case DAO_OBJECT :
			case DAO_CINVALUE :
				self->activeCode = vmc;
				ta = self->activeTypes[ vmc->a ];
				tb = self->activeTypes[ vmc->b ];
				if( DaoProcess_TryUserArith( self, vA, NULL, vC, ta, tb ) ) goto CheckException;
				goto RaiseErrorInvalidOperation;
			case DAO_NONE    : vC->xInteger.value = 0; break;
			case DAO_BOOLEAN : vC->xInteger.value = sizeof(dao_integer); break;
			case DAO_INTEGER : vC->xInteger.value = sizeof(dao_integer); break;
			case DAO_FLOAT   : vC->xInteger.value = sizeof(dao_float); break;
			case DAO_COMPLEX : vC->xInteger.value = sizeof(dao_complex); break;
			case DAO_ENUM    : vC->xInteger.value = sizeof(int) + sizeof(DaoType*); break;
			case DAO_STRING  : vC->xInteger.value = vA->xString.value->size; break;
			case DAO_LIST    : vC->xInteger.value = vA->xList.value->size; break;
			case DAO_MAP     : vC->xInteger.value = vA->xMap.value->size; break;
			case DAO_TUPLE   : vC->xInteger.value = vA->xTuple.size; break;
#ifdef DAO_WITH_NUMARRAY
			case DAO_ARRAY   : vC->xInteger.value = vA->xArray.size; break;
#endif
			default : goto RaiseErrorInvalidOperation;
			}
		}OPNEXT() OPCASE( SAME ){
			DaoProcess_DoCheckSame( self, vmc );
		}OPNEXT() OPCASE( ISA ){
			DaoProcess_DoCheckIsa( self, vmc );
			goto CheckException;
		}OPNEXT() OPCASE( NAMEVA ){
			DaoProcess_BindNameValue( self, vmc );
		}OPNEXT() OPCASE( PAIR ){
			DaoProcess_DoPair( self, vmc );
		}OPNEXT() OPCASE( TUPLE ){
			DaoProcess_DoTuple( self, vmc );
			goto CheckException;
		}OPNEXT() OPCASE( LIST ){
			self->activeCode = vmc;
			if( (vmc->b >> 14) == DVM_ENUM_MODE0 ){
				DaoProcess_DoList( self, vmc );
			}else{
				DaoProcess_DoAPList( self, vmc );
			}
			goto CheckException;
		}OPNEXT() OPCASE( MAP ){
			self->activeCode = vmc;
			DaoProcess_DoMap( self, vmc );
			goto CheckException;
		}OPNEXT() OPCASE( VECTOR ){
			self->activeCode = vmc;
			if( (vmc->b >> 14) == DVM_ENUM_MODE0 ){
				DaoProcess_DoVector( self, vmc );
			}else{
				DaoProcess_DoAPVector( self, vmc );
			}
			goto CheckException;
		}OPNEXT() OPCASE( MATRIX ){
			self->activeCode = vmc;
			DaoProcess_DoMatrix( self, vmc );
			goto CheckException;
		}OPNEXT() OPCASE( PACK ) OPCASE( MPACK ){
			DaoProcess_DoPacking( self, vmc );
			goto CheckException;
		}OPNEXT() OPCASE( CASE ) OPCASE( GOTO ){
			vmc = vmcBase + vmc->b;
		}OPJUMP() OPCASE( SWITCH ){
			vmc = DaoProcess_DoSwitch( self, vmc );
		}OPJUMP() OPCASE( ITER ){
			self->activeCode = vmc;
			DaoProcess_DoIter( self, vmc );
			goto CheckException;
		}OPNEXT() OPCASE( TEST ){
			vA = locVars[vmc->a];
			switch( vA->type ){
			case DAO_NONE :
				vmc = vmcBase + vmc->b; break;
			case DAO_BOOLEAN :
			case DAO_INTEGER :
				vmc = vA->xInteger.value ? vmc+1 : vmcBase + vmc->b; break;
			case DAO_FLOAT   :
				vmc = vA->xFloat.value ? vmc+1 : vmcBase + vmc->b; break;
			case DAO_ENUM  :
				if( vA->xEnum.subtype == DAO_ENUM_SYM ) goto RaiseErrorInvalidOperation;
				vmc = vA->xEnum.value ? vmc+1 : vmcBase + vmc->b;
				break;
			default :
				goto RaiseErrorInvalidOperation;
			}
		}OPJUMP() OPCASE( MATH ){
			if( DaoVM_DoMath( self, vmc, locVars[vmc->c], locVars[vmc->b] ) )
				goto RaiseErrorInvalidOperation;
		}OPNEXT() OPCASE( CALL ) OPCASE( MCALL ){
			if( vmSpace->stopit ) goto FinishProcess;
			DaoProcess_DoCall( self, vmc );
			goto CheckException;
		}OPNEXT() OPCASE( ROUTINE ){
			self->activeCode = vmc;
			DaoProcess_MakeRoutine( self, vmc );
			goto CheckException;
		}OPNEXT() OPCASE( JITC ){
			jitCallData.localValues = locVars;
			jitCallData.globalValues = glbVars->items.pVar;
			dao_jit.Execute( self, & jitCallData, vmc->a );
			if( self->exceptions->size > exceptCount ) goto CheckException;
			vmc += vmc->b;
			OPJUMP()
		}OPNEXT() OPCASE( RETURN ){
			self->activeCode = vmc;
			value = DaoProcess_DoReturn( self, vmc );
			if( self->defers->size > self->topFrame->deferBase ){
				self->topFrame->state |= DVM_FRAME_FINISHED;
				if( DaoProcess_PushDefers( self, value ) ) goto CallEntry;
			}
			if( vmSpace->stopit ) goto FinishProcess;
			goto FinishCall;
		}OPNEXT() OPCASE( YIELD ){
			int i, opb = vmc->b & 0xff;
			DaoValue **args = locVars + vmc->a;
			DaoValue *buffer[ DAO_MAX_PARAM ];

			self->activeCode = vmc;
			if( routine->routType->cbtype == NULL ){
				DaoProcess_RaiseError( self, NULL, "Not yielding in code section methods." );
				goto CheckException;
			}
			if( (vmc->b & DAO_CALL_EXPAR) && opb && args[opb-1]->type == DAO_TUPLE ){
				DaoTuple *tup = (DaoTuple*) args[ opb-1 ];
				for(i=0; i<opb-1; ++j) buffer[i] = args[i];
				opb -= 1;
				for(i=0; i<tup->size; ++i){
					if( opb >= DAO_MAX_PARAM ){
						DaoProcess_RaiseError( self, "Param", "too many parameters" );
						goto CheckException;
					}
					buffer[opb++] = tup->values[i];
				}
				args = buffer;
			}
			sect = DaoProcess_InitCodeSection( self, opb );
			if( sect == NULL ) goto FinishProcess;
			self->topFrame->state = DVM_FRAME_SECT; /* remove DVM_FRAME_KEEP; */
			for(i=0; i<sect->b; i++){
				if( i >= opb ) break;
				if( DaoProcess_SetValue( self, sect->a + i, args[i] ) == 0 ){
					DaoProcess_RaiseError( self, "Param", "invalid yield" );
				}
			}
			self->status = DAO_PROCESS_STACKED;
			goto CheckException;
		}OPNEXT() OPCASE( SECT ){
			goto ReturnFalse;
		}OPNEXT() OPCASE( DATA_B ){
			locVars[vmc->c]->xInteger.value = vmc->b != 0;
		}OPNEXT() OPCASE( DATA_I ){
			locVars[vmc->c]->xInteger.value = vmc->b;
		}OPNEXT() OPCASE( DATA_F ){
			locVars[vmc->c]->xFloat.value = vmc->b;
		}OPNEXT() OPCASE( DATA_C ){
			dao_complex *com = & locVars[vmc->c]->xComplex.value;
			com->real = 0; com->imag = vmc->b;
		}OPNEXT() OPCASE( GETCL_B ) OPCASE( GETCL_I ){
			locVars[vmc->c]->xInteger.value = dataCL[vmc->b]->xInteger.value;
		}OPNEXT() OPCASE( GETCL_F ){
			locVars[vmc->c]->xFloat.value = dataCL[vmc->b]->xFloat.value;
		}OPNEXT() OPCASE( GETCL_C ){
			locVars[vmc->c]->xComplex.value = dataCL[vmc->b]->xComplex.value;
		}OPNEXT() OPCASE( GETCK_B ) OPCASE( GETCK_I ){
			value = clsConsts->items.pConst[vmc->b]->value;;
			locVars[vmc->c]->xInteger.value = value->xInteger.value;
		}OPNEXT() OPCASE( GETCK_F ){
			value = clsConsts->items.pConst[vmc->b]->value;;
			locVars[vmc->c]->xFloat.value = value->xFloat.value;
		}OPNEXT() OPCASE( GETCK_C ){
			value = clsConsts->items.pConst[vmc->b]->value;;
			locVars[vmc->c]->xComplex.value = value->xComplex.value;
		}OPNEXT() OPCASE( GETCG_B ) OPCASE( GETCG_I ){
			value = glbConsts->items.pConst[vmc->b]->value;
			locVars[vmc->c]->xInteger.value = value->xInteger.value;
		}OPNEXT() OPCASE( GETCG_F ){
			value = glbConsts->items.pConst[vmc->b]->value;
			locVars[vmc->c]->xFloat.value = value->xFloat.value;
		}OPNEXT() OPCASE( GETCG_C ){
			value = glbConsts->items.pConst[vmc->b]->value;
			locVars[vmc->c]->xComplex.value = value->xComplex.value;
		}OPNEXT() OPCASE( GETVH_B ) OPCASE( GETVH_I ){
			locVars[vmc->c]->xInteger.value = dataVH[vmc->a]->activeValues[vmc->b]->xInteger.value;
		}OPNEXT() OPCASE( GETVH_F ){
			locVars[vmc->c]->xFloat.value = dataVH[vmc->a]->activeValues[vmc->b]->xFloat.value;
		}OPNEXT() OPCASE( GETVH_C ){
			locVars[vmc->c]->xComplex.value = dataVH[vmc->a]->activeValues[vmc->b]->xComplex.value;
		}OPNEXT() OPCASE( GETVS_B ) OPCASE( GETVS_I ){
			locVars[vmc->c]->xInteger.value = upValues[vmc->b]->value->xInteger.value;
		}OPNEXT() OPCASE( GETVS_F ){
			locVars[vmc->c]->xFloat.value = upValues[vmc->b]->value->xFloat.value;
		}OPNEXT() OPCASE( GETVS_C ){
			locVars[vmc->c]->xComplex.value = upValues[vmc->b]->value->xComplex.value;
		}OPNEXT() OPCASE( GETVO_B ) OPCASE( GETVO_I ){
			locVars[vmc->c]->xInteger.value = dataVO[vmc->b]->xInteger.value;
		}OPNEXT() OPCASE( GETVO_F ){
			locVars[vmc->c]->xFloat.value = dataVO[vmc->b]->xFloat.value;
		}OPNEXT() OPCASE( GETVO_C ){
			locVars[vmc->c]->xComplex.value = dataVO[vmc->b]->xComplex.value;
		}OPNEXT() OPCASE( GETVK_B ) OPCASE( GETVK_I ){
			LocalInt(vmc->c) = clsVars->items.pVar[vmc->b]->value->xInteger.value;
		}OPNEXT() OPCASE( GETVK_F ){
			LocalFloat(vmc->c) = clsVars->items.pVar[vmc->b]->value->xFloat.value;
		}OPNEXT() OPCASE( GETVK_C ){
			LocalComplex(vmc->c) = clsVars->items.pVar[vmc->b]->value->xComplex.value;
		}OPNEXT() OPCASE( GETVG_B ) OPCASE( GETVG_I ){
			LocalInt(vmc->c) = glbVars->items.pVar[vmc->b]->value->xInteger.value;
		}OPNEXT() OPCASE( GETVG_F ){
			LocalFloat(vmc->c) = glbVars->items.pVar[vmc->b]->value->xFloat.value;
		}OPNEXT() OPCASE( GETVG_C ){
			LocalComplex(vmc->c) = glbVars->items.pVar[vmc->b]->value->xComplex.value;
		}OPNEXT() OPCASE( SETVH_BB ) OPCASE( SETVH_II ){
			dataVH[vmc->c]->activeValues[vmc->b]->xInteger.value = LocalInt(vmc->a);
		}OPNEXT() OPCASE( SETVH_FF ){
			dataVH[vmc->c]->activeValues[vmc->b]->xFloat.value = LocalFloat(vmc->a);
		}OPNEXT() OPCASE( SETVH_CC ){
			dataVH[vmc->c]->activeValues[vmc->b]->xComplex.value = LocalComplex(vmc->a);
		}OPNEXT() OPCASE( SETVS_BB ) OPCASE( SETVS_II ){
			upValues[vmc->b]->value->xInteger.value = LocalInt(vmc->a);
		}OPNEXT() OPCASE( SETVS_FF ){
			upValues[vmc->b]->value->xFloat.value = LocalFloat(vmc->a);
		}OPNEXT() OPCASE( SETVS_CC ){
			upValues[vmc->b]->value->xComplex.value = LocalComplex(vmc->a);
		}OPNEXT() OPCASE( SETVO_BB ) OPCASE( SETVO_II ){
			dataVO[vmc->b]->xInteger.value = LocalInt(vmc->a);
		}OPNEXT() OPCASE( SETVO_FF ){
			dataVO[vmc->b]->xFloat.value = LocalFloat(vmc->a);
		}OPNEXT() OPCASE( SETVO_CC ){
			dataVO[vmc->b]->xComplex.value = LocalComplex(vmc->a);
		}OPNEXT() OPCASE( SETVK_BB ) OPCASE( SETVK_II ){
			clsVars->items.pVar[vmc->b]->value->xInteger.value = LocalInt(vmc->a);
		}OPNEXT() OPCASE( SETVK_FF ){
			clsVars->items.pVar[vmc->b]->value->xFloat.value = LocalFloat(vmc->a);
		}OPNEXT() OPCASE( SETVK_CC ){
			clsVars->items.pVar[vmc->b]->value->xComplex.value = LocalComplex(vmc->a);
		}OPNEXT() OPCASE( SETVG_BB ) OPCASE( SETVG_II ){
			glbVars->items.pVar[vmc->b]->value->xInteger.value = LocalInt(vmc->a);
		}OPNEXT() OPCASE( SETVG_FF ){
			glbVars->items.pVar[vmc->b]->value->xFloat.value = LocalFloat(vmc->a);
		}OPNEXT() OPCASE( SETVG_CC ){
			glbVars->items.pVar[vmc->b]->value->xComplex.value = LocalComplex(vmc->a);
		}OPNEXT() OPCASE( MOVE_BB ){
			LocalInt(vmc->c) = LocalInt(vmc->a) != 0;
		}OPNEXT() OPCASE( MOVE_II ){
			LocalInt(vmc->c) = LocalInt(vmc->a);
		}OPNEXT() OPCASE( ADD_III ){
			LocalInt(vmc->c) = LocalInt(vmc->a) + LocalInt(vmc->b);
		}OPNEXT() OPCASE( SUB_III ){
			LocalInt(vmc->c) = LocalInt(vmc->a) - LocalInt(vmc->b);
		}OPNEXT() OPCASE( MUL_III ){
			LocalInt(vmc->c) = LocalInt(vmc->a) * LocalInt(vmc->b);
		}OPNEXT() OPCASE( DIV_III ){
			inum = LocalInt(vmc->b);
			if( inum == 0 ) goto RaiseErrorDivByZero;
			LocalInt(vmc->c) = LocalInt(vmc->a) / inum;
		}OPNEXT() OPCASE( MOD_III ){
			inum = LocalInt(vmc->b);
			if( inum == 0 ) goto RaiseErrorDivByZero;
			LocalInt(vmc->c) = LocalInt(vmc->a) % inum;
		}OPNEXT() OPCASE( POW_III ){
			LocalInt(vmc->c) = pow( LocalInt(vmc->a), LocalInt(vmc->b) );
		}OPNEXT() OPCASE( AND_BBB ) OPCASE( AND_BII ){
			LocalInt(vmc->c) = LocalInt(vmc->b) && LocalInt(vmc->a);
		}OPNEXT() OPCASE( OR_BBB ) OPCASE( OR_BII ){
			LocalInt(vmc->c) = LocalInt(vmc->a) || LocalInt(vmc->b);
		}OPNEXT() OPCASE( LT_BBB ) OPCASE( LT_BII ){
			LocalInt(vmc->c) = LocalInt(vmc->a) < LocalInt(vmc->b);
		}OPNEXT() OPCASE( LE_BBB ) OPCASE( LE_BII ){
			LocalInt(vmc->c) = LocalInt(vmc->a) <= LocalInt(vmc->b);
		}OPNEXT() OPCASE( EQ_BBB ) OPCASE( EQ_BII ){
			LocalInt(vmc->c) = LocalInt(vmc->a) == LocalInt(vmc->b);
		}OPNEXT() OPCASE( NE_BBB ) OPCASE( NE_BII ){
			LocalInt(vmc->c) = LocalInt(vmc->a) != LocalInt(vmc->b);
		}OPNEXT() OPCASE( BITAND_III ){
			LocalInt(vmc->c) = LocalInt(vmc->a) & LocalInt(vmc->b);
		}OPNEXT() OPCASE( BITOR_III ){
			LocalInt(vmc->c) = LocalInt(vmc->a) | LocalInt(vmc->b);
		}OPNEXT() OPCASE( BITXOR_III ){
			LocalInt(vmc->c) = LocalInt(vmc->a) ^ LocalInt(vmc->b);
		}OPNEXT() OPCASE( BITLFT_III ){
			LocalInt(vmc->c) = LocalInt(vmc->a) << LocalInt(vmc->b);
		}OPNEXT() OPCASE( BITRIT_III ){
			LocalInt(vmc->c) = LocalInt(vmc->a) >> LocalInt(vmc->b);
		}OPNEXT() OPCASE( TILDE_I ){
			LocalInt(vmc->c) = ~ LocalInt(vmc->a);
		}OPNEXT() OPCASE( TILDE_C ){
			vA = locVars[vmc->a];
			vC = locVars[vmc->c];
			vC->xComplex.value.real =   vA->xComplex.value.real;
			vC->xComplex.value.imag = - vA->xComplex.value.imag;
		}OPNEXT() OPCASE( MOVE_FF ){
			LocalFloat(vmc->c) = LocalFloat(vmc->a);
		}OPNEXT() OPCASE( ADD_FFF ){
			LocalFloat(vmc->c) = LocalFloat(vmc->a) + LocalFloat(vmc->b);
		}OPNEXT() OPCASE( SUB_FFF ){
			LocalFloat(vmc->c) = LocalFloat(vmc->a) - LocalFloat(vmc->b);
		}OPNEXT() OPCASE( MUL_FFF ){
			LocalFloat(vmc->c) = LocalFloat(vmc->a) * LocalFloat(vmc->b);
		}OPNEXT() OPCASE( DIV_FFF ){
			LocalFloat(vmc->c) = LocalFloat(vmc->a) / LocalFloat(vmc->b);
		}OPNEXT() OPCASE( MOD_FFF ){
			fnum = LocalFloat(vmc->b);
			if( fnum == 0.0 ) goto RaiseErrorDivByZero;
			inum = (dao_integer)(LocalFloat(vmc->a) / fnum);
			LocalFloat(vmc->c) = LocalFloat(vmc->a) - inum * fnum;
		}OPNEXT() OPCASE( POW_FFF ){
			LocalFloat(vmc->c) = pow( LocalFloat(vmc->a), LocalFloat(vmc->b) );
		}OPNEXT() OPCASE( AND_BFF ){
			fnum = LocalFloat(vmc->a);
			LocalInt(vmc->c) = fnum ? LocalFloat(vmc->b) : fnum;
		}OPNEXT() OPCASE( OR_BFF ){
			fnum = LocalFloat(vmc->a);
			LocalInt(vmc->c) = fnum ? fnum : LocalFloat(vmc->b);
		}OPNEXT() OPCASE( LT_BFF ){
			LocalInt(vmc->c) = LocalFloat(vmc->a) < LocalFloat(vmc->b);
		}OPNEXT() OPCASE( LE_BFF ){
			LocalInt(vmc->c) = LocalFloat(vmc->a) <= LocalFloat(vmc->b);
		}OPNEXT() OPCASE( EQ_BFF ){
			LocalInt(vmc->c) = LocalFloat(vmc->a) == LocalFloat(vmc->b);
		}OPNEXT() OPCASE( NE_BFF ){
			LocalInt(vmc->c) = LocalFloat(vmc->a) != LocalFloat(vmc->b);
		}OPNEXT() OPCASE( ADD_SSS ){
			vA = locVars[vmc->a];  vB = locVars[vmc->b];
			vC = locVars[vmc->c];
			if( vmc->a == vmc->c ){
				DString_Append( vA->xString.value, vB->xString.value );
			}else if( vmc->b == vmc->c ){
				DString_Insert( vB->xString.value, vA->xString.value, 0, 0, 0 );
			}else{
				DString_Assign( vC->xString.value, vA->xString.value );
				DString_Append( vC->xString.value, vB->xString.value );
			}
		}OPNEXT() OPCASE( LT_BSS ){
			vA = locVars[vmc->a];  vB = locVars[vmc->b];
			LocalInt(vmc->c) = DString_CompareUTF8( vA->xString.value, vB->xString.value )<0;
		}OPNEXT() OPCASE( LE_BSS ){
			vA = locVars[vmc->a];  vB = locVars[vmc->b];
			LocalInt(vmc->c) = DString_CompareUTF8( vA->xString.value, vB->xString.value )<=0;
		}OPNEXT() OPCASE( EQ_BSS ){
			vA = locVars[vmc->a];  vB = locVars[vmc->b];
			LocalInt(vmc->c) = DString_Compare( vA->xString.value, vB->xString.value )==0;
		}OPNEXT() OPCASE( NE_BSS ){
			vA = locVars[vmc->a];  vB = locVars[vmc->b];
			LocalInt(vmc->c) = DString_Compare( vA->xString.value, vB->xString.value )!=0;
		}OPNEXT() OPCASE( MOVE_BI ){
			LocalInt(vmc->c) = LocalInt(vmc->a) != 0;
		}OPNEXT() OPCASE( MOVE_BF ){
			LocalInt(vmc->c) = LocalFloat(vmc->a) != 0.0;
		}OPNEXT() OPCASE( MOVE_IB ){
			LocalInt(vmc->c) = LocalInt(vmc->a) != 0;
		}OPNEXT() OPCASE( MOVE_IF ){
			LocalInt(vmc->c) = LocalFloat(vmc->a);
		}OPNEXT() OPCASE( MOVE_FB ){
			LocalFloat(vmc->c) = LocalInt(vmc->a) != 0;
		}OPNEXT() OPCASE( MOVE_FI ){
			LocalFloat(vmc->c) = LocalInt(vmc->a);
		}OPNEXT() OPCASE( MOVE_CF ){
			locVars[vmc->c]->xComplex.value.real = locVars[vmc->a]->xFloat.value;
			locVars[vmc->c]->xComplex.value.imag = 0.0;
		}OPNEXT() OPCASE( MOVE_CC ){
			LocalComplex(vmc->c) = LocalComplex(vmc->a);
		}OPNEXT() OPCASE( MOVE_SS ){
			DString_Assign( locVars[vmc->c]->xString.value, locVars[vmc->a]->xString.value );
		}OPNEXT() OPCASE( MOVE_PP ){
			value = locVars[vmc->a];
			switch( value->type ){
			case DAO_OBJECT : if( value->xObject.isNull ) value = NULL; break;
			case DAO_CDATA  : if( value->xCdata.data == NULL ) value = NULL; break;
			}
			if( value == NULL ) goto RaiseErrorNullObject;
			GC_Assign( & locVars[vmc->c], value );
		}OPNEXT() OPCASE( MOVE_XX ){
			value = locVars[vmc->a];
			switch( value->type ){
			case DAO_OBJECT : if( value->xObject.isNull ) value = NULL; break;
			case DAO_CDATA  : if( value->xCdata.data == NULL ) value = NULL; break;
			}
			if( value == NULL ) goto RaiseErrorNullObject;
			DaoValue_CopyX( value, locVars + vmc->c, locTypes[vmc->c] );
		}OPNEXT() OPCASE( NOT_B ) OPCASE( NOT_I ){
			LocalInt(vmc->c) = ! LocalInt(vmc->a);
		}OPNEXT() OPCASE( NOT_F ){
			LocalInt(vmc->c) = ! LocalFloat(vmc->a);
		}OPNEXT() OPCASE( MINUS_I ){
			LocalInt(vmc->c) = - LocalInt(vmc->a);
		}OPNEXT() OPCASE( MINUS_F ){
			LocalFloat(vmc->c) = - LocalFloat(vmc->a);
		}OPNEXT() OPCASE( MINUS_C ){
			acom = LocalComplex(vmc->a);
			vC = locVars[vmc->c];
			vC->xComplex.value.real = - acom.real;
			vC->xComplex.value.imag = - acom.imag;
		}OPNEXT() OPCASE( ADD_CCC ){
			acom = LocalComplex(vmc->a);  bcom = LocalComplex(vmc->b);
			vC = locVars[vmc->c];
			vC->xComplex.value.real = acom.real + bcom.real;
			vC->xComplex.value.imag = acom.imag + bcom.imag;
		}OPNEXT() OPCASE( SUB_CCC ){
			acom = LocalComplex(vmc->a);  bcom = LocalComplex(vmc->b);
			vC = locVars[vmc->c];
			vC->xComplex.value.real = acom.real - bcom.real;
			vC->xComplex.value.imag = acom.imag - bcom.imag;
		}OPNEXT() OPCASE( MUL_CCC ){
			acom = LocalComplex(vmc->a);  bcom = LocalComplex(vmc->b);
			vC = locVars[vmc->c];
			vC->xComplex.value.real = acom.real * bcom.real - acom.imag * bcom.imag;
			vC->xComplex.value.imag = acom.real * bcom.imag + acom.imag * bcom.real;
		}OPNEXT() OPCASE( DIV_CCC ){
			acom = LocalComplex(vmc->a);  bcom = LocalComplex(vmc->b);
			vC = locVars[vmc->c];
			dnum = bcom.real * bcom.real + bcom.imag * bcom.imag;
			vC->xComplex.value.real = (acom.real*bcom.real + acom.imag*bcom.imag) / dnum;
			vC->xComplex.value.imag = (acom.imag*bcom.real - acom.real*bcom.imag) / dnum;
		}OPNEXT() OPCASE( EQ_BCC ){
			dao_complex *ca = & locVars[vmc->a]->xComplex.value;
			dao_complex *cb = & locVars[vmc->b]->xComplex.value;
			LocalInt(vmc->c) = ca->real == cb->real && ca->imag == cb->imag;
		}OPNEXT() OPCASE( NE_BCC ){
			dao_complex *ca = & locVars[vmc->a]->xComplex.value;
			dao_complex *cb = & locVars[vmc->b]->xComplex.value;
			LocalInt(vmc->c) = ca->real != cb->real || ca->imag != cb->imag;
		}OPNEXT() OPCASE( GETI_SI ){
			str = locVars[vmc->a]->xString.value;
			id = LocalInt(vmc->b);
			if( id <0 ) id += str->size;
			if( id <0 || id >= str->size ) goto RaiseErrorIndexOutOfRange;
			LocalInt(vmc->c) = (uchar_t) str->chars[id];
		}OPNEXT() OPCASE( SETI_SII ){
			str = locVars[vmc->c]->xString.value;
			id = LocalInt(vmc->b);
			inum = LocalInt(vmc->a);
			if( id <0 ) id += str->size;
			if( id <0 || id >= str->size ) goto RaiseErrorIndexOutOfRange;
			DString_Detach( str, str->size );
			str->chars[id] = (char) inum;
		}OPNEXT() OPCASE( GETI_LI ){
			list = & locVars[vmc->a]->xList;
			id = LocalInt(vmc->b);
			if( id <0 ) id += list->value->size;
			if( id <0 || id >= list->value->size ) goto RaiseErrorIndexOutOfRange;
			/* All GETX instructions assume the C regisgter is an intermediate register! */
			/* So no type checking is necessary here! */
			value = list->value->items.pValue[id];
			GC_Assign( & locVars[vmc->c], value );
		}OPNEXT() OPCASE( SETI_LI ){
			list = & locVars[vmc->c]->xList;
			id = LocalInt(vmc->b);
			if( id <0 ) id += list->value->size;
			if( id <0 || id >= list->value->size ) goto RaiseErrorIndexOutOfRange;
			DaoValue_Copy( locVars[vmc->a], list->value->items.pValue + id );
		}OPNEXT()
		OPCASE( GETI_LBI )
		OPCASE( GETI_LII )
		OPCASE( GETI_LFI )
		OPCASE( GETI_LCI )
		OPCASE( GETI_LSI ){
			list = & locVars[vmc->a]->xList;
			id = LocalInt(vmc->b);
			if( id <0 ) id += list->value->size;
			if( id <0 || id >= list->value->size ) goto RaiseErrorIndexOutOfRange;
			vA = list->value->items.pValue[id];
			switch( vmc->code ){
			case DVM_GETI_LSI :
				GC_Assign( & locVars[vmc->c], vA );
				break;
			case DVM_GETI_LBI :
			case DVM_GETI_LII : locVars[vmc->c]->xInteger.value = vA->xInteger.value; break;
			case DVM_GETI_LFI : locVars[vmc->c]->xFloat.value = vA->xFloat.value; break;
			case DVM_GETI_LCI : locVars[vmc->c]->xComplex.value = vA->xComplex.value; break;
			}
			}OPNEXT()
		OPCASE( SETI_LBIB )
		OPCASE( SETI_LIII ){
			list = & locVars[vmc->c]->xList;
			id = LocalInt(vmc->b);
			if( id <0 ) id += list->value->size;
			if( id <0 || id >= list->value->size ) goto RaiseErrorIndexOutOfRange;
			list->value->items.pValue[id]->xInteger.value = locVars[vmc->a]->xInteger.value;
		}OPNEXT() OPCASE( SETI_LFIF ){
			list = & locVars[vmc->c]->xList;
			id = LocalInt(vmc->b);
			if( id <0 ) id += list->value->size;
			if( id <0 || id >= list->value->size ) goto RaiseErrorIndexOutOfRange;
			list->value->items.pValue[id]->xFloat.value = locVars[vmc->a]->xFloat.value;
		}OPNEXT() OPCASE( SETI_LCIC ){
			list = & locVars[vmc->c]->xList;
			id = LocalInt(vmc->b);
			if( id <0 ) id += list->value->size;
			if( id <0 || id >= list->value->size ) goto RaiseErrorIndexOutOfRange;
			list->value->items.pValue[id]->xComplex.value = locVars[vmc->a]->xComplex.value;
		}OPNEXT() OPCASE( SETI_LSIS ){
			list = & locVars[vmc->c]->xList;
			vA = locVars[vmc->a];
			id = LocalInt(vmc->b);
			if( id <0 ) id += list->value->size;
			if( id <0 || id >= list->value->size ) goto RaiseErrorIndexOutOfRange;
			DString_Assign( list->value->items.pValue[id]->xString.value, vA->xString.value );
		}OPNEXT()
#ifdef DAO_WITH_NUMARRAY
		OPCASE( GETI_ABI ) OPCASE( GETI_AII ) OPCASE( GETI_AFI ) OPCASE( GETI_ACI ){
			array = & locVars[vmc->a]->xArray;
			id = LocalInt(vmc->b);
			if( array->original && DaoArray_Sliced( array ) == 0 ) goto RaiseErrorSlicing;
			if( id <0 ) id += array->size;
			if( id <0 || id >= array->size ) goto RaiseErrorIndexOutOfRange;
			switch( vmc->code ){
			case DVM_GETI_ABI : LocalInt(vmc->c) = array->data.b[id]; break;
			case DVM_GETI_AII : LocalInt(vmc->c) = array->data.i[id]; break;
			case DVM_GETI_AFI : LocalFloat(vmc->c) = array->data.f[id]; break;
			case DVM_GETI_ACI : LocalComplex(vmc->c) = array->data.c[id]; break;
			}

		}OPNEXT() OPCASE(SETI_ABIB) OPCASE(SETI_AIII) OPCASE(SETI_AFIF) OPCASE(SETI_ACIC){
			array = & locVars[vmc->c]->xArray;
			id = LocalInt(vmc->b);
			if( array->original && DaoArray_Sliced( array ) == 0 ) goto RaiseErrorSlicing;
			if( id <0 ) id += array->size;
			if( id <0 || id >= array->size ) goto RaiseErrorIndexOutOfRange;
			switch( vmc->code ){
			case DVM_SETI_ABIB : array->data.b[id] = locVars[vmc->a]->xBoolean.value; break;
			case DVM_SETI_AIII : array->data.i[id] = locVars[vmc->a]->xInteger.value; break;
			case DVM_SETI_AFIF : array->data.f[id] = locVars[vmc->a]->xFloat.value; break;
			case DVM_SETI_ACIC : array->data.c[id] = locVars[vmc->a]->xComplex.value; break;
			}

		}OPNEXT() OPCASE(GETMI_ABI) OPCASE(GETMI_AII) OPCASE(GETMI_AFI) OPCASE(GETMI_ACI){
			array = & locVars[vmc->a]->xArray;
			if( array->original && DaoArray_Sliced( array ) == 0 ) goto RaiseErrorSlicing;
			id = DaoArray_ComputeIndex( array, locVars + vmc->a + 1, vmc->b );
			if( id < 0 ) goto RaiseErrorIndexOutOfRange;
			switch( vmc->code ){
			case DVM_GETMI_ABI: locVars[vmc->c]->xBoolean.value = array->data.b[id]; break;
			case DVM_GETMI_AII: locVars[vmc->c]->xInteger.value = array->data.i[id]; break;
			case DVM_GETMI_AFI: locVars[vmc->c]->xFloat.value = array->data.f[id]; break;
			case DVM_GETMI_ACI: locVars[vmc->c]->xComplex.value = array->data.c[id]; break;
			}

		}OPNEXT() OPCASE(SETMI_ABIB) OPCASE(SETMI_AIII) OPCASE(SETMI_AFIF) OPCASE(SETMI_ACIC){
			array = & locVars[vmc->c]->xArray;
			if( array->original && DaoArray_Sliced( array ) == 0 ) goto RaiseErrorSlicing;
			id = DaoArray_ComputeIndex( array, locVars + vmc->c + 1, vmc->b  );
			if( id < 0 ) goto RaiseErrorIndexOutOfRange;
			switch( vmc->code ){
			case DVM_SETMI_ABIB: array->data.b[id] = locVars[vmc->a]->xBoolean.value; break;
			case DVM_SETMI_AIII: array->data.i[id] = locVars[vmc->a]->xInteger.value; break;
			case DVM_SETMI_AFIF: array->data.f[id] = locVars[vmc->a]->xFloat.value; break;
			case DVM_SETMI_ACIC: array->data.c[id] = locVars[vmc->a]->xComplex.value; break;
			}
		}OPNEXT()
#else
		OPCASE( GETI_ABI ) OPCASE( GETI_AII ) OPCASE( GETI_AFI ) OPCASE( GETI_ACI )
		OPCASE( SETI_ABIB ) OPCASE( SETI_AIII ) OPCASE( SETI_AFIF ) OPCASE( SETI_ACIC )
		OPCASE( GETMI_ABI ) OPCASE( GETMI_AII ) OPCASE( GETMI_AFI ) OPCASE( GETMI_ACI )
		OPCASE( SETMI_ABIB ) OPCASE( SETMI_AIII ) OPCASE( SETMI_AFIF ) OPCASE( SETMI_ACIC )
			{
				self->activeCode = vmc;
				DaoProcess_RaiseError( self, NULL, "numeric array is disabled" );
			}OPNEXT()
#endif
		OPCASE( GETI_TI ){
			tuple = & locVars[vmc->a]->xTuple;
			id = LocalInt(vmc->b);
			if( id <0 || id >= tuple->size ) goto RaiseErrorIndexOutOfRange;
			value = tuple->values[id];
			GC_Assign( & locVars[vmc->c], value );
		}OPNEXT() OPCASE( SETI_TI ){
			tuple = & locVars[vmc->c]->xTuple;
			id = LocalInt(vmc->b);
			abtp = NULL;
			if( id <0 || id >= tuple->size ) goto RaiseErrorIndexOutOfRange;
			self->activeCode = vmc;
			abtp = tuple->ctype->nested->items.pType[id];
			if( abtp->tid == DAO_PAR_NAMED ) abtp = & abtp->aux->xType;
			if( DaoProcess_Move( self, locVars[vmc->a], tuple->values + id, abtp ) ==0 )
				goto CheckException;
		}OPNEXT() OPCASE( GETF_TB ) OPCASE( GETF_TI ){
			/*
			// Do not get reference here!
			// Getting reference is always more expensive due to reference counting.
			// The compiler always generates SETX, if element modification is done
			// through index or field accessing: A[B] += C, A.B += C.
			*/
			tuple = & locVars[vmc->a]->xTuple;
			locVars[vmc->c]->xInteger.value = tuple->values[vmc->b]->xInteger.value;
		}OPNEXT() OPCASE( GETF_TF ){
			tuple = & locVars[vmc->a]->xTuple;
			locVars[vmc->c]->xFloat.value = tuple->values[vmc->b]->xFloat.value;
		}OPNEXT() OPCASE( GETF_TC ){
			tuple = & locVars[vmc->a]->xTuple;
			locVars[vmc->c]->xComplex.value = tuple->values[vmc->b]->xComplex.value;
		}OPNEXT() OPCASE( GETF_TX ){
			tuple = & locVars[vmc->a]->xTuple;
			value = tuple->values[vmc->b];
			GC_Assign( & locVars[vmc->c], value );
		}OPNEXT() OPCASE( SETF_TBB ) OPCASE( SETF_TII ){
			tuple = & locVars[vmc->c]->xTuple;
			tuple->values[vmc->b]->xInteger.value = LocalInt(vmc->a);
		}OPNEXT() OPCASE( SETF_TFF ){
			tuple = & locVars[vmc->c]->xTuple;
			tuple->values[vmc->b]->xFloat.value = LocalFloat(vmc->a);
		}OPNEXT() OPCASE( SETF_TCC ){
			tuple = & locVars[vmc->c]->xTuple;
			tuple->values[vmc->b]->xComplex.value = LocalComplex(vmc->a);
		}OPNEXT() OPCASE( SETF_TSS ){
			tuple = & locVars[vmc->c]->xTuple;
			vA = locVars[vmc->a];
			DString_Assign( tuple->values[vmc->b]->xString.value, vA->xString.value );
		}OPNEXT() OPCASE( SETF_TPP ){
			tuple = & locVars[vmc->c]->xTuple;
			value = locVars[vmc->a];
			GC_Assign( & tuple->values[vmc->b], value );
		}OPNEXT() OPCASE( SETF_TXX ){
			tuple = & locVars[vmc->c]->xTuple;
			DaoValue_Copy( locVars[vmc->a], tuple->values + vmc->b );
		}OPNEXT() OPCASE( GETF_CX ){
			double *RI = (double*)(dao_complex*) & locVars[vmc->a]->xComplex.value;
			locVars[vmc->c]->xFloat.value = RI[vmc->b];
		}OPNEXT() OPCASE( SETF_CX ){
			double *RI = (double*)(dao_complex*) & locVars[vmc->c]->xComplex.value;
			RI[vmc->b] = locVars[vmc->a]->xFloat.value;
		}OPNEXT() OPCASE( GETF_KC ){
			value = locVars[vmc->a]->xClass.constants->items.pConst[vmc->b]->value;
			GC_Assign( & locVars[vmc->c], value );
		}OPNEXT() OPCASE( GETF_KG ){
			value = locVars[vmc->a]->xClass.variables->items.pVar[vmc->b]->value;
			GC_Assign( & locVars[vmc->c], value );
		}OPNEXT() OPCASE( GETF_OC ){
			value = locVars[vmc->a]->xObject.defClass->constants->items.pConst[vmc->b]->value;
			GC_Assign( & locVars[vmc->c], value );
		}OPNEXT() OPCASE( GETF_OG ){
			value = locVars[vmc->a]->xObject.defClass->variables->items.pVar[vmc->b]->value;
			GC_Assign( & locVars[vmc->c], value );
		}OPNEXT() OPCASE( GETF_OV ){
			object = & locVars[vmc->a]->xObject;
			if( object->isNull ) goto AccessNullInstance;
			value = object->objValues[vmc->b];
			GC_Assign( & locVars[vmc->c], value );
		}OPNEXT() OPCASE( GETF_KCB ) OPCASE( GETF_KCI ){
			value = locVars[vmc->a]->xClass.constants->items.pConst[vmc->b]->value;
			locVars[vmc->c]->xInteger.value = value->xInteger.value;
		}OPNEXT() OPCASE( GETF_KCF ){
			value = locVars[vmc->a]->xClass.constants->items.pConst[vmc->b]->value;
			locVars[vmc->c]->xFloat.value = value->xFloat.value;
		}OPNEXT() OPCASE( GETF_KCC ){
			value = locVars[vmc->a]->xClass.constants->items.pConst[vmc->b]->value;
			locVars[vmc->c]->xComplex.value = value->xComplex.value;
		}OPNEXT() OPCASE( GETF_KGB ) OPCASE( GETF_KGI ){
			value = locVars[vmc->a]->xClass.variables->items.pVar[vmc->b]->value;
			locVars[vmc->c]->xInteger.value = value->xInteger.value;
		}OPNEXT() OPCASE( GETF_KGF ){
			value = locVars[vmc->a]->xClass.variables->items.pVar[vmc->b]->value;
			locVars[vmc->c]->xFloat.value = value->xFloat.value;
		}OPNEXT() OPCASE( GETF_KGC ){
			value = locVars[vmc->a]->xClass.variables->items.pVar[vmc->b]->value;
			locVars[vmc->c]->xComplex.value = value->xComplex.value;
		}OPNEXT() OPCASE( GETF_OCB ) OPCASE( GETF_OCI ){
			value = locVars[vmc->a]->xObject.defClass->constants->items.pConst[vmc->b]->value;
			locVars[vmc->c]->xInteger.value = value->xInteger.value;
		}OPNEXT() OPCASE( GETF_OCF ){
			value = locVars[vmc->a]->xObject.defClass->constants->items.pConst[vmc->b]->value;
			locVars[vmc->c]->xFloat.value = value->xFloat.value;
		}OPNEXT() OPCASE( GETF_OCC ){
			value = locVars[vmc->a]->xObject.defClass->constants->items.pConst[vmc->b]->value;
			locVars[vmc->c]->xComplex.value = value->xComplex.value;
		}OPNEXT() OPCASE( GETF_OGB ) OPCASE( GETF_OGI ){
			value = locVars[vmc->a]->xObject.defClass->variables->items.pVar[vmc->b]->value;
			locVars[vmc->c]->xInteger.value = value->xInteger.value;
		}OPNEXT() OPCASE( GETF_OGF ){
			value = locVars[vmc->a]->xObject.defClass->variables->items.pVar[vmc->b]->value;
			locVars[vmc->c]->xFloat.value = value->xFloat.value;
		}OPNEXT() OPCASE( GETF_OGC ){
			value = locVars[vmc->a]->xObject.defClass->variables->items.pVar[vmc->b]->value;
			locVars[vmc->c]->xComplex.value = value->xComplex.value;
		}OPNEXT() OPCASE( GETF_OVB ) OPCASE( GETF_OVI ){
			object = & locVars[vmc->a]->xObject;
			if( object->isNull ) goto AccessNullInstance;
			locVars[vmc->c]->xInteger.value = object->objValues[vmc->b]->xInteger.value;
		}OPNEXT() OPCASE( GETF_OVF ){
			object = & locVars[vmc->a]->xObject;
			if( object->isNull ) goto AccessNullInstance;
			locVars[vmc->c]->xFloat.value = object->objValues[vmc->b]->xFloat.value;
		}OPNEXT() OPCASE( GETF_OVC ){
			object = & locVars[vmc->a]->xObject;
			if( object->isNull ) goto AccessNullInstance;
			locVars[vmc->c]->xComplex.value = object->objValues[vmc->b]->xComplex.value;
		}OPNEXT() OPCASE( SETF_KG ){
			klass = & locVars[vmc->c]->xClass;
			DaoValue_Copy( locVars[vmc->a], & klass->variables->items.pVar[vmc->b]->value );
		}OPNEXT() OPCASE( SETF_OG ){
			klass = locVars[vmc->c]->xObject.defClass;
			DaoValue_Copy( locVars[vmc->a], & klass->variables->items.pVar[vmc->b]->value );
		}OPNEXT() OPCASE( SETF_OV ){
			object = & locVars[vmc->c]->xObject;
			if( object->isNull ) goto AccessNullInstance;
			DaoValue_Copy( locVars[vmc->a], object->objValues + vmc->b );
		}OPNEXT() OPCASE( SETF_KGBB ) OPCASE( SETF_KGII ){
			klass = & locVars[vmc->c]->xClass;
			klass->variables->items.pVar[vmc->b]->value->xInteger.value = LocalInt(vmc->a);
		}OPNEXT() OPCASE( SETF_KGFF ){
			klass = & locVars[vmc->c]->xClass;
			klass->variables->items.pVar[vmc->b]->value->xFloat.value = LocalFloat(vmc->a);
		}OPNEXT() OPCASE( SETF_KGCC ){
			klass = & locVars[vmc->c]->xClass;
			klass->variables->items.pVar[vmc->b]->value->xComplex.value = LocalComplex(vmc->a);
		}OPNEXT() OPCASE( SETF_OGBB ) OPCASE( SETF_OGII ){
			klass = locVars[vmc->c]->xObject.defClass;
			klass->variables->items.pVar[vmc->b]->value->xInteger.value = LocalInt(vmc->a);
		}OPNEXT() OPCASE( SETF_OGFF ){
			klass = locVars[vmc->c]->xObject.defClass;
			klass->variables->items.pVar[vmc->b]->value->xFloat.value = LocalFloat(vmc->a);
		}OPNEXT() OPCASE( SETF_OGCC ){
			klass = locVars[vmc->c]->xObject.defClass;
			klass->variables->items.pVar[vmc->b]->value->xComplex.value = LocalComplex(vmc->a);
		}OPNEXT() OPCASE( SETF_OVBB ) OPCASE( SETF_OVII ){
			object = (DaoObject*) locVars[vmc->c];
			if( object->isNull ) goto AccessNullInstance;
			object->objValues[vmc->b]->xInteger.value = LocalInt(vmc->a);
		}OPNEXT() OPCASE( SETF_OVFF ){
			object = (DaoObject*) locVars[vmc->c];
			if( object->isNull ) goto AccessNullInstance;
			object->objValues[vmc->b]->xFloat.value = LocalFloat(vmc->a);
		}OPNEXT() OPCASE( SETF_OVCC ){
			object = (DaoObject*) locVars[vmc->c];
			if( object->isNull ) goto AccessNullInstance;
			object->objValues[vmc->b]->xComplex.value = LocalComplex(vmc->a);
		}OPNEXT() OPCASE( TEST_B ) OPCASE( TEST_I ){
			vmc = LocalInt(vmc->a) ? vmc+1 : vmcBase+vmc->b;
		}OPJUMP() OPCASE( TEST_F ){
			vmc = LocalFloat(vmc->a) ? vmc+1 : vmcBase+vmc->b;
		}OPJUMP() OPCASE( MATH_B ) OPCASE( MATH_I ){
			switch( vmc->a ){
			case DVM_MATH_CEIL : LocalInt(vmc->c) = ceil( LocalInt(vmc->b) ); break;
			case DVM_MATH_FLOOR: LocalInt(vmc->c) = floor( LocalInt(vmc->b) ); break;
			case DVM_MATH_ABS  : LocalInt(vmc->c) = abs( LocalInt(vmc->b) );  break;
			case DVM_MATH_ACOS : LocalFloat(vmc->c) = acos( LocalInt(vmc->b) ); break;
			case DVM_MATH_ASIN : LocalFloat(vmc->c) = asin( LocalInt(vmc->b) ); break;
			case DVM_MATH_ATAN : LocalFloat(vmc->c) = atan( LocalInt(vmc->b) ); break;
			case DVM_MATH_COS  : LocalFloat(vmc->c) = cos( LocalInt(vmc->b) );  break;
			case DVM_MATH_COSH : LocalFloat(vmc->c) = cosh( LocalInt(vmc->b) ); break;
			case DVM_MATH_EXP  : LocalFloat(vmc->c) = exp( LocalInt(vmc->b) );  break;
			case DVM_MATH_LOG  : LocalFloat(vmc->c) = log( LocalInt(vmc->b) );  break;
			case DVM_MATH_SIN  : LocalFloat(vmc->c) = sin( LocalInt(vmc->b) );  break;
			case DVM_MATH_SINH : LocalFloat(vmc->c) = sinh( LocalInt(vmc->b) ); break;
			case DVM_MATH_SQRT : LocalFloat(vmc->c) = sqrt( LocalInt(vmc->b) ); break;
			case DVM_MATH_TAN  : LocalFloat(vmc->c) = tan( LocalInt(vmc->b) );  break;
			case DVM_MATH_TANH : LocalFloat(vmc->c) = tanh( LocalInt(vmc->b) ); break;
			default : break;
			}
		}OPNEXT() OPCASE( MATH_F ){
			switch( vmc->a ){
			case DVM_MATH_CEIL : LocalFloat(vmc->c) = ceil( LocalFloat(vmc->b) ); break;
			case DVM_MATH_FLOOR : LocalFloat(vmc->c) = floor( LocalFloat(vmc->b) ); break;
			case DVM_MATH_ABS  : LocalFloat(vmc->c) = fabs( LocalFloat(vmc->b) );  break;
			case DVM_MATH_ACOS : LocalFloat(vmc->c) = acos( LocalFloat(vmc->b) ); break;
			case DVM_MATH_ASIN : LocalFloat(vmc->c) = asin( LocalFloat(vmc->b) ); break;
			case DVM_MATH_ATAN : LocalFloat(vmc->c) = atan( LocalFloat(vmc->b) ); break;
			case DVM_MATH_COS  : LocalFloat(vmc->c) = cos( LocalFloat(vmc->b) );  break;
			case DVM_MATH_COSH : LocalFloat(vmc->c) = cosh( LocalFloat(vmc->b) ); break;
			case DVM_MATH_EXP  : LocalFloat(vmc->c) = exp( LocalFloat(vmc->b) );  break;
			case DVM_MATH_LOG  : LocalFloat(vmc->c) = log( LocalFloat(vmc->b) );  break;
			case DVM_MATH_SIN  : LocalFloat(vmc->c) = sin( LocalFloat(vmc->b) );  break;
			case DVM_MATH_SINH : LocalFloat(vmc->c) = sinh( LocalFloat(vmc->b) ); break;
			case DVM_MATH_SQRT : LocalFloat(vmc->c) = sqrt( LocalFloat(vmc->b) ); break;
			case DVM_MATH_TAN  : LocalFloat(vmc->c) = tan( LocalFloat(vmc->b) );  break;
			case DVM_MATH_TANH : LocalFloat(vmc->c) = tanh( LocalFloat(vmc->b) ); break;
			default : break;
			}
		}OPNEXT() OPCASE( CAST_B ) OPCASE( CAST_I ) OPCASE( CAST_F )
		OPCASE( CAST_C ) OPCASE( CAST_S ) OPCASE( CAST_VE ) {
			vA = locVars[vmc->a];
			vC = locVars[vmc->c];
			if( vA->type == DAO_BOOLEAN + (vmc->code - DVM_CAST_B) ){
				switch( vmc->code ){
				case DVM_CAST_B :
				case DVM_CAST_I : vC->xInteger.value = vA->xInteger.value; break;
				case DVM_CAST_F : vC->xFloat.value   = vA->xFloat.value;   break;
				case DVM_CAST_C : vC->xComplex.value = vA->xComplex.value; break;
				case DVM_CAST_S : DString_Assign( vC->xString.value, vA->xString.value );break;
				case DVM_CAST_VE: vC->xEnum.value = vA->xEnum.value; break;
				}
			}else{
				DaoProcess_DoCast( self, vmc );
				goto CheckException;
			}
		}OPNEXT() OPCASE( CAST_VX ){
			vA = locVars[vmc->a];
			abtp = locTypes[vmc->c];
			if( vA->type == abtp->tid ){
				GC_Assign( & locVars[vmc->c], vA );
			}else{
				DaoProcess_DoCast( self, vmc );
				goto CheckException;
			}
		}OPNEXT() OPCASE( ISA_ST ){
			vA = locVars[vmc->a];
			locVars[vmc->c]->xInteger.value = vA && vA->type == locVars[vmc->b]->xType.tid;
		}OPNEXT() OPCASE( TUPLE_SIM ){
			DaoProcess_DoTupleSim( self, vmc );
		}OPNEXT()
		OPDEFAULT()
		{
			goto CheckException;
RaiseErrorIndexOutOfRange:
			self->activeCode = vmc;
			DaoProcess_RaiseError( self, "Index::Range", NULL );
			goto CheckException;
RaiseErrorSlicing:
			self->activeCode = vmc;
			DaoProcess_RaiseError( self, "Index", "slicing" );
			goto CheckException;
RaiseErrorDivByZero:
			self->activeCode = vmc;
			DaoProcess_RaiseError( self, "Float::DivByZero", "" );
			goto CheckException;
RaiseErrorInvalidOperation:
			operands = DaoVmCode_CheckOperands( vmc );
			self->activeCode = vmc;
			DaoProcess_RaiseError( self, NULL, "invalid operation" );
			goto CheckException;
ModifyConstant:
			self->activeCode = vmc;
			DaoProcess_RaiseError( self, NULL, "attempt to modify a constant" );
			goto CheckException;
AccessNullInstance:
			self->activeCode = vmc;
			DaoProcess_RaiseError( self, NULL, "cannot access class null instance" );
			goto CheckException;
RaiseErrorNullObject:
			self->activeCode = vmc;
			DaoProcess_RaiseError( self, NULL, "operate on none object" );
			goto CheckException;
CheckException:

			locVars = self->activeValues;
			self->baseFrame = rollback; /* may have been changed; */
			if( vmSpace->stopit ) goto FinishProcess;
			if( (++count) % 1000 == 0 ) DaoGC_TryInvoke( self );
			if( invokehost ) handler->InvokeHost( handler, self );
			if( self->exceptions->size > exceptCount ){
				goto FinishCall;
			}else if( self->status == DAO_PROCESS_STACKED ){
				goto CallEntry;
			}else if( self->status == DAO_PROCESS_SUSPENDED ){
				self->topFrame->entry = (short)(vmc - vmcBase);
				if( profiler ) profiler->LeaveFrame( profiler, self, self->topFrame, 0 );
				goto ReturnFalse;
			}else if( self->status == DAO_PROCESS_ABORTED ){
				goto FinishProcess;
			}
			OPNEXT()
		}
	}OPEND()

FinishCall:

	if( self->defers->size > self->topFrame->deferBase ){
		self->topFrame->state |= DVM_FRAME_FINISHED;
		if( DaoProcess_PushDefers( self, NULL ) ) goto CallEntry;
	}

	if( self->topFrame->state & DVM_FRAME_KEEP ){
		self->topFrame->state &= ~DVM_FRAME_RUNNING;
		self->status = DAO_PROCESS_FINISHED;
		if( self->exceptions->size > exceptCount0 ) goto AbortProcess;
		goto ReturnTrue;
	}
	/* Better to print while the frame is still on the stack: */
	if( self->topFrame->prev == self->firstFrame && self == vmSpace->mainProcess ){
		int interun = self->vmSpace->options & DAO_OPTION_INTERUN;
		int autoglb = self->activeNamespace->options & DAO_NS_AUTO_GLOBAL;
		int print = interun && autoglb; // XXX: use autoglb?
		int status = self->status;
		if( (print || vmSpace->evalCmdline) && self->stackValues[0] ){
			DaoStream_WriteChars( vmSpace->stdioStream, "= " );
			DaoValue_Print( self->stackValues[0], self, vmSpace->stdioStream, NULL );
			DaoStream_WriteNewLine( vmSpace->stdioStream );
		}
		self->status = status;
	}
	DaoProcess_PopFrame( self );
	DaoGC_TryInvoke( self );
	goto CallEntry;

FinishProcess:

	if( vmSpace->stopit ){
		DList_Clear( self->exceptions );
		DaoProcess_RaiseError( self, NULL, "Execution cancelled" );
	}
	if( self->exceptions->size ) DaoProcess_PrintException( self, NULL, 1 );
	DaoProcess_PopFrames( self, rollback );
	/*if( eventHandler ) eventHandler->mainRoutineExit(); */

AbortProcess:
	self->status = DAO_PROCESS_ABORTED;

ReturnFalse:

#ifdef DAO_WITH_CONCURRENT
	/*
	// active==0:       if the process is started outside of the tasklet pool;
	// self->active!=0: if the process has been added to the active process list;
	// Now it must be manually removed from the active process list:
	*/
	if( active == 0 && self->active ) DaoCallServer_MarkActiveProcess( self, 0 );
#endif
	DaoGC_TryInvoke( self );
	self->depth -= 1;
	return 0;

ReturnTrue:

#ifdef DAO_WITH_CONCURRENT
	if( active == 0 && self->active ) DaoCallServer_MarkActiveProcess( self, 0 );
#endif
	DaoGC_TryInvoke( self );
	self->depth -= 1;
	return 1;
}
int DaoProcess_Execute( DaoProcess *self )
{
	int ret = DaoProcess_Start( self );
#ifdef DAO_WITH_CONCURRENT
	if( self->status >= DAO_PROCESS_SUSPENDED ){
		DMutex mutex;
		DCondVar condv;
		DMutex_Init( & mutex );
		DCondVar_Init( & condv );
		if( DaoCallServer_GetThreadCount() == 0 ) DaoCallServer_AddThread( NULL, NULL );
		DMutex_Lock( & mutex );
		while( self->status >= DAO_PROCESS_SUSPENDED ){
			DCondVar_TimedWait( & condv, & mutex, 0.01 );
		}
		DMutex_Unlock( & mutex );
		DMutex_Destroy( & mutex );
		DCondVar_Destroy( & condv );
	}
#endif
	if( self->status == DAO_PROCESS_ABORTED ) ret = 0;
	return ret;
}

DaoVmCode* DaoProcess_DoSwitch( DaoProcess *self, DaoVmCode *vmc )
{
	DaoVmCode *mid;
	DaoValue **cst = self->activeRoutine->routConsts->value->items.pValue;
	DaoValue *opa = self->activeValues[ vmc->a ];
	int first, last, cmp, id;
	dao_integer min, max;

	if( vmc->c ==0 ) return self->topFrame->codes + vmc->b;
	if( vmc[1].c == DAO_CASE_TABLE ){
		if( opa->type == DAO_BOOLEAN || opa->type == DAO_INTEGER ){
			min = cst[ vmc[1].a ]->xInteger.value;
			max = cst[ vmc[vmc->c].a ]->xInteger.value;
			if( opa->xInteger.value >= min && opa->xInteger.value <= max )
				return self->topFrame->codes + vmc[ opa->xInteger.value - min + 1 ].b;
		}else if( opa->type== DAO_ENUM ){
			min = cst[ vmc[1].a ]->xEnum.value;
			max = cst[ vmc[vmc->c].a ]->xEnum.value;
			if( opa->xEnum.value >= min && opa->xEnum.value <= max )
				return self->topFrame->codes + vmc[ opa->xEnum.value - min + 1 ].b;
		}
		return self->topFrame->codes + vmc->b;
	}else if( vmc[1].c == DAO_CASE_TYPES ){
		int max = 0;
		mid = NULL;
		for(id=1; id<=vmc->c; id++){
			int mt = DaoType_MatchValue( (DaoType*) cst[vmc[id].a], opa, NULL );
			if( mt > max ){
				mid = vmc + id;
				max = mt;
			}
		}
		if( mid ) return self->topFrame->codes + mid->b;
		return self->topFrame->codes + vmc->b;
	}else if( vmc[1].c == DAO_CASE_UNORDERED ){
		for(id=1; id<=vmc->c; id++){
			mid = vmc + id;
			if( DaoValue_ComparePro( opa, cst[ mid->a ], self ) ==0 ){
				return self->topFrame->codes + mid->b;
			}
		}
	}
	first = 1;
	last = vmc->c;
	while( first <= last ){
		id = ( first + last ) / 2;
		mid = vmc + id;
		cmp = DaoValue_ComparePro( opa, cst[ mid->a ], self );
		if( cmp ==0 ){
			return self->topFrame->codes + mid->b;
		}else if( cmp <0 ){
			last = id - 1;
		}else{
			first = id + 1;
		}
	}
	return self->topFrame->codes + vmc->b;
}
int DaoProcess_Move( DaoProcess *self, DaoValue *A, DaoValue **C, DaoType *t )
{
	if( ! DaoValue_Move( A, C, t ) ){
		DaoType *type;
		if( self->activeCode->code == DVM_MOVE || self->activeCode->code == DVM_MOVE_PP ){
			if( (A->type == DAO_CDATA || A->type == DAO_CSTRUCT) && t && t->tid == A->type ){
				if( DaoType_MatchTo( A->xCdata.ctype, t, NULL ) ){
					DaoValue_Copy( A, C );
					return 1;
				}
			}
		}
		type = DaoNamespace_GetType( self->activeNamespace, A );
		DaoProcess_RaiseTypeError( self, type, t, "moving" );
		return 0;
	}
	return 1;
}

DaoValue* DaoProcess_SetValue( DaoProcess *self, ushort_t reg, DaoValue *value )
{
	DaoType *tp = self->activeTypes[reg];
	int res = DaoValue_Move( value, self->activeValues + reg, tp );
	if( res ) return self->activeValues[ reg ];
	return NULL;
}
DaoValue* DaoProcess_PutValue( DaoProcess *self, DaoValue *value )
{
	if( self->topFrame != self->topFrame->active && self->topFrame->returning == 0xffff ){
		int res = DaoValue_Move( value, self->stackValues, NULL );
		self->stackReturn = 0;
		if( res ) return self->stackValues[0];
		return NULL;
	}
	self->stackReturn = self->activeCode->c + (self->activeValues - self->stackValues);
	if( value == NULL ){
		DaoType *type = self->activeTypes[self->activeCode->c];
		if( type != NULL && (type->tid & DAO_ANY) == 0 ){
			int tm = type->tid == DAO_NONE;
			if( type->tid == DAO_VARIANT ){
				tm = DaoType_MatchValue( type, dao_none_value, NULL );
			}
			if( tm == 0 ){
				DaoProcess_RaiseTypeError( self, dao_type_none, type, "moving" );
				return NULL;
			}
		}
	}
	return DaoProcess_SetValue( self, self->activeCode->c, value );
}
DaoNone* DaoProcess_PutNone( DaoProcess *self )
{
	DaoProcess_PutValue( self, dao_none_value );
	return (DaoNone*) dao_none_value;
}
dao_integer* DaoProcess_PutBoolean( DaoProcess *self, dao_boolean value )
{
	DaoBoolean tmp = {DAO_BOOLEAN,0,0,0,0,0};
	DaoValue *res = DaoProcess_PutValue( self, (DaoValue*) & tmp );
	if( res == NULL ) return NULL;
	res->xBoolean.value = value != 0;
	return & res->xBoolean.value;
}
dao_integer* DaoProcess_PutInteger( DaoProcess *self, dao_integer value )
{
	DaoInteger tmp = {DAO_INTEGER,0,0,0,0,0};
	DaoValue *res = DaoProcess_PutValue( self, (DaoValue*) & tmp );
	if( res == NULL ) return NULL;
	res->xInteger.value = value;
	return & res->xInteger.value;
}
dao_float* DaoProcess_PutFloat( DaoProcess *self, dao_float value )
{
	DaoFloat tmp = {DAO_FLOAT,0,0,0,0,0.0};
	DaoValue *res = DaoProcess_PutValue( self, (DaoValue*) & tmp );
	if( res == NULL ) return NULL;
	res->xFloat.value = value;
	return & res->xFloat.value;
}
dao_complex* DaoProcess_PutComplex( DaoProcess *self, dao_complex value )
{
	DaoComplex tmp = {DAO_COMPLEX,0,0,0,0,{0.0,0.0}};
	DaoValue *res;
	tmp.value = value;
	res = DaoProcess_PutValue( self, (DaoValue*) & tmp );
	if( res == NULL ) return NULL;
	return & res->xComplex.value;
}
DString* DaoProcess_PutChars( DaoProcess *self, const char *mbs )
{
	DString str = DString_WrapChars( mbs );
	DaoString tmp = {DAO_STRING,0,0,0,0,NULL};
	DaoValue *res, *dest;
	tmp.value = & str;
	dest = self->activeValues[ self->activeCode->c ];
	if( dest && dest->type == DAO_STRING ){
		DString_Reset( dest->xString.value, 0 );
	}
	res = DaoProcess_PutValue( self, (DaoValue*) & tmp );
	if( res == NULL ) return NULL;
	return res->xString.value;
}
DString* DaoProcess_PutString( DaoProcess *self, DString *str )
{
	DaoString tmp = {DAO_STRING,0,0,0,0,NULL};
	DaoValue *res;
	tmp.value = str;
	res = DaoProcess_PutValue( self, (DaoValue*) & tmp );
	if( res == NULL ) return NULL;
	return res->xString.value;
}
DString* DaoProcess_PutBytes( DaoProcess *self, const char *bytes, daoint N )
{
	DString str = DString_WrapBytes( bytes, N );
	DaoString tmp = {DAO_STRING,0,0,0,0,NULL};
	DaoValue *res;
	tmp.value = & str;
	res = DaoProcess_PutValue( self, (DaoValue*) & tmp );
	if( res == NULL ) return NULL;
	return res->xString.value;
}
DaoList* DaoProcess_PutList( DaoProcess *self )
{
	return DaoProcess_GetList( self, self->activeCode );
}
DaoMap* DaoProcess_PutMap( DaoProcess *self, unsigned int hashing )
{
	return DaoProcess_GetMap( self, self->activeCode, hashing );
}
DaoArray* DaoProcess_PutArray( DaoProcess *self )
{
	return DaoProcess_GetArray( self, self->activeCode );
}
DaoStream* DaoProcess_PutFile( DaoProcess *self, FILE *file )
{
	DaoStream *stream = DaoStream_New();
	DaoStream_SetFile( stream, file );
	if( DaoProcess_PutValue( self, (DaoValue*) stream ) ) return stream;
	DaoStream_Delete( stream );
	return NULL;
}
void DaoCdata_Delete( DaoCdata *self );
DaoCdata* DaoProcess_PutCdata( DaoProcess *self, void *data, DaoType *type )
{
	DaoCdata *cdata = DaoWrappers_MakeCdata( type, data, 1 );
	if( DaoProcess_PutValue( self, (DaoValue*)cdata ) ) return cdata;
	DaoGC_TryDelete( (DaoValue*) cdata );
	return NULL;
}
DaoCdata* DaoProcess_WrapCdata( DaoProcess *self, void *data, DaoType *type )
{
	DaoCdata *cdata = DaoWrappers_MakeCdata( type, data, 0 );
	if( DaoProcess_PutValue( self, (DaoValue*)cdata ) ) return cdata;
	DaoGC_TryDelete( (DaoValue*) cdata );
	return NULL;
}
DaoCdata*  DaoProcess_CopyCdata( DaoProcess *self, void *d, int n, DaoType *t )
{
	DaoCdata *cdt;
	void *d2 = dao_malloc( n );
	memcpy( d2, d, n );
	cdt = DaoProcess_PutCdata( self, d2, t );
	return cdt;
}
DaoType* DaoProcess_GetCallReturnType( DaoProcess *self, DaoVmCode *vmc, int tid )
{
	DaoType *type = self->activeTypes[ vmc->c ];

	if( type && type->tid == DAO_VARIANT ) type = DaoType_GetVariantItem( type, tid );

	if( type == NULL || type->tid != tid ){
		if( self->activeCode->code == DVM_CALL || self->activeCode->code == DVM_MCALL ){
			type = (DaoType*) self->topFrame->routine->routType->aux;
			if( type && type->tid == DAO_VARIANT ) type = DaoType_GetVariantItem( type, tid );
		}
	}
	return type;
}
DaoEnum* DaoProcess_GetEnum( DaoProcess *self, DaoVmCode *vmc )
{
	DaoType *tp = DaoProcess_GetCallReturnType( self, vmc, DAO_ENUM );
	DaoValue *dC = self->activeValues[ vmc->c ];

	self->stackReturn = self->activeCode->c + (self->activeValues - self->stackValues);
	if( tp && (tp->tid & DAO_ANY) ) tp = NULL;
	if( tp && tp->tid != DAO_ENUM ) return NULL;
	if( dC && dC->type == DAO_ENUM && tp && tp->tid == DAO_ENUM ){
		if( tp != dC->xEnum.etype ) DaoEnum_SetType( & dC->xEnum, tp );
		return & dC->xEnum;
	}
	dC = (DaoValue*) DaoEnum_New( tp, 0 );
	GC_Assign( & self->activeValues[vmc->c], dC );
	return & dC->xEnum;
}
DaoEnum* DaoProcess_PutEnum( DaoProcess *self, const char *symbols )
{
	DaoEnum *denum = DaoProcess_GetEnum( self, self->activeCode );
	DaoEnum_SetSymbols( denum, symbols );
	return denum;
}
/**/
DaoList* DaoProcess_GetListByType( DaoProcess *self, DaoVmCode *vmc, DaoType *tp )
{
	/* create a new list in any case. */
	DaoList *list = (DaoList*)self->activeValues[ vmc->c ];
	self->stackReturn = self->activeCode->c + (self->activeValues - self->stackValues);
	if( list && list->type == DAO_LIST && list->ctype == tp ){
		if( list->refCount == 1 ){
			DaoList_Clear( list );
			return list;
		}
		if( list->refCount == 2 && !(self->trait & DAO_VALUE_CONST) ){
			DaoVmCode *vmc2 = vmc + 1;
			if( (vmc2->code == DVM_MOVE || vmc2->code == DVM_MOVE_PP) && vmc2->a != vmc2->c ){
				if( self->activeValues[vmc2->c] == (DaoValue*) list ){
					DaoList_Clear( list );
					return list;
				}
			}
		}
	}
	if( tp == NULL || tp->tid != DAO_LIST ) tp = dao_type_list_any;
	list = DaoList_New();
	GC_Assign( & list->ctype, tp );
	DaoValue_Move( (DaoValue*) list, self->activeValues + vmc->c, tp );
	return list;
}
DaoList* DaoProcess_GetList( DaoProcess *self, DaoVmCode *vmc )
{
	DaoType *tp = DaoProcess_GetCallReturnType( self, vmc, DAO_LIST );
	return DaoProcess_GetListByType( self, vmc, tp );
}
DaoMap* DaoProcess_GetMap( DaoProcess *self,  DaoVmCode *vmc, unsigned int hashing )
{
	DaoMap *map = (DaoMap*) self->activeValues[ vmc->c ];
	DaoType *tp = DaoProcess_GetCallReturnType( self, vmc, DAO_MAP );

	self->stackReturn = self->activeCode->c + (self->activeValues - self->stackValues);
	if( map && map->type == DAO_MAP && map->ctype == tp ){
		if( hashing == 1 ) hashing = DAO_HASH_SEED;
		if( map->refCount == 1 ){
			DaoMap_Reset( map, hashing );
			return map;
		}
		if( map->refCount == 2 && !(self->trait & DAO_VALUE_CONST) ){
			DaoVmCode *vmc2 = vmc + 1;
			if( (vmc2->code == DVM_MOVE || vmc2->code == DVM_MOVE_PP) && vmc2->a != vmc2->c ){
				if( self->activeValues[vmc2->c] == (DaoValue*) map ){
					DaoMap_Reset( map, hashing );
					return map;
				}
			}
		}
	}
	if( tp == NULL || tp->tid != DAO_MAP ) tp = dao_type_map_any;
	map = DaoMap_New( hashing );
	GC_Assign( & map->ctype, tp );
	DaoValue_Move( (DaoValue*) map, self->activeValues + vmc->c, tp );
	return map;
}

DaoArray* DaoProcess_GetArrayByType( DaoProcess *self, DaoVmCode *vmc, DaoType *tp )
{
#ifdef DAO_WITH_NUMARRAY
	DaoValue *dC = self->activeValues[ vmc->c ];
	DaoArray *array = (DaoArray*) dC;
	int type = DAO_NONE;

	self->stackReturn = self->activeCode->c + (self->activeValues - self->stackValues);
	if( tp && tp->tid == DAO_ARRAY && tp->nested->size ){
		type = tp->nested->items.pType[0]->tid;
		if( type > DAO_COMPLEX ) type = DAO_NONE;
	}
	if( type && array && array->type == DAO_ARRAY && array->etype == type ){
		if( array->refCount == 1 ) return array;
		if( array->refCount == 2 && !(self->trait & DAO_VALUE_CONST) ){
			DaoVmCode *vmc2 = vmc + 1;
			if( (vmc2->code == DVM_MOVE || vmc2->code == DVM_MOVE_PP) && vmc2->a != vmc2->c ){
				if( self->activeValues[vmc2->c] == (DaoValue*) array ){
					return array;
				}
			}
		}
	}
	if( dC && dC->type == DAO_ARRAY && dC->xArray.refCount == 1 ){
		GC_DecRC( dC->xArray.original );
		dC->xArray.original = NULL;
		DaoArray_SetNumType( (DaoArray*) dC, type );
	}else{
		dC = (DaoValue*) DaoArray_New( type );
		DaoValue_Copy( dC, & self->activeValues[ vmc->c ] );
	}
	return & dC->xArray;
#else
	self->activeCode = vmc;
	DaoProcess_RaiseError( self, NULL, getCtInfo( DAO_DISABLED_NUMARRAY ) );
	return NULL;
#endif
}
DaoArray* DaoProcess_GetArray( DaoProcess *self, DaoVmCode *vmc )
{
	DaoType *tp = DaoProcess_GetCallReturnType( self, vmc, DAO_ARRAY );
	return DaoProcess_GetArrayByType( self, vmc, tp );
}
DaoTuple* DaoProcess_GetTuple( DaoProcess *self, DaoType *type, int size, int init )
{
	DaoValue *val = self->activeValues[ self->activeCode->c ];
	DaoTuple *tup = val && val->type == DAO_TUPLE ? & val->xTuple : NULL;

	self->stackReturn = self->activeCode->c + (self->activeValues - self->stackValues);
	if( tup && tup->ctype == type && tup->size == size ){
		if( tup->refCount == 1 ) return tup;
		if( tup->refCount == 2 && !(self->trait & DAO_VALUE_CONST) ){
			DaoVmCode *vmc = self->activeCode + 1;
			int code = vmc->code;
			if( (code == DVM_MOVE || code == DVM_MOVE_PP) && vmc->a != vmc->c ){
				if( self->activeValues[vmc->c] == (DaoValue*) tup ) return tup;
			}
		}
	}
	if( type ){
		tup = DaoTuple_Create( type, size, init );
	}else{
		tup = DaoTuple_New( size );
	}
	GC_Assign( & self->activeValues[ self->activeCode->c ], tup );
	return tup;
}
DaoTuple* DaoProcess_PutTuple( DaoProcess *self, int size )
{
	int i, N = abs(size);
	int M = self->factory->size;
	DaoValue **values = self->factory->items.pValue;
	DaoType *type = DaoProcess_GetCallReturnType( self, self->activeCode, DAO_TUPLE );
	DaoTuple *tuple;

	self->stackReturn = self->activeCode->c + (self->activeValues - self->stackValues);
	if( type == NULL ) return NULL;
	if( type->tid & DAO_ANY ) return DaoProcess_GetTuple( self, NULL, N, size > 0 );
	if( type->tid != DAO_TUPLE ) return NULL;
	if( size == 0 ) return DaoProcess_GetTuple( self, type, type->nested->size, 1 );
	if( type->variadic == 0 && N != type->nested->size ) return NULL;
	if( N < type->nested->size ) return NULL;
	tuple = DaoProcess_GetTuple( self, type, N, size > 0 );
	if( size > 0 ) return tuple;
	if( M < size ) return NULL;
	for(i=0; i<N; i++) DaoTuple_SetItem( tuple, values[M-N+i], i );
	DList_Erase( self->factory, M - size, -1 );
	return tuple;
}
DaoType* DaoProcess_GetReturnType( DaoProcess *self )
{
	DaoStackFrame *frame = self->topFrame;
	DaoType *type = self->activeTypes[ self->activeCode->c ]; /* could be specialized; */
	if( frame->retype ) return frame->retype;
	if( type == NULL || (type->attrib & DAO_TYPE_UNDEF) ){
		if( frame->routine ) type = (DaoType*) frame->routine->routType->aux;
	}
	if( type == NULL ) type = self->activeTypes[ self->activeCode->c ];
	GC_Assign( & frame->retype, type );
	return type;
}

void DaoProcess_MakeTuple( DaoProcess *self, DaoTuple *tuple, DaoValue *its[], int N )
{
	DaoType **types, *tp, *vlt = NULL, *ct = tuple->ctype;
	int i, M;
	if( ct == NULL ) return;
	if( ct->nested == NULL || (ct->nested->size - (ct->variadic != 0)) > N ){
		DaoProcess_RaiseError( self, NULL, "invalid tuple enumeration" );
		return;
	}
	types = ct->nested->items.pType;
	M = ct->nested->size - (ct->variadic != 0);
	if( ct->variadic ) vlt = (DaoType*) types[M]->aux;
	for(i=0; i<N; i++){
		DaoValue *val = its[i];
		if( val->type == DAO_PAR_NAMED ){
			DaoNameValue *nameva = & val->xNameValue;
			DNode *node = MAP_Find( ct->mapNames, nameva->name );
			if( node == NULL || node->value.pInt != i ){
				DaoProcess_RaiseError( self, NULL, "name not matched" );
				return;
			}
			val = nameva->value;
		}
		tp = i < M ? types[i] : vlt;
		if( tp && tp->tid == DAO_PAR_NAMED ) tp = & tp->aux->xType;
		if( DaoValue_Move( val, tuple->values + i, tp ) == 0 ){
			DaoProcess_RaiseError( self, NULL, "invalid tuple enumeration" );
			return;
		}
	}
}

void DaoProcess_BindNameValue( DaoProcess *self, DaoVmCode *vmc )
{
	DaoValue *dB = self->activeValues[ vmc->b ];
	DaoValue *dC = self->activeValues[ vmc->c ];
	DaoType *type = self->activeTypes[ vmc->c ];
	DaoNameValue *nameva = NULL;
	if( type && dC && dC->type == DAO_PAR_NAMED && dC->xNameValue.ctype == type ){
		DaoNameValue *NV = (DaoNameValue*) dC;
		DaoVmCode *vmc2 = vmc + 1;
		uchar_t codetype = DaoVmCode_GetOpcodeType( vmc2 );
		if( NV->refCount == 1 ){
			nameva = NV;
		}else if( NV->refCount == 2 && codetype == DAO_CODE_MOVE && vmc2->a != vmc2->c ){
			if( self->activeValues[vmc2->c] == dC ) nameva = NV;
		}
	}
	if( nameva == NULL ){
		DaoString *S = (DaoString*) self->activeRoutine->routConsts->value->items.pValue[ vmc->a ];
		if( type == NULL ){
			DaoNamespace *ns = self->activeNamespace;
			DaoValue *tp = (DaoValue*) DaoNamespace_GetType( ns, dB );
			type = DaoNamespace_MakeType( ns, S->value->chars, DAO_PAR_NAMED, tp, NULL, 0 );
		}
		nameva = DaoNameValue_New( S->value, NULL );
		nameva->ctype = type;
		GC_IncRC( nameva->ctype );
		DaoProcess_SetValue( self, vmc->c, (DaoValue*) nameva );
	}
	DaoValue_Move( dB, & nameva->value, (DaoType*) nameva->ctype->aux );
}
void DaoProcess_DoPair( DaoProcess *self, DaoVmCode *vmc )
{
	DaoNamespace *ns = self->activeNamespace;
	DaoType *tp = self->activeTypes[ vmc->c ];
	DaoType *ta = self->activeTypes[ vmc->a ];
	DaoType *tb = self->activeTypes[ vmc->b ];
	DaoTuple *tuple;
	self->activeCode = vmc;
	//XXX if( tp == NULL ) tp = DaoNamespace_MakePairValueType( ns, dA, dB );
	if( ta == NULL ) ta = DaoNamespace_GetType( ns, self->activeValues[ vmc->a ] );
	if( tb == NULL ) tb = DaoNamespace_GetType( ns, self->activeValues[ vmc->b ] );
	if( tp == NULL ) tp = DaoNamespace_MakePairType( ns, ta, tb );
	tuple = DaoProcess_GetTuple( self, tp, 2, 1 );
	tuple->subtype = DAO_PAIR;
	DaoValue_Copy( self->activeValues[ vmc->a ], & tuple->values[0] );
	DaoValue_Copy( self->activeValues[ vmc->b ], & tuple->values[1] );
}
void DaoProcess_DoTuple( DaoProcess *self, DaoVmCode *vmc )
{
	DaoValue *val;
	DaoTuple *tuple;
	DaoType *tp, *ct = self->activeTypes[ vmc->c ];
	DaoType *routype = self->activeRoutine->routType;
	int argcount = self->topFrame->parCount;
	int parcount = routype->nested->size - routype->variadic;
	int parcount2 = argcount < parcount ? parcount : argcount; /* including defaults; */
	int i, count, mode = vmc->b >> 14;
	
	switch( mode ){
	case DVM_ENUM_MODE0 : count = vmc->b & (0xffff>>2); break;
	case DVM_ENUM_MODE1 : count = parcount2 - vmc->a; break; /* skip self parameter; */
	case DVM_ENUM_MODE2 : count = argcount; break;
	}

	self->activeCode = vmc;
	tuple = DaoProcess_GetTuple( self, ct && ct->variadic == 0 ? ct : NULL, count, 0 );
	if( ct == NULL ){
		DaoNamespace *ns = self->activeNamespace;
		ct = DaoType_New( "tuple<", DAO_TUPLE, NULL, NULL );
		for(i=0; i<count; i++){
			val = self->activeValues[ vmc->a + i ];
			tp = DaoNamespace_GetType( ns, val );
			if( tp == NULL ) tp = DaoNamespace_GetType( ns, dao_none_value );
			if( i >0 ) DString_AppendChars( ct->name, "," );
			if( tp->tid == DAO_PAR_NAMED ){
				DaoNameValue *nameva = & val->xNameValue;
				if( ct->mapNames == NULL ) ct->mapNames = DMap_New( DAO_DATA_STRING, 0 );
				MAP_Insert( ct->mapNames, nameva->name, i );
				DString_Append( ct->name, nameva->name );
				DString_AppendChars( ct->name, ":" );
				DString_Append( ct->name, tp->aux->xType.name );
				val = nameva->value;
			}else{
				DString_Append( ct->name, tp->name );
			}
			DList_Append( ct->nested, tp );
			DaoTuple_SetItem( tuple, val, i );
		}
		DString_AppendChars( ct->name, ">" );
		tp = DaoNamespace_FindType( ns, ct->name );
		if( tp ){
			DaoType_Delete( ct );
			ct = tp;
		}else{
			DaoType_CheckAttributes( ct );
			DaoType_InitDefault( ct );
			DaoNamespace_AddType( ns, ct->name, ct );
		}
		tuple->ctype = ct;
		GC_IncRC( ct );
	}else if( mode ){
		GC_Assign( & tuple->ctype, ct );
		for(i=0; i<count; i++) DaoTuple_SetItem( tuple, self->activeValues[vmc->a + i], i );
	}else{
		if( tuple->ctype == NULL ){
			tuple->ctype = ct;
			GC_IncRC( ct );
		}
		DaoProcess_MakeTuple( self, tuple, self->activeValues + vmc->a, count );
	}
}
void DaoProcess_DoTupleSim( DaoProcess *self, DaoVmCode *vmc )
{
	DaoTuple *tuple;
	DaoType *ct = self->activeTypes[ vmc->c ];
	DaoType **types = ct->nested->items.pType;
	int i, count = vmc->b;

	self->activeCode = vmc;
	tuple = DaoProcess_GetTuple( self, ct, count, 0 );

	for(i=0; i<count; i++){
		DaoValue *val = self->activeValues[vmc->a + i];
		if( DaoValue_Move( val, tuple->values + i, types[i] ) == 0 ){
			DaoProcess_RaiseError( self, NULL, "invalid tuple enumeration" );
			return;
		}
	}
}
void DaoProcess_DoCheckSame( DaoProcess *self, DaoVmCode *vmc )
{
	DaoValue *dA = self->activeValues[ vmc->a ];
	DaoValue *dB = self->activeValues[ vmc->b ];
	DaoType *type = (DaoType*) dB;
	dao_integer *res = 0;

	self->activeCode = vmc;
	res = DaoProcess_PutBoolean( self, 0 );

	if( dA->type != dB->type ) return;

	if( dA->type == DAO_OBJECT ){
		*res = dA->xObject.rootObject->defClass == dB->xObject.rootObject->defClass;
	}else if( dA->type == DAO_CDATA || dA->type == DAO_CSTRUCT ){
		*res = dA->xCdata.ctype == dB->xCdata.ctype;
	}else if( dA->type >= DAO_ENUM && dA->type <= DAO_TUPLE ){
		DaoType *t1 = NULL;
		DaoType *t2 = NULL;
		*res = 0;
		switch( dA->type ){
		case DAO_ENUM :
			t1 = dA->xEnum.etype;
			t2 = dB->xEnum.etype;
			break;
		case DAO_ARRAY :
			t1 = dao_array_types[ dA->xArray.etype ];
			t2 = dao_array_types[ dB->xArray.etype ];
			break;
		case DAO_LIST : t1 = dA->xList.ctype; t2 = dB->xList.ctype; break;
		case DAO_MAP  : t1 = dA->xMap.ctype;  t2 = dB->xMap.ctype; break;
		case DAO_TUPLE : t1 = dA->xTuple.ctype; t2 = dB->xTuple.ctype; break;
		default : break;
		}
		*res = DaoType_MatchTo( t1, t2, NULL ) >= DAO_MT_EQ;
	}else if( dA->type == DAO_TYPE ){
		*res = DaoType_MatchTo( (DaoType*) dA, (DaoType*) dB, NULL ) >= DAO_MT_EQ;
	}else if( dA-> type <= DAO_STRING ){
		*res = 1;
	}
}

void DaoProcess_DoCheckIsa( DaoProcess *self, DaoVmCode *vmc )
{
	DaoValue *dA = self->activeValues[ vmc->a ];
	DaoValue *dB = self->activeValues[ vmc->b ];
	DaoType *type = (DaoType*) dB;
	dao_integer *res = 0;

	self->activeCode = vmc;
	res = DaoProcess_PutBoolean( self, 0 );

	if( dB->type == DAO_CTYPE ){
		dB = (DaoValue*) dB->xCtype.cdtype;
		if( dA->type == DAO_CTYPE ) dA = (DaoValue*) dA->xCtype.cdtype;
	}else if( dB->type == DAO_CLASS ){
		dB = (DaoValue*) dB->xClass.objType;
		if( dA->type == DAO_CLASS ) dA = (DaoValue*) dA->xClass.objType;
	}

	if( dB->type != DAO_TYPE ){
		DaoProcess_RaiseError( self, "Value", "invalid type operand" );
		return;
	}
	if( dA == dB ) return;

	if( dA->type == DAO_OBJECT ){
		*res = DaoType_ChildOf( dA->xObject.defClass->objType, (DaoType*) dB ) != 0;
		return;
	}else if( dA->type == DAO_CSTRUCT || dA->type == DAO_CDATA || dA->type == DAO_CTYPE ){
		*res = DaoType_ChildOf( dA->xCstruct.ctype, (DaoType*) dB ) != 0;
		return;
	}else if( type->tid == DAO_VARIANT ){
		int i, n, mt = 0, id = 0, max = 0;
		for(i=0,n=type->nested->size; i<n; i++){
			if( dA->type == DAO_TYPE ){
				mt = DaoType_MatchTo( & dA->xType, type->nested->items.pType[i], NULL );
			}else{
				mt = DaoType_MatchValue( type->nested->items.pType[i], dA, NULL );
			}
			if( mt > max ){
				max = mt;
				id = i + 1;
			}
			if( max >= DAO_MT_EQ ) break;
		}
		*res = id;
		return;
	}else if( dA->type == DAO_TYPE ){
		*res = DaoType_ChildOf( (DaoType*) dA, (DaoType*) dB ) != 0;
		if( DaoType_ChildOf( (DaoType*) dB, (DaoType*) dA ) ) *res = 0;
		return;
	}
	if( dA->type < DAO_ARRAY ){
		*res = dA->type == type->tid;
	}else{
		*res = DaoType_MatchValue( type, dA, NULL ) != 0;
	}
}
void DaoProcess_DoGetItem( DaoProcess *self, DaoVmCode *vmc )
{
	daoint id;
	DaoValue *B = dao_none_value;
	DaoValue *A = self->activeValues[ vmc->a ];
	DaoInteger di = {DAO_INTEGER,0,0,0,0,0};
	DaoType *ct = self->activeTypes[ vmc->c ];
	DaoTypeCore *tc = DaoValue_GetTyper( A )->core;

	self->activeCode = vmc;
	if( A == NULL || A->type == 0 ){
		DaoProcess_RaiseError( self, "Value", "on none object" );
		return;
	}
	if( vmc->code == DVM_GETI ){
		B = self->activeValues[ vmc->b ];
	}else if( vmc->code == DVM_GETDI ){
		di.value = vmc->b;
		B = (DaoValue*) & di;
	}
	if( vmc->code == DVM_GETDI && A->type == DAO_PAR_NAMED ){
		if( vmc->b == 0 ){
			DaoProcess_PutString( self, A->xNameValue.name );
		}else if( vmc->b == 1 ){
			GC_Assign( & self->activeValues[ vmc->c ], A->xNameValue.value );
		}else{
			DaoProcess_RaiseError( self, "Index", "index out of range" );
			return;
		}
	}else if( A->type == DAO_LIST && (B->type >= DAO_BOOLEAN && B->type <= DAO_FLOAT ) ){
		DaoList *list = & A->xList;
		id = DaoValue_GetInteger( B );
		if( id < 0 ) id += list->value->size;
		if( id >=0 && id < list->value->size ){
			GC_Assign( & self->activeValues[ vmc->c ], list->value->items.pValue[id] );
		}else{
			DaoProcess_RaiseError( self, "Index", "index out of range" );
			return;
		}
#ifdef DAO_WITH_NUMARRAY
	}else if( A->type == DAO_ARRAY && (B->type >= DAO_BOOLEAN && B->type <= DAO_FLOAT )){
		DaoValue temp = {0};
		DaoValue *C = (DaoValue*) & temp;
		DaoArray *na = & A->xArray;
		id = DaoValue_GetInteger( B );
		memset( C, 0, sizeof(DaoValue) );
		if( na->original && DaoArray_Sliced( na ) == 0 ){
			DaoProcess_RaiseError( self, "Index", "slicing" );
			return;
		}
		if( id < 0 ) id += na->size;
		if( id < 0 || id >= na->size ){
			DaoProcess_RaiseError( self, "Index::Range", "" );
			return;
		}
		C->type = na->etype;
		switch( na->etype ){
			case DAO_BOOLEAN : C->xBoolean.value = na->data.b[id]; break;
			case DAO_INTEGER : C->xInteger.value = na->data.i[id]; break;
			case DAO_FLOAT   : C->xFloat.value = na->data.f[id];  break;
			case DAO_COMPLEX : C->xComplex.value = na->data.c[id]; break;
			default : break;
		}
		DaoProcess_Move( self, C, & self->activeValues[ vmc->c ], ct );
#endif
	}else if( vmc->code == DVM_GETI ){
		tc->GetItem( A, self, self->activeValues + vmc->b, 1 );
	}else if( vmc->code == DVM_GETDI ){
		tc->GetItem( A, self, & B, 1 );
	}else if( vmc->code == DVM_GETMI || (vmc->code >= DVM_GETMI_AII && vmc->code <= DVM_GETMI_ACI) ){
		tc->GetItem( A, self, self->activeValues + vmc->a + 1, vmc->b );
	}
}
void DaoProcess_DoGetField( DaoProcess *self, DaoVmCode *vmc )
{
	DaoValue *C, *A = self->activeValues[ vmc->a ];
	DaoTypeCore *tc = DaoValue_GetTyper( A )->core;
	DaoNamespace *ns = self->activeNamespace;
	DString *name = self->activeRoutine->routConsts->value->items.pValue[ vmc->b ]->xString.value;

	self->activeCode = vmc;
	if( A == NULL || A->type == 0 ){
		DaoProcess_RaiseError( self, "Value", "on none object" );
		return;
	}
	tc->GetField( A, self, name );
}



void DaoProcess_DoSetItem( DaoProcess *self, DaoVmCode *vmc )
{
	DaoValue *A, *B = dao_none_value, *C = self->activeValues[ vmc->c ];
	DaoTypeCore *tc = DaoValue_GetTyper( C )->core;
	DaoInteger di = {DAO_INTEGER,0,0,0,0,0};
	daoint id, rc = 0;

	self->activeCode = vmc;
	A = self->activeValues[ vmc->a ];
	if( C == NULL || C->type == 0 ){
		DaoProcess_RaiseError( self, "Value", "on none object" );
		return;
	}

	if( vmc->code == DVM_SETI ){
		B = self->activeValues[ vmc->b ];
	}else if( vmc->code == DVM_SETDI ){
		di.value = vmc->b;
		B = (DaoValue*) & di;
	}
	if( vmc->code == DVM_SETDI && C->type == DAO_PAR_NAMED ){
		if( vmc->b == 1 ){
			DaoNameValue *nameva = (DaoNameValue*) C;
			DaoValue_Move( A, & nameva->value, (DaoType*) nameva->ctype->aux );
		}else{
			DaoProcess_RaiseError( self, "Index", "index out of range" );
			return;
		}
	}else if( C->type == DAO_LIST && B->type == DAO_INTEGER ){
		rc = DaoList_SetItem( & C->xList, A, B->xInteger.value );
	}else if( C->type == DAO_LIST && B->type == DAO_FLOAT ){
		rc = DaoList_SetItem( & C->xList, A, (daoint) B->xFloat.value );
#ifdef DAO_WITH_NUMARRAY
	}else if( C->type == DAO_ARRAY && (B->type >= DAO_BOOLEAN && B->type <= DAO_FLOAT)
			 && (A->type >= DAO_BOOLEAN && A->type <= DAO_FLOAT) ){
		DaoArray *na = & C->xArray;
		id = DaoValue_GetFloat( B );
		if( na->original && DaoArray_Sliced( na ) == 0 ){
			DaoProcess_RaiseError( self, "Index", "slicing" );
			return;
		}
		if( id < 0 ) id += na->size;
		if( id < 0 || id >= na->size ){
			DaoProcess_RaiseError( self, "Index::Range", "" );
			return;
		}
		switch( na->etype ){
		case DAO_BOOLEAN : na->data.b[ id ] = DaoValue_IsZero( A ) != 0; break;
		case DAO_INTEGER : na->data.i[ id ] = DaoValue_GetInteger( A ); break;
		case DAO_FLOAT  : na->data.f[ id ]  = DaoValue_GetFloat( A ); break;
		case DAO_COMPLEX : na->data.c[ id ] = DaoValue_GetComplex( A ); break;
		default : break;
		}
#endif
	}else if( vmc->code == DVM_SETI ){
		tc->SetItem( C, self, self->activeValues + vmc->b, 1, A );
	}else if( vmc->code == DVM_SETDI ){
		tc->SetItem( C, self, & B, 1, A );
	}else if( vmc->code == DVM_SETMI || (vmc->code >= DVM_SETMI_AIII && vmc->code <= DVM_SETMI_ACIC) ){
		tc->SetItem( C, self, self->activeValues + vmc->c + 1, vmc->b, A );
	}
	if( rc ) DaoProcess_RaiseError( self, "Value", "value type" );
}
void DaoProcess_DoSetField( DaoProcess *self, DaoVmCode *vmc )
{
	DaoValue *A, *C = self->activeValues[ vmc->c ];
	DaoValue *fname = self->activeRoutine->routConsts->value->items.pValue[ vmc->b ];
	DaoTypeCore *tc = DaoValue_GetTyper( C )->core;

	self->activeCode = vmc;
	A = self->activeValues[ vmc->a ];
	if( C == NULL || C->type == 0 ){
		DaoProcess_RaiseError( self, "Value", "on none object" );
		return;
	}
	tc->SetField( C, self, fname->xString.value, A );
}


DaoValue* DaoProcess_DoReturn( DaoProcess *self, DaoVmCode *vmc )
{
	DaoStackFrame *topFrame = self->topFrame;
	DaoStackFrame *lastframe = topFrame->prev;
	DaoValue **src = self->activeValues + vmc->a;
	DaoValue **dest = self->stackValues;
	DaoValue *retValue = dao_none_value;
	DaoType *type = NULL;
	daoint i, n, returning = topFrame->returning;

	self->activeCode = vmc;

	if( vmc->b && (topFrame->routine->attribs & DAO_ROUT_DEFER) ){
		lastframe = topFrame->host->prev;
		returning = topFrame->host->returning;
	}

	if( returning != 0xffff ){
#ifdef DEBUG
		assert( lastframe && lastframe->routine );
#endif
		type = lastframe->routine->body->regType->items.pType[ returning ];
		dest = self->stackValues + lastframe->stackBase + returning;
	}else if( self->topFrame->state & DVM_FRAME_SECT ){
		type = (DaoType*) lastframe->routine->routType->cbtype->aux;
	}
	if( (topFrame->routine->attribs & DAO_ROUT_INITOR) && !(topFrame->state & DVM_FRAME_SECT) ){
		retValue = (DaoValue*)self->activeObject;
	}else if( vmc->b == 1 ){
		retValue = self->activeValues[ vmc->a ];
	}else if( vmc->b > 1 && dest != self->stackValues ){
		DaoTuple *tup = (DaoTuple*) *dest;
		DaoTuple *tuple = NULL;
		if( tup && tup->type == DAO_TUPLE && tup->ctype == type && tup->refCount == 1 ){
			if( tup->size > vmc->b ) goto InvalidReturn;
			tuple = tup;
		}else if( type && type->tid == DAO_TUPLE ){
			if( type->nested->size > vmc->b ) goto InvalidReturn;
			tuple = DaoTuple_Create( type, vmc->b, 0 );
		}else{
			tuple = DaoTuple_New( vmc->b );
		}
		if( tuple->ctype ){
			DaoType **TS = tuple->ctype->nested->items.pType;
			for(i=0,n=tuple->size; i<n; i++){
				DaoType *tp = TS[i]->tid == DAO_PAR_NAMED ? (DaoType*)TS[i]->aux : TS[i];
				DaoValue_Move( src[i], tuple->values + i, tp );
			}
		}else{
			for(i=0,n=tuple->size; i<n; i++) DaoValue_Copy( src[i], tuple->values + i );
		}
		retValue = (DaoValue*) tuple;
	}else if( vmc->b > 1 ){
		DaoTuple *tuple = DaoTuple_New( vmc->b );
		retValue = (DaoValue*) tuple;
		for(i=0; i<vmc->b; i++) DaoValue_CopyX( src[i], tuple->values + i, NULL );
	}
	if( retValue == NULL ){
		int cmdline = self->vmSpace->evalCmdline;
		int opt1 = self->vmSpace->options & DAO_OPTION_INTERUN;
		int opt2 = self->activeNamespace->options & DAO_NS_AUTO_GLOBAL;
		int retnull = type == NULL || type->tid == DAO_NONE || type->tid == DAO_UDT;
		if( retnull || cmdline || (opt1 && opt2) ) retValue = dao_none_value;
	}
	if( DaoValue_Move( retValue, dest, type ) == 0 ) goto InvalidReturn;
	return retValue;
InvalidReturn:
#if 0
	fprintf( stderr, "retValue = %p %i %p %s\n", retValue, retValue->type, type, type->name->chars );
#endif
	DaoProcess_RaiseError( self, "Value", "invalid returned value" );
	return NULL;
}
int DaoVM_DoMath( DaoProcess *self, DaoVmCode *vmc, DaoValue *C, DaoValue *A )
{
	DaoValue temp = {0};
	DaoValue *value = (DaoValue*) & temp;
	DaoNamespace *ns = self->activeRoutine->nameSpace;
	DaoType *type = self->activeTypes[vmc->c];
	int func = vmc->a;
	self->activeCode = vmc;
	memset( value, 0, sizeof(DaoValue) );
	if( A->type == DAO_COMPLEX ){
		dao_complex par = A->xComplex.value;
		dao_complex cres = {0.0,0.0};
		double rres = 0.0;
		int isreal = 0;
		switch( func ){
		case DVM_MATH_ABS  : rres = abs_c( par ); isreal = 1; break;
		case DVM_MATH_ARG  : rres = arg_c( par ); isreal = 1; break;
		case DVM_MATH_NORM  : rres = norm_c( par ); isreal = 1; break;
		case DVM_MATH_IMAG  : rres = par.imag; isreal = 1; break;
		case DVM_MATH_REAL  : rres = par.real; isreal = 1; break;
		case DVM_MATH_CEIL : cres = ceil_c( par ); break;
		case DVM_MATH_COS  : cres = cos_c( par );  break;
		case DVM_MATH_COSH : cres = cosh_c( par ); break;
		case DVM_MATH_EXP  : cres = exp_c( par );  break;
		case DVM_MATH_FLOOR : cres = floor_c( par ); break;
		case DVM_MATH_LOG  : cres = log_c( par );  break;
		case DVM_MATH_SIN  : cres = sin_c( par );  break;
		case DVM_MATH_SINH : cres = sinh_c( par ); break;
		case DVM_MATH_SQRT : cres = sqrt_c( par ); break;
		case DVM_MATH_TAN  : cres = tan_c( par );  break;
		case DVM_MATH_TANH : cres = tanh_c( par ); break;
		default : return 1;
		}
		if( isreal ){
			if( C && C->type == DAO_FLOAT ){
				C->xFloat.value = rres;
			}else{
				value->type = DAO_FLOAT;
				value->xFloat.value = rres;
				return DaoValue_Move( value, self->activeValues + vmc->c, dao_type_float ) == 0;
			}
		}else{
			if( C && C->type == DAO_COMPLEX ){
				C->xComplex.value = cres;
			}else{
				value->type = DAO_COMPLEX;
				value->xComplex.value = cres;
				return DaoValue_Move( value, self->activeValues + vmc->c, dao_type_complex ) == 0;
			}
		}
		return 0;
	}else if( A->type == DAO_INTEGER && func <= DVM_MATH_ABS ){
		daoint res = A->xInteger.value;
		if( func == DVM_MATH_ABS ) res = abs( res );
		if( C && C->type == DAO_INTEGER ){
			C->xInteger.value = res;
		}else{
			value->type = DAO_INTEGER;
			value->xInteger.value = res;
			return DaoValue_Move( value, self->activeValues + vmc->c, dao_type_int ) == 0;
		}
	}else if( A->type && A->type <= DAO_FLOAT ){
		double par = DaoValue_GetFloat( A );
		double res = 0.0;
		switch( func ){
		case DVM_MATH_ABS  : res = fabs( par );  break;
		case DVM_MATH_ACOS : res = acos( par ); break;
		case DVM_MATH_ASIN : res = asin( par ); break;
		case DVM_MATH_ATAN : res = atan( par ); break;
		case DVM_MATH_CEIL : res = ceil( par ); break;
		case DVM_MATH_COS  : res = cos( par );  break;
		case DVM_MATH_COSH : res = cosh( par ); break;
		case DVM_MATH_EXP  : res = exp( par );  break;
		case DVM_MATH_FLOOR: res = floor( par ); break;
		case DVM_MATH_LOG  : res = log( par );  break;
		case DVM_MATH_SIN  : res = sin( par );  break;
		case DVM_MATH_SINH : res = sinh( par ); break;
		case DVM_MATH_SQRT : res = sqrt( par ); break;
		case DVM_MATH_TAN  : res = tan( par );  break;
		case DVM_MATH_TANH : res = tanh( par ); break;
		default : return 1;
		}
		value->type = DAO_FLOAT;
		value->xFloat.value = res;
		return DaoValue_Move( value, self->activeValues + vmc->c, dao_type_float ) == 0;
	}
	return 1;
}
DaoValue* DaoTypeCast( DaoProcess *proc, DaoType *ct, DaoValue *dA, DaoValue *dC, int invarToVar );
int ConvertStringToNumber( DaoProcess *proc, DaoValue *dA, DaoValue *dC );
void DaoProcess_PopValues( DaoProcess *self, int N );
static void* DaoType_DownCastCxxData( DaoType *self, DaoType *totype, void *data )
{
	daoint i, n;
	if( self == totype || totype == NULL || data == NULL ) return data;
	for(i=0,n=totype->bases->size; i<n; i++){
		void *p = DaoType_DownCastCxxData( self, totype->bases->items.pType[i], data );
		if( p ){
			if( totype->typer->casts[i] ) return (*totype->typer->casts[i])( p, 1 );;
			return p;
		}
	}
	return NULL;
}
void DaoProcess_DoCast( DaoProcess *self, DaoVmCode *vmc )
{
	int i, n, mt, mt2, invarToVar = 0;
	int top = self->factory->size;
	DaoType *at, *ct = self->activeTypes[ vmc->c ];
	DaoValue *va = self->activeValues[ vmc->a ];
	DaoValue *vc = self->activeValues[ vmc->c ];
	DaoValue **vc2 = self->activeValues + vmc->c;
	DaoRoutine *meth;
	DNode *node;

	self->activeCode = vmc;
	if( va == NULL ){
		DaoProcess_RaiseError( self, "Value", "operate on none object" );
		return;
	}
	if( va->type == DAO_PAR_NAMED ) va = va->xNameValue.value;
	if( ct == NULL ) goto FastCasting;

	if( va->type == DAO_CINVALUE ){
		if( va->xCinValue.cintype->target == ct ){
			va = va->xCinValue.value;
		}else if( DaoType_MatchTo( va->xCinValue.cintype->target, ct, NULL ) >= DAO_MT_EQ ){
			va = va->xCinValue.value;
		}
	}

	if( va->type == ct->tid && ct->tid <= DAO_STRING ) goto FastCasting;
	if( ct->tid == DAO_UDT || ct->tid == DAO_ANY ){
		at = self->activeTypes[ vmc->a ];
		if( at == NULL || at->invar == 0 || ct->invar != 0 ) goto FastCasting;
		ct = DaoNamespace_GetType( self->activeNamespace, va );
		ct = DaoType_GetBaseType( ct );
		if( ct == NULL ) goto FailConversion;
		if( DaoType_IsImmutable( ct ) ) goto FastCasting;
		invarToVar = 1;
	}

	if( vc && vc->type == ct->tid && va->type <= DAO_STRING ){
		if( va->type == ct->tid ) goto FastCasting;
		if( va->type == DAO_STRING ){
			if( ConvertStringToNumber( self, va, vc ) == 0 ) goto FailConversion;
			return;
		}
		switch( ct->tid ){
		case DAO_BOOLEAN :
		case DAO_INTEGER : vc->xInteger.value = DaoValue_GetInteger( va ); return;
		case DAO_FLOAT   : vc->xFloat.value = DaoValue_GetFloat( va ); return;
		case DAO_COMPLEX : vc->xComplex.value = DaoValue_GetComplex( va ); return;
		case DAO_STRING  : DaoValue_GetString( va, vc->xString.value ); return;
		}
	}

	if( ct->tid == DAO_ENUM && (vc == NULL || vc->type != DAO_ENUM) ){
		vc = (DaoValue*) DaoEnum_New( NULL, 0 );
		GC_Assign( vc2, vc );
	}
	if( ct->tid == DAO_ENUM && va->type == DAO_ENUM ){
		DaoEnum_SetType( & vc->xEnum, ct );
		if( DaoEnum_SetValue( & vc->xEnum, & va->xEnum ) ==0 ) goto FailConversion;
		return;
	}else if( ct->tid == DAO_ENUM && (va->type == DAO_BOOLEAN || va->type == DAO_INTEGER) ){
		if( ct->mapNames == NULL ) goto FailConversion;
		for(node=DMap_First(ct->mapNames);node;node=DMap_Next(ct->mapNames,node)){
			if( node->value.pInt == va->xInteger.value ) break;
		}
		if( node == NULL ) goto FailConversion;
		DaoEnum_SetType( & vc->xEnum, ct );
		vc->xEnum.value = node->value.pInt;
		return;
	}else if( ct->tid == DAO_ENUM && va->type == DAO_STRING ){
		if( ct->mapNames == NULL ) goto FailConversion;
		node = DMap_Find( ct->mapNames, va->xString.value );
		if( node == NULL ) goto FailConversion;
		DaoEnum_SetType( & vc->xEnum, ct );
		vc->xEnum.value = node->value.pInt;
		return;
	}else if( ct->tid == DAO_ENUM ){
		goto FailConversion;
	}

	if( ct->tid == DAO_CINVALUE ){
		DaoCinType *cintype = (DaoCinType*) ct->aux;
		DaoType *at;

		if( va->type == DAO_CINVALUE && va->xCinValue.cintype == cintype ) goto FastCasting;
		if( va->type == DAO_CINVALUE && DaoType_MatchValue( ct, va, NULL ) ) goto FastCasting;

		at = DaoNamespace_GetType( self->activeNamespace, va );
		if( cintype->target == at || DaoType_MatchTo( cintype->target, at, NULL ) >= DAO_MT_EQ ){
			va = (DaoValue*) DaoWrappers_MakeCinValue( cintype, va );
			goto FastCasting;
		}
		goto FailConversion;
	}else if( ct->tid == DAO_INTERFACE ){
		DaoInterface *inter = (DaoInterface*) ct->aux;
		if( ct->aux == NULL ){ /* type "interface": */
			if( va->type != DAO_INTERFACE ) goto FailConversion;
			goto FastCasting;
		}
		if( va->type == DAO_CINVALUE && DaoType_MatchValue( ct, va, NULL ) ) goto FastCasting;

		at = DaoNamespace_GetType( self->activeNamespace, va );
		if( inter->concretes ){
			DaoCinType *cintype = DaoInterface_GetConcrete( inter, at );
			if( cintype ){
				va = (DaoValue*) DaoWrappers_MakeCinValue( cintype, va );
				goto FastCasting;
			}
		}
		switch( va->type ){
		case DAO_OBJECT  :
			va = (DaoValue*) va->xObject.rootObject;
			at = va->xObject.defClass->objType;
			break;
		case DAO_CSTRUCT :
		case DAO_CDATA   :
			if( va->xCstruct.object ) va = (DaoValue*) va->xCstruct.object->rootObject;
			at = va->xObject.defClass->objType;
			break;
		}
		/* automatic binding when casted to an interface: */
		mt = DaoInterface_BindTo( inter, at, NULL );
	}else if( ct->tid == DAO_TYPE && (ct->nested == NULL || ct->nested->size == 0) ){
		if( va->type == DAO_TYPE ) goto FastCasting;
	}
	if( invarToVar == 0 ){
		mt = DaoType_MatchValue( ct, va, NULL );
		/* 
		printf( "mt = %i, ct = %s\n", mt, ct->name->chars );
		 */
		if( mt >= DAO_MT_EQ || (mt && ct->tid == DAO_INTERFACE) ){
			DaoValue_Copy( va, vc2 );
			return;
		}
	}
	if( ct->tid == DAO_VARIANT ) goto NormalCasting;
	if( va->type == DAO_OBJECT ){
		DaoValue *value = DaoObject_CastToBase( (DaoObject*) va, ct );
		if( value ){
			va = value;
			if( invarToVar ) goto FailConversion;
			goto FastCasting;
		}else{
			DaoClass *scope = self->activeObject ? self->activeObject->defClass : NULL;
			DaoValue *tpar = (DaoValue*) ct;
			meth = va->xObject.defClass->castOperators;
			if( meth && DaoProcess_PushCallable( self, meth, va, & tpar, 1 ) == 0 ) return;
			goto FailConversion;
		}
	}else if( va->type == DAO_CSTRUCT || va->type == DAO_CDATA ){
		DaoValue *tpar = (DaoValue*) ct;
		if( DaoType_MatchTo( va->xCdata.ctype, ct, NULL ) ){ /* up casting: */
			/*
			// No real casting here. C codes should use DaoValue_TryCastCdata(),
			// or DaoCdata_CastData() to do the real casting on the C data pointer.
			*/
			if( invarToVar ) goto FailConversion;
			goto FastCasting;
		}else if( va->type == DAO_CDATA && DaoType_MatchTo( ct, va->xCdata.ctype, NULL ) ){
			/* down casting: */
			void *data = DaoType_DownCastCxxData( va->xCdata.ctype, ct, va->xCdata.data );
			if( data ){
				va = (DaoValue*) DaoWrappers_MakeCdata( ct, data, 0 );
				goto FastCasting;
			}
		}
		meth = DaoType_GetCastor( va->xCdata.ctype );
		if( meth && DaoProcess_PushCallable( self, meth, va, & tpar, 1 ) == 0 ) return;
		goto FailConversion;
	}
NormalCasting:
	va = DaoTypeCast( self, ct, va, vc, invarToVar );
	if( va && va->type ) DaoValue_Copy( va, vc2 );
	DaoProcess_PopValues( self, self->factory->size - top );
	if( va == NULL || va->type == 0 ) goto FailConversion;
	return;
FastCasting:
	GC_Assign( vc2, va );
	return;
FailConversion :
	at = self->activeTypes[ vmc->a ];
	if( at == NULL ) at = DaoNamespace_GetType( self->activeNamespace, va );
	DaoProcess_RaiseTypeError( self, at, ct, "casting" );
}
static int DaoProcess_TryAsynCall( DaoProcess *self, DaoVmCode *vmc )
{
	DaoStackFrame *frame = self->topFrame;
	DaoStackFrame *prev = frame->prev;
	if( vmc->b & DAO_CALL_ASYNC ){
		DaoCallServer_AddCall( self );
		self->status = DAO_PROCESS_RUNNING;
		return 1;
	}
	if( vmc->code != DVM_MCALL ) return 0;
	if( frame->object && frame->object->isAsync ){
		DaoCallServer_AddCall( self );
		self->status = DAO_PROCESS_RUNNING;
		return 1;
	}
	return 0;
}
static int DaoProcess_InitBase( DaoProcess *self, DaoVmCode *vmc, DaoValue *caller )
{
	if( (vmc->b & DAO_CALL_INIT) && self->activeObject ){
		DaoClass *klass = self->activeObject->defClass;
		int init = self->activeRoutine->attribs & DAO_ROUT_INITOR;
		if( self->activeRoutine->routHost == klass->objType && init ){
			return klass->parent == caller;
		}
	}
	return 0;
}
static int DaoProcess_TryTailCall( DaoProcess *self, DaoRoutine *rout, DaoValue *O, DaoVmCode *vmc )
{
	int async = vmc->b & DAO_CALL_ASYNC;
	DaoObject *root = NULL;

	/*
	// No tail call optimization for wrapped C/C++ functions.
	// Because it is less useful and more inconvenient to setup properly.
	*/
	if( rout->pFunc != NULL ) return 0;

	if( !(vmc->b & DAO_CALL_TAIL) || self->topFrame->prev == self->baseFrame ) return 0;
	/* no tail call optimization when there is deferred code blocks: */
	if( self->defers->size > self->topFrame->deferBase ) return 0;

	switch( O ? O->type : 0 ){
	case DAO_CDATA   :
	case DAO_CSTRUCT :
		if( O->xCstruct.object ) root = O->xCstruct.object->rootObject;
		break;
	case DAO_OBJECT  : root = O->xObject.rootObject; break;
	}
	/* No tail call optimization for possible asynchronous calls: */
	/* No tail call optimization in constructors etc.: */
	/* (self->topFrame->state>>1): get rid of the DVM_FRAME_RUNNING flag: */
	if( async == 0 && (self->topFrame->state>>1) == 0 && daoConfig.optimize ){
		/* No optimization if the tail call has a return type different from the current: */
		if( rout->routType->aux == self->activeRoutine->routType->aux ){
			DaoProcess_PopFrame( self );
			return 1;
		}
	}
	return 0;
}
static void DaoProcess_PrepareCall( DaoProcess *self, DaoRoutine *rout,
		DaoValue *O, DaoValue *P[], DaoType *T[], int N, DaoVmCode *vmc, int noasync )
{
	DaoRoutine *rout2 = rout;
	int need_self = rout->routType->attrib & DAO_TYPE_SELF;
	if( DaoProcess_CheckInvarMethod( self, rout ) == 0 ) return;
	rout = DaoProcess_PassParams( self, rout, NULL, O, P, T, N, vmc->code );
	if( rout == NULL ){
		DaoProcess_RaiseError( self, "Param", "not matched (passing)" );
		DaoProcess_ShowCallError( self, rout2, O, P, N, vmc->code|((int)vmc->b<<16) );
		return;
	}
	if( need_self && rout->routHost && rout->routHost->tid == DAO_OBJECT ){
		if( O == NULL && N && P[0]->type == DAO_OBJECT ) O = P[0];
		if( O ) O = DaoObject_CastToBase( O->xObject.rootObject, rout->routHost );
		if( O == NULL ){
			DaoProcess_RaiseError( self, NULL, "self object is null" );
			return;
		}else if( O == O->xObject.defClass->objType->value ){
			DaoProcess_RaiseError( self, NULL, "self object is the default object" );
			return;
		}
	}
	if( noasync == 0 ) DaoProcess_TryTailCall( self, rout, O, vmc );
	DaoProcess_PushRoutine( self, rout, DaoValue_CastObject( O ) );
	if( noasync ) return;
	DaoProcess_TryAsynCall( self, vmc );
}
static void DaoProcess_DoCxxCall( DaoProcess *self, DaoVmCode *vmc,
		DaoType *hostype, DaoRoutine *func, DaoValue *selfpar, DaoValue *P[], DaoType *T[], int N, int noasync )
{
	DaoRoutine *rout = func;
	DaoVmSpace *vmspace = self->vmSpace;
	DaoValue *caller = self->activeValues[ vmc->a ];
	int status, code = vmc->code;
	int callmode = code|((int)vmc->b<<16);

	func = DaoRoutine_Resolve( rout, selfpar, NULL, P, T, N, callmode );
	if( func == NULL ){
		DaoProcess_ShowCallError( self, rout, selfpar, P, N, callmode );
		return;
	}
	if( DaoProcess_CheckInvarMethod( self, func ) == 0 ) return;
	if( (func = DaoProcess_PassParams( self, func, hostype, selfpar, P, T, N, code )) == NULL ){
		DaoProcess_ShowCallError( self, rout, selfpar, P, N, callmode );
		return;
	}
	DaoProcess_PushFunction( self, func );
	if( noasync == 0 && DaoProcess_TryAsynCall( self, vmc ) ) return;
#if 0
	if( caller->type == DAO_CTYPE ){
		DaoType *retype = caller->xCtype.cdtype;
		printf( ">>>>>>>>>>>>> %s %s\n", retype->name->chars, caller->xCdata.ctype->name->chars );
		GC_Assign( & self->topFrame->retype, retype );
	}
#endif
	DaoProcess_CallFunction( self, func, self->stackValues + self->topFrame->stackBase, self->parCount );
	status = self->status;
	DaoProcess_PopFrame( self );

	if( status == DAO_PROCESS_SUSPENDED ) self->status = status;
}
static void DaoProcess_DoNewCall( DaoProcess *self, DaoVmCode *vmc,
		DaoClass *klass, DaoValue *selfpar, DaoValue *params[], DaoType *types[], int npar )
{
	DaoValue *ret;
	DaoRoutine *rout;
	DaoRoutine *routines = klass->initRoutines;
	DaoObject *obj, *othis = NULL, *onew = NULL;
	int i, code = vmc->code;
	int callmode = code | (vmc->b<<16);
	int initbase = DaoProcess_InitBase( self, vmc, (DaoValue*) klass );
	if( initbase ){
		othis = self->activeObject;
	}else{
		othis = onew = DaoObject_New( klass );
	}
	rout = DaoRoutine_Resolve( routines, selfpar, NULL, params, types, npar, callmode );
	if( rout == NULL ){
		selfpar = (DaoValue*) othis;
		rout = DaoRoutine_Resolve( routines, selfpar, NULL, params, types, npar, callmode );
	}
	if( rout == NULL ) goto InvalidParameter;
	if( rout->pFunc ){
		if( DaoProcess_CheckInvarMethod( self, rout ) == 0 ) return;
		rout = DaoProcess_PassParams( self, rout, klass->objType, selfpar, params, types, npar, vmc->code );
		if( rout == NULL ) goto InvalidParameter;
		DaoProcess_PushFunction( self, rout );
		DaoProcess_SetActiveFrame( self, self->firstFrame ); /* return value in stackValues[0] */
		self->topFrame->active = self->firstFrame;
		DaoProcess_CallFunction( self, rout, self->stackValues + self->topFrame->stackBase, self->parCount );
		DaoProcess_PopFrame( self );

		ret = self->stackValues[0];
		if( ret && (ret->type == DAO_CDATA || ret->type == DAO_CSTRUCT) ){
			DaoCdata *cdata = & self->stackValues[0]->xCdata;
			DaoObject_SetParentCdata( othis, cdata );
			GC_Assign( & cdata->object, othis );
		}
		DaoProcess_PutValue( self, (DaoValue*) othis );
	}else{
		obj = othis;
		if( initbase ){
			obj = (DaoObject*) DaoObject_CastToBase( obj, rout->routHost );
			if( obj->isInited ) return;
		}
		obj->isInited = 1;
		DaoProcess_PrepareCall( self, rout, (DaoValue*) obj, params, types, npar, vmc, 1 );
		if( self->exceptions->size ) goto DeleteObject;
	}
	return;
InvalidParameter:
	DaoProcess_ShowCallError( self, routines, selfpar, params, npar, DVM_CALL );
DeleteObject:
	if( onew ){ GC_IncRC( onew ); GC_DecRC( onew ); }
}
void DaoProcess_DoCall2( DaoProcess *self, DaoVmCode *vmc, DaoValue *caller, DaoValue *selfpar, DaoValue *params[], DaoType *types[], int npar )
{
	int i, sup = 0;
	int code = vmc->code;
	int callmode = code | (vmc->b<<16);
	DaoStackFrame *topFrame = self->topFrame;
	DaoRoutine *rout, *rout2;
	DList *array, *bindings;

	if( caller->type == DAO_ROUTINE ){
		rout = (DaoRoutine*) caller;
		if( rout->pFunc ){
			DaoProcess_DoCxxCall( self, vmc, NULL, rout, selfpar, params, types, npar, 0 );
			return;
		}else if( rout->overloads == NULL && rout->body == NULL ){  /* function curry: */
			DaoValue *caller = (DaoValue*) rout->original;
			DaoVmCode vmc2 = *vmc;
			if( rout->original == NULL ){
				DaoProcess_RaiseError( self, "Type", "abstract routine not callable" );
				return;
			}
			if( rout->original->routType->attrib & DAO_TYPE_SELF ) vmc2.code = DVM_MCALL;
			array = DList_New(0);
			bindings = rout->routConsts->value;
			for(i=0; i<bindings->size; i++) DList_Append( array, bindings->items.pValue[i] );
			for(i=0; i<npar; i++) DList_Append( array, params[i] );
			DaoProcess_DoCall2( self, & vmc2, caller, NULL, array->items.pValue, NULL, array->size );
			DList_Delete( array );
			return;
		}
		/* No need to pass implicit self type, invar method will be checked separately */
		rout = DaoRoutine_Resolve( rout, selfpar, NULL, params, types, npar, callmode );
		if( rout == NULL ){
			rout2 = (DaoRoutine*) caller;
			goto InvalidParameter;
		}
		if( rout->pFunc ){
			DaoProcess_DoCxxCall( self, vmc, NULL, rout, selfpar, params, types, npar, 0 );
		}else{
			if( rout->attribs & DAO_ROUT_DECORATOR ){
#ifdef DAO_WITH_DECORATOR
				DaoRoutine *drout = (DaoRoutine*) rout;
				if( params[0]->type != DAO_ROUTINE ){
					DaoProcess_RaiseError( self, "Param", "invalid function decoration" );
					return;
				}
				drout = DaoRoutine_Decorate( & params[0]->xRoutine, drout, params, npar, 0 );
				DaoProcess_PutValue( self, (DaoValue*) drout );
#else
				DaoProcess_RaiseError( self, NULL, getCtInfo( DAO_DISABLED_DECORATOR ) );
#endif
				return;
			}
			DaoProcess_PrepareCall( self, rout, selfpar, params, types, npar, vmc, 0 );
		}
	}else if( caller->type == DAO_CLASS ){
		DaoProcess_DoNewCall( self, vmc, & caller->xClass, selfpar, params, types, npar );
		if( self->topFrame != topFrame ){
			GC_Assign( & self->topFrame->retype, caller->xClass.objType );
		}
	}else if( caller->type == DAO_OBJECT ){
		DaoClass *host = self->activeObject ? self->activeObject->defClass : NULL;
		rout = rout2 = DaoClass_FindMethod( caller->xObject.defClass, "()", host );
		if( rout == NULL ){
			DaoProcess_RaiseError( self, "Type", "class instance not callable" );
			return;
		}
		rout = DaoRoutine_Resolve( rout, caller, NULL, params, types, npar, callmode );
		if( rout == NULL ) goto InvalidParameter;
		if( rout->pFunc ){
			DaoProcess_DoCxxCall( self, vmc, NULL, rout, caller, params, types, npar, 0 );
		}else if( rout->type == DAO_ROUTINE ){
			DaoProcess_PrepareCall( self, rout, caller, params, types, npar, vmc, 0 );
		}
	}else if( caller->type == DAO_CTYPE ){
		DaoType *type = caller->xCtype.ctype;
		rout = rout2 = DaoType_GetInitor( type );
		if( rout == NULL ){
			DaoProcess_RaiseError( self, "Type", "C type not callable" );
			return;
		}
		rout = DaoRoutine_Resolve( rout, selfpar, NULL, params, types, npar, callmode );
		if( rout == NULL /*|| rout->pFunc == NULL*/ ) goto InvalidParameter;
		DaoProcess_DoCxxCall( self, vmc, type, rout, selfpar, params, types, npar, 1 );
		if( self->exceptions->size ) return;

		sup = DaoProcess_InitBase( self, vmc, caller );
		//printf( "sup = %i\n", sup );
		if( caller->type == DAO_CTYPE && sup ){
			DaoCstruct *cdata = (DaoCstruct*) self->activeValues[ vmc->c ];
			if( cdata && (cdata->type == DAO_CDATA || cdata->type == DAO_CSTRUCT) ){
				//printf( "%p %p %p\n", cdata, cdata->object, self->activeObject->rootObject );
				GC_Assign( & self->activeObject->parent, cdata );
				GC_Assign( & cdata->object, self->activeObject->rootObject );
			}
		}
	}else if( caller->type == DAO_CDATA || caller->type == DAO_CSTRUCT ){
		rout = rout2 = DaoType_FindFunctionChars( caller->xCdata.ctype, "()" );
		if( rout == NULL ){
			DaoProcess_RaiseError( self, "Type", "C object not callable" );
			return;
		}
		rout = DaoRoutine_Resolve( rout, caller, NULL, params, types, npar, callmode );
		if( rout == NULL /*|| rout->pFunc == NULL*/ ) goto InvalidParameter;
		DaoProcess_DoCxxCall( self, vmc, NULL, rout, caller, params, types, npar, 0 );
	}else if( caller->type == DAO_TYPE ){
		DaoType *type = (DaoType*) caller;
		rout = rout2 = DaoType_GetInitor( type );
		if( rout == NULL ){
			DaoProcess_RaiseError( self, "Type", "no constructor for the type" );
			return;
		}
		rout = DaoRoutine_Resolve( rout, selfpar, NULL, params, types, npar, callmode );
		if( rout == NULL /*|| rout->pFunc == NULL*/ ) goto InvalidParameter;
		DaoProcess_DoCxxCall( self, vmc, type, rout, selfpar, params, types, npar, 1 );
	}else{
		DaoProcess_RaiseError( self, "Type", "object not callable" );
	}
	return;
InvalidParameter:
	DaoProcess_ShowCallError( self, rout2, selfpar, params, npar, callmode );
}
static int DaoProcess_FastPassParams( DaoProcess *self, DaoType *partypes[], DaoValue *params[], int npar )
{
	int i;
	DaoValue **dests = self->stackValues + self->topFrame->stackBase;
	for(i=0; i<npar; ++i){
		DaoValue *param = params[i];
		if( dests[i] && dests[i]->xBase.refCount == 1 && param->type == dests[i]->type ){
			switch( param->type ){
			case DAO_BOOLEAN :
			case DAO_INTEGER :
				dests[i]->xInteger.value = param->xInteger.value;
				break;
			case DAO_FLOAT :
				dests[i]->xFloat.value = param->xFloat.value;
				break;
			case DAO_COMPLEX :
				dests[i]->xComplex.value = param->xComplex.value;
				break;
			case DAO_STRING :
				DString_Assign( dests[i]->xString.value, param->xString.value );
				break;
			case DAO_ENUM :
				if( dests[i]->xEnum.etype != param->xEnum.etype ){
					GC_Assign( & dests[i]->xEnum.etype, param->xEnum.etype );
				}
				dests[i]->xEnum.value = param->xEnum.value;
				break;
			default :
				GC_Assign( & dests[i], param );
				break;
			}
		}else if( param->type >= DAO_ARRAY ){
			switch( param->type ){
			case DAO_OBJECT : if( param->xObject.isNull ) return 0; break;
			case DAO_CDATA  : if( param->xCdata.data == NULL ) return 0; break;
			}
			GC_Assign( & dests[i], param );
		}else{
			DaoType *partype = partypes[i];
			if( partype->attrib & DAO_TYPE_PARNAMED ) partype = (DaoType*) partype->aux;
			DaoValue_CopyX( param, dests + i, partype );
		}
	}
	return 1;
}
void DaoProcess_DoCall( DaoProcess *self, DaoVmCode *vmc )
{
	int i, status, ret;
	int mode = vmc->b;
	int npar = vmc->b & 0xff;
	int mcall = vmc->code == DVM_MCALL;
	DaoType  *typebuf[DAO_MAX_PARAM+1];
	DaoValue *parbuf[DAO_MAX_PARAM+1];
	DaoValue *selfpar = NULL;
	DaoValue *caller = self->activeValues[ vmc->a ];
	DaoValue **params = self->activeValues + vmc->a + 1;
	DaoType  **types = self->activeTypes + vmc->a + 1;
	DaoProfiler *profiler = self->vmSpace->profiler;
	DaoRoutine *rout, *rout2 = NULL;
	DaoType *retype;

	self->activeCode = vmc;
	if( (mode & DAO_CALL_FAST) && caller->xRoutine.overloads == NULL ){
		DaoType **partypes = caller->xRoutine.routType->nested->items.pType;
		rout = (DaoRoutine*) caller;
		params = self->activeValues + vmc->a + 1;
		if( DaoProcess_CheckInvarMethod( self, rout ) == 0 ) return;
		for(i=0; i<npar; ++i){
			GC_IncRC( params[i] );
			parbuf[i] = params[i];
		}
		if( rout->pFunc == NULL ) DaoProcess_TryTailCall( self, rout, NULL, vmc );
		if( rout->pFunc ){
			DaoStackFrame *frame = DaoProcess_PushFrame( self, rout->parCount );
			DaoValue **values = self->stackValues + frame->stackBase;
			GC_Assign( & frame->routine, rout );
			frame->active = frame->prev->active;
			self->status = DAO_PROCESS_STACKED;
			ret = DaoProcess_FastPassParams( self, partypes, parbuf, npar );
			if( ret == 0 ) goto FastCallError;
			if( profiler ) profiler->EnterFrame( profiler, self, self->topFrame, 1 );
			DaoProcess_CallFunction( self, rout, values, rout->parCount );
			status = self->status;
			DaoProcess_PopFrame( self );
			if( status == DAO_PROCESS_SUSPENDED ) self->status = status;
		}else{
			DaoStackFrame *frame = DaoProcess_PushFrame( self, rout->body->regCount );
			frame->active = frame;
			self->status = DAO_PROCESS_STACKED;
			DaoProcess_InitTopFrame( self, rout, NULL );
			ret = DaoProcess_FastPassParams( self, partypes, parbuf, npar );
			if( ret == 0 ) goto FastCallError;
			if( profiler ) profiler->EnterFrame( profiler, self, self->topFrame, 1 );
		}
		for(i=0; i<npar; ++i) GC_DecRC( parbuf[i] );
		return;
FastCallError:
		for(i=0; i<npar; ++i) GC_DecRC( parbuf[i] );
		DaoProcess_PopFrame( self );
		DaoProcess_RaiseError( self, "Param", "null instance" );
		return;
	}

	if( caller == NULL || caller->type ==0 ){
		DaoProcess_RaiseError( self, "Type", "none object not callable" );
		return;
	}
	if( self->activeObject && mcall == 0 ) selfpar = (DaoValue*) self->activeObject;
	if( mode & DAO_CALL_NOSELF ) selfpar = NULL;
	if( (mode & DAO_CALL_EXPAR) && npar > mcall && params[npar-1]->type == DAO_TUPLE ){
		DaoTuple *tup = & params[npar-1]->xTuple;
		DaoType **itypes = tup->ctype->nested->items.pType;
		DList *ts = tup->ctype->nested;
		int i, m, n = 0;
		/* Handle explicit "self" argument: */
		if( ts->size && (itypes[0]->attrib & DAO_TYPE_SELFNAMED) ) selfpar = NULL;
		for(i=0; i<npar-1; ++i, ++n){
			parbuf[n] = params[i];
			typebuf[n] = types[i];
		}
		for(i=0,m=tup->size; i<m; ++i, ++n){
			DaoType *type = NULL;
			if( n > DAO_MAX_PARAM ){
				DaoProcess_RaiseError( self, "Param", "too many parameters" );
				return;
			}
			if( ts->size ){
				type = i < ts->size ? itypes[i] : itypes[ts->size-1];
				if( tup->ctype->variadic ) type = (DaoType*) type->aux;
			}
			parbuf[n] = tup->values[i];
			typebuf[n] = type;
		}
		params = parbuf;
		types = typebuf;
		npar = n;
	}
	DaoProcess_DoCall2( self, vmc, caller, selfpar, params, types, npar );
}

int DaoObject_InvokeMethod( DaoObject *self, DaoObject *othis, DaoProcess *proc,
		DString *name, DaoValue *P[], int N, int ignore_return, int execute );
static void DaoProcess_InitIter( DaoProcess *self, DaoVmCode *vmc )
{
	DString *name = self->string;
	DaoValue *va = self->activeValues[ vmc->a ];
	DaoValue *vc = self->activeValues[ vmc->c ];
	DaoType *type = DaoNamespace_GetType( self->activeNamespace, va );
	DaoInteger *index;
	DaoTuple *iter;
	int rc = DAO_ERROR_FIELD_NOTEXIST;

	if( va == NULL || va->type == 0 ) return;

	if( vc == NULL || vc->type != DAO_TUPLE || vc->xTuple.ctype != dao_type_for_iterator ){
		vc = (DaoValue*) DaoProcess_PutTuple( self, 2 );
	}

	iter = & vc->xTuple;
	iter->values[0]->xBoolean.value = 0;
	DaoTuple_SetItem( iter, dao_none_value, 1 );

	index = DaoInteger_New(0);
	GC_IncRC( index );
	if( va->type == DAO_STRING ){
		iter->values[0]->xInteger.value = va->xString.value->size >0;
		DaoValue_Copy( (DaoValue*) index, iter->values + 1 );
#ifdef DAO_WITH_NUMARRAY
	}else if( va->type == DAO_ARRAY ){
		iter->values[0]->xInteger.value = DaoArray_GetWorkSize( (DaoArray*) va ) >0;
		DaoValue_Copy( (DaoValue*) index, iter->values + 1 );
#endif
	}else if( va->type == DAO_LIST ){
		iter->values[0]->xInteger.value = va->xList.value->size >0;
		DaoValue_Copy( (DaoValue*) index, iter->values + 1 );
	}else if( va->type == DAO_MAP ){
		DNode *node = DMap_First( va->xMap.value );
		DaoValue **data = iter->values;
		data[0]->xInteger.value = va->xMap.value->size >0;
		if( data[1]->type != DAO_CDATA || data[1]->xCdata.ctype != dao_type_cdata ){
			/*
			// Do not use DaoWrappers_MakeCdata()!
			// DaoWrappers_MakeCdata() will make a wrapper that is unique
			// for the wrapped data "node", since the wrapped data will be
			// updated during each iteration, the correspondence between
			// wrapped data and the wrapper will be invalidated.
			// As a consequence, nested for-in loop on the same map will
			// not work!
			*/
			DaoCdata *it = DaoCdata_Wrap( dao_type_cdata, node );
			GC_Assign( & data[1], it );
		}else{
			data[1]->xCdata.data = node;
		}
	}else if( va->type == DAO_TUPLE ){
		iter->values[0]->xInteger.value = va->xTuple.size >0;
		DaoValue_Copy( (DaoValue*) index, iter->values + 1 );
	}else{
		DString_SetChars( name, "for" );
		if( va->type == DAO_OBJECT ){
			rc = DaoObject_InvokeMethod( & va->xObject, NULL, self, name, & vc, 1, 1, 0 );
		}else{
			DaoRoutine *meth = DaoType_FindFunction( type, name );
			if( meth ) rc = DaoProcess_Call( self, meth, va, &vc, 1 );
		}
		if( rc ) DaoProcess_RaiseError( self, daoExceptionNames[rc], name->chars );
	}
	GC_DecRC( index );
}
static void DaoProcess_TestIter( DaoProcess *self, DaoVmCode *vmc )
{
	int i, res = 1;
	for(i=0; i<vmc->b; ++i){
		DaoTuple *iter = (DaoTuple*) self->activeValues[vmc->a+i];
		res &= iter->values[0]->xBoolean.value != 0;
	}
	self->activeValues[vmc->c]->xBoolean.value = res;
}
void DaoProcess_DoIter( DaoProcess *self, DaoVmCode *vmc )
{
	if( vmc->b ){
		DaoProcess_TestIter( self, vmc );
	}else{
		DaoProcess_InitIter( self, vmc );
	}
}


void DaoProcess_DoList(  DaoProcess *self, DaoVmCode *vmc )
{
	DaoNamespace *ns = self->activeNamespace;
	DaoValue **regValues = self->activeValues;
	DaoList *list = DaoProcess_GetList( self, vmc );
	DaoType *type = self->activeTypes[vmc->c];
	const ushort_t opA = vmc->a;
	int i;

	DList_Resize( list->value, vmc->b, NULL );
	if( vmc->b > 0 && type ==NULL ){
		DaoType *abtp = DaoNamespace_GetType( ns, regValues[opA] );
		DaoType *t = DaoNamespace_MakeType( ns, "list", DAO_LIST, NULL, & abtp, 1 );
		GC_Assign( & list->ctype, t );
	}
	if( vmc->b && list->ctype == dao_type_list_empty ){
		GC_Assign( & list->ctype, dao_type_list_any );
	}
	if( type == dao_type_list_empty ) list->trait |= DAO_VALUE_CONST;
	for(i=0; i<vmc->b; i++){
		if( DaoList_SetItem( list, regValues[opA+i], i ) ){
			DaoProcess_RaiseError( self, "Value", "invalid items" );
			return;
		}
	}
}
static void DaoProcess_SetVectorValues( DaoProcess *self, DaoArray *a, DaoValue *v[], int N );
void DaoProcess_DoVector( DaoProcess *self, DaoVmCode *vmc )
{
#ifdef DAO_WITH_NUMARRAY
	const ushort_t opA = vmc->a;
	const ushort_t count = vmc->b;
	DaoType *type = self->activeTypes[vmc->c];
	DaoArray *array = DaoProcess_GetArray( self, vmc );

	if( type == dao_type_array_empty ) array->trait |= DAO_VALUE_CONST;
	if( count && array->etype == DAO_NONE ){
		DaoValue *p = self->activeValues[opA];
		switch( p->type ){
		case DAO_BOOLEAN :
		case DAO_INTEGER :
		case DAO_FLOAT :
		case DAO_COMPLEX : array->etype = p->type; break;
		case DAO_ARRAY : array->etype = p->xArray.etype; break;
		default : DaoProcess_RaiseError( self, "Value", "invalid items" ); return;
		}
	}else if( array->etype == DAO_NONE ){
		array->etype = DAO_FLOAT;
	}
	DaoProcess_SetVectorValues( self, array, self->activeValues + opA, count );
#else
	self->activeCode = vmc;
	DaoProcess_RaiseError( self, NULL, getCtInfo( DAO_DISABLED_NUMARRAY ) );
#endif
}
void DaoProcess_SetVectorValues( DaoProcess *self, DaoArray *array, DaoValue *values[], int N )
{
	daoint *dims = NULL;
	daoint i, j, k = 0;
	int m, ndim = 0;

#ifdef DAO_WITH_NUMARRAY
	for( j=0; j<N; j++){
		DaoValue *p = values[j];
		if( p == NULL || p->type == DAO_NONE ) goto InvalidItem;
		if( p->type > DAO_COMPLEX && p->type != DAO_ARRAY ) goto InvalidItem;
		if( p->type == DAO_ARRAY ){
			if( j && dims == NULL ) goto InvalidItem;
		}else{
			if( j && dims ) goto InvalidItem;
			continue;
		}
		if( dims == NULL ){
			ndim = p->xArray.ndim;
			dims = p->xArray.dims;
		}
		if( dims == p->xArray.dims ) continue;
		if( ndim != p->xArray.ndim ) goto InvalidItem;
		for(m=0; m<ndim; m++) if( dims[m] != p->xArray.dims[m] ) goto InvalidItem;
		continue;
InvalidItem:
		DaoProcess_RaiseError( self, "Value", "array item type or shape not matching" );
		return;
	}
	if( dims ){
		DaoArray_SetDimCount( array, ndim + 1 );
		array->dims[0] = N;
		memmove( array->dims + 1, dims, ndim*sizeof(daoint) );
		DaoArray_ResizeArray( array, array->dims, ndim + 1 );
	}else{
		DaoArray_ResizeVector( array, N );
	}
	k = 0;
	if( array->etype == DAO_BOOLEAN ){
		dao_boolean *vals = array->data.b;
		for( j=0; j<N; j++ ){
			DaoValue *p = values[j];
			if( p && p->type == DAO_ARRAY ){
				DaoArray *array2 = & p->xArray;
				for(i=0; i<array2->size; i++){
					vals[k] = DaoArray_GetBoolean( array2, i );
					k++;
				}
			}else{
				vals[k] = ! DaoValue_IsZero( p );
				k ++;
			}
		}
	}else if( array->etype == DAO_INTEGER ){
		dao_integer *vals = array->data.i;
		for( j=0; j<N; j++ ){
			DaoValue *p = values[j];
			if( p && p->type == DAO_ARRAY ){
				DaoArray *array2 = & p->xArray;
				for(i=0; i<array2->size; i++){
					vals[k] = DaoArray_GetInteger( array2, i );
					k++;
				}
			}else{
				vals[k] = DaoValue_GetInteger( p );
				k ++;
			}
		}
	}else if( array->etype == DAO_FLOAT ){
		dao_float *vals = array->data.f;
		for( j=0; j<N; j++ ){
			DaoValue *p = values[j];
			if( p && p->type == DAO_ARRAY ){
				DaoArray *array2 = & p->xArray;
				for(i=0; i<array2->size; i++){
					vals[k] = DaoArray_GetFloat( array2, i );
					k++;
				}
			}else{
				vals[k] = DaoValue_GetFloat( p );
				k ++;
			}
		}
	}else{
		dao_complex *vals = array->data.c;
		for( j=0; j<N; j++ ){
			DaoValue *p = values[j];
			if( p && p->type == DAO_ARRAY ){
				DaoArray *array2 = & p->xArray;
				for(i=0; i<array2->size; i++){
					vals[k] = DaoArray_GetComplex( array2, i );
					k++;
				}
			}else{
				vals[k] = DaoValue_GetComplex( p );
				k ++;
			}
		}
	}
#endif
}
void DaoProcess_DoAPList(  DaoProcess *self, DaoVmCode *vmc )
{
	int opb = vmc->b & (0xffff>>2);
	DaoList *list = DaoProcess_GetList( self, vmc );
	DaoValue **items, **regValues = self->activeValues;
	DaoValue *countValue = regValues[vmc->a + 1 + (opb == 3)];
	DaoValue *initValue = regValues[vmc->a];
	DaoValue *stepValue = opb == 3 ? regValues[vmc->a+1] : NULL;
	daoint i, num = DaoValue_GetInteger( countValue );
	double step = stepValue ? DaoValue_GetFloat( stepValue ) : 0.0;

	self->activeCode = vmc;
	if( countValue->type < DAO_INTEGER || countValue->type > DAO_FLOAT ){
		DaoProcess_RaiseError( self, "Value", "need number" );
		return;
	}
	if( initValue->type < DAO_INTEGER || initValue->type >= DAO_ENUM ){
		DaoProcess_RaiseError( self, "Value", "need a number or string as first value" );
		return;
	}
	DList_Resize( list->value, num, initValue );
	if( num == 0 || stepValue == NULL ) goto SetupType;

	items = list->value->items.pValue;
	switch( initValue->type ){
	case DAO_INTEGER :
		{
			daoint value = initValue->xInteger.value;
			if( stepValue->type == DAO_INTEGER ){
				daoint step = stepValue->xInteger.value;
				for(i=0; i<num; i++, value+=step) items[i]->xInteger.value = value;
			}else{
				for(i=0; i<num; i++, value+=step) items[i]->xInteger.value = value;
			}
			break;
		}
	case DAO_FLOAT :
		{
			double value = initValue->xFloat.value;
			for(i=0; i<num; i++, value+=step) items[i]->xFloat.value = value;
			break;
		}
	case DAO_COMPLEX :
		{
			dao_complex value = initValue->xComplex.value;
			dao_complex step = DaoValue_GetComplex( stepValue );
			for(i=0; i<num; i++){
				items[i]->xComplex.value = value;
				value.real += step.real;
				value.imag += step.imag;
			}
			break;
		}
	case DAO_STRING :
		{
			DString *value = initValue->xString.value;
			DString *one, *step = NULL, *buf = NULL;
			if( stepValue->type == DAO_STRING ){
				step = stepValue->xString.value;
			}else{
				step = buf = DString_New();
				DaoValue_GetString( stepValue, buf );
			}
			one = DString_Copy( value );
			for(i=0; i<num; i++){
				DString_Assign( items[i]->xString.value, one );
				if( step ) DString_Append( one, step );
			}
			DString_Delete( one );
			if( buf ) DString_Delete( buf );
			break;
		}
	default: break;
	}
SetupType:
	if( self->activeTypes[ vmc->c ] == NULL ){
		DaoNamespace *ns = self->activeNamespace;
		DaoType *et = DaoNamespace_GetType( ns, initValue );
		DaoType *tp = DaoNamespace_MakeType( ns, "list", DAO_LIST, NULL, & et, et !=NULL );
		GC_Assign( & list->ctype, tp );
	}
}
void DaoProcess_DoAPVector( DaoProcess *self, DaoVmCode *vmc )
{
#ifdef DAO_WITH_NUMARRAY
	int opb = vmc->b & (0xffff>>2);
	DaoArray *array = NULL;
	DaoValue **regValues = self->activeValues;
	DaoValue *countValue = regValues[vmc->a + 1 + (opb == 3)];
	DaoValue *initValue = regValues[vmc->a];
	DaoValue *stepValue = opb == 3 ? regValues[vmc->a+1] : NULL;
	double step = stepValue ? DaoValue_GetFloat( stepValue ) : 0.0;
	daoint num = DaoValue_GetInteger( countValue );
	daoint i, j, k, m, N, S, transvec = 0; /* transposed vector */

	self->activeCode = vmc;
	if( countValue->type < DAO_INTEGER || countValue->type > DAO_FLOAT ){
		DaoProcess_RaiseError( self, "Value", "need number" );
		return;
	}
	array = DaoProcess_GetArray( self, vmc );
	if( array->etype == DAO_NONE ) array->etype = initValue->type;
	DaoArray_ResizeVector( array, num );

	switch( array->etype ){
	case DAO_INTEGER :
		{
			double value;
			if( stepValue == NULL || stepValue->type == DAO_INTEGER ){
				if( initValue->type == DAO_INTEGER ){
					daoint value = initValue->xInteger.value;
					daoint step = stepValue ? stepValue->xInteger.value : 0;
					for(i=0; i<num; i++, value+=step) array->data.i[i] = value;
					break;
				}
			}
			value = DaoValue_GetFloat( initValue );
			for(i=0; i<num; i++, value+=step) array->data.i[i] = (daoint)value;
			break;
		}
	case DAO_FLOAT :
		{
			double value = DaoValue_GetFloat( initValue );
			for(i=0; i<num; i++, value+=step) array->data.f[i] = value;
			break;
		}
	case DAO_COMPLEX :
		{
			dao_complex value = DaoValue_GetComplex( initValue );
			dao_complex step = DaoValue_GetComplex( stepValue ? stepValue : dao_none_value );
			for(i=0; i<num; i++){
				array->data.c[i] = value;
				COM_IP_ADD( value, step );
			}
			break;
		}
	default: break;
	}
#else
	DaoProcess_RaiseError( self, NULL, getCtInfo( DAO_DISABLED_NUMARRAY ) );
#endif
}
void DaoProcess_DoMap( DaoProcess *self, DaoVmCode *vmc )
{
	int i, c;
	int opA = vmc->a;
	int bval = vmc->b & (0xffff>>2);
	int mode = vmc->b >> 14;
	DaoNamespace *ns = self->activeNamespace;
	DaoValue **pp = self->activeValues;
	DaoType *type = self->activeTypes[vmc->c];
	DaoMap *map = DaoProcess_GetMap( self, vmc, mode == DVM_ENUM_MODE1 );

	if( type == dao_type_map_empty ) map->trait |= DAO_VALUE_CONST;
	if( bval == 2 && pp[opA]->type ==0 && pp[opA+1]->type ==0 ) return;
	for( i=0; i<bval-1; i+=2 ){
		if( DaoMap_Find( map, pp[opA+i] ) != NULL ){
			DString *key = DaoValue_GetString( pp[opA+i], self->string );
			const char *msg = "Enumeration with duplicated key \"%s\"!";
			DaoProcess_RaiseException2( self, "Warning", msg, key->chars );
		}
		if( (c = DaoMap_Insert2( map, pp[opA+i], pp[opA+i+1], self ) ) ){
			if( c ==1 ){
				DaoProcess_RaiseError( self, "Type", "key not matching" );
			}else if( c ==2 ){
				DaoProcess_RaiseError( self, "Type", "value not matching" );
			}
			break;
		}
	}
	if( bval >0 && self->activeTypes[ vmc->c ] ==NULL ){
		/* for constant evaluation only */
		DaoType *tp[2], *t, *any = dao_type_any;
		tp[0] = DaoNamespace_GetType( ns, pp[opA] );
		tp[1] = DaoNamespace_GetType( ns, pp[opA+1] );
		for(i=2; i<bval; i+=2){
			DaoType *tk = DaoNamespace_GetType( ns, pp[opA+i] );
			DaoType *tv = DaoNamespace_GetType( ns, pp[opA+i+1] );
			if( DaoType_MatchTo( tk, tp[0], 0 )==0 ) tp[0] = any;
			if( DaoType_MatchTo( tv, tp[1], 0 )==0 ) tp[1] = any;
			if( tp[0] ==any && tp[1] ==any ) break;
		}
		t = DaoNamespace_MakeType( ns, "map", DAO_MAP, NULL, tp, 2 );
		GC_Assign( & map->ctype, t );
	}
}
void DaoProcess_DoMatrix( DaoProcess *self, DaoVmCode *vmc )
{
#ifdef DAO_WITH_NUMARRAY
	const ushort_t opA = vmc->a;
	const ushort_t bval = vmc->b;
	daoint i, size, numtype = DAO_INTEGER;
	DaoValue **regv = self->activeValues;
	DaoArray *array = NULL;
	daoint dim[2];

	dim[0] = bval >> 8;
	dim[1] = bval & 0xff;
	size = dim[0] * dim[1];
	array = DaoProcess_GetArray( self, vmc );
	if( size ){
		numtype = regv[opA]->type;
		if( numtype == DAO_NONE || numtype > DAO_COMPLEX ){
			DaoProcess_RaiseError( self, NULL, "invalid matrix enumeration" );
			return;
		}
	}
	if( array->etype == DAO_NONE ) array->etype = numtype;
	/* TODO: more restrict type checking on elements. */
	DaoArray_ResizeArray( array, dim, 2 );
	if( numtype == DAO_INTEGER ){
		dao_integer *vec = array->data.i;
		for(i=0; i<size; i++) vec[i] = DaoValue_GetInteger( regv[ opA+i ] );
	}else if( numtype == DAO_FLOAT ){
		dao_float *vec = array->data.f;
		for(i=0; i<size; i++) vec[i] = DaoValue_GetFloat( regv[ opA+i ] );
	}else{
		dao_complex *vec = array->data.c;
		for(i=0; i<size; i++) vec[i] = DaoValue_GetComplex( regv[ opA+i ] );
	}
#else
	self->activeCode = vmc;
	DaoProcess_RaiseError( self, NULL, getCtInfo( DAO_DISABLED_NUMARRAY ) );
#endif
}

DaoType* DaoRoutine_PartialCheck( DaoNamespace *NS, DaoType *T, DList *RS, DList *TS, int C, int *W, int *M );

void DaoProcess_DoPacking( DaoProcess *self, DaoVmCode *vmc )
{
	int i, k;
	int opa = vmc->a;
	int opb = vmc->b;
	DaoObject *object;
	DaoVariable **mtype;
	DaoValue **values = self->activeValues + opa + 1;
	DaoValue *p = self->activeValues[opa];
	DaoValue *selfobj = NULL;
	DNode *node;

	if( vmc->code == DVM_MPACK && p->type != DAO_ROUTINE ){
		selfobj = values[0];
		values ++;
		opb --;
	}

	self->activeCode = vmc;
	switch( p->type ){
	case DAO_CLASS :
		{
			DaoClass *klass = & p->xClass;
			object = DaoObject_New( klass );
			DaoProcess_SetValue( self, vmc->c, (DaoValue*)object );
			mtype = klass->instvars->items.pVar;
			if( !(klass->attribs & DAO_CLS_AUTO_INITOR)
					|| (klass->attribs & (DAO_CLS_PRIVATE_VAR|DAO_CLS_PROTECTED_VAR)) ){
				DaoProcess_RaiseError( self, NULL, "cannot initialize instance" );
				break;
			}else if( opb >= object->valueCount ){
				DaoProcess_RaiseError( self, NULL, "enumerating too many members" );
				break;
			}
			for( i=0; i<opb; i++){
				k = i+1; /* skip self */
				p = values[i];
				if( p->type == DAO_PAR_NAMED ){
					DaoNameValue *nameva = & p->xNameValue;
					node = DMap_Find( klass->lookupTable, nameva->name );
					if( node == NULL || LOOKUP_ST( node->value.pInt ) != DAO_OBJECT_VARIABLE ){
						DaoProcess_RaiseError( self, "Field::NotExist", "" );
						break;
					}
					k = LOOKUP_ID( node->value.pInt );
					p = nameva->value;
				}
				if( DaoValue_Move( p, object->objValues + k, mtype[k]->dtype ) ==0 ){
					DaoType *type = DaoNamespace_GetType( self->activeNamespace, p );
					DaoProcess_RaiseTypeError( self, type, mtype[k]->dtype, "moving" );
					break;
				}
			}
			break;
		}
	case DAO_ROUTINE :
		{
			int wh = 0, mc = 0, call = DVM_CALL + (vmc->code - DVM_PACK);
			DaoNamespace *NS = self->activeNamespace;
			DaoRoutine *parout = DaoRoutine_New( NS, NULL, 0 );
			DaoRoutine *routine = (DaoRoutine*) p;
			DaoType *routype = routine->routType;
			DaoList *bindings = NULL;
			DList *routines = NULL;
			DList *partypes = DList_New(0);

			for(i=0; i<opb; i++) DList_Append( partypes, DaoNamespace_GetType( NS, values[i] ) );

			if( routine->overloads ){
				routines = routine->overloads->routines;
			}else if( routine->body == NULL && routine->pFunc == NULL && routine->original ){
				bindings = routine->routConsts;
				routine = routine->original;
			}
			parout->routType = DaoRoutine_PartialCheck( NS, routype, routines, partypes, call, & wh, & mc );
			GC_IncRC( parout->routType );
			DList_Delete( partypes );
			if( mc > 1 ){
				DaoRoutine_Delete( parout );
				DaoProcess_RaiseError( self, NULL,
						"ambigious partial function application on overloaded functions" );
				break;
			}else if( parout->routType == NULL ){
				DaoRoutine_Delete( parout );
				DaoProcess_RaiseError( self, NULL, "invalid partial function application" );
				break;
			}
			if( routine->overloads ){
				parout->original = routines->items.pRoutine[wh];
			}else{
				parout->original = routine;
			}
			GC_IncRC( parout->original );
			if( bindings ) DList_Assign( parout->routConsts->value, bindings->value );
			/* skip the self value if the routine needs none: */
			i = vmc->code == DVM_MPACK && (parout->original->routType->attrib & DAO_TYPE_SELF) == 0;
			for(; i<opb; i++) DList_Append( parout->routConsts->value, values[i] );
			DaoProcess_SetValue( self, vmc->c, (DaoValue*) parout );
			break;
		}
	case DAO_TYPE :
		{
			DaoType *type = (DaoType*) p;
			DaoType *retype = DaoProcess_GetCallReturnType( self, vmc, type->tid );
			dao_complex c = {0.0,0.0};
			dao_complex *cplx;
			DString *str;
			DaoArray *vec;
			DaoList *list;
			DaoTuple *tuple;
			if( retype != type && DaoType_MatchTo( type, retype, NULL ) == 0 ){
				DaoProcess_RaiseError( self, NULL, "invalid enumeration" );
				break;
			}
			switch( type->tid ){
			case DAO_COMPLEX :
			case DAO_STRING :
				for(i=0; i<opb; ++i){
					int tid = values[i]->type;
					if( tid == 0 || tid > DAO_FLOAT ){
						DaoProcess_RaiseError( self, NULL, "need numbers in enumeration" );
						return;
					}
				}
				break;
			}
			switch( type->tid ){
			case DAO_COMPLEX :
				cplx = DaoProcess_PutComplex( self, c );
				if( opb > 0 ) cplx->real = DaoValue_GetFloat( values[0] );
				if( opb > 1 ) cplx->imag = DaoValue_GetFloat( values[1] );
				if( opb > 2 ) DaoProcess_RaiseError( self, NULL, "too many values" );
				break;
			case DAO_STRING :
				str = DaoProcess_PutChars( self, "" );
				DString_Reserve( str, opb );
				for(i=0; i<opb; ++i){
					daoint ch = DaoValue_GetInteger( values[i] );
					if( ch < 0 ){
						DaoProcess_RaiseError( self, NULL, "invalid character" );
						return;
					}
					DString_AppendWChar( str, ch );
				}
				break;
#ifdef DAO_WITH_NUMARRAY
			case DAO_ARRAY :
				vec = DaoProcess_GetArrayByType( self, vmc, type );
				DaoProcess_SetVectorValues( self, vec, values, opb );
				break;
#endif
			case DAO_LIST :
				list = DaoProcess_GetListByType( self, vmc, type );
				DList_Resize( list->value, opb, NULL );
				for(i=0; i<opb; ++i){
					if( DaoList_SetItem( list, values[i], i ) ){
						DaoProcess_RaiseError( self, "Value", "invalid items" );
						return;
					}
				}
				break;
			case DAO_TUPLE :
				tuple = DaoProcess_GetTuple( self, type, opb, 0 );
				DaoProcess_MakeTuple( self, tuple, values, opb );
				break;
			default :
				DaoProcess_RaiseError( self, NULL, "invalid enumeration" );
				break;
			}
			break;
		}
	default :
		DaoProcess_RaiseError( self, NULL, "invalid enumeration" );
		break;
	}
}

/*
// Operator (in daoBitBoolArithOpers) validity rules,
// for operation involving DaoObject:
//
// A. when one of the operand is not DaoObject:
//    1. all these operators are not valid, unless overloaded;
//
// B. when both operands are DaoObject:
//
//    1. AND, OR, LT, LE, EQ, NE are valid, only if none operator
//       in daoBitBoolArithOpers is overloaded; In this case,
//       the operations will be based on pointers;
//
//    2. AND, OR, LT, LE, EQ, NE are based on pointers, if they
//       are used inside the function overloaded for the same
//       operator. Example:
//
//       class Test{
//         operator == ( A : Test, B : Test ){
//           return A == B; # this will be based on pointers!
//         }
//       }
//
//    3. since "A>B" (or "A>=B") is compiled as "B<A" (or "B<=A"),
//       when a DVM_LT or DVM_LE is executed, "operator<()"
//       or "operator<=()" will be search first, if not found,
//       then "operator>()" or "operator>=()" is searched,
//       and applied by swapping A and B'
//
//    4. "A<B" and "A>B" inside "operator<()" and "operator>()"
//        or "A<=B" and "A>=B" inside "operator<=()" and "operator>=()"
//        will be based on pointers.
*/
/*
// Examples of possible ways of operator overloading:
// All these overloading functions must be "static",
// namely, they do not require a class instance for being invoked:
//
// Unary operation:
// operator ! ( C : Number, A : Number ){... return C_or_something_else}
// operator ! ( A : Number ){... return something}
//
// Binary operation:
// operator + ( C : Number, A : Number, B : Number ){... return C_or_else}
// operator + ( A : Number, B : Number ){... return something}
//
// The first method is always tried first if C is found NOT to be null,
// and have reference count equal to one;
//
// For binary operation, if C == A, the following will be tried first:
// operator += ( C : Number, B : Number ){... return C_or_else}
*/
static int DaoProcess_TryUserArith( DaoProcess *self, DaoValue *A, DaoValue *B, DaoValue *C, DaoType *TA, DaoType *TB )
{
	DaoRoutine *rout = 0;
	DaoObject *object = (DaoObject*)A;
	DaoCdata *cdata = (DaoCdata*)A;
	DaoCinValue *cinvalue = (DaoCinValue*)A;
	DaoClass *klass;
	DString *name = self->string;
	DaoValue **p, *par[3];
	DaoType *argt[3] = { NULL };
	DaoValue *value = NULL;
	int code = self->activeCode->code;
	int boolres = code >= DVM_AND && code <= DVM_NE;
	int bothobj = B ? A->type == B->type : 0;
	int recursive = 0;
	int compo = 0; /* try composite operator */
	int nopac = 0; /* do not pass C as parameter */
	int npar = 3;
	int first = 1;
	int n, rc = 0;

	/* C = A + B */
	par[0] = C;
	par[1] = A;
	par[2] = B;
	argt[1] = TA;
	argt[2] = TB;
	if( C == A && daoBitBoolArithOpers2[ code-DVM_NOT ] ){
		DString_SetChars( name, daoBitBoolArithOpers2[ code-DVM_NOT ] );
		if( A->type == DAO_OBJECT ){
			if( DString_EQ( name, self->activeRoutine->routName ) ) recursive = 1;
			if( recursive && object->defClass->objType == self->activeRoutine->routHost ) return 0;
			klass = object->defClass;
			rc = DaoObject_GetData( object, name, & value,  self->activeObject );
		}else if( A->type == DAO_CINVALUE ){
			value = (DaoValue*) DaoType_FindFunction( cinvalue->cintype->vatype, name );
		}else{ /* DAO_CDATA */
			value = (DaoValue*) DaoType_FindFunction( cdata->ctype, name );
		}
		if( rc == 0 && value && value->type == DAO_ROUTINE ){
			rout = (DaoRoutine*) value;
			/* Check the method with self parameter first, then other methods: */
			if( DaoProcess_PushCallableX( self, rout, A, & B, & TB, B!=NULL ) == 0 ) return 1;
			if( DaoProcess_PushCallableX( self, rout, NULL, par+1, argt+1, 2 ) == 0 ) return 1;
		}
	}
	DString_SetChars( name, daoBitBoolArithOpers[ code-DVM_NOT ] );
TryAgain:
	if( A->type == DAO_OBJECT ){
		if( DString_EQ( name, self->activeRoutine->routName ) ) recursive = 1;
		if( recursive && object->defClass->objType == self->activeRoutine->routHost ) return 0;
		klass = object->defClass;
		rc = DaoObject_GetData( object, name, & value,  self->activeObject );
	}else if( A->type == DAO_CINVALUE ){
		value = (DaoValue*) DaoType_FindFunction( cinvalue->cintype->vatype, name );
	}else{ /* DAO_CDATA */
		value = (DaoValue*) DaoType_FindFunction( cdata->ctype, name );
	}
	if( rc == 0 && value && value->type == DAO_ROUTINE ){
		rout = (DaoRoutine*) value;
		if( C && C->xBase.refCount == 1 ){ /* Check methods that can take three parameters: */
			/* Check only static method that takes parameters: C, A, B: */
			if( DaoProcess_PushCallableX( self, rout, NULL, par, argt, 2+(B!=NULL) ) == 0 ) return 1;
		}
		/* Check the method with self parameter first, then other methods: */
		if( DaoProcess_PushCallableX( self, rout, A, & B, & TB, B!=NULL ) == 0 ) return 1;
		if( DaoProcess_PushCallableX( self, rout, NULL, par+1, argt+1, 1+(B!=NULL) ) == 0 ) return 1;
	}
	if( first && (code == DVM_LT || code == DVM_LE) ){
		first = 0;
		if( code == DVM_LT ){
			DString_SetChars( name, ">" );
		}else{
			DString_SetChars( name, ">=" );
		}
		if( B && (B->type == DAO_OBJECT || B->type == DAO_CDATA
					|| B->type == DAO_CSTRUCT || B->type == DAO_CINVALUE ) ){
			par[1] = B;
			par[2] = A;
			A = par[1];
			B = par[2];
			goto TryAgain;
		}
	}
	return 0;
}
void DaoProcess_DoBinArith( DaoProcess *self, DaoVmCode *vmc )
{
	DaoValue *A = self->activeValues[ vmc->a ];
	DaoValue *B = self->activeValues[ vmc->b ];
	DaoValue *C = self->activeValues[ vmc->c ];

	self->activeCode = vmc;
	if( A == NULL || B == NULL ){
		DaoProcess_RaiseError( self, "Value", "on none object" );
		return;
	}

	if( A->type >= DAO_BOOLEAN && A->type <= DAO_INTEGER && B->type >= DAO_BOOLEAN && B->type <= DAO_INTEGER ){
		dao_integer va = DaoValue_GetInteger( A );
		dao_integer vb = DaoValue_GetInteger( B );
		dao_integer res = 0;
		switch( vmc->code ){
		case DVM_DIV:
			if( vb == 0 ) goto ErrorDivByZero;
			res = va / vb;
			break;
		case DVM_MOD:
			if( vb == 0 ) goto ErrorDivByZero;
			res = va % vb;
			break;
		case DVM_ADD: res = va + vb; break;
		case DVM_SUB: res = va - vb; break;
		case DVM_MUL: res = va * vb; break;
		case DVM_POW: res = pow( va, vb ); break;
		default : break;
		}
		DaoProcess_PutInteger( self, res );
	}else if( A->type >= DAO_BOOLEAN && A->type <= DAO_FLOAT && B->type >= DAO_BOOLEAN && B->type <= DAO_FLOAT ){
		double va = DaoValue_GetFloat( A );
		double vb = DaoValue_GetFloat( B );
		double res = 0.0;
		switch( vmc->code ){
		case DVM_DIV:
			if( vb == 0.0 ) goto ErrorDivByZero;
			res = va / vb;
			break;
		case DVM_MOD:
			if( vb == 0.0 ) goto ErrorDivByZero;
			res = va - vb * (dao_integer)(va/vb);
			break;
		case DVM_ADD: res = va + vb; break;
		case DVM_SUB: res = va - vb; break;
		case DVM_MUL: res = va * vb; break;
		case DVM_POW: res = pow( va, vb ); break;
		default : break;
		}
		DaoProcess_PutFloat( self, res );
	}else if( B->type >= DAO_BOOLEAN && B->type <= DAO_FLOAT && A->type == DAO_COMPLEX ){
		dao_complex res = {0.0,0.0};
		double f = DaoValue_GetFloat( B );
		res.real = A->xComplex.value.real;
		res.imag = A->xComplex.value.imag;
		switch( vmc->code ){
		case DVM_ADD: res.real += f; break;
		case DVM_SUB: res.real -= f; break;
		case DVM_MUL: res.real *= f; res.imag *= f; break;
		case DVM_DIV: res.real /= f; res.imag /= f; break;
		default: break; /* XXX: pow for complex??? */
		}
		DaoProcess_PutComplex( self, res );
	}else if( A->type >= DAO_BOOLEAN && A->type <= DAO_FLOAT && B->type == DAO_COMPLEX ){
		dao_complex res = {0.0,0.0};
		double n, f = DaoValue_GetFloat( A );
		double real = B->xComplex.value.real;
		double imag = B->xComplex.value.imag;
		switch( vmc->code ){
		case DVM_DIV:
			n = real * real + imag * imag;
			res.real = f * real / n;
			res.imag = f * imag / n;
			break;
		case DVM_ADD: res.real = f + real;  res.imag = imag; break;
		case DVM_SUB: res.real = f - real;  res.imag = - imag; break;
		case DVM_MUL: res.real = f * real;  res.imag = f * imag; break;
		default: break; /* XXX: pow for complex??? */
		}
		DaoProcess_PutComplex( self, res );
	}else if( A->type == DAO_COMPLEX && B->type == DAO_COMPLEX ){
		dao_complex res = {0.0,0.0};
		double AR = A->xComplex.value.real;
		double AI = A->xComplex.value.imag;
		double BR = B->xComplex.value.real;
		double BI = B->xComplex.value.imag;
		double N = 0;
		switch( vmc->code ){
		case DVM_ADD:
			res.real = AR + BR;
			res.imag = AI + BI;
			break;
		case DVM_SUB:
			res.real = AR - BR;
			res.imag = AI - BI;
			break;
		case DVM_MUL:
			res.real = AR * BR - AI * BI;
			res.imag = AR * BI + AI * BR;
			break;
		case DVM_DIV:
			N = BR * BR + BI * BI;
			res.real = (AR * BR + AI * BI) / N;
			res.imag = (AR * BI - AI * BR) / N;
			break;
		default: break; /* XXX: pow for complex??? */
		}
		DaoProcess_PutComplex( self, res );
#ifdef DAO_WITH_NUMARRAY
	}else if( B->type >= DAO_INTEGER && B->type <= DAO_COMPLEX && A->type == DAO_ARRAY ){
		DaoArray *na = & A->xArray;
		DaoArray *nc = na;
		if( vmc->a != vmc->c ){
			nc = DaoProcess_GetArray( self, vmc );
			if( nc->etype == DAO_NONE ) nc->etype = na->etype;
		}
		DaoArray_array_op_number( nc, na, B, vmc->code, self );
	}else if( A->type >= DAO_INTEGER && A->type <= DAO_COMPLEX && B->type == DAO_ARRAY ){
		DaoArray *nb = & B->xArray;
		DaoArray *nc = nb;
		if( vmc->b != vmc->c ){
			nc = DaoProcess_GetArray( self, vmc );
			if( nc->etype == DAO_NONE ) nc->etype = nb->etype;
		}
		DaoArray_number_op_array( nc, A, nb, vmc->code, self );
	}else if( A->type == DAO_ARRAY && B->type == DAO_ARRAY ){
		DaoArray *na = & A->xArray;
		DaoArray *nb = & B->xArray;
		DaoArray *nc;
		if( vmc->a == vmc->c ){
			nc = na;
		}else if( vmc->b == vmc->c ){
			nc = nb;
		}else{
			nc = DaoProcess_GetArray( self, vmc );
			if( nc->etype == DAO_NONE ) nc->etype = na->etype > nb->etype ? na->etype : nb->etype;
		}
		DaoArray_ArrayArith( nc, na, nb, vmc->code, self );
#endif
	}else if( A->type == DAO_STRING && B->type == DAO_STRING && vmc->code == DVM_ADD ){
		if( vmc->a == vmc->c ){
			DString_Append( A->xString.value, B->xString.value );
		}else if( vmc->b == vmc->c ){
			DString_Insert( B->xString.value, A->xString.value, 0, 0, 0 );
		}else{
			DaoValue *C = DaoProcess_PutValue( self, A );
			DString_Append( C->xString.value, B->xString.value );
		}
	}else if( A->type == DAO_STRING && B->type == DAO_STRING && vmc->code == DVM_DIV ){
		if( vmc->a == vmc->c ){
			DString *base = DString_Copy( A->xString.value );
			DString_Assign( A->xString.value, B->xString.value );
			Dao_MakePath( base, A->xString.value );
			DString_Delete( base );
		}else if( vmc->b == vmc->c ){
			Dao_MakePath( A->xString.value, B->xString.value );
		}else{
			DaoValue *C = DaoProcess_PutValue( self, B );
			Dao_MakePath( A->xString.value, C->xString.value );
		}
	}else if( A->type == DAO_ENUM && B->type == DAO_ENUM
			 && (vmc->code == DVM_ADD || vmc->code == DVM_SUB) ){
		DaoType *ta = A->xEnum.etype;
		DaoType *tb = B->xEnum.etype;
		DaoEnum *denum = & A->xEnum;
		int rc = 0;
		if( A->xEnum.subtype == DAO_ENUM_SYM && B->xEnum.subtype == DAO_ENUM_SYM ){
			DaoNamespace *NS = self->activeNamespace;
			denum = DaoProcess_GetEnum( self, vmc );
			if( denum->etype == NULL ){ /* Can happen in constant evaluation: */
				DaoType *tp;
				DNode *it;
				DString_Reset( self->string, 0 );
				DString_Append( self->string, ta->mapNames->root->key.pString );
				DString_AppendChar( self->string, ';' );
				DString_Append( self->string, tb->mapNames->root->key.pString );
				tp = DaoNamespace_MakeEnumType( NS, self->string->chars );
				DaoEnum_SetType( denum, tp );
			}
			DaoEnum_AddValue( denum, (DaoEnum*) A );
			DaoEnum_AddValue( denum, (DaoEnum*) B );
			return;
		}
		if( vmc->c != vmc->a ){
			denum = DaoProcess_GetEnum( self, vmc );
			if( denum->etype == NULL ) DaoEnum_SetType( denum, A->xEnum.etype );
			DaoEnum_SetValue( denum, & A->xEnum );
		}
		if( vmc->code == DVM_ADD ){
			rc = DaoEnum_AddValue( denum, & B->xEnum );
		}else{
			rc = DaoEnum_RemoveValue( denum, & B->xEnum );
		}
		if( rc == 0 ){
			if( denum->subtype != DAO_ENUM_FLAG )
				DaoProcess_RaiseError( self, "Type", "not combinable enum" );
			else
				DaoProcess_RaiseError( self, "Type", "symbol not found in the enum" );
			return;
		}
	}else if( A->type == DAO_OBJECT || A->type == DAO_CDATA
			|| A->type == DAO_CSTRUCT || A->type == DAO_CINVALUE ){
		DaoType *ta = self->activeTypes[ vmc->a ];
		DaoType *tb = self->activeTypes[ vmc->b ];
		self->activeCode = vmc;
		if( DaoProcess_TryUserArith( self, A, B, C, ta, tb ) == 0 ){
			DaoProcess_RaiseError( self, "Type", NULL );
		}
	}else{
		DaoProcess_RaiseError( self, "Type", "" );
	}
	return;
ErrorDivByZero:
	DaoProcess_RaiseError( self, "Float::DivByZero", "" );
}
/* binary operation with boolean result. */
void DaoProcess_DoBinBool(  DaoProcess *self, DaoVmCode *vmc )
{
	DaoValue *A = self->activeValues[ vmc->a ];
	DaoValue *B = self->activeValues[ vmc->b ];
	DaoValue *C = NULL;
	int D = 0, rc = 0;

	self->activeCode = vmc;
	if( A == NULL ) A = dao_none_value;
	if( B == NULL ) B = dao_none_value;

	if( A->type >= DAO_BOOLEAN && A->type <= DAO_INTEGER
			&& B->type >= DAO_BOOLEAN && B->type <= DAO_INTEGER ){
		switch( vmc->code ){
		case DVM_AND: D = DaoValue_GetInteger( A ) && DaoValue_GetInteger( B ); break;
		case DVM_OR:  D = DaoValue_GetInteger( A ) || DaoValue_GetInteger( B ); break;
		case DVM_LT:  D = DaoValue_GetInteger( A ) < DaoValue_GetInteger( B ); break;
		case DVM_LE:  D = DaoValue_GetInteger( A ) <= DaoValue_GetInteger( B ); break;
		case DVM_EQ:  D = DaoValue_GetInteger( A ) == DaoValue_GetInteger( B ); break;
		case DVM_NE:  D = DaoValue_GetInteger( A ) != DaoValue_GetInteger( B ); break;
		default: break;
		}
	}else if( A->type >= DAO_BOOLEAN && A->type <= DAO_FLOAT
			&& B->type >= DAO_BOOLEAN && B->type <= DAO_FLOAT ){
		switch( vmc->code ){
		case DVM_AND: D = DaoValue_GetFloat( A ) && DaoValue_GetFloat( B ); break;
		case DVM_OR:  D = DaoValue_GetFloat( A ) || DaoValue_GetFloat( B ); break;
		case DVM_LT:  D = DaoValue_GetFloat( A ) < DaoValue_GetFloat( B ); break;
		case DVM_LE:  D = DaoValue_GetFloat( A ) <= DaoValue_GetFloat( B ); break;
		case DVM_EQ:  D = DaoValue_GetFloat( A ) == DaoValue_GetFloat( B ); break;
		case DVM_NE:  D = DaoValue_GetFloat( A ) != DaoValue_GetFloat( B ); break;
		default: break;
		}
	}else if( A->type == DAO_COMPLEX && B->type == DAO_COMPLEX ){
		double AR = A->xComplex.value.real, AI = A->xComplex.value.imag;
		double BR = B->xComplex.value.real, BI = B->xComplex.value.imag;
		switch( vmc->code ){
		case DVM_EQ: D = (AR == BR) && (AI == BI); break;
		case DVM_NE: D = (AR != BR) || (AI != BI); break;
		default: goto InvalidOperation;
		}
	}else if( A->type == DAO_STRING && B->type == DAO_STRING ){
		switch( vmc->code ){
		case DVM_LT:  D = DString_CompareUTF8( A->xString.value, B->xString.value )<0; break;
		case DVM_LE:  D = DString_CompareUTF8( A->xString.value, B->xString.value )<=0; break;
		case DVM_EQ:  D = DString_Compare( A->xString.value, B->xString.value )==0; break;
		case DVM_NE:  D = DString_Compare( A->xString.value, B->xString.value )!=0; break;
		default: goto InvalidOperation;
		}
	}else if( A->type == DAO_ENUM && B->type == DAO_ENUM ){
		switch( vmc->code ){
		case DVM_AND: D = A->xEnum.value && B->xEnum.value; break;
		case DVM_OR:  D = A->xEnum.value || A->xEnum.value; break;
		case DVM_LT:  D = DaoValue_Compare( A, B ) < 0; break;
		case DVM_LE:  D = DaoValue_Compare( A, B ) <= 0; break;
		case DVM_EQ:  D = DaoValue_Compare( A, B ) == 0; break;
		case DVM_NE:  D = DaoValue_Compare( A, B ) != 0; break;
		default: break;
		}
	}else if( A->type == B->type && (A->type == DAO_TUPLE || A->type == DAO_LIST) ){
		switch( vmc->code ){
		case DVM_LT:  D = DaoValue_ComparePro( A, B, self ) < 0; break;
		case DVM_LE:  D = DaoValue_ComparePro( A, B, self ) <= 0; break;
		case DVM_EQ:  D = DaoValue_ComparePro( A, B, self ) == 0; break;
		case DVM_NE:  D = DaoValue_ComparePro( A, B, self ) != 0; break;
		default: goto InvalidOperation;
		}
	}else if( A->type == B->type && A->type == DAO_ARRAY ){
		D = DaoValue_Compare( A, B );
		switch( vmc->code ){
		case DVM_LT:
			if( abs( D ) > 1 ) goto InvalidOperation;
			D = D <  0;
			break;
		case DVM_LE:
			if( abs( D ) > 1 ) goto InvalidOperation;
			D = D <= 0;
			break;
		case DVM_EQ: D = D == 0; break;
		case DVM_NE: D = D != 0; break;
		default: break;
		}
	}else if( A->type == 0 || B->type == 0 ){
		switch( vmc->code ){
		case DVM_AND: D = B && A; break;
		case DVM_OR:  D = A || B; break;
		case DVM_LT:  D = A->type < B->type; break;
		case DVM_LE:  D = A->type <= B->type; break;
		case DVM_EQ:  D = A->type == B->type; break;
		case DVM_NE:  D = A->type != B->type; break;
		default: break;
		}
		if( A->type == DAO_CSTRUCT || B->type == DAO_CSTRUCT ){
			D = vmc->code == DVM_NE;
		}else if( A->type == DAO_CDATA || B->type == DAO_CDATA ){
			DaoCdata *cdata = (DaoCdata*)( A->type == DAO_CDATA ? & A->xCdata : & B->xCdata );
			if( vmc->code == DVM_EQ ){
				D = cdata->data ? 0 : 1;
			}else if( vmc->code == DVM_NE ){
				D = cdata->data ? 1 : 0;
			}
		}else if( A->type == DAO_OBJECT || B->type == DAO_OBJECT ){
			DaoObject *object = (DaoObject*)(A->type == DAO_OBJECT ? A : B);
			if( vmc->code == DVM_EQ ){
				D = object->isNull ? 1 : 0;
			}else if( vmc->code == DVM_NE ){
				D = object->isNull ? 0 : 1;
			}
		}
	}else if( A->type == DAO_OBJECT || A->type == DAO_CSTRUCT
			|| A->type == DAO_CDATA || A->type == DAO_CINVALUE ){
		DaoType *ta = self->activeTypes[ vmc->a ];
		DaoType *tb = self->activeTypes[ vmc->b ];
		rc = DaoProcess_TryUserArith( self, A, B, C, ta, tb );
		if( rc ) return;
		if( A->type == DAO_OBJECT || A->type == DAO_CSTRUCT ){
			switch( vmc->code ){
			case DVM_AND: D = A && B; break;
			case DVM_OR:  D = A || B; break;
			case DVM_LT:  D = A < B; break;
			case DVM_LE:  D = A <= B; break;
			case DVM_EQ:  D = A == B; break;
			case DVM_NE:  D = A != B; break;
			default: break;
			}
			DaoProcess_PutBoolean( self, D );
		}else{  /* A->type == DAO_CDATA */
			if( B->type != DAO_CDATA ){
				switch( vmc->code ){
				case DVM_AND: D = A && B; break;
				case DVM_OR : D = A || B; break;
				default : D = vmc->code == DVM_NE; break;
				}
			}else{
				switch( vmc->code ){
				case DVM_AND: D = A->xCdata.data && B->xCdata.data; break;
				case DVM_OR : D = A->xCdata.data || B->xCdata.data; break;
				case DVM_LT:  D = A->xCdata.data < B->xCdata.data; break;
				case DVM_LE:  D = A->xCdata.data <= B->xCdata.data; break;
				case DVM_EQ:  D = A->xCdata.data == B->xCdata.data; break;
				case DVM_NE:  D = A->xCdata.data != B->xCdata.data; break;
				default: break;
				}
			}
			DaoProcess_PutBoolean( self, D );
		}
	}else if( vmc->code == DVM_EQ ){
		D = A == B;
		if( A->type == DAO_TYPE && B->type == DAO_TYPE ){
			D = DaoType_MatchTo( (DaoType*) A, (DaoType*) B, NULL ) >= DAO_MT_EQ;
		}
	}else if( vmc->code == DVM_NE ){
		D = A != B;
		if( A->type == DAO_TYPE && B->type == DAO_TYPE ){
			D = DaoType_MatchTo( (DaoType*) A, (DaoType*) B, NULL ) < DAO_MT_EQ;
		}
	}else{
InvalidOperation:
		DaoProcess_RaiseError( self, "Type", "" );
		return;
	}
	DaoProcess_PutBoolean( self, D );
}
void DaoProcess_DoUnaArith( DaoProcess *self, DaoVmCode *vmc )
{
	DaoValue *A = self->activeValues[ vmc->a ];
	DaoValue *C = NULL;
	int ta = A->type;
	self->activeCode = vmc;
	if( A->type ==0 ){
		DaoProcess_RaiseError( self, "Type", "on none object" );
		return;
	}

	if( ta == DAO_INTEGER ){
		C = DaoProcess_SetValue( self, vmc->c, A );
		C->xInteger.value = - C->xInteger.value;
	}else if( ta == DAO_FLOAT ){
		C = DaoProcess_SetValue( self, vmc->c, A );
		C->xFloat.value = - C->xFloat.value;
	}else if( ta == DAO_COMPLEX ){
		C = DaoProcess_SetValue( self, vmc->c, A );
		C->xComplex.value.real = - C->xComplex.value.real;
		C->xComplex.value.imag = - C->xComplex.value.imag;
#ifdef DAO_WITH_NUMARRAY
	}else if( ta == DAO_ARRAY ){
		DaoArray *array = & A->xArray;
		daoint i, n;
		C = A;
		if( array->etype <= DAO_FLOAT ){
			DaoArray *res = DaoProcess_GetArray( self, vmc );
			DaoArray_SetNumType( res, array->etype );
			DaoArray_ResizeArray( res, array->dims, array->ndim );
			if( array->etype == DAO_INTEGER ){
				dao_integer *va = array->data.i;
				dao_integer *vc = res->data.i;
				for(i=0,n=array->size; i<n; i++ ) vc[i] = - va[i];
			}else{
				dao_float *va = array->data.f;
				dao_float *vc = res->data.f;
				for(i=0,n=array->size; i<n; i++ ) vc[i] = - va[i];
			}
		}else{
			DaoArray *res = DaoProcess_GetArray( self, vmc );
			dao_complex *va, *vc;
			DaoArray_SetNumType( res, array->etype );
			DaoArray_ResizeArray( res, array->dims, array->ndim );
			va = array->data.c;
			vc = res->data.c;
			for(i=0,n=array->size; i<n; i++ ){
				vc[i].real = - va[i].real;
				vc[i].imag = - va[i].imag;
			}
		}
#endif
	}else if( ta == DAO_OBJECT || ta == DAO_CDATA || ta == DAO_CSTRUCT || ta == DAO_CINVALUE ){
		DaoType *ta = self->activeTypes[ vmc->a ];
		DaoType *tb = self->activeTypes[ vmc->b ];
		C = self->activeValues[ vmc->c ];
		if( DaoProcess_TryUserArith( self, A, NULL, C, ta, tb ) == 0 ){
			DaoProcess_RaiseError( self, "Type", NULL );
		}
	}else{
		DaoProcess_RaiseError( self, "Type", NULL );
	}
}
void DaoProcess_DoUnaBool( DaoProcess *self, DaoVmCode *vmc )
{
	DaoValue *A = self->activeValues[ vmc->a ];
	int ta = A->type;
	self->activeCode = vmc;
	if( A->type ==0 ){
		DaoProcess_RaiseError( self, "Type", "on none object" );
		return;
	}

	if( ta == DAO_BOOLEAN || ta == DAO_INTEGER ){
		DaoProcess_PutBoolean( self, ! A->xInteger.value );
	}else if( ta == DAO_FLOAT ){
		DaoProcess_PutBoolean( self, ! A->xFloat.value );
#ifdef DAO_WITH_NUMARRAY
	}else if( ta == DAO_ENUM && A->xEnum.subtype != DAO_ENUM_SYM ){
		DaoProcess_PutFloat( self, ! A->xEnum.value );
#endif
	}else if( ta == DAO_OBJECT || ta == DAO_CDATA || ta == DAO_CSTRUCT || ta == DAO_CINVALUE ){
		DaoValue *C = self->activeValues[ vmc->c ];
		DaoType *ta = self->activeTypes[ vmc->a ];
		DaoType *tb = self->activeTypes[ vmc->b ];
		if( DaoProcess_TryUserArith( self, A, NULL, C, ta, tb ) == 0 ){
			DaoProcess_RaiseError( self, "Type", NULL );
		}
	}else{
		DaoProcess_RaiseError( self, "Type", NULL );
	}
}
void DaoProcess_DoInTest( DaoProcess *self, DaoVmCode *vmc )
{
	DaoValue *A = self->activeValues[ vmc->a ];
	DaoValue *B = self->activeValues[ vmc->b ];
	dao_integer *C = DaoProcess_PutBoolean( self, 0 );
	daoint i, n;
	if( A->type == DAO_INTEGER && B->type == DAO_STRING ){
		dao_integer bv = A->xInteger.value;
		daoint size = B->xString.value->size;
		char *mbs = B->xString.value->chars;
		for(i=0; i<size; i++){
			if( mbs[i] == bv ){
				*C = 1;
				break;
			}
		}
	}else if( A->type == DAO_STRING && B->type == DAO_STRING ){
		*C = DString_Find( B->xString.value, A->xString.value, 0 ) != DAO_NULLPOS;
	}else if( A->type == DAO_ENUM && B->type == DAO_ENUM ){
		DaoType *ta = A->xEnum.etype;
		DaoType *tb = B->xEnum.etype;
		if( ta == tb ){
			*C = A->xEnum.value == (A->xEnum.value & B->xEnum.value);
		}else{
			DMap *ma = ta->mapNames;
			DMap *mb = tb->mapNames;
			DNode *it, *node;
			*C = 1;
			for(it=DMap_First(ma); it; it=DMap_Next(ma,it) ){
				if( A->xEnum.subtype == DAO_ENUM_FLAG ){
					if( (it->value.pInt & A->xEnum.value) != it->value.pInt ) continue;
				}else if( it->value.pInt != A->xEnum.value ){
					continue;
				}
				if( (node = DMap_Find( mb, it->key.pVoid )) == NULL ){
					*C = 0;
					break;
				}
				if( (node->value.pInt & B->xEnum.value) != node->value.pInt ){
					*C = 0;
					break;
				}
			}
		}
	}else if( B->type == DAO_LIST ){
		DList *items = B->xList.value;
		DaoType *ta = DaoNamespace_GetType( self->activeNamespace, A );
		if( ta && B->xList.ctype && B->xList.ctype->nested->size ){
			DaoType *tb = B->xList.ctype->nested->items.pType[0];
			if( tb && DaoType_MatchTo( ta, tb, NULL ) == 0 ) return;
		}
		for(i=0,n=items->size; i<n; i++){
			*C = DaoValue_ComparePro( A, items->items.pValue[i], self ) ==0;
			if( *C ) break;
		}
	}else if( B->type == DAO_MAP ){
		DaoType *ta = DaoNamespace_GetType( self->activeNamespace, A );
		if( ta && B->xMap.ctype && B->xMap.ctype->nested->size ){
			DaoType *tb = B->xMap.ctype->nested->items.pType[0];
			if( tb && DaoType_MatchTo( ta, tb, NULL ) == 0 ) return;
		}
		*C = DaoMap_Find2( (DaoMap*) B, A, self ) != NULL;
	}else if( B->type == DAO_TUPLE && B->xTuple.subtype == DAO_PAIR ){
		int c1 = DaoValue_ComparePro( B->xTuple.values[0], A, self );
		int c2 = DaoValue_ComparePro( A, B->xTuple.values[1], self );
		*C = c1 <=0 && c2 <= 0;
	}else if( B->type == DAO_TUPLE ){
		for(i=0; i<B->xTuple.size; ++i){
			if( DaoValue_ComparePro( A, B->xTuple.values[i], self ) == 0 ){
				*C = 1;
				break;
			}
		}
	}else{
		DaoProcess_RaiseError( self, "Type", NULL );
	}
}
void DaoProcess_DoBitLogic( DaoProcess *self, DaoVmCode *vmc )
{
	DaoValue *A = self->activeValues[ vmc->a ];
	DaoValue *B = self->activeValues[ vmc->b ];
	dao_integer inum = 0;

	self->activeCode = vmc;
	if( A->type && B->type && A->type <= DAO_FLOAT && B->type <= DAO_FLOAT ){
		switch( vmc->code ){
		case DVM_BITAND: inum = DaoValue_GetInteger(A) & DaoValue_GetInteger(B);break;
		case DVM_BITOR : inum = DaoValue_GetInteger(A) | DaoValue_GetInteger(B);break;
		case DVM_BITXOR: inum = DaoValue_GetInteger(A) ^ DaoValue_GetInteger(B);break;
		default : break;
		}
		if( A->type == DAO_FLOAT || B->type == DAO_FLOAT ){
			DaoProcess_PutFloat( self, inum );
		}else{
			DaoProcess_PutInteger( self, inum );
		}
	}else if( A->type == DAO_ENUM && B->type == DAO_ENUM ){
		DaoEnum *en = DaoProcess_GetEnum( self, vmc );
		if( A->xEnum.etype != B->xEnum.etype ) goto InvalidOperation;
		if( A->xEnum.subtype <= DAO_ENUM_STATE ) goto InvalidOperation;
		if( en == NULL || en->etype != A->xEnum.etype ) goto InvalidOperation;
		switch( vmc->code ){
		case DVM_BITAND : en->value = A->xEnum.value & B->xEnum.value; break;
		case DVM_BITOR  : en->value = A->xEnum.value | B->xEnum.value; break;
		default : goto InvalidOperation;
		}
	}else if( A->type == DAO_OBJECT || A->type == DAO_CDATA
			|| A->type == DAO_CSTRUCT || A->type == DAO_CINVALUE ){
		DaoValue *C = self->activeValues[ vmc->c ];
		DaoType *ta = self->activeTypes[ vmc->a ];
		DaoType *tb = self->activeTypes[ vmc->b ];
		if( DaoProcess_TryUserArith( self, A, B, C, ta, tb ) == 0 ){
			DaoProcess_RaiseError( self, "Type", NULL );
		}
	}else{
InvalidOperation:
		DaoProcess_RaiseError( self, "Value", "invalid operands" );
	}
}
void DaoProcess_DoBitShift( DaoProcess *self, DaoVmCode *vmc )
{
	DaoValue *A = self->activeValues[ vmc->a ];
	DaoValue *B = self->activeValues[ vmc->b ];
	if( A->type && B->type && A->type <= DAO_FLOAT && B->type <= DAO_FLOAT ){
		dao_integer inum = 0;
		if( vmc->code == DVM_BITLFT ){
			inum = DaoValue_GetInteger(A) << DaoValue_GetInteger(B);
		}else{
			inum = DaoValue_GetInteger(A) >> DaoValue_GetInteger(B);
		}
		if( A->type == DAO_FLOAT || B->type == DAO_FLOAT ){
			DaoProcess_PutFloat( self, inum );
		}else if( A->type == DAO_FLOAT || B->type == DAO_FLOAT ){
			DaoProcess_PutFloat( self, inum );
		}else{
			DaoProcess_PutInteger( self, inum );
		}
	}else if( A->type == DAO_OBJECT || A->type == DAO_CDATA
			|| A->type == DAO_CSTRUCT || A->type == DAO_CINVALUE ){
		DaoValue *C = self->activeValues[ vmc->c ];
		DaoType *ta = self->activeTypes[ vmc->a ];
		DaoType *tb = self->activeTypes[ vmc->b ];
		self->activeCode = vmc;
		if( DaoProcess_TryUserArith( self, A, B, C, ta, tb ) == 0 ){
			DaoProcess_RaiseError( self, "Type", NULL );
		}
	}else{
		self->activeCode = vmc;
		DaoProcess_RaiseError( self, "Value", "invalid operands" );
	}
}
void DaoProcess_DoBitFlip( DaoProcess *self, DaoVmCode *vmc )
{
	DaoValue *A = self->activeValues[ vmc->a ];
	self->activeCode = vmc;
	if( A->type >= DAO_INTEGER && A->type <= DAO_FLOAT ){
		switch( A->type ){
		case DAO_INTEGER : DaoProcess_PutInteger( self, ~A->xInteger.value ); break;
		case DAO_FLOAT   : DaoProcess_PutFloat( self, ~(dao_integer)A->xFloat.value ); break;
		}
	}else if( A->type == DAO_COMPLEX ){
		dao_complex *C = DaoProcess_PutComplex( self, A->xComplex.value );
		C->imag = - C->imag;
	}else if( A->type == DAO_ENUM ){
		DaoType *etype = A->xEnum.etype;
		DaoValue *C = DaoProcess_PutValue( self, A );
		DNode *it = DMap_First(etype->mapNames);
		int min = 0, max = 0, value = 0;
		if( it ) min = max = it->value.pInt;
		for(; it; it=DMap_Next(etype->mapNames,it)){
			if( it->value.pInt < min ) min = it->value.pInt;
			if( it->value.pInt > max ) max = it->value.pInt;
			value |= it->value.pInt;
		}
		if( A->xEnum.subtype == DAO_ENUM_FLAG ){
			C->xEnum.value = value & (~A->xEnum.value);
		}else if( A->xEnum.value == min ){
			C->xEnum.value = max;
		}else{
			C->xEnum.value = min;
		}
	}else if( A->type == DAO_OBJECT || A->type == DAO_CDATA
			|| A->type == DAO_CSTRUCT || A->type == DAO_CINVALUE ){
		DaoValue *C = self->activeValues[ vmc->c ];
		DaoType *ta = self->activeTypes[ vmc->a ];
		DaoType *tb = self->activeTypes[ vmc->b ];
		if( DaoProcess_TryUserArith( self, A, NULL, C, ta, tb ) == 0 ){
			DaoProcess_RaiseError( self, "Type", NULL );
		}
	}else{
		DaoProcess_RaiseError( self, "Value", "invalid operands" );
	}
}
/* Set dC->type before calling to instruct this function what type number to convert: */
int ConvertStringToNumber( DaoProcess *proc, DaoValue *dA, DaoValue *dC )
{
	DaoLexer *lexer;
	DaoParser *parser;
	DaoToken *tok, **tokens;
	DString *mbs = proc->string;
	double fvalue = 0;
	dao_integer ivalue = 0;
	int tid = dC->type;
	int base, tokid = 0;
	int ec, sign = 1;

	if( dA->type != DAO_STRING || tid == DAO_NONE || tid > DAO_COMPLEX ) return 0;

	DString_SetBytes( mbs, dA->xString.value->chars, dA->xString.value->size );
	DString_Trim( mbs, 1, 1, 0 );
	if( mbs->size ==0 ) return 0;

	parser = DaoVmSpace_AcquireParser( proc->vmSpace );
	lexer = parser->lexer;

	DaoLexer_Tokenize( lexer, mbs->chars, DAO_LEX_COMMENT|DAO_LEX_SPACE );
	tokens = lexer->tokens->items.pToken;

	if( lexer->tokens->size == 0 ) goto ReturnFalse;
	switch( tokens[tokid]->name ){
	case DTOK_ADD : tokid += 1; break;
	case DTOK_SUB : tokid += 1; sign = -1; break;
	}
	if( tokid >= lexer->tokens->size ) goto ReturnFalse;
	tok = tokens[tokid++];

	switch( tid ){
	case DAO_BOOLEAN :
		dC->xBoolean.value = strcmp( tok->string.chars, "true" ) == 0;
		break;
	case DAO_INTEGER :
		if( tok->name < DTOK_DIGITS_DEC || tok->name > DTOK_NUMBER_DEC ) goto ReturnFalse;
		dC->xInteger.value = DaoToken_ToInteger( tok );
		if( sign <0 ) dC->xInteger.value = - dC->xInteger.value;
		break;
	case DAO_FLOAT :
		if( tok->name < DTOK_DIGITS_DEC || tok->name > DTOK_NUMBER_SCI ) goto ReturnFalse;
		dC->xFloat.value = DaoToken_ToFloat( tok );
		if( sign <0 ) dC->xFloat.value = - dC->xFloat.value;
		break;
	case DAO_COMPLEX :
		dC->xComplex.value.real = 0.0;
		dC->xComplex.value.imag = 0.0;
		if( tok->name >= DTOK_DIGITS_DEC && tok->name <= DTOK_NUMBER_SCI ){
			dC->xComplex.value.real = DaoToken_ToFloat( tok );
			if( sign <0 ) dC->xComplex.value.real = - dC->xComplex.value.real;

			if( tokid >= lexer->tokens->size ) goto ReturnTrue;
			tok = tokens[tokid];
			switch( tok->name ){
			case DTOK_ADD : tokid += 1; break;
			case DTOK_SUB : tokid += 1; sign = -1; break;
			default : goto ReturnFalse;
			}
			if( tokid >= lexer->tokens->size ) goto ReturnFalse;
			tok = tokens[tokid++];
		}
		if( tok->name != DTOK_NUMBER_IMG ) goto ReturnFalse;
		dC->xComplex.value.imag = DaoToken_ToFloat( tok );
		if( sign <0 ) dC->xComplex.value.imag = - dC->xComplex.value.imag;
		break;
	}
	if( tokid < lexer->tokens->size ) goto ReturnFalse;

ReturnTrue:
	DaoVmSpace_ReleaseParser( proc->vmSpace, parser );
	return 1;
ReturnFalse:
	DaoVmSpace_ReleaseParser( proc->vmSpace, parser );
	return 0;
}
#ifdef DAO_WITH_NUMARRAY
static DaoArray* DaoProcess_PrepareArray( DaoProcess *self, DaoValue *dC, int etype )
{
	DaoArray *array = NULL;
	if( dC && dC->type == DAO_ARRAY && dC->xArray.refCount == 1 && array->original == NULL ){
		array = (DaoArray*) dC;
		DaoArray_SetNumType( array, etype );
	}else{
		array = DaoProcess_NewArray( self, etype );
	}
	return array;
}
#endif
static DaoTuple* DaoProcess_PrepareTuple( DaoProcess *self, DaoValue *dC, DaoType *ct, int size )
{
	DaoTuple *tuple = NULL;

	if( size < (ct->nested->size - ct->variadic) ) return NULL;
	if( ct->variadic == 0 ) size = ct->nested->size;

	if( dC && dC->type == DAO_TUPLE && dC->xTuple.ctype == ct ){
		if( dC->xTuple.size == size && dC->xTuple.refCount == 1 ) return (DaoTuple*) dC;
	}
	tuple = DaoProcess_NewTuple( self, size );
	tuple->ctype = ct;
	GC_IncRC( ct );
	return tuple;
}
DaoValue* DaoTypeCast( DaoProcess *proc, DaoType *ct, DaoValue *dA, DaoValue *dC, int invarToVar )
{
	DaoNamespace *ns = proc->activeNamespace;
	DaoTuple *tuple = NULL, *tuple2 = NULL;
	DaoList *list = NULL, *list2 = NULL;
	DaoMap *map = NULL, *map2 = NULL;
	DaoType *tp = NULL, *tp2 = NULL;
	DaoArray *array = NULL, *array2 = NULL;
	DaoValue **data, **data2, *K = NULL, *V = NULL;
	DaoValue *itvalue;
	DString *str;
	DNode *node;
	daoint i, n, size;
	int mt, type, variadic, tsize;

	if( ct == NULL ) goto FailConversion;
	if( ct->tid & DAO_ANY ) ct = DaoNamespace_GetType( proc->activeNamespace, dA );
	if( dA->type == ct->tid && ct->tid >= DAO_NONE && ct->tid < DAO_ARRAY ) goto Rebind;
	if( ct->tid > DAO_NONE && ct->tid <= DAO_STRING && (dC == NULL || dC->type != ct->tid) ){
		dC = DaoValue_SimpleCopy( ct->value );
		DaoProcess_CacheValue( proc, dC );
	}
	if( dA->type == DAO_STRING && ct->tid > DAO_NONE && ct->tid <= DAO_COMPLEX ){
		if( ConvertStringToNumber( proc, dA, dC ) ==0 ) goto FailConversion;
		return dC;
	}
	if( ct->valtype ){
		if( DaoValue_Compare( ct->aux, dA ) != 0 ) goto FailConversion;
		return dA;
	}
	switch( ct->tid ){
	case DAO_BOOLEAN :
		if( dA->type == DAO_STRING ){
			dC->xBoolean.value = strcmp( dA->xString.value->chars, "true" ) == 0;
			break;
		}
		dC->xBoolean.value = DaoValue_IsZero( dA ) != 0;
		break;
	case DAO_INTEGER :
		dC->xInteger.value = DaoValue_GetInteger( dA );
		break;
	case DAO_FLOAT :
		dC->xFloat.value = DaoValue_GetFloat( dA );
		break;
	case DAO_COMPLEX :
		if( dA->type == DAO_COMPLEX ) goto Rebind;
		if( dA->type >= DAO_ARRAY ) goto FailConversion;
		dC->xComplex.value = DaoValue_GetComplex( dA );
		break;
	case DAO_STRING :
		if( dA->type == DAO_STRING ) goto Rebind;
		if( dA->type >= DAO_ARRAY ) goto FailConversion;
		DaoValue_GetString( dA, dC->xString.value );
		break;
#ifdef DAO_WITH_NUMARRAY
	case DAO_ARRAY :
		array2 = & dA->xArray;
		if( ct->nested->size >0 ) tp = ct->nested->items.pType[0];
		if( dA->type != DAO_ARRAY ) goto FailConversion;
		if( invarToVar == 0 ){
			if( tp == NULL ) goto Rebind;
			if( array2->etype == tp->tid ) goto Rebind;
		}
		if( tp->tid < DAO_BOOLEAN || tp->tid > DAO_COMPLEX ) goto FailConversion;
		if( array2->original && DaoArray_Sliced( array2 ) == 0 ) goto FailConversion;

		array = DaoProcess_PrepareArray( proc, dC, tp->tid );
		DaoArray_ResizeArray( array, array2->dims, array2->ndim );
		for(i=0,size=array2->size; i<size; i++){
			switch( array->etype ){
			case DAO_BOOLEAN : array->data.b[i] = DaoArray_GetBoolean( array2, i ); break;
			case DAO_INTEGER : array->data.i[i] = DaoArray_GetInteger( array2, i ); break;
			case DAO_FLOAT   : array->data.f[i] = DaoArray_GetFloat( array2, i );  break;
			case DAO_COMPLEX : array->data.c[i] = DaoArray_GetComplex( array2, i ); break;
			}
		}
		dC = (DaoValue*) array;
		break;
#endif
	case DAO_LIST :
		if( invarToVar == 0 ){
			if( DaoType_MatchValue( ct, dA, NULL ) >= DAO_MT_EQ ) goto Rebind;
		}
		if( ct->nested->size >0 ) tp = ct->nested->items.pType[0];

		if( tp == NULL ) goto FailConversion;
		if( dC && dC->type == DAO_LIST && dC->xList.refCount == 1 && dC->xList.ctype == ct ){
			list = (DaoList*) dC;
		}else{
			list = DaoProcess_NewList( proc );
			list->ctype = ct;
			GC_IncRC( ct );
			dC = (DaoValue*) list;
		}
		if( dA->type == DAO_LIST ){
			list2 = & dA->xList;
			DList_Resize( list->value, list2->value->size, NULL );
			data = list->value->items.pValue;
			data2 = list2->value->items.pValue;
			for(i=0,n=list2->value->size; i<n; i++ ){
				V = DaoTypeCast( proc, tp, data2[i], V, invarToVar );
				if( V == NULL ) goto FailConversion;
				DaoValue_Copy( V, data + i );
			}
		}else if( dA->type == DAO_TUPLE ){
			tuple2 = (DaoTuple*) dA;
			DList_Resize( list->value, tuple2->size, NULL );
			data = list->value->items.pValue;
			data2 = tuple2->values;
			for(i=0,n=tuple2->size; i<n; i++ ){
				V = DaoTypeCast( proc, tp, data2[i], V, invarToVar  );
				if( V == NULL ) goto FailConversion;
				DaoValue_Copy( V, data + i );
			}
		}else goto FailConversion;
		break;
	case DAO_MAP :
		if( dA->type != DAO_MAP ) goto FailConversion;
		map2 = & dA->xMap;
		if( invarToVar == 0 ){
			if( DaoType_MatchTo( map2->ctype, ct, NULL ) >= DAO_MT_EQ ) goto Rebind;
		}

		if( dC && dC->type == DAO_MAP && dC->xMap.refCount == 1 && dC->xMap.ctype == ct ){
			map = (DaoMap*) dC;
			DMap_Reset( map->value );
		}else{
			map = DaoProcess_NewMap( proc, map2->value->hashing );
			map->ctype = ct;
			GC_IncRC( ct );
			dC = (DaoValue*) map;
		}
		if( ct->nested->size >0 ) tp = ct->nested->items.pType[0];
		if( ct->nested->size >1 ) tp2 = ct->nested->items.pType[1];
		if( tp == NULL || tp2 == NULL ) goto FailConversion;
		node = DMap_First( map2->value );
		for(; node!=NULL; node=DMap_Next(map2->value,node) ){
			K = DaoTypeCast( proc, tp, node->key.pValue, K, invarToVar  );
			V = DaoTypeCast( proc, tp2, node->value.pValue, V, invarToVar  );
			if( K == NULL || V == NULL ) goto FailConversion;
			DMap_Insert( map->value, K, V );
		}
		break;
	case DAO_TUPLE :
		tsize = ct->nested->size - ct->variadic;
		if( dA->type == DAO_TUPLE ){
			tuple2 = (DaoTuple*) dA;
			if( invarToVar == 0 && (tuple2->ctype == ct || ct->nested->size == 0) ) goto Rebind;
			tuple = DaoProcess_PrepareTuple( proc, dC, ct, tuple2->size );
			if( tuple == NULL ) goto FailConversion;
			for(i=0; i<tuple->size; i++){
				DaoValue *V = tuple2->values[i];
				tp2 = dao_type_any;
				if( i < ct->nested->size ){
					tp2 = ct->nested->items.pType[i];
				}else if( ct->variadic ){
					tp2 = ct->nested->items.pType[tsize];
				}
				if( tp2->tid >= DAO_PAR_NAMED && tp2->tid <= DAO_PAR_VALIST ) tp2 = & tp2->aux->xType;
				V = DaoTypeCast( proc, tp2, V, K, invarToVar  );
				if( V == NULL ) goto FailConversion;
				DaoValue_Copy( V, tuple->values + i );
			}
		}else if( dA->type == DAO_LIST ){
			list = (DaoList*) dA;
			tuple = DaoProcess_PrepareTuple( proc, dC, ct, list->value->size );
			if( tuple == NULL ) goto FailConversion;
			for(i=0; i<tuple->size; i++){
				DaoValue *V = list->value->items.pValue[i];
				tp2 = dao_type_any;
				if( i < ct->nested->size ){
					tp2 = ct->nested->items.pType[i];
				}else if( ct->variadic ){
					tp2 = ct->nested->items.pType[tsize];
				}
				if( tp2->tid >= DAO_PAR_NAMED && tp2->tid <= DAO_PAR_VALIST ) tp2 = & tp2->aux->xType;
				V = DaoTypeCast( proc, tp2, V, K, invarToVar  );
				if( V == NULL ) goto FailConversion;
				DaoValue_Copy( V, tuple->values + i );
			}
		}else{
			goto FailConversion;
		}
		dC = (DaoValue*) tuple;
		break;
	case DAO_CLASS :
		if( dA == NULL || dA->type != DAO_CLASS ) goto FailConversion;
		if( invarToVar ) goto FailConversion;
		if( ct->aux == NULL ) goto Rebind; /* to "class"; */
		dC = DaoClass_CastToBase( (DaoClass*)dA, ct );
		if( dC == NULL ) goto FailConversion;
		break;
	case DAO_OBJECT :
		if( dA->type == DAO_CDATA || dA->type == DAO_CSTRUCT ) dA = (DaoValue*) dA->xCdata.object;
		/* XXX compiling time checking??? */
		if( dA == NULL || dA->type != DAO_OBJECT ) goto FailConversion;
		if( invarToVar && !(dA->xObject.defClass->attribs & DAO_CLS_INVAR) ) goto FailConversion;
		dC = DaoObject_CastToBase( & dA->xObject, ct );
		if( dC == NULL ) goto FailConversion;
		break;
	case DAO_CTYPE :
		if( invarToVar ) goto FailConversion;
		if( dA->type == DAO_CLASS ){
			dC = DaoClass_CastToBase( (DaoClass*)dA, ct );
		}else if( dA->type == DAO_CTYPE ){
			if( DaoType_ChildOf( dA->xCtype.ctype, ct ) ) dC = dA;
		}
		if( dC == NULL ) goto FailConversion;
		break;
	case DAO_CDATA :
	case DAO_CSTRUCT :
		dC = NULL;
		if( dA->type == DAO_CDATA || dA->type == DAO_CSTRUCT ){
			if( invarToVar && !(dA->xCtype.attribs & DAO_CLS_INVAR) ) goto FailConversion;
			if( DaoType_ChildOf( dA->xCdata.ctype, ct ) ) dC = dA;
		}else if( dA->type == DAO_OBJECT ){
			if( invarToVar && !(dA->xObject.defClass->attribs & DAO_CLS_INVAR) ) goto FailConversion;
			dC = DaoObject_CastToBase( & dA->xObject, ct );
		}
		if( dC == NULL ) goto FailConversion;
		break;
	case DAO_VARIANT :
		tp = NULL;
		mt = DAO_MT_NOT;
		for(i=0,n=ct->nested->size; i<n; i++){
			DaoType *itp = ct->nested->items.pType[i];
			int mt2 = DaoType_MatchValue( itp, dA, NULL );
			if( mt2 > mt ){
				mt = mt2;
				tp = itp;
			}
		}
		if( tp == NULL ) goto FailConversion;
		return DaoTypeCast( proc, tp, dA, dC, invarToVar );
	case DAO_ROUTINE :
		if( DaoType_MatchValue( ct, dA, NULL ) == 0 ) goto FailConversion;
		dC = dA;
		break;
	default :
		if( DaoType_MatchValue( ct, dA, NULL ) < DAO_MT_EQ ) goto FailConversion;
		dC = dA;
	}
	return dC;
Rebind :
	return dA;
FailConversion :
	return NULL;
}

DaoRoutine* DaoRoutine_Check( DaoRoutine *self, DaoType *selftp, DaoType *ts[], int np, int code, DList *es );
void DaoPrintCallError( DList *errors, DaoStream *stdio );

void DaoProcess_ShowCallError( DaoProcess *self, DaoRoutine *rout, DaoValue *selfobj, DaoValue *ps[], int np, int callmode )
{
	DaoStream *ss = DaoStream_New();
	DaoNamespace *ns = self->activeNamespace;
	DaoType *selftype = selfobj ? DaoNamespace_GetType( ns, selfobj ) : NULL;
	DaoType *ts[DAO_MAX_PARAM];
	DList *errors = DList_New(0);
	int i;
	for(i=0; i<np; i++) ts[i] = DaoNamespace_GetType( ns, ps[i] );
	DaoRoutine_Check( rout, selftype, ts, np, callmode, errors );
	ss->mode |= DAO_STREAM_STRING;
	DaoPrintCallError( errors, ss );
	DList_Delete( errors );
	DaoProcess_RaiseError( self, "Param", ss->streamString->chars );
	DaoStream_Delete( ss );
}

int DaoRoutine_SetVmCodes2( DaoRoutine *self, DArray *vmCodes );

static void DaoProcess_MapTypes( DaoProcess *self, DMap *deftypes )
{
	DaoRoutine *routine = self->activeRoutine;
	DNode *it = DMap_First(routine->body->localVarType);
	for(; it; it = DMap_Next(routine->body->localVarType,it) ){
		DaoValue *V = self->activeValues[ it->key.pInt ];
		if( V == NULL || V->type != DAO_TYPE || it->value.pType->tid != DAO_TYPE ) continue;
		MAP_Insert( deftypes, it->value.pType->nested->items.pType[0], V );
	}
}
static int DaoNamespace_CopyStaticVar( DaoNamespace *self, int id, DMap *map )
{
	DNode *it = MAP_Find( map, id );
	if( it == NULL ){
		DaoVariable *var = self->variables->items.pVar[id];
		var = DaoVariable_New( var->value, var->dtype, DAO_STATIC_VARIABLE );
		DList_Append( self->variables, var );
		it = MAP_Insert( map, id, self->variables->size-1 );
	}
	return it->value.pInt;
}
void DaoProcess_MakeRoutine( DaoProcess *self, DaoVmCode *vmc )
{
	DaoType *tp;
	DaoValue **pp2, **pp = self->activeValues + vmc->a + 1;
	DaoRoutine *closure, *proto = (DaoRoutine*) self->activeValues[vmc->a];
	DaoNamespace *NS = proto->nameSpace;
	DMap *deftypes;
	int i, j, k, m, K;
	if( proto->body->vmCodes->size ==0 && proto->body->annotCodes->size ){
		if( DaoRoutine_SetVmCodes( proto, proto->body->annotCodes ) ==0 ){
			DaoProcess_RaiseError( self, NULL, "invalid closure" );
			return;
		}
	}
	if( vmc->b == 0 && proto->body->upValues == NULL && proto->body->hasStatic == 0 ){
		DaoProcess_SetValue( self, vmc->c, (DaoValue*) proto );
		if( proto->attribs & DAO_ROUT_DEFER ) DList_Append( self->defers, proto );
		return;
	}

	closure = DaoRoutine_Copy( proto, 1, 1, 1 );
	pp2 = closure->routConsts->value->items.pValue;

	for(j=0; j<vmc->b; j+=2){
		k = pp[j+1]->xInteger.value;
		if( k < DAO_MAX_PARAM ){
			DaoValue_Copy( pp[j], pp2 + k );
		}else{
			DaoVariable_Set( closure->body->upValues->items.pVar[k-DAO_MAX_PARAM], pp[j], NULL );
		}
	}

	deftypes = DMap_New(0,0);
	DaoProcess_MapTypes( self, deftypes );
	tp = DaoNamespace_MakeRoutType( self->activeNamespace, closure->routType, pp2, NULL, NULL);
	tp = DaoType_DefineTypes( tp, closure->nameSpace, deftypes );
	GC_Assign( & closure->routType, tp );
	DaoRoutine_MapTypes( closure, proto, deftypes );
	DMap_Delete( deftypes );

	/* It's necessary to put it in "self" process in any case, so that it can be GC'ed: */
	DaoProcess_SetValue( self, vmc->c, (DaoValue*) closure );
	DList_Assign( closure->body->annotCodes, proto->body->annotCodes );
	if( DaoRoutine_SetVmCodes2( closure, proto->body->vmCodes ) ==0 ){
		DaoProcess_RaiseError( self, NULL, "function creation failed" );
	}
	if( proto->body->hasStatic ){
		DMap *updated = DMap_New(0,0);
		DNode *it;
		for(i=0; i<closure->body->vmCodes->size; ++i){
			DaoVmCodeX *vmcx = closure->body->annotCodes->items.pVmc[i];
			DaoVmCode *vmc = closure->body->vmCodes->data.codes + i;
			switch( vmc->code ){
			case DVM_GETVG :
			case DVM_GETVG_I : case DVM_GETVG_F : case DVM_GETVG_C :
				if( vmc->a == 0 ) break;
				vmc->b = vmcx->b = DaoNamespace_CopyStaticVar( NS, vmc->b, updated );
				break;
			case DVM_SETVG :
			case DVM_SETVG_II : case DVM_SETVG_FF : case DVM_SETVG_CC :
				if( (vmc->c & 1) == 0 ) break;
				vmc->b = vmcx->b = DaoNamespace_CopyStaticVar( NS, vmc->b, updated );
				break;
			}
		}
		DMap_Delete( updated );
	}
	if( proto->attribs & DAO_ROUT_DEFER ) DList_Append( self->defers, closure );
#if 0
	DaoRoutine_PrintCode( proto, self->vmSpace->stdioStream );
	DaoRoutine_PrintCode( closure, self->vmSpace->stdioStream );
	printf( "%s\n", closure->routType->name->chars );
#endif
}




void DaoSTD_Debug( DaoProcess *proc, DaoValue *p[], int N );

static DaoException* DaoProcess_RaiseExceptionEx( DaoProcess *self, DaoType *etype, const char *info )
{
	DaoType *warning = dao_type_warning;
	DaoStream *stream = self->vmSpace->errorStream;
	DaoException *except;

	if( self->activeRoutine == NULL ){
		DaoStream_WriteChars( stream, "ERROR: no active routine to raise an exception!\n" );
		return NULL;
	}

	if( DaoType_ChildOf( etype, warning ) ){
		/* XXX support warning suppression */
		except = DaoException_New( etype );
		DaoException_Init( except, self, info, NULL );
		DaoException_Print( except, stream );
		DaoException_Delete( except );
		return except;
	}
	except = DaoException_New( etype );
	DaoException_Init( except, self, info, NULL );
	DList_Append( self->exceptions, (DaoValue*) except );
	if( (self->vmSpace->options & DAO_OPTION_DEBUG) ){
		if( self->vmSpace->stopit ==0 ){
			DaoProcess_Trace( self, 10 );
			DaoProcess_PrintException( self, NULL, 0 );
			DaoSTD_Debug( self, NULL, 0 );
		}
	}
	return except;
}
void DaoProcess_RaiseException( DaoProcess *self, const char *type, const char *info, DaoValue *data )
{
	DaoException *exception;
	DaoType *etype = DaoVmSpace_MakeExceptionType( self->vmSpace, type );
	if( etype == NULL ){
		DaoProcess_RaiseError( self, "Param", "invalid exception type name" );
		return;
	}
	exception = DaoProcess_RaiseExceptionEx( self, etype, info );
	if( data != NULL && exception != NULL ) DaoValue_Copy( data, & exception->data );
}
void DaoProcess_RaiseException2( DaoProcess *self, const char *type, const char *info, char *args )
{
	DString info2 = DString_WrapChars( info );
	DString args2 = DString_WrapChars( args );
	DString *msg = DString_New();
	daoint i = 0, j = 0;
	while( i < info2.size && j < args2.size ){
		daoint i2 = DString_FindChars( & info2, "%s", i );
		daoint j2 = DString_FindChars( & args2, "\n", j );
		if( i2 == DAO_NULLPOS ) i2 = info2.size;
		if( j2 == DAO_NULLPOS ) j2 = args2.size;
		DString_AppendBytes( msg, info + i, i2 - i );
		if( i2 < info2.size ) DString_AppendBytes( msg, args + j, j2 - j );
		i = i2 + 2;
		j = j2 + 1;
	}
	if( i < info2.size ) DString_AppendBytes( msg, info + i, info2.size - i );
	DaoProcess_RaiseException( self, type, msg->chars, NULL );
	DString_Delete( msg );
}
static void DaoProcess_RaiseEx( DaoProcess *self, const char *type, const char *info, int err )
{
	int ecount = self->exceptions->size;
	DString *name = DString_New();
	DString_SetChars( name, err ? "Error" : "Warning" );
	if( type != NULL && strlen( type ) != 0 ){
		DString_AppendChars( name, "::" );
		DString_AppendChars( name, type );
	}
	DaoProcess_RaiseException( self, name->chars, info, NULL );
	DString_Delete( name );
}
void DaoProcess_RaiseWarning( DaoProcess *self, const char *type, const char *info )
{
	DaoProcess_RaiseEx( self, type, info, 0 );
}
void DaoProcess_RaiseError( DaoProcess *self, const char *type, const char *info )
{
	DaoProcess_RaiseEx( self, type, info, 1 );
}
void DaoProcess_RaiseTypeError( DaoProcess *self, DaoType *from, DaoType *to, const char *op )
{
	DString *details = DString_New();
	if( from == NULL ) from = dao_type_udf;
	if( to == NULL ) to = dao_type_udf;
	DString_AppendChars( details, op );
	DString_AppendChars( details, " from \'" );
	DString_Append( details,  from->name );
	DString_AppendChars( details, "\' to \'" );
	DString_Append( details,  to->name );
	DString_AppendChars( details, "\'." );
	DaoProcess_RaiseError( self, "Type", details->chars );
	DString_Delete( details );
}

void DaoProcess_PrintException( DaoProcess *self, DaoStream *stream, int clear )
{
	DaoType *extype = dao_type_exception;
	DaoValue **excobjs = self->exceptions->items.pValue;
	int i, n;

	if( stream == NULL ) stream = self->vmSpace->errorStream;
	for(i=0,n=self->exceptions->size; i<n; i++){
		DaoException *except = NULL;
		if( excobjs[i]->type == DAO_CSTRUCT ){
			except = (DaoException*) excobjs[i];
		}else if( excobjs[i]->type == DAO_OBJECT ){
			except = (DaoException*)DaoObject_CastToBase( & excobjs[i]->xObject, extype );
		}
		if( except == NULL ) continue;
		DaoException_Print( except, stream );
	}
	if( clear ) DList_Clear( self->exceptions );
}


static void DaoProcess_DoGetConstField( DaoProcess *self, DaoVmCode *vmc, int mode )
{
	DaoValue *C = NULL, *tmp = NULL;
	DaoValue *A = self->activeValues[ vmc->a ];
	DaoType *type = (DaoType*) A;
	DaoEnum denum2 = {DAO_ENUM,DAO_ENUM_SYM,0,0,0,0,0,NULL};
	DaoEnum *denum = & denum2;
	DaoClass *thisClass = NULL;
	DaoRoutine *routine = self->activeRoutine;
	DString *name = routine->routConsts->value->items.pValue[vmc->b]->xString.value;
	int opb;

	self->activeCode = vmc;
	if( A == NULL || A->type == 0 ){
		DaoProcess_RaiseError( self, "Value", "on none object" );
		return;
	}
	switch( A->type ){
	case DAO_TUPLE :
		opb = DaoTuple_GetIndex( (DaoTuple*) A, name );
		if( opb < 0 ) goto InvalidConstField;
		C = A->xTuple.values[opb];
		break;
	case DAO_TYPE :
		if( type->tid == DAO_TYPE ) type = type->nested->items.pType[0];
		if( type && type->tid == DAO_ENUM && type->mapNames ){
			DNode *node = DMap_Find( type->mapNames, name );
			if( node ){
				denum->etype = type;
				denum->value = node->value.pInt;
				denum->subtype = type->subtid;
				C = (DaoValue*) denum;
			}
		}
		break;
	case DAO_NAMESPACE :
		if( DaoNamespace_GetData( & A->xNamespace, name ) == NULL ) goto InvalidConstField;
		opb = DaoNamespace_FindConst( & A->xNamespace, name );
		if( opb >= 0 ) C = DaoNamespace_GetConst( & A->xNamespace, opb );
		break;
	case DAO_CLASS :
		if( routine->routHost ) thisClass = DaoValue_CastClass( routine->routHost->aux );
		if( mode & DAO_CONST_EVAL_METHDEF ) thisClass = (DaoClass*) A;
		tmp = DaoClass_GetData( (DaoClass*) A, name, thisClass );
		if( tmp != NULL && tmp->type == DAO_CONSTANT ) C = tmp->xConst.value;
		break;
	default :
		type = DaoNamespace_GetType( self->activeNamespace, A );
		if( mode & DAO_CONST_EVAL_GETVALUE ){
			C = DaoType_FindValueOnly( type, name );
		}else{
			C = DaoType_FindValue( type, name );
		}
		break;
	}
	DaoProcess_PutValue( self, C );
	return;
InvalidConstField:
	DaoProcess_RaiseError( self, "Field", "invalid field" );
	DaoProcess_PutValue( self, NULL );
}
DaoValue* DaoProcess_MakeConst( DaoProcess *self, int mode )
{
	daoint size;
	DaoValue *A;
	DaoVmCodeX vmcx = {0,0,0,0,0,0,0,0,0};
	DaoVmCode *vmc = self->activeCode;

	self->activeValues = self->stackValues;
	DArray_Clear( self->activeRoutine->body->vmCodes );
	DArray_PushCode( self->activeRoutine->body->vmCodes, *vmc );
	if( self->activeRoutine->body->annotCodes->size == 0 )
		DList_Append( self->activeRoutine->body->annotCodes, & vmcx );

	/*
	// DaoProcess_PopFrame() and DaoProcess_SetActiveFrame() will be called
	// for C function calls, and self->activeTypes will be updated to the
	// frame's ::codes and ::types.
	*/
	self->activeCode = self->activeRoutine->body->vmCodes->data.codes;
	self->topFrame->codes = self->activeCode;
	self->topFrame->types = self->activeTypes;
	vmc = self->activeCode;
	switch( vmc->code ){
	case DVM_ADD : case DVM_SUB : case DVM_MUL :
	case DVM_DIV : case DVM_MOD : case DVM_POW :
		DaoProcess_DoBinArith( self, vmc );
		break;
	case DVM_AND : case DVM_OR : case DVM_LT :
	case DVM_LE :  case DVM_EQ : case DVM_NE :
		DaoProcess_DoBinBool( self, vmc );
		break;
	case DVM_IN :
		DaoProcess_DoInTest( self, vmc );
		break;
	case DVM_NOT :
		DaoProcess_DoUnaBool( self, vmc ); break;
	case DVM_MINUS :
		DaoProcess_DoUnaArith( self, vmc ); break;
	case DVM_BITAND : case DVM_BITOR : case DVM_BITXOR :
		DaoProcess_DoBitLogic( self, vmc ); break;
	case DVM_BITLFT : case DVM_BITRIT :
		DaoProcess_DoBitShift( self, vmc ); break;
	case DVM_TILDE :
		DaoProcess_DoBitFlip( self, vmc ); break;
	case DVM_SIZE :
		size = -1;
		A = self->activeValues[ vmc->a ];
		switch( A->type ){
		case DAO_NONE    : size = 0; break;
		case DAO_INTEGER : size = sizeof(dao_integer); break;
		case DAO_FLOAT   : size = sizeof(dao_float); break;
		case DAO_COMPLEX : size = sizeof(dao_complex); break;
		case DAO_ENUM    : size = sizeof(int); break; break;
		case DAO_STRING  : size = A->xString.value->size; break;
		case DAO_LIST    : size = A->xList.value->size; break;
		case DAO_MAP     : size = A->xMap.value->size; break;
		case DAO_TUPLE   : size = A->xTuple.size; break;
#ifdef DAO_WITH_NUMARRAY
		case DAO_ARRAY   : size = A->xArray.size; break;
#endif
		default : break;//goto RaiseErrorInvalidOperation;
		}
		if( size < 0 ) break;
		DaoProcess_PutInteger( self, size );
		break;
	case DVM_SAME :
		DaoProcess_DoCheckSame( self, vmc ); break;
	case DVM_ISA :
		DaoProcess_DoCheckIsa( self, vmc ); break;
	case DVM_NAMEVA :
		DaoProcess_BindNameValue( self, vmc ); break;
	case DVM_PAIR :
		DaoProcess_DoPair( self, vmc ); break;
	case DVM_TUPLE :
		DaoProcess_DoTuple( self, vmc ); break;
	case DVM_GETI :
	case DVM_GETDI :
	case DVM_GETMI :
		DaoProcess_DoGetItem( self, vmc ); break;
	case DVM_GETF :
		DaoProcess_DoGetConstField( self, vmc, mode ); break;
	case DVM_LIST :
		if( (vmc->b >> 14) == DVM_ENUM_MODE0 ){
			DaoProcess_DoList( self, vmc );
		}else{
			DaoProcess_DoAPList( self, vmc );
		}
		break;
	case DVM_MAP :
		DaoProcess_DoMap( self, vmc );
		break;
	case DVM_VECTOR :
		if( (vmc->b >> 14) == DVM_ENUM_MODE0 ){
			DaoProcess_DoVector( self, vmc );
		}else{
			DaoProcess_DoAPVector( self, vmc );
		}
		break;
	case DVM_MATRIX :
		DaoProcess_DoMatrix( self, vmc ); break;
	case DVM_MATH :
		DaoVM_DoMath( self, vmc, self->activeValues[ vmc->c ], self->activeValues[1] );
		break;
	case DVM_PACK :
	case DVM_MPACK :
		DaoProcess_DoPacking( self, vmc );
		break;
	case DVM_CALL :
	case DVM_MCALL :
		DaoProcess_DoCall( self, vmc );
		break;
	default: break;
	}
	if( self->status == DAO_PROCESS_STACKED ){
		self->topFrame->returning = 0xffff;
		DaoProcess_Execute( self );
	}
	if( self->exceptions->size >0 ) return NULL;

	/* avoid GC */
	/* DList_Clear( self->regArray ); */
	return self->stackValues[ vmc->c ];
}


void DaoProcess_SetStdio( DaoProcess *self, DaoStream *stream )
{
	GC_Assign( & self->stdioStream, stream );
}

void* DaoProcess_GetAuxData( DaoProcess *self, void *key )
{
	DNode *node;
	if( self->aux == NULL ) self->aux = DMap_New(0,0);
	node = DMap_Find( self->aux, key );
	if( node ) return node->value.pVoid;
	return NULL;
}
void* DaoProcess_SetAuxData( DaoProcess *self, void *key, void *value )
{
	void *prev = DaoProcess_GetAuxData( self, key );
	if( prev != NULL ){
		typedef void (*aux_delete)(void*);
		aux_delete del = (aux_delete) key;
		(*del)( prev );
	}
	DMap_Insert( self->aux, key, value );
	return value;
}



void DaoProcess_FreeRegexCaches( DMap *regexCaches )
{
	DNode *it = DMap_First( regexCaches );
	for( ; it !=NULL; it = DMap_Next(regexCaches, it) ) dao_free( it->value.pVoid );
	DMap_Delete( regexCaches );
}

DaoRegex* DaoProcess_MakeRegex( DaoProcess *self, DString *src )
{
	DMap *regexCaches = NULL;
	DaoRegex *pat = NULL;
	DNode *node;
	char buf[50];
	int i;
	DString_Trim( src, 1, 1, 0 );
	if( src->size ==0 ){
		if( self->activeRoutine )
			DaoProcess_RaiseError( self, NULL, "pattern with empty string" );
		return NULL;
	}
	regexCaches = (DMap*) DaoProcess_GetAuxData( self, DaoProcess_FreeRegexCaches );
	if( regexCaches == NULL ){
		regexCaches = DHash_New( DAO_DATA_STRING, 0 );
		DaoProcess_SetAuxData( self, DaoProcess_FreeRegexCaches, regexCaches );
	}
	node = DMap_Find( regexCaches, src );
	if( node ) return (DaoRegex*) node->value.pVoid;

	pat = DaoRegex_New( src );
	for( i=0; i<pat->count; i++ ){
		DaoRgxItem *it = pat->items + i;
		if( it->type ==0 ){
			sprintf( buf, "incorrect pattern, at char %i.", it->length );
			if( self->activeRoutine ) DaoProcess_RaiseError( self, NULL, buf );
			DaoRegex_Delete( pat );
			return NULL;
		}
	}
	DMap_Insert( regexCaches, src, pat );
	return pat;
}


void DaoProcess_CacheValue( DaoProcess *self, DaoValue *value )
{
	DList_Append( self->factory, NULL );
	GC_IncRC( value );
	self->factory->items.pValue[ self->factory->size - 1 ] = value;
}
void DaoProcess_PopValues( DaoProcess *self, int N )
{
	if( N < 0 ) return;
	if( N >= (int)self->factory->size ){
		DList_Clear( self->factory );
	}else{
		DList_Erase( self->factory, self->factory->size - N, N );
	}
}
DaoValue** DaoProcess_GetLastValues( DaoProcess *self, int N )
{
	if( N > (int)self->factory->size ) return self->factory->items.pValue;
	return self->factory->items.pValue + (self->factory->size - N);
}

DaoNone* DaoProcess_NewNone( DaoProcess *self )
{
	DaoNone *res = DaoNone_New();
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}
DaoBoolean* DaoProcess_NewBoolean( DaoProcess *self, dao_boolean v )
{
	DaoBoolean *res = DaoBoolean_New( v );
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}
DaoInteger* DaoProcess_NewInteger( DaoProcess *self, dao_integer v )
{
	DaoInteger *res = DaoInteger_New( v );
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}
DaoFloat* DaoProcess_NewFloat( DaoProcess *self, dao_float v )
{
	DaoFloat *res = DaoFloat_New( v );
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}
DaoComplex* DaoProcess_NewComplex( DaoProcess *self, dao_complex v )
{
	DaoComplex *res = DaoComplex_New( v );
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}
DaoString* DaoProcess_NewString( DaoProcess *self, const char *s, daoint n )
{
	DaoString *res = DaoString_New();
	if( s ) DString_SetBytes( res->value, s, n );
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}
DaoEnum* DaoProcess_NewEnum( DaoProcess *self, DaoType *type, int value )
{
	DaoEnum *res = DaoEnum_New( type, value );
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}
DaoTuple* DaoProcess_NewTuple( DaoProcess *self, int count )
{
	int i, N = abs( count );
	int M = self->factory->size;
	DaoValue **values = self->factory->items.pValue;
	DaoTuple *res = NULL;
	if( count < 0 ){
		if( M < N ) return NULL;
		res = DaoTuple_New( N );
		for(i=0; i<N; i++) DaoTuple_SetItem( res, values[M-N+i], i );
	}
	if( res == NULL ) res = DaoTuple_New( N );
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}
DaoList* DaoProcess_NewList( DaoProcess *self )
{
	DaoList *res = DaoList_New();
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}
DaoMap* DaoProcess_NewMap( DaoProcess *self, unsigned int hashing )
{
	DaoMap *res = DaoMap_New( hashing );
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}
DaoArray* DaoProcess_NewArray( DaoProcess *self, int type )
{
#ifdef DAO_WITH_NUMARRAY
	DaoArray *res = DaoArray_New( type );
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
#else
	return NULL;
#endif
}
DaoStream* DaoProcess_NewStream( DaoProcess *self, FILE *f )
{
	DaoStream *res = DaoStream_New();
	DaoStream_SetFile( res, f );
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}
DaoCdata* DaoProcess_NewCdata( DaoProcess *self, DaoType *type, void *data, int owned )
{
	DaoCdata *res = DaoWrappers_MakeCdata( type, data, owned );
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}
