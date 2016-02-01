

/*
	Parallel processing module
*/


#include <sgs_int.h>

#define IFN( x ) { sgs_PushCFunc( C, x ); return SGS_SUCCESS; }
#define STDLIB_WARN( warn ) return sgs_Msg( C, SGS_WARNING, warn );


#ifdef _WIN32

#  define WIN32_LEAN_AND_MEAN
#  include <windows.h>

#  define sgsthread_sleep( ms ) Sleep( (DWORD) ms )

#  define sgsmutex_t CRITICAL_SECTION
#  define sgsthread_t HANDLE
#  define threadret_t DWORD __stdcall

#  define sgsthread_create( toT, func, data ) toT = CreateThread( NULL, 1024, func, data, 0, NULL )
#  define sgsthread_self( toT ) toT = GetCurrentThread()
#  define sgsthread_join( T ) WaitForMultipleObjects( 1, &T, TRUE, INFINITE )
#  define sgsthread_equal( T1, T2 ) (T1 == T2)

#  define sgsmutex_init( M ) InitializeCriticalSection( &M )
#  define sgsmutex_destroy( M ) DeleteCriticalSection( &M )
#  define sgsmutex_lock( M ) EnterCriticalSection( &M )
#  define sgsmutex_unlock( M ) LeaveCriticalSection( &M )


#else

#  include <unistd.h>
#  include <pthread.h>

static void sgsthread_sleep( uint32_t ms )
{
	if( ms >= 1000 )
	{
		sleep( ms / 1000 );
		ms %= 1000;
	}
	if( ms > 0 )
	{
		usleep( ms * 1000 );
	}
}

#  define sgsmutex_t pthread_mutex_t
#  define sgsthread_t pthread_t
#  define threadret_t void*

#  define sgsthread_create( T, func, data ) pthread_create( &T, NULL, func, data )
#  define sgsthread_self( toT ) toT = pthread_self()
#  define sgsthread_join( T ) pthread_join( T, NULL )
#  define sgsthread_equal( T1, T2 ) pthread_equal( T1, T2 )

#  define sgsmutex_init( M ) pthread_mutex_init( &M, NULL )
#  define sgsmutex_destroy( M ) pthread_mutex_destroy( &M )
#  define sgsmutex_lock( M ) pthread_mutex_lock( &M )
#  define sgsmutex_unlock( M ) pthread_mutex_unlock( &M )


#endif



#define PP_STATE_INIT    0
#define PP_STATE_RUNNING 1
#define PP_STATE_DONE    2

typedef struct _ppmapitem_t ppmapitem_t;
struct _ppmapitem_t
{
	char* data;
	sgs_SizeVal keysize;
	sgs_SizeVal datasize;
	ppmapitem_t* next;
};

typedef struct _ppthread_t
{
	volatile int state;
	
	sgsthread_t self;
	sgsthread_t thread;
	sgsmutex_t mutex;
	
	sgs_MemFunc mf;
	void* mfud;
	
	char* code;
	sgs_SizeVal codesize;
	SGS_CTX;
	
	ppmapitem_t* data;
}
ppthread_t;


static ppmapitem_t* ppthread_map_find( ppthread_t* THR, char* key, sgs_SizeVal keysize )
{
	ppmapitem_t* item = THR->data;
	while( item )
	{
		if( item->keysize == keysize && !memcmp( item->data, key, (size_t) keysize ) )
			return item;
		item = item->next;
	}
	return NULL;
}

static void ppthread_map_set( ppthread_t* THR,
	char* key, sgs_SizeVal keysize, char* data, sgs_SizeVal datasize )
{
	ppmapitem_t* item = ppthread_map_find( THR, key, keysize );
	if( item )
	{
		char* nd = (char*) THR->mf( THR->mfud, NULL, (size_t) ( keysize + datasize ) );
		memcpy( nd, key, (size_t) keysize );
		memcpy( nd + keysize, data, (size_t) datasize );
		THR->mf( THR->mfud, item->data, 0 );
		item->data = nd;
		item->datasize = datasize;
	}
	else
	{
		item = (ppmapitem_t*) THR->mf( THR->mfud, NULL, sizeof( ppmapitem_t ) );
		item->keysize = keysize;
		item->datasize = datasize;
		item->data = (char*) THR->mf( THR->mfud, NULL, (size_t) ( keysize + datasize ) );
		memcpy( item->data, key, (size_t) keysize );
		memcpy( item->data + keysize, data, (size_t) datasize );
		item->next = THR->data;
		THR->data = item;
	}
}

static void ppthread_map_free( ppthread_t* THR )
{
	ppmapitem_t* item = THR->data;
	while( item )
	{
		ppmapitem_t* N = item;
		item = item->next;
		
		THR->mf( THR->mfud, N->data, 0 );
		THR->mf( THR->mfud, N, 0 );
	}
	THR->data = NULL;
}


#define PPTHREAD_HDR ppthread_t* THR = (ppthread_t*) obj->data

SGS_DECLARE sgs_ObjInterface ppthread_iface[1];
#define PPTHREAD_IHDR( name ) ppthread_t* THR; \
	int method_call = sgs_Method( C ); \
	SGSFN( method_call ? "ppthread." #name : "ppthread_" #name ); \
	if( !method_call \
		|| !( sgs_IsObject( C, 0, ppthread_iface ) \
		|| sgs_IsObject( C, 0, ppthread_iface_thr ) ) \
		) STDLIB_WARN( "expected ppthread as 'this'" ) \
	THR = (ppthread_t*) sgs_GetObjectData( C, 0 ); \
	SGS_UNUSED( THR );


SGS_DECLARE sgs_ObjInterface ppthread_iface_thr[1];
static int pproc_sleep( SGS_CTX );
static threadret_t ppthread_threadfunc( void* arg )
{
	ppthread_t* THR = (ppthread_t*) arg;
	
	THR->C = sgs_CreateEngineExt( THR->mf, THR->mfud );
	
	sgs_SetGlobalByName( THR->C, "pproc_sleep", sgs_MakeCFunc( pproc_sleep ) );
	
	sgs_CreateObject( THR->C, NULL, THR, ppthread_iface_thr );
	sgs_SetGlobalByName( THR->C, "_T", sgs_StackItem( THR->C, -1 ) );
	sgs_Pop( THR->C, 1 );
	
	sgs_ExecBuffer( THR->C, THR->code, (size_t) THR->codesize );
	
	sgs_DestroyEngine( THR->C );
	
	sgsmutex_lock( THR->mutex );
	THR->state = PP_STATE_DONE;
	sgsmutex_unlock( THR->mutex );
	
	return 0;
}


static ppthread_t* ppthread_create( SGS_CTX, char* code, sgs_SizeVal codesize )
{
	SGS_SHCTX_USE;
	sgs_MemFunc mf = S->memfunc;
	void* mfud = S->mfuserdata;
	
	ppthread_t* THR = (ppthread_t*) mf( mfud, NULL, sizeof( ppthread_t ) );
	
	sgsthread_self( THR->self );
	sgsthread_self( THR->thread );
	sgsmutex_init( THR->mutex );
	
	THR->code = (char*) mf( mfud, NULL, (size_t) codesize );
	memcpy( THR->code, code, (size_t) codesize );
	THR->codesize = codesize;
	
	THR->state = PP_STATE_INIT;
	
	THR->mf = mf;
	THR->mfud = mfud;
	
	THR->data = NULL;
	
	return THR;
}

static void ppthread_destroy( ppthread_t* THR )
{
	sgs_MemFunc mf = THR->mf;
	void* mfud = THR->mfud;
	/* SGS_CTX should already be destroyed here */
	mf( mfud, THR->code, 0 );
	
	ppthread_map_free( THR );
	
	sgsmutex_destroy( THR->mutex );
	mf( mfud, THR, 0 );
}

static int ppthread_start( ppthread_t* THR )
{
	int ret;
	sgsmutex_lock( THR->mutex );
	
	if( !sgsthread_equal( THR->self, THR->thread ) )
		ret = 0;
	else if( THR->state == PP_STATE_RUNNING )
		ret = 0;
	else
	{
		THR->state = PP_STATE_RUNNING;
		sgsthread_create( THR->thread, ppthread_threadfunc, THR );
		ret = 1;
	}
	
	sgsmutex_unlock( THR->mutex );
	return ret;
}

static int ppthread_wait( ppthread_t* THR )
{
	if( !sgsthread_equal( THR->self, THR->thread ) )
	{
		sgsthread_join( THR->thread );
		return 1;
	}
	return 0;
}


static int ppthreadI_start( SGS_CTX )
{
	PPTHREAD_IHDR( start );
	if( !sgs_LoadArgs( C, "." ) )
		return 0;
	
	sgs_PushBool( C, ppthread_start( THR ) );
	return 1;
}

static int ppthreadI_wait( SGS_CTX )
{
	PPTHREAD_IHDR( wait );
	if( !sgs_LoadArgs( C, "." ) )
		return 0;
	
	sgs_PushBool( C, ppthread_wait( THR ) );
	return 1;
}

static int ppthreadI_has( SGS_CTX )
{
	char* str;
	sgs_SizeVal size;
	
	PPTHREAD_IHDR( has );
	if( !sgs_LoadArgs( C, "m", &str, &size ) )
		return 0;
	
	sgsmutex_lock( THR->mutex );
	{
		ppmapitem_t* item = ppthread_map_find( THR, str, size );
		sgs_PushBool( C, !!item );
	}
	sgsmutex_unlock( THR->mutex );
	
	return 1;
}

static int ppthreadI_get( SGS_CTX )
{
	int ret;
	char* str;
	sgs_SizeVal size;
	
	PPTHREAD_IHDR( get );
	if( !sgs_LoadArgs( C, "m", &str, &size ) )
		return 0;
	
	sgsmutex_lock( THR->mutex );
	{
		ppmapitem_t* item = ppthread_map_find( THR, str, size );
		if( !item )
		{
			sgs_Msg( C, SGS_WARNING, "could not find item \"%.*s\"", size, str );
			ret = 0;
		}
		else
		{
			sgs_PushStringBuf( C, item->data + item->keysize, item->datasize );
			sgs_Unserialize( C, sgs_StackItem( C, -1 ) );
			ret = 1;
		}
	}
	sgsmutex_unlock( THR->mutex );
	
	return ret;
}

static int ppthreadI_set( SGS_CTX )
{
	char* str, *var;
	sgs_SizeVal size, varsize;
	
	PPTHREAD_IHDR( set );
	if( !sgs_LoadArgs( C, "m?v", &str, &size ) )
		return 0;
	
	sgs_Serialize( C, sgs_StackItem( C, 1 ) );
	if( !sgs_ParseString( C, -1, &var, &varsize ) )
		STDLIB_WARN( "failed to serialize item (argument 2)" )
	
	sgsmutex_lock( THR->mutex );
	ppthread_map_set( THR, str, size, var, varsize );
	sgsmutex_unlock( THR->mutex );
	
	sgs_PushBool( C, 1 );
	return 1;
}

static int ppthreadI_set_if( SGS_CTX )
{
	int ret = -1;
	char* str, *var, *var2;
	sgs_SizeVal size, varsize, var2size;
	
	PPTHREAD_IHDR( set_if );
	if( !sgs_LoadArgs( C, "m?v?v", &str, &size ) )
		return 0;
	
	sgs_Serialize( C, sgs_StackItem( C, 1 ) );
	if( !sgs_ParseString( C, -1, &var, &varsize ) )
		STDLIB_WARN( "failed to serialize item (argument 2)" )
	
	sgs_Serialize( C, sgs_StackItem( C, 2 ) );
	if( !sgs_ParseString( C, -1, &var2, &var2size ) )
		STDLIB_WARN( "failed to serialize item (argument 3)" )
	
	sgsmutex_lock( THR->mutex );
	{
		ppmapitem_t* item = ppthread_map_find( THR, str, size );
		if( !item )
		{
			sgs_Msg( C, SGS_WARNING, "could not find item \"%.*s\"", size, str );
			ret = 0;
		}
		else if( item->datasize == var2size && 
			memcmp( item->data + item->keysize, var2, (size_t) var2size ) == 0 )
		{
			ppthread_map_set( THR, str, size, var, varsize );
			sgs_PushBool( C, 1 );
			ret = 1;
		}
		else
		{
			sgs_PushBool( C, 0 );
			ret = 1;
		}
	}
	sgsmutex_unlock( THR->mutex );
	
	return ret;
}

static int ppthreadP_state( SGS_CTX, sgs_VarObj* obj )
{
	int state;
	PPTHREAD_HDR;
	state = THR->state;
	sgs_PushInt( C, state );
	return SGS_SUCCESS;
}


static int ppthread_getindex( SGS_ARGS_GETINDEXFUNC )
{
	SGS_BEGIN_INDEXFUNC
		SGS_CASE( "start" )  IFN( ppthreadI_start )
		SGS_CASE( "wait" )   IFN( ppthreadI_wait )
		
		SGS_CASE( "has" )    IFN( ppthreadI_has )
		SGS_CASE( "get" )    IFN( ppthreadI_get )
		SGS_CASE( "set" )    IFN( ppthreadI_set )
		SGS_CASE( "set_if" ) IFN( ppthreadI_set_if )
		
		SGS_CASE( "state" )  return ppthreadP_state( C, obj );
	SGS_END_INDEXFUNC;
}

static int ppthread_destruct( SGS_CTX, sgs_VarObj* obj )
{
	PPTHREAD_HDR;
	
	if( THR->state == PP_STATE_RUNNING )
	{
		sgsthread_join( THR->thread );
	}
	
	ppthread_destroy( THR );
	
	return SGS_SUCCESS;
}

static sgs_ObjInterface ppthread_iface[1] =
{{
	"ppthread",
	ppthread_destruct, NULL,
	ppthread_getindex, NULL,
	NULL, NULL, NULL, NULL,
	NULL, NULL
}};

static sgs_ObjInterface ppthread_iface_thr[1] =
{{
	"ppthread_interface",
	NULL, NULL,
	ppthread_getindex, NULL,
	NULL, NULL, NULL, NULL,
	NULL, NULL
}};

static int pproc_serialize_function( SGS_CTX,
	sgs_iFunc* func, char** out, sgs_SizeVal* outsize )
{
	int ret;
	sgs_MemBuf B = sgs_membuf_create();
	sgs_CompFunc F;
	{
		F.consts = sgs_membuf_create();
		F.code = sgs_membuf_create();
		F.lnbuf = sgs_membuf_create();
		F.gotthis = func->gotthis;
		F.numargs = func->numargs;
		F.numtmp = func->numtmp;
		F.numclsr = func->numclsr;
	}
	
	sgs_membuf_appbuf( &F.consts, C, ((char*)(func+1)), func->instr_off );
	sgs_membuf_appbuf( &F.code, C, ((char*)(func+1)) +
		func->instr_off, func->size - func->instr_off );
	sgs_membuf_appbuf( &F.lnbuf, C, func->lineinfo,
		( func->size - func->instr_off ) / 2 );
	
	ret = sgsBC_Func2Buf( C, &F, &B );
	
	sgs_membuf_destroy( &F.consts, C );
	sgs_membuf_destroy( &F.code, C );
	sgs_membuf_destroy( &F.lnbuf, C );
	
	if( ret )
	{
		*out = B.ptr;
		*outsize = (sgs_SizeVal) B.size;
	}
	else
	{
		sgs_membuf_destroy( &B, C );
	}
	return ret;
}

static int pproc_create_thread( SGS_CTX )
{
	char* str;
	sgs_SizeVal size;
	sgs_StkIdx ssz = sgs_StackSize( C );
	uint32_t type;
	
	SGSFN( "pproc_create_thread" );
	if( ssz != 1 || !( type = sgs_ItemType( C, 0 ) ) ||
		( type != SGS_VT_FUNC && type != SGS_VT_STRING ) ||
		( type == SGS_VT_STRING && !sgs_ParseString( C, 0, &str, &size ) ) )
		return sgs_ArgErrorExt( C, 0, 0, "string/function", "" );
	
	if( type == SGS_VT_FUNC )
	{
		sgs_Variable var = sgs_StackItem( C, 0 );
		if( !pproc_serialize_function( C, var.data.F, &str, &size ) )
			STDLIB_WARN( "failed to serialize function" )
	}
	else
	{
		char* code;
		size_t codesize;
		if( sgs_Compile( C, str, (size_t) size, &code, &codesize ) != SGS_SUCCESS )
			STDLIB_WARN( "failed to compile the code" )
		str = code;
		size = (sgs_SizeVal) codesize;
	}
	sgs_CreateObject( C, NULL, ppthread_create( C, str, size ), ppthread_iface );
	sgs_Dealloc( str );
	return 1;
}

static int pproc_sleep( SGS_CTX )
{
	sgs_Int ms;
	SGSFN( "pproc_sleep" );
	if( !sgs_LoadArgs( C, "i", &ms ) )
		return 0;
	
	sgsthread_sleep( (uint32_t) ms );
	return 0;
}


#ifdef SGS_COMPILE_MODULE
#  define pproc_module_entry_point sgscript_main
#endif


#ifdef __cplusplus
extern "C"
#endif
#ifdef _WIN32
__declspec(dllexport)
#endif
int pproc_module_entry_point( SGS_CTX )
{
	SGS_MODULE_CHECK_VERSION( C );
	sgs_RegFuncConst rfc[] =
	{
		{ "pproc_create_thread", pproc_create_thread },
		{ "pproc_sleep", pproc_sleep },
	};
	sgs_RegFuncConsts( C, rfc, SGS_ARRAY_SIZE( rfc ) );
	return SGS_SUCCESS;
}
