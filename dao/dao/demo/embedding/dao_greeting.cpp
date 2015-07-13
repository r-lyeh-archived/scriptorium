#include"dao_Greeting.h"
DAO_INIT_MODULE;
DaoVmSpace *__daoVmSpace = NULL;
#ifdef __cplusplus
extern "C"{
#endif
static void dao__GetGreetingObject( DaoProcess *_proc, DaoValue *_p[], int _n );
static void dao__Testing( DaoProcess *_proc, DaoValue *_p[], int _n );
static DaoFuncItem dao__Funcs[] = 
{
  { dao__GetGreetingObject, "GetGreetingObject(  )=>Greeting" },
  { dao__Testing, "Testing( bl :int =CxxNS::FALSE )" },
  { NULL, NULL }
};
/* ./greeting.h */
static void dao__GetGreetingObject( DaoProcess *_proc, DaoValue *_p[], int _n )
{

  Greeting* _GetGreetingObject = GetGreetingObject(  );
  DaoProcess_WrapCdata( _proc, (void*) _GetGreetingObject, dao_Greeting_Typer );
}
/* ./greeting.h */
static void dao__Testing( DaoProcess *_proc, DaoValue *_p[], int _n )
{
  CxxNS::Bool bl = (CxxNS::Bool) DaoValue_TryGetInteger( _p[0] );

  Testing( bl );
}
#ifdef __cplusplus
}
#endif
static DaoNumItem dao__Nums[] = 
{
  { "AA", DAO_INTEGER, AA },
  { "BB", DAO_INTEGER, BB },
  { "CC", DAO_INTEGER, CC },
  { NULL, 0, 0 }
};
#ifdef __cplusplus
extern "C"{
#endif
static void dao_CxxNS_Testing( DaoProcess *_proc, DaoValue *_p[], int _n );
static void dao_CxxNS_Testing_dao_2( DaoProcess *_proc, DaoValue *_p[], int _n );
static void dao_CxxNS_Testing_dao_3( DaoProcess *_proc, DaoValue *_p[], int _n );
static void dao_CxxNS_Testing2( DaoProcess *_proc, DaoValue *_p[], int _n );
static DaoFuncItem dao_CxxNS_Funcs[] = 
{
  { dao_CxxNS_Testing, "Testing( greeting :Greeting, bl :int =FALSE )" },
  { dao_CxxNS_Testing_dao_2, "Testing( a :int, bl :int =FALSE )" },
  { dao_CxxNS_Testing_dao_3, "Testing( t :CxxNS::Test, b :int =0, o :CxxNS::Test|null =null, g :CxxNS::Test|null =null, c :int =0 )" },
  { dao_CxxNS_Testing2, "Testing2( t :CxxNS::Test, b :int =0, o :CxxNS::Test|null =null, g :CxxNS::Test|null =null, c :int =0 )=>int" },
  { NULL, NULL }
};
/* ./greeting.h */
static void dao_CxxNS_Testing( DaoProcess *_proc, DaoValue *_p[], int _n )
{
  Greeting* greeting= (Greeting*) DaoValue_TryCastCdata( _p[0], dao_Greeting_Typer );
  CxxNS::Bool bl = (CxxNS::Bool) DaoValue_TryGetInteger( _p[1] );

  CxxNS::Testing( greeting, bl );
}
/* ./greeting.h */
static void dao_CxxNS_Testing_dao_2( DaoProcess *_proc, DaoValue *_p[], int _n )
{
  int a = (int) DaoValue_TryGetInteger( _p[0] );
  CxxNS::Bool bl = (CxxNS::Bool) DaoValue_TryGetInteger( _p[1] );

  CxxNS::Testing( a, bl );
}
/* ./greeting.h */
static void dao_CxxNS_Testing_dao_3( DaoProcess *_proc, DaoValue *_p[], int _n )
{
  CxxNS::Test* t= (CxxNS::Test*) DaoValue_TryCastCdata( _p[0], dao_CxxNS_0_Test_Typer );
  int b = (int) DaoValue_TryGetInteger( _p[1] );
  CxxNS::Test* o= (CxxNS::Test*) DaoValue_TryCastCdata( _p[2], dao_CxxNS_0_Test_Typer );
  CxxNS::Test* g= (CxxNS::Test*) DaoValue_TryCastCdata( _p[3], dao_CxxNS_0_Test_Typer );
  int c = (int) DaoValue_TryGetInteger( _p[4] );

  if(_n<=2) CxxNS::Testing( t, b );
  else if(_n<=3) CxxNS::Testing( t, b, *o );
  else CxxNS::Testing( t, b, *o, *g, c );
}
/* ./greeting.h */
static void dao_CxxNS_Testing2( DaoProcess *_proc, DaoValue *_p[], int _n )
{
  CxxNS::Test* t= (CxxNS::Test*) DaoValue_TryCastCdata( _p[0], dao_CxxNS_0_Test_Typer );
  int b = (int) DaoValue_TryGetInteger( _p[1] );
  CxxNS::Test* o= (CxxNS::Test*) DaoValue_TryCastCdata( _p[2], dao_CxxNS_0_Test_Typer );
  CxxNS::Test* g= (CxxNS::Test*) DaoValue_TryCastCdata( _p[3], dao_CxxNS_0_Test_Typer );
  int c = (int) DaoValue_TryGetInteger( _p[4] );

  int _Testing2;
  if(_n<=2) _Testing2 = CxxNS::Testing2( t, b );
  else if(_n<=3) _Testing2 = CxxNS::Testing2( t, b, *o );
  else _Testing2 = CxxNS::Testing2( t, b, *o, *g, c );
  DaoProcess_PutInteger( _proc, (int) _Testing2 );
}
#ifdef __cplusplus
}
#endif
static DaoNumItem dao_CxxNS_Nums[] = 
{
  { "FALSE", DAO_INTEGER, CxxNS::FALSE },
  { "TRUE", DAO_INTEGER, CxxNS::TRUE },
  { "AA", DAO_INTEGER, CxxNS::AA },
  { "BB", DAO_INTEGER, CxxNS::BB },
  { "CC", DAO_INTEGER, CxxNS::CC },
  { NULL, 0, 0 }
};
#ifdef __cplusplus
extern "C"{
#endif
static void dao_CxxNS2_Testing( DaoProcess *_proc, DaoValue *_p[], int _n );
static DaoFuncItem dao_CxxNS2_Funcs[] = 
{
  { dao_CxxNS2_Testing, "Testing( test :CxxNS::Test, bl :int =CxxNS::FALSE )" },
  { NULL, NULL }
};
/* ./greeting.h */
static void dao_CxxNS2_Testing( DaoProcess *_proc, DaoValue *_p[], int _n )
{
  CxxNS::Test* test= (CxxNS::Test*) DaoValue_TryCastCdata( _p[0], dao_CxxNS_0_Test_Typer );
  CxxNS::Bool bl = (CxxNS::Bool) DaoValue_TryGetInteger( _p[1] );

  CxxNS2::Testing( test, bl );
}
#ifdef __cplusplus
}
#endif
static DaoTypeBase *dao__Typers[] = 
{
	dao___darwin_pthread_handler_rec_Typer,
	dao__opaque_pthread_attr_t_Typer,
	dao__opaque_pthread_cond_t_Typer,
	dao__opaque_pthread_condattr_t_Typer,
	dao__opaque_pthread_mutex_t_Typer,
	dao__opaque_pthread_mutexattr_t_Typer,
	dao__opaque_pthread_once_t_Typer,
	dao__opaque_pthread_rwlock_t_Typer,
	dao__opaque_pthread_rwlockattr_t_Typer,
	dao__opaque_pthread_t_Typer,
	dao___sbuf_Typer,
	dao___sFILEX_Typer,
	dao___sFILE_Typer,
	dao___darwin_i386_thread_state_Typer,
	dao___darwin_fp_control_Typer,
	dao___darwin_fp_status_Typer,
	dao___darwin_mmst_reg_Typer,
	dao___darwin_xmm_reg_Typer,
	dao___darwin_i386_float_state_Typer,
	dao___darwin_i386_exception_state_Typer,
	dao___darwin_x86_debug_state32_Typer,
	dao___darwin_x86_thread_state64_Typer,
	dao___darwin_x86_float_state64_Typer,
	dao___darwin_x86_exception_state64_Typer,
	dao___darwin_x86_debug_state64_Typer,
	dao___darwin_mcontext32_Typer,
	dao___darwin_mcontext64_Typer,
	dao___darwin_sigaltstack_Typer,
	dao___darwin_ucontext_Typer,
	dao_sigval_Typer,
	dao_sigevent_Typer,
	dao___siginfo_Typer,
	dao___sigaction_u_Typer,
	dao___sigaction_Typer,
	dao_sigaction_Typer,
	dao_sigvec_Typer,
	dao_sigstack_Typer,
	dao_timeval_Typer,
	dao_rusage_Typer,
	dao_rlimit_Typer,
	dao_wait_Typer,
	dao_otto_Typer,
	dao_otto2_Typer,
	dao_Greeting_Typer,
	dao_Greeting_0_Null_Typer,
	dao_Greeting2_Typer,
	dao_AutobindTest_Typer,
	dao_CxxNS_0_Test_Typer,
	NULL
};
static const char *dao__Aliases[] = 
{
	"_opaque_pthread_attr_t", "__darwin_pthread_attr_t",
	"_opaque_pthread_cond_t", "__darwin_pthread_cond_t",
	"_opaque_pthread_condattr_t", "__darwin_pthread_condattr_t",
	"_opaque_pthread_mutex_t", "__darwin_pthread_mutex_t",
	"_opaque_pthread_mutexattr_t", "__darwin_pthread_mutexattr_t",
	"_opaque_pthread_once_t", "__darwin_pthread_once_t",
	"_opaque_pthread_rwlock_t", "__darwin_pthread_rwlock_t",
	"_opaque_pthread_rwlockattr_t", "__darwin_pthread_rwlockattr_t",
	"_opaque_pthread_t", "__darwin_pthread_t",
	"__sFILE", "FILE",
	"__darwin_fp_control", "__darwin_fp_control_t",
	"__darwin_fp_status", "__darwin_fp_status_t",
	"__darwin_mcontext32", "mcontext_t",
	"__darwin_sigaltstack", "stack_t",
	"__darwin_ucontext", "ucontext_t",
	"_opaque_pthread_attr_t", "pthread_attr_t",
	"__siginfo", "siginfo_t",
	"CxxNS::Test", "Test2",
	NULL
};
#ifdef __cplusplus
extern "C"{
#endif
int DaoOnLoad( DaoVmSpace *vms, DaoNamespace *ns )
{
	__daoVmSpace = vms;
	DaoNamespace *CxxNS = DaoVmSpace_GetNamespace( vms, "CxxNS" );
	DaoNamespace *CxxNS2 = DaoVmSpace_GetNamespace( vms, "CxxNS2" );
	DaoNamespace_AddConstNumbers( ns, dao__Nums );
	DaoNamespace_AddConstNumbers( CxxNS, dao_CxxNS_Nums );
	DaoNamespace_WrapTypes( ns, dao__Typers );
	DaoNamespace_TypeDefines( ns, dao__Aliases );
	DaoNamespace_WrapFunctions( ns, dao__Funcs );
	DaoNamespace_WrapFunctions( CxxNS, dao_CxxNS_Funcs );
	DaoNamespace_WrapFunctions( CxxNS2, dao_CxxNS2_Funcs );
	return 0;
}
#ifdef __cplusplus
}
#endif
