#ifndef __DAO_GREETING_H__
#define __DAO_GREETING_H__
#include<stdlib.h>
#include<assert.h>
#include<string.h>
#include<dao.h>

#include"greeting.h"


#ifndef DAO_GREETING_STATIC
#ifndef DAO_DLL_GREETING
#define DAO_DLL_GREETING DAO_DLL_EXPORT
#endif
#else
#define DAO_DLL_GREETING
#endif

extern DaoVmSpace *__daoVmSpace;
#ifdef __cplusplus
extern "C"{
#endif
extern DaoTypeBase *dao___darwin_pthread_handler_rec_Typer;
extern DaoTypeBase *dao__opaque_pthread_attr_t_Typer;
extern DaoTypeBase *dao__opaque_pthread_cond_t_Typer;
extern DaoTypeBase *dao__opaque_pthread_condattr_t_Typer;
extern DaoTypeBase *dao__opaque_pthread_mutex_t_Typer;
extern DaoTypeBase *dao__opaque_pthread_mutexattr_t_Typer;
extern DaoTypeBase *dao__opaque_pthread_once_t_Typer;
extern DaoTypeBase *dao__opaque_pthread_rwlock_t_Typer;
extern DaoTypeBase *dao__opaque_pthread_rwlockattr_t_Typer;
extern DaoTypeBase *dao__opaque_pthread_t_Typer;
extern DaoTypeBase *dao___sbuf_Typer;
extern DaoTypeBase *dao___sFILEX_Typer;
extern DaoTypeBase *dao___sFILE_Typer;
extern DaoTypeBase *dao___darwin_i386_thread_state_Typer;
extern DaoTypeBase *dao___darwin_fp_control_Typer;
extern DaoTypeBase *dao___darwin_fp_status_Typer;
extern DaoTypeBase *dao___darwin_mmst_reg_Typer;
extern DaoTypeBase *dao___darwin_xmm_reg_Typer;
extern DaoTypeBase *dao___darwin_i386_float_state_Typer;
extern DaoTypeBase *dao___darwin_i386_exception_state_Typer;
extern DaoTypeBase *dao___darwin_x86_debug_state32_Typer;
extern DaoTypeBase *dao___darwin_x86_thread_state64_Typer;
extern DaoTypeBase *dao___darwin_x86_float_state64_Typer;
extern DaoTypeBase *dao___darwin_x86_exception_state64_Typer;
extern DaoTypeBase *dao___darwin_x86_debug_state64_Typer;
extern DaoTypeBase *dao___darwin_mcontext32_Typer;
extern DaoTypeBase *dao___darwin_mcontext64_Typer;
extern DaoTypeBase *dao___darwin_sigaltstack_Typer;
extern DaoTypeBase *dao___darwin_ucontext_Typer;
extern DaoTypeBase *dao_sigval_Typer;
extern DaoTypeBase *dao_sigevent_Typer;
extern DaoTypeBase *dao___siginfo_Typer;
extern DaoTypeBase *dao___sigaction_u_Typer;
extern DaoTypeBase *dao___sigaction_Typer;
extern DaoTypeBase *dao_sigaction_Typer;
extern DaoTypeBase *dao_sigvec_Typer;
extern DaoTypeBase *dao_sigstack_Typer;
extern DaoTypeBase *dao_timeval_Typer;
extern DaoTypeBase *dao_rusage_Typer;
extern DaoTypeBase *dao_rlimit_Typer;
extern DaoTypeBase *dao_wait_Typer;
extern DaoTypeBase *dao_otto_Typer;
extern DaoTypeBase *dao_otto2_Typer;
extern DaoTypeBase *dao_Greeting_Typer;
extern DaoTypeBase *dao_Greeting_0_Null_Typer;
extern DaoTypeBase *dao_Greeting2_Typer;
extern DaoTypeBase *dao_AutobindTest_Typer;
extern DaoTypeBase *dao_CxxNS_0_Test_Typer;
#ifdef __cplusplus
}
#endif
class DAO_DLL_GREETING DaoCxxVirt_otto 
{
	public:
	DaoCxxVirt_otto(){ self = 0; cdata = 0; }
	void DaoInitWrapper( otto *self, DaoCdata *d );

	otto *self;
	DaoCdata *cdata;

	void vtest( int &_cs  );
	otto test( int &_cs, const otto& value );


};
class DAO_DLL_GREETING DaoCxx_otto : public otto, public DaoCxxVirt_otto
{ 

	public:
	DaoCxx_otto( int b=123 ) : otto( b ){}

	~DaoCxx_otto();
	void DaoInitWrapper();

	void vtest(  );
	otto test( const otto& value );
	otto DaoWrap_test( const otto& value ){ return otto::test( value ); }

};

DaoCxx_otto* DAO_DLL_GREETING DaoCxx_otto_New( int b );
class DAO_DLL_GREETING DaoCxxVirt_otto2  : public DaoCxxVirt_otto
{
	public:
	DaoCxxVirt_otto2(){ self = 0; cdata = 0; }
	void DaoInitWrapper( otto2 *self, DaoCdata *d );

	otto2 *self;
	DaoCdata *cdata;

	void vtest( int &_cs  );


};
class DAO_DLL_GREETING DaoCxx_otto2 : public otto2, public DaoCxxVirt_otto2
{ 

	public:

	~DaoCxx_otto2();
	void DaoInitWrapper();

	otto test( const otto& value );
	void vtest(  );

};

DaoCxx_otto2* DAO_DLL_GREETING DaoCxx_otto2_New(  );
class DAO_DLL_GREETING DaoCxxVirt_Greeting 
{
	public:
	DaoCxxVirt_Greeting(){ self = 0; cdata = 0; }
	void DaoInitWrapper( Greeting *self, DaoCdata *d );

	Greeting *self;
	DaoCdata *cdata;

	void DoGreeting( int &_cs, const char* name );
	void VirtWithDefault( int &_cs, const Greeting& g );


};
class DAO_DLL_GREETING DaoCxx_Greeting : public Greeting, public DaoCxxVirt_Greeting
{ 

	public:
	DaoCxx_Greeting( const char* msg=NULL ) : Greeting( msg ){}

	~DaoCxx_Greeting();
	void DaoInitWrapper();

	void DoGreeting( const char* name );
	void VirtWithDefault( const Greeting& g=Greeting() );

};

DaoCxx_Greeting* DAO_DLL_GREETING DaoCxx_Greeting_New( const char* msg );
Greeting::Null* DAO_DLL_GREETING Dao_Greeting_0_Null_New();
class DAO_DLL_GREETING DaoCxxVirt_Greeting2  : public DaoCxxVirt_Greeting
{
	public:
	DaoCxxVirt_Greeting2(){ self = 0; cdata = 0; }
	void DaoInitWrapper( Greeting2 *self, DaoCdata *d );

	Greeting2 *self;
	DaoCdata *cdata;



};
class DAO_DLL_GREETING DaoCxx_Greeting2 : public Greeting2, public DaoCxxVirt_Greeting2
{ 

	public:

	~DaoCxx_Greeting2();
	void DaoInitWrapper();

	void DoGreeting( const char* name );
	void VirtWithDefault( const Greeting& g=Greeting() );

};

DaoCxx_Greeting2* DAO_DLL_GREETING DaoCxx_Greeting2_New(  );
AutobindTest* DAO_DLL_GREETING Dao_AutobindTest_New();
CxxNS::Test* DAO_DLL_GREETING Dao_CxxNS_0_Test_New();
#ifdef __cplusplus
extern "C"{
#endif
#ifdef __cplusplus
}
#endif
#endif
