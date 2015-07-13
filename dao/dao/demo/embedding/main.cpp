
#include<stdio.h>
#include"greeting.h"

// If you want to use direct APIs of Dao, define the following
// preprocessor option before EVERY including of dao.h.
// Or add it in your Makefile or project compiling settings.
//#define DAO_DIRECT_API
#include<dao.h>

// The wrapping of C++ class Greeting is done by the autobind.dao tool.
// The wrapped types and functions must be imported into a Dao namespace
// by calling the entry function for the wrapping.
//
// The entry function is defined as plain C funtion, so it must be declared
// as plain C function here:
#ifdef __cplusplus
extern "C"{
#endif
extern int DaoOnLoad( DaoVmSpace *vms, DaoNamespace *ns );
#ifdef __cplusplus
}
#endif

const char* dao_source = 
//"load Cpp;\n"
"obj = GetGreetingObject()\n"
"obj.PrintMessage()\n"
"obj.SetMessage( \"Hello, from Dao, Dao rocks!\" )\n"
"\n"
"\n"
"class DaoGreeting : Greeting\n"
"{\n"
"\n"
"\n" // The constructor must invoke a parent constructor.
"  sub DaoGreeting() : Greeting( 'hi' ){}\n"
"\n"
"  sub DoGreeting( name : string ){\n"
"    PrintMessage()\n"
"    io.writef( 'Dao: hi %s!\n', name )\n"
"  }\n"
"}\n"
"\n"
"obj2 = DaoGreeting()\n"
"obj2.DoGreeting( 'bob' );\n"
"((Greeting)obj2).DoGreeting( 'bob' )\n"
"\n"
"\n"  // Check if the re-implemented virtual method will be called. 
"obj.TestGreeting( obj2, 'alice' )\n"
//"load test;"
"io.writeln('In script test.dao')\n"
"class DaoOtto : otto\n"
"{\n"
"	sub doOtto(){\n"
"		test( self );\n"
"		io.writef( 'Dao: in doOtto() a=%d\n', geta())\n"
"	}\n"
"}\n"
"obj5 = DaoOtto()\n"
"obj5.doOtto()\n"
"io.writeln('END script test.dao')\n"
"ot = otto();\n"
"ot.test(ot); #error \n"
;

const char* dao_source2 = 
//"load Cpp;\n"
"obj = CxxNS::Test();\n"
"obj.index = 1;\n"
"obj.value = 1.2;\n"
"obj.Print();\n"
"obj = Test2();\n"
"obj.index = 2;\n"
"obj.value = 3.4;\n"
"obj.Print();\n"
"Testing();\n"
"Testing( CxxNS::TRUE );\n"
"CxxNS::Testing(0);\n"
"CxxNS::Testing(obj);\n"
"CxxNS2::Testing(obj);\n"
;

const char* dao_source3 = 
"function getSomeString(){ return 'StringFromDao' }";

int main( int argc, char *argv[] )
{
	DString *src;
	DaoVmSpace *vms;
	DaoNamespace *ns;
	DaoNamespace *ns2;
	DaoProcess *vmp;

	// Search and load the Dao library.
	// DaoInitLibrary() can take a parameter which is the path
	// to the dynamic loading file of the Dao library.
	// If the parameter is NULL, the current path is searched,
	// then the path defined by environment variable DAO_DIR,
	// then $(HOME)/dao, and then the default system path:
	// /usr/local/dao/ or C:\dao\.
	//
	// With direct APIs, the example must be linked against the Dao library.
	// So if direct APIs are used, the following call is not necessary.
#ifndef DAO_DIRECT_API
	if( DaoInitLibrary( NULL ) ==0 ) return 1;
#endif

	// Initialize Dao library, and get the default DaoVmSpace object.
	// DaoVmSpace is responsible for handling interpreter settings,
	// paths and module loading etc. It is need to create several
	// other types of objects.
	vms = DaoInit();

	// Get the main namespace of an DaoVmSpace object.
	// You can also call DaoNamespace_New( vms ) to create one.
	ns  = DaoVmSpace_MainNamespace( vms );
	ns2  = DaoVmSpace_GetNamespace( vms, "dao" );

	// Get the main virtual machine process of an DaoVmSpace object.
	// You can also call DaoProcess_New( vms ) to create one.
	vmp = DaoVmSpace_MainProcess( vms );

	// Call the entry function to import the type wrapping Greeting
	// into the namespace ns.
	//
	// Calling to this function is not necessary, if and only if
	// the wrapping codes are compiled as dynamic loading library,
	// and there is a proper load statement in the Dao codes.
	//
	// Here the wrapping codes are compiled together with this
	// example, so this entry function must be called:
	DaoOnLoad( vms, ns2 );

	// Prepare the Dao source codes:
	src = DString_New(1);
	DString_SetMBS( src, dao_source );

	// Execute the Dao scripts:
	// Since the wrapped functions and types are imported into
	// namespace ns, it is need to access the wrapped functions and types
	// in the Dao scripts when it is executed:
	DaoProcess_Eval( vmp, ns, src, 1 );

	DString_SetMBS( src, dao_source2 );
	DaoProcess_Eval( vmp, ns, src, 1 );

	// Check if the Dao scripts have indeed modified the C++ object.
	Greeting *obj = GetGreetingObject();
	obj->PrintMessage();

	DString_SetMBS( src, dao_source3 );
	DaoProcess_Eval( vmp, ns, src, 1 );

	DaoValue *value = DaoNamespace_FindData( ns, "getSomeString" );
	if( DaoValue_CastRoutine( value ) ){
		DaoProcess_Call( vmp, (DaoMethod*)value, NULL, NULL, 0 );
	}
	value = DaoProcess_GetReturned( vmp );
	printf( "%i %s\n", DaoValue_Type( value ), DaoString_GetMBS(  (DaoString*) value ) );

	DString_Delete( src );
	DaoQuit(); // Finalizing
	return 0;
}
