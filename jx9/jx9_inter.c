/*
 * Compile this file together with the jx9 engine source code to generate
 * the simple JX9 interpreter executable. For example: 
 *  gcc -W -Wall -O6 -D JX9_ENABLE_MATH_FUNC -o jx9 jx9_interp.c jx9.c
 */
/*
 * The JX9 interpreter is a simple standalone JX9 interpreter that allows
 * the user to enter and execute JX9 files against a JX9 engine. 
 * To start the jx9 program, just type "jx9" followed by the name of the JX9 file
 * to compile and execute. That is, the first argument is to the interpreter, the rest
 * are scripts arguments, press "Enter" and the JX9 code will be executed.
 * If something goes wrong while processing the JX9 script due to a compile-time error
 * your error output (STDOUT) should display the compile-time error messages.
 *
 * Usage example of the jx9 interpreter:
 *   jx9 scripts/hello_world.jx9.txt
 * Running the interpreter with script arguments
 *    jx9 scripts/mp3_tag.jx9.txt /usr/local/path/to/my_mp3s
 *
 *
 * For an introduction to the JX9 C/C++ interface, please refer to this page
 *        http://jx9.symisc.net/api_intro.html
 * For the full C/C++ API reference guide, please refer to this page
 *        http://jx9.symisc.net/c_api.html
 * For the full JX9 language reference manual, please refer to this page
 *        http://jx9.symisc.net/jx9_lang.html
 */

/* $SymiscID: jx9_interp.c v1.7.4 Win7 2012-12-06 13:22 stable <devel@symisc.net> $ */

/* Make sure you have the latest release of the JX9 engine
 * from:
 *  http://jx9.symisc.net/downloads.html
 */
#include <stdio.h>
#include <stdlib.h>
/* Make sure this header file is available.*/
#include "jx9.h"
/* 
 * Display an error message and exit.
 */
static void Fatal(const char *zMsg)
{
	if( zMsg ){
		puts(zMsg);
	}
	/* Shutdown the library */
	jx9_lib_shutdown();
	/* Exit immediately */
	exit(0);
}
/*
 * Banner.
 */
static const char zBanner[] = {
	"============================================================\n"
	"Simple JX9 Interpreter                                      \n"
	"                                      http://jx9.symisc.net/\n"
	"============================================================\n"
};
/*
 * Display the banner, a help message and exit.
 */
static void Help(void)
{
	puts(zBanner);
	puts("jx9 [-h|-r|-d] path/to/jx9.txt_file [script args]");
	puts("\t-d: Dump JX9 bytecode instructions");
	puts("\t-r: Report run-time errors");
	puts("\t-h: Display this message an exit");
	/* Exit immediately */
	exit(0);
}
#ifdef __WINNT__
#include <Windows.h>
#else
/* Assume UNIX */
#include <unistd.h>
#endif
/*
 * The following define is used by the UNIX built and have
 * no particular meaning on windows.
 */
#ifndef STDOUT_FILENO
#define STDOUT_FILENO	1
#endif
/*
 * VM output consumer callback.
 * Each time the virtual machine generates some outputs, the following
 * function gets called by the underlying virtual machine to consume
 * the generated output.
 * All this function does is redirecting the VM output to STDOUT.
 * This function is registered later via a call to jx9_vm_config()
 * with a configuration verb set to: JX9_VM_CONFIG_OUTPUT.
 */
static int Output_Consumer(const void *pOutput, unsigned int nOutputLen, void *pUserData /* Unused */)
{
#ifdef __WINNT__
	BOOL rc;
	rc = WriteFile(GetStdHandle(STD_OUTPUT_HANDLE), pOutput, (DWORD)nOutputLen, 0, 0);
	if( !rc ){
		/* Abort processing */
		return JX9_ABORT;
	}
#else
	ssize_t nWr;
	nWr = write(STDOUT_FILENO, pOutput, nOutputLen);
	if( nWr < 0 ){
		/* Abort processing */
		return JX9_ABORT;
	}
#endif /* __WINT__ */
	/* All done, VM output was redirected to STDOUT */
	return JX9_OK;
}
/*
 * Main program: Compile and execute the given JX9 script. 
 */
int main(int argc, char **argv)
{
	jx9 *pEngine; /* JX9 engine handle */
	jx9_vm *pVm;  /* Compiled JX9 program */
	int dump_vm = 0;    /* Dump VM instructions if TRUE */
	int err_report = 0; /* Report run-time errors if TRUE */
	int n;              /* Script arguments */
	int rc;
	/* Process interpreter arguments first*/
	for(n = 1 ; n < argc ; ++n ){
		int c;
		if( argv[n][0] != '-' ){
			/* No more interpreter arguments */
			break;
		}
		c = argv[n][1];
		if( c == 'd' || c == 'D' ){
			/* Dump bytecode instructions */
			dump_vm = 1;
		}else if( c == 'r' || c == 'R' ){
			/* Report run-time errors */
			err_report = 1;
		}else{
			/* Display a help message and exit */
			Help();
		}
	}
	if( n >= argc ){
		puts("Missing JX9 file to compile");
		Help();
	}

	/* Allocate a new JX9 engine instance */
	rc = jx9_init(&pEngine);
	if( rc != JX9_OK ){
		/*
		 * If the supplied memory subsystem is so sick that we are unable
		 * to allocate a tiny chunk of memory, there is no much we can do here.
		 */
		Fatal("Error while allocating a new JX9 engine instance");
	}

	/* Now, it's time to compile our JX9 script */
	rc = jx9_compile_file(
		pEngine, /* JX9 Engine */
		argv[n], /* Path to the JX9 file to compile */
		&pVm    /* OUT: Compiled JX9 program */
		);
	if( rc != JX9_OK ){ /* Compile error */
		if( rc == JX9_IO_ERR ){
			Fatal("IO error while opening the target file");
		}else if( rc == JX9_VM_ERR ){
			Fatal("VM initialization error");
		}else{
			const char *zErrLog;
			int nLen;
			/* Extract error log */
			jx9_config(pEngine, 
				JX9_CONFIG_ERR_LOG, 
				&zErrLog, 
				&nLen
				);
			if( nLen > 0 ){
				/* zErrLog is null terminated */
				puts(zErrLog);
			}
			/* Compile-time error, your output (STDOUT) should display the error messages */
			Fatal(0);
		}
	}
	
	/*
	 * Now we have our script compiled, it's time to configure our VM.
	 * We will install the VM output consumer callback defined above
	 * so that we can consume the VM output and redirect it to STDOUT.
	 */
	rc = jx9_vm_config(pVm, 
		JX9_VM_CONFIG_OUTPUT, 
		Output_Consumer,    /* Output Consumer callback */
		0                   /* Callback private data */
		);
	if( rc != JX9_OK ){
		Fatal("Error while installing the VM output consumer callback");
	}
	
	
	/* Register script agruments so we can access them later using the $argv[]
	 * array from the compiled JX9 program.
	 */
	for( n = n + 1; n < argc ; ++n ){
		jx9_vm_config(pVm, JX9_VM_CONFIG_ARGV_ENTRY, argv[n]/* Argument value */);
	}

	if( err_report ){
		/* Report script run-time errors */
		jx9_vm_config(pVm, JX9_VM_CONFIG_ERR_REPORT);
	}
	if( dump_vm ){
		/* Dump JX9 byte-code instructions */
		jx9_vm_dump_v2(pVm, 
			Output_Consumer, /* Dump consumer callback */
			0
			);
	}

	/*
	 * And finally, execute our program. Note that your output (STDOUT in our case)
	 * should display the result.
	 */
	jx9_vm_exec(pVm, 0);

	/* All done, cleanup the mess left behind.
	*/
	jx9_vm_release(pVm);
	jx9_release(pEngine);

	return 0;
}