/* 
 * panic.c --
 *
 *	Source code for the "panic" library procedure for Tcl;
 *	individual applications will probably override this with
 *	an application-specific panic procedure.
 *
 * Copyright 1988-1991 Regents of the University of California
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appears in all copies.  The University of California
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 *
 * $Id: panic.c,v 1.1.1.1 2001/04/29 20:34:05 karll Exp $
 */

#include <stdio.h>
#include <stdlib.h>

/*
 *----------------------------------------------------------------------
 *
 * panic --
 *
 *	Print an error message and kill the process.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The process dies, entering the debugger if possible.
 *
 *----------------------------------------------------------------------
 */

	/* VARARGS ARGSUSED */
void
panic(format, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
    char *format;		/* Format string, suitable for passing to
				 * fprintf. */
    char *arg1, *arg2, *arg3;	/* Additional arguments (variable in number)
				 * to pass to fprintf. */
    char *arg4, *arg5, *arg6, *arg7, *arg8;
{
    (void) fprintf(stderr, format, arg1, arg2, arg3, arg4, arg5, arg6,
	    arg7, arg8);
    (void) fflush(stderr);
    abort();
}
