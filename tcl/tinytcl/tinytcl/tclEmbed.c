/* 
 * tclEmbed.c --
 *
 *	Test driver for TCL.
 *
 * Copyright 1987-1991 Regents of the University of California
 * All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appears in all copies.  The University of California
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 *
 * $Id: tclEmbed.c,v 1.1.1.1 2001/04/29 20:34:35 karll Exp $
 */

#include <termios.h>
#include <unistd.h>
#include <fcntl.h>

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include "tcl.h"

Tcl_Interp *interp;
char dumpFile[100];
int quitFlag = 0;

char initCmd[] =
    "echo \"procplace.com embedded tcl 6.7\"; source drongo.tcl";

struct termios saved_tio;


ttsetup()
{
    struct termios tio;

    if (fcntl (0, F_SETFL, O_NONBLOCK) < 0) {
        perror ("stdin fcntl");
    }

    tcgetattr (0, &tio);
    saved_tio = tio;
    tio.c_lflag &= ~(ECHO|ECHONL|ICANON|IEXTEN);
    tcsetattr (0, TCSANOW, &tio);
}

ttteardown()
{
    tcsetattr (0, TCSANOW, &saved_tio);
}

	/* ARGSUSED */
int
cmdGetkey(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char *argv[];
{
    char c;

    if (read (0, &c, 1) < 0) {
        return TCL_OK;
    }

    sprintf(interp->result, "%d", c);
    return TCL_OK;
}

	/* ARGSUSED */
int
cmdPause(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char *argv[];
{
    usleep (50000);
    return TCL_OK;
}

	/* ARGSUSED */
int
cmdEcho(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char *argv[];
{
    int i;

    for (i = 1; ; i++) {
	if (argv[i] == NULL) {
	    if (i != argc) {
		echoError:
		sprintf(interp->result,
		    "argument list wasn't properly NULL-terminated in \"%s\" command",
		    argv[0]);
	    }
	    break;
	}
	if (i >= argc) {
	    goto echoError;
	}
	fputs(argv[i], stdout);
	if (i < (argc-1)) {
	    printf(" ");
	}
    }
    printf("\n");
    return TCL_OK;
}

setup()
{
    ttsetup();
}

teardown()
{
    ttteardown();
}
	/* ARGSUSED */
int
main()
{
    char *result;

    setup();

    interp = Tcl_CreateInterp();
#ifdef TCL_MEM_DEBUG
    Tcl_InitMemory(interp);
#endif
    Tcl_CreateCommand(interp, "echo", cmdEcho, (ClientData) "echo",
	    (Tcl_CmdDeleteProc *) NULL);

    Tcl_CreateCommand(interp, "getkey", cmdGetkey, (ClientData) "getkey",
	    (Tcl_CmdDeleteProc *) NULL);

    Tcl_CreateCommand(interp, "pause", cmdPause, (ClientData) "pause",
	    (Tcl_CmdDeleteProc *) NULL);
/*
    Tcl_CreateCommand(interp, "checkmem", cmdCheckmem, (ClientData) 0,
	    (Tcl_CmdDeleteProc *) NULL);
*/
    result = Tcl_Eval(interp, initCmd, 0, (char **) NULL);
    if (result != TCL_OK) {
	printf("%s\n", interp->result);
	teardown();
	exit(1);
    }

    teardown();
    exit(0);
}

