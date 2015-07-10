/*
 * tclXfilecmds.c
 *
 * Extended Tcl readdir command.
 *-----------------------------------------------------------------------------
 * Copyright 1991-1994 Karl Lehenbauer and Mark Diekhans.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies.  Karl Lehenbauer and
 * Mark Diekhans make no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without express or
 * implied warranty.
 *-----------------------------------------------------------------------------
 * $Id: readdir.c,v 1.1.1.1 2001/04/29 20:35:50 karll Exp $
 *-----------------------------------------------------------------------------
 */

#include "tclExtdInt.h"
#include "tclDos.h"


/*
 *-----------------------------------------------------------------------------
 *
 * Tcl_ReaddirCmd --
 *     Implements the rename TCL command:
 *         readdir dirPath
 *
 * Results:
 *      Standard TCL result.
 *-----------------------------------------------------------------------------
 */
int
Tcl_ReaddirCmd (clientData, interp, argc, argv)
    ClientData  clientData;
    Tcl_Interp *interp;
    int         argc;
    char      **argv;
{
    char          *dirPath;
    DIR           *dirPtr;
    struct dirent *entryPtr;

    if (argc != 2) {
        Tcl_AppendResult (interp, "bad # args: ", argv [0], 
                          " dirPath", (char *) NULL);
        return TCL_ERROR;
    }

    dirPath = argv[1];

    dirPtr = opendir (dirPath);
    if (dirPtr == NULL)  {
        Tcl_AppendResult (interp, dirPath, ": ", Tcl_UnixError (interp),
                          (char *) NULL);
        goto errorExit;
    }

    while (TRUE) {
        entryPtr = readdir (dirPtr);
        if (entryPtr == NULL)
            break;
        if (entryPtr->d_name [0] == '.') {
            if (entryPtr->d_name [1] == '\0')
                continue;
            if ((entryPtr->d_name [1] == '.') &&
                (entryPtr->d_name [2] == '\0'))
                continue;
        }
        Tcl_AppendElement (interp, entryPtr->d_name, 0);
    }
    closedir (dirPtr);
    return TCL_OK;

  errorExit:
    return TCL_ERROR;
}

int
Tcl_InitReaddir(Tcl_Interp *interp)
{
    Tcl_CreateCommand(interp, "readdir", Tcl_ReaddirCmd, (ClientData) NULL,
	    (Tcl_CmdDeleteProc *) NULL);

    return TCL_OK;
}


