
README file for procplace Tiny Tcl 6.8

$Id: README.txt,v 1.1 2001/04/29 20:58:32 karll Exp $

Tiny Tcl 6.8 is a rommable, minimal Tcl for embedded applications.

Derived from the venerable Tcl 6.7 release, Tiny Tcl 6.8 has a solid Tcl
feature set, excluding newer capabilities of Tcl 7 and 8 such as the
bytecode compiler, namespaces, sockets, and async event handling, among others.

Still, major applications have been written in Tcl 6.

Excluding C library functions, Tiny Tcl compiles down to less than 60 Kbytes
on most machines, far smaller than any Tcl 7 or Tcl 8 derivatives.

This version of Tiny Tcl includes support for MS-DOS and DOS workalikes.
The code for interfacing to DOS can be found in the files that have DOS
in the filename, like tcldosaz.c, tcldosgl.c, tcldosut.c and dos.c.

COMPILER AND BUILD ENVIRONMENT

The compiler is assumed to be Borland C 5.0.  Earlier versions should work.
More recent Borland and Microsoft compilers can no longer create DOS 
executables, and are thus pretty much Windows-only tools.

All filenames in the Tiny Tcl source tree are DOS "8.3" compliant.

The build environment is assumed to be DOS or Windows.  You can run tiny
tcl in a DOS window under Windows while developing, if you like.

DOS INTERFACE FUNCTIONS

dos.c adds the following commands

    bios_memsize
    bios_equiplist
    com
    kbhit
    getkey
    sound
    rawclock
    getdate
    setdate
    gettime
    settime
    diskfree
    getfat
    getdfree
    drive
    memfree
    stackfree
    wait
    gotoxy
    cls
    getverify
    heapcheck
    mkdir
    unlink
    execvp
    video

To remove these, remove the call to Tcl_InitDos from tinytcl.c and
take dos.c out of the makefile and tlib.rsp file.


BUILDING GENERIC ONLY

If you build with TCL_GENERIC_ONLY defined, you'll get a Tcl without
any I/O commands -- open, close, gets, puts, etc.

This will generate the smallest possible executable.

TCLX FUNCTIONS

MEMORY DEBUGGING

Finding memory overwrites and related problems can be very tricky, even on
a modern protected mode operating system such as Unix/Linux.  It can be
particularly difficult in an embedded environment.  Compiling Tiny Tcl
with TCL_MEM_DEBUG defined will greatly improve Tcl's chances of finding
any memory overwrite problems that your C extensions might use, at some
cost in performance.

Note also that memory debugging adds significant memory overhead as well.
On a 640K target system application that was close to the memory limit, we
found that turning off memory debugging freed about 120K of RAM... a major
impact.

TRACING TCL EXECUTION

TclX's cmdtrace command is included.  You can trace execution by executing
"cmdtrace on" and stop it with "cmdtrace off"

STACK SIZE

In typical embedded applications, there is no bounds protection to insure
that the stack is not overflowed.  You can usually turn on overflow checking
in your compiler.  That is not a complete solution because usually the overflow
is determined after the fact.

The file borland.c defines the stack size for the Borland compiler.  It
is set by default to 65,400 bytes, which is the highest we could set it to.
This gives you the maximum Tcl execution depth... We find that each call of
one Tcl routine from another typically adds about 1100 bytes on an Intel 8086
target.  If you are extrememly memory limited and are not using most of the
stack, you can lower the number defined in borland.c.

Note that the default amount of stack, 4K, is barely enough to get Tcl off the
ground, with little or no application execution capability.

If you have mystery crashes, there's a good chance you've got the stack size
set too small.

RUNTIME ENVIRONMENT

The startup code is in tinytcl.c.  It is a char array called initCmd.

By default this is defined as

char initCmd[] =
    "puts stdout \"\nEmbedded Tcl 6.8.0\n\"; source tcl_sys/autoinit.tcl";

You can replace it with whatever Tcl initialization you want to occur for
your application.  Note that in the tcl_sys directory are files devel.tcl,
containing a few handy procs to have around while compiling, and system.tcl,
an undocumented file that contains procs to beep the speaker and make a list
of currently defined procs and globals (snapshot) and then revert to the
snapshot by unloading all procs and globals defined after snapshot was run.
