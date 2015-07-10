#
# TTC JC2 Handheld Computer Project
#
# Makefile for Borland 5 C++ compiler
#
# $Id: Makefile,v 1.1.1.1 2001/04/29 20:34:04 karll Exp $
#
# Target is an 8086-compatible processor.
#
# -Y - generate code for use in overlays
# -N - check for stack overflows
# -Td - target for a DOS executable
# -O1 - generate smallest possible code
# -O2 - generate fastest possible code
#       NOTE: weird problem with -O2 -- colors change in DOS window when
#             running program.  I don't think -O2 is reliable.
# -DTCL_MEM_DEBUG = enable memory debugging
# -v include symbol table
# -1- - target is an 8086 or compatible
#
# -ml - large memory model
# -c - generate OBJ file - don't link
#-Oa -Ob -Oc -Oe -Og -Oi -Ol -Om -Ov

# -Oe -Og -Oi - these optimizations cause video problems.
#bcc -O -N -Td -D__STDC__ -1- -ml -c -I/gcl520/h $<

.c.obj:
	bcc -O -Oa -Ob -Oc -Ol -Om -Ov -N -Td -D__STDC__ -1- -ml -c -I/gcl520/h $<


#	bcc -v -N -Td -D__STDC__ -DTCL_MEM_DEBUG -1- -ml -c -I/gcl520/h $<

#	bcc -Td -D__STDC__ -1- -ml -c -I/gcl520/h $<

#	bcc -Td -DTCL_GENERIC_ONLY -D__STDC__ -1- -ml -c $<

# TCLUNXAZ.OBJ TCLEMBED.OBJ TCLUXUTL.OBJ TCLGLOB.OBJ TCLUXSTR.OBJ

OBJS= TCLGET.OBJ TCLPROC.OBJ TCLVAR.OBJ TCLASSEM.OBJ TCLCMDAH.OBJ TCLCMDMZ.OBJ TCLHASH.OBJ TCLPARSE.OBJ PANIC.OBJ REGEXP.OBJ TCLCMDIL.OBJ TCLALLOC.OBJ TCLBASIC.OBJ TCLEXPR.OBJ TCLUTIL.OBJ TCLENV.OBJ TINYTCL.OBJ TCLDOSAZ.OBJ TCLDOSUT.OBJ TCLDOSST.OBJ TCLDOSGL.OBJ TCLXDBG.OBJ TCLXGEN.OBJ BORLAND.OBJ DOS.OBJ READDIR.OBJ

all:	tcl.exe

tinytcl.lib:	$(OBJS)
	del tinytcl.lib
	tlib tinytcl @tlib.rsp
#
# linker switches
# -tD - compile to DOS exe
#
#
tcl.exe:	tinytcl.lib
	tlink /v+ /Tde/n c0l,tcl,tinytcl,tinytcl mathl cl emu

# fp87 = use floating point coprocessor,
# emu = use emulation

clean:
	del *.obj
	del tinytcl.lib
	del *.map
	del tcl.exe

