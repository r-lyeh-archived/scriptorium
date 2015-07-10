/*
 * Stuff specific to the Borland C compiler.
 *
 * $Id: borland.c,v 1.1.1.1 2001/04/29 20:34:04 karll Exp $
 *
 */

/* _stklen defines how big the runtime stack is.  It must be set at
 * compile time as an assignment.
 */

extern unsigned _stklen = 65400U;
