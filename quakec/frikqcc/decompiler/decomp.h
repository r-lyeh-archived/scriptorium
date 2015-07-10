/* ProQCC v1.60 FINAL
 * Copyright (C) 1997-2000 Lee Smith
 *
 * Based on qcc by John Carmack, id Software
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#ifndef _DECOMP_H
#define _DECOMP_H

void DecompileReadData(char *srcfile);
int DecompileGetFunctionIdxByName(char *name);
int DecompileAlreadySeen(char *fname);
void DecompileCalcProfiles(void);
char *DecompileGlobal(gofs_t ofs, def_t * req_t);
gofs_t DecompileScaleIndex(dfunction_t * df, gofs_t ofs);
char *DecompileImmediate(dfunction_t * df, gofs_t ofs, int fun, char *knew);
char *DecompileGet(dfunction_t * df, gofs_t ofs, def_t * req_t);
void DecompileIndent(int c);
void DecompileDecompileStatement(dfunction_t * df, dstatement_t * s, int *indent);
void DecompileDecompileFunction(dfunction_t * df);
char *DecompileString(char *string);
char *DecompileValueString(etype_t type, void *val);
char *DecompilePrintParameter(ddef_t * def);
ddef_t *GetField(char *name);
ddef_t *DecompileGetParameter(gofs_t ofs);
void DecompileFunction(char *name);
void DecompileDecompileFunctions(void);
void DecompileProgsDat(char *name);
char *DecompileGlobalStringNoContents(gofs_t ofs);
char *DecompileGlobalString(gofs_t ofs);
void DecompilePrintStatement(dstatement_t * s);
void DecompilePrintFunction(char *name);





#endif
