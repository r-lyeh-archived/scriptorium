#include "qcc.h"
#include "hash.h"

#include <time.h>
#include <limits.h>


#ifndef O_BINARY
#define O_BINARY 0
#endif

// FrikaC added
boolean conditional;
boolean dontcomplain;
int warninglevel;

char		destfile[1024];
float		pr_globals[MAX_REGS];
int			pr_global_refs[MAX_REGS]; // FrikaC

int			numpr_globals;

char		strings[MAX_STRINGS];
int			strofs;

dstatement_t	statements[MAX_STATEMENTS];
int			numstatements;
int			statement_linenums[MAX_STATEMENTS];

dfunction_t	functions[MAX_FUNCTIONS];
int			numfunctions;

ddef_t		globals[MAX_GLOBALS];
int			numglobaldefs;

ddef_t		fields[MAX_FIELDS];
int			numfielddefs;

char		precache_sounds[MAX_SOUNDS][MAX_DATA_PATH];
int			precache_sounds_block[MAX_SOUNDS];
int			numsounds;

char		precache_models[MAX_MODELS][MAX_DATA_PATH];
int			precache_models_block[MAX_SOUNDS];
int			nummodels;

char		precache_files[MAX_FILES][MAX_DATA_PATH];
int			precache_files_block[MAX_SOUNDS];
int			numfiles;

char	sourcedir[1024];
char includedir[1024]; // you're gonna hate me for this

boolean summary;

int pr_optimize_eliminate_temps;
int pr_optimize_shorten_ifs;
int pr_optimize_nonvec_parms;
int pr_optimize_constant_names;
int pr_optimize_defs;
int pr_optimize_hash_strings;
int pr_optimize_locals;
int pr_optimize_function_names;
int pr_optimize_filenames;
int pr_optimize_unreferenced;
int pr_optimize_logicops;
int pr_optimize_recycle;
int pr_optimize_constant_arithmetic;

int num_constant_names;
int num_stores_shortened;
int num_ifs_shortened;
int num_nonvec_parms;
int num_defs;
int num_strings;
int num_locals_saved;
int num_funcs_saved;
int num_files_saved;
int num_unreferenced;
int num_logic_jumps;
int num_recycled;
int num_constant_ops_saved;

boolean pr_pause; 


void PR_Expect (char *string, token_type_t type)
{
	if (STRCMP(string, pr_token))
		PR_ParseError (550, "Expected %s, found %s",string, pr_token);
	if (type)
		if (pr_token_type != type)
			PR_ParseError (550, "Expected %s, found %s",string, pr_token);
	PR_Lex ();
} 

boolean PR_Check (char *string, token_type_t type)
{
	if (STRCMP(string, pr_token))
		return false;
	if (type)
		if (pr_token_type != type)
			return false;

	PR_Lex ();
	return true;
}

// CopyString returns an offset from the string heap
int	CopyString (char *str, int len)
{
	int		old;
	def_t	*cn = NULL;
	struct	hash_element *cell = NULL;
	int allocate;
	int index = 0;
	
	allocate = 0;
	if (!len)
	{
		// this is not a string immediate (it's a filename/variable name/function name)
		len = strlen(str)+1;
		if (pr_optimize_hash_strings)
		{
			allocate = 1;
			index = hash(str);
						
			for (cell = htable[index]; cell != NULL; cell = cell->next) 
			{
				cn = cell->def;
				if (cn->type)
					continue;
				if (!strcmp(strings+cn->ofs, str))
				{
					num_strings += len;
					return cn->ofs;
				}
			}
		}
	}
	old = strofs;
	memcpy (strings+strofs, str, len);
	strofs += len;

	if (allocate)
	{
		def_t *def = (struct def_s *)PR_Malloc (sizeof(def_t)); // 
		def->ofs = old;
		def->type = NULL;      // hack used to indicate def allocated here
		def->constant = 0;
		def->name = "";
		def->scope = NULL;
		cell = (struct hash_element *) PR_Malloc (sizeof(struct hash_element));
		cell->next = htable[index];
		cell->def = def;
		htable[index] = cell;
		stats[index]++;
	}
	return old;
}

void PrintStrings (void)
{
	int		i, l, j;
	printf("--- strings ---\n");
	for (i=0 ; i<strofs ; i += l)
	{
		l = strlen(strings+i) + 1;
		printf ("%5i : ",i);
		for (j=0 ; j<l ; j++)
		{
			if (strings[i+j] == '\n')
			{
				putchar ('\\');
				putchar ('n');
			}
			else
				putchar (strings[i+j]);
		}
		printf ("\n");
	}
}


void PrintFunctions (void)
{
	int		i,j;
	dfunction_t	*d;
	printf("--- functions ---\n");
	for (i=0 ; i<numfunctions ; i++)
	{
		d = &functions[i];
		printf ("%s : %s : %i %i (", strings + d->s_file, strings + d->s_name, d->first_statement, d->parm_start);
		for (j=0 ; j<d->numparms ; j++)
			printf ("%i ",d->parm_size[j]);
		printf (")\n");
	}
}

void PrintFields (void)
{
	int		i;
	ddef_t	*d;
	printf("--- fields ---\n");
	for (i=0 ; i<numfielddefs ; i++)
	{
		d = &fields[i];
		printf ("%5i : (%i) %s\n", d->ofs, d->type, strings + d->s_name);
	}
}

void PrintGlobals (void)
{
	int		i;
	ddef_t	*d;
		printf("\n--- globals ---\n");
	for (i=0 ; i<numglobaldefs ; i++)
	{
		d = &globals[i];
		printf ("%5i : (%i) %s\n", d->ofs, d->type, strings + d->s_name);
	}
}


void InitData (void)
{
	int		i;
	
	numstatements = 1;
	strofs = 1;
	numfunctions = 1;
	numglobaldefs = 1;
	numfielddefs = 1;
	
	def_ret.ofs = OFS_RETURN;
	for (i=0 ; i<MAX_PARMS ; i++)
		def_parms[i].ofs = OFS_PARM0 + 3*i;
}

extern time_t StartTime;

void WriteData (int crc)
{
	def_t		*def;
	ddef_t		*dd;
	dprograms_t	progs;
	char tname[1024];
	int			h;
	unsigned int			i;
	int size;

	for (def = pr.def_head.next ; def ; def = def->next)
	{
		if ((def->type->type == ev_field) && def->constant)
		{
			dd = &fields[numfielddefs];
			numfielddefs++;
			dd->type = def->type->aux_type->type == ev_int ? ev_float : def->type->aux_type->type;
			if (def->save == 0)
			{
					strcpy(tname, def->name);
					strcat(tname, "__");
					dd->s_name = CopyString(tname, 0);
			}
			else 
				dd->s_name = CopyString (def->name, 0);
			dd->ofs = G_INT(def->ofs);
		}
		else if (pr_optimize_constant_names && def->constant && (def->type->type != ev_function))
		{
			num_constant_names += strlen(def->name) + 1;
			num_constant_names += sizeof(ddef_t);
			continue;
		}
		else if (pr_optimize_unreferenced && pr_global_refs[def->ofs] <= 0)
		{
			if (!(def->type->type != ev_function))
			{
				num_unreferenced += 1;
				continue;
			}
		}
		else if (pr_optimize_unreferenced && def->type->type == ev_vector)
		{

			if (pr_global_refs[def->ofs] + pr_global_refs[def->ofs + 1] + pr_global_refs[def->ofs +1] == 3)
			{
				num_unreferenced += 3;
				def = def->next; // def_x
				def = def->next; // def_y
				def = def->next; // def_z
				continue;
			}
		}
		dd = &globals[numglobaldefs];
		dd->type = def->type->type == ev_int ? ev_float : def->type->type;
		if (def->save && ( dd->type != ev_field || def->constant != 1))
			dd->type |= DEF_SAVEGLOBAL;
		if (def->name)
		{
			if (pr_optimize_locals && (def->scope || !(STRCMP(def->name, "IMMEDIATE"))))
			{
				num_locals_saved += strlen(def->name);
				dd->s_name = 0;
			}
			else
				dd->s_name = CopyString (def->name, 0);
		}

		dd->ofs = def->ofs;
		numglobaldefs++;
	}
	strofs = (strofs+3)&~3;
	if (strofs > INT_MAX)
		PR_ParseWarning(122, "strofs exceeds INT_MAX by %i", strofs - INT_MAX);
	if (numstatements > INT_MAX)
		PR_ParseWarning(123, "numstatements exceeds INT_MAX by %i", numstatements - INT_MAX);
	if (numfunctions > SHRT_MAX)
		PR_ParseWarning(124, "numfunctions exceeds SHRT_MAX by %i", numfunctions - SHRT_MAX);
	if (numglobaldefs > SHRT_MAX)
		PR_ParseWarning(125, "numglobaldefs exceeds SHRT_MAX by %i", numglobaldefs - SHRT_MAX);
	if (numfielddefs > SHRT_MAX)
		PR_ParseWarning(126, "numfielddefs exceeds SHRT_MAX by %i", numfielddefs - SHRT_MAX);
	if (numpr_globals > SHRT_MAX)
		PR_ParseWarning(127, "numpr_globals exceeds SHRT_MAX by %i", numpr_globals - SHRT_MAX);
	if (crc != NQ_PROGHEADER_CRC && crc != QW_PROGHEADER_CRC)
		PR_ParseWarning(208, "System defs do match internal crcs.");
	if (summary)
	{

		summary_print("----------- Summary -----------\n");

		i = I_FloatTime() - StartTime;
		summary_print (" %02i:%02i elapsed time\n", (i / 60) % 59, i % 59);
		summary_print ("%6i strofs         (MAX: %6i)\n", strofs, MAX_STRINGS	);
		summary_print ("%6i numstatements  (MAX: %6i)\n", numstatements, MAX_STATEMENTS);
		summary_print ("%6i numfunctions   (MAX: %6i)\n", numfunctions, SHRT_MAX);
		summary_print ("%6i numglobaldefs  (MAX: %6i)\n", numglobaldefs, SHRT_MAX);
		summary_print ("%6i numfielddefs   (MAX: %6i)\n", numfielddefs, SHRT_MAX);
		summary_print ("%6i numpr_globals  (MAX: %6i)\n", numpr_globals, SHRT_MAX);
	}
	h = SafeOpenWrite (destfile);
	SafeWrite (h, &progs, sizeof(progs));

	progs.ofs_strings = lseek (h, 0, SEEK_CUR);
	progs.numstrings = strofs;
	SafeWrite (h, strings, strofs);

	progs.ofs_statements = lseek (h, 0, SEEK_CUR);
	progs.numstatements = numstatements;
	for (i=0 ; i<numstatements ; i++)
	{
		statements[i].op = LittleShort(statements[i].op);
		statements[i].a = LittleShort(statements[i].a);
		statements[i].b = LittleShort(statements[i].b);
		statements[i].c = LittleShort(statements[i].c);
	}
	SafeWrite (h, statements, numstatements*sizeof(dstatement_t));

	progs.ofs_functions = lseek (h, 0, SEEK_CUR);
	progs.numfunctions = numfunctions;
	for (i=0 ; i<numfunctions ; i++)
	{
		functions[i].first_statement = LittleLong (functions[i].first_statement);
		functions[i].parm_start = LittleLong (functions[i].parm_start);
		functions[i].s_name = LittleLong (functions[i].s_name < 0 || functions[i].s_name > strofs ? 0 : functions[i].s_name);
		functions[i].s_file = LittleLong (functions[i].s_file < 0 || functions[i].s_file > strofs ? 0 : functions[i].s_file);
		functions[i].numparms = LittleLong (functions[i].numparms > MAX_PARMS ? MAX_PARMS : functions[i].numparms);
		functions[i].locals = LittleLong (functions[i].locals);
	}	
	SafeWrite (h, functions, numfunctions*sizeof(dfunction_t));

	progs.ofs_globaldefs = lseek (h, 0, SEEK_CUR);
	progs.numglobaldefs = numglobaldefs;
	for (i=0 ; i<numglobaldefs ; i++)
	{
		globals[i].type = LittleShort (globals[i].type);
		globals[i].ofs = LittleShort (globals[i].ofs);
		globals[i].s_name = LittleLong (globals[i].s_name);
	}
	SafeWrite (h, globals, numglobaldefs*sizeof(ddef_t));

	progs.ofs_fielddefs = lseek (h, 0, SEEK_CUR);
	progs.numfielddefs = numfielddefs;
	for (i=0 ; i<numfielddefs ; i++)
	{
		fields[i].type = LittleShort (fields[i].type);
		fields[i].ofs = LittleShort (fields[i].ofs);
		fields[i].s_name = LittleLong (fields[i].s_name < 0 || fields[i].s_name > strofs ? 0: fields[i].s_name);
	}
	SafeWrite (h, fields, numfielddefs*sizeof(ddef_t));

	progs.ofs_globals = lseek (h, 0, SEEK_CUR);
	progs.numglobals = numpr_globals;
	for (i=0 ; i<numpr_globals ; i++)
		((int *)pr_globals)[i] = LittleLong (((int *)pr_globals)[i]);
	SafeWrite (h, pr_globals, numpr_globals*4);

	i = (int)lseek(h, 0, SEEK_CUR);
	if (summary)
		summary_print ("%6i TOTAL SIZE\n", i);	
	size = (i+16)&(~15);
	progs.entityfields = pr.size_fields;

	progs.version = PROG_VERSION;
	progs.crc = crc;
	if (summary)
	{
		summary_print("%6i Progheader CRC ", crc);
		if (crc == NQ_PROGHEADER_CRC)
			summary_print("(   Quake   )\n");
		else if (crc == QW_PROGHEADER_CRC)
			summary_print("(Quake World)\n");
		else
			summary_print("(  UNKNOWN  )\n");
	}

// byte swap the header and write it out
	for (i=0 ; i<sizeof(progs)/4 ; i++)
		((int *)&progs)[i] = LittleLong ( ((int *)&progs)[i] );		
	lseek (h, 0, SEEK_SET);
	SafeWrite (h, &progs, sizeof(progs));

// look for progs
	if ((def = PR_GetDef(&type_entity, "progs", NULL, false, 0, 0)))
	{
		lseek(h, progs.ofs_globals + 4 * def->ofs, SEEK_SET);
		i = - (size + 112);
		SafeWrite (h, &i, 4);
	}

	for (def = pr.def_head.next ; def ; def = def->next)
	{
		if (def->type->arraysize)
		{
			lseek(h, progs.ofs_globals + 4 * def->ofs, SEEK_SET);
			i = (-(size + 112)) + progs.ofs_globals + 4 * (def->arraystart);
			//printf("filled in %s with %i\n", def->name, def->arraystart);
			SafeWrite (h, &i, 4);		
		}
	}
	if (summary)
	{
		summary_print ("%6i precache_sounds(MAX: %6i)\n", numsounds, MAX_SOUNDS);
		summary_print ("%6i precache_models(MAX: %6i)\n", nummodels, MAX_MODELS);
	}
	close (h);
	
	if (summary)
	{
		if (pr_optimize_eliminate_temps || pr_optimize_shorten_ifs || pr_optimize_nonvec_parms
			|| pr_optimize_constant_names || pr_optimize_defs || pr_optimize_hash_strings ||
			pr_optimize_locals || pr_optimize_function_names || pr_optimize_filenames ||
			pr_optimize_unreferenced || pr_optimize_logicops || pr_optimize_recycle
			|| pr_optimize_constant_arithmetic) 
		{
			summary_print("----------- Optimization Summary -----------\n");
			if (pr_optimize_eliminate_temps)
				summary_print("%d stores shortened\n", num_stores_shortened);
			if (pr_optimize_shorten_ifs)
				summary_print("%d ifs shortened\n", num_ifs_shortened);
			if (pr_optimize_nonvec_parms)
				summary_print("%d non-vector parms\n", num_nonvec_parms);
			if (pr_optimize_constant_names)
				summary_print("%d bytes of constant defs/names eliminated\n", num_constant_names);
			if (pr_optimize_defs)
				summary_print("%d duplicate defs eliminated\n", num_defs);
			if (pr_optimize_hash_strings)
				summary_print("%d bytes of duplicate strings eliminated\n", num_strings);
			if (pr_optimize_locals)
				summary_print("%d bytes of immediate and local names eliminated\n", num_locals_saved);
			if (pr_optimize_function_names)
				summary_print("%d bytes of function names eliminated\n", num_funcs_saved);
			if (pr_optimize_filenames)
				summary_print("%d bytes of filenames eliminated\n", num_files_saved);
			if (pr_optimize_unreferenced)
				summary_print("%d unreferenced global defs eliminated\n", num_unreferenced);
			if (pr_optimize_logicops)
				summary_print("%d logic jumps added\n", num_logic_jumps);
			if (pr_optimize_recycle)
				summary_print("%d temporary globals recycled\n", num_recycled);
			if (pr_optimize_constant_arithmetic)
				summary_print("%d constant arithmetic statements eliminated\n", num_constant_ops_saved);
		}
	}
}



/*
===============
PR_String

Returns a string suitable for printing (no newlines, max 60 chars length)
===============
*/
char *PR_String (char *string)
{
	static char buf[80];
	char	*s;
	
	s = buf;
	*s++ = '"';
	while (string && *string)
	{
		if (s == buf + sizeof(buf) - 2)
			break;
		if (*string == '\n')
		{
			*s++ = '\\';
			*s++ = 'n';
		}
		else if (*string == '"')
		{
			*s++ = '\\';
			*s++ = '"';
		}
		else
			*s++ = *string;
		string++;
		if (s - buf > 60)
		{
			*s++ = '.';
			*s++ = '.';
			*s++ = '.';
			break;
		}
	}
	*s++ = '"';
	*s++ = 0;
	return buf;
}



def_t	*PR_DefForFieldOfs (gofs_t ofs)
{
	def_t	*d;
	
	for (d=pr.def_head.next ; d ; d=d->next)
	{
		if (d->type->type != ev_field)
			continue;
		if (*((int *)&pr_globals[d->ofs]) == ofs)
			return d;
	}
	Sys_Error ("Q614: PR_DefForFieldOfs: couldn't find %i",ofs);
	return NULL;
}

/*
============
PR_ValueString

Returns a string describing *data in a type specific manner
=============
*/
char *PR_ValueString (etype_t type, void *val)
{
	static char	line[256];
	def_t		*def;
	dfunction_t	*f;
	
	switch (type)
	{
	case ev_string:
		sprintf (line, "%s", PR_String(strings + *(int *)val));
		break;
	case ev_entity:	
		sprintf (line, "entity %i", *(int *)val);
		break;
	case ev_function:
		f = functions + *(int *)val;
		if (!f)
			sprintf (line, "undefined function");
		else
			sprintf (line, "%s()", strings + f->s_name);
		break;
	case ev_field:
		def = PR_DefForFieldOfs ( *(int *)val );
		sprintf (line, ".%s", def->name);
		break;
	case ev_void:
		sprintf (line, "void");
		break;
	case ev_float:
		sprintf (line, "%5.1f", *(float *)val);
		break;
	case ev_vector:
		sprintf (line, "'%5.1f %5.1f %5.1f'", ((float *)val)[0], ((float *)val)[1], ((float *)val)[2]);
		break;
	case ev_pointer:
		sprintf (line, "pointer");
		break;
	case ev_int:
		sprintf (line, "%%%d", *(int*)val);
		break;
	default:
		sprintf (line, "bad type %i", type);
		break;
	}
	
	return line;
}

/*
============
PR_GlobalString

Returns a string with a description and the contents of a global,
padded to 20 field width
============
*/
char *PR_GlobalStringNoContents (gofs_t ofs)
{
	int		i;
	def_t	*def;
	void	*val;
	static char	line[128];
	val = (void *)&pr_globals[ofs];
	def = pr_global_defs[ofs];
	if (!def)
		sprintf (line,"%i(?\?\?)", ofs);
	else if (!def->name)
		sprintf (line, "%i(temp)", ofs);
	else
		sprintf (line,"%i(%s)", ofs, def->name);
	
	i = strlen(line);
	for ( ; i<16 ; i++)
		strcat (line," ");
	strcat (line," ");
		
	return line;
}

char *PR_GlobalString (gofs_t ofs)
{
	char	*s;
	int		i;
	def_t	*def;
	void	*val;
	static char	line[128];
	
	val = (void *)&pr_globals[ofs];
	def = pr_global_defs[ofs];
	if (!def)
		return PR_GlobalStringNoContents(ofs);
	if (def->constant && def->type->type != ev_function)
	{
		s = PR_ValueString (def->type->type, &pr_globals[ofs]);
		sprintf (line,"%i(%s)", ofs, s);
	}
	else
		sprintf (line,"%i(%s)", ofs, def->name);
	
	i = strlen(line);
	for ( ; i<16 ; i++)
		strcat (line," ");
	strcat (line," ");
		
	return line;
}

/*
============
PR_PrintOfs
============
*/
void PR_PrintOfs (gofs_t ofs)
{
	printf ("%s\n",PR_GlobalString(ofs));
}

/*
=================
PR_PrintStatement
=================
*/
extern char *opnames[];

void PR_PrintStatement (dstatement_t *s)
{
	int		i;
	
	printf ("%4i : %4i : %s ", (int)(s - statements), statement_linenums[s-statements],opnames[s->op]);
	i = strlen(opnames[s->op]);
	for ( ; i<10 ; i++)
		printf (" ");
		
	if (s->op == OP2_IF || s->op == OP2_IFNOT)
		printf ("%sbranch %i",PR_GlobalString(s->a),s->b);
	else if (s->op == OP2_GOTO)
	{
		printf ("branch %i",s->a);
	}
	else if ( (unsigned)(s->op - OP2_STORE_F) < 6)
	{
		printf ("%s",PR_GlobalString(s->a));
		printf ("%s", PR_GlobalStringNoContents(s->b));
	}
	else
	{
		if (s->a)
			printf ("%s",PR_GlobalString(s->a));
		if (s->b)
			printf ("%s",PR_GlobalString(s->b));
		if (s->c)
			printf ("%s", PR_GlobalString(s->c));
		//	printf ("%s", PR_GlobalStringNoContents(s->c));
	}
	printf ("\n");
}


/*
============
PR_PrintDefs
============
*/
void PR_PrintDefs (void)
{
	def_t	*d;
	
	for (d=pr.def_head.next ; d ; d=d->next)
		PR_PrintOfs (d->ofs);
}


/*
==============
PR_BeginCompilation

called before compiling a batch of files, clears the pr struct
==============
*/
void	PR_BeginCompilation (void *memory, int memsize)
{
	int		i;

	pr.memory = (char *) memory;
	pr.max_memory = memsize;

	numpr_globals = RESERVED_OFS;
	pr.def_tail = &pr.def_head;

	for (i=0 ; i<RESERVED_OFS ; i++)
		pr_global_defs[i] = &def_void;
		
// link the function type in so state forward declarations match proper type
	pr.types = &type_function;
	type_function.next = NULL;
	pr_error_count = 0;

	num_stores_shortened = 0;
	num_ifs_shortened = 0;
	num_nonvec_parms = 0;
	num_constant_names = 0;
	num_defs = 0;
	num_strings = 0;
	num_logic_jumps = 0;

	// JPG - do this once at start; no need ot keep checking
	inithash();
}

/*
==============
PR_FinishCompilation

called after all files are compiled to check for errors
Returns false if errors were detected.
==============
*/
boolean	PR_FinishCompilation (void)
{
	def_t		*d;
	boolean	errors;
	int i;
	s_file = "LINK" - strings;
	pr_source_line = 0;
	errors = false;
// check to make sure all functions prototyped have code
	for (d=pr.def_head.next ; d ; d=d->next)
	{
		if (d->constant)
		{
			if (!d->defined)
			{
				PR_ParseError (551, "%s was not defined (see prototype %s(%i))",d->name, d->s_file, d->line);
				errors = true;
				pr_source_line++; // just to be cute
			}
		}
	}

// FrikaC: make sure all labels were defined
	for(i = num_defines; i > 0; i--)
	{
		if (pr_defines[i].label)
		{
			if (!pr_defines[i].defined)
			{
				PR_ParseError (552, "Label %s was not defined",pr_defines[i].name);
				errors = true;
				pr_source_line++; // just to be cute
			}
		}
	}


	return !errors;
}

//=============================================================================

// FIXME: byte swap?

// this is a 16 bit, non-reflected CRC using the polynomial 0x1021
// and the initial and final xor values shown below...  in other words, the
// CCITT standard CRC used by XMODEM

#define CRC_INIT_VALUE	0xffff
#define CRC_XOR_VALUE	0x0000

static unsigned short crctable[256] =
{
	0x0000,	0x1021,	0x2042,	0x3063,	0x4084,	0x50a5,	0x60c6,	0x70e7,
	0x8108,	0x9129,	0xa14a,	0xb16b,	0xc18c,	0xd1ad,	0xe1ce,	0xf1ef,
	0x1231,	0x0210,	0x3273,	0x2252,	0x52b5,	0x4294,	0x72f7,	0x62d6,
	0x9339,	0x8318,	0xb37b,	0xa35a,	0xd3bd,	0xc39c,	0xf3ff,	0xe3de,
	0x2462,	0x3443,	0x0420,	0x1401,	0x64e6,	0x74c7,	0x44a4,	0x5485,
	0xa56a,	0xb54b,	0x8528,	0x9509,	0xe5ee,	0xf5cf,	0xc5ac,	0xd58d,
	0x3653,	0x2672,	0x1611,	0x0630,	0x76d7,	0x66f6,	0x5695,	0x46b4,
	0xb75b,	0xa77a,	0x9719,	0x8738,	0xf7df,	0xe7fe,	0xd79d,	0xc7bc,
	0x48c4,	0x58e5,	0x6886,	0x78a7,	0x0840,	0x1861,	0x2802,	0x3823,
	0xc9cc,	0xd9ed,	0xe98e,	0xf9af,	0x8948,	0x9969,	0xa90a,	0xb92b,
	0x5af5,	0x4ad4,	0x7ab7,	0x6a96,	0x1a71,	0x0a50,	0x3a33,	0x2a12,
	0xdbfd,	0xcbdc,	0xfbbf,	0xeb9e,	0x9b79,	0x8b58,	0xbb3b,	0xab1a,
	0x6ca6,	0x7c87,	0x4ce4,	0x5cc5,	0x2c22,	0x3c03,	0x0c60,	0x1c41,
	0xedae,	0xfd8f,	0xcdec,	0xddcd,	0xad2a,	0xbd0b,	0x8d68,	0x9d49,
	0x7e97,	0x6eb6,	0x5ed5,	0x4ef4,	0x3e13,	0x2e32,	0x1e51,	0x0e70,
	0xff9f,	0xefbe,	0xdfdd,	0xcffc,	0xbf1b,	0xaf3a,	0x9f59,	0x8f78,
	0x9188,	0x81a9,	0xb1ca,	0xa1eb,	0xd10c,	0xc12d,	0xf14e,	0xe16f,
	0x1080,	0x00a1,	0x30c2,	0x20e3,	0x5004,	0x4025,	0x7046,	0x6067,
	0x83b9,	0x9398,	0xa3fb,	0xb3da,	0xc33d,	0xd31c,	0xe37f,	0xf35e,
	0x02b1,	0x1290,	0x22f3,	0x32d2,	0x4235,	0x5214,	0x6277,	0x7256,
	0xb5ea,	0xa5cb,	0x95a8,	0x8589,	0xf56e,	0xe54f,	0xd52c,	0xc50d,
	0x34e2,	0x24c3,	0x14a0,	0x0481,	0x7466,	0x6447,	0x5424,	0x4405,
	0xa7db,	0xb7fa,	0x8799,	0x97b8,	0xe75f,	0xf77e,	0xc71d,	0xd73c,
	0x26d3,	0x36f2,	0x0691,	0x16b0,	0x6657,	0x7676,	0x4615,	0x5634,
	0xd94c,	0xc96d,	0xf90e,	0xe92f,	0x99c8,	0x89e9,	0xb98a,	0xa9ab,
	0x5844,	0x4865,	0x7806,	0x6827,	0x18c0,	0x08e1,	0x3882,	0x28a3,
	0xcb7d,	0xdb5c,	0xeb3f,	0xfb1e,	0x8bf9,	0x9bd8,	0xabbb,	0xbb9a,
	0x4a75,	0x5a54,	0x6a37,	0x7a16,	0x0af1,	0x1ad0,	0x2ab3,	0x3a92,
	0xfd2e,	0xed0f,	0xdd6c,	0xcd4d,	0xbdaa,	0xad8b,	0x9de8,	0x8dc9,
	0x7c26,	0x6c07,	0x5c64,	0x4c45,	0x3ca2,	0x2c83,	0x1ce0,	0x0cc1,
	0xef1f,	0xff3e,	0xcf5d,	0xdf7c,	0xaf9b,	0xbfba,	0x8fd9,	0x9ff8,
	0x6e17,	0x7e36,	0x4e55,	0x5e74,	0x2e93,	0x3eb2,	0x0ed1,	0x1ef0
};

void CRC_Init(unsigned short *crcvalue)
{
	*crcvalue = CRC_INIT_VALUE;
}

void CRC_ProcessByte(unsigned short *crcvalue, byte data)
{
	*crcvalue = (*crcvalue << 8) ^ crctable[(*crcvalue >> 8) ^ data];
}

unsigned short CRC_Value(unsigned short crcvalue)
{
	return crcvalue ^ CRC_XOR_VALUE;
}
//=============================================================================

/*
============
PR_WriteProgdefs

Writes the global and entity structures out
Returns a crc of the header, to be stored in the progs file for comparison
at load time.
============
*/
int	PR_WriteProgdefs (char *filename)
{
	def_t	*d;
	FILE	*f;
	unsigned short		crc;
	int		c;
	
//	printf ("writing %s\n", filename);
	f = Sys_fopen (filename, "w");
	if (f <= 0)
		Sys_Error("Q615: Error writing %s: %s", filename, strerror(errno));

// print global vars until the first field is defined
	fprintf (f,"\n/* file generated by qcc, do not modify */\n\ntypedef struct\n{\tint\tpad[%i];\n", RESERVED_OFS);
	for (d=pr.def_head.next ; d ; d=d->next)
	{
		if (!STRCMP (d->name, "end_sys_globals"))
			break;
			
		switch (d->type->type)
		{
		case ev_float:
			fprintf (f, "\tfloat\t%s;\n",d->name);
			break;
		case ev_vector:
			fprintf (f, "\tvec3_t\t%s;\n",d->name);
			d=d->next->next->next;	// skip the elements
			break;
		case ev_string:
			fprintf (f,"\tstring_t\t%s;\n",d->name);
			break;
		case ev_function:
			fprintf (f,"\tfunc_t\t%s;\n",d->name);
			break;
		case ev_entity:
			fprintf (f,"\tint\t%s;\n",d->name);
			break;
		default:
			fprintf (f,"\tint\t%s;\n",d->name);
			break;
		}
	}
	fprintf (f,"} globalvars_t;\n\n");

// print all fields
	fprintf (f,"typedef struct\n{\n");
	for (d=pr.def_head.next ; d ; d=d->next)
	{
		if (!STRCMP (d->name, "end_sys_fields"))
			break;
			
		if (d->type->type != ev_field)
			continue;
			
		switch (d->type->aux_type->type)
		{
		case ev_float:
			fprintf (f,"\tfloat\t%s;\n",d->name);
			break;
		case ev_vector:
			fprintf (f,"\tvec3_t\t%s;\n",d->name);
			d=d->next->next->next;	// skip the elements
			break;
		case ev_string:
			fprintf (f,"\tstring_t\t%s;\n",d->name);
			break;
		case ev_function:
			fprintf (f,"\tfunc_t\t%s;\n",d->name);
			break;
		case ev_entity:
			fprintf (f,"\tint\t%s;\n",d->name);
			break;
		default:
			fprintf (f,"\tint\t%s;\n",d->name);
			break;
		}
	}
	fprintf (f,"} entvars_t;\n\n");
	
	fclose (f);
	
// do a crc of the file
	CRC_Init (&crc);
	f = Sys_fopen (filename, "r+");
	while ((c = fgetc(f)) != EOF)
		CRC_ProcessByte (&crc, c);
		
	fprintf (f,"#define PROGHEADER_CRC %i\n", crc);
	fclose (f);

	return crc;
}

void PrintFunction (char *name)
{
	int		i;
	dstatement_t	*ds;
	dfunction_t		*df;
	
	for (i=0 ; i<numfunctions ; i++)
	{
		if (!strcmp (name, strings + functions[i].s_name))
			break;
	}
	if (i==numfunctions)
		Sys_Error ("Q616: No function named \"%s\"", name);
	df = functions + i;	
	
	printf ("\nStatements for %s:\n", name);
	ds = statements + df->first_statement;
	while (1)
	{
		PR_PrintStatement (ds);
		if (!ds->op)
			break;
		ds++;
	}
}



void *mem_list[131072];
int mem_size = 0;



void *PR_Malloc (long size)
{
	void *ptr;

	ptr = malloc (size);
	if (!ptr)
		Sys_Error ("Q617: Malloc failure for %lu bytes",size);
	memset(ptr, 0, size);

	mem_list[mem_size++] = ptr; // Store it to remove on EndProgram
	if (mem_size > 131072)
		Sys_Error ("Q618: Ran out of mem pointer space.\nChange mem_list size in qcc.c and recompile");

	return ptr;
}

extern int pr_bracelevel;
extern int ifdefdepth;
void PR_Clear (void)
{
	int i;
	
	for (i =0; i < mem_size; i++);
		free(mem_list[i]);
	
	memset(htable, 0, sizeof(htable));
	memset(&pr, 0, sizeof(pr_info_t));
	memset(&pr_defines, 0, sizeof(pr_defines));
	memset(&macrohash, 0, sizeof(macrohash));
	memset(&pr_global_refs, 0, sizeof(pr_global_refs));

	num_defines = 0;
	mem_size = 0;
	pr_bracelevel = ifdefdepth = 0;
	numpr_globals = strofs = pr_immediate_strlen = numsounds =
	num_stores_shortened =
	num_ifs_shortened = num_nonvec_parms = num_constant_names =
	num_defs = num_strings = num_locals_saved = num_funcs_saved =
	num_files_saved = num_unreferenced = numsounds = numstatements =
	num_logic_jumps = num_recycled = nummodels = numfiles = 0;

	pr_error_count = pr_warning_count = 0;
	// this is a sucky hack
	// actually...this whole function is a sucky hack
	PR_BeginCompilation (PR_Malloc (0x100000), 0x100000);
	
	// actually...this whole compiler is a sucky hack
}

void ParseParms (char *src)
{

	int flag;
	src = COM_Parse(src);
	while(src)
	{

		flag = 2;
		if (com_token[0] == '-')
			flag = 0;
		else if (com_token[0] == '-')
			flag = 1;
		com_token[0] = '/';


		if (!STRCMP("/d", com_token))
		{
			src = COM_Parse(src);
			if (!src)
				return;
			num_defines++;
			macrohash[hash(com_token)] = num_defines;
			pr_defines[num_defines].name = (char *)PR_Malloc(strlen(com_token) + 1);
			strcpy(pr_defines[num_defines].name, com_token);
		}
		else if (!STRCMP("/i", com_token))
		{
			src = COM_Parse(src);
			strcpy (includedir, src);
			if (includedir[strlen(includedir)-1] != '/')
				strcat (includedir, "/");
		}
		else if (!STRCMP("/summary", com_token))
			summary = flag;
		else if (!STRCMP("/nolog", com_token))
			logging = !flag;
		else if (!STRCMP("/nopause", com_token))
			pr_pause = !flag;
		else if (!STRCMP("/Ot", com_token))
			pr_optimize_eliminate_temps = flag;
		else if (!STRCMP("/Oi", com_token))
			pr_optimize_shorten_ifs = flag;
		else if (!STRCMP("/Op", com_token))
			pr_optimize_nonvec_parms = flag;
		else if (!STRCMP("/Oc", com_token))
			pr_optimize_constant_names = flag;
		else if (!STRCMP("/Od", com_token))
			pr_optimize_defs = flag;
		else if (!STRCMP("/Os", com_token))
			pr_optimize_hash_strings = flag;
		else if (!STRCMP("/Ol", com_token))
			pr_optimize_locals = flag;
		else if (!STRCMP("/On", com_token))
			pr_optimize_function_names = flag;
		else if (!STRCMP("/Of", com_token))
			pr_optimize_filenames = flag;
		else if (!STRCMP("/Ou", com_token))
			pr_optimize_unreferenced = flag;
		else if (!STRCMP("/Oo", com_token))
			pr_optimize_logicops = flag;
		else if (!STRCMP("/Or", com_token))
			pr_optimize_recycle = flag;
		else if (!STRCMP("/Oa", com_token))
			pr_optimize_constant_arithmetic = flag;
		else if (!STRCMP("/O2", com_token))
		{
			pr_optimize_eliminate_temps = pr_optimize_shorten_ifs = pr_optimize_nonvec_parms 
			= pr_optimize_constant_names = pr_optimize_defs = pr_optimize_hash_strings = 
			pr_optimize_locals = pr_optimize_function_names = pr_optimize_filenames = 
			pr_optimize_unreferenced =  pr_optimize_recycle =
			pr_optimize_constant_arithmetic  = flag;
		}
		else if (!STRCMP("/O1", com_token))
		{
			pr_optimize_eliminate_temps = pr_optimize_shorten_ifs = pr_optimize_nonvec_parms 
			= pr_optimize_constant_names = pr_optimize_defs = pr_optimize_hash_strings = 
			pr_optimize_locals = pr_optimize_function_names = pr_optimize_filenames = pr_optimize_constant_arithmetic = 
			pr_optimize_unreferenced = pr_optimize_recycle = flag;
		}
		else if (!STRCMP("/Debug", com_token))
			pr_optimize_constant_names = pr_optimize_filenames = pr_optimize_function_names = pr_optimize_locals = !flag;
		else if (!STRCMP("/warn", com_token))
		{
			src = COM_Parse(src);
			if (!src)
				return;
			warninglevel = atoi(com_token);
		}
		else if (!STRCMP("/src", com_token))
		{
			src = COM_Parse(src);
			strcpy (sourcedir, src);
			if (sourcedir[strlen(sourcedir)-1] != '/')
				strcat (sourcedir, "/");
		}

		src = COM_Parse(src);
	}
}
