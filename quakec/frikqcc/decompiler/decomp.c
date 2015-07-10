
#include "qcc.h"
#include "decomp.h"

#include <stdio.h>

extern opcode_t pr_opcodes [];


FILE *Decompileofile;
FILE *Decompileprogssrc;
FILE *Decompileprofile;
char *DecompileFilesSeen[1024];
int DecompileFileCtr = 0;
char *DecompileProfiles[MAX_FUNCTIONS];

char *type_names[8] =
{
    "void",
    "string",
    "float",
    "vector",
    "entity",
    "ev_field",
    "void()",
    "ev_pointer"
};

char *temp_type (unsigned short temp, dstatement_t *start, dfunction_t *df)
{
	int i;
	dstatement_t *stat;
	stat = start - 1;
	// determine the type of a temp

    while(stat > statements)
	{
		if (temp == stat->a)
			return type_names[pr_opcodes[stat->op].type_a->type->type];
		else if (temp == stat->b)
			return type_names[pr_opcodes[stat->op].type_b->type->type];
		else if (temp == stat->c)
			return type_names[pr_opcodes[stat->op].type_c->type->type];
		stat--;
    }

	// method 2
	// find a call to this function
	for (i = 0; i < numstatements; i++)
	{
		stat = &statements[i];

		if (stat->op >= OP2_CALL0 && stat->op <= OP2_CALL8 && ((eval_t *)&pr_globals[stat->a])->function == df - functions)
		{
			for(i++; i < numstatements; i++)
			{
				stat = &statements[i];
				if (OFS_RETURN == stat->a && pr_opcodes[stat->op].type_a->type->type != ev_void)
					return type_names[pr_opcodes[stat->op].type_a->type->type];
				else if (OFS_RETURN == stat->b && pr_opcodes[stat->op].type_b->type->type != ev_void)
					return type_names[pr_opcodes[stat->op].type_b->type->type];
				else if (stat->op == OP2_DONE)
					break;
				else if (stat->op >= OP2_CALL0 && stat->op <= OP2_CALL8 && stat->a != df - functions)
					break;
			}
		}
	}

	printf("Warning: Could not determine return type for %s\n", df->s_name + strings);

    return "float";

}

boolean IsConstant(ddef_t *def)
{

    int i;
	dstatement_t *d;

	if (def->type & DEF_SAVEGLOBGAL)
		return false;

    for (i = 1; i < numstatements; i++)
	{
		d = &statements[i];
		if (d->b == def->ofs)
		{
			if (pr_opcodes[d->op].right_associative)
			{
				if (d->op - OP2_STORE_F < 6)
				{
					return false;
				}
			}
		}
    }
	return true;
}

char *type_name (ddef_t *def)
{
	ddef_t *j;

	static char fname [1024];

	switch(def->type)
	{
	case ev_field:
	case ev_pointer:
		j = GetField(def->s_name + strings);
		if (j)
			return va(".%s",type_names[j->type]);
		else
			return type_names[def->type];
	case ev_void:
	case ev_string:
	case ev_entity:
	case ev_vector:
	case ev_float:
		return type_names[def->type];
	case ev_function:
		return "void()";
	default:
		return "float";
	}
};
extern float pr_globals[MAX_REGS];
extern int numpr_globals;

extern char strings[MAX_STRINGS];
extern int strofs;

extern dstatement_t statements[MAX_STATEMENTS];
extern int numstatements;
extern int statement_linenums[MAX_STATEMENTS];

extern dfunction_t functions[MAX_FUNCTIONS];
extern int numfunctions;

extern ddef_t globals[MAX_GLOBALS];
extern int numglobaldefs;

extern ddef_t fields[MAX_FIELDS];
extern int numfielddefs;

int maindecstatus = 6;

#define FILELISTSIZE 62



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
/*
============
PR_ValueString

Returns a string describing *data in a type specific manner
=============
*/


char *PR_ValueString (etype_t type, void *val)
{
	static char	line[256];

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

	case ev_void:
		sprintf (line, "void");
		break;
	case ev_float:
		{
			unsigned int high = *(unsigned int*)val & 0xff000000;
			if (high == 0xff000000 || !high)
				sprintf (line, "%%%d", *(int*)val);
			else
				sprintf (line, "%5.1f", *(float *)val);
		}
		break;
	case ev_vector:
		sprintf (line, "'%5.1f %5.1f %5.1f'", ((float *)val)[0], ((float *)val)[1], ((float *)val)[2]);
		break;
	case ev_pointer:
		sprintf (line, "pointer");
		break;
	default:
		sprintf (line, "bad type %i", type);
		break;
	}
	
	return line;
}


static char *filenames[] =
{
	"makevectors",		"defs.qc",
	"button_wait",		"buttons.qc",
	"anglemod",		"ai.qc",
	"boss_face",		"boss.qc",
	"info_intermission",	"client.qc",
	"CanDamage",	"combat.qc",
	"demon1_stand1",	"demon.qc",
	"dog_bite",		"dog.qc",
	"door_blocked",		"doors.qc",
	"Laser_Touch",		"enforcer.qc",
	"knight_attack",			"fight.qc",
	"f_stand1",	"fish.qc",
	"hknight_shot",	"hknight.qc",
	"SUB_regen",		"items.qc",
	"knight_stand1",	"knight.qc",
	"info_null",		"misc.qc",
	"monster_use",		"monsters.qc",
	"OgreGrenadeExplode",	"ogre.qc",
	"old_idle1",			"oldone.qc",
	"plat_spawn_inside_trigger", "plats.qc",
	"player_stand1",		"player.qc",
	"shal_stand",	"shalrath.qc",
	"sham_stand1",		"shambler.qc",
	"army_stand1",		"soldier.qc",
	"SUB_Null",			"subs.qc",
	"tbaby_stand1",		"tarbaby.qc",
	"trigger_reactivate",	"triggers.qc",
	"W_Precache",			"weapons.qc",
	"LaunchMissile",	"wizard.qc",
	"main",		"world.qc",
	"zombie_stand1",	"zombie.qc"
};

static char *builtins[] =
{

    NULL,
    "void (vector ang)",
    "void (entity e, vector o)",
    "void (entity e, string m)",
    "void (entity e, vector min, vector max)",
    NULL,
    "void ()",
    "float ()",
    "void (entity e, float chan, string samp, float vol, float atten)",
    "vector (vector v)",
    "void (string e)",
    "void (string e)",
    "float (vector v)",
    "float (vector v)",
    "entity ()",
    "void (entity e)",
    "void (vector v1, vector v2, float nomonsters, entity forent)",
    "entity ()",
    "entity (entity start, .string fld, string match)",
    "string (string s)",
    "string (string s)",
    "void (entity client, string s)",
    "entity (vector org, float rad)",
    "void (string s)",
    "void (entity client, string s)",
    "void (string s)",
    "string (float f)",
    "string (vector v)",
    "void ()",
    "void ()",
    "void ()",
    "void (entity e)",
    "float (float yaw, float dist)",
    NULL,
    "float (float yaw, float dist)",
    "void (float style, string value)",
    "float (float v)",
    "float (float v)",
    "float (float v)",
    NULL,
    "float (entity e)",
    "float (vector v)",
    NULL,
    "float (float f)",
    "vector (entity e, float speed)",
    "float (string s)",
    "void (string s)",
    "entity (entity e)",
    "void (vector o, vector d, float color, float count)",
    "void ()",
    NULL,
    "vector (vector v)",
    "void (float to, float f)",
    "void (float to, float f)",
    "void (float to, float f)",
    "void (float to, float f)",
    "void (float to, float f)",
    "void (float to, float f)",
    "void (float to, string s)",
    "void (float to, entity s)",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    "void (float step)",
    "string (string s)",
    "void (entity e)",
    "void (string s)",
    NULL,
    "void (string var, string val)",
    "void (entity client, string s, ...)",
    "void (vector pos, string samp, float vol, float atten)",
    "string (string s)",
    "string (string s)",
    "string (string s)",
    "void (entity e)",
	"void(entity killer, entity killee)",
	"string(entity e, string key)",
	"float(string s)",
	"void(vector where, float set)"

};

char *DecompileValueString(etype_t type, void *val);
ddef_t *DecompileGetParameter(gofs_t ofs);
char *DecompilePrintParameter(ddef_t * def);

void DecompileReadData(char *srcfile)
{
    dprograms_t progs;
    int h, i;
	void *p;
	char name[1024];

    h = SafeOpenRead(srcfile);
    SafeRead(h, &progs, sizeof(progs));

    lseek(h, progs.ofs_strings, SEEK_SET);
    strofs = progs.numstrings;
    SafeRead(h, strings, strofs);

    lseek(h, progs.ofs_statements, SEEK_SET);
    numstatements = progs.numstatements;
    SafeRead(h, statements, numstatements * sizeof(dstatement_t));

    lseek(h, progs.ofs_functions, SEEK_SET);
    numfunctions = progs.numfunctions;
    SafeRead(h, functions, numfunctions * sizeof(dfunction_t));

    lseek(h, progs.ofs_globaldefs, SEEK_SET);
    numglobaldefs = progs.numglobaldefs;
    SafeRead(h, globals, numglobaldefs * sizeof(ddef_t));

    lseek(h, progs.ofs_fielddefs, SEEK_SET);
    numfielddefs = progs.numfielddefs;
    SafeRead(h, fields, numfielddefs * sizeof(ddef_t));

    lseek(h, progs.ofs_globals, SEEK_SET);
    numpr_globals = progs.numglobals;
    SafeRead(h, pr_globals, numpr_globals * 4);
	printf("Decompiling...\n");
	printf("Read Data from %s:\n", srcfile);
	printf("Total Size is %6i\n", (int)lseek(h, 0, SEEK_END));
	printf("Version Code is %i\n", progs.version);
	printf("CRC is %i\n", progs.crc);
	printf("%6i strofs\n", strofs);
	printf("%6i numstatements\n", numstatements);
	printf("%6i numfunctions\n", numfunctions);
	printf("%6i numglobaldefs\n", numglobaldefs);
	printf("%6i numfielddefs\n", numfielddefs);
	printf("%6i numpr_globals\n", numpr_globals);
	printf("----------------------\n");

// fix up the functions
    for (i = 1; i < numfunctions; i++)
	{
			if (strlen(functions[i].s_name + strings) <= 0)
			{
				sprintf(name, "function%i", i);
				name[strlen(name)] = 0;
				p = malloc(strlen(name + 1));
				strcpy(p, name);
				functions[i].s_name = (char *)p - strings;
			}
	}
}

int 
DecompileGetFunctionIdxByName(char *name)
{

    int i;

    for (i = 1; i < numfunctions; i++)
		if (!strcmp(name, strings + functions[i].s_name)) 
		{
			break;
		}
    return i;
}

int 
DecompileAlreadySeen(char *fname)
{

    int i;
    char *knew;

    if (DecompileFileCtr > 1000)
	{
		printf("Fatal Error - too many source files.\n");
		exit(1);
	}

    for (i = 0; i < DecompileFileCtr; i++)
	{
		if (!strcmp(fname, DecompileFilesSeen[i]))
		    return 1;
    }

    knew = (char *)malloc(strlen(fname) + 1);
    strcpy(knew, fname);
    DecompileFilesSeen[DecompileFileCtr] = knew;
    DecompileFileCtr++;



    return 0;
}

void 
DecompileCalcProfiles(void)
{

    int i, j, ps;
    char *knew;
    static char fname[512];
    static char line[512];
    dfunction_t *df;
    dstatement_t *ds, *rds;
    ddef_t *par;
    unsigned short dom;

    for (i = 1; i < numfunctions; i++)
	{

		df = functions + i;
		fname[0] = '\0';
		line[0] = '\0';
		DecompileProfiles[i] = NULL;

		if (df->first_statement <= 0) 
		{
			if ((df->first_statement > -82) && builtins[-df->first_statement])
				sprintf(fname, "%s %s", builtins[-df->first_statement], strings + functions[i].s_name);
			else
			{
				sprintf(fname, "void () %s", strings + functions[i].s_name, strings + functions[i].s_name);
				printf("Warning: unknown builtin %s\n", strings + functions[i].s_name);
			}
		}
		else
		{

			ds = statements + df->first_statement;
			rds = NULL;

			/*
			 * find a return statement, to determine the result type 
			 */

			while (1) 
			{
				dom = (ds->op) % 100;
				if (!dom)
					break;
				if (dom == OP2_RETURN)
				{
					if (ds->a != 0)
					{
						if (DecompileGetParameter(ds->a))
						{
							rds = ds;
							break;
						}
					}
					if (rds == NULL)
						rds = ds;

				}
				ds++;
			}

			/*
			 * print the return type  
			 */

			if ((rds != NULL) && (rds->a != 0))
			{
				par = DecompileGetParameter(rds->a);

				if (par)
				{
					sprintf(fname, "%s ", type_name(par));
				}
				else
				{	
					sprintf(fname, "%s ", temp_type(rds->a, rds, df));
				}
			}
			else
			{
				sprintf(fname, "void ");
			}
			strcat(fname, "(");

			/*
			 * determine overall parameter size 
			 */

			for (j = 0, ps = 0; j < df->numparms; j++)
			ps += df->parm_size[j];

			if (ps > 0) 
			{
				for (j = df->parm_start; j < (df->parm_start) + ps; j++)
				{
					line[0] = '\0';
					par = DecompileGetParameter(j);

					if (!par)
					{
						//Error("Error - No parameter names with offset %i.", j);
						printf("No parameter names with offset %i\n", j);
						if (j < (df->parm_start) + ps - 1)
							sprintf(line, "float par%i, ", j - df->parm_start);
						else
							sprintf(line, "float par%i", j - df->parm_start);
					}
					else
					{
						if (par->type == ev_vector)
							j += 2;
						if (j < (df->parm_start) + ps - 1)
						{
							sprintf(line, "%s, ", DecompilePrintParameter(par));
						}
						else
						{
							sprintf(line, "%s", DecompilePrintParameter(par));
						}
					}
					strcat(fname, line);
				}
			}
			strcat(fname, ") ");
			line[0] = '\0';
			sprintf(line, strings + functions[i].s_name);
			strcat(fname, line);

		}

		if (i >= MAX_FUNCTIONS)
		{
		   printf("Fatal Error - too many functions.\n");
			exit(1);
		}
		knew = (char *)malloc(strlen(fname) + 1);
		strcpy(knew, fname);
		DecompileProfiles[i] = knew;
    }

}

char *DecompileGlobal(gofs_t ofs, def_t * req_t)
{
    int i;
    ddef_t *def;
    static char line[256];
    char *res;
    char found = 0;

    line[0] = '\0';

    def = NULL;

    for (i = 0; i < numglobaldefs; i++)
	{
		def = &globals[i];

		if (def->ofs == ofs)
		{

			found = 1;
			break;
		}
	}

	if (found)
	{

		if (!strcmp(strings + def->s_name, "IMMEDIATE"))
			sprintf(line, "%s", DecompileValueString((etype_t)(def->type), &pr_globals[def->ofs]));
		else 
		{

			sprintf(line, "%s", strings + def->s_name);
			if (def->type == ev_vector && req_t == &def_float)
				strcat(line, "_x");

		}
		res = (char *)malloc(strlen(line) + 1);
		strcpy(res, line);

		return res;
    }
    return NULL;
}

gofs_t 
DecompileScaleIndex(dfunction_t *df, gofs_t ofs)
{
    gofs_t nofs = 0;

    if (ofs > RESERVED_OFS)
	nofs = ofs - df->parm_start + RESERVED_OFS;
    else
	nofs = ofs;

    return nofs;
}

#define MAX_NO_LOCAL_IMMEDIATES 4096 /* dln: increased, not enough! */

char *DecompileImmediate(dfunction_t *df, gofs_t ofs, int fun, char *knew)
{
    int i;
    char *res;
    static char *IMMEDIATES[MAX_NO_LOCAL_IMMEDIATES];
    gofs_t nofs;

    // free 'em all 

    if (fun == 0) 
	{

		for (i = 0; i < MAX_NO_LOCAL_IMMEDIATES; i++)
		{
			if (IMMEDIATES[i]) 
			{
				free(IMMEDIATES[i]);
				IMMEDIATES[i] = NULL;
			}
		}
		return NULL;
    }
    nofs = DecompileScaleIndex(df, ofs);
 

    // check consistency 

    if ((nofs <= 0) || (nofs > MAX_NO_LOCAL_IMMEDIATES - 1))
	{
		printf("Fatal Error - Index (%i) out of bounds.\n", nofs);
		exit(1);
	}
  
    // insert at nofs 

    if (fun == 1) 
	{

		if (IMMEDIATES[nofs])
			free(IMMEDIATES[nofs]);

		IMMEDIATES[nofs] = (char *)malloc(strlen(knew) + 1);
		strcpy(IMMEDIATES[nofs], knew);

    }
    // get from nofs 

    else if (fun == 2)
	{

		if (IMMEDIATES[nofs])
		{

			res = (char *)malloc(strlen(IMMEDIATES[nofs]) + 1);
			strcpy(res, IMMEDIATES[nofs]);

			return res;
		}
		else
		{
			printf("Fatal Error - %i not defined.\n", nofs);
			res = malloc(10);
			sprintf(res, "unk%i",nofs);
			return res;
		}
    }
    return NULL;
}

char *DecompileGet(dfunction_t *df, gofs_t ofs, def_t *req_t)
{
    char *farg1;
	farg1 = NULL;

    farg1 = DecompileGlobal(ofs, req_t);

    if (farg1 == NULL)
		farg1 = DecompileImmediate(df, ofs, 2, NULL);

    return farg1;
}

void DecompilePrintStatement(dstatement_t *s);

void DecompileIndent(int c)
{
    int i;

    if (c < 0)
		c = 0;

    for (i = 0; i < c; i++) 
	{
		fprintf(Decompileofile, "\t");
    }
}

void DecompileDecompileStatement(dfunction_t * df, dstatement_t * s, int *indent)
{
    static char line[512];
    static char fnam[512];
    char *arg1, *arg2, *arg3;
    int nargs, i, j;
    dstatement_t *t;
    unsigned short dom, doc, ifc, tom;
    def_t *typ1, *typ2, *typ3;
	ddef_t *par;
    dstatement_t *k;
    int dum;

    arg1 = arg2 = arg3 = NULL;

    line[0] = '\0';
    fnam[0] = '\0';

    dom = s->op;

    doc = dom / 10000;
    ifc = (dom % 10000) / 100;

    // use program flow information 

    for (i = 0; i < ifc; i++) 
	{
		(*indent)--;
		DecompileIndent(*indent);
		fprintf(Decompileofile, "}\n");//FrikaC style modification
    }
    for (i = 0; i < doc; i++) 
	{
		DecompileIndent(*indent);
		fprintf(Decompileofile, "do\n");
		DecompileIndent(*indent);
		fprintf(Decompileofile, "{\n");
		(*indent)++;
    }

    /*
     * remove all program flow information 
     */
    s->op %= 100;
    typ1 = pr_opcodes[s->op].type_a;
    typ2 = pr_opcodes[s->op].type_b;
    typ3 = pr_opcodes[s->op].type_c;

    /*
     * states are handled at top level 
     */

	if (s->op == OP2_DONE)
	{

	}
    else if (s->op == OP2_STATE) 
	{

		par = DecompileGetParameter(s->a);
		if (!par)
		{
		    printf("Error - Can't determine frame number.\n");
			exit(1);
		}
		arg2 = DecompileGet(df, s->b, NULL);
		if (!arg2)
		{
		   printf("Error - No state parameter with offset %i.\n", s->b);
			exit(1);
		}
		DecompileIndent(*indent);
		fprintf(Decompileofile, "state [ %s, %s ];\n", DecompileValueString((etype_t)(par->type), &pr_globals[par->ofs]), arg2);

	//	free(arg2);
    }
	else if (s->op == OP2_RETURN)
	{
		DecompileIndent(*indent);
		fprintf(Decompileofile, "return");

		if (s->a)
		{
			fprintf(Decompileofile, " ");
			arg1 = DecompileGet(df, s->a, typ1);
			fprintf(Decompileofile, "(%s)", arg1);
		}
		fprintf(Decompileofile, ";\n");

    }
	else if ((OP2_MUL_F <= s->op && s->op <= OP2_SUB_V) ||
		(OP2_EQ_F <= s->op && s->op <= OP2_GT) ||
		(OP2_AND <= s->op && s->op <= OP2_BITOR))
	{

		arg1 = DecompileGet(df, s->a, typ1);
		arg2 = DecompileGet(df, s->b, typ2);
		arg3 = DecompileGlobal(s->c, typ3);

		if (arg3) 
		{
			DecompileIndent(*indent);
			fprintf(Decompileofile, "%s = %s %s %s;\n", arg3, arg1, pr_opcodes[s->op].name, arg2);
		}
		else
		{
			sprintf(line, "(%s %s %s)", arg1, pr_opcodes[s->op].name, arg2);
			DecompileImmediate(df, s->c, 1, line);
		}

    }
	else if (OP2_LOAD_F <= s->op && s->op <= OP2_ADDRESS)
	{

// RIGHT HERE

	arg1 = DecompileGet(df, s->a, typ1);
	arg2 = DecompileGet(df, s->b, typ2);
	arg3 = DecompileGlobal(s->c, typ3);

	if (arg3) {
	    DecompileIndent(*indent);
	    fprintf(Decompileofile, "%s = %s.%s;\n", arg3, arg1, arg2);
	} else {
	    sprintf(line, "%s.%s", arg1, arg2);
	    DecompileImmediate(df, s->c, 1, line);
	}
    } else if (OP2_STORE_F <= s->op && s->op <= OP2_STORE_FNC) {

	arg1 = DecompileGet(df, s->a, typ1);
	arg3 = DecompileGlobal(s->b, typ2);

	if (arg3) {
	    DecompileIndent(*indent);
	    fprintf(Decompileofile, "%s = %s;\n", arg3, arg1);
	} else {
	    sprintf(line, "%s", arg1);
	    DecompileImmediate(df, s->b, 1, line);
	}

    } else if (OP2_STOREP_F <= s->op && s->op <= OP2_STOREP_FNC) {

	arg1 = DecompileGet(df, s->a, typ2);
	arg2 = DecompileGet(df, s->b, typ2);

	DecompileIndent(*indent);
	fprintf(Decompileofile, "%s = %s;\n", arg2, arg1);

    } else if (OP2_NOT_F <= s->op && s->op <= OP2_NOT_FNC) {

	arg1 = DecompileGet(df, s->a, typ1);
	sprintf(line, "!%s", arg1);
	DecompileImmediate(df, s->c, 1, line);

    }
	else if (OP2_CALL0 <= s->op && s->op <= OP2_CALL8)
	{

	nargs = s->op - OP2_CALL0;

	arg1 = DecompileGet(df, s->a, NULL);
	sprintf(line, "%s (", arg1);
	sprintf(fnam, "%s", arg1);

	for (i = 0; i < nargs; i++)
	{

	    typ1 = NULL;

	    j = 4 + 3 * i;

	    if (arg1)
		free(arg1);

	    arg1 = DecompileGet(df, j, typ1);
	    strcat(line, arg1);

#ifndef DONT_USE_DIRTY_TRICKS
	    if (!strcmp(fnam, "WriteCoord"))
		if (!strcmp(arg1, "org") || !strcmp(arg1, "trace_endpos") || !strcmp(arg1, "p1") || !strcmp(arg1, "p2") || !strcmp(arg1, "o"))
		    strcat(line, "_x");
#endif

	    if (i < nargs - 1)
		strcat(line, ", ");//frikqcc modified
	}

	strcat(line, ")");
	DecompileImmediate(df, 1, 1, line);

	if ((((s + 1)->a != 1) && ((s + 1)->b != 1) &&
		((s + 2)->a != 1) && ((s + 2)->b != 1)) ||
	    ((((s + 1)->op) % 100 == OP2_CALL0) && ((((s + 2)->a != 1)) || ((s + 2)->b != 1)))) {
	    DecompileIndent(*indent);
	    fprintf(Decompileofile, "%s;\n", line);
	}
    } else if (s->op == OP2_IF || s->op == OP2_IFNOT) {

	arg1 = DecompileGet(df, s->a, NULL);
	arg2 = DecompileGlobal(s->a, NULL);

	if (s->op == OP2_IFNOT)
	{

	    if (s->b < 1)
		{
			printf("Found a negative IFNOT jump.\n");
			exit(1);
		}

	    /*
	     * get instruction right before the target 
	     */
	    t = s + s->b - 1;
	    tom = t->op % 100;

	    if (tom != OP2_GOTO) {

		/*
		 * pure if 
		 */
		DecompileIndent(*indent);
		fprintf(Decompileofile, "if (%s)\n", arg1);//FrikaC modified
		DecompileIndent(*indent);
		fprintf(Decompileofile, "{\n");

		(*indent)++;

	    } else {
		
		if (t->a > 0) {
		    /*
		     * ite 
		     */

		    DecompileIndent(*indent);
		    fprintf(Decompileofile, "if (%s)\n", arg1);//FrikaC modified
			DecompileIndent(*indent);
			fprintf(Decompileofile, "{\n");

			(*indent)++;

		} else {


		    if ((t->a + s->b) > 1) {
			/*
			 * pure if  
			 */

			DecompileIndent(*indent);
		    fprintf(Decompileofile, "if (%s)\n", arg1);//FrikaC modified
			DecompileIndent(*indent);
			fprintf(Decompileofile, "{\n");
			(*indent)++;
		    } else {

			dum = 1;
			for (k = t + (t->a); k < s; k++) {
			    tom = k->op % 100;
			    if (tom == OP2_GOTO || tom == OP2_IF || tom == OP2_IFNOT)
				dum = 0;
			}
			if (dum) 
			{

			    DecompileIndent(*indent);
			    fprintf(Decompileofile, "while (%s)\n", arg1);
				DecompileIndent(*indent); //FrikaC
				fprintf(Decompileofile, "{\n");
			    (*indent)++;
			} else {

			    DecompileIndent(*indent);


				fprintf(Decompileofile, "if (%s)\n", arg1);//FrikaC modified
				DecompileIndent(*indent);
				fprintf(Decompileofile, "{\n");
			    (*indent)++;
			}
		    }
		}
	    }

	}
	else
	{
	    /*
	     * do ... while 
	     */

	    (*indent)--;
	    fprintf(Decompileofile, "\n");
	    DecompileIndent(*indent);
	    fprintf(Decompileofile, "} while (%s);\n", arg1);

	}

    }
	else if (s->op == OP2_GOTO)
	{

		if (s->a > 0)
		{
			/*
			 * else 
			 */
			(*indent)--;
			DecompileIndent(*indent);
			fprintf(Decompileofile, "}\n");
			DecompileIndent(*indent);
			fprintf(Decompileofile, "else\n");
			DecompileIndent(*indent);
			fprintf(Decompileofile, "{\n");
			(*indent)++;

		}
		else
		{
			/*
			 * while 
			 */
			(*indent)--;

			DecompileIndent(*indent);
			fprintf(Decompileofile, "}\n");

		}

    }
	else
	{
		if (s->op <= OP2_BITOR)
			printf("Warnint: Unknown usage of OP_%s", pr_opcodes[s->op].opname);
		else
			printf("Warning: Unknown command\n");

    }


    if (arg1)
		free(arg1);
    if (arg2)
		free(arg2);
    if (arg3)
		free(arg3);

    return;
}

void DecompileDecompileFunction(dfunction_t * df)
{
    dstatement_t *ds;
    int indent;

    // Initialize 
 
    DecompileImmediate(df, 0, 0, NULL);

    indent = 1;

    ds = statements + df->first_statement;
	if(ds->op == OP2_STATE)
		ds++;
    while (1) 
	{
	
		DecompileDecompileStatement(df, ds, &indent);
		if (!ds->op)
			break;
		ds++;
    }

    if (indent != 1)
	{
		printf("Warning: Indentation structure corrupt\n");

	}
}

char *DecompileString(char *string)
{
    static char buf[255];
    char *s;
    int c = 1;

    s = buf;
    *s++ = '"';
    while (string && *string)
	{
		if (c == sizeof(buf) - 2)
			break;
		if (*string == '\n')
		{
			*s++ = '\\';
			*s++ = 'n';
			c++;
		}
		else if (*string == '"')
		{
			*s++ = '\\';
			*s++ = '"';
			c++;
		}
		else
		{
			*s++ = *string;
			c++;
		}
		string++;
		if (c > (int)(sizeof(buf) - 10))
		{
			*s++ = '.';
			*s++ = '.';
			*s++ = '.';
			c += 3;
			break;
		}
    }
    *s++ = '"';
    *s++ = 0;
    return buf;
}

char *DecompileValueString(etype_t type, void *val)
{
    static char line[1024];

    line[0] = '\0';

    switch (type)
	{
		case ev_string:
			sprintf(line, "%s", DecompileString(strings + *(int *)val));
			break;
		case ev_void:
			sprintf(line, "void");
			break;
		case ev_float:
			if (*(float *)val > 999999 || *(float *)val < -999999) // ugh
				sprintf(line, "%.f", *(float *)val);
			else if ((*(float *)val < 0.001) && (*(float *)val > 0))
				sprintf(line, "%.6f", *(float *)val);
			else			
				sprintf(line, "%g", *(float *)val);
			break;
		case ev_vector:
			sprintf(line, "'%g %g %g'", ((float *)val)[0], ((float *)val)[1], ((float *)val)[2]);
			break;
		default:
			sprintf(line, "bad type %i", type);
			break;
	}

	return line;
}

char *DecompilePrintParameter(ddef_t * def)
{
    static char line[128];

    line[0] = '0';

    if (!strcmp(strings + def->s_name, "IMMEDIATE"))
	{
		sprintf(line, "%s", DecompileValueString((etype_t)(def->type), &pr_globals[def->ofs]));
    }
	else
	{
		sprintf(line, "%s %s", type_name(def), strings + def->s_name);
	}
    return line;
}

ddef_t *GetField(char *name)
{
    int i;
    ddef_t *d;

    for (i = 1; i < numfielddefs; i++)
	{
		d = &fields[i];

		if (!strcmp(strings + d->s_name, name))
		    return d;
    }
    return NULL;
}
ddef_t *DecompileGetParameter(gofs_t ofs)
{
    int i;
    ddef_t *def;

    def = NULL;

    for (i = 0; i < numglobaldefs; i++)
	{
		def = &globals[i];

		if (def->ofs == ofs)
		{
			return def;
		}
    }

    return NULL;
}

void DecompileFunction(char *name)
{
    int i, findex, ps;
    dstatement_t *ds, *ts;
    dfunction_t *df;
    ddef_t *par;
    char *arg2;
    unsigned short dom, tom;
    int j, start, end;
    dfunction_t *dfpred;
    ddef_t *ef;
    static char line[256];
    dstatement_t *k;
    int dum;


    for (i = 1; i < numfunctions; i++)
		if (!strcmp(name, strings + functions[i].s_name))
			break;
    if (i == numfunctions)
	{
		printf("Fatal Error: No function named \"%s\"\n", name);
		exit(1);
	}
    df = functions + i;

    findex = i;

    // Check ''local globals'' 

    dfpred = df - 1;

    for (j = 0, ps = 0; j < dfpred->numparms; j++)
		ps += dfpred->parm_size[j];

    start = dfpred->parm_start + dfpred->locals + ps;

    if (dfpred->first_statement < 0 && df->first_statement > 0)
		start -= 1;

    if (start == 0)
		start = 1;

    end = df->parm_start;

    for (j = start; j < end; j++)
	{

		par = DecompileGetParameter(j);

		if (par)
		{
			if (par->type & (1 << 15))
				par->type -= (1 << 15);

			if (par->type == ev_function)
			{

				if (strcmp(strings + par->s_name, "IMMEDIATE"))
					if (strcmp(strings + par->s_name, name))
					{
						fprintf(Decompileofile, "%s;\n", DecompileProfiles[DecompileGetFunctionIdxByName(strings + par->s_name)]);
					}
			}
			else if (par->type != ev_pointer)
				if (strcmp(strings + par->s_name, "IMMEDIATE"))
				{

					if (par->type == ev_field)
					{

						ef = GetField(strings + par->s_name);

						if (!ef)
						{
							printf("Fatal Error: Could not locate a field named \"%s\"\n", strings + par->s_name);
							exit(1);
						}
						if (ef->type == ev_vector)
							j += 3;

	#ifndef DONT_USE_DIRTY_TRICKS
						if ((ef->type == ev_function) && !strcmp(strings + ef->s_name, "th_pain"))
						{
							fprintf(Decompileofile, ".void(entity attacker, float damage) th_pain;\n");
						}
						else
	#endif

							fprintf(Decompileofile, ".%s %s;\n", type_name(ef), strings + ef->s_name);

					}
					else
					{

						if (par->type == ev_vector)
							j += 2;

						if (par->type == ev_entity || par->type == ev_void)
						{

							fprintf(Decompileofile, "%s %s;\n", type_name(par), strings + par->s_name);

						}
						else
						{

							line[0] = '\0';
							sprintf(line, "%s", DecompileValueString((etype_t)(par->type), &pr_globals[par->ofs]));

							if (IsConstant(par))
							{
								fprintf(Decompileofile, "%s %s    = %s;\n", type_name(par), strings + par->s_name, line);
							}
							else
							{
								if (pr_globals[par->ofs] != 0)
									fprintf(Decompileofile, "%s %s /* = %s */;\n", type_name(par), strings + par->s_name, line);
								else
									fprintf(Decompileofile, "%s %s;\n", type_name(par), strings + par->s_name, line);
							}
						}
					}
				}
			}
		}
		/*
		 * Check ''local globals'' 
		 */

		if (df->first_statement <= 0)
		{

			fprintf(Decompileofile, "%s", DecompileProfiles[findex]);
			fprintf(Decompileofile, " = #%i; \n", -df->first_statement);

			return;
		}
		ds = statements + df->first_statement;

		while (1)
		{

			dom = (ds->op) % 100;

			if (!dom)
				break;
			else if (dom == OP2_GOTO)
			{
				// check for i-t-e 
				if (ds->a > 0)
				{
					ts = ds + ds->a;
					ts->op += 100;	// mark the end of a if/ite construct 
				}
			}
			else if (dom == OP2_IFNOT)
			{
				// check for pure if 

				ts = ds + ds->b;
				tom = (ts - 1)->op % 100;

				if (tom != OP2_GOTO)
					ts->op += 100;	// mark the end of a if/ite construct 
			else if ((ts - 1)->a < 0)
			{

				if (((ts - 1)->a + ds->b) > 1)
				{
					// pure if
					ts->op += 100;	// mark the end of a if/ite construct 
				}
				else
				{

					dum = 1;
					for (k = (ts - 1) + ((ts - 1)->a); k < ds; k++)
					{
						tom = k->op % 100;
						if (tom == OP2_GOTO || tom == OP2_IF || tom == OP2_IFNOT)
							dum = 0;
					}
					if (!dum)
					{
						// pure if  
						ts->op += 100;	// mark the end of a if/ite construct 
					}
				}
			}
		}
		else if (dom == OP2_IF)
		{
			ts = ds + ds->b;
			ts->op += 10000;	// mark the start of a do construct 

		}
		ds++;
	}

    /*
     * print the prototype 
     */
    fprintf(Decompileofile, "\n%s", DecompileProfiles[findex]);

    // handle state functions 

    ds = statements + df->first_statement;

    if (ds->op == OP2_STATE)
	{

		par = DecompileGetParameter(ds->a);
		if (!par)
		{
		    printf("Fatal Error - Can't determine frame number.");
			exit(1);
		}

		arg2 = DecompileGet(df, ds->b, NULL);
		if (!arg2)
		{
		    printf("Fatal Error - No state parameter with offset %i.", ds->b);
			exit(1);
		}

		fprintf(Decompileofile, " = [ %s, %s ]", DecompileValueString((etype_t)(par->type), &pr_globals[par->ofs]), arg2);

		free(arg2);

    }
	else
	{
		fprintf(Decompileofile, " =");
    }
    fprintf(Decompileofile, "\n{\n");

    /*
     * calculate the parameter size 
     */

    for (j = 0, ps = 0; j < df->numparms; j++)
	ps += df->parm_size[j];

    /*
     * print the locals 
     */

    if (df->locals > 0) {

	if ((df->parm_start) + df->locals - 1 >= (df->parm_start) + ps)
	{

	    for (i = df->parm_start + ps; i < (df->parm_start) + df->locals; i++) 
		{

			par = DecompileGetParameter(i);

			if (!par)
			{
				// temps
				continue;
			}
			else
			{
				if (!strcmp(strings + par->s_name, "IMMEDIATE"))
					continue; // immediates don't belong
				if (par->type == ev_function)
				{
					printf("Warning Fields and functions must be global\n");
				}
				else
					fprintf(Decompileofile, "\tlocal %s;\n", DecompilePrintParameter(par));
				if (par->type == ev_vector)
					i += 2;
			}
	    }

	    fprintf(Decompileofile, "\n");

		}
    }
    /*
     * do the hard work 
     */

    DecompileDecompileFunction(df);

    fprintf(Decompileofile, "};\n");
}
extern boolean safedecomp;
int fake_name;
char synth_name[1024]; // fake name part2

boolean TrySynthName(char *first)
{
	int i;

	// try to figure out the filename
	// based on the first function in the file
	for (i=0; i < FILELISTSIZE; i+=2)
	{
		if (!strcmp(filenames[i], first))
		{
			sprintf(synth_name, filenames[i + 1]);
			return true;
		}
	}
	return false;
}

void DecompileDecompileFunctions(void)
{
    int i;
	unsigned int o;
    dfunction_t *d;
	boolean bogusname;
    FILE *f;
    char fname[512];

    DecompileCalcProfiles();

    Decompileprogssrc = fopen("progs.src", "w");
    if (!Decompileprogssrc)
	{
		printf("Fatal Error - Could not open \"progs.src\" for output.\n");
		exit(1);
	}

    fprintf(Decompileprogssrc, "./progs.dat\n\n");


    for (i = 1; i < numfunctions; i++)
	{
		d = &functions[i];

		fname[0] = '\0';
		if (d->s_file <= strofs && d->s_file >= 0)
			sprintf(fname, "%s", strings + d->s_file);
		// FrikaC -- not sure if this is cool or what?
		bogusname = false;
		if (strlen(fname) <= 0)
				bogusname = true;
		else for (o = 0; o < strlen(fname); o++)
		{
			if ((fname[o] < 'a' || fname[o] > 'z') && 
			  (fname[o] < '0' || fname[o] > '9') &&
			  (fname[o] <'A' || fname[o] > 'Z') &&
			  (fname[o] != '.' && fname[o] != '!' && fname[o] != '_'))
			{
				if (fname[o] == '/')
					fname[o] = '.';
				else if (fname[o] == '\\')
					fname[o] = '.';
				else
				{
						bogusname = true;
						break;
				}
			}
		}

		if (bogusname)
		{
			if (!DecompileAlreadySeen(fname))
			{
				synth_name[0] = 0;
				if(!TrySynthName(va("%s", strings + d->s_name)))
							fake_name++;
			}
			if (synth_name[0])
				sprintf(fname, synth_name);
			else
				sprintf(fname, "frik%i.qc", fake_name);
		}



		if (!DecompileAlreadySeen(fname))
		{
			printf("decompiling %s...\n", fname);
			fprintf(Decompileprogssrc, "%s\n", fname);
			f = fopen(fname, "w");
		}
		else
			f = fopen(fname, "a+");
		if (!f)
		{
			printf("Fatal Error - Could not open \"%s\" for output.\n", fname);
			exit(1);
		}
		Decompileofile = f;
		DecompileFunction(strings + d->s_name);

		if (fclose(f))
		{
			printf("Fatal Error - Could not close \"%s\" properly.\n", fname);
			exit(1);
		}

	}
	if (fclose(Decompileprogssrc))
	{
		printf("Fatal Error - Could not close \"progs.src\" properly.\n");
		exit(1);
	}
}

void DecompileProgsDat(char *name)
{
    DecompileReadData(name);
    DecompileDecompileFunctions();

}

char *DecompileGlobalStringNoContents(gofs_t ofs)
{
    int i;
    ddef_t *def;
    static char line[128];

    line[0] = '0';
    sprintf(line, "%i(\?)", ofs);

    for (i = 0; i < numglobaldefs; i++)
	{
		def = &globals[i];

		if (def->ofs == ofs)
		{
			line[0] = '0';
			sprintf(line, "%i(%s)", def->ofs, strings + def->s_name);
			break;
		}
    }

    i = strlen(line);
    for (; i < 16; i++)
		strcat(line, " ");
    strcat(line, " ");

    return line;
}

char *DecompileGlobalString(gofs_t ofs)
{
    char *s;
    int i;
    ddef_t *def;
    static char line[128];

    line[0] = '0';
    sprintf(line, "%i(\?)", ofs);

    for (i = 0; i < numglobaldefs; i++)
	{
		def = &globals[i];

		if (def->ofs == ofs)
		{

			line[0] = '0';
			if (!strcmp(strings + def->s_name, "IMMEDIATE"))
			{
				s = PR_ValueString((etype_t)(def->type), &pr_globals[ofs]);
				sprintf(line, "%i(%s)", def->ofs, s);
			}
			else
				sprintf(line, "%i(%s)", def->ofs, strings + def->s_name);
		}
    }

    i = strlen(line);
    for (; i < 16; i++)
		strcat(line, " ");
    strcat(line, " ");

    return line;
}

void DecompilePrintStatement(dstatement_t * s)
{
    int i;

    printf("%4i : %s ", (int)(s - statements), pr_opcodes[s->op].opname);
    i = strlen(pr_opcodes[s->op].opname);
    for (; i < 10; i++)
		printf(" ");

    if (s->op == OP2_IF || s->op == OP2_IFNOT)
		printf("%sbranch %i", DecompileGlobalString(s->a), s->b);
    else if (s->op == OP2_GOTO)
	{
		printf("branch %i", s->a);
    }
	else if ((unsigned)(s->op - OP2_STORE_F) < 6)
	{
		printf("%s", DecompileGlobalString(s->a));
		printf("%s", DecompileGlobalStringNoContents(s->b));
    }
	else
	{
		if (s->a)
			printf("%s", DecompileGlobalString(s->a));
		if (s->b)
			printf("%s", DecompileGlobalString(s->b));
		if (s->c)
			printf("%s", DecompileGlobalStringNoContents(s->c));
	}
    printf("\n");
}

void DecompilePrintFunction(char *name)
{
    int i;
    dstatement_t *ds;
    dfunction_t *df;

    for (i = 0; i < numfunctions; i++)
		if (!strcmp(name, strings + functions[i].s_name))
			break;
    if (i == numfunctions)
	{
		printf("Fatal Error: No function names \"%s\"\n", name);
		exit(1);
	}
    df = functions + i;

    printf("Statements for %s:\n", name);
    ds = statements + df->first_statement;
    while (1)
	{
		DecompilePrintStatement(ds);

		if (!ds->op)
			break;
		ds++;
    }
}

