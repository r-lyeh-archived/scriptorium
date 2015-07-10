
#include "qcc.h"
#include "hash.h"


int		pr_source_line;

char		*pr_file_p;
char		*pr_line_start;		// start of current source line

int			pr_bracelevel;

char		pr_token[2048];

token_type_t	pr_token_type;
type_t		*pr_immediate_type;
eval_t		pr_immediate;
int			pr_immediate_index;

char	pr_immediate_string[2048];
int		pr_immediate_strlen;

int		pr_error_count;

char	*pr_punctuation[] =
// longer symbols must be before a shorter partial match
{"&=", "|=", "+=", "-=", "*=", "/=", "--", "++",";", "(", ")", "==", "=", ",", "...", ".", "{", "}",
"[", "]", "+", "*", "-", "!=", "!", "#", "&&", "&",
">=", ">", "<=", "<", "||", "|", "/", ":", "@", "?" , NULL};

int	pr_punct_length[] =
{2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 2, 1, 1, 3, 1, 1, 1,
1, 1, 1, 1, 1, 2, 1, 1, 2, 1,
2, 1, 2, 1, 2, 1, 1, 1, 1, 1, 0};

// simple types.  function types are dynamically allocated
type_t	type_void = {ev_void, &def_void};
type_t	type_string = {ev_string, &def_string};
type_t	type_float = {ev_float, &def_float};
type_t	type_vector = {ev_vector, &def_vector};
type_t	type_entity = {ev_entity, &def_entity};
type_t	type_field = {ev_field, &def_field};
type_t	type_function = {ev_function, &def_function,NULL,&type_void};
// type_function is a void() function used for state defs
type_t	type_pointer = {ev_pointer, &def_pointer};
// type_int -- FrikaC new
type_t	type_int = {ev_int, &def_int};

type_t	type_floatfield = {ev_field, &def_field, NULL, &type_float};

int		type_size[9] = {1,1,1,3,1,1,1,1,1};

def_t	def_void = {&type_void, "temp"};
def_t	def_string = {&type_string, "temp"};
def_t	def_float = {&type_float, "temp"};
def_t	def_vector = {&type_vector, "temp"};
def_t	def_entity = {&type_entity, "temp"};
def_t	def_field = {&type_field, "temp"};
def_t	def_function = {&type_function, "temp"};
def_t	def_pointer = {&type_pointer, "temp"};
def_t	def_int = {&type_int, "temp"};


def_t	def_ret, def_parms[MAX_PARMS];

def_t	*def_for_type[9] = {&def_void, &def_string, &def_float, &def_vector, &def_entity, &def_field, &def_function, &def_pointer, &def_int};

void PR_LexWhitespace (void);

define_t	pr_defines[MAX_DEFINES];
int macrohash[HSIZE1];
int num_defines;


/*
==============
PR_PrintNextLine
==============
*/
void PR_PrintNextLine (void)
{
	char	*t;

	printf ("%3i:",pr_source_line);
	for (t=pr_line_start ; *t && *t != '\n' ; t++)
		printf ("%c",*t);
	printf ("\n");
}

/*
==============
PR_NewLine

Call at start of file and when *pr_file_p == '\n'
==============
*/
void PR_NewLine (void)
{

	if (*pr_file_p == '\n')
		pr_line_start = pr_file_p + 1;
	else
		pr_line_start = pr_file_p;
	pr_source_line++;
}

/*
==============
PR_LexString

Parses a quoted string
==============
*/
void PR_LexString (void)
{
	int		c;
	int mask1 = 0;
	int mask2;
	
	pr_immediate_strlen = 0;
	pr_file_p++;
	do
	{

		mask2 = 128;
		c = *pr_file_p++;
		if (!c)
			PR_ParseError (525, "EOF inside quote");
		if (c=='\n')
			PR_ParseError (526, "Newline inside quote");
		if (c == '\r')
			continue;
		if (c=='\\')
		{	// escape char
			mask2 = 0;
			c = *pr_file_p++;
			if (!c)
				PR_ParseError (525, "EOF inside quote");
			if (c == 'n')
				c = '\n';
			else if (c == '"')
				c = '"' | mask1;
			else if (c == '\\')
				c = '\\' | mask1;
			else if (c == 'b')
			{
				mask1 ^= 128;
				continue;
			}
			else if (c == '[')
				c = 16;
			else if (c == ']')
				c = 17;
			else if (c == '.')
				c = 28 | mask1;
			else if (c == '\n' || c=='\r')
			{
				// FrikaC: Windoze/DOS have both CR & LF, so let's keep checking
				// this has the side effect of potentially bumping the line num more than needed..
				// FIXME
				while(*pr_file_p == '\n' || *pr_file_p =='\r')
					pr_file_p++;
				PR_NewLine();
				continue;
			}
			else if (c == 't')
				c = '\t';
			else if (c == '<')
				c = 29;
			else if (c == '-')
				c = 30;
			else if (c == '>')
				c = 31;
			else if (c >= '0' && c <= '9')
				c = 18 + c - '0';
			else if (c == '(')
				c = 128;
			else if (c == '=')
				c = 129;
			else if (c == ')')
				c = 130;
			else if (c == '{')
			{
				int d;
				c = 0;
				while ((d = *pr_file_p++) != '}')
				{
					c = c * 10 + d - '0';
					if (d < '0' || d > '9' || c > 255)
						PR_ParseError(527, "Bad character code");
				}
			}
			else
				PR_ParseError (528, "Unknown escape char");
		}
		else if (c=='\"')
		{
			mask1 = 0;
			PR_LexWhitespace ();
			if (*pr_file_p == ':')
			{
				pr_file_p++;
				pr_token[pr_immediate_strlen++] = 0;
				PR_LexWhitespace();
			}
			if (*pr_file_p != '\"')
			{
				pr_token[pr_immediate_strlen++] = 0;
				pr_token_type = tt_immediate;
				pr_immediate_type = &type_string;
				memcpy (pr_immediate_string, pr_token, pr_immediate_strlen);
				return;
			}
			pr_file_p++;
			continue;
		}
		pr_token[pr_immediate_strlen++] = c | (mask1 & mask2);
	} while (1);
}

/*
==============
PR_LexNumber
==============
*/
float PR_LexNumber (void)
{
	int		c;
	int		len;
	
	len = 0;
	c = *pr_file_p;
	do
	{
		pr_token[len] = c;
		len++;
		pr_file_p++;
		c = *pr_file_p;
	} while ((c >= '0' && c<= '9') || c == '.');
	pr_token[len] = 0;
	return atof (pr_token);
}

/*
==============
PR_LexInt
==============
*/
unsigned int PR_LexInt (void)
{
	int		c;
	int		len;
	
	len = 0;
	c = *pr_file_p;
	do
	{
		pr_token[len] = c;
		len++;
		pr_file_p++;
		c = *pr_file_p;
	} while ((c >= '0' && c<= '9') || c == '-');
	pr_token[len] = 0;
	return atoi (pr_token);
}

/*
==============
PR_LexHex
==============
*/
unsigned int PR_LexHex (void)
{
	unsigned int		c;
	int		len;
	
	len = 0;
	pr_file_p += 2;
	c = *pr_file_p;
	do
	{
		pr_token[len] = c;
		len++;
		pr_file_p++;
		c = *pr_file_p;
	} while ((c >= '0' && c<= '9') || (c >= 'a' && c <= 'f'));
	pr_token[len] = 0;
	sscanf(pr_token, "%x", &c);
	return c;
}

/*
==============
PR_LexVector

Parses a single quoted vector
==============
*/
void PR_LexVector (void)
{
	int		i;
	
	pr_file_p++;
	pr_token_type = tt_immediate;
	pr_immediate_type = &type_vector;
	for (i=0 ; i<3 ; i++)
	{
		pr_immediate.vector[i] = PR_LexNumber ();
		PR_LexWhitespace ();
	}
	if (*pr_file_p != '\'')
		PR_ParseError (529, "Bad vector");
	pr_file_p++;
}

boolean PR_FindMacro (void);
/*
==============
PR_LexName

Parses an identifier
==============
*/
void PR_LexName (void)
{
	int		c;
	int		len;
	def_t *d;
	len = 0;
	c = *pr_file_p;

	do
	{
		pr_token[len] = c;
		len++;
		pr_file_p++;
		c = *pr_file_p;
	} while ( (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' 
	|| (c >= '0' && c <= '9'));

	pr_token[len] = 0;
//	PR_FindMacro();
	pr_token_type = tt_name;

	d = PR_GetDef(NULL, pr_token, pr_scope, false, 0, 0);
	if (d)
	{
		memcpy (&pr_immediate, pr_globals + d->ofs, 4*type_size[d->type->type]);
		pr_immediate_type = d->type;
	}

}

/*
==============
PR_LexPunctuation
==============
*/
void PR_LexPunctuation (void)
{
	int	i;
	char	*p;
	int	len;

	pr_token_type = tt_punct;

	for (i=0; (p = pr_punctuation[i]) != NULL; i++)
	{
		len = pr_punct_length[i];
		if (!STRNCMP(p, pr_file_p, len) )
		{
			memcpy(pr_token, p, len+1);
			if (p[0] == '{')
				pr_bracelevel++;
			else if (p[0] == '}')
			{
				pr_bracelevel--;
				if (pr_bracelevel < 0)
				{
					PR_ParseWarning(118, "Too many closing braces");
					pr_bracelevel = 0;
				}
			}
			pr_file_p += len;
			return;
		}
	}
	
	PR_ParseError (530,"Unknown punctuation");
}

		
/*
==============
PR_LexWhitespace
==============
*/
void PR_LexWhitespace (void)
{
	int		c;
	
	while (1)
	{
	// skip whitespace
		while ( (c = *pr_file_p) <= ' ')
		{
			if (c=='\n')
				PR_NewLine ();
			if (c == 0)
				return;		// end of file
			pr_file_p++;
		}
		
	// skip // comments
		if (c=='/' && pr_file_p[1] == '/')
		{
		//	if (pr_file_p[2] == '^')
		//		pr_file_p++;
			while (*pr_file_p && *pr_file_p != '\n')
				pr_file_p++;
			if (*pr_file_p == 0)
				return;
			PR_NewLine();
			pr_file_p++;
			continue;
		}
		
	// skip /* */ comments
		if (c=='/' && pr_file_p[1] == '*')
		{
			do
			{
				pr_file_p++;
				if (pr_file_p[0]=='\n')
					PR_NewLine();
				if (!*pr_file_p)
					return;
			} while (pr_file_p[-1] != '*' || pr_file_p[0] != '/');
			pr_file_p++;
			continue;
		}
		
		break;		// a real character has been found
	}
}

//============================================================================

// just parses text, returning false if an eol is reached
boolean PR_SimpleGetToken (void)
{
	int		c;
	int		i;
	
// skip whitespace
	while ( (c = *pr_file_p) <= ' ')
	{
		if (c=='\n' || c == 0)
			return false;
		pr_file_p++;
	}
	i = 0;
	while ( (c = *pr_file_p) > ' ' && c != ',' && c != ';' && c != ')')
	{
		pr_token[i] = c;
		i++;
		pr_file_p++;
	}
	pr_token[i] = 0;
	return true;
}

int		pr_nummacros;

void PR_ClearGrabMacros (void)
{
	pr_nummacros = 0;
}

int PR_FindDefine (char *name)
{
	int i, h;
	h = hash(name);
	i = macrohash[h];
	if (i && !STRCMP(name, pr_defines[i].name))
		return i;
	for (i=num_defines ; i>0 ; i--)
	{
		if (!STRCMP (pr_token, pr_defines[i].name))
		{
			macrohash[h] = i;
			return i;
		}
	}
	return 0;
}

boolean PR_FindMacro (void)
{
	int i;
	i = PR_FindDefine(pr_token);
	if (!i)
		return false;
	pr_token_type = tt_immediate;
	pr_immediate_type = pr_defines[i].type;
	memcpy(&pr_immediate, &(pr_defines[i].value), sizeof(eval_t));
	HashImmediate();
	return true;
}


void PR_ParseFrame (void)
{
	while (PR_SimpleGetToken ())
	{
		num_defines++;
		macrohash[hash(pr_token)] = num_defines;
		pr_defines[num_defines].name = (char *)PR_Malloc(strlen(pr_token) + 1);
		strcpy(pr_defines[num_defines].name, pr_token);
		pr_defines[num_defines].value._float = pr_nummacros;
		pr_defines[num_defines].type = &type_float;
		pr_nummacros++;

	}
}

/*
==============
PR_LexGrab

Deals with counting sequence numbers and replacing frame macros
==============
*/
void PR_LexGrab (void)
{	
	pr_file_p++;	// skip the $
	if (!PR_SimpleGetToken ())
		PR_ParseError (531, "Hanging $");
	
// check for $frame
	if (!STRCMP (pr_token, "frame"))
	{
		PR_ParseFrame ();
		PR_Lex ();
	}
// ignore other known $commands
	else if (!STRCMP (pr_token, "cd")
	|| !STRCMP (pr_token, "origin")
	|| !STRCMP (pr_token, "base")
	|| !STRCMP (pr_token, "flags")
	|| !STRCMP (pr_token, "scale")
	|| !STRCMP (pr_token, "skin") )
	{	// skip to end of line
		while (PR_SimpleGetToken ())
		;
		PR_Lex ();
	}
// hexen II commands
	else if (!STRCMP (pr_token, "framevalue") || !STRCMP(pr_token, "framerestore"))
	{
		PR_Lex();
		pr_nummacros = pr_immediate._float;
		PR_Lex();
	}
	else if (!STRCMP (pr_token, "framesave"))
	{
		if (!PR_SimpleGetToken())
			PR_ParseError(532, "$framesave: bad bookmark");

		num_defines++;
		pr_defines[num_defines].name = (char *)PR_Malloc(strlen(pr_token) + 1);
		strcpy(pr_defines[num_defines].name, pr_token);
		macrohash[hash(pr_token)] = num_defines;
		PR_Lex();
		pr_defines[num_defines].type = &type_float;
		pr_defines[num_defines].value._float = pr_nummacros;
		PR_Lex();
	}
// look for a frame name macro

	else
	{
		if (!PR_FindMacro ())
			PR_ParseError(533, "Unknown frame macro %s", pr_token);

	}
}
extern char	sourcedir[1024];
extern char	includedir[1024];
extern char destfile[1024];
char old_file[1024];
extern boolean pr_pause;
extern boolean logging;

//============================================================================

int ifdefdepth;
int ignoredepth;

void PR_LexPrecomp (void)
{
	char *oldpos;
	char	*src, *src2;
	char filename[1024];
	int backup_line, i;

	// yeah it isn't quite Precompiler is it?
	pr_file_p++;	// skip the hash
	if (!PR_SimpleGetToken ())
		PR_ParseError (534, "Invalid precompiler command"); // that's not possible
	
	if (!STRCMP(pr_token, "ifdef"))
	{

		if (ifdefdepth > ignoredepth)
		{
			ifdefdepth++;
			return;
		}
		ifdefdepth++;
		ignoredepth = ifdefdepth;
		PR_Lex();
		if (!PR_FindDefine(pr_token))
		{
			ignoredepth--;
			while(ifdefdepth > ignoredepth)
				PR_Lex();
			return;
		}
		PR_Lex();
		return;
	}
	if (!STRCMP(pr_token, "ifndef"))
	{
		if (ifdefdepth > ignoredepth)
		{
			ifdefdepth++;
			return;
		}
		ifdefdepth++;
		ignoredepth = ifdefdepth;
		PR_Lex();
		if (PR_FindDefine(pr_token))
		{
			ignoredepth--;
			while(ifdefdepth > ignoredepth)
				PR_Lex();
			return;
		}
		PR_Lex();
		return;
	}
	if (!STRCMP(pr_token, "endif"))
	{
		ifdefdepth--;
		if (ifdefdepth < 0)
			PR_ParseWarning(119, "Too many #endifs");
		PR_Lex();
		return;
	}
	if (!STRCMP(pr_token, "else"))
	{
		if (ifdefdepth != (ignoredepth + 1))
		{
			PR_Lex();
			return;
		}
		ignoredepth = ifdefdepth > ignoredepth ? ifdefdepth : ifdefdepth - 1;

		do
		{
			PR_Lex();
		}
		while(ifdefdepth > ignoredepth);
		return;
	}

	if (ifdefdepth > ignoredepth)
	{
//		print("ignored %s on %s(%i)", pr_token, s_file + strings, pr_source_line);
		return;
	}
	if (PR_Check("pragma", tt_name))
	{
		if (PR_Check("message", tt_name))
		{
			if (pr_immediate_type != &type_string)
				PR_ParseError(535, "Message must be a string");
			print("%s", pr_immediate_string);
			PR_Lex();
			return;
		}

		if (!STRCMP(pr_token, "PROGS_DAT"))
		{
			if (!PR_SimpleGetToken ())
				PR_ParseError(536, "Mangled PROGS_DAT directive");
			strcpy(destfile, pr_token);
			// changed output..
			PR_Lex();
			return;

		}
		if (!STRCMP(pr_token, "incudedir"))
		{
			if(!PR_SimpleGetToken())
				PR_ParseError(537, "Mangled includedir");
			strcpy(includedir, pr_token);
			PR_Lex();
			return;
		}
		if (!STRCMP(pr_token, "options"))
		{
			// FIXME
			PR_ParseError(599, "Options must be a string");
			ParseParms(pr_immediate_string);
			PR_Lex();
			return;	
		}

		PR_ParseWarning(120, "unknown pragma %s", pr_token);
		// skip to the end of the line
		while (PR_SimpleGetToken ())
			;
		PR_Lex ();
		return;
	}
	if (!STRCMP(pr_token, "includelist"))
	{

		while(1)
		{
			if(PR_SimpleGetToken())
			{
				oldpos = pr_file_p;
				backup_line = pr_source_line;
				strcpy(old_file, strings + s_file);
				if (!STRCMP(pr_token, "#endlist"))
					break;
				src2 = pr_token;
				sprintf (filename, "%s%s", sourcedir, src2);
				LoadFile (filename, (void **)&src2);
				print("%s", filename);
				if(!PR_CompileFile (src2, filename))
				{
					EndProgram(false);
					return;
				}

				pr_file_p = oldpos; // uhhh
				pr_source_line = backup_line;
				s_file = old_file - strings;
			}
			else if(!*pr_file_p++)
			{
				PR_ParseError(538, "EOF in #includelist, terminate with #endlist");
			}
		}
		pr_file_p = oldpos; // uhhh
		pr_source_line = backup_line;
		s_file = old_file - strings;
		PR_Lex();
		return;
	}
	if (!STRCMP(pr_token, "include"))
	{
		if(!PR_SimpleGetToken())
			PR_ParseError(539, "Mangled include directive");
		src2 = pr_token;

		if (pr_token[0] == '\"')
		{
			if (pr_file_p[-1] != '\"')
				PR_ParseError(540, "Invalid #include string");
			src2++;
			pr_token[strlen(pr_token) - 1] = 0; // testing?
			sprintf (filename, "%s%s", sourcedir, src2);
		}
		else if (pr_token[0] == '<')
		{
			if (pr_file_p[-1] != '>')
				PR_ParseError(540, "Invalid #include string");
			src2++;
			pr_token[strlen(pr_token) - 1] = 0; // testing?
			sprintf (filename, "%s%s", includedir, src2);
		}
		else
				sprintf (filename, "%s%s", sourcedir, src2);
		// back up all global data on current file
		strcpy(old_file, strings + s_file);
		oldpos = pr_file_p;
		backup_line = pr_source_line;
	//	printf ("including %s\n", filename);
		LoadFile (filename, (void **)&src);
		if(!PR_CompileFile (src, src2))
		{
			EndProgram(false);
			return;
		}
		pr_file_p = oldpos; // uhhh
		pr_source_line = backup_line;
		s_file = old_file - strings;
		PR_Lex();
		return;
	}
	if (PR_Check("error", 0))
	{
		if (pr_immediate_type != &type_string)
			PR_ParseError(541, "Error must be a string");
		PR_ParseError(500, pr_immediate_string);
		PR_Lex();
		return;
	}
	if (PR_Check("warning", 0))
	{
		if (pr_immediate_type != &type_string)
			PR_ParseError(542, "Warning must be a string");
		PR_ParseWarning(100, pr_immediate_string);
		PR_Lex();
		return;
	}
	if (PR_Check("define", 0))
	{
		if (pr_token_type != tt_name)
			PR_ParseError(543, "#define: Invalid name");

		backup_line = pr_source_line;
		i = ++num_defines;
		pr_defines[i].name = (char *)PR_Malloc(strlen(pr_token) + 1);
		macrohash[hash(pr_token)] = i;
		strcpy(pr_defines[i].name, pr_token);
		PR_Lex();
		if (pr_token_type == tt_immediate)
		{
			pr_defines[i].type = pr_immediate_type;
			memcpy(&(pr_defines[i].value), &pr_immediate, sizeof(eval_t));
			PR_Lex();
		}

		return;
	}
	if (PR_Check("undef", 0))
	{

		if (pr_token_type != tt_name)
			PR_ParseError(544, "#undef: Invalid name");
		num_defines--;
		backup_line = PR_FindDefine(pr_token);
		for(i = backup_line; i <= num_defines; i++)
			memcpy(&(pr_defines[i]), &(pr_defines[i+1]), sizeof(define_t));
		for (i = 0; i < HSIZE1; i++)
		{
			if (macrohash[i] > backup_line)
				macrohash[i]--;
			else if (macrohash[i] == backup_line)
				macrohash[i] = 0;
		}	
		PR_Lex();
		return;
	}

	if(!PR_FindMacro())
	{
		PR_ParseWarning(121, "unknown directive %s", pr_token);
	}

}

/*
==============
PR_Lex

Sets pr_token, pr_token_type, and possibly pr_immediate and pr_immediate_type
==============
*/
void PR_Lex (void)
{
	int		c;

	pr_token[0] = 0;
	pr_immediate_index = -1;


	if (!pr_file_p)
	{
		pr_token_type = tt_eof;
		return;
	}

	PR_LexWhitespace ();

	c = *pr_file_p;
		
	if (!c)
	{
		pr_token_type = tt_eof;
		return;
	}

// handle quoted strings as a unit
	if (c == '\"')
	{
		PR_LexString ();
		HashImmediate();
		return;
	}
	if (c == '%')
	{
		c = *++pr_file_p;
		expectint = true;
	}
// handle quoted vectors as a unit
	if (c == '\'')
	{
		if (pr_file_p[2] == '\'')
		{
			pr_token_type = tt_immediate;
			if (expectint)
			{
				pr_immediate_type = &type_int;
				pr_immediate._int = pr_file_p[1];
			}
			else
			{
				pr_immediate_type = &type_float;
				pr_immediate._float = pr_file_p[1];
			}
			expectint = false;
			pr_file_p += 3;
		}
		else
			PR_LexVector ();
		HashImmediate();
		return;
	}

	if (c == '0' && pr_file_p[1] == 'x')
	{
		pr_token_type = tt_immediate;
		pr_immediate_type = &type_float;
		pr_immediate._int = PR_LexHex ();
		HashImmediate();
		return;
	}

// if the first character is a valid identifier, parse until a non-id
// character is reached

	if ( (c >= '0' && c <= '9') || ( c=='-' && pr_file_p[1]>='0' && pr_file_p[1] <='9') )
	{
		pr_token_type = tt_immediate;
		if (expectint)
		{
			pr_immediate_type = &type_int;
			pr_immediate._int = PR_LexInt ();
			expectint = false;
		}
		else
		{
			pr_immediate_type = &type_float;
			pr_immediate._float = PR_LexNumber ();
		}
		

		HashImmediate();
		return;
	}
	
	if ( (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' )
	{
		PR_LexName ();
		return;
	}
	
	if (c == '$')
	{
		PR_LexGrab ();
		return;
	}
	if (c == '#')
	{
		if ( (pr_file_p[1] >= 'a' && pr_file_p[1] <= 'z') || (pr_file_p[1] >= 'A' && pr_file_p[1] <= 'Z') || pr_file_p[1] == '_' )
		{
			PR_LexPrecomp();
			return;
		}
	}
// parse symbol strings until a non-symbol is found
	PR_LexPunctuation ();
}

//=============================================================================

/*
================
ErrorLog
================
*/
FILE *logfile;

void ErrorLog(char *fmt, ...)
{
    va_list argptr;
    static char data[1024];


    va_start(argptr, fmt);
    vsprintf(data, fmt, argptr);
    va_end(argptr);
	if (!logfile)
		logfile = Sys_fopen("error.log", "w");
	if (!logfile)
		Sys_Error("Q613: Error writing error.log: %s\nTry using -nolog" , strerror(errno));

    fprintf(logfile, data);
    //close(f);
}
/*
============
PR_ParseError

Aborts the current file load
============
*/
boolean logging;
extern char	sourcedir[1024];
extern int num_breaks, num_continues;

void PR_ParseError (int num, char *error, ...)
{
	va_list		argptr;
	char		string[1024];
//	*(int *)-4 = 0;	// dump to debugger
	va_start (argptr,error);
	vsprintf (string,error,argptr);
	va_end (argptr);
	num_breaks = 0;
	num_continues = 0;

	conditional = false;

	print("%s(%i): error: Q%i: %s", strings + s_file, pr_source_line, num, string);

	if (logging)
		ErrorLog("error: Q%i: %s:%i:%s\n", num, strings + s_file, pr_source_line, string);
	if (strcmp(s_file + strings, "LINK"))
		longjmp (pr_parse_abort, 1);
	else
		pr_error_count++;

}
/*
============
PR_ParseWarning
============
*/

int		pr_warning_count;

void PR_ParseWarning (int level, char *warn, ...)
{
	va_list		argptr;
	char		string[1024];
	if ((level / 100) > warninglevel)
			return;
	va_start (argptr,warn);
	vsprintf (string,warn,argptr);
	va_end (argptr);

	pr_warning_count++;

	print ("%s(%i): warning:Q%i: %s", level, strings + s_file, pr_source_line, string);

	if (logging)
		ErrorLog("warning: Q%i: %s:%i:%s\n", level, strings + s_file, pr_source_line, string);
}


/*
============
PR_ParseName

Checks to see if the current token is a valid name
============
*/
char *PR_ParseName (void)
{
	static char	ident[MAX_NAME];
	
	if (pr_token_type != tt_name)
		PR_ParseError (545, "\"%s\" is not a name", pr_token);
	if (strlen(pr_token) >= MAX_NAME-1)
		PR_ParseError (546, "Name too long");
	strcpy (ident, pr_token);
	PR_Lex ();
	
	return ident;
}

/*
============
PR_FindType

Returns a preexisting complex type that matches the parm, or allocates
a new one and copies it out.
============
*/
type_t *PR_FindType (type_t *type)
{
	def_t	*def;
	type_t	*check;
	int		i;
	
	for (check = pr.types ; check ; check = check->next)
	{
		if (check->type != type->type
		|| check->aux_type != type->aux_type
		|| check->num_parms != type->num_parms
		|| type->arraysize)
			continue;
	
		for (i=0 ; i< type->num_parms ; i++)
			if (check->parm_types[i] != type->parm_types[i])
				break;
			
		if (i == type->num_parms)
			return check;	
	}
	
// allocate a new one
	check = (struct type_s *) PR_Malloc (sizeof (*check));
	*check = *type;
	check->next = pr.types;
	pr.types = check;
	
// allocate a generic def for the type, so fields can reference it
	def = (struct def_s *) PR_Malloc (sizeof(def_t));
	if (type->def && type->def->name)
		def->name = type->def->name;
	else
		def->name = "COMPLEX TYPE";
	def->type = check;
	check->def = def;
	return check;
}


/*
============
PR_SkipToSemicolon

For error recovery, also pops out of nested braces
============
*/
void PR_SkipToSemicolon (void)
{
	do
	{
		if (!pr_bracelevel)
		{
			if (PR_Check(";", tt_punct))
				return;
			if (PR_Check("}", tt_punct))
			{
				PR_Check(";", tt_punct);
				return;
			}
		}

		PR_Lex ();
	} while (pr_token_type != tt_eof);
}


/*
============
PR_ParseType

Parses a variable type, including field and functions types
============
*/
char	pr_parm_names[MAX_PARMS + MAX_EXTRA_PARMS][MAX_NAME];
type_t *PR_ParseTypeFunction (type_t *type);

type_t *PR_ParseType (void)
{
	type_t	newtype;
	type_t	*type, *insert, *t;
	
	if (PR_Check (".", tt_punct))
	{
		memset (&newtype, 0, sizeof(newtype));
		newtype.type = ev_field;
		newtype.aux_type = PR_ParseType ();
		if (newtype.aux_type->arraysize)
			PR_ParseError(547, "Field arrays not supported");

		return PR_FindType (&newtype);
	}
	
	if (!STRCMP (pr_token, "float") )
		type = &type_float;
	else if (!STRCMP (pr_token, "int") )
		type = &type_int;
	else if (!STRCMP (pr_token, "vector") )
		type = &type_vector;
	else if (!STRCMP (pr_token, "entity") )
		type = &type_entity;
	else if (!STRCMP (pr_token, "string") )
		type = &type_string;
	else if (!STRCMP (pr_token, "void") )
		type = &type_void;
	else
	{
		PR_ParseError (548, "\"%s\" is not a type", pr_token);
		type = &type_float;	// shut up compiler error
	}
	PR_Lex ();
	/*
	if (PR_Check("field", tt_name))
	{
		memset (&newtype, 0, sizeof(newtype));
		newtype.type = ev_field;
		newtype.aux_type = type;
		type = PR_FindType (&newtype);
	}*/
	if (PR_Check ("(", tt_punct))
		type = PR_ParseTypeFunction (type);
	if (PR_Check ("[", tt_punct))
	{
		memset (&newtype, 0, sizeof(newtype));
		newtype.type = ev_void;
		if (pr_immediate._float <= 0)
			PR_ParseError(549, "Cannot allocate an array of %i", pr_immediate._float);
		newtype.arraysize = pr_immediate._float;
		PR_Lex();

		newtype.aux_type = type;
		PR_Expect("]", tt_punct);

		type = PR_FindType (&newtype);
		insert = type;
		while(PR_Check("[", tt_punct))
		{
			memset (&newtype, 0, sizeof(newtype));
			newtype.type = ev_void;
			if (pr_immediate._float <= 0)
				PR_ParseError(549, "Cannot allocate an array of %i", pr_immediate._float);
			newtype.arraysize = pr_immediate._float;
			PR_Lex();

			newtype.aux_type = insert->aux_type;
			PR_Expect("]", tt_punct);
			insert->aux_type = PR_FindType (&newtype);
			insert = insert->aux_type;
		}
	}
	return type;
}


type_t *PR_ParseTypeFunction (type_t *type)
{
	type_t	newtype;
	type_t	*ttype;
	char	*name;
	static int recurse = 0;

// function type
	memset (&newtype, 0, sizeof(newtype));
	newtype.type = ev_function;
	newtype.aux_type = type;	// return type
	newtype.num_parms = 0;
	if (!PR_Check (")", tt_punct))
	{
		do
		{
			if (PR_Check ("...", tt_punct))
			{
				newtype.num_parms = -1 - newtype.num_parms;	// variable args
				break;
			}
			else
			{
				recurse++;
				ttype = PR_ParseType ();
				recurse--;

				if (ttype->type == ev_void)
					if (PR_Check(")", tt_punct))
						return PR_FindType (&newtype);
				name = PR_ParseName ();
				if (!recurse)
					strcpy (pr_parm_names[newtype.num_parms], name);
				newtype.parm_types[newtype.num_parms] = ttype;
				newtype.num_parms++;
			} 
		} while (PR_Check (",", tt_punct));
	
		PR_Expect (")", tt_punct);
	}
	
	return PR_FindType (&newtype);
}