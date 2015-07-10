// cmdlib.c

#include "cmdlib.h"
#include "qcc.h"

#ifdef WIN32
#ifdef WIN32GUI
#include <windows.h>
#endif

#else
#include <sys/time.h>
#endif

#define PATHSEPERATOR   '\\'


// set these before calling CheckParm
int myargc;
char **myargv;

char	com_token[1024];
int		com_eof;
/*
============
va

does a varargs printf into a temp buffer, so I don't need to have
varargs versions of all text functions.
FIXME: make this buffer size safe someday
============
*/
char    *va(char *format, ...)
{
	va_list         argptr;
	static char             string[1024];
	
	va_start (argptr, format);
	vsprintf (string, format,argptr);
	va_end (argptr);

	return string;
}
/*
================
I_FloatTime
================
*/
#include <time.h>

time_t I_FloatTime (void)
{
    time_t t;

    time(&t);
	
    return t;
}


/*
==============
COM_Parse

Parse a token out of a string
==============
*/
char *COM_Parse (char *data)
{
	int		c;
	int		len;
	
	len = 0;
	com_token[0] = 0;
	
	if (!data)
		return NULL;
		
// skip whitespace
skipwhite:
	while ( (c = *data) <= ' ')
	{
		if (c == 0)
		{
			com_eof = true;
			return NULL;			// end of file;
		}
		data++;
	}
	
// skip // comments
	if (c=='/' && data[1] == '/')
	{
		while (*data && *data != '\n')
			data++;
		if (!*data)
		{
			com_eof = true;
			return NULL;			// end of file;
		}
		goto skipwhite;
	}
// skip /* */ comments
	if (c=='/' && data[1] == '*')
	{
		data+= 2; // don't fall for /*/
		while (*data && !(*data == '*' && data[1] == '/'))
			data++;
		if (!*data)
		{
			com_eof = true;
			return NULL;			// end of file;
		}
		data+=2;
		goto skipwhite;
	}
// handle quoted strings specially
	if (c == '\"')
	{
		data++;
		do
		{
			c = *data++;
			if (c=='\"')
			{
				com_token[len] = 0;
				return data;
			}
			com_token[len] = c;
			len++;
		} while (1);
	}

// parse single characters
	if (c=='{' || c=='}'|| c==')'|| c=='(' || c=='\'' || c==':')
	{
		com_token[len] = c;
		len++;
		com_token[len] = 0;
		return data+1;
	}

// parse a regular word
	do
	{
		com_token[len] = c;
		data++;
		len++;
		c = *data;
	if (c=='{' || c=='}'|| c==')'|| c=='(' || c=='\'' || c==':')
			break;
	} while (c>32);
	
	com_token[len] = 0;
	return data;
}




/*
================
filelength
================
*/
#ifndef WIN32
int filelength (int handle)
{
	struct stat	fileinfo;
    
	if (fstat (handle,&fileinfo) == -1)
	{
		Sys_Error ("Q601: Error fstating");
	}

	return fileinfo.st_size;
}

int tell (int handle)
{
	return lseek (handle, 0, SEEK_CUR);
}
#endif

char *strupr (char *start)
{
	char	*in;
	in = start;
	while (*in)
	{
		*in = toupper(*in);
		in++;
	}
	return start;
}

char *strlower (char *start)
{
	char	*in;
	in = start;
	while (*in)
	{
		*in = tolower(*in);
		in++;
	}
	return start;
}


/*
=============================================================================

						MISC FUNCTIONS

=============================================================================
*/

#ifdef WIN32GUI
extern HWND main_hWnd;
char summary_buf[4096];
#endif


void EndProgram (boolean completed)
{
	print(" ");
	print("%s - %i error(s), %i warning(s)", destfile, pr_error_count, pr_warning_count);
#ifndef WIN32GUI
	printf("\n");
	if (!pr_pause)
		exit(1);

	printf ("press a key\n");
	getchar();
	exit(1);
#else
	if (completed && summary)
		MessageBox(main_hWnd, summary_buf, "Summary", MB_OK | MB_SETFOREGROUND | MB_ICONINFORMATION);
	if (!pr_pause)
		if (!pr_error_count)
			Sys_Quit();

#endif
}

/*
=================
Sys_Error

For abnormal program terminations
=================
*/


void Sys_Error (char *error, ...)
{
	va_list argptr;
	char		text[1024];
	va_start (argptr, error);
	vsprintf (text, error,argptr);
	va_end (argptr);
#ifdef macintosh
	Error(text);
	exit(0);
#endif
#ifdef WIN32GUI
	MessageBox(main_hWnd, text, "FrikQCC Error", MB_OK | MB_SETFOREGROUND | MB_ICONSTOP);
	exit(0);
#else
	print ("************ ERROR ************");
	print(text);
	print(" ");
	EndProgram(false);

#endif
}


/*
=================
CheckParm

Checks for the given parameter in the program's command line arguments
Returns the argument number (1 to argc-1) or 0 if not present
=================
*/
int CheckParm (char *check)
{
	int             i;

	for (i = 1;i<myargc;i++)
	{
		if ( !STRCMP(check, myargv[i]) )
			return i;
	}
	return 0;
}


int SafeOpenWrite (char *filename)
{
	int     handle;

	umask (0);
	
	handle = open(filename,O_WRONLY | O_CREAT | O_TRUNC | O_BINARY
	, 0666);

	if (handle == -1)
		Sys_Error ("Q602: Error opening %s: %s",filename,strerror(errno));

	return handle;
}

int SafeOpenRead (char *filename)
{
	int     handle;

	handle = open(filename,O_RDONLY | O_BINARY);

	if (handle <= 0)
		Sys_Error ("Q602: Error opening %s: %s",filename,strerror(errno));

	return handle;
}


void SafeRead (int handle, void *buffer, long count)
{
	if (read (handle,buffer,count) != count)
		Sys_Error ("Q603: File read failure");
}


void SafeWrite (int handle, void *buffer, long count)
{
	if (write (handle,buffer,count) != count)
		Sys_Error ("Q604: File write failure");
}



/*
==============
LoadFile
==============
*/
long    LoadFile (char *filename, void **bufferptr)
{
	int             handle;
	long    length;
	void    *buffer;

	handle = SafeOpenRead (filename);
	length = filelength (handle);
	buffer = PR_Malloc (length+1);
	((byte *)buffer)[length] = 0;
	SafeRead (handle, buffer, length);
	close (handle);

	*bufferptr = buffer;
	return length;
}


/*
==============
SaveFile
==============
*/
void    SaveFile (char *filename, void *buffer, long count)
{
	int             handle;

	handle = SafeOpenWrite (filename);
	SafeWrite (handle, buffer, count);
	close (handle);
}



void DefaultExtension (char *path, char *extension)
{
	char    *src;
//
// if path doesn't have a .EXT, append extension
// (extension should include the .)
//
	src = path + strlen(path) - 1;

	while (*src != PATHSEPERATOR && src != path)
	{
		if (*src == '.')
			return;                 // it has an extension
		src--;
	}

	strcat (path, extension);
}


void DefaultPath (char *path, char *basepath)
{
	char    temp[128];

	if (path[0] == PATHSEPERATOR)
		return;                   // absolute path location
	strcpy (temp,path);
	strcpy (path,basepath);
	strcat (path,temp);
}


void    StripFilename (char *path)
{
	int             length;

	length = strlen(path)-1;
	while (length > 0 && path[length] != PATHSEPERATOR)
		length--;
	path[length] = 0;
}

void    StripExtension (char *path)
{
	int             length;

	length = strlen(path)-1;
	while (length > 0 && path[length] != '.')
	{
		length--;
		if (path[length] == '/')
			return;		// no extension
	}
	if (length)
		path[length] = 0;
}


void ExtractFilename (char *path, char *dest)
{
	char    *src;

	src = path + strlen(path) - 1;

//
// back up until a \ or the start
//
	while (src > path && *(src-1) != PATHSEPERATOR)
		src--;


	strcpy (dest,src);
}



/*
==============
ParseNum / ParseHex
==============
*/
long ParseHex (char *hex)
{
	char    *str;
	long    num;

	num = 0;
	str = hex;

	while (*str)
	{
		num <<= 4;
		if (*str >= '0' && *str <= '9')
			num += *str-'0';
		else if (*str >= 'a' && *str <= 'f')
			num += 10 + *str-'a';
		else if (*str >= 'A' && *str <= 'F')
			num += 10 + *str-'A';
		else
			PR_ParseError (553, "Bad hex number: %s",hex);
		str++;
	}

	return num;
}


long ParseNum (char *str)
{
	if (str[0] == '$')
		return ParseHex (str+1);
	if (str[0] == '0' && str[1] == 'x')
		return ParseHex (str+2);
	return atol (str);
}



/*
============================================================================

					BYTE ORDER FUNCTIONS

============================================================================
*/

#ifndef INLINE

#ifdef __BIG_ENDIAN__

short   LittleShort (short l)
{
	byte    b1,b2;

	b1 = l&255;
	b2 = (l>>8)&255;

	return (b1<<8) + b2;
}

short   BigShort (short l)
{
	return l;
}

long    LittleLong (long l)
{
	byte    b1,b2,b3,b4;

	b1 = l&255;
	b2 = (l>>8)&255;
	b3 = (l>>16)&255;
	b4 = (l>>24)&255;

	return ((long)b1<<24) + ((long)b2<<16) + ((long)b3<<8) + b4;
}

long    BigLong (long l)
{
	return l;
}


float	LittleFloat (float l)
{
	union {byte b[4]; float f;} in, out;
	
	in.f = l;
	out.b[0] = in.b[3];
	out.b[1] = in.b[2];
	out.b[2] = in.b[1];
	out.b[3] = in.b[0];
	
	return out.f;
}

float	BigFloat (float l)
{
	return l;
}


#else


short   BigShort (short l)
{
	byte    b1,b2;

	b1 = l&255;
	b2 = (l>>8)&255;

	return (b1<<8) + b2;
}

short   LittleShort (short l)
{
	return l;
}


long    BigLong (long l)
{
	byte    b1,b2,b3,b4;

	b1 = l&255;
	b2 = (l>>8)&255;
	b3 = (l>>16)&255;
	b4 = (l>>24)&255;

	return ((long)b1<<24) + ((long)b2<<16) + ((long)b3<<8) + b4;
}

long    LittleLong (long l)
{
	return l;
}

float	BigFloat (float l)
{
	union {byte b[4]; float f;} in, out;
	
	in.f = l;
	out.b[0] = in.b[3];
	out.b[1] = in.b[2];
	out.b[2] = in.b[1];
	out.b[3] = in.b[0];
	
	return out.f;
}

float	LittleFloat (float l)
{
	return l;
}

#endif

#endif
