// cmdlib.h


#ifndef __CMDLIB__
#define __CMDLIB__

#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef WIN32
#pragma warning (disable:4237)
#include <io.h>
#include <direct.h>
#define lseek _lseek
#define close _close
#define mkdir _mkdir
#else
#include <unistd.h>
#include <sys/fcntl.h>
#include <sys/file.h>
#endif

#ifdef NeXT
#include <libc.h>
#endif


#ifndef true
#define true 1
#endif
#ifndef false
#define false 0
#endif

#ifndef __BYTEBOOL__
#define __BYTEBOOL__
//typedef enum {false, true} boolean;
//typedef bool boolean; 
typedef unsigned char boolean; //make windows.h compatible
typedef unsigned char byte;
#endif

// the dec offsetof macro doesn't work very well...
#define myoffsetof(type,identifier) ((size_t)&((type *)0)->identifier)


// set these before calling CheckParm
extern int myargc;
extern char **myargv;

#define strupr strupr2
char *strupr (char *in);
char *strlower (char *in);

#ifndef WIN32
int filelength (int handle);
int tell (int handle);
#endif

time_t I_FloatTime (void);

void EndProgram (boolean completed);
void	Sys_Error (char *error, ...);
int		CheckParm (char *check);
char	*va(char *format, ...);

int 	SafeOpenWrite (char *filename);
int 	SafeOpenRead (char *filename);
void 	SafeRead (int handle, void *buffer, long count);
void 	SafeWrite (int handle, void *buffer, long count);
void 	*PR_Malloc (long size);

long	LoadFile (char *filename, void **bufferptr);
void	SaveFile (char *filename, void *buffer, long count);

void 	DefaultExtension (char *path, char *extension);
void 	DefaultPath (char *path, char *basepath);
void 	StripFilename (char *path);
void 	StripExtension (char *path);

void 	ExtractFilename (char *path, char *dest);

long 	ParseNum (char *str);

#ifndef INLINE
short	BigShort (short l);
short	LittleShort (short l);
long	BigLong (long l);
long	LittleLong (long l);
float	BigFloat (float l);
float	LittleFloat (float l);
#endif

char *COM_Parse (char *data);

extern	char	com_token[1024];
extern	int	com_eof;

#ifdef INLINE
#ifdef __BIG_ENDIAN__

inline static short
LittleShort (short l)
{
	byte    b1,b2;

	b1 = l&255;
	b2 = (l>>8)&255;

	return (b1<<8) + b2;
}

inline static short
BigShort (short l)
{
	return l;
}

inline static long
LittleLong (long l)
{
	byte    b1,b2,b3,b4;

	b1 = l&255;
	b2 = (l>>8)&255;
	b3 = (l>>16)&255;
	b4 = (l>>24)&255;

	return ((long)b1<<24) + ((long)b2<<16) + ((long)b3<<8) + b4;
}

inline static long
BigLong (long l)
{
	return l;
}


inline static float
LittleFloat (float l)
{
	union {byte b[4]; float f;} in, out;
	
	in.f = l;
	out.b[0] = in.b[3];
	out.b[1] = in.b[2];
	out.b[2] = in.b[1];
	out.b[3] = in.b[0];
	
	return out.f;
}

inline static float
BigFloat (float l)
{
	return l;
}


#else


inline static short
BigShort (short l)
{
	byte    b1,b2;

	b1 = l&255;
	b2 = (l>>8)&255;

	return (b1<<8) + b2;
}

inline static short
LittleShort (short l)
{
	return l;
}

inline static long
BigLong (long l)
{
	byte    b1,b2,b3,b4;

	b1 = l&255;
	b2 = (l>>8)&255;
	b3 = (l>>16)&255;
	b4 = (l>>24)&255;

	return ((long)b1<<24) + ((long)b2<<16) + ((long)b3<<8) + b4;
}

inline static long
LittleLong (long l)
{
	return l;
}

inline static float
BigFloat (float l)
{
	union {byte b[4]; float f;} in, out;
	
	in.f = l;
	out.b[0] = in.b[3];
	out.b[1] = in.b[2];
	out.b[2] = in.b[1];
	out.b[3] = in.b[0];
	
	return out.f;
}

inline static float
LittleFloat (float l)
{
	return l;
}

#endif

#endif

#endif

#ifndef O_BINARY
#define O_BINARY 0
#endif
