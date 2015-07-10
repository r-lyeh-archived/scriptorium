//============================================================================

#include "qcc.h"
#include "hash.h"
#include <time.h>

#ifdef macintosh
#include <console.h>    // RIOT - SIOUX command-line argument support
#endif

void PrintFunction (char *name);
char commandline[1024];

/*
============
main
============
*/

time_t StartTime;
int main (int argc, char **argv)
{
	char	*src;
	char	*src2;
	char	filename[1024];
	int		p, crc, handle;

	int i;

#ifdef macintosh
        argc = ccommand(&argv);     // RIOT
#endif

	print("--------------- frikqcc v2.7 ----------------");

	myargc = argc;
	myargv = argv;

	summary = false;
	pr_pause = !(argc > 1);
	strcpy (sourcedir, "");
	logging = true;
	commandline[0]=0;

	for(i = 2; i < argc; i++)
	{
		strcat(commandline, argv[i]);
		strcat(commandline, " ");
	}
	ParseParms(commandline);


	if ( CheckParm ("-?") || CheckParm ("-help") || CheckParm ("-h") || CheckParm("/?"))
	{
		print("frikqcc looks for progs.src in the current directory.");
		print("to look in a different directory: frikqcc -src <directory>");
		print("to set warning level: frikqcc -warn <warnlevel>");
		print("to decompile a progs.dat: frikqcc -dec <datfile>");
		print(" ");
		print("Optimizations:");
		print("\t/Ot\tshorten stores");
		print("\t/Oi\tshorten ifs");
		print("\t/Op\tnon-vector parms");
		print("\t/Oc\teliminate constant defs/names");
		print("\t/Od\teliminate duplicate defs");
		print("\t/Os\thash lookup in CopyString");
		print("\t/Ol\teliminate local, static and immediate names");
		print("\t/On\teliminate unneeded function names");
		print("\t/Of\tstrip filenames");
		print("\t/Ou\tremove unreferenced variable defs/names");
		print("\t/Oa\tremove constant arithmetic");
		print("\t/Oo\tAdd logic jumps");
		print("\t/Or\teliminate temps");
		print("\t/O1\tuse all optimizations except /Oo");
		print("\t/O2\tuse all optimizations");
		EndProgram(false);
		return 1;
	}


	remove("error.log");
	InitData ();
	pr_dumpasm = false;

// no cameras past this point
	StartTime = I_FloatTime();

	PR_BeginCompilation (PR_Malloc (0x100000), 0x100000);

	sprintf (filename, "%spreprogs.src", sourcedir);
	handle = open(filename, O_RDONLY);
	destfile[0] = 0;

	if(handle == -1)
	{
		//close(handle);
		sprintf (filename, "%sprogs.src", sourcedir);
		LoadFile (filename, (void **)&src);
	
		src = COM_Parse (src);
		if (!src)
			Sys_Error ("Q606: No destination filename.  frikqcc -help for info.\n");
		strcpy (destfile, com_token);
	}
	else
	{

		close(handle);
		LoadFile (filename, (void **)&src2);

		if (!PR_CompileFile (src2, "preprogs.src") )
		{
			EndProgram(false);
			return 1;
		}
		if(!destfile[0])
		{
			print("Warning: No destination file was set, assuming progs.dat");
			strcpy(destfile, "progs.dat");
		}

		goto endcompilation;
	}



// compile all the files
	do
	{
		src = COM_Parse(src);
		if (!src)
			break;
		sprintf (filename, "%s%s", sourcedir, com_token);
		print ("%s", filename);
		LoadFile (filename, (void **)&src2);
		if(!PR_CompileFile (src2, com_token))
			EndProgram(false);
	} while (1);

endcompilation:

	if (!PR_FinishCompilation ())
	{
		EndProgram(false);
		return 1;
	}

	p = CheckParm ("-asm");
	if (p)
	{
		for (p++ ; p<argc ; p++)
		{
			if (argv[p][0] == '-')
				break;
			PrintFunction (argv[p]);
		}
	}



// write progdefs.h

	crc = PR_WriteProgdefs ("progdefs.h");
	if (p = PR_FindDefine("SYSTEM_CRC"))
		crc = pr_defines[p].value._float;

// write data file

	WriteData (crc);
	if (CheckParm("-strings"))
		PrintStrings();
	if (CheckParm("-globals"))
		PrintGlobals();
	if (CheckParm("-functions"))
		PrintFunctions();
	if (CheckParm("-fields"))
		PrintFields();

	EndProgram(true);
	return 1;
}
