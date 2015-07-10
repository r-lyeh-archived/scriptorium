// 詐欺コンパイラ
// インタプリタ(これ自身)の後ろにバイトコードを埋め込む
#include "PSL/PSL.h"

#include <stdio.h>
#if _WIN32
#include <conio.h>
#define EXT ".exe"
#else
#define EXT ".out"
#endif

typedef unsigned long dword;
using namespace PSL;

void compile(FILE *exe, const char *filename)
{
	PSLVM p;
	if (p.loadScript(filename))
		return;

	string exename = filename;
	int period = exename.rfind('.');
	if (period > 0)
		exename /= period;
	exename += EXT;
	FILE *fp = fopen(exename, "wb");
	if (!fp)
		return;
	fseek(exe, 0, SEEK_END);
	int end = ftell(exe);
	variable::buffer byte(end);
	fseek(exe, 0, SEEK_SET);
	fread(byte.get(), 1, end, exe);
	fwrite(byte.get(), 1, end, fp);
	p.writeCompiledCode(fp);
	dword l = 0xDEADC0DE;
	fwrite(&end, 1, sizeof(dword), fp);
	fwrite(&l, 1, sizeof(dword), fp);

	fclose(fp);
}

bool compiled(FILE *exe)
{
	int i = sizeof(dword);
	fseek(exe, -i, SEEK_END);
	dword l;
	fread(&l, 1, sizeof(dword), exe);
	return l == 0xDEADC0DE;
}

void execute(FILE *exe, variable &arg)
{
	int i = sizeof(dword) * 2;
	fseek(exe, -i, SEEK_END);
	dword l;
	int end = ftell(exe);
	fread(&l, 1, sizeof(dword), exe);
	fseek(exe, l, SEEK_SET);
	PSLVM p;
	if (p.loadCompiledCode(exe, end - l))
		return;
	p.run(arg);
}

int main(int argc, char **argv)
{
	FILE *exe = fopen(argv[0], "rb");
	if (!exe)
		return 1;

	if (!compiled(exe))
	{
		if (argc >= 2)
			compile(exe, argv[1]);
	}
	else
	{
		variable arg;
		for (int i = 0; i < argc; i++)
			arg[i] = argv[i];
		execute(exe, arg);
#if _WIN32
		getch();
#endif
	}

	fclose(exe);
	return 0;
}
