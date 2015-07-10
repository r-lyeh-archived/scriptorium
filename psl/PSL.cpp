
#include "PSL/PSL.h"

#include <stdio.h>
#ifdef _WIN32
#include <conio.h>
#else
#define getch getchar
#endif

int main(int argc, char **argv)
{
	if (argc < 2)
		return 0;

	using namespace PSL;
	PSLVM p;
	if (PSLVM::error e = p.loadScript(argv[1]))
	{
		if (e == PSLVM::FOPEN_ERROR)
		{
			string input = argv[1];
			for (int i = 2; i < argc; ++i)
			{
				input += ' ';
				input += argv[i];
			}
			input += ';';
			p.loadString(input);
			variable v = p.run();
			printf("%s", v.toString().c_str());
			return 0;
		}
		else
		{
			printf(" - compile error\n");
		}
	}
	else
	{
		if (argc > 2)
			p.writeCompiledCode(argv[2]);

#ifdef PSL_DEBUG
		variable r;
		while (!(r = p.stepExec())) if (getch() == 'q')break;
#endif
		variable v = p.run(argv[1]);
	}

	return 0;
}
