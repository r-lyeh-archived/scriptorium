This project is simply an experiment to convert the example debugger to C# .Net from C++ MFC.
It is as identically flawed and terrible as the original.

To build:
o The files 'SciLexer.dll' and 'ScintillaNet.dll' may need to be copied into the executable folder ie. bin\Debug
o If the MS Visual Studio designer does not show and edit the main form correctly, you may need to copy 'SciLexer.dll' to 
  a PATH location such as Windows\SysWOW64 (or Windows\System32 for 32bit OS)
  If there is need to add Scintilla to the toolbox: ToolBox (right click), Choose Items, Browse (locate ScintillaNet.dll, check box, ok)
  If there is need to add Scintilla Reference: Solution Explorer, References (right click), Add Reference, Browse (locate ScintillaNet.dll) 

Notes:
o App.cs may have some TODO comments at the top
o You will need to allow firewall access to run the debugger.
o The debugger works the same as the original... Run debugger first, then run a GM script with debugger support (eg. GME someScript.gm -d ) 

