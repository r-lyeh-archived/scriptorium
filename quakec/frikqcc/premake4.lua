solution "frik"
	configurations { "Release", "Debug" }
	platforms {"native", "x32", "x64"}
	configuration "Release"
		flags      
		{
			"OptimizeSpeed", "OptimizeSize", "EnableSSE"
		}
	
	configuration "Debug"
		flags
		{
			"Symbols", "ExtraWarnings", "FatalWarnings"
		}

project "frikqcc"
	targetname  "frikqcc"
	language    "C"
	kind        "ConsoleApp"
	targetdir   "build"
	
	files
	{
		"compiler/**.c", "compiler/**.h",
	}

project "frikdec"
	targetname  "frikdec"
	language    "C"
	kind        "ConsoleApp"
	targetdir   "build"

	files
	{
		"decompiler/**.c", "decompiler/**.h",
	}
