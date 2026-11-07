#include "stdafx.h"
#include "CommandLine.h"
#include <crt_externs.h>

const char* Platform::GetProcessCommandLine()
{
	// This is read while xrCore is initializing, before Memory._initialize.
	static const std::string CommandLine = []
	{
		std::string Result;
		char** Arguments = *_NSGetArgv();
		for (int Index = 1; Index < *_NSGetArgc(); ++Index)
		{
			if (!Result.empty())
			{
				Result += ' ';
			}
			Result += Arguments[Index];
		}
		return Result;
	}();
	return CommandLine.c_str();
}

