// Copyright (c) 2026 IX-Ray and contributors. All rights reserved.
//
// This file is part of the IX-Ray engine and is licensed for use only within
// the IX-Ray project (https://github.com/ixray-team). The project-wide license
// in LICENSE.md does not apply to this file.
//
// You may use, modify and distribute this file only as part of IX-Ray or of
// a game or modification built on IX-Ray. Using, copying or adapting this file,
// or any part of it, in any other project is prohibited without prior written
// permission from the IX-Ray team.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
// OTHER LIABILITY ARISING FROM THE USE OF THIS FILE.

// xrReflectTool: scans sources for XREFLECT / XPROPERTY markers (src/xrScripts/xrReflect.h)
// and generates luabind registration code. Runs before the module that owns the sources is
// compiled, so it is a standalone tool that uses only the standard library.
//
// xrReflectTool --module <Name> --sources <list file> --output-dir <dir> [--pch <header>] [--stamp <file>]
//
// Output:
//   <Module>.reflect.gen.cpp  header reflections and XReflectRegister(lua_State*)
//   <stem>.reflect.inl        reflections declared in <stem>.cpp; that .cpp includes it at the end

#include "ReflectModel.h"
#include "ReflectParser.h"
#include "LuaGenerator.h"

#include <algorithm>
#include <cstdio>
#include <fstream>
#include <map>

using namespace XReflectTool;

namespace
{
	void PrintUsage()
	{
		std::fprintf(stderr, "usage: xrReflectTool --module <Name> --sources <list file> --output-dir <dir> [--pch <header>] [--stamp <file>]\n");
	}
}

int main(int Argc, char** Argv)
{
	std::string Module;
	std::string SourceList;
	std::string OutputDir;
	std::string Pch;
	std::string StampPath;

	for (int Index = 1; Index + 1 < Argc; Index += 2)
	{
		std::string Key = Argv[Index];
		std::string Value = Argv[Index + 1];
		if (Key == "--module")
		{
			Module = Value;
		}
		else if (Key == "--sources")
		{
			SourceList = Value;
		}
		else if (Key == "--output-dir")
		{
			OutputDir = Value;
		}
		else if (Key == "--pch")
		{
			Pch = Value;
		}
		else if (Key == "--stamp")
		{
			StampPath = Value;
		}
		else
		{
			PrintUsage();
			return 2;
		}
	}

	if (Module.empty() || SourceList.empty() || OutputDir.empty())
	{
		PrintUsage();
		return 2;
	}

	std::vector<std::string> Paths;
	{
		std::ifstream List(SourceList);
		std::string Line;
		while (std::getline(List, Line))
		{
			Line = Trim(Line);
			if (!Line.empty())
			{
				Paths.push_back(std::filesystem::path(Line).lexically_normal().generic_string());
			}
		}
	}
	std::sort(Paths.begin(), Paths.end());
	Paths.erase(std::unique(Paths.begin(), Paths.end()), Paths.end());

	std::vector<ParseError> Errors;
	std::vector<FileInfo> Files;
	std::map<std::string, std::string> Stems;
	for (const std::string& Path : Paths)
	{
		std::string Source = ReadFile(Path);
		if (Source.find("XREFLECT") == std::string::npos && Source.find("XPROPERTY") == std::string::npos)
		{
			continue;
		}
		if (Path.ends_with("xrScripts/xrReflect.h"))
		{
			continue;
		}


		FileInfo File;
		File.Path = Path;
		std::string Extension = std::filesystem::path(Path).extension().string();
		File.IsSource = Extension == ".cpp" || Extension == ".cxx" || Extension == ".cc";
		File.Stem = std::filesystem::path(Path).stem().string();

		ParseFile(File, Source, Errors);

		if (File.Classes.empty() && File.Functions.empty() && File.Enums.empty())
		{
			continue;
		}

		if (File.IsSource)
		{
			if (auto [It, Inserted] = Stems.emplace(File.Stem, Path); !Inserted)
			{
				Errors.push_back({ Path, 1, "reflected sources need unique names, '" + File.Stem + "' clashes with " + It->second });
			}

			std::string Include = File.Stem + ".reflect.inl\"";
			if (Source.find(Include) == std::string::npos)
			{
				Errors.push_back({ Path, 1, "add #include \"" + File.Stem + ".reflect.inl\" at the end of this file" });
			}
		}

		Files.push_back(std::move(File));
	}

	std::vector<ClassInfo*> Classes = ResolveModel(Files, Errors);

	std::filesystem::create_directories(OutputDir);
	if (Errors.empty())
	{
		GenerateLua(Module, Pch, OutputDir, Files, Classes, Errors);
	}

	if (!Errors.empty())
	{
		for (const ParseError& Err : Errors)
		{
			std::fprintf(stderr, "%s(%d): error XR0001: %s\n", Err.File.c_str(), Err.Line, Err.Message.c_str());
		}
		return 1;
	}

	if (!StampPath.empty())
	{
		std::ofstream Stamp(StampPath, std::ios::binary | std::ios::trunc);
		Stamp << Module << "\n";
	}

	return 0;
}
