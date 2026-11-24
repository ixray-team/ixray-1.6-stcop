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

#include "IniGenerator.h"

#include <algorithm>
#include <cctype>
#include <map>
#include <string_view>

namespace XReflectTool
{
	namespace
	{
		// Reads the IniRead fields of the class, not of its bases.
		std::string GenerateReader(const ClassInfo& Class)
		{
			std::string Body;
			for (const MemberInfo& Member : Class.Members)
			{
				if (!Member.IniRead)
				{
					continue;
				}

				const std::string Field = "Object." + Member.Function.CppName;
				std::string Line;
				if (Member.IniNested)
				{
					Line = "\tXReflect::ReadIniNested(Ini, Section, Prefix, \"" + Member.IniKey + "\", " + Field + ");\n";
				}
				else
				{
					Line = "\tXReflect::ReadIniValue(Ini, Section, Prefix, \"" + Member.IniKey + "\", " + Field + ", " +
						(Member.IniRequired ? "true" : "false") + ");\n";
				}
				if (!Member.IniIf.empty())
				{
					Line = "\tif (Object." + Member.IniIf + ")\n\t{\n\t" + Line + "\t}\n";
				}
				Body += Guard(Member.Condition, Class.Condition, Line);
			}

			std::string Out = "// " + Class.File + "(" + std::to_string(Class.Line) + ")\n";
			Out += "template <>\nvoid XReflect::IniReader<" + Class.QualifiedName + ">::Read(" + Class.QualifiedName +
				"& Object, const CInifile& Ini, const char* Section, const char* Prefix)\n{\n";
			Out += Body;
			Out += "}\n";
			return Guard(Class.Condition, {}, Out) + "\n";
		}
	}

	static std::string Quote(const std::string& Text)
	{
		std::string Out = "\"";
		for (char Char : Text)
		{
			if (Char == '\\' || Char == '"')
			{
				Out += '\\';
			}
			Out += Char;
		}
		return Out + "\"";
	}

	static constexpr std::string_view Whitespace = " \t\n\r\f\v";

	static std::string_view TakePreviousWord(std::string_view& Text)
	{
		const size_t Last = Text.find_last_not_of(Whitespace);
		if (Last == std::string_view::npos)
		{
			Text = {};
			return {};
		}
		size_t First = Last + 1;
		while (First > 0 && (Text[First - 1] == '_' || std::isalnum(static_cast<unsigned char>(Text[First - 1]))))
		{
			--First;
		}
		const std::string_view Word = Text.substr(First, Last + 1 - First);
		Text = Text.substr(0, First);
		return Word;
	}

	static bool DefinesClass(std::string_view Source, std::string_view Name)
	{
		for (size_t Position = Source.find(Name); Position != std::string_view::npos; Position = Source.find(Name, Position + 1))
		{
			auto Before = Source.substr(0, Position);
			if (Before.empty() || !Whitespace.contains(Before.back()))
			{
				continue;
			}

			auto Keyword = TakePreviousWord(Before);
			if (Keyword.size() > 4 && Keyword.ends_with("_API") && !Before.empty() && Whitespace.contains(Before.back()))
			{
				Keyword = TakePreviousWord(Before);
			}
			if (Keyword != "class" && Keyword != "struct")
			{
				continue;
			}

			auto After = Source.substr(Position + Name.size());
			const size_t First = After.find_first_not_of(Whitespace);
			if (First != std::string_view::npos && After.substr(First).starts_with("final"))
			{
				After.remove_prefix(First + 5);
			}
			const size_t Delimiter = After.find_first_not_of(Whitespace);
			if (Delimiter != std::string_view::npos && (After[Delimiter] == ':' || After[Delimiter] == '{'))
			{
				return true;
			}
		}
		return false;
	}

	// Owner checks need the header that defines the class.
	static std::string FindClassHeader(const std::string& Name, const std::vector<std::string>& Paths, std::map<std::string, std::string>& Cache)
	{
		for (const std::string& Path : Paths)
		{
			if (!Path.ends_with(".h") && !Path.ends_with(".hpp"))
			{
				continue;
			}

			auto It = Cache.find(Path);
			if (It == Cache.end())
			{
				It = Cache.emplace(Path, ReadFile(Path)).first;
			}
			if (DefinesClass(It->second, Name))
			{
				return Path;
			}
		}
		return {};
	}

	// Fields of the ltx schema: key, C++ type, required, the key of the If field, the nested struct
	static std::string GenerateSchema(const ClassInfo& Class, size_t Index, std::string& Entries)
	{
		std::string Fields;
		size_t Count = 0;
		for (const MemberInfo& Member : Class.Members)
		{
			if (!Member.IniRead)
			{
				continue;
			}

			std::string IfKey;
			if (!Member.IniIf.empty())
			{
				auto If = std::find_if(Class.Members.begin(), Class.Members.end(), [&](const MemberInfo& Other) { return Other.IniRead && Other.Function.CppName == Member.IniIf; });
				IfKey = If != Class.Members.end() ? If->IniKey : Member.IniIf;
			}

			Fields += "\t{ " + Quote(Member.IniKey) + ", " + Quote(Member.IniType) + ", " + (Member.IniRequired ? "true" : "false") + ", " + Quote(IfKey) + ", " +
				(Member.IniNested ? "true" : "false") + " },\n";
			++Count;
		}

		std::string Bases;
		for (const std::string& Base : Class.PublicBases)
		{
			Bases += (Bases.empty() ? "" : ",") + Base;
		}

		const std::string Array = "IniSchemaFields" + std::to_string(Index);
		Entries += "\t{ " + Quote(Class.QualifiedName) + ", " + Quote(Class.IniOwner) + ", " + Quote(Bases) + ", " + (Count != 0 ? Array : "nullptr") + ", " +
			std::to_string(Count) + " },\n";
		return Count != 0 ? "static const XReflect::SIniField " + Array + "[] =\n{\n" + Fields + "};\n\n" : "";
	}

	void GenerateIni(const std::string& Module, const std::string& Pch, const std::string& OutputDir, const std::vector<FileInfo>& Files,
		const std::vector<ClassInfo*>& Classes, const std::vector<std::string>& Paths, std::vector<ParseError>& Errors)
	{
		std::string SchemaFields;
		std::string SchemaEntries;
		size_t SchemaCount = 0;

		std::string OwnerChecks;
		std::vector<std::string> OwnerHeaders;
		std::map<std::string, std::string> HeaderCache;
		for (const ClassInfo* Class : Classes)
		{
			if (!Class->IniRead)
			{
				continue;
			}
			SchemaFields += GenerateSchema(*Class, SchemaCount++, SchemaEntries);

			if (Class->IniOwner.empty())
			{
				continue;
			}

			const std::string Header = FindClassHeader(Class->IniOwner, Paths, HeaderCache);
			if (Header.empty())
			{
				Errors.push_back({ Class->File, Class->Line, "no header of the module defines the Owner class " + Class->IniOwner });
				continue;
			}
			if (std::find(OwnerHeaders.begin(), OwnerHeaders.end(), Header) == OwnerHeaders.end())
			{
				OwnerHeaders.push_back(Header);
			}
			OwnerChecks += "\t\tif constexpr (std::is_base_of_v<" + Class->IniOwner + ", T>)\n\t\t{\n\t\t\tOut.push_back(" + Quote(Class->QualifiedName) + ");\n\t\t}\n";
		}

		// The object factory includes it: every registered class lists the schemas of its sections
		std::string Owners = "// Generated by xrReflectTool. Do not edit.\n\n#pragma once\n\n";
		for (const std::string& Header : OwnerHeaders)
		{
			Owners += "#include \"" + Header + "\"\n";
		}
		Owners += "\nnamespace XReflect\n{\n\ttemplate <typename T>\n\tvoid CollectIniSchemas(xr_vector<const char*>& Out)\n\t{\n";
		Owners += OwnerChecks.empty() ? "\t\t(void)Out;\n" : OwnerChecks;
		Owners += "\t}\n}\n";
		if (Errors.empty())
		{
			WriteIfChanged(OutputDir + "/IniOwners.gen.h", ToCrlf(Owners), Errors);
		}

		std::string Out = "// Generated by xrReflectTool. Do not edit.\n\n";
		if (!Pch.empty())
		{
			Out += "#include \"" + Pch + "\"\n";
		}
		Out += "\n";

		std::string Readers;
		std::vector<std::string> Headers;
		for (const ClassInfo* Class : Classes)
		{
			if (!Class->IniRead)
			{
				continue;
			}

			const FileInfo& Owner = *std::find_if(Files.begin(), Files.end(), [&](const FileInfo& File) { return File.Path == Class->File; });
			if (Owner.IsSource)
			{
				Errors.push_back({ Class->File, Class->Line, "declare XREFLECT(IniRead) classes in headers" });
				continue;
			}

			if (std::find(Headers.begin(), Headers.end(), Owner.Path) == Headers.end())
			{
				Headers.push_back(Owner.Path);
			}
			Readers += GenerateReader(*Class);
		}

		for (const std::string& Header : Headers)
		{
			Out += "#include \"" + Header + "\"\n";
		}
		Out += "\n" + Readers;

		// The ltx schema of the editor
		Out += SchemaFields;
		if (SchemaCount != 0)
		{
			Out += "static const XReflect::SIniSchema IniSchemas[] =\n{\n" + SchemaEntries + "};\n\n";
		}
		Out += "const XReflect::SIniSchema* XReflect::GetIniSchemas(u32& Count)\n{\n";
		Out += SchemaCount != 0 ? "\tCount = (u32)std::size(IniSchemas);\n\treturn IniSchemas;\n" : "\tCount = 0;\n\treturn nullptr;\n";
		Out += "}\n";

		if (Errors.empty())
		{
			WriteIfChanged(OutputDir + "/" + Module + ".ini.gen.cpp", ToCrlf(Out), Errors);
		}
	}
}
