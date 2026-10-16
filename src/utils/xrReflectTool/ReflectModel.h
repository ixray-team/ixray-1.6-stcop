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

#pragma once

// Reflection model shared by the parser and the code generators.

#include <filesystem>
#include <map>
#include <string>
#include <utility>
#include <vector>

namespace XReflectTool
{
	struct ParseError
	{
		std::string File;
		int Line;
		std::string Message;
	};

	struct Specifiers
	{
		std::map<std::string, std::string> Values;
		int Line = 0;
		std::string Condition;

		bool Has(const std::string& Key) const
		{
			return Values.contains(Key);
		}

		std::string Get(const std::string& Key, const std::string& Default = {}) const
		{
			auto It = Values.find(Key);
			return It == Values.end() || It->second.empty() ? Default : It->second;
		}
	};

	struct FunctionInfo
	{
		std::string CppName;
		std::string ReturnType;
		std::vector<std::string> ParamTypes;
		std::string Qualifiers;
		std::string OperatorSymbol;
	};

	enum class EMemberKind
	{
		Method,
		Field,
		Constructor,
		Operator,
		Enum,
		Extension
	};

	struct MemberInfo
	{
		EMemberKind Kind;
		std::string LuaName;
		FunctionInfo Function;
		bool ReadOnly = false;
		std::string Condition;
		std::string Policies;
		// Enum: qualified enumerator prefix and the enumerator names.
		std::string EnumScope;
		std::vector<std::pair<std::string, std::string>> Enumerators; // Lua name, C++ name
		// Extension: free function bound as a method, its qualified name.
		std::string QualifiedName;
	};

	struct ClassInfo
	{
		std::string File;
		int Line = 0;
		std::string QualifiedName;
		std::string LuaName;
		std::string Condition;
		std::string RuntimeCondition;
		bool Constructible = false;
		bool ToString = false;
		std::vector<std::string> PublicBases;
		std::vector<std::string> ExplicitBases;
		std::vector<MemberInfo> Members;
	};

	struct FreeFunctionInfo
	{
		std::string File;
		int Line = 0;
		std::string LuaName;
		std::string QualifiedName;
		std::string Condition;
		std::string RuntimeCondition;
		std::string SelfClass;
		std::string Policies;
		FunctionInfo Function;
	};

	struct FreeEnumInfo
	{
		std::string File;
		int Line = 0;
		std::string SelfClass;
		MemberInfo Member;
	};

	struct FileInfo
	{
		std::string Path;
		bool IsSource = false;
		std::string Stem;
		std::vector<ClassInfo> Classes;
		std::vector<FreeFunctionInfo> Functions;
		std::vector<FreeEnumInfo> Enums;
	};

	std::string Trim(std::string Text);
	std::vector<std::string> SplitList(const std::string& Text, char Separator);
	std::string JoinList(const std::vector<std::string>& Items);

	std::string ReadFile(const std::filesystem::path& Path);
	std::string ToCrlf(const std::string& Text);
	bool WriteIfChanged(const std::string& Path, const std::string& Content, std::vector<ParseError>& Errors);
}
