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

#include "LuaGenerator.h"

#include <algorithm>
#include <cctype>
#include <map>
#include <set>
#include <sstream>

namespace XReflectTool
{
	namespace
	{
		// "A.B.Name" -> namespaces {A, B}, leaf "Name".
		void SplitLuaName(const std::string& LuaName, std::vector<std::string>& Namespaces, std::string& Leaf)
		{
			Namespaces = SplitList(LuaName, '.');
			Leaf = Namespaces.empty() ? LuaName : Namespaces.back();
			if (!Namespaces.empty())
			{
				Namespaces.pop_back();
			}
		}

		// Wraps a luabind scope expression into module(L, "A")[namespace_("B")[ ... ]].
		std::string WrapInModule(const std::vector<std::string>& Namespaces, const std::string& Body, const std::string& Indent)
		{
			std::string Open = Namespaces.empty() ? "luabind::module(L)\n" : "luabind::module(L, \"" + Namespaces[0] + "\")\n";
			std::string Inner = Body;
			for (size_t Index = Namespaces.size(); Index > 1; --Index)
			{
				Inner = Indent + "\tluabind::namespace_(\"" + Namespaces[Index - 1] + "\")\n" + Indent + "\t[\n" + Inner + "\n" + Indent + "\t]";
			}
			return Indent + Open + Indent + "[\n" + Inner + "\n" + Indent + "];\n";
		}

		std::string RuntimeGuard(const std::string& Condition, const std::string& Indent, const std::string& Text)
		{
			if (Condition.empty())
			{
				return Text;
			}

			std::string Indented;
			std::stringstream Stream(Text);
			std::string Line;
			while (std::getline(Stream, Line))
			{
				Indented += (Line.empty() || Line[0] == '#' ? "" : "\t") + Line + "\n";
			}
			return Indent + "if (" + Condition + ")\n" + Indent + "{\n" + Indented + Indent + "}\n";
		}

		// "out_value<2>() + adopt<1>()" -> ", luabind::out_value<2>() + luabind::adopt<1>()".
		std::string PolicyArgument(const std::string& Policies)
		{
			if (Policies.empty())
			{
				return {};
			}

			std::vector<std::string> Terms;
			std::string Term;
			int Depth = 0;
			for (char C : Policies)
			{
				if (C == '<' || C == '(')
				{
					++Depth;
				}
				else if (C == '>' || C == ')')
				{
					--Depth;
				}
				else if (C == '+' && Depth == 0)
				{
					Terms.push_back(Trim(Term));
					Term.clear();
					continue;
				}
				Term += C;
			}
			Terms.push_back(Trim(Term));

			std::string Result;
			for (const std::string& Item : Terms)
			{
				std::string Qualified = Item.starts_with("luabind::") || Item.starts_with("::") ? Item : "luabind::" + Item;
				Result += (Result.empty() ? "" : " + ") + Qualified;
			}
			return ", " + Result;
		}

		std::string GenerateClass(const ClassInfo& Class)
		{
			std::vector<std::string> Namespaces;
			std::string Leaf;
			SplitLuaName(Class.LuaName, Namespaces, Leaf);

			std::string Indent = Namespaces.size() > 1 ? "\t\t\t" : "\t\t";
			std::string Body = Indent + "luabind::class_<" + Class.QualifiedName;
			if (Class.PublicBases.size() == 1)
			{
				Body += ", " + Class.PublicBases[0];
			}
			else if (Class.PublicBases.size() > 1)
			{
				Body += ", luabind::bases<" + JoinList(Class.PublicBases) + ">";
			}
			Body += ">(\"" + Leaf + "\")";

			std::string Defs;
			if (Class.Constructible)
			{
				Defs += Indent + "\t.def(luabind::constructor<>())\n";
			}
			if (Class.ToString)
			{
				Defs += Indent + "\t.def(luabind::tostring(luabind::const_self))\n";
			}

			for (const MemberInfo& Member : Class.Members)
			{
				if (!Member.LuaExport)
				{
					continue;
				}

				const FunctionInfo& Function = Member.Function;
				std::string Def;
				switch (Member.Kind)
				{
				case EMemberKind::Constructor:
				{
					const int Full = (int)Function.ParamTypes.size();
					const int Min = Function.FirstDefault >= 0 ? Function.FirstDefault : Full;
					for (int Count = Min; Count <= Full; ++Count)
					{
						std::vector<std::string> Params(Function.ParamTypes.begin(), Function.ParamTypes.begin() + Count);
						Def += (Def.empty() ? "" : "\n" + Indent + "\t") + ".def(luabind::constructor<" + JoinList(Params) + ">())";
					}
					break;
				}
				case EMemberKind::Operator:
				{
					std::string Self = Function.Qualifiers.find("const") != std::string::npos ? "luabind::const_self" : "luabind::self";
					Def = ".def(" + Self + " " + Function.OperatorSymbol + " luabind::other<" + Function.ParamTypes[0] + ">())";
					break;
				}
				case EMemberKind::Method:
					Def = ".def(\"" + Member.LuaName + "\", static_cast<" + Function.ReturnType + " (" + Class.QualifiedName + "::*)(" +
						JoinList(Function.ParamTypes) + ")" + Function.Qualifiers + ">(&" + Class.QualifiedName + "::" + Function.CppName + ")" +
						PolicyArgument(Member.Policies) + ")";
					break;
				case EMemberKind::Extension:
					Def = ".def(\"" + Member.LuaName + "\", static_cast<" + Function.ReturnType + " (*)(" + JoinList(Function.ParamTypes) + ")>(&" +
						Member.QualifiedName + ")" + PolicyArgument(Member.Policies) + ")";
					break;
				case EMemberKind::Enum:
				{
					Def = ".enum_(\"" + Member.LuaName + "\")\n" + Indent + "\t[";
					for (size_t Index = 0; Index < Member.Enumerators.size(); ++Index)
					{
						const auto& [LuaName, CppName] = Member.Enumerators[Index];
						Def += std::string(Index ? "," : "") + "\n" + Indent + "\t\tluabind::value(\"" + LuaName + "\", int(" + Member.EnumScope + CppName + "))";
					}
					Def += "\n" + Indent + "\t]";
					break;
				}
				case EMemberKind::Field:
					Def = std::string(Member.ReadOnly ? ".def_readonly" : ".def_readwrite") + "(\"" + Member.LuaName + "\", &" +
						Class.QualifiedName + "::" + Function.CppName + ")";
					break;
				case EMemberKind::Property:
					Def = ".property(\"" + Member.LuaName + "\", &" + Class.QualifiedName + "::" + Function.CppName;
					if (!Member.Setter.empty())
					{
						Def += ", &" + Class.QualifiedName + "::" + Member.Setter;
					}
					Def += ")";
					break;
				}
				Defs += Guard(Member.Condition, Class.Condition, Indent + "\t" + Def + "\n");
			}
			if (!Defs.empty())
			{
				Body += "\n" + Defs.substr(0, Defs.size() - 1);
			}

			std::string Out = "// " + Class.File + "(" + std::to_string(Class.Line) + ")\n";
			Out += "template <>\nvoid XReflect::Exporter<" + Class.QualifiedName + ">::Register(lua_State* L)\n{\n";
			Out += WrapInModule(Namespaces, Body, "\t");
			Out += "}\n";
			return Guard(Class.Condition, {}, Out) + "\n";
		}

		// Emits class registrations in the global topological order. OnlyFile limits the output
		// to the classes of one source file; otherwise header classes are emitted and the
		// classes declared in .cpp files are left to their generated source functions.
		std::string GenerateClassCalls(const std::vector<ClassInfo*>& SortedClasses, const std::string& OnlyFile, const std::set<std::string>& SourceFiles)
		{
			std::string Out;
			for (const ClassInfo* Class : SortedClasses)
			{
				if (!Class->LuaExport)
				{
					continue;
				}
				if (OnlyFile.empty() ? SourceFiles.contains(Class->File) : Class->File != OnlyFile)
				{
					continue;
				}

				std::string Call = "\tXReflect::Exporter<" + Class->QualifiedName + ">::Register(L);\n";
				Out += Guard(Class->Condition, {}, RuntimeGuard(Class->RuntimeCondition, "\t", Call));
			}
			return Out;
		}

		std::string GenerateRegistration(const FileInfo& File, const std::vector<ClassInfo*>& SortedClasses)
		{
			std::string Out;

			// Consecutive functions with the same Lua namespace and conditions share one module statement.
			const std::vector<FreeFunctionInfo>& Functions = File.Functions;
			for (size_t First = 0; First < Functions.size();)
			{
				std::vector<std::string> Namespaces;
				std::string Leaf;
				SplitLuaName(Functions[First].LuaName, Namespaces, Leaf);
				std::string Indent = Namespaces.size() > 1 ? "\t\t\t" : "\t\t";

				std::string Body;
				size_t Last = First;
				for (; Last < Functions.size(); ++Last)
				{
					const FreeFunctionInfo& Function = Functions[Last];
					std::vector<std::string> OtherNamespaces;
					SplitLuaName(Function.LuaName, OtherNamespaces, Leaf);
					if (OtherNamespaces != Namespaces || Function.Condition != Functions[First].Condition ||
						Function.RuntimeCondition != Functions[First].RuntimeCondition)
					{
						break;
					}

					std::string Pointer = Function.Function.ReturnType + " (*)(" + JoinList(Function.Function.ParamTypes) + ")";
					Body += (Body.empty() ? "" : ",\n") + Indent + "luabind::def(\"" + Leaf + "\", static_cast<" + Pointer + ">(&" + Function.QualifiedName + "))";
				}

				std::string Statement = WrapInModule(Namespaces, Body, "\t");
				Out += Guard(Functions[First].Condition, {}, RuntimeGuard(Functions[First].RuntimeCondition, "\t", Statement));
				First = Last;
			}
			return Out;
		}

		std::string SourceFunctionName(const std::string& Module, const FileInfo& File)
		{
			std::string Name = "XReflectRegister_" + Module + "_";
			for (char C : File.Stem)
			{
				Name += (std::isalnum((unsigned char)C) || C == '_') ? C : '_';
			}
			return Name;
		}
	}

	void GenerateLua(const std::string& Module, const std::string& Pch, const std::string& OutputDir, const std::vector<FileInfo>& Files,
		const std::vector<ClassInfo*>& Classes, std::vector<ParseError>& Errors)
	{
		// Module file: header reflections and the module entry point.
		std::string Out = "// Generated by xrReflectTool. Do not edit.\n\n";
		if (!Pch.empty())
		{
			Out += "#include \"" + Pch + "\"\n";
		}
		Out += "#include \"pch_script.h\"\n\n";
		for (const FileInfo& File : Files)
		{
			if (!File.IsSource)
			{
				Out += "#include \"" + File.Path + "\"\n";
			}
		}
		Out += "\n";

		for (const ClassInfo* Class : Classes)
		{
			const FileInfo& Owner = *std::find_if(Files.begin(), Files.end(), [&](const FileInfo& File) { return File.Path == Class->File; });
			if (!Owner.IsSource && Class->LuaExport)
			{
				Out += GenerateClass(*Class);
			}
		}

		for (const FileInfo& File : Files)
		{
			if (File.IsSource)
			{
				Out += "void " + SourceFunctionName(Module, File) + "(lua_State* L);\n";
			}
		}

		std::set<std::string> SourceFiles;
		for (const FileInfo& File : Files)
		{
			if (File.IsSource)
			{
				SourceFiles.insert(File.Path);
			}
		}

		Out += "\nvoid XReflectRegister(lua_State* L)\n{\n";
		Out += GenerateClassCalls(Classes, "", SourceFiles);
		for (const FileInfo& File : Files)
		{
			if (!File.IsSource)
			{
				Out += GenerateRegistration(File, Classes);
			}
		}
		for (const FileInfo& File : Files)
		{
			if (File.IsSource)
			{
				Out += "\t" + SourceFunctionName(Module, File) + "(L);\n";
			}
		}
		Out += "}\n";

		if (Errors.empty())
		{
			WriteIfChanged(OutputDir + "/" + Module + ".reflect.gen.cpp", ToCrlf(Out), Errors);

			// Source files: their reflections must live in their own translation unit.
			for (const FileInfo& File : Files)
			{
				if (!File.IsSource)
				{
					continue;
				}

				std::string Inl = "// Generated by xrReflectTool from " + File.Path + ". Do not edit.\n\n";
				for (const ClassInfo* Class : Classes)
				{
					if (Class->File == File.Path && Class->LuaExport)
					{
						Inl += GenerateClass(*Class);
					}
				}
				Inl += "void " + SourceFunctionName(Module, File) + "(lua_State* L)\n{\n" + GenerateClassCalls(Classes, File.Path, std::set<std::string>{}) +
					GenerateRegistration(File, Classes) + "}\n";
				WriteIfChanged(OutputDir + "/" + File.Stem + ".reflect.inl", ToCrlf(Inl), Errors);
			}
		}
	}
}
