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

// Reflection markers for xrReflectTool (src/utils/xrReflectTool).
// The macros expand to nothing for the compiler. xrReflectTool scans headers
// before the build and generates luabind registration code and ini readers for them.

struct lua_State;

#define XREFLECT(...)
#define XPROPERTY(...)

namespace XReflect
{
	template <typename T>
	struct Exporter
	{
		static void Register(lua_State* L);
	};

	template <typename T>
	struct IniReader
	{
		static void Read(T& Object, const CInifile& Ini, const char* Section, const char* Prefix);
	};

	// The ltx schema of the editor, generated from the IniRead classes of a module
	struct SIniField
	{
		const char* Key;
		const char* Type;
		bool Required;
		// Key of the bool field that enables this one, empty when always read
		const char* If;
		// Key is the prefix of a nested schema of the class Type
		bool Nested;
	};

	struct SIniSchema
	{
		const char* Name;
		// Object class that reads the section, empty for the nested schemas
		const char* Owner;
		// Comma separated
		const char* Bases;
		const SIniField* Fields;
		u32 FieldCount;
	};

	const SIniSchema* GetIniSchemas(u32& Count);

	namespace Detail
	{
		inline const char* IniKey(string256& Buffer, const char* Prefix, const char* Key)
		{
			if (!Prefix || !Prefix[0])
			{
				return Key;
			}
			xr_strconcat(Buffer, Prefix, Key);
			return Buffer;
		}
	}

	template <typename T>
	void ReadIniValue(const CInifile& Ini, const char* Section, const char* Prefix, const char* Key, T& Value, bool Required)
	{
		string256 Buffer;
		const char* Line = Detail::IniKey(Buffer, Prefix, Key);
		if constexpr (std::is_same_v<T, shared_str>)
		{
			Value = Required ? Ini.r_string(Section, Line) : Ini.read_if_exists<str_c>(Section, Line, Value.c_str());
		}
		else
		{
			Value = Required ? Ini.read<T>(Section, Line) : Ini.read_if_exists<T>(Section, Line, Value);
		}
	}

	template <typename T>
	void ReadIniNested(const CInifile& Ini, const char* Section, const char* Prefix, const char* NestedPrefix, T& Value)
	{
		string256 Buffer;
		IniReader<T>::Read(Value, Ini, Section, Detail::IniKey(Buffer, Prefix, NestedPrefix));
	}

	template <typename T>
	void ReadIni(T& Object, const shared_str& Section, const CInifile& Ini = *pSettings)
	{
		IniReader<T>::Read(Object, Ini, Section.c_str(), "");
	}
}

// Grants the generated code access to non-public members.
#define XGENERATED_BODY() template <typename> friend struct ::XReflect::Exporter;
