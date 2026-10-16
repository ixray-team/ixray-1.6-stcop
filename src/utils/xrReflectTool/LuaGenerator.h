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

// luabind backend: turns the reflection model into Lua registration code.

#include "ReflectModel.h"

namespace XReflectTool
{
	// Writes <Module>.reflect.gen.cpp and <stem>.reflect.inl into OutputDir.
	// The generated module entry point is `void XReflectRegister(lua_State*)`. It is not exported,
	// so every module (DLL) has its own and shared sources call the one of the module they are built into.
	void GenerateLua(const std::string& Module, const std::string& Pch, const std::string& OutputDir, const std::vector<FileInfo>& Files,
		const std::vector<ClassInfo*>& Classes, std::vector<ParseError>& Errors);
}
