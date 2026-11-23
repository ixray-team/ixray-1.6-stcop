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

#include "ReflectModel.h"

namespace XReflectTool
{
	// Writes SoundShaderType.gen.h (the SoundShaderType enum and the shader names) and
	// <Module>.sound.gen.cpp (class infos of the XREFLECT(SoundShader) classes) into OutputDir.
	void GenerateSound(const std::string& Module, const std::string& Pch, const std::string& OutputDir, const std::vector<FileInfo>& Files,
		const std::vector<ClassInfo*>& Classes, std::vector<ParseError>& Errors);
}
