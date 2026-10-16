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

// Parses XREFLECT / XPROPERTY markers into the reflection model.

#include "ReflectModel.h"

namespace XReflectTool
{
	// Parses one source file. File.Path, IsSource and Stem must be set.
	void ParseFile(FileInfo& File, const std::string& Source, std::vector<ParseError>& Errors);

	// Attaches Self enums and functions to their classes and sorts classes so bases come first.
	std::vector<ClassInfo*> ResolveModel(std::vector<FileInfo>& Files, std::vector<ParseError>& Errors);
}
