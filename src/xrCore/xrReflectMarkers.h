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
// The macros expand to nothing for the compiler. xrReflectTool scans the sources of a module before
// the build and generates code for them: luabind registration and ini readers (src/xrScripts/xrReflect.h),
// sound shader registration (src/xrSound/New/SoundShader.h).

#define XREFLECT(...)
#define XPROPERTY(...)
