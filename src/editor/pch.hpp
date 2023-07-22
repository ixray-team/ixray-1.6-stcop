////////////////////////////////////////////////////////////////////////////
//	Module 		: pch.hpp
//	Created 	: 04.12.2007
//  Modified 	: 04.12.2007
//	Author		: Dmitriy Iassenev
//	Description : preocmpiled header for editor library
////////////////////////////////////////////////////////////////////////////

#ifndef PCH_HPP_INCLUDED
#define PCH_HPP_INCLUDED

#ifdef DEBUG
#	define VERIFY(expression)	do { if (!(expression)) throw; } while (0)
#	define NODEFAULT			do { __debugbreak(); } while (0)
#else // DEBUG
#	define VERIFY(expression)	do {} while (0)
#	define NODEFAULT			__assume(0)
#endif // DEBUG

typedef unsigned int			u32;
typedef char const *			LPCSTR;
typedef char *					LPSTR;

#pragma unmanaged
#include <malloc.h>
#pragma managed

#include <stdlib.h>
#include <vcclr.h>

#pragma warning(disable:4127)
#pragma warning(disable:4100)

// Our headers here:
#include "converting.hpp"

// Our forms here:
namespace editor {
	ref class window_ide;
}

#include "window_weather_editor.h"
#include "window_weather.h"
#include "window_levels.h"
#include "window_view.h"
#include "window_ide.h"

#endif // PCH_HPP_INCLUDED