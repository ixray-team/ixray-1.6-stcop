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
#else // #ifdef DEBUG
#	define VERIFY(expression)	do {} while (0)
#	define NODEFAULT			__assume(0)
#endif // #ifdef DEBUG

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

// do not forget to call
// 'cs_free'
// on the block of memory being returned
#include <converting.hpp>

#endif // #ifndef PCH_HPP_INCLUDED