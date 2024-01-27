// stdafx.h : include file for standard system include files,
//  or project specific include files that are used frequently, but
//      are changed infrequently
//
// Third generation by Oles.
#pragma once

#include "../../xrCore/xrCore.h"
#include <io.h>
#include <fcntl.h>
#include <sys\stat.h>

#undef ENGINE_API
#define ENGINE_API
#define XR_EPROPS_API
#define ECORE_API
#define NVTT_SHARED 1

#ifndef MAKEFOURCC
#define MAKEFOURCC(ch0, ch1, ch2, ch3)                              \
	((DWORD)(BYTE)(ch0) | ((DWORD)(BYTE)(ch1) << 8) |   \
	((DWORD)(BYTE)(ch2) << 16) | ((DWORD)(BYTE)(ch3) << 24 ))
#endif //defined(MAKEFOURCC)

#pragma warning( disable : 4995 )
