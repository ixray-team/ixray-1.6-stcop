#pragma once

#define VC_EXTRALEAN				// Exclude rarely-used stuff from Windows headers
#define WIN32_LEAN_AND_MEAN			// Exclude rarely-used stuff from Windows headers
#ifndef STRICT
#	define STRICT					// Enable strict syntax
#endif // STRICT
#define IDIRECTPLAY2_OR_GREATER		// ?
#define DIRECTINPUT_VERSION	0x0800	//
#define _CRT_SECURE_NO_DEPRECATE	// vc8.0 stuff, don't deprecate several ANSI functions

// windows.h
#ifndef _WIN32_WINNT
#ifdef _MSC_VER
#define _WIN32_WINNT 0x0601
#else // ifdef _MSC_VER
#define _WIN32_WINNT 0x0500
#endif // ifdef _MSC_VER
#endif // ifndef _WIN32_WINNT

#ifdef __BORLANDC__
	#include <vcl.h>
	#include <mmsystem.h>
	#include <stdint.h>
#endif

#define NOGDICAPMASKS
#define NOMENUS
#define NOICONS
#define NOKEYSTATES
#define NODRAWTEXT
#define NOMEMMGR
#define NOSERVICE
#define NOHELP
#define NOPROFILER
#define NOMINMAX

#pragma warning(push)
#pragma warning(disable:4005)
#include <windows.h>
#include <atlstr.h>
#include <atlimage.h>
#ifndef __BORLANDC__
	#include <windowsx.h>
#endif
#pragma warning(pop)

#include <sys\utime.h>

#pragma warning(push)
#pragma warning(disable:4995)
#include <direct.h>
#include <io.h>
#include <commdlg.h>
#include <cderr.h>
#pragma warning(pop)

using xr_special_char = wchar_t;