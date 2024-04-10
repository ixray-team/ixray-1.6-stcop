 
#pragma once

#ifdef WIN32
#	ifdef _M_X64
#		define IXR_WIN64
#		define IXR_X64
#	elif defined(_M_ARM64)
#		define IXR_WIN64
#		define IXR_ARM64
#		define IXR_X64
#	else
#		define IXR_WIN32
#	endif
#   define IXR_WINDOWS
#	include "Windows/PlatformInit.h"
#elif defined(__linux__)
#	define IXR_LINUX
#   define IXR_X64
#	include "Linux/PlatformInit.h"
#endif