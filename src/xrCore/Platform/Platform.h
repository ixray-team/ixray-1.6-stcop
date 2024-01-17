 
#pragma once

#ifdef WIN32
#   ifdef M_X64
#       define IXR_WIN64
#   else
#       define IXR_WIN32
#   endif
#   define IXR_WINDOWS
#	include "Windows/PlatformInit.h"
#elif defined(__linux__)
#	define IXR_LINUX
#	include "Linux/PlatformInit.h"
#endif