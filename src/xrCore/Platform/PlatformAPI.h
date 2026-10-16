 
#pragma once

#ifdef IXR_WINDOWS
#	include "Windows/PlatformAPI.h"
#elif defined(IXR_LINUX)
#	include "Linux/PlatformAPI.h"
#elif defined(IXR_BSD)
#	include "BSD/PlatformAPI.h"
#elif defined(IXR_MACOS)
#   include "MacOS/PlatformAPI.h" // AI: Platform.h defines IXR_MACOS; this used to check IXR_APPLE, which is never defined, so no platform header was included at all
#endif