 
#pragma once

#ifdef IXR_WINDOWS
#	include "Windows/PlatformAPI.h"
#elif defined(IXR_LINUX)
#	include "Linux/PlatformAPI.h"
#elif defined(IXR_BSD)
#	include "BSD/PlatformAPI.h"
#endif