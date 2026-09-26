#pragma once

#ifdef IXR_WINDOWS
#	include "Windows/OSVersion.h"
#else
namespace Platform
{
inline bool IsWindows7OrOlder()
{
	return false;
}
} // namespace Platform
#endif
