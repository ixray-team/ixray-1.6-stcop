#pragma once

#include <windows.h>
#include <VersionHelpers.h>

namespace Platform
{
inline bool IsWindows7OrOlder()
{
	return !IsWindows8OrGreater();
}
} // namespace Platform
