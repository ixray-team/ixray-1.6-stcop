#pragma once

#include "../../xrCore/xrCore.h"

#include <string>

// Явное преобразование std::string на внешней filesystem/JSON границе.
inline xr_string ToXrString(const std::string& Value)
{
	return xr_string(Value.data(), static_cast<u32>(Value.size()));
}
