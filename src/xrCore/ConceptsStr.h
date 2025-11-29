#pragma once

#include <type_traits>
#include "Concepts.h"

namespace XRay::Concepts
{
	template <typename T>
	concept XRayString = std::same_as<std::remove_cvref_t<T>, xr_string> || std::same_as<std::remove_cvref_t<T>, shared_str>;
}
