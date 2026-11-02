#pragma once

#include "../../../xrCore/xrCore.h"
#include <functional>

namespace SdkPackages
{
struct PackageProgress
{
	xr_string Stage;
	xr_string Detail;
	size_t PackageIndex = 0;
	size_t PackageCount = 0;
	u64 BytesCompleted = 0;
	u64 BytesTotal = 0;
	size_t PackagesCompleted = 0;
	size_t ActivePackages = 0;
};
using ProgressCallback = std::function<void(const PackageProgress&)>;
} // namespace SdkPackages
