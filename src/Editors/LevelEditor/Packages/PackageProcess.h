#pragma once

#include "PackageStore.h"
#include <stop_token>

namespace SdkPackages::PackageProcess
{
bool Run(const xr_vector<xr_string>& Command, xr_string& Output, std::stop_token Stop = {}, u64 Timeout = 180000, size_t Limit = 65536);
} // namespace SdkPackages::PackageProcess
