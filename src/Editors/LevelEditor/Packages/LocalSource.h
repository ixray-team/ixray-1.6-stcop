#pragma once

#include "PackageStore.h"
#include <stop_token>

namespace SdkPackages::LocalSource
{
bool Prepare(PackageInfo& Info, const xr_path& Stage, xr_path& Checkout, xr_string& Error, std::stop_token Stop = {}, const ProgressCallback& Progress = {});
} // namespace SdkPackages::LocalSource
