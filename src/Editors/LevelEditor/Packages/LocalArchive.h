#pragma once

#include "PackageStore.h"

namespace SdkPackages::LocalSource
{
inline constexpr u64 MaxFileSize = 2ull * 1024 * 1024 * 1024;
inline constexpr u64 MaxExpandedSize = 16ull * 1024 * 1024 * 1024;
inline constexpr u32 MaxEntries = 100000;
inline constexpr size_t MaxDirectorySize = 64 * 1024 * 1024;

bool ExtractArchive(const xr_path& Path, const xr_path& Target, xr_string& Error, std::stop_token Stop, const ProgressCallback& Progress);
} // namespace SdkPackages::LocalSource
