#pragma once

#include "GitSource.h"

namespace SdkPackages::HttpSource
{
bool RelativeFile(const xr_string& File);

bool RawUrl(const xr_string& Repository, const xr_string& Commit, const xr_string& File, xr_string& Url, xr_string& Error);

bool ReadRaw(const xr_string& Repository, const xr_string& Commit, const xr_string& File, xr_string& Bytes, xr_string& Error, std::stop_token Stop = {}, bool* IsMissing = nullptr);
} // namespace SdkPackages::HttpSource
