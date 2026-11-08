#pragma once

#include "PackageStore.h"

namespace SdkPackages::TextureDependencyFormats
{
struct Description
{
	xr_string Bump;
	xr_string Detail;
	u32 DetailUsage = 0;
	u32 BumpMode = 0;
	bool HasSpecification = false;
	bool HasDetail = false;
	bool HasEmptyBumpReference = false;
};

bool ReadIndex(const xr_path& Path, xr_hash_map<xr_string, Description>& Result, xr_string& Error, std::stop_token Stop = {});
bool ReadThumbnail(const xr_path& Path, Description& Result, xr_string& Error, std::stop_token Stop = {});
bool ReadSequence(const xr_path& Path, xr_vector<xr_string>& Frames, xr_string& Error, std::stop_token Stop = {});
} // namespace SdkPackages::TextureDependencyFormats
