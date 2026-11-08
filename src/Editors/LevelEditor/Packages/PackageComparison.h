#pragma once

#include "PackageStore.h"

namespace SdkPackages
{
struct ComparisonProperty
{
	xr_string Section;
	xr_string Owner;
	xr_string Name;
	xr_string Current;
	xr_string Imported;
	bool IsDifferent = false;
	bool IsRawData = false;
	bool HasCurrent = true;
	bool HasImported = true;
};

struct AssetComparison
{
	xr_vector<ComparisonProperty> Properties;
	xr_vector<xr_string> Notices;
	bool IsSupported = false;
};
} // namespace SdkPackages
