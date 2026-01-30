#pragma once

constexpr u32 c_vCacheSize			= 24;			// entries
constexpr u32 c_SS_LowVertLimit		= 64;			// polys
constexpr u32 c_SS_HighVertLimit	= 2*1024;		// polys
constexpr u32 c_SS_maxsize			= 32;			// meters
constexpr u32 c_PM_FaceLimit			= 128;			// face-limit
constexpr float c_PM_MetricLimit_static	= 0.10f;		// vertex-count-simplification-limit
constexpr float c_PM_MetricLimit_mu		= 0.05f;		// vertex-count-simplification-limit

struct SBuildOptions
{
	BOOL b_radiosity;
	BOOL b_noise;
	SBuildOptions() :b_radiosity(FALSE), b_noise(FALSE)
	{
	}
};

extern SBuildOptions g_build_options;






