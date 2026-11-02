#pragma once

#include "../xrCore.h"
#include <stop_token>

namespace Platform
{
struct ProcessOptions
{
	xr_vector<xr_pair<xr_string, xr_string>> Environment;
	u64 Timeout = 180000;
	size_t OutputLimit = 65536;
};

// Runs an argument vector directly, captures stdout/stderr and owns descendants.
XRCORE_API bool RunProcess(const xr_vector<xr_string>& Command, const ProcessOptions& Options, xr_string& Output, std::stop_token Stop = {});
} // namespace Platform
