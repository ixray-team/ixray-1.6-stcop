#pragma once

#include "PackageStore.h"
#include <stop_token>

namespace SdkPackages::GitSource
{
bool IsCommit(const xr_string& Commit);

bool ResolveRevision(const xr_string& Url, const xr_string& Ref, xr_string& Commit, xr_string& Error, std::stop_token Stop = {});

bool Checkout(const xr_string& Url, const xr_string& Ref, const xr_path& Destination, xr_string& Commit, xr_string& Error, std::stop_token Stop = {}, const xr_string& ExpectedCommit = {});
} // namespace SdkPackages::GitSource
