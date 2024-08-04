#pragma once

class CAddonManager final
{
public:
	struct AddonInfo
	{
		shared_str EntryDir;
		shared_str AddonName;
		shared_str ScriptInit;
	};

	xr_vector<AddonInfo> Addons;

public:
	CAddonManager() = default;
	void CanApply(xr_string& TempPath, CLocatorAPI::file& Desc);

private:
	void ReadMetaInfo(const xr_string& InitFile);
};

extern XRCORE_API CAddonManager* g_pAddonsManager;