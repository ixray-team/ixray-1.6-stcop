#pragma once

#include "xrCore.h"

class CAddonManager final
{
public:
	struct AddonInfo
	{
		shared_str EntryDir;     // Корень аддона
		shared_str AddonName;    // Уникальное имя
		shared_str ScriptInit;   // init-скрипт (без .script)

		xr_vector<xr_string> Dependencies;
	};

public:
	CAddonManager() = default;
	~CAddonManager() = default;

	void Initialize();
	bool CanApply(xr_string& TempPath, CLocatorAPI::file& Desc);

private:
	void CollectAddons();
	void ReadMetaInfo(const xr_string& InitFile);
	void ResolveDependencies();
	void MountAddons();

public:
	xr_vector<AddonInfo> Addons;
};

extern XRCORE_API CAddonManager* g_pAddonsManager;