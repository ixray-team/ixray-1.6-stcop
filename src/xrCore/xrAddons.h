#pragma once

#include "xrCore.h"

class CAddonManager final
{
private:
	shared_str currentPlatform;
public:
	struct AddonInfo
	{
		shared_str EntryDir;     // Корень аддона
		shared_str AddonName;    // Уникальное имя
		shared_str ScriptInit;   // init-скрипт (без .script)

		xr_vector<xr_string> Dependencies;
		xr_set<xr_string> Platforms;
	};

public:
	CAddonManager() = default;
	~CAddonManager() = default;

	void Initialize();
	bool CanApply(xr_string& TempPath, CLocatorAPI::file& Desc);
	bool CanApplyFile(xr_string& TempPath, CLocatorAPI::file& Desc);
	void MapShaderPath(xr_string& TempPath) const;

	void MountAddons();
	// Overlays a directory or a .db archive on top of $game_data$ while the game runs.
	// Files in it replace the ones with the same relative path.
	XRCORE_API bool MountRuntime(const char* FullPath);
	// Undoes the last MountRuntime (LIFO)
	XRCORE_API void UnmountRuntime();
	u32 GetRuntimeMountCount() const { return RuntimeMountCount; }
	XRCORE_API const AddonInfo* FindAddon(const CLocatorAPI::file& Desc) const;


private:
	void CollectAddons();
	void ReadMetaInfo(const xr_string& InitFile);
	void ResolveDependencies();

public:
	xr_vector<AddonInfo> Addons;

private:
	// Runtime mounts are the last RuntimeMountCount entries of Addons
	u32 RuntimeMountCount = 0;
};

extern XRCORE_API CAddonManager* GAddonsManager;