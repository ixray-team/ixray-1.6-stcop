#include "stdafx.h"
#include "xrAddons.h"

XRCORE_API CAddonManager* g_pAddonsManager = nullptr;

void CAddonManager::CanApply(xr_string& TempPath, CLocatorAPI::file& Desc)
{
	if (TempPath.ends_with("\\addon.init"))
	{
		ReadMetaInfo(TempPath);
		return;
	}

	static xr_string addons_path = FS.get_path("$arch_dir_addons$")->m_Path;
	static xr_string CurrentAddonName = "";
	static bool IsProcessingAddon = false;


	bool PathIsDir = std::filesystem::is_directory(TempPath.data());
	if (TempPath.Contains(addons_path) && !PathIsDir && IsProcessingAddon)
	{
		static xr_string data_path = FS.get_path("$game_data$")->m_Path;
		Desc.wrap = xr_strdup(TempPath.data());
		TempPath = data_path + TempPath.substr(CurrentAddonName.length() + addons_path.length());
	}
	else if (PathIsDir)
	{
		if (IsProcessingAddon && !TempPath.Contains(CurrentAddonName))
		{
			IsProcessingAddon = false;
			Msg("Processing %s addon completed!", CurrentAddonName.c_str());

			CurrentAddonName = "";
		}

		if (!IsProcessingAddon)
		{
			if (CurrentAddonName.empty())
			{
				CurrentAddonName += TempPath.substr(addons_path.length());
				IsProcessingAddon = std::filesystem::exists((TempPath + "addon.init"));
			}
			else if (TempPath.Contains(CurrentAddonName))
			{
				CurrentAddonName += TempPath.substr(CurrentAddonName.length() + addons_path.length());
				IsProcessingAddon = std::filesystem::exists((TempPath + "addon.init"));
			}
			else if (!TempPath.Contains(CurrentAddonName))
			{
				CurrentAddonName = "";
				IsProcessingAddon = std::filesystem::exists((TempPath + "addon.init"));

				if (IsProcessingAddon)
				{
					CurrentAddonName += TempPath.substr(addons_path.length());
				}
			}
		}
	}
}

#include <fstream>
#include <sstream>

void CAddonManager::ReadMetaInfo(const xr_string& InitFile)
{
	std::fstream FileStream(InitFile.c_str());
	std::ostringstream filebuffer;
	filebuffer << FileStream.rdbuf();

	xr_string StringBuffer = filebuffer.str().data();
	
	if (StringBuffer.empty())
		return;

	AddonInfo& NewAddon = Addons.emplace_back();
	NewAddon.EntryDir = InitFile.data();

	size_t NameEntryIndex = StringBuffer.find("name:");
	if (NameEntryIndex == xr_string::npos)
	{
		size_t NameEndIndex = StringBuffer.find("\n", NameEntryIndex);

		size_t Offet = NameEntryIndex + 6;
		NewAddon.AddonName = StringBuffer.substr(Offet, NameEndIndex - Offet).c_str();
	}

	size_t ScriptEntryIndex = StringBuffer.find("script:");
	if (ScriptEntryIndex == xr_string::npos)
		return;

	size_t ScriptEndIndex = StringBuffer.find(".script", ScriptEntryIndex);
	size_t Offet = ScriptEntryIndex;
	NewAddon.ScriptInit = StringBuffer.substr(Offet + 8, ScriptEndIndex - Offet).c_str();
}