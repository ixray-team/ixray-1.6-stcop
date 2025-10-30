#include "stdafx.h"

#include "xrAddons.h"

#include "EngineExternal.h"
#include <FormatParsers/YAML/xr_yaml_reader.h>

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

static void removeAllSubstrings(xr_string& str, const xr_string& toRemove) {
	size_t pos = 0;
	while ((pos = str.find(toRemove, pos)) != xr_string::npos) {
		str.erase(pos, toRemove.length());
	}
}

void CAddonManager::ReadMetaInfo(const xr_string& InitFile)
{
	static xr_string addons_path = FS.get_path("$arch_dir_addons$")->m_Path;
	static xr_string CurrentAddonName = "";
	static bool IsProcessingAddon = false;

	bool PathIsDir = std::filesystem::is_directory(InitFile.data());
	if (InitFile.Contains(addons_path) && !PathIsDir && IsProcessingAddon)
	{
		static xr_string data_path = FS.get_path("$game_data$")->m_Path;
		CurrentAddonName = InitFile;
	}

	xr_string pathAddon = InitFile;
	removeAllSubstrings(pathAddon, "addon.init");
	CYaml yamlParser(pathAddon.c_str(), "addon.init");
	auto& rootNode = yamlParser.GetRootNode();

	AddonInfo NewAddon;
	NewAddon.EntryDir = InitFile.data();

	const std::string name = yamlParser.GetStringRoot(rootNode, "name", "");

	if (!name.empty())
	{
		NewAddon.AddonName = name.c_str();
	}

	const std::string script = yamlParser.GetStringRoot(rootNode, "script", "");
	if (!script.empty())
	{
		xr_string scriptStr = script.c_str();
		size_t dotPos = scriptStr.find(".script");
		if (dotPos != xr_string::npos)
		{
			scriptStr = scriptStr.substr(0, dotPos);
		}
		NewAddon.ScriptInit = scriptStr.c_str();
	}
}